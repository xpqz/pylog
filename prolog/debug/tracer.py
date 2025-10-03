"""
Core tracing infrastructure for PyLog debug system.

Provides the 4-port tracer (call/exit/redo/fail) with minimal overhead
when disabled and deterministic output for reproducibility.
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, replace
from typing import Any, Dict, List, Optional, Callable

from prolog.ast.terms import Term, Atom, Struct, Var
from prolog.debug.filters import TraceFilters, should_emit_event


@dataclass(frozen=True, slots=True)
class TraceEvent:
    """
    Single trace event with all metadata.

    Immutable dataclass with slots for memory efficiency.
    Schema version 1 for JSONL format compatibility.
    """

    version: int  # Schema version (currently 1)
    run_id: str  # UUID for this query run
    step_id: int  # Global monotonic counter (post-filter)
    port: str  # 'call'|'exit'|'redo'|'fail'
    goal: Term  # Current goal being traced
    goal_pretty: str  # Pretty-printed goal
    goal_canonical: str  # Canonical form for parsing
    frame_depth: int  # Frame stack depth
    cp_depth: int  # Choicepoint stack depth
    goal_height: int  # Goal stack height
    write_stamp: int  # Write stamp from engine
    pred_id: str  # Interned "name/arity"
    bindings: Optional[Dict[str, Any]] = None  # Variable bindings (optional)
    monotonic_ns: Optional[int] = None  # For timing (optional)

    def __post_init__(self):
        """Validate event fields."""
        allowed_ports = {"call", "exit", "redo", "fail"}
        if self.port not in allowed_ports:
            raise ValueError(f"invalid port: {self.port!r}")
        if self.step_id < 0:
            raise ValueError("step_id must be >= 0")
        if (
            self.frame_depth < 0
            or self.cp_depth < 0
            or self.goal_height < 0
            or self.write_stamp < 0
        ):
            raise ValueError("depths/heights/write_stamp must be non-negative")


@dataclass(frozen=True, slots=True)
class InternalEvent:
    """
    Internal debug event for detailed execution tracking.

    These events track internal engine operations beyond the standard 4-port model.
    They are OFF by default and add extra events when enabled.
    """

    step_id: int  # Global monotonic counter (shared with TraceEvent)
    kind: str  # Event type: 'cp_push'|'cp_pop'|'frame_push'|'frame_pop'|'cut_commit'|'catch_switch'
    details: Dict[str, Any]  # Event-specific details


class PortsTracer:
    """
    Main tracer managing events and output.

    Key policies:
    - step_id increments ONLY after filters (post-filter)
    - pred_id cache resets per run_id
    - Default caps: max_term_depth=4, max_items_per_list=10
    - Default bindings_policy='none'
    """

    def __init__(self, engine, filters: Optional[TraceFilters] = None):
        """Initialize tracer with engine reference and optional filters."""
        self.engine = engine
        self.step_counter = 0
        self.run_id = str(uuid.uuid4())
        self.sinks: List[Any] = []  # Output sinks
        self.spypoints: set = set()  # Set of (name, arity) tuples
        self.bindings_policy = "none"  # 'none'|'names'|'names_values'
        self.max_term_depth = 4
        self.max_items_per_list = 10
        self.enable_internal_events = False

        # Predicate ID interning cache (reset per run)
        self._pred_id_cache: Dict[tuple, str] = {}

        # Filters (TraceFilters integration)
        self._filters: Optional[TraceFilters] = filters
        self._custom_filter: Optional[Callable] = None

    def _intern_pred_id(self, name: str, arity: int) -> str:
        """
        Intern a predicate ID for efficiency.

        Returns "name/arity" format, cached per run.
        """
        key = (name, arity)
        if key not in self._pred_id_cache:
            self._pred_id_cache[key] = f"{name}/{arity}"
        return self._pred_id_cache[key]

    def _reset_for_new_run(self):
        """Reset tracer state for a new query run."""
        self.step_counter = 0
        self.run_id = str(uuid.uuid4())
        self._pred_id_cache.clear()

    def set_filters(self, filters: TraceFilters) -> None:
        """Set TraceFilters instance for event filtering."""
        self._filters = filters

    def set_filter(self, filter_func: Callable) -> None:
        """Set simple filter function (REPL compatibility)."""
        # Store custom filter function separately
        self._custom_filter = filter_func
        # Clear TraceFilters to avoid conflicts
        self._filters = None

    def _create_event(self, port: str, goal: Term) -> TraceEvent:
        """
        Create a trace event from current engine state.

        Note: step_id is set to 0 here and updated in emit_event
        after filters pass.
        """
        # Extract predicate name and arity from goal
        if isinstance(goal, Atom):
            pred_name = goal.name
            pred_arity = 0
        elif isinstance(goal, Struct):
            pred_name = goal.functor
            pred_arity = len(goal.args)
        elif isinstance(goal, Var):
            # Variables get a cleaner pred_id
            pred_name = "var"
            pred_arity = 0
        else:
            # Other term types
            pred_name = str(goal)
            pred_arity = 0

        pred_id = self._intern_pred_id(pred_name, pred_arity)

        # Get pretty and canonical forms
        # Phase 1: Use str() for both; canonical form will be enhanced in Phase 4+
        goal_pretty = str(goal)
        goal_canonical = str(goal)

        # Extract stack depths from engine
        override = getattr(self.engine, "_frame_depth_override", None)
        if override is not None:
            frame_depth = override
        else:
            frame_depth = len(self.engine.frame_stack)
        cp_depth = len(getattr(self.engine, "cp_stack", []))
        goal_height = len(getattr(self.engine, "goal_stack", []))
        write_stamp = getattr(self.engine, "write_stamp", 0)

        # Create event with step_id=0 (will be set post-filter)
        return TraceEvent(
            version=1,
            run_id=self.run_id,
            step_id=0,  # Set in emit_event after filters
            port=port,
            goal=goal,
            goal_pretty=goal_pretty,
            goal_canonical=goal_canonical,
            frame_depth=frame_depth,
            cp_depth=cp_depth,
            goal_height=goal_height,
            write_stamp=write_stamp,
            pred_id=pred_id,
            bindings=None,  # Will be added based on policy
            monotonic_ns=None,  # Will be added if timestamps enabled
        )

    def _create_event_with_bindings(
        self, port: str, goal: Term, bindings: Dict[str, Any]
    ) -> TraceEvent:
        """
        Create a trace event with variable bindings from current engine state.

        Args:
            port: Trace port ('call'|'exit'|'redo'|'fail')
            goal: Current goal being traced
            bindings: Variable bindings dictionary

        Returns:
            TraceEvent with bindings included
        """
        # Create base event without bindings
        event = self._create_event(port, goal)

        # Return event with bindings included
        return replace(event, bindings=bindings)

    def _can_prefilter_by_predicate(self) -> bool:
        """
        Check if the current custom filter can be optimized by predicate-based pre-filtering.
        This is a heuristic to identify common filter patterns that only check pred_id.
        """
        # For now, assume any custom filter might benefit from pre-filtering
        # In the future, we could analyze the filter function or use attributes
        return True

    def _prefilter_by_predicate(self, port: str, goal: Term) -> bool:
        """
        Fast predicate-based pre-filtering before event creation.

        Performance optimization: This method avoids expensive _create_event() calls
        by testing filters with minimal predicate information. Particularly effective
        for filters that only check pred_id (e.g., spypoints, predicate-based filtering).

        Returns True if the event should potentially be emitted.
        """
        # Extract predicate info efficiently
        if isinstance(goal, Atom):
            pred_name = goal.name
            pred_arity = 0
        elif isinstance(goal, Struct):
            pred_name = goal.functor
            pred_arity = len(goal.args)
        else:
            # For other cases, don't pre-filter (conservative)
            return True

        pred_id = f"{pred_name}/{pred_arity}"

        # Create a minimal pseudo-event for the filter
        # Performance: This avoids expensive _create_event operations like:
        # - String conversion of goal (str(goal))
        # - Stack depth calculation (len(frame_stack), len(cp_stack), etc.)
        # - Engine state extraction (write_stamp, frame overrides)
        # For filters that only check pred_id, this can provide significant speedup.
        class MinimalEvent:
            def __init__(self, port, pred_id):
                self.port = port
                self.pred_id = pred_id
                # Add other commonly used attributes as None/defaults
                self.step_id = 0
                self.frame_depth = 0
                self.goal_pretty = None
                self.goal_canonical = None
                self.bindings = None
                self.cp_depth = 0
                self.goal_height = 0
                self.run_id = ""
                self.write_stamp = 0

        minimal_event = MinimalEvent(port, pred_id)

        try:
            # Test the filter with minimal event
            return self._custom_filter(minimal_event)
        except (AttributeError, TypeError):
            # If filter fails with minimal event, fall back to full event creation
            return True

    def _should_emit(self, event: TraceEvent) -> bool:
        """
        Apply filters to determine if event should be emitted.
        """
        # Check custom filter first (for backwards compatibility)
        if self._custom_filter is not None:
            return self._custom_filter(event)

        # Use TraceFilters if available
        if self._filters is not None:
            return should_emit_event(event, self._filters)

        # No filters - emit all events
        return True

    def emit_event(self, port_or_event, goal: Term = None):
        """
        Emit a trace event through configured sinks.

        Key policy: step_id increments ONLY for emitted events (post-filter).
        Performance note: Fast path avoids isinstance checks and can pre-filter.

        Args:
            port_or_event: Either a port string ('call'|'exit'|'redo'|'fail') or a TraceEvent
            goal: Goal term (required if port_or_event is a string)
        """
        # Fast path for the common case (port, goal) with pre-filtering
        if goal is not None:
            # Fast pre-filter check for custom filters that can work with minimal data
            if self._custom_filter is not None and self._can_prefilter_by_predicate():
                # Quick predicate-based filtering before expensive event creation
                if not self._prefilter_by_predicate(port_or_event, goal):
                    return  # Filtered out early, no step_id increment

            # Create event from port and goal
            event = self._create_event(port_or_event, goal)
        else:
            # Handle TraceEvent case or error
            if isinstance(port_or_event, TraceEvent):
                event = port_or_event
            else:
                # Create event from port and goal
                event = self._create_event(port_or_event, goal)

        # Apply filters (final check for complex filters)
        if not self._should_emit(event):
            return  # Filtered out, no step_id increment

        # Increment step counter ONLY for emitted events
        self.step_counter += 1

        # Update event with actual step_id
        event = replace(event, step_id=self.step_counter)

        # Send to all sinks
        for sink in self.sinks:
            sink.write_event(event)

    def emit_event_with_bindings(self, port: str, goal: Term, bindings: Dict[str, Any]):
        """
        Emit a trace event with variable bindings through configured sinks.

        Args:
            port: Trace port ('call'|'exit'|'redo'|'fail')
            goal: Current goal being traced
            bindings: Variable bindings dictionary
        """
        # Create event with bindings
        event = self._create_event_with_bindings(port, goal, bindings)

        # Apply filters
        if not self._should_emit(event):
            return  # Filtered out, no step_id increment

        # Increment step counter ONLY for emitted events
        self.step_counter += 1

        # Update event with actual step_id
        event = replace(event, step_id=self.step_counter)

        # Send to all sinks
        for sink in self.sinks:
            sink.write_event(event)

    def emit_internal_event(self, kind: str, details: Dict[str, Any]):
        """
        Emit an internal debug event if enabled.

        Args:
            kind: Event type ('cp_push'|'cp_pop'|'frame_push'|'frame_pop'|'cut_commit'|'catch_switch')
            details: Event-specific details
        """
        if not self.enable_internal_events:
            return

        # Increment step counter for internal events
        self.step_counter += 1

        # Create internal event
        event = InternalEvent(step_id=self.step_counter, kind=kind, details=details)

        # Send to all sinks
        for sink in self.sinks:
            sink.write_event(event)

    def add_sink(self, sink):
        """Add a sink to the tracer (convenience method)."""
        self.sinks.append(sink)
        return sink
