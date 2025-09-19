"""
Core tracing infrastructure for PyLog debug system.

Provides the 4-port tracer (call/exit/redo/fail) with minimal overhead
when disabled and deterministic output for reproducibility.
"""

import uuid
from dataclasses import dataclass, replace
from typing import Any, Dict, List, Optional, TYPE_CHECKING

from prolog.ast.terms import Term, Atom, Struct, Var

if TYPE_CHECKING:
    from prolog.engine.engine import Engine


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
        if self.frame_depth < 0 or self.cp_depth < 0 or self.goal_height < 0 or self.write_stamp < 0:
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

    def __init__(self, engine: 'Engine'):
        """Initialize tracer with engine reference."""
        self.engine = engine
        self.step_counter = 0
        self.run_id = str(uuid.uuid4())
        self.sinks: List[Any] = []  # Output sinks
        self.spypoints: set = set()  # Set of (name, arity) tuples
        self.bindings_policy = 'none'  # 'none'|'names'|'names_values'
        self.max_term_depth = 4
        self.max_items_per_list = 10
        self.enable_internal_events = False

        # Predicate ID interning cache (reset per run)
        self._pred_id_cache: Dict[tuple, str] = {}

        # Filters (will be implemented in Phase 5)
        self._filters = None

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
        override = getattr(self.engine, '_frame_depth_override', None)
        if override is not None:
            frame_depth = override
        else:
            frame_depth = len(self.engine.frame_stack)
        cp_depth = len(getattr(self.engine, 'cp_stack', []))
        goal_height = len(getattr(self.engine, 'goal_stack', []))
        write_stamp = getattr(self.engine, 'write_stamp', 0)

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
            monotonic_ns=None  # Will be added if timestamps enabled
        )

    def _should_emit(self, event: TraceEvent) -> bool:
        """
        Apply filters to determine if event should be emitted.

        For Phase 1, always return True (no filters yet).
        """
        # Phase 5 will implement actual filtering
        return True

    def emit_event(self, port: str, goal: Term):
        """
        Emit a trace event through configured sinks.

        Key policy: step_id increments ONLY for emitted events (post-filter).
        """
        # Create event
        event = self._create_event(port, goal)

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
        event = InternalEvent(
            step_id=self.step_counter,
            kind=kind,
            details=details
        )

        # Send to all sinks
        for sink in self.sinks:
            sink.write_event(event)

    def add_sink(self, sink):
        """Add a sink to the tracer (convenience method)."""
        self.sinks.append(sink)
        return sink
