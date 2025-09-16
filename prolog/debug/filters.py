"""
Trace output filters with composable rules and spypoint management.

Provides selective tracing through port, predicate, depth, and sampling filters.
"""

import random
from typing import Optional, Set, Dict, Any
from dataclasses import dataclass, field

from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.unify.store import Store
from prolog.unify.unify_helpers import deref_term


@dataclass
class TraceFilters:
    """
    Configuration for trace filtering with precedence rules.

    Filter precedence: port → predicate → depth → sampling
    """
    ports: Optional[Set[str]] = None
    predicates: Optional[Set[str]] = None
    spypoints: Set[str] = field(default_factory=set)
    min_depth: int = 0
    max_depth: Optional[int] = None
    sampling_rate: int = 1
    sampling_seed: Optional[int] = None
    bindings_policy: str = 'none'
    max_term_depth: int = 10
    max_items_per_list: int = 10

    # Internal fields
    _rng: Optional[random.Random] = field(default=None, init=False, repr=False)
    _sample_counter: int = field(default=0, init=False, repr=False)

    def __post_init__(self):
        """Validate configuration and initialize RNG."""
        # Validate sampling rate
        if self.sampling_rate < 1:
            raise ValueError(f"sampling_rate must be >= 1, got {self.sampling_rate}")

        # Validate depths
        if self.min_depth < 0:
            raise ValueError(f"min_depth must be >= 0, got {self.min_depth}")
        if self.max_depth is not None and self.max_depth < 0:
            raise ValueError(f"max_depth must be >= 0, got {self.max_depth}")
        if self.max_depth is not None and self.min_depth > self.max_depth:
            raise ValueError(f"min_depth ({self.min_depth}) > max_depth ({self.max_depth})")

        # Validate ports
        valid_ports = {'call', 'exit', 'redo', 'fail'}
        if self.ports is not None:
            invalid = self.ports - valid_ports
            if invalid:
                raise ValueError(f"Invalid ports: {invalid}")

        # Validate bindings policy
        valid_policies = {'none', 'names', 'names_values'}
        if self.bindings_policy not in valid_policies:
            raise ValueError(f"Invalid bindings_policy: {self.bindings_policy}")

        # Initialize RNG if seed provided
        if self.sampling_seed is not None:
            self._rng = random.Random(self.sampling_seed)

    def add_spypoint(self, pred_id: str):
        """Add a spypoint."""
        self.spypoints.add(pred_id)

    def remove_spypoint(self, pred_id: str):
        """Remove a spypoint."""
        self.spypoints.discard(pred_id)

    def clear_spypoints(self):
        """Clear all spypoints."""
        self.spypoints.clear()

    def _truncate_term(self, term: Any, depth: int, max_depth: int,
                      max_items: int, store: Optional[Store] = None) -> str:
        """
        Convert term to string with truncation.

        Args:
            term: Term to convert
            depth: Current nesting depth
            max_depth: Maximum depth to show
            max_items: Maximum list items to show
            store: Store for dereferencing variables

        Returns:
            String representation with truncation
        """
        if depth >= max_depth:
            return "..."

        # Dereference if we have a store
        if store:
            _, term = deref_term(term, store)

        # Handle different term types
        if isinstance(term, Atom):
            return term.name if hasattr(term, 'name') else str(term)
        elif isinstance(term, Int):
            return str(term.value if hasattr(term, 'value') else term)
        elif isinstance(term, Var):
            # Unbound variable
            return f"_{term.hint}" if term.hint else f"_G{term.id}"
        elif isinstance(term, Struct):
            if depth + 1 >= max_depth:
                # Would exceed depth on next level
                return f"{term.functor}(...)"

            # Recursively format arguments
            formatted_args = []
            for arg in term.args:
                formatted = self._truncate_term(arg, depth + 1, max_depth, max_items, store)
                formatted_args.append(formatted)

            args_str = ", ".join(formatted_args)
            return f"{term.functor}({args_str})"
        elif isinstance(term, List):
            if depth + 1 >= max_depth:
                return "[...]"

            # Prefer Python list-like representation if available
            if hasattr(term, 'items'):
                limited = term.items[:max_items]
                parts = [self._truncate_term(x, depth + 1, max_depth, max_items, store) for x in limited]
                if len(term.items) > max_items:
                    parts.append("...")
                return "[" + ", ".join(parts) + "]"

            # Prolog [H|T] representation
            parts, count, current = [], 0, term
            while count < max_items and isinstance(current, List):
                if hasattr(current, 'head'):
                    parts.append(self._truncate_term(current.head, depth + 1, max_depth, max_items, store))
                    count += 1
                    current = current.tail
                    if isinstance(current, Atom) and getattr(current, 'name', None) == "[]":
                        # Proper list ending
                        return "[" + ", ".join(parts) + "]"
                else:
                    break

            # Reached cap or improper list
            if isinstance(current, List):
                # Hit max_items limit with more list to go
                parts.append("...")
            elif not (isinstance(current, Atom) and getattr(current, 'name', None) == "[]"):
                # Improper list [H|T] where T is not []
                parts.append("|")
                parts.append(self._truncate_term(current, depth + 1, max_depth, max_items, store))

            return "[" + ", ".join(parts) + "]"
        else:
            # Fallback
            return str(term)

    def process_bindings(self, event: Any, bindings: Dict[str, Any],
                        fresh_var_map: Optional[Dict[int, str]] = None) -> Dict[str, Any]:
        """Process bindings according to policy."""
        result = {}

        if self.bindings_policy == 'none':
            # Don't include bindings field at all
            return result

        if self.bindings_policy == 'names':
            # Include names but not values
            result['bindings'] = {k: None for k in bindings.keys()}
        elif self.bindings_policy == 'names_values':
            # Include names and values with truncation
            result['bindings'] = {}

            for name, value in bindings.items():
                # Apply truncation
                truncated = self._truncate_term(
                    value, 0, self.max_term_depth, self.max_items_per_list
                )

                # Convert to appropriate representation
                if isinstance(value, Atom):
                    result['bindings'][name] = value.name if hasattr(value, 'name') else str(value)
                elif isinstance(value, Int):
                    result['bindings'][name] = value.value if hasattr(value, 'value') else value
                elif isinstance(value, Var):
                    # Unbound variable
                    if fresh_var_map and value.id in fresh_var_map:
                        result['bindings'][name] = fresh_var_map[value.id]
                    else:
                        result['bindings'][name] = value.hint if value.hint else f"_G{value.id}"
                else:
                    # Complex term - use truncated string
                    result['bindings'][name] = truncated

        return result


def should_emit_event(event: Any, filters: TraceFilters) -> bool:
    """
    Check if an event should be emitted based on filters.

    Filter precedence: port → predicate → depth → sampling
    Returns True if event passes all filters.
    """
    # Check port filter (first)
    if filters.ports is not None:
        if event.port not in filters.ports:
            return False

    # Check predicate filter (second)
    # Logic:
    # - If predicates is None and no spypoints: allow all
    # - If predicates is None but spypoints exist: only allow spypoints
    # - If predicates is empty set: block all except spypoints
    # - If predicates has values: allow union of predicates and spypoints
    if filters.predicates is not None or filters.spypoints:
        if filters.predicates is None:
            # No predicate filter set
            if filters.spypoints:
                # Only allow spypoints
                if event.pred_id not in filters.spypoints:
                    return False
            # else: allow all (no predicate filter, no spypoints)
        elif filters.predicates == set():
            # Empty predicate set blocks all except spypoints
            if event.pred_id not in filters.spypoints:
                return False
        else:
            # Predicates filter is set with values
            if event.pred_id not in filters.predicates and event.pred_id not in filters.spypoints:
                return False

    # Check depth filter (third)
    if filters.min_depth > 0 and event.frame_depth < filters.min_depth:
        return False
    if filters.max_depth is not None and event.frame_depth > filters.max_depth:
        return False

    # Check sampling filter (last)
    if filters.sampling_rate > 1:
        # Increment counter for events that reach sampling check
        filters._sample_counter += 1

        if filters._rng is not None:
            # Deterministic sampling with RNG - consume only when needed
            return filters._rng.random() < (1.0 / filters.sampling_rate)
        else:
            # Simple modulo sampling based on considered events
            return (filters._sample_counter % filters.sampling_rate) == 0

    return True