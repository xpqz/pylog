"""
Trace output filters with composable rules and spypoint management.

Provides selective tracing through port, predicate, depth, and sampling filters.
"""

from typing import Optional, Set, Dict, Any
from dataclasses import dataclass, field


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

    def __post_init__(self):
        """Validate configuration."""
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

    def add_spypoint(self, pred_id: str):
        """Add a spypoint."""
        self.spypoints.add(pred_id)

    def remove_spypoint(self, pred_id: str):
        """Remove a spypoint."""
        self.spypoints.discard(pred_id)

    def clear_spypoints(self):
        """Clear all spypoints."""
        self.spypoints.clear()

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
            # Include names and values
            # TODO: Implement truncation based on max_term_depth and max_items_per_list
            result['bindings'] = {}
            for k, v in bindings.items():
                # Simple stub - real implementation would handle truncation
                if hasattr(v, 'value'):
                    result['bindings'][k] = v.value
                elif hasattr(v, 'name'):
                    result['bindings'][k] = v.name
                else:
                    result['bindings'][k] = str(v)

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
    if filters.predicates is not None or filters.spypoints:
        # If predicates is None and no spypoints, allow all
        # If predicates is None but spypoints exist, only allow spypoints
        # If predicates exists, allow union of predicates and spypoints
        if filters.predicates is None:
            if filters.spypoints and event.pred_id not in filters.spypoints:
                return False
        elif event.pred_id not in filters.predicates and event.pred_id not in filters.spypoints:
            return False

    # Check depth filter (third)
    if filters.min_depth > 0 and event.frame_depth < filters.min_depth:
        return False
    if filters.max_depth is not None and event.frame_depth > filters.max_depth:
        return False

    # Check sampling filter (last)
    if filters.sampling_rate > 1:
        # Simple deterministic sampling based on step_id
        if filters.sampling_seed is not None:
            # Use seed for deterministic sampling
            import random
            rng = random.Random(filters.sampling_seed)
            # Generate deterministic sequence based on step_id
            for _ in range(event.step_id + 1):
                val = rng.random()
            return val < (1.0 / filters.sampling_rate)
        else:
            # Simple modulo sampling without seed
            return event.step_id % filters.sampling_rate == 0

    return True