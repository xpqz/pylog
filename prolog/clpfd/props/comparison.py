"""Comparison propagators for CLP(FD).

Implements #</2, #=</2, #>/2, #>=/2 constraint propagation.
"""

from typing import Optional, List, Tuple
from prolog.clpfd.api import get_domain, set_domain


def create_less_than_propagator(x_id: int, y_id: int):
    """Create a less-than propagator for X #< Y.

    Enforces: X.max < Y.min by adjusting domains.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #< Y
    """
    def less_than_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate X #< Y by adjusting domain bounds."""
        x_dom = get_domain(store, x_id)
        y_dom = get_domain(store, y_id)

        if x_dom is None or y_dom is None:
            return ('ok', None)

        changed = []

        # X #< Y means X < Y for all valid assignments
        # The strongest form of bounds consistency:
        # X.max < Y.min must be possible

        # First check if the constraint can be satisfied at all
        if x_dom.min() is not None and y_dom.max() is not None:
            if x_dom.min() >= y_dom.max():
                return ('fail', None)  # No solution possible

        # Update X: must be strictly less than Y
        # Standard bounds consistency: max(X) < max(Y)
        # So X can be at most max(Y) - 1
        if y_dom.max() is not None:
            new_x_dom = x_dom.remove_gt(y_dom.max() - 1)
            if new_x_dom.is_empty():
                return ('fail', None)
            if new_x_dom is not x_dom:
                set_domain(store, x_id, new_x_dom, trail)
                changed.append(x_id)
                x_dom = new_x_dom

        # Update Y: must be strictly greater than X
        # Standard bounds consistency: min(Y) > min(X)
        # So Y must be at least min(X) + 1
        if x_dom.min() is not None:
            new_y_dom = y_dom.remove_lt(x_dom.min() + 1)
            if new_y_dom.is_empty():
                return ('fail', None)
            if new_y_dom is not y_dom:
                set_domain(store, y_id, new_y_dom, trail)
                changed.append(y_id)

        return ('ok', changed if changed else None)

    return less_than_propagator


def create_less_equal_propagator(x_id: int, y_id: int):
    """Create a less-than-or-equal propagator for X #=< Y.

    Enforces: X.max <= Y.max by adjusting domains.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #=< Y
    """
    def less_equal_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate X #=< Y by adjusting domain bounds."""
        x_dom = get_domain(store, x_id)
        y_dom = get_domain(store, y_id)

        if x_dom is None or y_dom is None:
            return ('ok', None)

        changed = []

        # X must be <= Y, so X.max <= Y.max and Y.min >= X.min

        # Narrow X: remove values > Y.max
        if y_dom.max() is not None:
            new_x_dom = x_dom.remove_gt(y_dom.max())
            if new_x_dom.is_empty():
                return ('fail', None)
            if new_x_dom is not x_dom:
                set_domain(store, x_id, new_x_dom, trail)
                changed.append(x_id)
                x_dom = new_x_dom

        # Narrow Y: remove values < X.min
        if x_dom.min() is not None:
            new_y_dom = y_dom.remove_lt(x_dom.min())
            if new_y_dom.is_empty():
                return ('fail', None)
            if new_y_dom is not y_dom:
                set_domain(store, y_id, new_y_dom, trail)
                changed.append(y_id)

        return ('ok', changed if changed else None)

    return less_equal_propagator


def create_greater_than_propagator(x_id: int, y_id: int):
    """Create a greater-than propagator for X #> Y.

    This is equivalent to Y #< X.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #> Y
    """
    # X #> Y is the same as Y #< X
    return create_less_than_propagator(y_id, x_id)


def create_greater_equal_propagator(x_id: int, y_id: int):
    """Create a greater-than-or-equal propagator for X #>= Y.

    This is equivalent to Y #=< X.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #>= Y
    """
    # X #>= Y is the same as Y #=< X
    return create_less_equal_propagator(y_id, x_id)