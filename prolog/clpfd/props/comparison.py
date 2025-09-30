"""Comparison propagators for CLP(FD).

Implements #</2, #=</2, #>/2, #>=/2 constraint propagation.
"""

from typing import Optional, List, Tuple
from prolog.clpfd.api import set_domain
from prolog.clpfd.entailment import normalize_arg


def create_less_than_propagator(x_id: int, y_id: int):
    """Create a less-than propagator for X #< Y.

    Enforces: X.max < Y.min by adjusting domains.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #< Y
    """

    def less_than_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Propagate X #< Y by adjusting domain bounds."""
        x_val, x_dom = normalize_arg(store, x_id)
        y_val, y_dom = normalize_arg(store, y_id)

        changed: List[int] = []

        # Determine current bounds (treat bound vars as singleton domains)
        x_min = x_dom.min() if x_dom is not None else x_val
        y_max = y_dom.max() if y_dom is not None else y_val

        # Inconsistency: minimum possible X >= maximum possible Y
        if x_min is not None and y_max is not None and x_min >= y_max:
            return ("fail", None)

        # Narrow X: X must be <= (max(Y) - 1)
        if y_max is not None:
            bound = y_max - 1
            if x_dom is not None:
                new_x_dom = x_dom.remove_gt(bound)
                if new_x_dom.is_empty():
                    return ("fail", None)
                if new_x_dom is not x_dom:
                    set_domain(store, x_id, new_x_dom, trail)
                    changed.append(x_id)
                    x_dom = new_x_dom
            elif x_val is not None and x_val > bound:
                return ("fail", None)

        # Narrow Y: Y must be >= (min(X) + 1)
        if x_min is not None:
            lower = x_min + 1
            if y_dom is not None:
                new_y_dom = y_dom.remove_lt(lower)
                if new_y_dom.is_empty():
                    return ("fail", None)
                if new_y_dom is not y_dom:
                    set_domain(store, y_id, new_y_dom, trail)
                    changed.append(y_id)
            elif y_val is not None and y_val < lower:
                return ("fail", None)

        return ("ok", changed if changed else None)

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

    def less_equal_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Propagate X #=< Y by adjusting domain bounds."""
        x_val, x_dom = normalize_arg(store, x_id)
        y_val, y_dom = normalize_arg(store, y_id)

        changed: List[int] = []

        x_min = x_dom.min() if x_dom is not None else x_val
        y_max = y_dom.max() if y_dom is not None else y_val

        # Inconsistency: smallest X greater than largest Y
        if x_min is not None and y_max is not None and x_min > y_max:
            return ("fail", None)

        # Narrow X: X must be <= max(Y)
        if y_max is not None:
            if x_dom is not None:
                new_x_dom = x_dom.remove_gt(y_max)
                if new_x_dom.is_empty():
                    return ("fail", None)
                if new_x_dom is not x_dom:
                    set_domain(store, x_id, new_x_dom, trail)
                    changed.append(x_id)
                    x_dom = new_x_dom
            elif x_val is not None and x_val > y_max:
                return ("fail", None)

        # Narrow Y: Y must be >= min(X)
        if x_min is not None:
            if y_dom is not None:
                new_y_dom = y_dom.remove_lt(x_min)
                if new_y_dom.is_empty():
                    return ("fail", None)
                if new_y_dom is not y_dom:
                    set_domain(store, y_id, new_y_dom, trail)
                    changed.append(y_id)
            elif y_val is not None and y_val < x_min:
                return ("fail", None)

        return ("ok", changed if changed else None)

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
