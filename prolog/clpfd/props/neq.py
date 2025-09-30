r"""Not-equal propagator for CLP(FD).

Implements X #\= Y propagation:
- If one side is a singleton {v}, remove v from the other's domain.
- If both sides are singletons with equal value, fail.
"""

from typing import Optional, List, Tuple
from prolog.clpfd.api import get_domain, set_domain


def create_not_equal_propagator(x_id: int, y_id: int):
    r"""Create a not-equal propagator for X #\= Y.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #\= Y
    """

    def neq_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        r"""Propagate X #\= Y by removing singleton values from counterpart.

        Returns:
            ('ok', changed_vars) if propagation succeeded
            ('fail', None) if inconsistency detected
        """
        x_dom = get_domain(store, x_id)
        y_dom = get_domain(store, y_id)

        # If no domains, nothing to propagate yet
        if x_dom is None and y_dom is None:
            return ("ok", None)

        changed: List[int] = []

        # If both have domains and both singletons
        if (
            x_dom is not None
            and y_dom is not None
            and x_dom.is_singleton()
            and y_dom.is_singleton()
        ):
            if x_dom.min() == y_dom.min():  # same value
                return ("fail", None)
            return ("ok", None)

        # If X is singleton, remove its value from Y's domain
        if x_dom is not None and x_dom.is_singleton() and y_dom is not None:
            val = x_dom.min()
            new_y = y_dom.remove_value(val)
            if new_y.is_empty():
                return ("fail", None)
            if new_y is not y_dom:
                set_domain(store, y_id, new_y, trail)
                changed.append(y_id)

        # If Y is singleton, remove its value from X's domain
        if y_dom is not None and y_dom.is_singleton() and x_dom is not None:
            val = y_dom.min()
            new_x = x_dom.remove_value(val)
            if new_x.is_empty():
                return ("fail", None)
            if new_x is not x_dom:
                set_domain(store, x_id, new_x, trail)
                changed.append(x_id)

        return ("ok", changed if changed else None)

    return neq_propagator
