"""Equality propagator for CLP(FD).

Implements X #= Y constraint propagation with domain intersection.
"""

from typing import Optional, List, Tuple, Any
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


def create_equality_propagator(x_id: int, y_id: int):
    """Create an equality propagator for X #= Y.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #= Y
    """
    def equality_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate X #= Y by intersecting domains.

        Returns:
            ('ok', changed_vars) if propagation succeeded
            ('fail', None) if domains become empty
        """
        # Get current domains
        x_dom = get_domain(store, x_id)
        y_dom = get_domain(store, y_id)

        # If either has no domain, nothing to propagate
        if x_dom is None or y_dom is None:
            return ('ok', None)

        # Intersect domains
        new_x_dom = x_dom.intersect(y_dom)
        new_y_dom = y_dom.intersect(x_dom)

        # Check for failure
        if new_x_dom.is_empty() or new_y_dom.is_empty():
            return ('fail', None)

        # Track changed variables
        changed = []

        # Update X if changed
        if new_x_dom is not x_dom:
            set_domain(store, x_id, new_x_dom, trail)
            changed.append(x_id)

        # Update Y if changed
        if new_y_dom is not y_dom:
            set_domain(store, y_id, new_y_dom, trail)
            changed.append(y_id)

        return ('ok', changed if changed else None)

    return equality_propagator