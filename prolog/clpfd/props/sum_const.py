"""Sum-with-constant propagator: Z = X + K."""

from typing import Optional, List, Tuple
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


def _shift_domain(dom: Domain, delta: int) -> Domain:
    """Shift a domain by an integer delta (add to all values)."""
    if dom is None:
        return None  # type: ignore
    intervals = tuple((low + delta, high + delta) for (low, high) in dom.intervals)
    return Domain(intervals)


def create_sum_const_propagator(z_id: int, x_id: int, k: int):
    """Create a propagator enforcing Z = X + K.

    Args:
        z_id: Var ID for Z
        x_id: Var ID for X
        k: constant integer
    """

    def prop(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        z_dom = get_domain(store, z_id)
        x_dom = get_domain(store, x_id)

        if z_dom is None and x_dom is None:
            return ("ok", None)

        changed: List[int] = []

        # Narrow Z from X
        if x_dom is not None:
            z_from_x = _shift_domain(x_dom, k)
            if z_dom is not None:
                new_z = z_dom.intersect(z_from_x)
            else:
                new_z = z_from_x

            if new_z.is_empty():
                return ("fail", None)
            if z_dom is None or new_z is not z_dom:
                set_domain(store, z_id, new_z, trail)
                changed.append(z_id)
                z_dom = new_z

        # Narrow X from Z
        if z_dom is not None:
            x_from_z = _shift_domain(z_dom, -k)
            if x_dom is not None:
                new_x = x_dom.intersect(x_from_z)
            else:
                new_x = x_from_z

            if new_x.is_empty():
                return ("fail", None)
            if x_dom is None or new_x is not x_dom:
                set_domain(store, x_id, new_x, trail)
                changed.append(x_id)

        return ("ok", changed if changed else None)

    return prop
