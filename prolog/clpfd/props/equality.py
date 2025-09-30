"""Equality propagator for CLP(FD).

Implements X #= Y constraint propagation with domain intersection.
"""

from typing import Optional, List, Tuple
from prolog.clpfd.api import set_domain
from prolog.clpfd.domain import Domain
from prolog.clpfd.entailment import normalize_arg
from prolog.clpfd.safe_bind import safe_bind_singleton


def create_equality_propagator(x_id: int, y_id: int):
    """Create an equality propagator for X #= Y.

    Args:
        x_id: Variable ID for X
        y_id: Variable ID for Y

    Returns:
        Propagator function that enforces X #= Y
    """

    def equality_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Propagate X #= Y by intersecting domains.

        Returns:
            ('ok', changed_vars) if propagation succeeded
            ('fail', None) if domains become empty
        """
        # Normalize arguments to value/domain pairs
        x_val, x_dom = normalize_arg(store, x_id)
        y_val, y_dom = normalize_arg(store, y_id)

        changed: List[int] = []

        # Both bound to concrete integers
        if x_val is not None and y_val is not None:
            return ("ok", None) if x_val == y_val else ("fail", None)

        # X bound, Y has domain
        if x_val is not None and y_dom is not None:
            target = Domain(((x_val, x_val),))
            new_y_dom = y_dom.intersect(target)
            if new_y_dom.is_empty():
                return ("fail", None)
            if new_y_dom is not y_dom:
                set_domain(store, y_id, new_y_dom, trail)
                changed.append(y_id)
                # If domain became singleton, safely bind the variable
                if new_y_dom.is_singleton():
                    if not safe_bind_singleton(y_id, new_y_dom.min(), trail, store):
                        return ("fail", None)
            return ("ok", changed if changed else None)

        # Y bound, X has domain
        if y_val is not None and x_dom is not None:
            target = Domain(((y_val, y_val),))
            new_x_dom = x_dom.intersect(target)
            if new_x_dom.is_empty():
                return ("fail", None)
            if new_x_dom is not x_dom:
                set_domain(store, x_id, new_x_dom, trail)
                changed.append(x_id)
                # If domain became singleton, safely bind the variable
                if new_x_dom.is_singleton():
                    if not safe_bind_singleton(x_id, new_x_dom.min(), trail, store):
                        return ("fail", None)
            return ("ok", changed if changed else None)

        # Both have domains (unbound roots)
        if x_dom is not None and y_dom is not None:
            new_x_dom = x_dom.intersect(y_dom)
            new_y_dom = y_dom.intersect(x_dom)

            if new_x_dom.is_empty() or new_y_dom.is_empty():
                return ("fail", None)

            if new_x_dom is not x_dom:
                set_domain(store, x_id, new_x_dom, trail)
                changed.append(x_id)
                # If domain became singleton, safely bind the variable
                if new_x_dom.is_singleton():
                    if not safe_bind_singleton(x_id, new_x_dom.min(), trail, store):
                        return ("fail", None)

            if new_y_dom is not y_dom:
                set_domain(store, y_id, new_y_dom, trail)
                changed.append(y_id)
                # If domain became singleton, safely bind the variable
                if new_y_dom.is_singleton():
                    if not safe_bind_singleton(y_id, new_y_dom.min(), trail, store):
                        return ("fail", None)

            return ("ok", changed if changed else None)

        # If one side has no domain yet (and is unbound), defer propagation
        return ("ok", None)

    return equality_propagator
