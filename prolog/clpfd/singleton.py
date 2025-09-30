"""Helper to bind variables with singleton domains."""

from prolog.ast.terms import Int
from prolog.unify.unify_helpers import bind_root_to_term
from prolog.clpfd.api import get_domain


def bind_singleton_domains(store, trail, var_ids):
    """Bind variables that have singleton domains.

    This is called after propagation to ensure variables with
    determined values are actually bound to those values.

    Args:
        store: Variable store
        trail: Trail for backtracking
        var_ids: List of variable IDs that may have singleton domains

    Returns:
        List of variable IDs that were bound
    """
    bound = []

    for var_id in var_ids:
        # Check if already bound
        deref = store.deref(var_id)
        if deref[0] == "BOUND":
            continue

        # Check if has singleton domain
        dom = get_domain(store, var_id)
        if dom and dom.is_singleton():
            # Bind to the single value
            bind_root_to_term(deref[1], Int(dom.min()), trail, store)
            bound.append(var_id)

    return bound
