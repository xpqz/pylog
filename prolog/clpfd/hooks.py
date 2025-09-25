"""CLP(FD) unification hooks.

Handles domain checking and merging during variable unification.
"""

from prolog.ast.terms import Int, Var
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.queue import PropagationQueue


def clpfd_unify_hook(engine, varid, other):
    """Handle CLP(FD) constraints during unification.

    This hook is called when a variable with CLP(FD) attributes is unified
    with another term. It ensures domain consistency and triggers propagation.

    Args:
        engine: Engine instance
        varid: Variable ID being unified (already dereferenced to root)
        other: Term being unified with

    Returns:
        True to allow unification, False to block it
    """
    # Get the domain of the variable
    var_domain = get_domain(engine.store, varid)

    if var_domain is None:
        # Variable has no domain constraints, allow unification
        return True

    # Handle different cases based on what we're unifying with
    if isinstance(other, Int):
        # Unifying with an integer - check if it's in the domain
        if not var_domain.contains(other.value):
            return False  # Value outside domain, fail unification

        # Value is in domain, unification can proceed
        # The actual binding will be done by the unification algorithm
        return True

    elif isinstance(other, Var):
        # Unifying with another variable
        other_deref = engine.store.deref(other.id)

        if other_deref[0] == "BOUND":
            # Other variable is bound to a value
            bound_value = other_deref[2]
            if isinstance(bound_value, Int):
                # Check if the value is in our domain
                return var_domain.contains(bound_value.value)
            else:
                # Bound to non-integer, FD variable can't unify with it
                return False
        else:
            # Other variable is unbound
            other_var = other_deref[1]
            other_domain = get_domain(engine.store, other_var)

            if other_domain is not None:
                # Both variables have domains - intersect them
                new_domain = var_domain.intersect(other_domain)

                if new_domain.is_empty():
                    # Domains are disjoint, unification fails
                    return False

                # Set the intersected domain on both variables
                # (The unification will make them aliases)
                set_domain(engine.store, varid, new_domain, engine.trail)

                # If there's a propagation queue, schedule propagation
                if hasattr(engine, 'clpfd_queue'):
                    # Wake up watchers for domain change
                    from prolog.clpfd.api import iter_watchers
                    from prolog.clpfd.queue import Priority

                    for pid, priority in iter_watchers(engine.store, varid):
                        engine.clpfd_queue.schedule(pid, priority)

                    # Run propagation to fixpoint
                    engine.clpfd_queue.run_to_fixpoint(engine.store, engine.trail, engine)
            else:
                # Other variable has no domain, it will inherit ours through aliasing
                pass

            return True

    else:
        # Unifying with a non-integer, non-variable term
        # FD variables can only be integers
        return False