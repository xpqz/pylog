"""CLP(FD) unification hooks.

Handles domain checking and merging during variable unification.
"""

from prolog.ast.terms import Int, Var
from prolog.clpfd.api import get_domain, set_domain, get_fd_attrs, iter_watchers
from prolog.clpfd.queue import Priority


def _ensure_queue(engine):
    """Ensure engine has a CLP(FD) propagation queue."""
    if not hasattr(engine, 'clpfd_queue'):
        from prolog.clpfd.queue import PropagationQueue
        engine.clpfd_queue = PropagationQueue()
    return engine.clpfd_queue


def _wake_watchers_and_propagate(engine, varid):
    """Wake all watchers for a variable and run propagation to fixpoint."""
    queue = _ensure_queue(engine)

    # Schedule all watchers
    for pid, priority in iter_watchers(engine.store, varid):
        queue.schedule(pid, priority)

    # Run to fixpoint
    if not queue.is_empty():
        queue.run_to_fixpoint(engine.store, engine.trail, engine)


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

        # Value is in domain - narrow to singleton and propagate
        from prolog.clpfd.domain import Domain
        singleton_domain = Domain(((other.value, other.value),))
        set_domain(engine.store, varid, singleton_domain, engine.trail)

        # Wake watchers and propagate since domain changed
        _wake_watchers_and_propagate(engine, varid)

        return True

    elif isinstance(other, Var):
        # Unifying with another variable
        other_deref = engine.store.deref(other.id)

        if other_deref[0] == "BOUND":
            # Other variable is bound to a value
            bound_value = other_deref[2]
            if isinstance(bound_value, Int):
                # Check if the value is in our domain
                if not var_domain.contains(bound_value.value):
                    return False

                # Narrow to singleton and propagate
                from prolog.clpfd.domain import Domain
                singleton_domain = Domain(((bound_value.value, bound_value.value),))
                set_domain(engine.store, varid, singleton_domain, engine.trail)
                _wake_watchers_and_propagate(engine, varid)
                return True
            else:
                # Bound to non-integer, FD variable can't unify with it
                return False
        else:
            # Other variable is unbound
            other_var = other_deref[1]
            other_domain = get_domain(engine.store, other_var)

            if other_domain is not None:
                # Both variables have domains - intersect them and merge watchers
                new_domain = var_domain.intersect(other_domain)

                if new_domain.is_empty():
                    # Domains are disjoint, unification fails
                    return False

                # Merge watchers from both variables
                var_attrs = get_fd_attrs(engine.store, varid) or {}
                other_attrs = get_fd_attrs(engine.store, other_var) or {}

                # Collect all watchers from both variables
                merged_watchers = {}
                for priority in Priority:
                    var_watchers = var_attrs.get('watchers', {}).get(priority, set())
                    other_watchers = other_attrs.get('watchers', {}).get(priority, set())
                    merged_watchers[priority] = var_watchers.union(other_watchers)

                # Create merged attributes
                merged_attrs = {
                    'domain': new_domain,
                    'watchers': merged_watchers
                }

                # Set merged attributes on BOTH variables
                # This ensures watchers are preserved regardless of which becomes root
                engine.store.put_attr(varid, 'clpfd', merged_attrs, engine.trail)
                engine.store.put_attr(other_var, 'clpfd', merged_attrs, engine.trail)

                # Wake watchers for both variables and propagate
                _wake_watchers_and_propagate(engine, varid)
                _wake_watchers_and_propagate(engine, other_var)
            else:
                # Other variable has no domain, it will inherit ours through aliasing
                # Still need to propagate in case there are watchers
                _wake_watchers_and_propagate(engine, varid)

            return True

    else:
        # Unifying with a non-integer, non-variable term
        # FD variables can only be integers
        return False