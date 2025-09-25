"""CLP(FD) builtins for constraint posting.

Implements domain posting and constraint builtins that integrate
with the engine via attributed variables.
"""

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.clpfd.api import get_domain, set_domain, add_watcher
from prolog.clpfd.domain import Domain
from prolog.clpfd.queue import PropagationQueue, Priority


def _builtin_in(engine, x_term, domain_term):
    """X in Domain - set domain for variable X.

    Args:
        engine: Engine instance
        x_term: Variable or value to constrain
        domain_term: Domain specification term

    Returns:
        True if domain was set/checked successfully, False otherwise
    """
    # Handle bound values
    if not isinstance(x_term, Var):
        # Check if value is in domain
        if isinstance(x_term, Int):
            domain = parse_domain_term(domain_term)
            return domain.contains(x_term.value)
        return False  # Non-integer can't be in finite domain

    # Deref X
    x_deref = engine.store.deref(x_term.id)

    if x_deref[0] == "BOUND":
        # Check if bound value is in domain
        value = x_deref[2]
        if isinstance(value, Int):
            domain = parse_domain_term(domain_term)
            return domain.contains(value.value)
        return False  # Non-integer can't be in finite domain

    # X is unbound
    x_var = x_deref[1]

    # Parse domain term
    domain = parse_domain_term(domain_term)

    # Check if variable already has a domain
    existing_domain = get_domain(engine.store, x_var)
    if existing_domain:
        # Intersect with existing domain
        domain = existing_domain.intersect(domain)
        if domain.is_empty():
            return False  # Inconsistent domains

    # Set domain with trailing
    set_domain(engine.store, x_var, domain, engine.trail)

    # Register unification hook if first CLP(FD) constraint
    if not hasattr(engine, '_clpfd_inited') or not engine._clpfd_inited:
        from prolog.clpfd.hooks import clpfd_unify_hook
        engine.register_attr_hook('clpfd', clpfd_unify_hook)
        engine._clpfd_inited = True

    return True


def parse_domain_term(term):
    """Parse domain specification into Domain object.

    Args:
        term: AST term representing domain

    Returns:
        Domain object

    Raises:
        ValueError: If term is not a valid domain specification
    """
    if isinstance(term, Int):
        # Singleton domain
        return Domain(((term.value, term.value),))

    elif isinstance(term, Struct):
        if term.functor == ".." and len(term.args) == 2:
            # Interval: low..high
            if isinstance(term.args[0], Int) and isinstance(term.args[1], Int):
                low, high = term.args[0].value, term.args[1].value
                if low <= high:
                    return Domain(((low, high),))
                return Domain(())  # Empty if low > high
            else:
                raise ValueError(f"Invalid interval bounds: {term.args}")

        elif term.functor == "\\/" and len(term.args) == 2:
            # Union of domains
            d1 = parse_domain_term(term.args[0])
            d2 = parse_domain_term(term.args[1])
            # Combine intervals (Domain constructor will normalize)
            return Domain(d1.intervals + d2.intervals)

        elif term.functor == "{}":
            # Enumerated set: {1,3,5}
            intervals = []
            for arg in term.args:
                if isinstance(arg, Int):
                    intervals.append((arg.value, arg.value))
                else:
                    raise ValueError(f"Non-integer in set: {arg}")
            return Domain(tuple(intervals))

    raise ValueError(f"Invalid domain term: {term}")


def _ensure_queue(engine):
    """Ensure engine has a CLP(FD) propagation queue."""
    if not hasattr(engine, 'clpfd_queue'):
        engine.clpfd_queue = PropagationQueue()
    return engine.clpfd_queue


def _post_constraint_propagator(engine, x_term, y_term, create_propagator_func):
    """Post a binary constraint propagator.

    Args:
        engine: Engine instance
        x_term: First term (variable or int)
        y_term: Second term (variable or int)
        create_propagator_func: Function to create the propagator

    Returns:
        True if constraint posted successfully, False if failed immediately
    """
    store = engine.store
    trail = engine.trail
    queue = _ensure_queue(engine)

    # Deref both terms
    x_deref = store.deref(x_term.id) if isinstance(x_term, Var) else None
    y_deref = store.deref(y_term.id) if isinstance(y_term, Var) else None

    # Get variable IDs and values
    x_var = None
    x_val = None
    y_var = None
    y_val = None

    if isinstance(x_term, Int):
        x_val = x_term.value
    elif x_deref:
        if x_deref[0] == "BOUND":
            if isinstance(x_deref[2], Int):
                x_val = x_deref[2].value
            else:
                return False  # Non-integer
        else:
            x_var = x_deref[1]

    if isinstance(y_term, Int):
        y_val = y_term.value
    elif y_deref:
        if y_deref[0] == "BOUND":
            if isinstance(y_deref[2], Int):
                y_val = y_deref[2].value
            else:
                return False  # Non-integer
        else:
            y_var = y_deref[1]

    # Handle different cases
    if x_var is not None and y_var is not None:
        # Both variables: create and register propagator
        prop = create_propagator_func(x_var, y_var)
        pid = queue.register(prop)

        # Add as watcher for both variables
        add_watcher(store, x_var, pid, Priority.HIGH, trail)
        add_watcher(store, y_var, pid, Priority.HIGH, trail)

        # Schedule initial propagation
        queue.schedule(pid, Priority.HIGH)

        # Run to fixpoint
        return queue.run_to_fixpoint(store, trail, engine)

    elif x_var is not None and y_val is not None:
        # X is variable, Y is value: narrow X's domain
        x_dom = get_domain(store, x_var)
        if x_dom is None:
            # No domain yet, create full domain
            x_dom = Domain(((-(2**31), 2**31-1),))

        # Apply constraint based on propagator type
        # We'll handle this in specific builtins
        return True

    elif x_val is not None and y_var is not None:
        # X is value, Y is variable: narrow Y's domain
        y_dom = get_domain(store, y_var)
        if y_dom is None:
            # No domain yet, create full domain
            y_dom = Domain(((-(2**31), 2**31-1),))

        # Apply constraint based on propagator type
        # We'll handle this in specific builtins
        return True

    else:
        # Both are values: check immediately
        # We'll handle this in specific builtins
        return True


def _builtin_fd_eq(engine, x_term, y_term):
    """X #= Y - constrain X and Y to be equal.

    Args:
        engine: Engine instance
        x_term: First term
        y_term: Second term

    Returns:
        True if constraint succeeded, False if failed
    """
    store = engine.store
    trail = engine.trail

    # Handle int-int case
    if isinstance(x_term, Int) and isinstance(y_term, Int):
        return x_term.value == y_term.value

    # Handle var-int and int-var cases specially for equality
    if isinstance(x_term, Var) and isinstance(y_term, Int):
        x_deref = store.deref(x_term.id)
        if x_deref[0] == "BOUND":
            return isinstance(x_deref[2], Int) and x_deref[2].value == y_term.value
        else:
            # Set domain to singleton
            x_var = x_deref[1]
            new_dom = Domain(((y_term.value, y_term.value),))
            existing = get_domain(store, x_var)
            if existing:
                new_dom = existing.intersect(new_dom)
                if new_dom.is_empty():
                    return False
            set_domain(store, x_var, new_dom, trail)
            return True

    if isinstance(x_term, Int) and isinstance(y_term, Var):
        return _builtin_fd_eq(engine, y_term, x_term)

    # Var-var case
    from prolog.clpfd.props.equality import create_equality_propagator
    return _post_constraint_propagator(engine, x_term, y_term, create_equality_propagator)


def _builtin_fd_lt(engine, x_term, y_term):
    """X #< Y - constrain X to be less than Y.

    Args:
        engine: Engine instance
        x_term: First term
        y_term: Second term

    Returns:
        True if constraint succeeded, False if failed
    """
    store = engine.store
    trail = engine.trail

    # Handle int-int case
    if isinstance(x_term, Int) and isinstance(y_term, Int):
        return x_term.value < y_term.value

    # Handle var-int case: X #< val
    if isinstance(x_term, Var) and isinstance(y_term, Int):
        x_deref = store.deref(x_term.id)
        if x_deref[0] == "BOUND":
            return isinstance(x_deref[2], Int) and x_deref[2].value < y_term.value
        else:
            # Remove values >= y_term.value
            x_var = x_deref[1]
            x_dom = get_domain(store, x_var)
            if x_dom is None:
                x_dom = Domain(((-(2**31), 2**31-1),))
            new_dom = x_dom.remove_ge(y_term.value)
            if new_dom.is_empty():
                return False
            if new_dom is not x_dom:
                set_domain(store, x_var, new_dom, trail)
            return True

    # Handle int-var case: val #< Y
    if isinstance(x_term, Int) and isinstance(y_term, Var):
        y_deref = store.deref(y_term.id)
        if y_deref[0] == "BOUND":
            return isinstance(y_deref[2], Int) and x_term.value < y_deref[2].value
        else:
            # Remove values <= x_term.value
            y_var = y_deref[1]
            y_dom = get_domain(store, y_var)
            if y_dom is None:
                y_dom = Domain(((-(2**31), 2**31-1),))
            new_dom = y_dom.remove_le(x_term.value)
            if new_dom.is_empty():
                return False
            if new_dom is not y_dom:
                set_domain(store, y_var, new_dom, trail)
            return True

    # Var-var case
    from prolog.clpfd.props.comparison import create_less_than_propagator
    return _post_constraint_propagator(engine, x_term, y_term, create_less_than_propagator)


def _builtin_fd_le(engine, x_term, y_term):
    """X #=< Y - constrain X to be less than or equal to Y.

    Args:
        engine: Engine instance
        x_term: First term
        y_term: Second term

    Returns:
        True if constraint succeeded, False if failed
    """
    store = engine.store
    trail = engine.trail

    # Handle int-int case
    if isinstance(x_term, Int) and isinstance(y_term, Int):
        return x_term.value <= y_term.value

    # Handle var-int case: X #=< val
    if isinstance(x_term, Var) and isinstance(y_term, Int):
        x_deref = store.deref(x_term.id)
        if x_deref[0] == "BOUND":
            return isinstance(x_deref[2], Int) and x_deref[2].value <= y_term.value
        else:
            # Remove values > y_term.value
            x_var = x_deref[1]
            x_dom = get_domain(store, x_var)
            if x_dom is None:
                x_dom = Domain(((-(2**31), 2**31-1),))
            new_dom = x_dom.remove_gt(y_term.value)
            if new_dom.is_empty():
                return False
            if new_dom is not x_dom:
                set_domain(store, x_var, new_dom, trail)
            return True

    # Handle int-var case: val #=< Y
    if isinstance(x_term, Int) and isinstance(y_term, Var):
        y_deref = store.deref(y_term.id)
        if y_deref[0] == "BOUND":
            return isinstance(y_deref[2], Int) and x_term.value <= y_deref[2].value
        else:
            # Remove values < x_term.value
            y_var = y_deref[1]
            y_dom = get_domain(store, y_var)
            if y_dom is None:
                y_dom = Domain(((-(2**31), 2**31-1),))
            new_dom = y_dom.remove_lt(x_term.value)
            if new_dom.is_empty():
                return False
            if new_dom is not y_dom:
                set_domain(store, y_var, new_dom, trail)
            return True

    # Var-var case
    from prolog.clpfd.props.comparison import create_less_equal_propagator
    return _post_constraint_propagator(engine, x_term, y_term, create_less_equal_propagator)


def _builtin_fd_gt(engine, x_term, y_term):
    """X #> Y - constrain X to be greater than Y.

    This is equivalent to Y #< X.

    Args:
        engine: Engine instance
        x_term: First term
        y_term: Second term

    Returns:
        True if constraint succeeded, False if failed
    """
    return _builtin_fd_lt(engine, y_term, x_term)


def _builtin_fd_ge(engine, x_term, y_term):
    """X #>= Y - constrain X to be greater than or equal to Y.

    This is equivalent to Y #=< X.

    Args:
        engine: Engine instance
        x_term: First term
        y_term: Second term

    Returns:
        True if constraint succeeded, False if failed
    """
    return _builtin_fd_le(engine, y_term, x_term)