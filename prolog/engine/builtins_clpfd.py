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
    # Fail immediately on empty domain
    if domain.is_empty():
        return False

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


def _post_constraint_propagator(engine, x_term, y_term, create_propagator_func, priority=None):
    """Post a binary constraint propagator.

    Args:
        engine: Engine instance
        x_term: First term (variable or int)
        y_term: Second term (variable or int)
        create_propagator_func: Function to create the propagator
        priority: Priority level (defaults to HIGH for equality, MED for others)

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

    # Determine priority if not specified
    if priority is None:
        # Use HIGH for equality, MED for inequalities
        from prolog.clpfd.props.equality import create_equality_propagator
        if create_propagator_func.__name__ == 'create_equality_propagator':
            priority = Priority.HIGH
        else:
            priority = Priority.MED

    # Handle different cases
    if x_var is not None and y_var is not None:
        # Both variables: create and register propagator
        prop = create_propagator_func(x_var, y_var)
        pid = queue.register(prop)

        # Add as watcher for both variables
        add_watcher(store, x_var, pid, priority, trail)
        add_watcher(store, y_var, pid, priority, trail)

        # Schedule initial propagation
        queue.schedule(pid, priority)

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

            # Wake watchers and propagate
            queue = _ensure_queue(engine)
            from prolog.clpfd.api import iter_watchers
            for pid, priority in iter_watchers(store, x_var):
                queue.schedule(pid, priority)
            if not queue.is_empty():
                queue.run_to_fixpoint(store, trail, engine)

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

                # Wake watchers and propagate
                queue = _ensure_queue(engine)
                from prolog.clpfd.api import iter_watchers
                for pid, priority in iter_watchers(store, x_var):
                    queue.schedule(pid, priority)
                if not queue.is_empty():
                    queue.run_to_fixpoint(store, trail, engine)

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

                # Wake watchers and propagate
                queue = _ensure_queue(engine)
                from prolog.clpfd.api import iter_watchers
                for pid, priority in iter_watchers(store, y_var):
                    queue.schedule(pid, priority)
                if not queue.is_empty():
                    queue.run_to_fixpoint(store, trail, engine)

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

                # Wake watchers and propagate
                queue = _ensure_queue(engine)
                from prolog.clpfd.api import iter_watchers
                for pid, priority in iter_watchers(store, x_var):
                    queue.schedule(pid, priority)
                if not queue.is_empty():
                    queue.run_to_fixpoint(store, trail, engine)

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

                # Wake watchers and propagate
                queue = _ensure_queue(engine)
                from prolog.clpfd.api import iter_watchers
                for pid, priority in iter_watchers(store, y_var):
                    queue.schedule(pid, priority)
                if not queue.is_empty():
                    queue.run_to_fixpoint(store, trail, engine)

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


def _builtin_fd_var(engine, term):
    """Check if term is a finite domain variable.

    Args:
        engine: Engine instance
        term: Term to check

    Returns:
        True if term is an FD variable, False otherwise
    """
    if not isinstance(term, Var):
        return False

    # Dereference the variable
    deref = engine.store.deref(term.id)

    if deref[0] == "BOUND":
        # Bound variables are not FD variables
        return False

    # Check if it has a domain
    varid = deref[1]
    domain = get_domain(engine.store, varid)
    return domain is not None


def _builtin_fd_inf(engine, x_term, inf_term):
    """Get the infimum (minimum) of an FD variable's domain.

    Args:
        engine: Engine instance
        x_term: FD variable
        inf_term: Term to unify with the minimum

    Returns:
        True if unification succeeds, False otherwise
    """
    if not isinstance(x_term, Var):
        return False

    # Dereference the variable
    deref = engine.store.deref(x_term.id)

    if deref[0] == "BOUND":
        # Bound variable - its value is both min and max
        value = deref[2]
        if isinstance(value, Int):
            from prolog.unify.unify import unify
            return unify(inf_term, value, engine.store, engine.trail, engine.occurs_check)
        return False

    # Get domain
    varid = deref[1]
    domain = get_domain(engine.store, varid)

    if domain is None:
        return False  # Not an FD variable

    min_val = domain.min()
    if min_val is None:
        return False  # Empty domain

    from prolog.unify.unify import unify
    return unify(inf_term, Int(min_val), engine.store, engine.trail, engine.occurs_check)


def _builtin_fd_sup(engine, x_term, sup_term):
    """Get the supremum (maximum) of an FD variable's domain.

    Args:
        engine: Engine instance
        x_term: FD variable
        sup_term: Term to unify with the maximum

    Returns:
        True if unification succeeds, False otherwise
    """
    if not isinstance(x_term, Var):
        return False

    # Dereference the variable
    deref = engine.store.deref(x_term.id)

    if deref[0] == "BOUND":
        # Bound variable - its value is both min and max
        value = deref[2]
        if isinstance(value, Int):
            from prolog.unify.unify import unify
            return unify(sup_term, value, engine.store, engine.trail, engine.occurs_check)
        return False

    # Get domain
    varid = deref[1]
    domain = get_domain(engine.store, varid)

    if domain is None:
        return False  # Not an FD variable

    max_val = domain.max()
    if max_val is None:
        return False  # Empty domain

    from prolog.unify.unify import unify
    return unify(sup_term, Int(max_val), engine.store, engine.trail, engine.occurs_check)


def _builtin_fd_dom(engine, x_term, dom_term):
    """Get the domain of an FD variable as a term.

    The domain is represented as a term that can be used with 'in'/2.
    For a simple interval, it's Low..High.
    For multiple intervals or holes, it uses \\/ (union).

    Args:
        engine: Engine instance
        x_term: FD variable
        dom_term: Term to unify with the domain representation

    Returns:
        True if unification succeeds, False otherwise
    """
    if not isinstance(x_term, Var):
        return False

    # Dereference the variable
    deref = engine.store.deref(x_term.id)

    if deref[0] == "BOUND":
        # Bound variable - its domain is a singleton
        value = deref[2]
        if isinstance(value, Int):
            from prolog.unify.unify import unify
            return unify(dom_term, value, engine.store, engine.trail, engine.occurs_check)
        return False

    # Get domain
    varid = deref[1]
    domain = get_domain(engine.store, varid)

    if domain is None:
        return False  # Not an FD variable

    # Convert domain to term representation
    domain_term = domain_to_term(domain)

    from prolog.unify.unify import unify
    return unify(dom_term, domain_term, engine.store, engine.trail, engine.occurs_check)


def domain_to_term(domain):
    """Convert a Domain object to a Prolog term representation.

    Args:
        domain: Domain object

    Returns:
        Term representing the domain
    """
    if domain.is_empty():
        # Empty domain - could use a special representation
        # For now, we'll use an empty interval
        return Struct("..", (Int(1), Int(0)))

    intervals = domain.intervals

    if len(intervals) == 1:
        # Single interval
        low, high = intervals[0]
        if low == high:
            # Singleton
            return Int(low)
        else:
            # Range
            return Struct("..", (Int(low), Int(high)))

    # Multiple intervals - build union
    def build_union(intervals):
        if len(intervals) == 1:
            low, high = intervals[0]
            if low == high:
                return Int(low)
            else:
                return Struct("..", (Int(low), Int(high)))
        else:
            # Recursively build union
            first = intervals[0]
            rest = intervals[1:]
            low, high = first
            if low == high:
                first_term = Int(low)
            else:
                first_term = Struct("..", (Int(low), Int(high)))
            rest_term = build_union(rest)
            return Struct("\\/", (first_term, rest_term))

    return build_union(intervals)
