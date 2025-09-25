"""CLP(FD) builtins for constraint posting.

Implements domain posting and constraint builtins that integrate
with the engine via attributed variables.
"""

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


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