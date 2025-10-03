"""Boolean domain utilities for CLP(FD) reification.

Provides utilities for working with Boolean (0/1) domains and detecting
Boolean variables. This module creates the foundational infrastructure for
representing constraint truth values as Boolean CLP(FD) variables.
"""

from typing import Optional

from prolog.clpfd.domain import Domain
from prolog.clpfd.api import get_domain, set_domain
from prolog.ast.terms import Int

# Standard Boolean domain
BOOL_DOMAIN = Domain(((0, 1),))


def is_boolean_domain(domain: Optional[Domain]) -> bool:
    """Check if domain represents Boolean values (subset of {0,1}).

    Args:
        domain: Domain object to check (or None)

    Returns:
        True if domain is a non-empty subset of {0, 1}, False otherwise
    """
    if domain is None or domain.is_empty():
        return False

    # Check if all values are in {0, 1}
    for low, high in domain.intervals:
        if low < 0 or high > 1:
            return False

    return True


def ensure_boolean_var(store, var_id: int, trail) -> bool:
    """Ensure variable has Boolean domain, narrowing if needed.

    Constrains the variable to have a Boolean domain by intersecting its
    current domain with {0, 1}. If the variable has no domain, sets it
    to the full Boolean domain.

    Args:
        store: Variable store
        var_id: Variable ID to constrain
        trail: Trail for backtracking

    Returns:
        True if successfully constrained to Boolean domain,
        False if domain becomes empty (incompatible with Boolean)
    """
    current = get_domain(store, var_id)
    if current is None:
        # No domain yet - set to Boolean
        set_domain(store, var_id, BOOL_DOMAIN, trail)
        return True

    # Intersect with Boolean domain
    new_domain = current.intersect(BOOL_DOMAIN)
    if new_domain.is_empty():
        return False  # Not compatible with Boolean

    if new_domain != current:
        set_domain(store, var_id, new_domain, trail)

    return True


def get_boolean_value(store, var_id: int) -> Optional[int]:
    """Get Boolean value if determined, None if unknown.

    Checks if a variable is bound to a Boolean value or has a
    singleton Boolean domain.

    Args:
        store: Variable store
        var_id: Variable ID to check

    Returns:
        1 if variable is bound/constrained to 1
        0 if variable is bound/constrained to 0
        None if variable is not yet determined
    """
    deref = store.deref(var_id)
    if deref[0] == "BOUND":
        # Variable is ground
        if isinstance(deref[2], Int):
            val = deref[2].value
            if val in (0, 1):
                return val
    else:
        # Check domain
        domain = get_domain(store, deref[1])
        if domain and domain.is_singleton():
            val = domain.min()
            if val in (0, 1):
                return val

    return None  # Unknown
