"""Entailment detection for CLP(FD) constraints.

Provides functions to determine when constraints are definitely satisfied
(entailed), definitely violated (disentailed), or undetermined based on
current variable domains.
"""

from enum import Enum
from typing import Optional, Union, Tuple

from prolog.clpfd.api import get_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


class Entailment(Enum):
    """Entailment states for constraint satisfaction."""

    TRUE = 1  # Constraint is definitely satisfied
    FALSE = 0  # Constraint is definitely violated
    UNKNOWN = -1  # Constraint satisfaction is undetermined


# Type alias for constraint arguments
# Either a variable ID (int) or a constant (None, value)
ConstraintArg = Union[int, Tuple[None, int]]


def normalize_arg(
    store: "Store", arg: ConstraintArg
) -> Tuple[Optional[int], Optional[Domain]]:
    """Normalize a constraint argument to (value, domain) pair.

    Args:
        store: Variable store
        arg: Either var_id or (None, int_value) for constant

    Returns:
        Tuple of (constant_value, domain):
        - For constants: (value, None)
        - For bound vars: (bound_value, None)
        - For unbound vars with domain: (None, domain)
        - For unbound vars without domain: (None, None)
    """
    # Handle constant case
    if isinstance(arg, tuple) and arg[0] is None:
        return (arg[1], None)

    # Handle variable case
    varid = arg
    result = store.deref(varid)

    if result[0] == "BOUND":
        # Variable is bound - extract integer value if possible
        term = result[2]
        if isinstance(term, Int):
            return (term.value, None)
        else:
            # Non-integer bound value
            return (None, None)
    else:
        # Variable is unbound - get its domain
        dom = get_domain(store, varid)
        return (None, dom)


def check_equality_entailment(
    store: "Store", x_arg: ConstraintArg, y_arg: ConstraintArg
) -> Entailment:
    """Check if X #= Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the equality constraint
    """
    x_val, x_dom = normalize_arg(store, x_arg)
    y_val, y_dom = normalize_arg(store, y_arg)

    # Both are constants (or bound to constants)
    if x_val is not None and y_val is not None:
        return Entailment.TRUE if x_val == y_val else Entailment.FALSE

    # X is constant, Y has domain
    if x_val is not None and y_dom is not None:
        if y_dom.is_empty():
            return Entailment.FALSE
        if not y_dom.contains(x_val):
            return Entailment.FALSE
        if y_dom.is_singleton() and y_dom.min() == x_val:
            return Entailment.TRUE
        return Entailment.UNKNOWN

    # Y is constant, X has domain
    if y_val is not None and x_dom is not None:
        if x_dom.is_empty():
            return Entailment.FALSE
        if not x_dom.contains(y_val):
            return Entailment.FALSE
        if x_dom.is_singleton() and x_dom.min() == y_val:
            return Entailment.TRUE
        return Entailment.UNKNOWN

    # Both have domains
    if x_dom is not None and y_dom is not None:
        # Check for empty domains
        if x_dom.is_empty() or y_dom.is_empty():
            return Entailment.FALSE

        # Check domain intersection
        intersection = x_dom.intersect(y_dom)
        if intersection.is_empty():
            return Entailment.FALSE

        # Check for singleton domains
        if x_dom.is_singleton() and y_dom.is_singleton():
            if x_dom.min() == y_dom.min():
                return Entailment.TRUE
            else:
                return Entailment.FALSE

        # Special case: one singleton
        if x_dom.is_singleton():
            val = x_dom.min()
            if not y_dom.contains(val):
                return Entailment.FALSE
            if y_dom.is_singleton():
                return Entailment.TRUE
        elif y_dom.is_singleton():
            val = y_dom.min()
            if not x_dom.contains(val):
                return Entailment.FALSE

    return Entailment.UNKNOWN


def check_less_than_entailment(
    store, x_arg: ConstraintArg, y_arg: ConstraintArg
) -> Entailment:
    """Check if X #< Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the less-than constraint
    """
    x_val, x_dom = normalize_arg(store, x_arg)
    y_val, y_dom = normalize_arg(store, y_arg)

    # Both are constants
    if x_val is not None and y_val is not None:
        return Entailment.TRUE if x_val < y_val else Entailment.FALSE

    # X is constant, Y has domain
    if x_val is not None and y_dom is not None:
        if y_dom.is_empty():
            return Entailment.FALSE
        if x_val < y_dom.min():
            return Entailment.TRUE
        if x_val >= y_dom.max():
            return Entailment.FALSE
        return Entailment.UNKNOWN

    # Y is constant, X has domain
    if y_val is not None and x_dom is not None:
        if x_dom.is_empty():
            return Entailment.FALSE
        if x_dom.max() < y_val:
            return Entailment.TRUE
        if x_dom.min() >= y_val:
            return Entailment.FALSE
        return Entailment.UNKNOWN

    # Both have domains
    if x_dom is not None and y_dom is not None:
        if x_dom.is_empty() or y_dom.is_empty():
            return Entailment.FALSE
        if x_dom.max() < y_dom.min():
            return Entailment.TRUE
        if x_dom.min() >= y_dom.max():
            return Entailment.FALSE

    return Entailment.UNKNOWN


def check_less_equal_entailment(
    store, x_arg: ConstraintArg, y_arg: ConstraintArg
) -> Entailment:
    """Check if X #=< Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the less-or-equal constraint
    """
    x_val, x_dom = normalize_arg(store, x_arg)
    y_val, y_dom = normalize_arg(store, y_arg)

    # Both are constants
    if x_val is not None and y_val is not None:
        return Entailment.TRUE if x_val <= y_val else Entailment.FALSE

    # X is constant, Y has domain
    if x_val is not None and y_dom is not None:
        if y_dom.is_empty():
            return Entailment.FALSE
        if x_val <= y_dom.min():
            return Entailment.TRUE
        if x_val > y_dom.max():
            return Entailment.FALSE
        return Entailment.UNKNOWN

    # Y is constant, X has domain
    if y_val is not None and x_dom is not None:
        if x_dom.is_empty():
            return Entailment.FALSE
        if x_dom.max() <= y_val:
            return Entailment.TRUE
        if x_dom.min() > y_val:
            return Entailment.FALSE
        return Entailment.UNKNOWN

    # Both have domains
    if x_dom is not None and y_dom is not None:
        if x_dom.is_empty() or y_dom.is_empty():
            return Entailment.FALSE
        if x_dom.max() <= y_dom.min():
            return Entailment.TRUE
        if x_dom.min() > y_dom.max():
            return Entailment.FALSE

    return Entailment.UNKNOWN


def check_greater_than_entailment(
    store, x_arg: ConstraintArg, y_arg: ConstraintArg
) -> Entailment:
    """Check if X #> Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the greater-than constraint
    """
    # X #> Y is equivalent to Y #< X
    return check_less_than_entailment(store, y_arg, x_arg)


def check_greater_equal_entailment(
    store, x_arg: ConstraintArg, y_arg: ConstraintArg
) -> Entailment:
    """Check if X #>= Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the greater-or-equal constraint
    """
    # X #>= Y is equivalent to Y #=< X
    return check_less_equal_entailment(store, y_arg, x_arg)


def check_not_equal_entailment(
    store, x_arg: ConstraintArg, y_arg: ConstraintArg
) -> Entailment:
    """Check if X #\\= Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the not-equal constraint
    """
    # Inverse of equality
    eq_result = check_equality_entailment(store, x_arg, y_arg)

    if eq_result == Entailment.TRUE:
        return Entailment.FALSE
    elif eq_result == Entailment.FALSE:
        return Entailment.TRUE
    else:
        return Entailment.UNKNOWN


# Negation mappings for constraints
CONSTRAINT_NEGATIONS = {
    "#=": "#\\=",  # ¬(X #= Y) = X #\\= Y
    "#\\=": "#=",  # ¬(X #\\= Y) = X #= Y
    "#<": "#>=",  # ¬(X #< Y) = X #>= Y
    "#=<": "#>",  # ¬(X #=< Y) = X #> Y
    "#>": "#=<",  # ¬(X #> Y) = X #=< Y
    "#>=": "#<",  # ¬(X #>= Y) = X #< Y
}
