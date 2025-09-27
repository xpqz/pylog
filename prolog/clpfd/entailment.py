"""Entailment detection for CLP(FD) constraints.

Provides functions to determine when constraints are definitely satisfied
(entailed), definitely violated (disentailed), or undetermined based on
current variable domains.
"""

from enum import Enum
from typing import Optional, Union, Tuple

from prolog.clpfd.api import get_domain
from prolog.clpfd.domain import Domain


class Entailment(Enum):
    """Entailment states for constraint satisfaction."""
    TRUE = 1      # Constraint is definitely satisfied
    FALSE = 0     # Constraint is definitely violated
    UNKNOWN = -1  # Constraint satisfaction is undetermined


# Type alias for constraint arguments
# Either a variable ID (int) or a constant (None, value)
ConstraintArg = Union[int, Tuple[None, int]]


def check_equality_entailment(store, x_arg: ConstraintArg, y_arg: ConstraintArg) -> Entailment:
    """Check if X #= Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the equality constraint
    """
    # Handle var-int and int-int cases
    if isinstance(x_arg, tuple) and x_arg[0] is None:
        # X is a constant
        x_val = x_arg[1]
        if isinstance(y_arg, tuple) and y_arg[0] is None:
            # Both constants
            return Entailment.TRUE if x_val == y_arg[1] else Entailment.FALSE
        else:
            # X is constant, Y is variable
            y_dom = get_domain(store, y_arg)
            if y_dom:
                if not y_dom.contains(x_val):
                    return Entailment.FALSE
                if y_dom.is_singleton() and y_dom.min() == x_val:
                    return Entailment.TRUE
            return Entailment.UNKNOWN

    if isinstance(y_arg, tuple) and y_arg[0] is None:
        # Y is constant, X is variable
        y_val = y_arg[1]
        x_dom = get_domain(store, x_arg)
        if x_dom:
            if not x_dom.contains(y_val):
                return Entailment.FALSE
            if x_dom.is_singleton() and x_dom.min() == y_val:
                return Entailment.TRUE
        return Entailment.UNKNOWN

    # Both are variables
    x_dom = get_domain(store, x_arg)
    y_dom = get_domain(store, y_arg)

    if x_dom and y_dom:
        # Check domain intersection
        intersection = x_dom.intersect(y_dom)

        if intersection.is_empty():
            return Entailment.FALSE  # Domains disjoint

        # Check for singleton domains
        if x_dom.is_singleton() and y_dom.is_singleton():
            if x_dom.min() == y_dom.min():
                return Entailment.TRUE
            else:
                return Entailment.FALSE

        # Special case: one singleton, check if value is in other domain
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


def check_less_than_entailment(store, x_arg: ConstraintArg, y_arg: ConstraintArg) -> Entailment:
    """Check if X #< Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the less-than constraint
    """
    # Handle constants
    if isinstance(x_arg, tuple) and x_arg[0] is None:
        x_val = x_arg[1]
        if isinstance(y_arg, tuple) and y_arg[0] is None:
            return Entailment.TRUE if x_val < y_arg[1] else Entailment.FALSE
        else:
            y_dom = get_domain(store, y_arg)
            if y_dom:
                if x_val < y_dom.min():
                    return Entailment.TRUE
                if x_val >= y_dom.max():
                    return Entailment.FALSE
            return Entailment.UNKNOWN

    if isinstance(y_arg, tuple) and y_arg[0] is None:
        y_val = y_arg[1]
        x_dom = get_domain(store, x_arg)
        if x_dom:
            if x_dom.max() < y_val:
                return Entailment.TRUE
            if x_dom.min() >= y_val:
                return Entailment.FALSE
        return Entailment.UNKNOWN

    # Both variables
    x_dom = get_domain(store, x_arg)
    y_dom = get_domain(store, y_arg)

    if x_dom and y_dom:
        if x_dom.max() < y_dom.min():
            return Entailment.TRUE
        if x_dom.min() >= y_dom.max():
            return Entailment.FALSE

    return Entailment.UNKNOWN


def check_less_equal_entailment(store, x_arg: ConstraintArg, y_arg: ConstraintArg) -> Entailment:
    """Check if X #=< Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant

    Returns:
        Entailment state for the less-or-equal constraint
    """
    # Handle constants
    if isinstance(x_arg, tuple) and x_arg[0] is None:
        x_val = x_arg[1]
        if isinstance(y_arg, tuple) and y_arg[0] is None:
            return Entailment.TRUE if x_val <= y_arg[1] else Entailment.FALSE
        else:
            y_dom = get_domain(store, y_arg)
            if y_dom:
                if x_val <= y_dom.min():
                    return Entailment.TRUE
                if x_val > y_dom.max():
                    return Entailment.FALSE
            return Entailment.UNKNOWN

    if isinstance(y_arg, tuple) and y_arg[0] is None:
        y_val = y_arg[1]
        x_dom = get_domain(store, x_arg)
        if x_dom:
            if x_dom.max() <= y_val:
                return Entailment.TRUE
            if x_dom.min() > y_val:
                return Entailment.FALSE
        return Entailment.UNKNOWN

    # Both variables
    x_dom = get_domain(store, x_arg)
    y_dom = get_domain(store, y_arg)

    if x_dom and y_dom:
        if x_dom.max() <= y_dom.min():
            return Entailment.TRUE
        if x_dom.min() > y_dom.max():
            return Entailment.FALSE

    return Entailment.UNKNOWN


def check_greater_than_entailment(store, x_arg: ConstraintArg, y_arg: ConstraintArg) -> Entailment:
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


def check_greater_equal_entailment(store, x_arg: ConstraintArg, y_arg: ConstraintArg) -> Entailment:
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


def check_not_equal_entailment(store, x_arg: ConstraintArg, y_arg: ConstraintArg) -> Entailment:
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
    "#=": "#\\=",     # ¬(X #= Y) = X #\\= Y
    "#\\=": "#=",     # ¬(X #\\= Y) = X #= Y
    "#<": "#>=",      # ¬(X #< Y) = X #>= Y
    "#=<": "#>",      # ¬(X #=< Y) = X #> Y
    "#>": "#=<",      # ¬(X #> Y) = X #=< Y
    "#>=": "#<",      # ¬(X #>= Y) = X #< Y
}