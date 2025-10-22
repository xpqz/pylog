"""WAM arithmetic builtins.

Implements ISO Prolog arithmetic evaluation and comparison predicates:
- is/2: Arithmetic evaluation and unification
- =:=/2: Arithmetic equality
- =\\=/2: Arithmetic inequality
- </2: Less than
- =</2: Less than or equal
- >/2: Greater than
- >=/2: Greater than or equal

All comparison builtins evaluate both arguments as arithmetic expressions
before performing the comparison.
"""

from __future__ import annotations

from prolog.wam.arithmetic import eval_arithmetic
from prolog.wam.heap import TAG_CON, new_con
from prolog.wam.unify import deref, unify

__all__ = [
    "builtin_is",
    "builtin_arith_eq",
    "builtin_arith_ne",
    "builtin_arith_lt",
    "builtin_arith_le",
    "builtin_arith_gt",
    "builtin_arith_ge",
    "register_arithmetic_builtins",
]


def builtin_is(machine) -> bool:
    """is/2: Evaluate arithmetic expression and unify with result.

    Signature: X is Expr

    Args:
        machine: Machine with X[0] = X (target), X[1] = Expr (expression)

    Returns:
        True if evaluation succeeds and unifies, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs (e.g., division by zero)

    Examples:
        ?- X is 2 + 3.          % X = 5
        ?- 5 is 2 + 3.          % succeeds
        ?- 6 is 2 + 3.          % fails
        ?- X is Y.              % instantiation_error
        ?- X is 5 // 0.         % evaluation_error(zero_divisor)

    Implementation note:
        To avoid heap allocation on failure, checks if LHS is already bound
        to a constant and compares directly before allocating new heap cell.
    """
    # Get arguments
    lhs_addr = machine.X[0]
    rhs_addr = machine.X[1]

    # Evaluate RHS expression
    result = eval_arithmetic(rhs_addr, machine)

    # Dereference LHS to check if it's already bound
    lhs_deref = deref(machine, lhs_addr)
    lhs_cell = machine.heap[lhs_deref]

    # If LHS is already bound to a constant, compare directly
    # This avoids allocating a heap cell when unification will fail
    if lhs_cell[0] == TAG_CON:
        # Direct comparison - no heap allocation
        return lhs_cell[1] == result

    # LHS is unbound or non-constant - allocate result and unify
    result_addr = new_con(machine, result)
    return unify(machine, lhs_addr, result_addr)


def builtin_arith_eq(machine) -> bool:
    """=:=/2: Arithmetic equality comparison.

    Signature: Expr1 =:= Expr2

    Args:
        machine: Machine with X[0] = Expr1, X[1] = Expr2

    Returns:
        True if expressions evaluate to equal values, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs

    Examples:
        ?- 5 =:= 2 + 3.         % succeeds
        ?- 5 =:= 4.             % fails
        ?- 2.5 =:= 5 / 2.       % succeeds
    """
    left_addr = machine.X[0]
    right_addr = machine.X[1]

    left_val = eval_arithmetic(left_addr, machine)
    right_val = eval_arithmetic(right_addr, machine)

    return left_val == right_val


def builtin_arith_ne(machine) -> bool:
    """=\\=/2: Arithmetic inequality comparison.

    Signature: Expr1 =\\= Expr2

    Args:
        machine: Machine with X[0] = Expr1, X[1] = Expr2

    Returns:
        True if expressions evaluate to different values, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs

    Examples:
        ?- 5 =\\= 4.            % succeeds
        ?- 5 =\\= 2 + 3.        % fails
    """
    left_addr = machine.X[0]
    right_addr = machine.X[1]

    left_val = eval_arithmetic(left_addr, machine)
    right_val = eval_arithmetic(right_addr, machine)

    return left_val != right_val


def builtin_arith_lt(machine) -> bool:
    """</2: Arithmetic less than comparison.

    Signature: Expr1 < Expr2

    Args:
        machine: Machine with X[0] = Expr1, X[1] = Expr2

    Returns:
        True if Expr1 evaluates to less than Expr2, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs

    Examples:
        ?- 3 < 5.               % succeeds
        ?- 5 < 5.               % fails
        ?- 7 < 5.               % fails
    """
    left_addr = machine.X[0]
    right_addr = machine.X[1]

    left_val = eval_arithmetic(left_addr, machine)
    right_val = eval_arithmetic(right_addr, machine)

    return left_val < right_val


def builtin_arith_le(machine) -> bool:
    """=</2: Arithmetic less than or equal comparison.

    Signature: Expr1 =< Expr2

    Args:
        machine: Machine with X[0] = Expr1, X[1] = Expr2

    Returns:
        True if Expr1 evaluates to less than or equal to Expr2, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs

    Examples:
        ?- 3 =< 5.              % succeeds
        ?- 5 =< 5.              % succeeds
        ?- 7 =< 5.              % fails
    """
    left_addr = machine.X[0]
    right_addr = machine.X[1]

    left_val = eval_arithmetic(left_addr, machine)
    right_val = eval_arithmetic(right_addr, machine)

    return left_val <= right_val


def builtin_arith_gt(machine) -> bool:
    """>/2: Arithmetic greater than comparison.

    Signature: Expr1 > Expr2

    Args:
        machine: Machine with X[0] = Expr1, X[1] = Expr2

    Returns:
        True if Expr1 evaluates to greater than Expr2, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs

    Examples:
        ?- 7 > 5.               % succeeds
        ?- 5 > 5.               % fails
        ?- 3 > 5.               % fails
    """
    left_addr = machine.X[0]
    right_addr = machine.X[1]

    left_val = eval_arithmetic(left_addr, machine)
    right_val = eval_arithmetic(right_addr, machine)

    return left_val > right_val


def builtin_arith_ge(machine) -> bool:
    """>=/2: Arithmetic greater than or equal comparison.

    Signature: Expr1 >= Expr2

    Args:
        machine: Machine with X[0] = Expr1, X[1] = Expr2

    Returns:
        True if Expr1 evaluates to greater than or equal to Expr2, False otherwise

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If arithmetic error occurs

    Examples:
        ?- 7 >= 5.              % succeeds
        ?- 5 >= 5.              % succeeds
        ?- 3 >= 5.              % fails
    """
    left_addr = machine.X[0]
    right_addr = machine.X[1]

    left_val = eval_arithmetic(left_addr, machine)
    right_val = eval_arithmetic(right_addr, machine)

    return left_val >= right_val


def register_arithmetic_builtins(registry: dict) -> None:
    """Register all arithmetic builtins in the given registry.

    Args:
        registry: Builtin registry dict to populate

    Registers:
        system:is/2
        system:=:=/2
        system:=\\=/2
        system:</2
        system:=</2
        system:>/2
        system:>=/2
    """
    registry["system:is/2"] = builtin_is
    registry["system:=:=/2"] = builtin_arith_eq
    registry["system:=\\=/2"] = builtin_arith_ne
    registry["system:</2"] = builtin_arith_lt
    registry["system:=</2"] = builtin_arith_le
    registry["system:>/2"] = builtin_arith_gt
    registry["system:>=/2"] = builtin_arith_ge
