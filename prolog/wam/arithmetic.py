"""WAM arithmetic evaluation engine.

Implements recursive arithmetic evaluation operating directly on WAM heap cells.
Supports standard Prolog arithmetic operators with proper error handling.

Operators:
- Binary: +, -, *, //, mod, /
- Unary: -, +

Error Conditions:
- instantiation_error: Unbound variable in expression
- type_error: Non-numeric argument
- evaluation_error(zero_divisor): Division by zero
"""

from __future__ import annotations

from prolog.ast.terms import Atom, Int
from prolog.wam.errors import EvaluationError, InstantiationError, TypeError
from prolog.wam.heap import TAG_CON, TAG_REF, TAG_STR
from prolog.wam.unify import deref

__all__ = [
    "eval_arithmetic",
]


def eval_arithmetic(addr: int, machine) -> int | float:
    """Evaluate arithmetic expression at heap address.

    Args:
        addr: Heap address of expression root
        machine: Machine instance with heap

    Returns:
        Numeric result (int or float)

    Raises:
        InstantiationError: If expression contains unbound variables
        TypeError: If expression contains non-numeric terms
        EvaluationError: If division by zero or other evaluation errors

    Examples:
        # Simple constant
        heap[0] = (TAG_CON, 42)
        eval_arithmetic(0, machine) → 42

        # Binary operation: 2 + 3
        heap[0] = (TAG_STR, 1)
        heap[1] = (TAG_CON, ("+", 2))
        heap[2] = (TAG_CON, 2)
        heap[3] = (TAG_CON, 3)
        eval_arithmetic(0, machine) → 5

        # Unary minus: -(5)
        heap[0] = (TAG_STR, 1)
        heap[1] = (TAG_CON, ("-", 1))
        heap[2] = (TAG_CON, 5)
        eval_arithmetic(0, machine) → -5

        # Unbound variable
        heap[0] = (TAG_REF, 0)
        eval_arithmetic(0, machine) → raises InstantiationError
    """
    # Dereference to get actual term
    addr = deref(machine, addr)
    cell = machine.heap[addr]
    tag = cell[0]

    # Constant: must be numeric
    if tag == TAG_CON:
        value = cell[1]
        if isinstance(value, (int, float)) and not isinstance(value, bool):
            return value
        else:
            # Non-numeric constant
            if isinstance(value, str):
                culprit = Atom(value)
            elif isinstance(value, bool):
                culprit = Atom(str(value).lower())
            else:
                culprit = Int(0)  # Placeholder
            raise TypeError("number", culprit)

    # Unbound variable
    elif tag == TAG_REF and cell[1] == addr:
        raise InstantiationError("arithmetic expression")

    # Structure: operator application
    elif tag == TAG_STR:
        functor_addr = cell[1]
        functor_cell = machine.heap[functor_addr]

        if functor_cell[0] != TAG_CON:

            raise TypeError("number", Atom("?"))

        functor_data = functor_cell[1]
        if not isinstance(functor_data, tuple) or len(functor_data) != 2:

            raise TypeError("number", Atom("?"))

        name, arity = functor_data

        # Binary operators
        if arity == 2:
            left_addr = functor_addr + 1
            right_addr = functor_addr + 2

            # Recursively evaluate operands
            left = eval_arithmetic(left_addr, machine)
            right = eval_arithmetic(right_addr, machine)

            if name == "+":
                return left + right
            elif name == "-":
                return left - right
            elif name == "*":
                return left * right
            elif name == "/":
                # Float division
                if right == 0:
                    raise EvaluationError("zero_divisor")
                return left / right
            elif name == "//":
                # Integer division (floor division toward negative infinity)
                if right == 0:
                    raise EvaluationError("zero_divisor")
                return int(left // right)
            elif name == "mod":
                # Modulo operation
                if right == 0:
                    raise EvaluationError("zero_divisor")
                return int(left % right)
            else:
                # Unknown binary operator
                raise TypeError("evaluable", Atom(f"{name}/{arity}"))

        # Unary operators
        elif arity == 1:
            arg_addr = functor_addr + 1
            arg = eval_arithmetic(arg_addr, machine)

            if name == "-":
                return -arg
            elif name == "+":
                return +arg
            else:
                # Unknown unary operator
                raise TypeError("evaluable", Atom(f"{name}/{arity}"))

        else:
            # Invalid arity for arithmetic

            raise TypeError("evaluable", Atom(f"{name}/{arity}"))

    else:
        # Unknown tag type

        raise TypeError("number", Atom("?"))
