"""Arithmetic evaluation utilities extracted from engine.py."""

from typing import Union
from prolog.ast.terms import Term, Atom, Int, Float, Var, Struct


def eval_int(store, t: Term) -> int:
    """Evaluate a term as an integer.

    Args:
        store: The unification store
        t: The term to evaluate.

    Returns:
        The integer value.

    Raises:
        ValueError: If the term is not an integer.
    """
    if isinstance(t, Int):
        return t.value
    if isinstance(t, Var):
        r = store.deref(t.id)
        if r[0] == "BOUND":
            _, _, b = r
            return eval_int(store, b)
    raise ValueError("non-integer")


def eval_arithmetic(store, term: Term) -> Union[int, float]:
    """Evaluate an arithmetic expression iteratively.

    Args:
        store: The unification store
        term: The term to evaluate.

    Returns:
        The numeric result (int or float).

    Raises:
        ValueError: If evaluation fails.
    """
    # Stack-based evaluation to avoid recursion
    # Each entry is either ('eval', term) or ('apply', op, values)
    eval_stack = [("eval", term)]
    value_stack = []

    while eval_stack:
        action, data = eval_stack.pop()

        if action == "eval":
            t = data

            # Dereference if variable
            if isinstance(t, Var):
                result = store.deref(t.id)
                if result[0] == "BOUND":
                    _, _, bound_term = result
                    t = bound_term
                else:
                    raise ValueError("Unbound variable in arithmetic")

            if isinstance(t, Int):
                value_stack.append(t.value)
            elif isinstance(t, Float):
                value_stack.append(t.value)
            elif isinstance(t, Struct):
                if (
                    t.functor in ["+", "-", "*", "//", "/", "mod", "max", "min", "**"]
                    and len(t.args) == 2
                ):
                    # Binary operator: evaluate args then apply
                    eval_stack.append(("apply", Atom(t.functor)))
                    # Push args in reverse order for correct evaluation
                    eval_stack.append(("eval", t.args[1]))
                    eval_stack.append(("eval", t.args[0]))
                elif t.functor == "-" and len(t.args) == 1:
                    # Unary minus
                    eval_stack.append(("apply_unary", Atom("-")))
                    eval_stack.append(("eval", t.args[0]))
                else:
                    raise ValueError(
                        f"Unknown arithmetic operator: {t.functor}/{len(t.args)}"
                    )
            else:
                raise ValueError(f"Cannot evaluate arithmetic: {t}")

        elif action == "apply":
            op_atom = data
            # Extract operator name from Atom
            op = op_atom.name if isinstance(op_atom, Atom) else str(op_atom)
            if len(value_stack) < 2:
                raise ValueError(f"Not enough values for operator {op}")

            right = value_stack.pop()
            left = value_stack.pop()

            if op == "+":
                value_stack.append(left + right)
            elif op == "-":
                value_stack.append(left - right)
            elif op == "*":
                value_stack.append(left * right)
            elif op == "//":
                if right == 0:
                    raise ValueError("Division by zero")
                value_stack.append(left // right)
            elif op == "/":
                if right == 0:
                    raise ValueError("Division by zero")
                value_stack.append(left / right)  # Float division
            elif op == "mod":
                if right == 0:
                    raise ValueError("Modulo by zero")
                value_stack.append(left % right)
            elif op == "max":
                value_stack.append(max(left, right))
            elif op == "min":
                value_stack.append(min(left, right))
            elif op == "**":
                try:
                    result = left**right
                    value_stack.append(result)
                except (OverflowError, ValueError):
                    raise ValueError("Power operation overflow or invalid")

        elif action == "apply_unary":
            op_atom = data
            # Extract operator name from Atom
            op = op_atom.name if isinstance(op_atom, Atom) else str(op_atom)
            if not value_stack:
                raise ValueError("Not enough values for unary operator")
            value = value_stack.pop()

            if op == "-":
                value_stack.append(-value)
            else:
                raise ValueError(f"Unknown unary operator: {op}")

    if len(value_stack) != 1:
        raise ValueError(
            f"Arithmetic evaluation error: expected 1 value, got {len(value_stack)}"
        )

    return value_stack[0]
