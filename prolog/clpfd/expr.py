"""CLP(FD) arithmetic expression parser.

Parses linear arithmetic expressions into coefficient-variable pairs.
Supports operators: +, -, * (with constants only)
"""

from typing import Dict, Tuple
from prolog.ast.terms import Term, Var, Int, Struct
from prolog.clpfd.api import get_domain
from prolog.engine.utils.arithmetic import eval_arithmetic


def _is_ground_arithmetic(expr: Term, engine) -> bool:
    """Check if an expression contains only ground arithmetic terms.

    Args:
        expr: Expression to check
        engine: Engine for dereferencing variables

    Returns:
        True if expression is ground arithmetic, False otherwise
    """
    if isinstance(expr, Int):
        return True
    elif isinstance(expr, Var):
        # Check if variable is bound to a ground term
        deref = engine.store.deref(expr.id)
        if deref[0] == "BOUND":
            return _is_ground_arithmetic(deref[2], engine)
        else:
            # Check if variable has a singleton domain (effectively bound)
            domain = get_domain(engine.store, deref[1])
            if domain and domain.is_singleton():
                return True
            return False
    elif isinstance(expr, Struct):
        # Check if it's a supported arithmetic operator and all args are ground
        if expr.functor in ["+", "-", "*", "//", "mod", "max", "min"]:
            return all(_is_ground_arithmetic(arg, engine) for arg in expr.args)
        else:
            return False
    else:
        return False


def _convert_to_ground_expr(expr: Term, engine) -> Term:
    """Convert an expression with singleton domains to ground integers.

    Args:
        expr: Expression to convert
        engine: Engine for dereferencing and domain access

    Returns:
        Expression with singleton domain variables replaced by Int values
    """
    if isinstance(expr, Int):
        return expr
    elif isinstance(expr, Var):
        # Check if variable is bound to a ground term
        deref = engine.store.deref(expr.id)
        if deref[0] == "BOUND":
            return _convert_to_ground_expr(deref[2], engine)
        else:
            # Check if variable has a singleton domain
            domain = get_domain(engine.store, deref[1])
            if domain and domain.is_singleton():
                return Int(domain.min())
            else:
                raise ValueError(f"Variable {expr} is not ground")
    elif isinstance(expr, Struct):
        # Convert all arguments
        new_args = tuple(_convert_to_ground_expr(arg, engine) for arg in expr.args)
        return Struct(expr.functor, new_args)
    else:
        raise ValueError(f"Cannot convert {expr} to ground expression")


def parse_linear_expression(expr: Term, engine) -> Tuple[Dict[int, int], int]:
    """Parse a linear arithmetic expression into coefficients and constant.

    Args:
        expr: Arithmetic expression term
        engine: Engine for dereferencing variables

    Returns:
        (coefficients, constant) where:
        - coefficients: dict mapping variable ID to coefficient
        - constant: integer constant term

    Raises:
        ValueError: If expression is non-linear or invalid
    """
    # First, try to evaluate ground arithmetic expressions
    try:
        if _is_ground_arithmetic(expr, engine):
            # Convert singleton domains to Int values for evaluation
            ground_expr = _convert_to_ground_expr(expr, engine)
            value = eval_arithmetic(engine.store, ground_expr)
            return {}, value
    except (ValueError, AttributeError):
        # Not a ground arithmetic expression, continue with linear parsing
        pass

    coeffs = {}
    const = 0

    def add_term(coeff: int, term: Term):
        """Add a term with its coefficient to the result."""
        nonlocal coeffs, const

        # Dereference if it's a variable
        if isinstance(term, Var):
            deref = engine.store.deref(term.id)
            if deref[0] == "BOUND":
                # Variable is bound to a value
                bound_term = deref[2]
                if isinstance(bound_term, Int):
                    const += coeff * bound_term.value
                else:
                    raise ValueError(f"Invalid arithmetic term: {bound_term}")
            else:
                # Unbound variable
                var_id = deref[1]  # Get root variable ID
                if var_id in coeffs:
                    coeffs[var_id] += coeff
                else:
                    coeffs[var_id] = coeff
        elif isinstance(term, Int):
            const += coeff * term.value
        else:
            raise ValueError(f"Invalid arithmetic term: {term}")

    def parse_expr(expr: Term, sign: int = 1):
        """Recursively parse an expression with given sign multiplier."""
        nonlocal coeffs, const

        if isinstance(expr, Var):
            add_term(sign, expr)
        elif isinstance(expr, Int):
            const += sign * expr.value
        elif isinstance(expr, Struct):
            if expr.functor == "+" and len(expr.args) == 2:
                # Binary addition
                parse_expr(expr.args[0], sign)
                parse_expr(expr.args[1], sign)
            elif expr.functor == "-" and len(expr.args) == 2:
                # Binary subtraction
                parse_expr(expr.args[0], sign)
                parse_expr(expr.args[1], -sign)
            elif expr.functor == "-" and len(expr.args) == 1:
                # Unary minus
                parse_expr(expr.args[0], -sign)
            elif expr.functor == "*" and len(expr.args) == 2:
                # Multiplication - at least one arg must be a constant
                left, right = expr.args

                # Try to evaluate left as constant
                if isinstance(left, Int):
                    # Constant * something
                    parse_expr(right, sign * left.value)
                elif isinstance(left, Var):
                    # Check if left is bound to a constant
                    deref = engine.store.deref(left.id)
                    if deref[0] == "BOUND" and isinstance(deref[2], Int):
                        # Bound to constant
                        parse_expr(right, sign * deref[2].value)
                    elif isinstance(right, Int):
                        # Variable * constant
                        parse_expr(left, sign * right.value)
                    elif isinstance(right, Var):
                        # Check if right is bound to a constant
                        right_deref = engine.store.deref(right.id)
                        if right_deref[0] == "BOUND" and isinstance(
                            right_deref[2], Int
                        ):
                            # Right is bound to constant
                            parse_expr(left, sign * right_deref[2].value)
                        else:
                            # Both are variables - non-linear
                            raise ValueError(f"Non-linear term: {expr}")
                    else:
                        raise ValueError(f"Non-linear term: {expr}")
                elif isinstance(right, Int):
                    # Something * constant
                    parse_expr(left, sign * right.value)
                elif isinstance(right, Var):
                    # Check if right is bound to a constant
                    deref = engine.store.deref(right.id)
                    if deref[0] == "BOUND" and isinstance(deref[2], Int):
                        # Right is bound to constant
                        parse_expr(left, sign * deref[2].value)
                    else:
                        raise ValueError(f"Non-linear term: {expr}")
                else:
                    raise ValueError(f"Non-linear term: {expr}")
            else:
                raise ValueError(f"Unsupported arithmetic operator: {expr.functor}")
        else:
            raise ValueError(f"Invalid arithmetic term: {expr}")

    # Parse the expression
    parse_expr(expr)

    # Remove zero coefficients
    coeffs = {k: v for k, v in coeffs.items() if v != 0}

    return coeffs, const
