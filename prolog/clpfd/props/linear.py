"""Linear constraint propagator for CLP(FD).

Implements bounds consistency for linear constraints of the form:
    c1*X1 + c2*X2 + ... + cn*Xn op k
where op is one of: =, !=, <, >, =<, >=
"""

from typing import Dict, Tuple, Optional, List
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


def create_linear_propagator(coeffs: Dict[int, int], const: int, op: str):
    """Create a linear constraint propagator.

    Args:
        coeffs: Dictionary mapping variable ID to coefficient
        const: Constant term (on right side, negated)
        op: Comparison operator ('=', '!=', '<', '>', '=<', '>=')

    Returns:
        A propagator function
    """
    # Remove zero coefficients
    coeffs = {k: v for k, v in coeffs.items() if v != 0}

    def linear_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate the linear constraint.

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        # Handle empty constraint (no variables)
        if not coeffs:
            # Evaluate constant constraint
            if op == '=':
                return ("ok", None) if const == 0 else ("fail", None)
            elif op == '!=':
                return ("ok", None) if const != 0 else ("fail", None)
            elif op == '<':
                return ("ok", None) if 0 < -const else ("fail", None)
            elif op == '>':
                return ("ok", None) if 0 > -const else ("fail", None)
            elif op == '=<':
                return ("ok", None) if 0 <= -const else ("fail", None)
            elif op == '>=':
                return ("ok", None) if 0 >= -const else ("fail", None)
            else:
                raise ValueError(f"Unknown operator: {op}")

        # Calculate bounds of the linear expression
        min_val = 0
        max_val = 0
        active_vars = {}  # Maps var_id to (coeff, domain)

        for var_id, coeff in coeffs.items():
            # Dereference variable
            deref = store.deref(var_id)
            if deref[0] == "BOUND":
                # Variable is bound to a value
                val = deref[2]
                if isinstance(val, Int):
                    min_val += coeff * val.value
                    max_val += coeff * val.value
                else:
                    # Non-integer binding, can't handle
                    continue
            else:
                # Unbound variable
                root_id = deref[1]
                dom = get_domain(store, root_id)
                if dom is None:
                    # No domain yet, skip this variable
                    continue

                active_vars[root_id] = (coeff, dom)

                if coeff > 0:
                    min_val += coeff * dom.min()
                    max_val += coeff * dom.max()
                else:
                    # Negative coefficient reverses min/max
                    min_val += coeff * dom.max()
                    max_val += coeff * dom.min()

        # Add constant term (remember it's negated)
        target = -const

        # Check feasibility based on operator
        if op == '=':
            if max_val < target or min_val > target:
                return ("fail", None)
        elif op == '!=':
            if min_val == max_val == target:
                return ("fail", None)
        elif op == '<':
            if min_val >= target:
                return ("fail", None)
        elif op == '>':
            if max_val <= target:
                return ("fail", None)
        elif op == '=<':
            if min_val > target:
                return ("fail", None)
        elif op == '>=':
            if max_val < target:
                return ("fail", None)

        # Propagate to individual variables
        changed = []

        for var_id, (coeff, dom) in active_vars.items():
            # Calculate bounds for this variable based on others
            # Sum of others (excluding this variable)
            other_min = min_val
            other_max = max_val

            if coeff > 0:
                other_min -= coeff * dom.min()
                other_max -= coeff * dom.max()
            else:
                other_min -= coeff * dom.max()
                other_max -= coeff * dom.min()

            # Calculate new bounds for this variable
            new_min = dom.min()
            new_max = dom.max()

            if op == '=':
                # c*X + others = target
                # c*X = target - others
                if coeff > 0:
                    # X = (target - others) / c
                    # Min when others is at max: X >= (target - other_max) / c
                    val_min = target - other_max
                    if val_min >= 0:
                        new_min = max(new_min, (val_min + coeff - 1) // coeff)  # Ceiling
                    else:
                        new_min = max(new_min, val_min // coeff)  # Floor of negative
                    # Max when others is at min: X <= (target - other_min) / c
                    new_max = min(new_max, (target - other_min) // coeff)  # Floor
                else:
                    # Negative coefficient c < 0
                    # c*X = target - others, so X = (target - others) / c
                    # Since c < 0, dividing by negative flips inequalities
                    # When others are at MAX, (target - others) is MIN, X is MAX (flipped)
                    # When others are at MIN, (target - others) is MAX, X is MIN (flipped)

                    if coeff == -1:
                        # Special case for -1: X = others - target
                        new_min = max(new_min, other_min - target)
                        new_max = min(new_max, other_max - target)
                    else:
                        # General negative coefficient
                        # Min of X when others at MIN (gives max of target-others)
                        val = target - other_min  # This is negative of what X should be
                        # X = val / coeff, and coeff < 0, so X = -val / |coeff|
                        # For min, we need ceiling when dividing negative
                        if val >= 0:
                            new_min = max(new_min, -(val // -coeff))
                        else:
                            new_min = max(new_min, ((-val - 1) // -coeff) + 1)

                        # Max of X when others at MAX (gives min of target-others)
                        val = target - other_max
                        # For max, we need floor when dividing negative
                        new_max = min(new_max, val // coeff)

            elif op == '!=':
                # Special case: if domain is singleton and equals forbidden value
                if dom.is_singleton():
                    if coeff * dom.min() + other_min == target:
                        return ("fail", None)
                # If the constraint forces a specific value, remove it
                if other_min == other_max:  # Others have fixed sum
                    forbidden_val = (target - other_min) // coeff
                    if coeff * forbidden_val + other_min == target:  # Check it's exact
                        if dom.contains(forbidden_val):
                            new_dom = dom.remove_value(forbidden_val)
                            if new_dom.is_empty():
                                return ("fail", None)
                            set_domain(store, var_id, new_dom, trail)
                            if var_id not in changed:
                                changed.append(var_id)
                continue  # Skip normal bounds update for disequality

            elif op == '<':
                # c*X + others < target
                if coeff > 0:
                    # X < (target - others) / c
                    # Max when others is at min
                    new_max = min(new_max, (target - other_min - 1) // coeff)
                else:
                    # X > (target - others) / c (inequality flips)
                    # Min when others is at max
                    new_min = max(new_min, -((-target + other_max) // -coeff) + 1)

            elif op == '>':
                # c*X + others > target
                if coeff > 0:
                    # X > (target - others) / c
                    # Min when others is at max
                    new_min = max(new_min, (target - other_max) // coeff + 1)
                else:
                    # X < (target - others) / c (inequality flips)
                    # Max when others is at min
                    new_max = min(new_max, -((other_min - target) // -coeff) - 1)

            elif op == '=<':
                # c*X + others =< target
                if coeff > 0:
                    # X =< (target - others) / c
                    # Max when others is at min
                    new_max = min(new_max, (target - other_min) // coeff)  # Floor
                else:
                    # X >= (target - others) / c (inequality flips due to negative coeff)
                    # Min when others is at max
                    val = target - other_max
                    # Ceiling division with negative divisor
                    new_min = max(new_min, -((-val - 1) // -coeff))

            elif op == '>=':
                # c*X + others >= target
                if coeff > 0:
                    # X >= (target - others) / c
                    # Min when others is at max
                    val_min = target - other_max
                    if val_min >= 0:
                        new_min = max(new_min, (val_min + coeff - 1) // coeff)  # Ceiling
                    else:
                        new_min = max(new_min, val_min // coeff)  # Floor of negative
                else:
                    # c*X + others >= target where c < 0
                    # c*X >= target - others
                    # X <= (target - others) / c = (others - target) / |c|
                    # Max when others is at max
                    new_max = min(new_max, (other_max - target) // -coeff)  # Floor

            # Apply new bounds
            if new_min > new_max:
                return ("fail", None)

            if new_min > dom.min() or new_max < dom.max():
                new_dom = dom
                if new_min > dom.min():
                    new_dom = Domain(((new_min, new_dom.max()),), dom.rev + 1)
                if new_max < new_dom.max():
                    # Use the potentially updated new_dom's rev
                    new_dom = Domain(((new_dom.min(), new_max),), new_dom.rev if new_dom != dom else dom.rev + 1)

                if new_dom.is_empty():
                    return ("fail", None)

                set_domain(store, var_id, new_dom, trail)
                if var_id not in changed:
                    changed.append(var_id)

        return ("ok", changed if changed else None)

    return linear_propagator