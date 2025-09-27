"""Sum constraint propagator for CLP(FD).

Specialized propagator for sum(Variables) = Value constraints.
More efficient than general linear propagator for this common case.
"""

from typing import List, Tuple, Optional
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


def create_sum_propagator(var_ids: List[int], target: int):
    """Create a sum constraint propagator.

    Args:
        var_ids: List of variable IDs to sum
        target: Target sum value

    Returns:
        A propagator function that enforces sum(vars) = target
    """

    def sum_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate the sum constraint.

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        # Handle empty sum
        if not var_ids:
            return ("ok", None) if target == 0 else ("fail", None)

        # Compute current bounds
        min_sum = 0
        max_sum = 0
        var_domains = {}  # Maps var_id to domain

        for var_id in var_ids:
            # Dereference variable
            deref = store.deref(var_id)
            if deref[0] == "BOUND":
                # Variable is bound to a value
                val = deref[2]
                if isinstance(val, Int):
                    min_sum += val.value
                    max_sum += val.value
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

                var_domains[root_id] = dom
                min_sum += dom.min()
                max_sum += dom.max()

        # Check feasibility
        if min_sum > target or max_sum < target:
            return ("fail", None)

        # Propagate to individual variables
        changed = []

        for var_id, dom in var_domains.items():
            # Calculate bounds for this variable based on others
            # Sum of others (excluding this variable)
            # When others are at min, this var is at max, so:
            other_min = min_sum - dom.min()
            # When others are at max, this var is at min, so:
            other_max = max_sum - dom.max()

            # Variable must equal target - sum_of_others
            # Min when others are at max: var >= target - other_max
            new_min = max(dom.min(), target - other_max)
            # Max when others are at min: var <= target - other_min
            new_max = min(dom.max(), target - other_min)

            if new_min > new_max:
                return ("fail", None)

            if new_min > dom.min() or new_max < dom.max():
                new_dom = Domain(((new_min, new_max),))

                if new_dom.is_empty():
                    return ("fail", None)

                set_domain(store, var_id, new_dom, trail)
                changed.append(var_id)

                # Update min_sum and max_sum for next iterations
                min_sum = min_sum - dom.min() + new_min
                max_sum = max_sum - dom.max() + new_max
                var_domains[var_id] = new_dom

        return ("ok", changed if changed else None)

    return sum_propagator