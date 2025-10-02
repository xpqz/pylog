"""NValue constraint propagator.

Implements nvalue/2 constraint where N equals the number of distinct values
in a list of variables. Provides bounds consistency via bounds reasoning.
"""

from typing import Tuple, Optional, List, Set, Dict
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


def create_nvalue_propagator(
    n_var: int, var_ids: List[int], fixed_ground_values: Set[int] = None
):
    """Create nvalue constraint propagator.

    Args:
        n_var: Variable ID for N (number of distinct values)
        var_ids: List of variable IDs whose distinct values are counted
        fixed_ground_values: Set of ground integer values from the original list

    Returns:
        A propagator function that enforces nvalue constraint
    """
    if fixed_ground_values is None:
        fixed_ground_values = set()

    def nvalue_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Enforce nvalue constraint via bounds consistency.

        Algorithm:
        1. Compute minimum possible distinct values (lower bound)
        2. Compute maximum possible distinct values (upper bound)
        3. Constrain N to be within these bounds
        4. If N is fixed, apply additional propagation

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        changed = []

        # Get N's domain
        n_deref = store.deref(n_var)
        if n_deref[0] == "BOUND":
            if isinstance(n_deref[2], Int):
                n_value = n_deref[2].value
                n_domain = None
            else:
                return ("fail", None)  # N bound to non-integer
        elif n_deref[0] == "UNBOUND":
            n_root = n_deref[1]
            n_domain = get_domain(store, n_root)
            if n_domain is not None and n_domain.is_empty():
                return ("fail", None)
            if n_domain is None:
                # N is unconstrained - create appropriate domain based on problem size
                # Minimum: 1 distinct value, Maximum: number of variables + fixed values
                max_distinct = len(var_ids) + len(fixed_ground_values)
                n_domain = Domain(((1, max_distinct),))
                # Set the domain for N
                set_domain(store, n_root, n_domain, trail)
            n_value = None
        else:
            return ("fail", None)

        # Gather current state of variables
        domains: Dict[int, "Domain"] = {}
        ground_values: Set[int] = set(
            fixed_ground_values
        )  # Start with fixed ground values
        all_possible_values: Set[int] = set(fixed_ground_values)  # Include fixed values

        for var_id in var_ids:
            deref = store.deref(var_id)
            if deref[0] == "BOUND":
                # Variable is bound to a value
                val = deref[2]
                if isinstance(val, Int):
                    ground_values.add(val.value)
                    all_possible_values.add(val.value)
                else:
                    return ("fail", None)  # Bound to non-integer
            elif deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None or dom.is_empty():
                    return ("fail", None)
                domains[root] = dom
                # Add all possible values from this domain
                for low, high in dom.intervals:
                    for value in range(low, high + 1):
                        all_possible_values.add(value)
            else:
                return ("fail", None)

        # Compute bounds on number of distinct values

        # Minimum distinct values:
        # - At least the ground values are distinct
        # - If variables exist and no ground values, at least 1 (all vars could take same value)
        # - If variables exist and ground values exist, at least len(ground_values)
        #   (variables could all take values already in ground_values)
        if len(var_ids) > 0 and len(ground_values) == 0:
            min_distinct = 1  # At least one distinct value from the variables
        else:
            min_distinct = len(ground_values)

        # Maximum distinct values:
        # - Each variable contributes at most 1 additional distinct value
        # - Limited by available values not already taken by ground values
        if len(var_ids) > 0:
            available_new_values = len(all_possible_values) - len(ground_values)
            max_additional_distinct = min(len(var_ids), available_new_values)
            max_distinct = len(ground_values) + max_additional_distinct
        else:
            # No variables, only ground values
            max_distinct = len(ground_values)

        # Debug logging
        # print(f"DEBUG nvalue: ground_values={ground_values}, all_possible_values={all_possible_values}")
        # print(f"DEBUG nvalue: var_ids={var_ids}, min_distinct={min_distinct}, max_distinct={max_distinct}")

        # Additional refinement for min_distinct:
        # Check if any variable domains are disjoint from ground values and each other
        unassigned_domains = list(domains.values())
        if unassigned_domains:
            # Use pigeonhole reasoning: if we have disjoint sets of values,
            # each set forces at least one distinct value
            disjoint_sets = []
            processed_values = set(ground_values)

            for domain in unassigned_domains:
                domain_values = set()
                for low, high in domain.intervals:
                    for value in range(low, high + 1):
                        domain_values.add(value)
                # Remove already processed values
                new_values = domain_values - processed_values
                if new_values:
                    # This domain can contribute new distinct values
                    disjoint_sets.append(new_values)
                    processed_values.update(new_values)

            # Each variable must take some value, and disjoint domains force distinctness
            # But this is complex to compute exactly, so we use a simple lower bound
            if len(unassigned_domains) > 0:
                # At minimum, we need to satisfy that each variable has a value
                # But this doesn't necessarily increase distinct count beyond ground values
                # Conservative: min_distinct = len(ground_values)
                pass

        # Refined upper bound: can't have more distinct values than total variables plus ground values
        # (This line was incorrectly limiting max_distinct)
        # max_distinct = min(max_distinct, len(var_ids))  # REMOVED - this was wrong!

        # Constrain N based on computed bounds
        if n_value is not None:
            # N is ground - check feasibility
            if n_value < min_distinct or n_value > max_distinct:
                return ("fail", None)
        else:
            # N is variable - constrain its domain
            n_bounds_domain = Domain(((min_distinct, max_distinct),))
            new_n_domain = n_domain.intersect(n_bounds_domain)
            if new_n_domain.is_empty():
                return ("fail", None)
            if new_n_domain.rev != n_domain.rev:
                set_domain(store, n_var, new_n_domain, trail)
                changed.append(n_var)
                n_domain = new_n_domain

        # If N is now singleton, apply stronger propagation
        final_n_value = n_value
        if n_value is None and n_domain.is_singleton():
            final_n_value = n_domain.min()

        if final_n_value is not None:
            # N is fixed - apply additional propagation based on the specific value
            current_distinct = len(ground_values)
            needed_distinct = final_n_value - current_distinct

            if needed_distinct < 0:
                return ("fail", None)  # Too many ground distinct values
            elif needed_distinct == 0:
                # No more distinct values needed - all unassigned variables must take existing values
                allowed_values = ground_values
                if not allowed_values:
                    # No ground values and need 0 distinct total - impossible unless no variables
                    if var_ids:
                        return ("fail", None)
                else:
                    # Constrain all unassigned variables to existing ground values
                    for var_id, domain in domains.items():
                        new_domain = domain
                        # Iterate through all values in domain intervals
                        for low, high in domain.intervals:
                            for value in range(low, high + 1):
                                if value not in allowed_values:
                                    new_domain = new_domain.remove_value(value)
                        if new_domain.is_empty():
                            return ("fail", None)
                        if new_domain.rev != domain.rev:
                            set_domain(store, var_id, new_domain, trail)
                            if var_id not in changed:
                                changed.append(var_id)
            else:
                # needed_distinct > 0: need more distinct values
                # This is harder to propagate without full reasoning
                # For bounds consistency, we mainly ensure feasibility

                # Check if it's possible to achieve the required distinct values
                available_new_values = all_possible_values - ground_values
                if len(available_new_values) < needed_distinct:
                    return ("fail", None)  # Not enough new values available

                # Advanced: if exactly needed_distinct new values are available,
                # we could force some assignments, but this is complex
                # For now, we rely on the bounds constraints already applied

        return ("ok", changed if changed else None)

    return nvalue_propagator
