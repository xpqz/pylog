"""Global Cardinality Constraint (GCC) propagator.

Implements global_cardinality/2 constraint where value occurrences are constrained.
Provides bounds consistency on both variable domains and count constraints.
"""

from typing import Tuple, Optional, List, Dict
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


def create_global_cardinality_propagator(
    var_ids: List[int], value_counts: Dict[int, Tuple[int, int]]
):
    """Create global cardinality constraint propagator.

    Args:
        var_ids: List of variable IDs participating in the constraint
        value_counts: Dictionary mapping values to (min_count, max_count) tuples

    Returns:
        A propagator function that enforces global cardinality constraint
    """

    def gcc_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Enforce global cardinality constraint via bounds consistency.

        Algorithm:
        1. Count minimum and maximum possible occurrences for each value
        2. Check feasibility against required counts
        3. Remove values that would violate count constraints
        4. Remove values from variables when count limits are reached

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        changed = []

        # Gather current domains for all variables
        domains: Dict[int, "Domain"] = {}
        ground_values: Dict[int, int] = {}  # var_id -> ground_value

        for var_id in var_ids:
            deref = store.deref(var_id)
            if deref[0] == "BOUND":
                # Variable is bound to a value
                val = deref[2]
                if isinstance(val, Int):
                    ground_values[var_id] = val.value
                else:
                    return ("fail", None)  # Bound to non-integer
            elif deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None or dom.is_empty():
                    return ("fail", None)
                domains[root] = dom
            else:
                return ("fail", None)  # Invalid deref state

        # Count current definite and possible occurrences for each value
        definite_counts = (
            {}
        )  # value -> count of variables definitely bound to this value
        possible_counts = {}  # value -> count of variables that could take this value

        # Initialize counts for all constrained values
        for value in value_counts:
            definite_counts[value] = 0
            possible_counts[value] = 0

        # Count ground values
        for var_id, ground_val in ground_values.items():
            if ground_val in definite_counts:
                definite_counts[ground_val] += 1
            # Also add to possible counts
            if ground_val in possible_counts:
                possible_counts[ground_val] += 1
            # If ground value is not in our constraint set, we need to track it for feasibility
            if ground_val not in value_counts:
                # This ground value is not constrained, which is fine
                pass

        # Count possible occurrences from variable domains
        for var_id, domain in domains.items():
            for value in value_counts:
                if domain.contains(value):
                    possible_counts[value] += 1

        # Check feasibility: definite counts must not exceed max, possible counts must meet min
        for value, (min_count, max_count) in value_counts.items():
            definite = definite_counts.get(value, 0)
            possible = possible_counts.get(value, 0)

            # Feasibility check
            if definite > max_count:
                return ("fail", None)  # Too many definite occurrences
            if possible < min_count:
                return ("fail", None)  # Cannot achieve minimum occurrences

        # Propagation: remove values that would violate constraints
        for var_id, domain in domains.items():
            new_domain = domain

            # Iterate over all values in the domain by going through intervals
            for low, high in domain.intervals:
                for value in range(low, high + 1):
                    if value in value_counts:
                        min_count, max_count = value_counts[value]
                        definite = definite_counts.get(value, 0)

                        # If this variable takes this value, would it violate max count?
                        if definite >= max_count:
                            # Max count already reached, remove this value
                            new_domain = new_domain.remove_value(value)
                            continue

                        # Check if removing this variable's possibility would make min unachievable
                        other_possible = sum(
                            1
                            for other_var_id, other_domain in domains.items()
                            if other_var_id != var_id and other_domain.contains(value)
                        )
                        total_other_possible = definite + other_possible

                        if total_other_possible < min_count:
                            # This variable MUST take this value to achieve minimum
                            # But we don't force it here - that would require stronger consistency
                            pass
                    # Value not mentioned in counts - allowed (unconstrained)

            # Update domain if changed
            if new_domain.is_empty():
                return ("fail", None)

            if new_domain.rev != domain.rev:
                set_domain(store, var_id, new_domain, trail)
                changed.append(var_id)

        # Additional propagation: if a value's min count requires all remaining capable variables
        for value, (min_count, max_count) in value_counts.items():
            definite = definite_counts.get(value, 0)
            needed = min_count - definite

            if needed > 0:
                # Find variables that can still take this value
                capable_vars = []
                for var_id, domain in domains.items():
                    if domain.contains(value):
                        capable_vars.append(var_id)

                # If exactly the right number of variables can take this value, force them
                if len(capable_vars) == needed:
                    for var_id in capable_vars:
                        domain = domains[var_id]
                        new_domain = Domain(((value, value),))  # Singleton domain
                        existing_domain = get_domain(store, var_id)
                        if existing_domain:
                            new_domain = existing_domain.intersect(new_domain)
                        if new_domain.is_empty():
                            return ("fail", None)
                        if new_domain.rev != domain.rev:
                            set_domain(store, var_id, new_domain, trail)
                            if var_id not in changed:
                                changed.append(var_id)
                            domains[var_id] = new_domain  # Update for consistency

        return ("ok", changed if changed else None)

    return gcc_propagator
