"""Element constraint propagator.

Implements element(Index, List, Value) constraint where Value is the Index-th
element of List (1-indexed). Provides bounds consistency with membership pruning.
"""

from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


def create_element_propagator(index_var, list_vars, value_var, list_values):
    """Create element constraint propagator.

    Args:
        index_var: Variable ID for index (None if ground)
        list_vars: List of variable IDs for list elements (None for ground elements)
        value_var: Variable ID for value (None if ground)
        list_values: List of ground values (None for variables)

    Returns:
        Propagator function
    """

    def element_propagator(store, trail, engine, cause):
        """Element constraint propagator implementation.

        Algorithm:
        1. If index is singleton: constrain value to List[index]
        2. If value is ground: constrain index to positions where List[i] can equal value
        3. General case: prune both index and value based on feasible combinations

        Returns:
            (status, changed) where status is "ok" or "fail" and changed is list of var IDs
        """
        changed = []
        debug = False  # Set to True for debugging

        if debug:
            print(f"\n=== Element propagator called with cause: {cause} ===")
            print(f"index_var: {index_var}, value_var: {value_var}")
            print(f"list_vars: {list_vars}")
            print(f"list_values: {list_values}")

        # Get current domains
        index_domain = None
        if index_var is not None:
            index_domain = get_domain(store, index_var)
            if debug:
                print(f"Index domain: {index_domain}")
            if index_domain is None:
                if debug:
                    print("FAIL: Index domain is None")
                return "fail", []  # No domain means failure
            if index_domain.is_empty():
                if debug:
                    print("FAIL: Index domain is empty")
                return "fail", []

        value_domain = None
        if value_var is not None:
            value_domain = get_domain(store, value_var)
            if debug:
                print(f"Value domain: {value_domain}")
            if value_domain is None:
                if debug:
                    print("FAIL: Value domain is None")
                return "fail", []
            if value_domain.is_empty():
                if debug:
                    print("FAIL: Value domain is empty")
                return "fail", []

        list_domains = []
        for i, var_id in enumerate(list_vars):
            if var_id is not None:
                dom = get_domain(store, var_id)
                if debug:
                    print(f"List element {i} domain: {dom}")
                if dom is None:
                    if debug:
                        print(f"FAIL: List element {i} domain is None")
                    return "fail", []
                if dom.is_empty():
                    if debug:
                        print(f"FAIL: List element {i} domain is empty")
                    return "fail", []
                list_domains.append(dom)
            else:
                # Ground element
                if debug:
                    print(f"List element {i} is ground: {list_values[i]}")
                list_domains.append(None)

        # Case 1: Index is singleton
        if index_domain is not None and index_domain.is_singleton():
            if debug:
                print("Case 1: Index is singleton")
            index_val = index_domain.min()
            if debug:
                print(f"Index value: {index_val}")
            if index_val < 1 or index_val > len(list_vars):
                if debug:
                    print(
                        f"FAIL: Index {index_val} out of bounds (1..{len(list_vars)})"
                    )
                return "fail", []  # Index out of bounds

            list_index = index_val - 1  # Convert to 0-based

            # Get the domain/value of the corresponding list element
            if list_vars[list_index] is None:
                # List element is ground
                list_element_value = list_values[list_index]
                if value_var is not None:
                    # Constrain value to this single value
                    new_value_domain = value_domain.intersect(
                        Domain(((list_element_value, list_element_value),))
                    )
                    if new_value_domain.is_empty():
                        return "fail", []
                    if new_value_domain != value_domain:
                        set_domain(store, value_var, new_value_domain, trail)
                        changed.append(value_var)
                else:
                    # Value is also ground - check consistency
                    # This should have been handled in the builtin, but double-check
                    pass
            else:
                # List element is variable
                list_var_id = list_vars[list_index]
                list_element_domain = list_domains[list_index]

                if value_var is not None:
                    # Both list element and value are variables - unify their domains
                    intersection = list_element_domain.intersect(value_domain)
                    if intersection.is_empty():
                        return "fail", []

                    if intersection != list_element_domain:
                        set_domain(store, list_var_id, intersection, trail)
                        changed.append(list_var_id)

                    if intersection != value_domain:
                        set_domain(store, value_var, intersection, trail)
                        changed.append(value_var)
                else:
                    # Value is ground, list element is variable
                    # This should have been handled in the builtin, but ensure consistency
                    pass

        # Case 2: Value is singleton (and index is not)
        elif value_domain is not None and value_domain.is_singleton():
            value_val = value_domain.min()

            # Find all list positions that could contain this value
            valid_indices = []
            for i in range(len(list_vars)):
                if list_vars[i] is None:
                    # Ground list element
                    if list_values[i] == value_val:
                        valid_indices.append(i + 1)  # 1-based
                else:
                    # Variable list element
                    if list_domains[i].contains(value_val):
                        valid_indices.append(i + 1)  # 1-based

            if not valid_indices:
                return "fail", []  # Value cannot occur anywhere

            if index_var is not None:
                # Constrain index to valid positions
                intervals = tuple((idx, idx) for idx in valid_indices)
                new_index_domain = index_domain.intersect(Domain(intervals))
                if new_index_domain.is_empty():
                    return "fail", []
                if new_index_domain != index_domain:
                    set_domain(store, index_var, new_index_domain, trail)
                    changed.append(index_var)

                # If index is now singleton, we can prune the corresponding list element
                if new_index_domain.is_singleton():
                    final_index = new_index_domain.min()
                    list_index = final_index - 1
                    if list_vars[list_index] is not None:
                        list_var_id = list_vars[list_index]
                        list_element_domain = list_domains[list_index]
                        new_list_domain = list_element_domain.intersect(
                            Domain(((value_val, value_val),))
                        )
                        if new_list_domain.is_empty():
                            return "fail", []
                        if new_list_domain != list_element_domain:
                            set_domain(store, list_var_id, new_list_domain, trail)
                            changed.append(list_var_id)

        # Case 3: General case - prune domains based on feasible combinations
        else:
            if debug:
                print("Case 3: General case")
            # This is the most complex case where we need to do full propagation

            # Step 1: Prune value domain to union of possible list values
            if value_var is not None and index_var is not None:
                # Calculate bounds of possible values
                min_possible = None
                max_possible = None
                value_intervals = []

                for i in range(len(list_vars)):
                    index_1based = i + 1
                    if index_domain.contains(index_1based):
                        if list_vars[i] is None:
                            # Ground list element
                            val = list_values[i]
                            value_intervals.append((val, val))
                            if min_possible is None or val < min_possible:
                                min_possible = val
                            if max_possible is None or val > max_possible:
                                max_possible = val
                        else:
                            # Variable list element - use its domain bounds
                            list_dom = list_domains[i]
                            if not list_dom.is_empty():
                                # Add this domain's intervals to possible values
                                value_intervals.extend(list_dom.intervals)
                                dom_min = list_dom.min()
                                dom_max = list_dom.max()
                                if min_possible is None or dom_min < min_possible:
                                    min_possible = dom_min
                                if max_possible is None or dom_max > max_possible:
                                    max_possible = dom_max

                if value_intervals:
                    # Create domain from all possible intervals
                    possible_domain = Domain(tuple(value_intervals))
                    new_value_domain = value_domain.intersect(possible_domain)
                    if new_value_domain.is_empty():
                        return "fail", []
                    if new_value_domain != value_domain:
                        set_domain(store, value_var, new_value_domain, trail)
                        changed.append(value_var)
                        value_domain = new_value_domain  # Update for next step

            # Step 2: Prune index domain to positions where list element can equal value
            if index_var is not None and value_var is not None:
                if debug:
                    print("Step 2: Pruning index domain")
                valid_indices = []
                for i in range(len(list_vars)):
                    index_1based = i + 1
                    if index_domain.contains(index_1based):
                        if list_vars[i] is None:
                            # Ground list element
                            if value_domain.contains(list_values[i]):
                                valid_indices.append(index_1based)
                                if debug:
                                    print(
                                        f"  Index {index_1based}: ground element {list_values[i]} is in value domain"
                                    )
                            else:
                                if debug:
                                    print(
                                        f"  Index {index_1based}: ground element {list_values[i]} NOT in value domain"
                                    )
                        else:
                            # Variable list element - check if domains intersect
                            intersection = list_domains[i].intersect(value_domain)
                            if not intersection.is_empty():
                                valid_indices.append(index_1based)
                                if debug:
                                    print(
                                        f"  Index {index_1based}: variable domain {list_domains[i]} intersects value domain {value_domain} -> {intersection}"
                                    )
                            else:
                                if debug:
                                    print(
                                        f"  Index {index_1based}: variable domain {list_domains[i]} does NOT intersect value domain {value_domain}"
                                    )

                if debug:
                    print(f"Valid indices: {valid_indices}")

                if not valid_indices:
                    if debug:
                        print("FAIL: No valid indices")
                    return "fail", []

                intervals = tuple((idx, idx) for idx in valid_indices)
                new_index_domain = index_domain.intersect(Domain(intervals))
                if debug:
                    print(f"New index domain: {new_index_domain}")
                if new_index_domain.is_empty():
                    if debug:
                        print("FAIL: New index domain is empty")
                    return "fail", []
                if new_index_domain != index_domain:
                    if debug:
                        print(f"Setting index domain to: {new_index_domain}")
                    set_domain(store, index_var, new_index_domain, trail)
                    changed.append(index_var)
                    # Update the index domain for use in Step 3
                    index_domain = new_index_domain

            # Step 3: Prune list element domains
            if debug:
                print("Step 3: Pruning list element domains")
            for i in range(len(list_vars)):
                if list_vars[i] is not None:  # Only variable list elements
                    index_1based = i + 1
                    list_var_id = list_vars[i]
                    current_list_domain = list_domains[i]

                    if debug:
                        print(f"  Processing list element {i} (index {index_1based})")

                    # If this index is not possible, don't constrain the list element
                    if index_var is not None and not index_domain.contains(
                        index_1based
                    ):
                        if debug:
                            print(
                                f"    Skipping: index {index_1based} not in index domain {index_domain}"
                            )
                        continue

                    # If value is constrained, intersect with value domain
                    if value_var is not None:
                        new_list_domain = current_list_domain.intersect(value_domain)
                        if debug:
                            print(
                                f"    Intersecting {current_list_domain} with {value_domain} -> {new_list_domain}"
                            )
                        if new_list_domain.is_empty():
                            # This position can't hold any valid value
                            # If this index is still possible, we have a failure
                            if index_var is None or index_domain.contains(index_1based):
                                if debug:
                                    print(
                                        f"    FAIL: Empty intersection for position {index_1based} which is still possible"
                                    )
                                return "fail", []
                        elif new_list_domain != current_list_domain:
                            if debug:
                                print(
                                    f"    Setting list element {i} domain to: {new_list_domain}"
                                )
                            set_domain(store, list_var_id, new_list_domain, trail)
                            changed.append(list_var_id)
                        else:
                            if debug:
                                print(f"    No change needed for list element {i}")

        if debug:
            print(f"Returning: ok, changed={changed}")
        return "ok", changed

    return element_propagator
