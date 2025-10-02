"""Table constraint propagator with GAC-lite filtering."""

from typing import Tuple, Optional, List, Set, Dict
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


def create_table_propagator(
    var_ids: List[Optional[int]],
    tuples: List[Tuple[int, ...]],
    ground_values: List[Optional[int]] = None,
):
    """Create table propagator with GAC-lite filtering via support lists.

    Args:
        var_ids: List of variable IDs participating in the constraint (None for ground values)
        tuples: List of valid tuples (each tuple is a sequence of integers)
        ground_values: List of ground values (None for variables)

    Returns:
        A propagator function that enforces the table constraint
    """
    # If ground_values not provided, create empty list
    if ground_values is None:
        ground_values = [None] * len(var_ids)
    # Precompute support structure: for each (var_pos, value), which tuples support it
    supports: Dict[Tuple[int, int], List[int]] = {}

    for tuple_idx, tuple_vals in enumerate(tuples):
        for var_pos, value in enumerate(tuple_vals):
            key = (var_pos, value)
            if key not in supports:
                supports[key] = []
            supports[key].append(tuple_idx)

    def table_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Enforce table constraint via GAC-lite filtering.

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        # Debug print
        # print(f"Table propagator called with {len(tuples)} tuples, {len(var_ids)} variables")
        # Handle empty tuples case
        if not tuples:
            return ("fail", None)

        # Check for arity mismatch or validate ground values first
        for tuple_vals in tuples:
            if len(tuple_vals) != len(var_ids):
                return ("fail", None)

        # Gather current domains and check if any variables are ground
        domains: Dict[int, Domain] = {}
        ground_assignment = ground_values.copy()  # Start with provided ground values

        for i, vid in enumerate(var_ids):
            if vid is None:
                # This represents a ground integer value - skip variable processing
                continue

            deref = store.deref(vid)
            if deref[0] == "BOUND":
                # Variable is bound to a value
                val = deref[2]
                if isinstance(val, Int):
                    ground_assignment[i] = val.value
                else:
                    return ("fail", None)  # Non-integer binding not supported
            elif deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None:
                    continue  # No domain yet - skip for now
                domains[root] = dom
                if dom.is_singleton():
                    ground_assignment[i] = dom.min()

        # Check if current ground assignment (if complete) is valid
        if all(val is not None for val in ground_assignment):
            ground_tuple = tuple(ground_assignment)
            if ground_tuple not in tuples:
                return ("fail", None)
            # All variables are ground and match a valid tuple
            return ("ok", None)

        # Find which tuples are still valid given current domains
        valid_tuples = []
        for tuple_idx, tuple_vals in enumerate(tuples):
            is_valid = True
            for var_pos, value in enumerate(tuple_vals):
                if ground_assignment[var_pos] is not None:
                    # Variable is ground - must match exactly
                    if ground_assignment[var_pos] != value:
                        is_valid = False
                        break
                else:
                    # Variable has domain - value must be in domain
                    vid = var_ids[var_pos]
                    if vid is not None:  # Make sure it's actually a variable
                        root = store.deref(vid)[1]
                        if root in domains:
                            if not domains[root].contains(value):
                                is_valid = False
                                break
            if is_valid:
                valid_tuples.append(tuple_idx)

        if not valid_tuples:
            return ("fail", None)  # No valid tuples remain

        # print(f"Valid tuples: {valid_tuples}")

        # GAC-lite: for each variable, keep only values that have support
        changed = []
        for var_pos, vid in enumerate(var_ids):
            if ground_assignment[var_pos] is not None:
                continue  # Skip ground variables

            deref = store.deref(vid)
            if deref[0] != "UNBOUND":
                continue

            root = deref[1]
            if root not in domains:
                continue

            current_dom = domains[root]
            supported_values = set()

            # Find all values supported by at least one valid tuple
            for tuple_idx in valid_tuples:
                value = tuples[tuple_idx][var_pos]
                if current_dom.contains(value):
                    supported_values.add(value)

            if not supported_values:
                return ("fail", None)  # No supported values

            # print(f"Variable {var_pos} supported values: {supported_values}")

            # Create new domain with only supported values
            new_intervals = []
            for value in sorted(supported_values):
                # Build intervals from consecutive supported values
                if not new_intervals or value != new_intervals[-1][1] + 1:
                    new_intervals.append((value, value))
                else:
                    # Extend last interval
                    new_intervals[-1] = (new_intervals[-1][0], value)

            new_dom = Domain(tuple(new_intervals))
            # print(f"Variable {var_pos} old domain: {current_dom}, new domain: {new_dom}")

            if new_dom.is_empty():
                return ("fail", None)

            # Check if the domain content actually changed (not just revision)
            if new_dom.intervals != current_dom.intervals:
                # print(f"Setting new domain for variable {var_pos}")
                set_domain(store, root, new_dom, trail)
                changed.append(root)
                domains[root] = new_dom  # Update for consistency
            # else:
            # print(f"Domain unchanged for variable {var_pos}")

        return ("ok", changed if changed else None)

    return table_propagator


def create_optimized_table_propagator(
    var_ids: List[Optional[int]],
    tuples: List[Tuple[int, ...]],
    ground_values: List[Optional[int]] = None,
):
    """Create optimized table propagator with cached support structures.

    Args:
        var_ids: List of variable IDs participating in the constraint (None for ground values)
        tuples: List of valid tuples (each tuple is a sequence of integers)
        ground_values: List of ground values (None for variables)

    Returns:
        A propagator function with optimized support checking
    """
    # If ground_values not provided, create empty list
    if ground_values is None:
        ground_values = [None] * len(var_ids)
    # Enhanced support structure with better indexing
    supports_by_var_value: Dict[Tuple[int, int], Set[int]] = {}
    values_by_var: List[Set[int]] = [set() for _ in var_ids]

    # Precompute all supports and collect values per variable position
    for tuple_idx, tuple_vals in enumerate(tuples):
        for var_pos, value in enumerate(tuple_vals):
            key = (var_pos, value)
            if key not in supports_by_var_value:
                supports_by_var_value[key] = set()
            supports_by_var_value[key].add(tuple_idx)
            values_by_var[var_pos].add(value)

    # State for incremental updates
    last_revisions = {}
    cached_valid_tuples = set(range(len(tuples)))

    def optimized_table_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Optimized table propagator with incremental support checking."""
        nonlocal last_revisions, cached_valid_tuples

        if not tuples:
            return ("fail", None)

        # Validate arity
        for tuple_vals in tuples:
            if len(tuple_vals) != len(var_ids):
                return ("fail", None)

        # Gather current state
        domains: Dict[int, Domain] = {}
        current_revisions = {}
        ground_assignment = ground_values.copy()  # Start with provided ground values

        for i, vid in enumerate(var_ids):
            if vid is None:
                # This represents a ground integer value - skip variable processing
                continue

            deref = store.deref(vid)
            if deref[0] == "BOUND":
                val = deref[2]
                if isinstance(val, Int):
                    ground_assignment[i] = val.value
                else:
                    return ("fail", None)
            elif deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None:
                    continue  # No domain yet - skip for now
                domains[root] = dom
                current_revisions[root] = dom.rev
                if dom.is_singleton():
                    ground_assignment[i] = dom.min()

        # Check if we can use incremental update
        changed_vars = []
        for vid, rev in current_revisions.items():
            if vid not in last_revisions or last_revisions[vid] != rev:
                changed_vars.append(vid)

        use_incremental = len(changed_vars) <= max(1, len(var_ids) // 3)

        if use_incremental and cached_valid_tuples:
            # Incremental: filter cached valid tuples
            new_valid_tuples = set()
            for tuple_idx in cached_valid_tuples:
                tuple_vals = tuples[tuple_idx]
                is_valid = True

                for var_pos, value in enumerate(tuple_vals):
                    if ground_assignment[var_pos] is not None:
                        if ground_assignment[var_pos] != value:
                            is_valid = False
                            break
                    else:
                        vid = var_ids[var_pos]
                        root = store.deref(vid)[1]
                        if root in domains and not domains[root].contains(value):
                            is_valid = False
                            break

                if is_valid:
                    new_valid_tuples.add(tuple_idx)

            valid_tuples = new_valid_tuples
        else:
            # Full recomputation
            valid_tuples = set()
            for tuple_idx, tuple_vals in enumerate(tuples):
                is_valid = True

                for var_pos, value in enumerate(tuple_vals):
                    if ground_assignment[var_pos] is not None:
                        if ground_assignment[var_pos] != value:
                            is_valid = False
                            break
                    else:
                        vid = var_ids[var_pos]
                        root = store.deref(vid)[1]
                        if root in domains and not domains[root].contains(value):
                            is_valid = False
                            break

                if is_valid:
                    valid_tuples.add(tuple_idx)

        if not valid_tuples:
            return ("fail", None)

        # Update cache
        cached_valid_tuples = valid_tuples
        last_revisions = current_revisions.copy()

        # Check if all variables are ground
        if all(val is not None for val in ground_assignment):
            return ("ok", None)

        # Apply GAC-lite filtering
        changed = []
        for var_pos, vid in enumerate(var_ids):
            if ground_assignment[var_pos] is not None:
                continue

            deref = store.deref(vid)
            if deref[0] != "UNBOUND":
                continue

            root = deref[1]
            if root not in domains:
                continue

            current_dom = domains[root]

            # Collect supported values efficiently
            supported_values = set()
            for tuple_idx in valid_tuples:
                value = tuples[tuple_idx][var_pos]
                if current_dom.contains(value):
                    supported_values.add(value)

            if not supported_values:
                return ("fail", None)

            # Build new domain from supported values
            sorted_values = sorted(supported_values)
            new_intervals = []

            start = sorted_values[0]
            end = start

            for value in sorted_values[1:]:
                if value == end + 1:
                    end = value
                else:
                    new_intervals.append((start, end))
                    start = end = value

            new_intervals.append((start, end))
            new_dom = Domain(tuple(new_intervals))

            if new_dom.is_empty():
                return ("fail", None)

            # Check if the domain content actually changed (not just revision)
            if new_dom.intervals != current_dom.intervals:
                set_domain(store, root, new_dom, trail)
                changed.append(root)
                domains[root] = new_dom

        return ("ok", changed if changed else None)

    return optimized_table_propagator


def _validate_tuples(tuples: List[Tuple[int, ...]], arity: int) -> bool:
    """Validate that all tuples have correct arity and contain only integers."""
    for tuple_vals in tuples:
        if len(tuple_vals) != arity:
            return False
        for val in tuple_vals:
            if not isinstance(val, int):
                return False
    return True


def _build_support_index(
    tuples: List[Tuple[int, ...]], var_count: int
) -> Dict[Tuple[int, int], List[int]]:
    """Build index mapping (variable_position, value) to supporting tuple indices."""
    supports = {}

    for tuple_idx, tuple_vals in enumerate(tuples):
        for var_pos, value in enumerate(tuple_vals):
            key = (var_pos, value)
            if key not in supports:
                supports[key] = []
            supports[key].append(tuple_idx)

    return supports


def _compute_supported_values(
    var_pos: int, valid_tuples: Set[int], tuples: List[Tuple[int, ...]]
) -> Set[int]:
    """Compute all values supported for a variable position by valid tuples."""
    supported = set()
    for tuple_idx in valid_tuples:
        supported.add(tuples[tuple_idx][var_pos])
    return supported


def _domain_from_values(values: Set[int]) -> Domain:
    """Create domain from a set of values, building optimal intervals."""
    if not values:
        return Domain(())

    sorted_values = sorted(values)
    intervals = []

    start = sorted_values[0]
    end = start

    for value in sorted_values[1:]:
        if value == end + 1:
            end = value
        else:
            intervals.append((start, end))
            start = end = value

    intervals.append((start, end))
    return Domain(tuple(intervals))
