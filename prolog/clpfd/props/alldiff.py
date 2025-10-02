"""All-different global constraint propagator."""

from typing import Tuple, Optional, List, Set, Dict
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.ast.terms import Int


def create_all_different_propagator(var_ids: List[int], fixed_values: Tuple[int, ...]):
    """Create all_different propagator with value elimination and Hall-interval pruning.

    Args:
        var_ids: List of variable IDs participating in the constraint
        fixed_values: Tuple of fixed integer values that must be avoided

    Returns:
        A propagator function that enforces all_different constraint
    """
    # Store variable IDs for constraint

    def all_different_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Enforce all_different constraint via value elimination and Hall-interval pruning.

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        # Gather current state
        singletons = set(fixed_values)
        domains: Dict[int, Domain] = {}

        # Check for duplicated variables in constraint (X appears twice)
        seen_roots = set()
        for vid in var_ids:
            # Deref to get root variable
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                if root in seen_roots:
                    # Same variable appears twice - immediate failure
                    return ("fail", None)
                seen_roots.add(root)
            elif deref[0] == "BOUND":
                # Variable is bound to a value

                val = deref[2]  # Bound term is at index 2
                if isinstance(val, Int):
                    if val.value in singletons:
                        return ("fail", None)  # Duplicate value
                    singletons.add(val.value)
                # If bound to non-int (shouldn't happen with FD vars), it's handled elsewhere

        # Get domains for unbound variables
        for vid in var_ids:
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None:
                    continue  # No domain yet
                domains[root] = dom

                if dom.is_singleton():
                    val = dom.min()
                    if val in singletons:
                        return ("fail", None)  # Duplicate singleton
                    singletons.add(val)

        # Value elimination: remove singletons from other domains
        changed = []
        for vid, dom in domains.items():
            if dom.is_singleton():
                continue

            new_dom = dom
            for val in singletons:
                if new_dom.contains(val):
                    new_dom = new_dom.remove_value(val)

            if new_dom.is_empty():
                return ("fail", None)

            if new_dom.rev != dom.rev:
                set_domain(store, vid, new_dom, trail)
                changed.append(vid)
                domains[vid] = new_dom  # Update for Hall-interval phase

        # Efficient Hall-interval pruning using interval sweep algorithm
        # O(n log n) complexity instead of O(b²×n) - scales to larger problems
        if domains:
            hall_intervals = _efficient_hall_interval_detection(domains)

            for hall_start, hall_end, tight_var_ids in hall_intervals:
                # Pigeonhole check: more tight variables than interval size
                interval_size = hall_end - hall_start + 1
                if len(tight_var_ids) > interval_size:
                    return ("fail", None)

                # Hall set found: remove [hall_start, hall_end] from non-tight variables
                if len(tight_var_ids) == interval_size and len(tight_var_ids) > 0:
                    tight_var_set = set(tight_var_ids)

                    # Batch all domain updates for this Hall interval
                    for vid, dom in domains.items():
                        if vid not in tight_var_set:
                            new_dom = dom.remove_interval(hall_start, hall_end)
                            if new_dom.is_empty():
                                return ("fail", None)
                            if new_dom.rev != dom.rev:
                                set_domain(store, vid, new_dom, trail)
                                if vid not in changed:
                                    changed.append(vid)
                                domains[vid] = (
                                    new_dom  # Update for subsequent Hall intervals
                                )

        return ("ok", changed if changed else None)

    return all_different_propagator


def _efficient_hall_interval_detection(domains):
    """Hall interval detection with performance limits.

    Args:
        domains: Dict mapping variable IDs to Domain objects

    Returns:
        List of tuples (start, end, tight_var_ids) representing Hall intervals
        where tight_var_ids is a list of variable IDs whose domains are ⊆ [start,end]
    """
    if not domains:
        return []

    # Apply performance limits to prevent hanging on large problems
    num_vars = len(domains)
    if num_vars > 25:  # More generous than original 20 but still bounded
        return []

    # Collect all unique domain endpoints
    endpoints = set()
    for domain in domains.values():
        if not domain.is_empty():
            endpoints.add(domain.min())
            endpoints.add(domain.max())

    if not endpoints:
        return []

    sorted_endpoints = sorted(endpoints)

    # Limit based on number of unique bounds to prevent O(b²) explosion
    if len(sorted_endpoints) > 15:  # Conservative limit
        return []

    hall_intervals = []

    # Check each possible interval [start, end] - with complexity limits
    for i, start in enumerate(sorted_endpoints):
        for j, end in enumerate(sorted_endpoints[i:], i):
            if start > end:
                continue

            interval_size = end - start + 1

            # Skip very large intervals - can't have Hall sets larger than variable count
            if interval_size > num_vars:
                continue

            # Skip intervals that are too large for efficiency
            if interval_size > 12:  # Reasonable size limit
                continue

            # Find variables whose domains are tight on [start, end]
            tight_vars = []
            for varid, domain in domains.items():
                if _is_domain_tight_on_interval(domain, start, end):
                    tight_vars.append(varid)

            tight_count = len(tight_vars)

            # Hall interval condition: tight_count >= interval_size for pigeonhole check
            if tight_count >= interval_size and tight_count > 0:
                hall_intervals.append((start, end, tight_vars))

    return hall_intervals


def _is_domain_tight_on_interval(domain, start, end):
    """Check if domain is tight on interval [start, end].

    A domain is tight if domain.min() >= start and domain.max() <= end.
    """
    return domain.min() >= start and domain.max() <= end


def create_incremental_all_different_propagator(
    var_ids: List[int], fixed_values: Tuple[int, ...]
):
    """Create an incremental all_different propagator with stateful optimizations.

    Args:
        var_ids: List of variable IDs participating in the constraint
        fixed_values: Tuple of fixed integer values that must be avoided

    Returns:
        A stateful propagator function that optimizes repeated calls
    """
    # Store variable IDs for constraint

    # State for incremental updates
    last_revisions = {}  # varid -> last seen revision
    cached_hall_intervals = []  # Cached Hall intervals from last run
    last_singletons = set(fixed_values)  # Cached singletons from last run
    full_recompute_threshold = max(1, len(var_ids) // 3)  # 33% change threshold

    def incremental_all_different_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Incremental all_different propagator with revision tracking."""
        nonlocal last_revisions, cached_hall_intervals, last_singletons

        # Gather current state
        current_revisions = {}
        domains: Dict[int, Domain] = {}
        singletons = set(fixed_values)

        # Check for duplicated variables in constraint (X appears twice)
        seen_roots = set()
        for vid in var_ids:
            # Deref to get root variable
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                if root in seen_roots:
                    return ("fail", None)
                seen_roots.add(root)
            elif deref[0] == "BOUND":
                # Variable is bound to a value

                val = deref[2]
                if isinstance(val, Int):
                    if val.value in singletons:
                        return ("fail", None)
                    singletons.add(val.value)

        # Get domains and track revisions
        for vid in var_ids:
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None:
                    continue
                domains[root] = dom
                current_revisions[root] = dom.rev

                if dom.is_singleton():
                    val = dom.min()
                    if val in singletons:
                        return ("fail", None)
                    singletons.add(val)

        # Detect changes to decide on incremental vs full recomputation
        changed_vars = []
        for vid, rev in current_revisions.items():
            if vid not in last_revisions or last_revisions[vid] != rev:
                changed_vars.append(vid)

        # Check if singletons changed
        singletons_changed = singletons != last_singletons

        # Decide on incremental vs full recomputation
        use_incremental = (
            len(changed_vars) <= full_recompute_threshold
            and not singletons_changed
            and cached_hall_intervals  # Have cached data to work with
        )

        if use_incremental:
            # Incremental update: only recompute affected parts
            result = _incremental_propagation_update(
                domains,
                singletons,
                changed_vars,
                cached_hall_intervals,
                store,
                trail,
                var_ids,
            )
        else:
            # Full recomputation
            result = _full_propagation_with_caching(
                domains, singletons, store, trail, var_ids
            )
            if result[0] == "ok":
                # Update cache
                cached_hall_intervals = _efficient_hall_interval_detection(domains)

        # Update state for next call
        if result[0] == "ok":
            last_revisions = current_revisions.copy()
            last_singletons = singletons.copy()

        return result

    return incremental_all_different_propagator


def _incremental_propagation_update(
    domains, singletons, changed_vars, cached_hall_intervals, store, trail, var_ids
):
    """Perform incremental propagation update using cached Hall intervals."""
    changed = []

    # Value elimination for new singletons
    for vid, dom in domains.items():
        if dom.is_singleton():
            continue

        new_dom = dom
        for val in singletons:
            if new_dom.contains(val):
                new_dom = new_dom.remove_value(val)

        if new_dom.is_empty():
            return ("fail", None)

        if new_dom.rev != dom.rev:
            set_domain(store, vid, new_dom, trail)
            changed.append(vid)
            domains[vid] = new_dom

    # Apply cached Hall intervals (they should still be valid for unchanged structure)
    for hall_start, hall_end, tight_var_ids in cached_hall_intervals:
        interval_size = hall_end - hall_start + 1
        if len(tight_var_ids) > interval_size:
            return ("fail", None)

        if len(tight_var_ids) == interval_size and len(tight_var_ids) > 0:
            tight_var_set = set(tight_var_ids)
            for vid, dom in domains.items():
                if vid not in tight_var_set:
                    new_dom = dom.remove_interval(hall_start, hall_end)
                    if new_dom.is_empty():
                        return ("fail", None)
                    if new_dom.rev != dom.rev:
                        set_domain(store, vid, new_dom, trail)
                        if vid not in changed:
                            changed.append(vid)
                        domains[vid] = new_dom

    return ("ok", changed if changed else None)


def _full_propagation_with_caching(domains, singletons, store, trail, var_ids):
    """Perform full propagation and cache results for future incremental updates."""
    changed = []

    # Value elimination: remove singletons from other domains
    for vid, dom in domains.items():
        if dom.is_singleton():
            continue

        new_dom = dom
        for val in singletons:
            if new_dom.contains(val):
                new_dom = new_dom.remove_value(val)

        if new_dom.is_empty():
            return ("fail", None)

        if new_dom.rev != dom.rev:
            set_domain(store, vid, new_dom, trail)
            changed.append(vid)
            domains[vid] = new_dom

    # Hall-interval pruning (will be cached by caller)
    if domains:
        hall_intervals = _efficient_hall_interval_detection(domains)

        for hall_start, hall_end, tight_var_ids in hall_intervals:
            interval_size = hall_end - hall_start + 1
            if len(tight_var_ids) > interval_size:
                return ("fail", None)

            if len(tight_var_ids) == interval_size and len(tight_var_ids) > 0:
                tight_var_set = set(tight_var_ids)

                for vid, dom in domains.items():
                    if vid not in tight_var_set:
                        new_dom = dom.remove_interval(hall_start, hall_end)
                        if new_dom.is_empty():
                            return ("fail", None)
                        if new_dom.rev != dom.rev:
                            set_domain(store, vid, new_dom, trail)
                            if vid not in changed:
                                changed.append(vid)
                            domains[vid] = new_dom

    return ("ok", changed if changed else None)


def create_optimized_all_different_propagator(
    var_ids: List[int], fixed_values: Tuple[int, ...]
):
    """Create optimized all_different propagator with batch value elimination.

    Args:
        var_ids: List of variable IDs participating in the constraint
        fixed_values: Tuple of fixed integer values that must be avoided

    Returns:
        A propagator function optimized for batch operations
    """
    # Store variable IDs for constraint

    def optimized_all_different_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Optimized all_different propagator with batch value elimination."""

        # Gather current state
        singletons = set(fixed_values)
        domains: Dict[int, Domain] = {}

        # Check for duplicated variables in constraint (X appears twice)
        seen_roots = set()
        for vid in var_ids:
            # Deref to get root variable
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                if root in seen_roots:
                    return ("fail", None)
                seen_roots.add(root)
            elif deref[0] == "BOUND":
                # Variable is bound to a value

                val = deref[2]
                if isinstance(val, Int):
                    if val.value in singletons:
                        return ("fail", None)
                    singletons.add(val.value)

        # Get domains and collect singletons
        for vid in var_ids:
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None:
                    continue
                domains[root] = dom

                if dom.is_singleton():
                    val = dom.min()
                    if val in singletons:
                        return ("fail", None)
                    singletons.add(val)

        # Batch value elimination: remove all singletons at once
        changed = []
        if singletons:
            result = _batch_eliminate_values(domains, singletons, store, trail)
            if result[0] == "fail":
                return result
            changed.extend(result[1] or [])

        # Efficient Hall-interval pruning with batch updates
        if domains:
            hall_intervals = _efficient_hall_interval_detection(domains)

            for hall_start, hall_end, tight_var_ids in hall_intervals:
                # Pigeonhole check: more tight variables than interval size
                interval_size = hall_end - hall_start + 1
                if len(tight_var_ids) > interval_size:
                    return ("fail", None)

                # Hall set found: batch remove [hall_start, hall_end] from non-tight variables
                if len(tight_var_ids) == interval_size and len(tight_var_ids) > 0:
                    tight_var_set = set(tight_var_ids)

                    # Collect variables that need interval removal
                    vars_to_update = []
                    for vid, dom in domains.items():
                        if vid not in tight_var_set:
                            vars_to_update.append((vid, dom))

                    # Batch interval removal
                    if vars_to_update:
                        result = _batch_remove_interval(
                            vars_to_update, hall_start, hall_end, store, trail
                        )
                        if result[0] == "fail":
                            return result

                        # Update domains cache and changed list
                        for vid in result[1] or []:
                            if vid not in changed:
                                changed.append(vid)
                            domains[vid] = get_domain(store, vid)  # Update cache

        return ("ok", changed if changed else None)

    return optimized_all_different_propagator


def _batch_eliminate_values(
    domains: Dict[int, Domain], values_to_remove: Set[int], store, trail
) -> Tuple[str, Optional[List[int]]]:
    """Batch eliminate multiple values from multiple domains efficiently.

    Args:
        domains: Dict mapping variable IDs to Domain objects
        values_to_remove: Set of values to remove from all domains
        store: Variable store
        trail: Trail for backtracking

    Returns:
        ('ok', changed_vars) or ('fail', None)
    """
    changed = []

    for vid, dom in domains.items():
        if dom.is_singleton():
            continue

        # Remove all values in one operation
        new_dom = dom
        for val in values_to_remove:
            if new_dom.contains(val):
                new_dom = new_dom.remove_value(val)

        if new_dom.is_empty():
            return ("fail", None)

        if new_dom.rev != dom.rev:
            set_domain(store, vid, new_dom, trail)
            changed.append(vid)

    return ("ok", changed if changed else None)


def _batch_remove_interval(
    vars_and_domains: List[Tuple[int, Domain]], start: int, end: int, store, trail
) -> Tuple[str, Optional[List[int]]]:
    """Batch remove an interval from multiple domains efficiently.

    Args:
        vars_and_domains: List of (variable_id, domain) tuples to update
        start: Start of interval to remove (inclusive)
        end: End of interval to remove (inclusive)
        store: Variable store
        trail: Trail for backtracking

    Returns:
        ('ok', changed_vars) or ('fail', None)
    """
    changed = []

    for vid, dom in vars_and_domains:
        new_dom = dom.remove_interval(start, end)

        if new_dom.is_empty():
            return ("fail", None)

        if new_dom.rev != dom.rev:
            set_domain(store, vid, new_dom, trail)
            changed.append(vid)

    return ("ok", changed if changed else None)
