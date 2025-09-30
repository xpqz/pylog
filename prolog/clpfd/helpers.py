"""Helper utilities for vector/global constraints.

Provides batch watcher management, domain operations over variable sets,
and utilities for implementing multi-variable global constraints.
"""

from typing import List, Tuple, Optional, Dict, Set
from prolog.clpfd.api import add_watcher, get_domain
from prolog.clpfd.priority import Priority
from prolog.clpfd.domain import Domain


def attach_watchers_to_array(
    store, var_ids: List[int], pid: int, priority: Priority, trail
):
    """Attach a propagator to all variables in an array.

    Args:
        store: Variable store
        var_ids: List of variable IDs to attach to
        pid: Propagator ID
        priority: Priority level for the watchers
        trail: Trail for backtracking
    """
    for vid in var_ids:
        add_watcher(store, vid, pid, priority, trail)


def attach_watchers_with_priorities(
    store, var_ids: List[int], watcher_specs: List[Tuple[int, Priority]], trail
):
    """Attach multiple propagators with different priorities to variable array.

    Args:
        store: Variable store
        var_ids: List of variable IDs to attach to
        watcher_specs: List of (propagator_id, priority) tuples
        trail: Trail for backtracking
    """
    for vid in var_ids:
        for pid, priority in watcher_specs:
            add_watcher(store, vid, pid, priority, trail)


def union_domains_across_variables(store, var_ids: List[int]) -> Domain:
    """Compute union of domains across a list of variables.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Domain representing the union of all variable domains
    """
    all_intervals = []

    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and not domain.is_empty():
            all_intervals.extend(domain.intervals)

    return Domain(tuple(all_intervals))


def intersect_feasible_values(store, var_ids: List[int]) -> Domain:
    """Compute intersection of feasible values across variables.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Domain representing the intersection of all variable domains
    """
    if not var_ids:
        return Domain(())

    # Start with first non-empty domain
    result_domain = None
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and not domain.is_empty():
            if result_domain is None:
                result_domain = domain
            else:
                result_domain = result_domain.intersect(domain)
                if result_domain.is_empty():
                    return result_domain

    return result_domain if result_domain is not None else Domain(())


def compute_total_cardinality(store, var_ids: List[int]) -> int:
    """Compute total cardinality across all variable domains.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Sum of domain sizes across all variables
    """
    total = 0
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None:
            total += domain.size()
    return total


def compute_min_cardinality(store, var_ids: List[int]) -> int:
    """Compute minimum cardinality across variable domains.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Minimum domain size across all variables (0 if any domain is empty)
    """
    if not var_ids:
        return 0

    min_size = float("inf")
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is None:
            continue
        size = domain.size()
        if size == 0:
            return 0  # Empty domain means constraint fails
        min_size = min(min_size, size)

    return int(min_size) if min_size != float("inf") else 0


def extract_global_bounds(
    store, var_ids: List[int]
) -> Tuple[Optional[int], Optional[int]]:
    """Extract global minimum and maximum values from variable domains.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Tuple of (global_min, global_max) or (None, None) if no domains
    """
    global_min = None
    global_max = None

    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and not domain.is_empty():
            if global_min is None or domain.min() < global_min:
                global_min = domain.min()
            if global_max is None or domain.max() > global_max:
                global_max = domain.max()

    return global_min, global_max


def collect_singleton_values(store, var_ids: List[int]) -> Set[int]:
    """Collect values from variables with singleton domains.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Set of integer values from singleton domains
    """
    singletons = set()
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and domain.is_singleton():
            singletons.add(domain.min())
    return singletons


def get_variable_domains(store, var_ids: List[int]) -> Dict[int, Domain]:
    """Get domains for all variables that have them.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Dictionary mapping variable IDs to their domains
    """
    domains = {}
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None:
            domains[vid] = domain
    return domains


def filter_variables_by_domain_size(
    store, var_ids: List[int], min_size: int = None, max_size: int = None
) -> List[int]:
    """Filter variables by domain size criteria.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        min_size: Minimum domain size (inclusive), None for no limit
        max_size: Maximum domain size (inclusive), None for no limit

    Returns:
        List of variable IDs meeting the size criteria
    """
    filtered = []
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is None:
            continue

        size = domain.size()
        if min_size is not None and size < min_size:
            continue
        if max_size is not None and size > max_size:
            continue

        filtered.append(vid)

    return filtered


def partition_variables_by_domain_status(
    store, var_ids: List[int]
) -> Tuple[List[int], List[int], List[int]]:
    """Partition variables by domain status.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Tuple of (singleton_vars, multi_value_vars, no_domain_vars)
    """
    singletons = []
    multi_value = []
    no_domain = []

    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is None:
            no_domain.append(vid)
        elif domain.is_singleton():
            singletons.append(vid)
        elif not domain.is_empty():
            multi_value.append(vid)
        # Empty domains are treated as constraint failures elsewhere

    return singletons, multi_value, no_domain


def create_value_frequency_map(store, var_ids: List[int]) -> Dict[int, int]:
    """Create a map of values to their frequency across variable domains.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Dictionary mapping values to frequency counts
    """
    frequency = {}

    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is None or domain.is_empty():
            continue

        # Count each value in the domain
        for low, high in domain.intervals:
            for value in range(low, high + 1):
                frequency[value] = frequency.get(value, 0) + 1

    return frequency


def find_variables_containing_value(store, var_ids: List[int], value: int) -> List[int]:
    """Find all variables whose domains contain a specific value.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        value: Value to search for

    Returns:
        List of variable IDs whose domains contain the value
    """
    containing_vars = []
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and domain.contains(value):
            containing_vars.append(vid)
    return containing_vars
