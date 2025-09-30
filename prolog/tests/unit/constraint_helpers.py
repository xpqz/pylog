"""Test infrastructure helpers for global constraint testing.

Utilities for creating test scenarios, setting up domains, and asserting
constraint properties. Designed for use in global constraint unit tests.
"""

import random
import time
from typing import List, Tuple
from prolog.ast.terms import Var
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import set_domain, get_domain


def create_test_variable_array(
    store, count: int, prefix: str = "Var"
) -> Tuple[List[int], List[Var]]:
    """Create an array of test variables with consistent naming.

    Args:
        store: Variable store
        count: Number of variables to create
        prefix: Prefix for variable hints

    Returns:
        Tuple of (variable_ids, variable_terms)
    """
    var_ids = []
    var_terms = []

    for i in range(count):
        vid = store.new_var(f"{prefix}{i}")
        var_ids.append(vid)
        var_terms.append(Var(vid, f"{prefix}{i}"))

    return var_ids, var_terms


def setup_uniform_domains(store, var_ids: List[int], low: int, high: int, trail):
    """Set uniform domains for all variables in an array.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        low: Lower bound for domain
        high: Upper bound for domain
        trail: Trail for backtracking
    """
    domain = Domain(((low, high),))
    for vid in var_ids:
        set_domain(store, vid, domain, trail)


def setup_distinct_domains(store, var_ids: List[int], trail, base_value: int = 1):
    """Set up distinct non-overlapping domains for variables.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        trail: Trail for backtracking
        base_value: Starting value for first domain
    """
    for i, vid in enumerate(var_ids):
        low = base_value + i * 10
        high = low + 5
        domain = Domain(((low, high),))
        set_domain(store, vid, domain, trail)


def setup_singleton_domains(store, var_ids: List[int], values: List[int], trail):
    """Set singleton domains for variables with specific values.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        values: List of singleton values (must match var_ids length)
        trail: Trail for backtracking
    """
    if len(var_ids) != len(values):
        raise ValueError("var_ids and values must have same length")

    for vid, value in zip(var_ids, values):
        domain = Domain(((value, value),))
        set_domain(store, vid, domain, trail)


def setup_overlapping_domains(store, var_ids: List[int], trail):
    """Set up domains with partial overlaps for testing.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        trail: Trail for backtracking
    """
    if len(var_ids) < 2:
        return

    # Create overlapping pattern: [1,5], [3,7], [5,9], etc.
    for i, vid in enumerate(var_ids):
        low = 1 + i * 2
        high = low + 4
        domain = Domain(((low, high),))
        set_domain(store, vid, domain, trail)


def setup_conflicting_domains(store, var_ids: List[int], trail):
    """Set up conflicting domains to test constraint failure cases.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        trail: Trail for backtracking
    """
    for i, vid in enumerate(var_ids):
        if i == 0:
            # First variable gets empty domain
            domain = Domain(())
        else:
            # Others get normal domains
            domain = Domain(((i, i + 5),))
        set_domain(store, vid, domain, trail)


def assert_all_different_property(store, var_ids: List[int]):
    """Assert that variable domains satisfy all-different property.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Raises:
        AssertionError: If all-different property is violated
    """
    singleton_values = set()

    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and domain.is_singleton():
            value = domain.min()
            if value in singleton_values:
                raise AssertionError(
                    f"All-different property violated: value {value} appears in multiple singleton domains"
                )
            singleton_values.add(value)


def assert_domain_consistency(store, var_ids: List[int]):
    """Assert that all variable domains are non-empty and consistent.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Raises:
        AssertionError: If any domain is empty or inconsistent
    """
    for vid in var_ids:
        domain = get_domain(store, vid)
        if domain is not None and domain.is_empty():
            raise AssertionError(f"Variable {vid} has empty domain")


def assert_global_bounds(
    store, var_ids: List[int], expected_min: int, expected_max: int
):
    """Assert global bounds across all variable domains.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        expected_min: Expected global minimum
        expected_max: Expected global maximum

    Raises:
        AssertionError: If bounds don't match expectations
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

    if global_min != expected_min:
        raise AssertionError(f"Expected global min {expected_min}, got {global_min}")
    if global_max != expected_max:
        raise AssertionError(f"Expected global max {expected_max}, got {global_max}")


def measure_propagation_time(propagation_func, *args, **kwargs) -> float:
    """Measure time taken by a propagation function.

    Args:
        propagation_func: Function to measure
        *args: Arguments to pass to function
        **kwargs: Keyword arguments to pass to function

    Returns:
        Elapsed time in seconds
    """
    start_time = time.time()
    propagation_func(*args, **kwargs)
    elapsed = time.time() - start_time
    return elapsed


def create_performance_test_arrays(
    store, sizes: List[int]
) -> List[Tuple[List[int], List[Var]]]:
    """Create multiple test arrays of different sizes for performance testing.

    Args:
        store: Variable store
        sizes: List of array sizes to create

    Returns:
        List of (var_ids, var_terms) tuples for each size
    """
    arrays = []
    for size in sizes:
        var_ids, var_terms = create_test_variable_array(store, size, f"Perf{size}")
        arrays.append((var_ids, var_terms))
    return arrays


def setup_random_domains(
    store,
    var_ids: List[int],
    trail,
    min_val: int = 1,
    max_val: int = 100,
    seed: int = 42,
):
    """Set up pseudo-random domains for testing edge cases.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        trail: Trail for backtracking
        min_val: Minimum possible value
        max_val: Maximum possible value
        seed: Random seed for reproducibility
    """
    random.seed(seed)

    for i, vid in enumerate(var_ids):
        # Create random intervals
        num_intervals = random.randint(1, 3)
        intervals = []

        for _ in range(num_intervals):
            low = random.randint(min_val, max_val - 5)
            high = random.randint(low, min(low + 10, max_val))
            intervals.append((low, high))

        domain = Domain(tuple(intervals))
        set_domain(store, vid, domain, trail)


def verify_propagation_fixpoint(store, var_ids: List[int], original_domains: dict):
    """Verify that propagation has reached a fixpoint.

    Args:
        store: Variable store
        var_ids: List of variable IDs
        original_domains: Dictionary of var_id -> original domain

    Returns:
        True if domains have changed (indicating propagation occurred)
    """
    changed = False
    for vid in var_ids:
        current_domain = get_domain(store, vid)
        original_domain = original_domains.get(vid)

        if current_domain != original_domain:
            changed = True

        # Verify domain is only smaller, never larger
        if original_domain is not None and current_domain is not None:
            if not current_domain.is_empty() and not original_domain.is_empty():
                assert current_domain.min() >= original_domain.min()
                assert current_domain.max() <= original_domain.max()

    return changed


def capture_domain_snapshot(store, var_ids: List[int]) -> dict:
    """Capture a snapshot of current domain state.

    Args:
        store: Variable store
        var_ids: List of variable IDs

    Returns:
        Dictionary mapping variable IDs to their current domains
    """
    snapshot = {}
    for vid in var_ids:
        snapshot[vid] = get_domain(store, vid)
    return snapshot


def assert_monotonic_propagation(before_snapshot: dict, after_snapshot: dict):
    """Assert that propagation was monotonic (domains only shrunk).

    Args:
        before_snapshot: Domain snapshot before propagation
        after_snapshot: Domain snapshot after propagation

    Raises:
        AssertionError: If any domain expanded
    """
    for vid in before_snapshot:
        before_domain = before_snapshot[vid]
        after_domain = after_snapshot.get(vid)

        if before_domain is None or after_domain is None:
            continue

        if before_domain.is_empty() and not after_domain.is_empty():
            raise AssertionError(f"Domain for variable {vid} expanded from empty")

        if not before_domain.is_empty() and not after_domain.is_empty():
            if (
                after_domain.min() < before_domain.min()
                or after_domain.max() > before_domain.max()
            ):
                raise AssertionError(f"Domain for variable {vid} expanded")


def create_constraint_stress_test(store, num_vars: int, num_constraints: int):
    """Create a stress test scenario with many variables and constraints.

    Args:
        store: Variable store
        num_vars: Number of variables to create
        num_constraints: Number of constraint relationships

    Returns:
        Tuple of (var_ids, constraint_pairs) for stress testing
    """
    var_ids, _ = create_test_variable_array(store, num_vars, "Stress")

    # Create constraint pairs for testing interactions
    constraint_pairs = []
    for i in range(num_constraints):
        v1 = var_ids[i % num_vars]
        v2 = var_ids[(i + 1) % num_vars]
        constraint_pairs.append((v1, v2))

    return var_ids, constraint_pairs


def validate_constraint_semantics(
    expected_solutions: List[List[int]], actual_domains: dict
):
    """Validate that domains are consistent with expected solution set.

    Args:
        expected_solutions: List of valid solution assignments
        actual_domains: Dictionary mapping variable IDs to domains

    Raises:
        AssertionError: If domains don't match expected solutions
    """
    if not expected_solutions:
        return

    var_ids = list(actual_domains.keys())
    var_ids.sort()  # Ensure consistent ordering

    for solution in expected_solutions:
        if len(solution) != len(var_ids):
            raise ValueError("Solution length doesn't match variable count")

        # Check if this solution is feasible given current domains
        feasible = True
        for i, value in enumerate(solution):
            vid = var_ids[i]
            domain = actual_domains[vid]
            if domain is None or not domain.contains(value):
                feasible = False
                break

        if not feasible:
            raise AssertionError(
                f"Expected solution {solution} is not feasible given current domains"
            )
