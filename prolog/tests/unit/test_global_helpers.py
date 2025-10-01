"""Tests for vector constraint helper utilities.

Tests for Phase 6.0: Foundations and helpers for vector constraints.
All helpers should be trail-safe and follow existing CLP(FD) patterns.
"""

import pytest
import time
from prolog.ast.terms import Atom, Int, Var, List
from prolog.unify.store import Store
from prolog.unify.trail import Trail
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import set_domain, get_domain
from prolog.clpfd.priority import Priority
from prolog.engine.builtins_clpfd import parse_var_list, validate_equal_length_vectors
from prolog.clpfd.helpers import (
    attach_watchers_to_array,
    attach_watchers_with_priorities,
    union_domains_across_variables,
    intersect_feasible_values,
    compute_total_cardinality,
    compute_min_cardinality,
    extract_global_bounds,
)
from prolog.tests.unit.constraint_helpers import (
    create_test_variable_array,
    setup_uniform_domains,
    setup_conflicting_domains,
    assert_all_different_property,
    measure_propagation_time,
)
from prolog.unify.unify_helpers import bind_root_to_term
from prolog.clpfd.api import iter_watchers
from prolog.unify.trail import undo_to


class TestVectorParsing:
    """Test vector/list parsing utilities for constraint arguments."""

    def test_parse_var_list_basic(self):
        """Test parsing a simple list of variables."""

        store = Store()
        x = Var(store.new_var("X"), "X")
        y = Var(store.new_var("Y"), "Y")
        z = Var(store.new_var("Z"), "Z")
        var_list = List((x, y, z))

        result = parse_var_list(var_list, store)
        assert len(result) == 3
        assert all(isinstance(vid, int) for vid in result)

    def test_parse_var_list_with_ints(self):
        """Test parsing mixed var/int list."""

        store = Store()
        x = Var(store.new_var("X"), "X")
        var_list = List((x, Int(5), Int(10)))

        result = parse_var_list(var_list, store)
        assert len(result) == 3
        assert isinstance(result[0], int)  # Variable ID
        assert result[1] == 5  # Integer value
        assert result[2] == 10  # Integer value

    def test_parse_var_list_bound_vars(self):
        """Test parsing list with bound variables."""

        store = Store()
        trail = Trail()
        x = Var(store.new_var("X"), "X")
        y = Var(store.new_var("Y"), "Y")

        # Bind Y to 42
        bind_root_to_term(y.id, Int(42), trail, store)

        var_list = List((x, y))
        result = parse_var_list(var_list, store)

        assert len(result) == 2
        assert isinstance(result[0], int)  # Unbound variable ID
        assert result[1] == 42  # Bound value

    def test_parse_var_list_invalid_terms(self):
        """Test parsing list with invalid terms fails gracefully."""

        store = Store()
        var_list = List((Atom("invalid"), Int(5)))

        with pytest.raises(ValueError, match="Invalid term in variable list"):
            parse_var_list(var_list, store)

    def test_parse_var_list_empty(self):
        """Test parsing empty list."""

        store = Store()
        var_list = List(())

        result = parse_var_list(var_list, store)
        assert result == []

    def test_validate_equal_length_vectors(self):
        """Test validation of equal-length constraint vectors."""

        store = Store()
        x1, x2, x3 = [Var(store.new_var(f"X{i}"), f"X{i}") for i in range(3)]
        y1, y2, y3 = [Var(store.new_var(f"Y{i}"), f"Y{i}") for i in range(3)]

        list1 = List((x1, x2, x3))
        list2 = List((y1, y2, y3))

        # Should succeed for equal length
        validate_equal_length_vectors([list1, list2])

        # Should fail for unequal length
        list3 = List((y1, y2))
        with pytest.raises(ValueError, match="Vectors must have equal length"):
            validate_equal_length_vectors([list1, list3])


class TestWatcherManagement:
    """Test watcher attachment/detachment utilities for variable arrays."""

    def test_attach_watchers_to_array(self):
        """Test batch watcher attachment for variable arrays."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(5)]
        pid = 123
        priority = Priority.HIGH

        attach_watchers_to_array(store, var_ids, pid, priority, trail)

        # Verify all variables have the watcher

        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            assert (pid, priority) in watchers

    def test_attach_watchers_with_priorities(self):
        """Test watcher attachment with different priorities."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]
        watcher_specs = [(100, Priority.HIGH), (200, Priority.MED), (300, Priority.LOW)]

        attach_watchers_with_priorities(store, var_ids, watcher_specs, trail)

        # Verify all watchers are attached to all variables

        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            for pid, priority in watcher_specs:
                assert (pid, priority) in watchers

    def test_detach_watchers_on_backtrack(self):
        """Test watcher cleanup works correctly with backtracking."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]
        pid = 456

        # Create checkpoint before attaching watchers
        checkpoint = len(trail.entries)

        # Attach watchers
        attach_watchers_to_array(store, var_ids, pid, Priority.MED, trail)

        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            assert (pid, Priority.MED) in watchers

        # Backtrack and verify watchers are gone

        undo_to(checkpoint, trail, store)
        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            assert (pid, Priority.MED) not in watchers

    def test_efficient_watcher_lookup(self):
        """Test that watcher iteration is efficient for large arrays."""

        store = Store()
        trail = Trail()
        # Large array of variables
        var_ids = [store.new_var(f"X{i}") for i in range(100)]
        pid = 789

        start_time = time.time()
        attach_watchers_to_array(store, var_ids, pid, Priority.HIGH, trail)

        # Verify lookup is fast (should be well under 100ms even for 100 vars)

        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            assert (pid, Priority.HIGH) in watchers

        elapsed = time.time() - start_time
        assert elapsed < 0.1  # Should be very fast


class TestDomainOperations:
    """Test domain operation utilities for variable sets."""

    def test_union_domains_across_variables(self):
        """Test computing union of domains across variable list."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]

        # Set different domains
        set_domain(store, var_ids[0], Domain(((1, 3),)), trail)
        set_domain(store, var_ids[1], Domain(((5, 7),)), trail)
        set_domain(store, var_ids[2], Domain(((2, 4), (8, 10))), trail)

        union_domain = union_domains_across_variables(store, var_ids)

        # Should contain union: 1..7, 8..10
        assert union_domain.contains(1)
        assert union_domain.contains(3)
        assert union_domain.contains(5)
        assert union_domain.contains(7)
        assert union_domain.contains(8)
        assert union_domain.contains(10)
        assert not union_domain.contains(11)

    def test_intersect_feasible_values(self):
        """Test intersection of feasible values across variables."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]

        # Set overlapping domains
        set_domain(store, var_ids[0], Domain(((1, 10),)), trail)
        set_domain(store, var_ids[1], Domain(((5, 15),)), trail)
        set_domain(store, var_ids[2], Domain(((3, 8),)), trail)

        intersection = intersect_feasible_values(store, var_ids)

        # Should contain intersection: 5..8
        assert intersection.min() == 5
        assert intersection.max() == 8
        assert intersection.contains(6)
        assert not intersection.contains(4)
        assert not intersection.contains(9)

    def test_cardinality_computations(self):
        """Test cardinality computations over variable sets."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(2)]

        # X0: {1,2,3}, X1: {4,5}
        set_domain(store, var_ids[0], Domain(((1, 3),)), trail)
        set_domain(store, var_ids[1], Domain(((4, 5),)), trail)

        total_card = compute_total_cardinality(store, var_ids)
        assert total_card == 5  # 3 + 2

        min_card = compute_min_cardinality(store, var_ids)
        assert min_card == 2  # min(3, 2)

    def test_min_max_extraction(self):
        """Test min/max value extraction from domain sets."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]

        set_domain(store, var_ids[0], Domain(((5, 10),)), trail)
        set_domain(store, var_ids[1], Domain(((1, 3), (8, 12))), trail)
        set_domain(store, var_ids[2], Domain(((7, 15),)), trail)

        min_val, max_val = extract_global_bounds(store, var_ids)
        assert min_val == 1  # Minimum across all domains
        assert max_val == 15  # Maximum across all domains

    def test_domain_operations_with_empty_domains(self):
        """Test domain operations handle empty domains gracefully."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(2)]

        # One variable has empty domain
        set_domain(store, var_ids[0], Domain(()), trail)  # Empty
        set_domain(store, var_ids[1], Domain(((1, 5),)), trail)

        union_domain = union_domains_across_variables(store, var_ids)
        assert union_domain.min() == 1
        assert union_domain.max() == 5

        min_val, max_val = extract_global_bounds(store, var_ids)
        assert min_val == 1  # Should ignore empty domain
        assert max_val == 5

    def test_domain_operations_no_domains(self):
        """Test domain operations when variables have no domains yet."""

        store = Store()
        var_ids = [store.new_var(f"X{i}") for i in range(2)]

        # Variables with no domains yet
        union_domain = union_domains_across_variables(store, var_ids)
        assert union_domain.is_empty()


class TestConstraintTestInfrastructure:
    """Test infrastructure and utilities for global constraint testing."""

    def test_create_test_variable_array(self):
        """Test helper for creating test variable arrays."""

        store = Store()
        var_ids, var_terms = create_test_variable_array(store, 5, "Test")

        assert len(var_ids) == 5
        assert len(var_terms) == 5
        assert all(isinstance(vid, int) for vid in var_ids)
        assert all(isinstance(term, Var) for term in var_terms)
        assert all(term.hint.startswith("Test") for term in var_terms)

    def test_setup_domain_for_testing(self):
        """Test domain setup utilities for testing."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]

        setup_uniform_domains(store, var_ids, 1, 10, trail)

        for vid in var_ids:
            domain = get_domain(store, vid)
            assert domain is not None
            assert domain.min() == 1
            assert domain.max() == 10

    def test_assert_all_different_property(self):
        """Test assertion helpers for global constraint properties."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]

        # Set up domains that enforce all_different
        set_domain(store, var_ids[0], Domain(((1, 1),)), trail)  # X0 = 1
        set_domain(store, var_ids[1], Domain(((2, 2),)), trail)  # X1 = 2
        set_domain(store, var_ids[2], Domain(((3, 3),)), trail)  # X2 = 3

        # Should pass the all_different check
        assert_all_different_property(store, var_ids)

        # Violate all_different constraint
        set_domain(store, var_ids[2], Domain(((1, 1),)), trail)  # X2 = 1 (same as X0)

        with pytest.raises(AssertionError, match="All-different property violated"):
            assert_all_different_property(store, var_ids)

    def test_performance_measurement_utilities(self):
        """Test performance measurement utilities."""

        def dummy_propagation():
            """Dummy propagation that takes some time."""
            time.sleep(0.001)  # 1ms
            return True

        elapsed = measure_propagation_time(dummy_propagation)
        assert elapsed >= 0.001  # Should be at least 1ms
        assert elapsed < 0.1  # But not too much more

    def test_constraint_helpers_large_arrays(self):
        """Test constraint helpers work with large variable arrays."""

        store = Store()
        trail = Trail()

        # Test with larger arrays (performance check)
        var_ids, var_terms = create_test_variable_array(store, 100, "Large")
        setup_uniform_domains(store, var_ids, 1, 200, trail)

        assert len(var_ids) == 100
        # Verify all domains are set correctly
        for vid in var_ids:
            domain = get_domain(store, vid)
            assert domain.min() == 1
            assert domain.max() == 200

    def test_error_condition_helpers(self):
        """Test helpers for setting up error conditions."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(2)]

        # Set up conflicting domains for testing constraint failure
        setup_conflicting_domains(store, var_ids, trail)

        # Should have empty domains or other conflict markers
        domains = [get_domain(store, vid) for vid in var_ids]
        # At least one domain should be empty or show conflict
        assert any(d is None or d.is_empty() for d in domains)


class TestMemoryEfficiency:
    """Test memory efficiency for large variable arrays."""

    def test_watcher_memory_efficiency(self):
        """Test watcher storage is memory efficient for large arrays."""

        store = Store()
        trail = Trail()
        # Test with many variables and many watchers
        var_ids = [store.new_var(f"X{i}") for i in range(50)]

        # Attach multiple watchers per variable
        for pid in range(10):
            attach_watchers_to_array(store, var_ids, pid, Priority.MED, trail)

        # Memory usage should be reasonable (this is more of a smoke test)
        # Verify all watchers are correctly stored

        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            assert len(watchers) == 10

    def test_domain_operation_scalability(self):
        """Test domain operations scale reasonably with problem size."""

        store = Store()
        trail = Trail()

        # Test with increasing array sizes
        for size in [10, 50, 100]:
            var_ids = [store.new_var(f"X{i}") for i in range(size)]

            # Set domains
            for i, vid in enumerate(var_ids):
                set_domain(store, vid, Domain(((i, i + 10),)), trail)

            start_time = time.time()
            union_domain = union_domains_across_variables(store, var_ids)
            elapsed = time.time() - start_time

            # Should scale reasonably (linear or better)
            assert elapsed < 0.1  # Should be fast even for 100 variables
            assert not union_domain.is_empty()


class TestTrailSafety:
    """Test that all helpers are trail-safe for backtracking."""

    def test_watcher_attachment_trail_safety(self):
        """Test watcher attachment/detachment is trail-safe."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(3)]

        initial_checkpoint = len(trail.entries)

        # Attach watchers in multiple steps with checkpoints
        attach_watchers_to_array(store, var_ids[:2], 100, Priority.HIGH, trail)
        checkpoint1 = len(trail.entries)

        attach_watchers_to_array(store, var_ids[2:], 200, Priority.MED, trail)

        # Verify state before backtrack

        watchers_0 = list(iter_watchers(store, var_ids[0]))
        watchers_2 = list(iter_watchers(store, var_ids[2]))
        assert (100, Priority.HIGH) in watchers_0
        assert (200, Priority.MED) in watchers_2

        # Backtrack to checkpoint1

        undo_to(checkpoint1, trail, store)

        # Should have only first set of watchers
        watchers_0 = list(iter_watchers(store, var_ids[0]))
        watchers_2 = list(iter_watchers(store, var_ids[2]))
        assert (100, Priority.HIGH) in watchers_0
        assert (200, Priority.MED) not in watchers_2

        # Backtrack to beginning
        undo_to(initial_checkpoint, trail, store)

        # Should have no watchers
        for vid in var_ids:
            watchers = list(iter_watchers(store, vid))
            assert len(watchers) == 0

    def test_domain_operations_trail_safety(self):
        """Test domain operations don't interfere with trail."""

        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(2)]

        initial_checkpoint = len(trail.entries)

        # Set domains
        set_domain(store, var_ids[0], Domain(((1, 5),)), trail)
        domain_checkpoint = len(trail.entries)

        # Domain operations should not add trail entries
        union_domains_across_variables(store, var_ids)
        assert len(trail.entries) == domain_checkpoint  # No new trail entries

        # Should still be able to backtrack domain changes

        undo_to(initial_checkpoint, trail, store)
        assert get_domain(store, var_ids[0]) is None
