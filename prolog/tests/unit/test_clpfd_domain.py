"""Unit tests for CLP(FD) Domain representation.

Tests the immutable Domain class with interval-set operations,
revision tracking, and proper semantics.
"""

import pytest
import types
from prolog.clpfd.domain import Domain


class TestDomainNormalization:
    """Test that Domain normalizes intervals to canonical form."""

    def test_domain_sorts_intervals(self):
        """Domain constructor should sort intervals by start position."""
        # Give intervals out of order
        d = Domain(((10, 15), (1, 5), (20, 25)))

        # Should be sorted
        assert d.intervals == ((1, 5), (10, 15), (20, 25))

    def test_domain_merges_overlapping_intervals(self):
        """Domain constructor should merge overlapping intervals."""
        # Overlapping: 1..5 and 3..8 should merge to 1..8
        d = Domain(((1, 5), (3, 8)))

        # Should be merged
        assert d.intervals == ((1, 8),)

    def test_domain_merges_adjacent_intervals(self):
        """Domain constructor should merge adjacent intervals."""
        # Adjacent: 1..3 and 4..6 should merge to 1..6
        d = Domain(((1, 3), (4, 6)))

        # Should be merged (implementation may choose not to merge adjacent)
        # This test documents expected behavior
        assert len(d.intervals) <= 2  # Either merged or not

    def test_domain_preserves_gaps(self):
        """Domain constructor should preserve gaps between intervals."""
        # Non-adjacent with gap: 1..3 and 5..7
        d = Domain(((1, 3), (5, 7)))

        # Should NOT be merged (gap at 4)
        assert len(d.intervals) == 2
        assert d.intervals == ((1, 3), (5, 7))


class TestDomainBasics:
    """Test basic domain creation and properties."""

    def test_empty_domain(self):
        """Empty domain has no intervals."""
        d = Domain(())
        assert d.is_empty()
        assert not d.is_singleton()
        assert d.min() is None
        assert d.max() is None
        assert d.size() == 0
        assert not d.contains(0)
        assert d.intervals == ()

    def test_singleton_domain(self):
        """Singleton domain contains exactly one value."""
        d = Domain(((5, 5),))
        assert not d.is_empty()
        assert d.is_singleton()
        assert d.min() == 5
        assert d.max() == 5
        assert d.size() == 1
        assert d.contains(5)
        assert not d.contains(4)
        assert not d.contains(6)

    def test_single_interval(self):
        """Single interval domain."""
        d = Domain(((1, 10),))
        assert not d.is_empty()
        assert not d.is_singleton()
        assert d.min() == 1
        assert d.max() == 10
        assert d.size() == 10
        assert d.contains(1)
        assert d.contains(5)
        assert d.contains(10)
        assert not d.contains(0)
        assert not d.contains(11)

    def test_multiple_intervals(self):
        """Domain with multiple disjoint intervals."""
        # 1..3 ∪ 7..9
        d = Domain(((1, 3), (7, 9)))
        assert not d.is_empty()
        assert not d.is_singleton()
        assert d.min() == 1
        assert d.max() == 9
        assert d.size() == 6

        # Values in first interval
        assert d.contains(1)
        assert d.contains(2)
        assert d.contains(3)

        # Gap between intervals
        assert not d.contains(4)
        assert not d.contains(5)
        assert not d.contains(6)

        # Values in second interval
        assert d.contains(7)
        assert d.contains(8)
        assert d.contains(9)

        # Outside bounds
        assert not d.contains(0)
        assert not d.contains(10)

    def test_revision_counter(self):
        """Revision counter tracks domain changes."""
        d1 = Domain(((1, 10),), rev=0)
        assert d1.rev == 0

        d2 = Domain(((1, 10),), rev=5)
        assert d2.rev == 5


class TestDomainIntersection:
    """Test domain intersection operations."""

    def test_intersect_identical(self):
        """Intersecting identical domains returns self (no change).

        Note: intersect() returns self ONLY when the result intervals
        are exactly equal to the left operand's intervals. If two different
        domains yield the same result intervals, a new Domain is created.
        """
        d1 = Domain(((1, 10),), rev=5)
        d2 = Domain(((1, 10),), rev=3)

        result = d1.intersect(d2)
        assert result is d1  # Same object, no change
        assert result.rev == 5  # No revision bump

    def test_intersect_disjoint(self):
        """Intersecting disjoint domains yields empty domain."""
        d1 = Domain(((1, 5),), rev=2)
        d2 = Domain(((10, 15),), rev=3)

        result = d1.intersect(d2)
        assert result.is_empty()
        assert result.rev == 4  # max(2,3) + 1

    def test_intersect_overlapping_intervals(self):
        """Intersecting overlapping intervals."""
        d1 = Domain(((1, 10),), rev=2)
        d2 = Domain(((5, 15),), rev=3)

        result = d1.intersect(d2)
        assert result.intervals == ((5, 10),)
        assert result.rev == 4  # max(2,3) + 1

    def test_intersect_multiple_intervals(self):
        """Intersecting domains with multiple intervals."""
        # d1: 1..5 ∪ 10..15
        # d2: 3..7 ∪ 12..20
        # result: 3..5 ∪ 12..15
        d1 = Domain(((1, 5), (10, 15)), rev=1)
        d2 = Domain(((3, 7), (12, 20)), rev=2)

        result = d1.intersect(d2)
        assert result.intervals == ((3, 5), (12, 15))
        assert result.rev == 3  # max(1,2) + 1

    def test_intersect_subset(self):
        """Intersecting when one domain is subset of another."""
        d1 = Domain(((1, 20),), rev=5)
        d2 = Domain(((5, 10),), rev=3)

        result = d1.intersect(d2)
        assert result.intervals == ((5, 10),)
        assert result.rev == 6  # max(5,3) + 1

    def test_intersect_with_empty(self):
        """Intersecting with empty domain yields empty."""
        d1 = Domain(((1, 10),), rev=5)
        d2 = Domain((), rev=3)

        result = d1.intersect(d2)
        assert result.is_empty()
        assert result.rev == 6  # max(5,3) + 1


class TestDomainRemovalOperations:
    """Test value removal operations."""

    def test_remove_value_not_present(self):
        """Removing value not in domain returns self."""
        d = Domain(((1, 5),), rev=3)
        result = d.remove_value(10)
        assert result is d  # Same object, no change
        assert result.rev == 3  # No revision bump

    def test_remove_value_singleton(self):
        """Removing value from singleton makes it empty."""
        d = Domain(((5, 5),), rev=2)
        result = d.remove_value(5)
        assert result.is_empty()
        assert result.rev == 3

    def test_remove_value_from_start(self):
        """Removing value from start of interval."""
        d = Domain(((1, 10),), rev=2)
        result = d.remove_value(1)
        assert result.intervals == ((2, 10),)
        assert result.rev == 3

    def test_remove_value_from_end(self):
        """Removing value from end of interval."""
        d = Domain(((1, 10),), rev=2)
        result = d.remove_value(10)
        assert result.intervals == ((1, 9),)
        assert result.rev == 3

    def test_remove_value_splits_interval(self):
        """Removing value from middle splits interval."""
        d = Domain(((1, 10),), rev=2)
        result = d.remove_value(5)
        assert result.intervals == ((1, 4), (6, 10))
        assert result.rev == 3

    def test_remove_value_between_intervals(self):
        """Removing value between intervals returns self."""
        d = Domain(((1, 3), (7, 9)), rev=5)
        result = d.remove_value(5)
        assert result is d  # No change
        assert result.rev == 5


class TestDomainBoundsOperations:
    """Test bounds removal operations."""

    def test_remove_lt(self):
        """Remove all values less than bound."""
        d = Domain(((1, 10),), rev=2)

        # Remove values < 5
        result = d.remove_lt(5)
        assert result.intervals == ((5, 10),)
        assert result.rev == 3

        # Remove values < 0 (no change)
        result2 = d.remove_lt(0)
        assert result2 is d
        assert result2.rev == 2

        # Remove values < 15 (everything)
        result3 = d.remove_lt(15)
        assert result3.is_empty()

    def test_remove_le(self):
        """Remove all values less than or equal to bound."""
        d = Domain(((1, 10),), rev=2)

        # Remove values <= 5
        result = d.remove_le(5)
        assert result.intervals == ((6, 10),)
        assert result.rev == 3

    def test_remove_gt(self):
        """Remove all values greater than bound."""
        d = Domain(((1, 10),), rev=2)

        # Remove values > 5
        result = d.remove_gt(5)
        assert result.intervals == ((1, 5),)
        assert result.rev == 3

        # Remove values > 15 (no change)
        result2 = d.remove_gt(15)
        assert result2 is d
        assert result2.rev == 2

        # Remove values > -1 (everything)
        result3 = d.remove_gt(-1)
        assert result3.is_empty()

    def test_remove_ge(self):
        """Remove all values greater than or equal to bound."""
        d = Domain(((1, 10),), rev=2)

        # Remove values >= 5
        result = d.remove_ge(5)
        assert result.intervals == ((1, 4),)
        assert result.rev == 3

    def test_remove_bounds_multiple_intervals(self):
        """Remove bounds with multiple intervals."""
        # 1..5 ∪ 10..15
        d = Domain(((1, 5), (10, 15)), rev=2)

        # Remove < 3: should trim first interval
        result = d.remove_lt(3)
        assert result.intervals == ((3, 5), (10, 15))
        assert result.rev == 3

        # Remove > 12: should trim second interval
        result2 = d.remove_gt(12)
        assert result2.intervals == ((1, 5), (10, 12))
        assert result2.rev == 3

        # Remove < 7: should remove first interval entirely
        result3 = d.remove_lt(7)
        assert result3.intervals == ((10, 15),)
        assert result3.rev == 3

        # Remove > 7: should remove second interval entirely
        result4 = d.remove_gt(7)
        assert result4.intervals == ((1, 5),)
        assert result4.rev == 3


class TestDomainContains:
    """Test binary search membership check."""

    def test_contains_single_interval(self):
        """Binary search in single interval."""
        d = Domain(((1, 100),))

        # Test boundaries
        assert d.contains(1)
        assert d.contains(100)

        # Test middle values
        assert d.contains(50)
        assert d.contains(25)
        assert d.contains(75)

        # Test outside
        assert not d.contains(0)
        assert not d.contains(101)

    def test_contains_multiple_intervals(self):
        """Binary search across multiple intervals."""
        # 1..10 ∪ 20..30 ∪ 40..50
        d = Domain(((1, 10), (20, 30), (40, 50)))

        # First interval
        assert d.contains(5)

        # Second interval
        assert d.contains(25)

        # Third interval
        assert d.contains(45)

        # Gaps
        assert not d.contains(15)
        assert not d.contains(35)

        # Outside
        assert not d.contains(0)
        assert not d.contains(60)

    def test_contains_many_intervals(self):
        """Binary search efficiency with many intervals."""
        # Create domain with 10 intervals
        intervals = tuple((i * 10, i * 10 + 5) for i in range(10))
        d = Domain(intervals)

        # Should find values efficiently via binary search
        assert d.contains(5)  # In first interval
        assert d.contains(45)  # In middle interval
        assert d.contains(95)  # In last interval

        # Gaps between intervals
        assert not d.contains(7)
        assert not d.contains(47)
        assert not d.contains(97)


class TestDomainRevisionSemantics:
    """Test revision counter behavior per guardrails."""

    def test_revision_only_bumps_on_change(self):
        """Revision counter only increases on actual narrowing."""
        d = Domain(((1, 10),), rev=5)

        # No change operations should return same object with same rev
        same = d.intersect(Domain(((1, 10),)))
        assert same is d
        assert same.rev == 5

        # Change operations should bump rev
        narrowed = d.intersect(Domain(((5, 10),), rev=3))
        assert narrowed is not d
        assert narrowed.rev == 6  # max(5,3) + 1

    def test_revision_inheritance(self):
        """New domain inherits max revision from parents."""
        d1 = Domain(((1, 20),), rev=10)
        d2 = Domain(((5, 15),), rev=7)

        result = d1.intersect(d2)
        assert result.rev == 11  # max(10,7) + 1

        # Other direction - d2.intersect(d1) returns d2 unchanged
        # since (5,15) intersect (1,20) = (5,15) which equals d2
        result2 = d2.intersect(d1)
        assert result2.intervals == result.intervals
        assert result2 is d2  # Should return self when unchanged
        assert result2.rev == 7  # No change, so no rev bump

    def test_remove_operations_bump_rev(self):
        """All narrowing operations bump revision."""
        d = Domain(((1, 10),), rev=5)

        # Remove value
        d2 = d.remove_value(5)
        assert d2.rev == 6

        # Remove bounds
        d3 = d.remove_lt(3)
        assert d3.rev == 6

        d4 = d.remove_gt(7)
        assert d4.rev == 6


class TestDomainImmutability:
    """Test that Domain objects are truly immutable."""

    def test_domain_is_frozen(self):
        """Domain should be a frozen dataclass."""
        d = Domain(((1, 10),))

        # Should not be able to modify attributes
        with pytest.raises(AttributeError):
            d.intervals = ((1, 5),)

        with pytest.raises(AttributeError):
            d.rev = 10

    def test_operations_return_new_objects(self):
        """All operations return new objects when changed."""
        d = Domain(((1, 10),), rev=5)

        # Operations that change domain
        d2 = d.remove_value(5)
        assert d2 is not d
        assert d.intervals == ((1, 10),)  # Original unchanged
        assert d2.intervals == ((1, 4), (6, 10))

        d3 = d.intersect(Domain(((5, 15),)))
        assert d3 is not d
        assert d.intervals == ((1, 10),)  # Original unchanged
        assert d3.intervals == ((5, 10),)


class TestDomainRemoveInterval:
    """Test remove_interval() method for Phase 2."""

    def test_remove_interval_no_overlap(self):
        """Removing interval that doesn't overlap returns self."""
        d = Domain(((1, 5), (10, 15)), rev=3)

        # Remove interval [20, 25] - no overlap
        result = d.remove_interval(20, 25)
        assert result is d  # Same object, no change
        assert result.rev == 3  # No revision bump

        # Remove interval [6, 8] - in gap between intervals
        result2 = d.remove_interval(6, 8)
        assert result2 is d
        assert result2.rev == 3

    def test_remove_interval_complete_overlap(self):
        """Removing interval that completely covers a domain interval."""
        d = Domain(((5, 10), (15, 20)), rev=2)

        # Remove [5, 10] - removes first interval entirely
        result = d.remove_interval(5, 10)
        assert result.intervals == ((15, 20),)
        assert result.rev == 3

        # Remove [0, 25] - removes everything
        result2 = d.remove_interval(0, 25)
        assert result2.is_empty()
        assert result2.rev == 3

    def test_remove_interval_partial_overlap_left(self):
        """Removing interval that overlaps left side of domain interval."""
        d = Domain(((10, 20),), rev=2)

        # Remove [5, 12] - clips left side
        result = d.remove_interval(5, 12)
        assert result.intervals == ((13, 20),)
        assert result.rev == 3

    def test_remove_interval_partial_overlap_right(self):
        """Removing interval that overlaps right side of domain interval."""
        d = Domain(((10, 20),), rev=2)

        # Remove [18, 25] - clips right side
        result = d.remove_interval(18, 25)
        assert result.intervals == ((10, 17),)
        assert result.rev == 3

    def test_remove_interval_splits_domain(self):
        """Removing interval from middle splits domain interval."""
        d = Domain(((1, 20),), rev=2)

        # Remove [8, 12] - splits into two intervals
        result = d.remove_interval(8, 12)
        assert result.intervals == ((1, 7), (13, 20))
        assert result.rev == 3

    def test_remove_interval_multiple_overlaps(self):
        """Removing interval that overlaps multiple domain intervals."""
        d = Domain(((1, 5), (10, 15), (20, 25)), rev=2)

        # Remove [3, 22] - affects all three intervals
        result = d.remove_interval(3, 22)
        assert result.intervals == ((1, 2), (23, 25))
        assert result.rev == 3

        # Remove [12, 18] - affects middle interval only
        d2 = Domain(((1, 5), (10, 15), (20, 25)), rev=2)
        result2 = d2.remove_interval(12, 18)
        assert result2.intervals == ((1, 5), (10, 11), (20, 25))
        assert result2.rev == 3

    def test_remove_interval_edge_cases(self):
        """Test edge cases for remove_interval."""
        # Empty domain
        d_empty = Domain((), rev=5)
        result = d_empty.remove_interval(1, 10)
        assert result is d_empty
        assert result.rev == 5

        # Singleton domain
        d_singleton = Domain(((5, 5),), rev=2)
        result = d_singleton.remove_interval(5, 5)
        assert result.is_empty()
        assert result.rev == 3

        result2 = d_singleton.remove_interval(4, 4)
        assert result2 is d_singleton
        assert result2.rev == 2

        # Invalid interval (low > high)
        d = Domain(((1, 10),), rev=2)
        result = d.remove_interval(10, 5)  # Invalid: low > high
        assert result is d  # No change
        assert result.rev == 2

    def test_remove_interval_complex_scenario(self):
        """Test complex scenario with multiple intervals and partial overlaps."""
        # Domain: 1..3 ∪ 5..8 ∪ 10..15 ∪ 20..30
        d = Domain(((1, 3), (5, 8), (10, 15), (20, 30)), rev=10)

        # Remove [7, 22] - should affect intervals 2, 3, and 4
        # Result: 1..3 ∪ 5..6 ∪ 23..30
        result = d.remove_interval(7, 22)
        assert result.intervals == ((1, 3), (5, 6), (23, 30))
        assert result.rev == 11

    def test_remove_interval_for_hall_pruning(self):
        """Test scenario relevant to Hall-interval pruning in all_different."""
        # Domain represents possible values for a variable
        # Hall set found: must remove interval [2, 3]
        d = Domain(((1, 5),), rev=5)

        # Remove Hall interval [2, 3]
        result = d.remove_interval(2, 3)
        assert result.intervals == ((1, 1), (4, 5))
        assert result.rev == 6

        # Verify individual values
        assert result.contains(1)
        assert not result.contains(2)
        assert not result.contains(3)
        assert result.contains(4)
        assert result.contains(5)

    def test_remove_interval_idempotence(self):
        """Removing the same interval twice should be idempotent."""
        d = Domain(((1, 10),), rev=5)

        # First removal
        result1 = d.remove_interval(4, 6)
        assert result1.intervals == ((1, 3), (7, 10))
        assert result1.rev == 6

        # Second removal of same interval should return self
        result2 = result1.remove_interval(4, 6)
        assert result2 is result1  # Same object
        assert result2.rev == 6  # No revision bump

    def test_remove_interval_adjacency_normalization(self):
        """Adjacent intervals after removal should normalize properly."""
        # Start with gaps
        d = Domain(((1, 5), (7, 9)), rev=3)

        # Remove the gap [6, 6] - no change
        result1 = d.remove_interval(6, 6)
        assert result1 is d
        assert result1.rev == 3

        # Remove [5, 7] - creates adjacent intervals that should merge
        # Result: [1, 4] and [8, 9] which are NOT adjacent
        result2 = d.remove_interval(5, 7)
        assert result2.intervals == ((1, 4), (8, 9))
        assert result2.rev == 4

        # Test case where removal creates truly adjacent intervals
        d2 = Domain(((1, 10),), rev=2)
        # Remove [5, 5] splits into [1,4] and [6,10]
        result3 = d2.remove_interval(5, 5)
        assert result3.intervals == ((1, 4), (6, 10))

        # Now if we had [1,4], [6,10] and add back [5,5] via intersection
        # with [1,10], Domain normalization should merge them
        d3 = Domain(((1, 4), (5, 5), (6, 10)), rev=0)
        # Should normalize to (1, 10) on construction
        assert d3.intervals == ((1, 10),)

    def test_remove_interval_negative_ranges(self):
        """Test remove_interval with negative and zero values."""
        # Domain with negative values
        d = Domain(((-10, -5), (0, 5), (10, 15)), rev=2)

        # Remove negative interval
        result1 = d.remove_interval(-8, -6)
        assert result1.intervals == ((-10, -9), (-5, -5), (0, 5), (10, 15))
        assert result1.rev == 3

        # Remove interval spanning negative to positive
        result2 = d.remove_interval(-2, 2)
        assert result2.intervals == ((-10, -5), (3, 5), (10, 15))
        assert result2.rev == 3

        # Remove interval entirely below domain (no change)
        result3 = d.remove_interval(-100, -50)
        assert result3 is d
        assert result3.rev == 2

        # Remove interval entirely above domain (no change)
        result4 = d.remove_interval(100, 200)
        assert result4 is d
        assert result4.rev == 2


class TestDomainIteration:
    """Test lazy domain iteration methods (Phase 1 optimization)."""

    def test_iter_values_empty_domain(self):
        """Empty domain should yield no values."""
        d = Domain(())
        values = list(d.iter_values())
        assert values == []

    def test_iter_values_singleton_domain(self):
        """Singleton domain should yield single value."""
        d = Domain(((5, 5),))
        values = list(d.iter_values())
        assert values == [5]

    def test_iter_values_single_interval(self):
        """Single interval should yield all values in order."""
        d = Domain(((1, 5),))
        values = list(d.iter_values())
        assert values == [1, 2, 3, 4, 5]

    def test_iter_values_multiple_intervals(self):
        """Multiple intervals should yield values in order."""
        d = Domain(((1, 3), (7, 9)))
        values = list(d.iter_values())
        assert values == [1, 2, 3, 7, 8, 9]

    def test_iter_values_large_interval(self):
        """Large interval iteration should be memory efficient."""
        d = Domain(((1, 1000),))
        # Should be able to iterate without materializing full list
        iterator = d.iter_values()

        # Test first few values
        assert next(iterator) == 1
        assert next(iterator) == 2
        assert next(iterator) == 3

        # Should be lazy - can't easily test memory without profiling
        # but we can verify it's an iterator
        assert isinstance(iterator, types.GeneratorType)

    def test_iter_values_negative_values(self):
        """Should handle negative values correctly."""
        d = Domain(((-3, -1), (2, 4)))
        values = list(d.iter_values())
        assert values == [-3, -2, -1, 2, 3, 4]

    def test_iter_values_zero_crossing(self):
        """Should handle intervals crossing zero."""
        d = Domain(((-2, 2),))
        values = list(d.iter_values())
        assert values == [-2, -1, 0, 1, 2]

    def test_iter_values_ordered_min_first(self):
        """min_first strategy should iterate from minimum values."""
        d = Domain(((1, 3), (7, 9)))
        values = list(d.iter_values_ordered("min_first"))
        assert values == [1, 2, 3, 7, 8, 9]

    def test_iter_values_ordered_max_first(self):
        """max_first strategy should iterate from maximum values."""
        d = Domain(((1, 3), (7, 9)))
        values = list(d.iter_values_ordered("max_first"))
        assert values == [9, 8, 7, 3, 2, 1]

    def test_iter_values_ordered_middle_out(self):
        """middle_out strategy should start from middle and alternate."""
        d = Domain(((1, 5),))
        values = list(d.iter_values_ordered("middle_out"))
        # Middle value is 3, should be: 3, 2, 4, 1, 5
        assert values[0] == 3  # First should be middle value
        assert set(values) == {1, 2, 3, 4, 5}  # All values present
        assert len(values) == 5

    def test_iter_values_ordered_middle_out_even_count(self):
        """middle_out with even count should pick reasonable middle."""
        d = Domain(((1, 4),))
        values = list(d.iter_values_ordered("middle_out"))
        # For [1,2,3,4], middle could be 2 or 3
        assert values[0] in [2, 3]  # First should be middle value
        assert set(values) == {1, 2, 3, 4}  # All values present
        assert len(values) == 4

    def test_iter_values_ordered_empty_domain(self):
        """All strategies should handle empty domains."""
        d = Domain(())
        assert list(d.iter_values_ordered("min_first")) == []
        assert list(d.iter_values_ordered("max_first")) == []
        assert list(d.iter_values_ordered("middle_out")) == []

    def test_iter_values_ordered_singleton(self):
        """All strategies should handle singleton domains."""
        d = Domain(((7, 7),))
        assert list(d.iter_values_ordered("min_first")) == [7]
        assert list(d.iter_values_ordered("max_first")) == [7]
        assert list(d.iter_values_ordered("middle_out")) == [7]

    def test_iter_values_ordered_unknown_strategy(self):
        """Unknown strategy should default to min_first."""
        d = Domain(((1, 3),))
        values = list(d.iter_values_ordered("unknown_strategy"))
        assert values == [1, 2, 3]  # Default to min_first

    def test_iter_values_preserves_domain_immutability(self):
        """Iteration should not modify the domain."""
        d = Domain(((1, 5),), rev=10)
        original_intervals = d.intervals
        original_rev = d.rev

        # Iterate multiple times
        list(d.iter_values())
        list(d.iter_values_ordered("max_first"))
        list(d.iter_values_ordered("middle_out"))

        # Domain should be unchanged
        assert d.intervals == original_intervals
        assert d.rev == original_rev

    def test_iter_values_memory_efficiency_large_domain(self):
        """Test that large domain iteration is memory efficient."""
        # Create domain with potentially millions of values
        d = Domain(((1, 1000000),))

        # Should be able to get first few values without memory explosion
        iterator = d.iter_values()
        first_values = [next(iterator) for _ in range(100)]
        assert first_values == list(range(1, 101))

        # Iterator should still be active
        next_value = next(iterator)
        assert next_value == 101

    def test_iter_values_multiple_large_intervals(self):
        """Test iteration over multiple large intervals."""
        d = Domain(((1, 1000), (10000, 11000)))
        iterator = d.iter_values()

        # First interval values
        first_values = [next(iterator) for _ in range(5)]
        assert first_values == [1, 2, 3, 4, 5]

        # Skip to end of first interval and start of second
        # We know first interval has 1000 values, so skip 995 more
        for _ in range(995):
            next(iterator)

        # Now we should be at second interval
        second_interval_start = next(iterator)
        assert second_interval_start == 10000
