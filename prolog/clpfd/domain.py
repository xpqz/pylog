"""CLP(FD) Domain representation.

Immutable finite domains as sorted disjoint intervals with revision tracking.
"""

from dataclasses import dataclass
from typing import Tuple, Optional, Iterator


def normalize_intervals(
    intervals: Tuple[Tuple[int, int], ...],
) -> Tuple[Tuple[int, int], ...]:
    """Normalize intervals to canonical form (sorted, merged where overlapping/adjacent)."""
    if not intervals:
        return ()

    # Sort by start position
    sorted_intervals = sorted(intervals, key=lambda x: x[0])

    # Merge overlapping and adjacent intervals
    result = []
    current_start, current_end = sorted_intervals[0]

    for start, end in sorted_intervals[1:]:
        if start <= current_end + 1:
            # Overlapping or adjacent - merge
            current_end = max(current_end, end)
        else:
            # Gap - save current and start new
            result.append((current_start, current_end))
            current_start, current_end = start, end

    # Don't forget the last interval
    result.append((current_start, current_end))

    return tuple(result)


def _middle_out_order(values: list[int]) -> list[int]:
    """Reorder values starting from middle and alternating outward.

    Args:
        values: List of values in ascending order

    Returns:
        List reordered starting from middle, alternating left/right
    """
    if not values:
        return []

    if len(values) == 1:
        return values

    middle_idx = len(values) // 2
    result = [values[middle_idx]]

    # Add values alternating from middle outward (lower first)
    for i in range(1, max(middle_idx + 1, len(values) - middle_idx)):
        if middle_idx - i >= 0:
            result.append(values[middle_idx - i])
        if middle_idx + i < len(values):
            result.append(values[middle_idx + i])

    return result


def _split_order(values: list[int]) -> list[int]:
    """Reorder values for indomain_split strategy.

    Args:
        values: List of values in ascending order

    Returns:
        List reordered starting from middle, alternating with upper values first
    """
    if not values:
        return []

    if len(values) == 1:
        return values

    middle_idx = len(values) // 2
    result = [values[middle_idx]]

    # Add values alternating from middle outward (upper first)
    for i in range(1, max(middle_idx + 1, len(values) - middle_idx)):
        if middle_idx + i < len(values):
            result.append(values[middle_idx + i])
        if middle_idx - i >= 0:
            result.append(values[middle_idx - i])

    return result


@dataclass(frozen=True)
class Domain:
    """Immutable finite domain as sorted disjoint intervals.

    Attributes:
        intervals: Sorted tuple of (min, max) inclusive ranges
        rev: Monotonic revision counter for change detection
    """

    intervals: Tuple[Tuple[int, int], ...]
    rev: int = 0

    def __post_init__(self):
        """Normalize intervals after dataclass initialization."""
        # Normalize intervals on creation
        normalized = normalize_intervals(self.intervals)
        # Use object.__setattr__ to bypass frozen restriction
        object.__setattr__(self, "intervals", normalized)

    def is_empty(self) -> bool:
        """Check if domain is empty."""
        return len(self.intervals) == 0

    def is_singleton(self) -> bool:
        """Check if domain contains exactly one value."""
        return len(self.intervals) == 1 and self.intervals[0][0] == self.intervals[0][1]

    def min(self) -> Optional[int]:
        """Get minimum value or None if empty."""
        return self.intervals[0][0] if self.intervals else None

    def max(self) -> Optional[int]:
        """Get maximum value or None if empty."""
        return self.intervals[-1][1] if self.intervals else None

    def size(self) -> int:
        """Count total values in domain."""
        return sum(high - low + 1 for low, high in self.intervals)

    def contains(self, value: int) -> bool:
        """Check if value is in domain using binary search."""
        left, right = 0, len(self.intervals) - 1
        while left <= right:
            mid = (left + right) // 2
            low, high = self.intervals[mid]
            if low <= value <= high:
                return True
            elif value < low:
                right = mid - 1
            else:
                left = mid + 1
        return False

    def intersect(self, other: "Domain") -> "Domain":
        """Return intersection (returns self if unchanged)."""
        # Two-pointer merge of sorted intervals
        result = []
        i, j = 0, 0
        while i < len(self.intervals) and j < len(other.intervals):
            a_low, a_high = self.intervals[i]
            b_low, b_high = other.intervals[j]

            # Find overlap
            low = max(a_low, b_low)
            high = min(a_high, b_high)

            if low <= high:
                result.append((low, high))

            # Advance pointer with smaller endpoint
            if a_high < b_high:
                i += 1
            else:
                j += 1

        new_intervals = tuple(result)

        # Return self ONLY if no change to self's intervals
        if new_intervals == self.intervals:
            return self

        # Changed - bump revision
        # The normalized intervals from Domain constructor might differ from result
        # So we need to create the Domain first and check
        new_domain = Domain(new_intervals, max(self.rev, other.rev) + 1)

        # If after normalization the intervals are same as self, return self
        if new_domain.intervals == self.intervals:
            return self

        return new_domain

    def remove_value(self, value: int) -> "Domain":
        """Remove single value, splitting intervals if needed."""
        if not self.contains(value):
            return self  # No change

        result = []
        for low, high in self.intervals:
            if value < low or value > high:
                # Value not in this interval
                result.append((low, high))
            elif low == high == value:
                # Remove singleton interval
                pass
            elif value == low:
                # Remove from start
                result.append((low + 1, high))
            elif value == high:
                # Remove from end
                result.append((low, high - 1))
            else:
                # Split interval
                result.append((low, value - 1))
                result.append((value + 1, high))

        return Domain(tuple(result), self.rev + 1)

    def remove_lt(self, bound: int) -> "Domain":
        """Remove all values < bound."""
        result = []
        for low, high in self.intervals:
            if high < bound:
                # Entire interval removed
                continue
            elif low < bound:
                # Trim interval start
                result.append((bound, high))
            else:
                # Keep entire interval
                result.append((low, high))

        new_intervals = tuple(result)
        if new_intervals == self.intervals:
            return self
        return Domain(new_intervals, self.rev + 1)

    def remove_le(self, bound: int) -> "Domain":
        """Remove all values <= bound."""
        return self.remove_lt(bound + 1)

    def remove_gt(self, bound: int) -> "Domain":
        """Remove all values > bound."""
        result = []
        for low, high in self.intervals:
            if low > bound:
                # Entire interval removed
                continue
            elif high > bound:
                # Trim interval end
                result.append((low, bound))
            else:
                # Keep entire interval
                result.append((low, high))

        new_intervals = tuple(result)
        if new_intervals == self.intervals:
            return self
        return Domain(new_intervals, self.rev + 1)

    def remove_ge(self, bound: int) -> "Domain":
        """Remove all values >= bound."""
        return self.remove_gt(bound - 1)

    def remove_interval(self, low: int, high: int) -> "Domain":
        """Remove closed interval [low, high] from domain.

        Args:
            low: Lower bound of interval to remove (inclusive)
            high: Upper bound of interval to remove (inclusive)

        Returns:
            Domain with interval removed (returns self if no change)
        """
        # Handle empty domain first
        if not self.intervals:
            return self  # Empty domain

        # Handle invalid interval or no overlap cases
        if low > high or high < self.min() or low > self.max():
            return self  # No overlap possible

        result = []
        for start, end in self.intervals:
            if end < low or start > high:
                # No overlap with removal interval
                result.append((start, end))
            elif start < low and end > high:
                # Removal splits this interval in two
                result.append((start, low - 1))
                result.append((high + 1, end))
            elif start < low <= end:
                # Removal clips right side of this interval
                result.append((start, low - 1))
            elif start <= high < end:
                # Removal clips left side of this interval
                result.append((high + 1, end))
            # else: interval completely removed (start >= low and end <= high)

        new_intervals = tuple(result)
        if new_intervals == self.intervals:
            return self  # No change

        return Domain(new_intervals, self.rev + 1)

    def iter_values(self) -> Iterator[int]:
        """Iterate over domain values lazily without materialization.

        Yields values in ascending order across all intervals.

        Returns:
            Iterator yielding domain values one at a time
        """
        for low, high in self.intervals:
            yield from range(low, high + 1)

    def iter_values_ordered(self, strategy: str) -> Iterator[int]:
        """Iterate values in specified order without full materialization.

        Args:
            strategy: Ordering strategy - "min_first", "max_first", "middle_out", or "split"

        Returns:
            Iterator yielding values in the specified order
        """
        if strategy == "min_first":
            yield from self.iter_values()
        elif strategy == "max_first":
            # Iterate intervals in reverse, values in reverse within each
            for low, high in reversed(self.intervals):
                yield from range(high, low - 1, -1)
        elif strategy == "middle_out":
            # For middle_out, we need to materialize (only this strategy)
            # This is acceptable since middle_out is rarely used for huge domains
            values = list(self.iter_values())
            yield from _middle_out_order(values)
        elif strategy == "split":
            # For split ordering, we need to materialize
            values = list(self.iter_values())
            yield from _split_order(values)
        else:
            # Unknown strategy defaults to min_first
            yield from self.iter_values()
