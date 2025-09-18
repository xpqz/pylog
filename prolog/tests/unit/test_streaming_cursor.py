"""
Unit tests for StreamingClauseCursor.

Tests the streaming cursor implementation that lazily yields clauses
from an iterator while maintaining backtracking support.
"""

import pytest
from typing import Iterator, List

from prolog.engine.cursors import StreamingClauseCursor


class TestStreamingClauseCursor:
    """Test StreamingClauseCursor behavior."""

    def test_has_more_on_empty_iterator(self):
        """Test has_more returns False for empty iterator."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=0,
            it=iter([])
        )
        assert cursor.has_more() is False

    def test_has_more_before_consumption(self):
        """Test has_more returns True when items available."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter([0, 1, 2])
        )
        assert cursor.has_more() is True

    def test_peek_does_not_consume(self):
        """Test peek returns next item without consuming."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter([10, 20, 30])
        )

        # Peek multiple times - should always return same value
        assert cursor.peek() == 10
        assert cursor.peek() == 10
        assert cursor.has_more() is True

        # Take consumes the peeked item
        assert cursor.take() == 10
        assert cursor.peek() == 20

    def test_take_consumes_items_in_order(self):
        """Test take consumes items maintaining order."""
        items = [100, 200, 300, 400]
        cursor = StreamingClauseCursor(
            functor="pred",
            arity=2,
            it=iter(items)
        )

        result = []
        while cursor.has_more():
            result.append(cursor.take())

        assert result == items
        assert cursor.has_more() is False

    def test_take_on_empty_raises(self):
        """Test take on exhausted cursor raises appropriate error."""
        cursor = StreamingClauseCursor(
            functor="empty",
            arity=0,
            it=iter([])
        )

        with pytest.raises(StopIteration):
            cursor.take()

    def test_clone_at_start(self):
        """Test cloning cursor at start position."""
        items = [1, 2, 3, 4, 5]
        cursor1 = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter(items)
        )

        # Clone before any consumption
        cursor2 = cursor1.clone()

        # Both cursors should yield same sequence
        assert cursor1.take() == 1
        assert cursor2.take() == 1

        assert cursor1.take() == 2
        assert cursor2.take() == 2

    def test_clone_after_partial_consumption(self):
        """Test cloning cursor after consuming some items."""
        cursor1 = StreamingClauseCursor(
            functor="test",
            arity=2,
            it=iter([10, 20, 30, 40])
        )

        # Consume first two items
        assert cursor1.take() == 10
        assert cursor1.take() == 20

        # Clone at this position
        cursor2 = cursor1.clone()

        # cursor2 should resume from where cursor1 is
        assert cursor2.take() == 30
        assert cursor1.take() == 30

        assert cursor2.take() == 40
        assert cursor1.take() == 40

        assert cursor1.has_more() is False
        assert cursor2.has_more() is False

    def test_clone_preserves_consumed_history(self):
        """Test clone maintains consumed items for backtracking."""
        cursor1 = StreamingClauseCursor(
            functor="parent",
            arity=2,
            it=iter([0, 1, 2, 3, 4])
        )

        # Consume some items
        cursor1.take()  # 0
        cursor1.take()  # 1

        # Clone should have access to consumed history
        cursor2 = cursor1.clone()

        # Both should have same consumed history
        assert cursor2._consumed == cursor1._consumed
        assert cursor2._consumed == [0, 1]

    def test_functor_and_arity_attributes(self):
        """Test functor and arity are accessible."""
        cursor = StreamingClauseCursor(
            functor="append",
            arity=3,
            it=iter([])
        )

        assert cursor.functor == "append"
        assert cursor.arity == 3

    def test_interleaved_peek_and_take(self):
        """Test interleaving peek and take operations."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter(range(5))
        )

        assert cursor.peek() == 0
        assert cursor.take() == 0

        assert cursor.peek() == 1
        assert cursor.peek() == 1  # Peek twice
        assert cursor.take() == 1

        assert cursor.has_more() is True
        assert cursor.take() == 2

        assert cursor.peek() == 3
        assert cursor.take() == 3

        assert cursor.take() == 4
        assert cursor.has_more() is False

    def test_large_iterator_streaming(self):
        """Test streaming behavior with large iterator."""
        # Create large iterator that shouldn't be materialized
        def large_generator():
            for i in range(10000):
                yield i

        cursor = StreamingClauseCursor(
            functor="data",
            arity=2,
            it=large_generator()
        )

        # Should be able to consume just first few without materializing all
        assert cursor.take() == 0
        assert cursor.take() == 1
        assert cursor.take() == 2

        # Should still have more
        assert cursor.has_more() is True

        # Clone and verify it can continue
        cursor2 = cursor.clone()
        assert cursor2.take() == 3
        assert cursor.take() == 3

    def test_peek_on_exhausted_cursor(self):
        """Test peek behavior when cursor is exhausted."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=0,
            it=iter([1])
        )

        cursor.take()  # Consume the only item
        assert cursor.has_more() is False

        # Peek on exhausted cursor should raise
        with pytest.raises(StopIteration):
            cursor.peek()

    def test_clone_independent_advancement(self):
        """Test that clones can be advanced independently."""
        cursor1 = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter([1, 2, 3, 4, 5])
        )

        # Advance cursor1
        cursor1.take()  # 1
        cursor1.take()  # 2

        # Clone at position 2
        cursor2 = cursor1.clone()

        # Advance cursor1 further
        cursor1.take()  # 3
        cursor1.take()  # 4

        # cursor2 should still be at position 2
        assert cursor2.take() == 3
        assert cursor2.take() == 4
        assert cursor2.take() == 5

        # cursor1 should be ahead
        assert cursor1.take() == 5
        assert cursor1.has_more() is False
        assert cursor2.has_more() is False


class TestStreamingClauseCursorBacktracking:
    """Test backtracking-specific behavior."""

    def test_cursor_resume_after_backtrack(self):
        """Test cursor can resume after backtracking."""
        cursor = StreamingClauseCursor(
            functor="choice",
            arity=1,
            it=iter(['a', 'b', 'c', 'd'])
        )

        # First attempt
        assert cursor.take() == 'a'

        # Check if more choices available (for choicepoint)
        assert cursor.has_more() is True

        # Simulate backtracking - cursor continues from position
        assert cursor.take() == 'b'
        assert cursor.take() == 'c'
        assert cursor.take() == 'd'
        assert cursor.has_more() is False

    def test_cursor_state_preserved_in_choicepoint(self):
        """Test cursor state is preserved correctly for choicepoints."""
        cursor = StreamingClauseCursor(
            functor="member",
            arity=2,
            it=iter([10, 20, 30])
        )

        # Take first clause
        first = cursor.take()
        assert first == 10

        # At this point, cursor should be saved in choicepoint
        # Clone simulates saving cursor state
        saved_cursor = cursor.clone()

        # Continue with original (would fail in real execution)
        # ... execution fails, backtrack occurs ...

        # Resume from saved cursor
        assert saved_cursor.has_more() is True
        assert saved_cursor.take() == 20
        assert saved_cursor.take() == 30
        assert saved_cursor.has_more() is False


class TestStreamingClauseCursorEdgeCases:
    """Test edge cases and error conditions."""

    def test_none_in_iterator(self):
        """Test handling of None values in iterator."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter([None, 0, None, 1])
        )

        assert cursor.take() is None
        assert cursor.take() == 0
        assert cursor.take() is None
        assert cursor.take() == 1

    def test_immediate_exhaustion(self):
        """Test cursor that becomes exhausted immediately."""
        items = []
        cursor = StreamingClauseCursor(
            functor="empty",
            arity=0,
            it=iter(items)
        )

        assert cursor.has_more() is False
        clone = cursor.clone()
        assert clone.has_more() is False

    def test_single_item_iterator(self):
        """Test cursor with single item."""
        cursor = StreamingClauseCursor(
            functor="fact",
            arity=0,
            it=iter([42])
        )

        assert cursor.has_more() is True
        assert cursor.peek() == 42
        assert cursor.take() == 42
        assert cursor.has_more() is False

    def test_iterator_raises_exception(self):
        """Test handling when iterator raises exception."""
        def failing_iterator():
            yield 1
            yield 2
            raise ValueError("Iterator failed")

        cursor = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=failing_iterator()
        )

        assert cursor.take() == 1
        assert cursor.take() == 2

        # Next access should propagate the exception
        with pytest.raises(ValueError, match="Iterator failed"):
            cursor.has_more()


class TestStreamingClauseCursorInterface:
    """Test that StreamingClauseCursor matches ClauseCursor interface."""

    def test_compatible_with_clausecursor_interface(self):
        """Verify StreamingClauseCursor has required ClauseCursor methods."""
        cursor = StreamingClauseCursor(
            functor="test",
            arity=1,
            it=iter([1, 2, 3])
        )

        # Check required methods exist
        assert hasattr(cursor, 'has_more')
        assert hasattr(cursor, 'peek')
        assert hasattr(cursor, 'take')
        assert hasattr(cursor, 'clone')

        # Check required attributes exist
        assert hasattr(cursor, 'functor')
        assert hasattr(cursor, 'arity')

        # Methods should be callable
        assert callable(cursor.has_more)
        assert callable(cursor.peek)
        assert callable(cursor.take)
        assert callable(cursor.clone)