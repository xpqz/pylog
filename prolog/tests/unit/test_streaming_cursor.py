"""
Unit tests for StreamingClauseCursor.

Tests the streaming cursor implementation that lazily yields clauses
from an iterator while maintaining backtracking support.
"""

import pytest

from prolog.engine.cursors import StreamingClauseCursor
from prolog.ast.clauses import ClauseCursor


class TestStreamingClauseCursorBasics:
    """Test basic StreamingClauseCursor operations."""

    def test_has_more_on_empty_iterator(self):
        """Test has_more returns False for empty iterator."""
        cursor = StreamingClauseCursor(functor="test", arity=0, it=iter([]))
        assert cursor.has_more() is False

    def test_has_more_before_consumption(self):
        """Test has_more returns True when items available."""
        cursor = StreamingClauseCursor(functor="test", arity=1, it=iter([0, 1, 2]))
        assert cursor.has_more() is True

    def test_peek_does_not_advance(self):
        """Test peek returns next item without advancing position."""
        cursor = StreamingClauseCursor(functor="test", arity=1, it=iter([10, 20, 30]))

        # First peek
        first_peek = cursor.peek()
        assert first_peek == 10

        # Consecutive peeks return same value
        second_peek = cursor.peek()
        assert second_peek == 10
        assert first_peek == second_peek

        # Position not advanced
        assert cursor.has_more() is True

        # take() after peek() returns that same element
        taken = cursor.take()
        assert taken == first_peek

        # Next peek shows next item
        assert cursor.peek() == 20

    def test_take_consumes_items_in_order(self):
        """Test take consumes items maintaining order."""
        items = [100, 200, 300, 400]
        cursor = StreamingClauseCursor(functor="pred", arity=2, it=iter(items))

        result = []
        while cursor.has_more():
            result.append(cursor.take())

        assert result == items

    def test_exhaustion_behavior(self):
        """Test behavior when cursor is exhausted."""
        cursor = StreamingClauseCursor(functor="test", arity=0, it=iter([1, 2]))

        # Consume all items
        cursor.take()  # 1
        assert cursor.has_more() is True
        cursor.take()  # 2

        # has_more toggles to false at the end
        assert cursor.has_more() is False

        # Subsequent take() raises StopIteration
        with pytest.raises(StopIteration):
            cursor.take()

    def test_empty_iterator(self):
        """Test empty iterator behavior."""
        cursor = StreamingClauseCursor(functor="empty", arity=0, it=iter([]))

        assert cursor.has_more() is False

        with pytest.raises(StopIteration):
            cursor.peek()

        with pytest.raises(StopIteration):
            cursor.take()

    def test_singleton_iterator(self):
        """Test single item iterator behavior."""
        cursor = StreamingClauseCursor(functor="fact", arity=0, it=iter([42]))

        assert cursor.has_more() is True
        assert cursor.peek() == 42
        assert cursor.take() == 42
        assert cursor.has_more() is False

        with pytest.raises(StopIteration):
            cursor.take()


class TestStreamingClauseCursorClone:
    """Test clone functionality for backtracking support."""

    def test_clone_at_start(self):
        """Test cloning cursor at start position."""
        items = [1, 2, 3, 4, 5]
        cursor1 = StreamingClauseCursor(functor="test", arity=1, it=iter(items))

        # Clone before any consumption
        cursor2 = cursor1.clone()

        # Both cursors yield same sequence
        assert cursor1.take() == 1
        assert cursor2.take() == 1
        assert cursor1.take() == 2
        assert cursor2.take() == 2

    def test_clone_at_same_logical_position(self):
        """Test clone yields same remaining sequence."""
        cursor1 = StreamingClauseCursor(
            functor="test", arity=2, it=iter([10, 20, 30, 40])
        )

        # Consume first two items
        cursor1.take()  # 10
        cursor1.take()  # 20

        # Clone at this position
        cursor2 = cursor1.clone()

        # Both yield same remaining sequence
        remaining1 = []
        while cursor1.has_more():
            remaining1.append(cursor1.take())

        remaining2 = []
        while cursor2.has_more():
            remaining2.append(cursor2.take())

        assert remaining1 == remaining2 == [30, 40]

    def test_clone_concurrent_use_undefined(self):
        """Test that concurrent clone usage is documented as undefined."""
        # This test documents that clones should not be used concurrently
        # The behavior is undefined if you interleave take() calls
        cursor1 = StreamingClauseCursor(functor="test", arity=1, it=iter([1, 2, 3, 4]))

        cursor2 = cursor1.clone()

        # Document that this pattern is undefined/not supported
        # Implementations may guard against it or leave undefined
        # For now we just document the constraint
        pass  # No assertion - just documentation


class TestStreamingClauseCursorExceptions:
    """Test exception handling."""

    def test_iterator_exception_surfaced_consistently(self):
        """Test exceptions from iterator are surfaced consistently."""

        def failing_iterator():
            yield 1
            yield 2
            raise ValueError("Iterator failed")

        cursor = StreamingClauseCursor(functor="test", arity=1, it=failing_iterator())

        cursor.take()  # 1
        cursor.take()  # 2

        # Exception surfaced on next has_more() call
        with pytest.raises(ValueError, match="Iterator failed"):
            cursor.has_more()

        # Subsequent calls also raise
        with pytest.raises(ValueError, match="Iterator failed"):
            cursor.peek()

        with pytest.raises(ValueError, match="Iterator failed"):
            cursor.take()


class TestStreamingClauseCursorInterface:
    """Test interface compatibility with ClauseCursor."""

    def test_required_methods_exist(self):
        """Test StreamingClauseCursor has ClauseCursor-compatible API."""
        cursor = StreamingClauseCursor(functor="test", arity=1, it=iter([1, 2, 3]))

        # Check required methods exist and are callable
        assert callable(cursor.has_more)
        assert callable(cursor.peek)
        assert callable(cursor.take)
        assert callable(cursor.clone)

        # Check required attributes exist
        assert cursor.functor == "test"
        assert cursor.arity == 1

    def test_interface_parity_with_clausecursor(self):
        """Helper to verify interface parity between cursors."""
        data = [10, 20, 30]

        # Create both cursor types with same data
        streaming = StreamingClauseCursor(functor="test", arity=2, it=iter(data))

        regular = ClauseCursor(matches=data, functor="test", arity=2)

        # Run identical assertions
        assert streaming.functor == regular.functor
        assert streaming.arity == regular.arity

        # Both should have more initially
        assert streaming.has_more() == regular.has_more()

        # Both should peek same first value
        assert streaming.peek() == regular.peek()

        # Both should take in same order
        for _ in range(len(data)):
            assert streaming.take() == regular.take()

        # Both should be exhausted
        assert streaming.has_more() == regular.has_more()
        assert streaming.has_more() is False


class TestStreamingClauseCursorEdgeCases:
    """Test edge cases."""

    def test_none_values_handled(self):
        """Test None values in iterator are handled correctly."""
        cursor = StreamingClauseCursor(
            functor="test", arity=1, it=iter([None, 0, None, 1])
        )

        assert cursor.take() is None
        assert cursor.take() == 0
        assert cursor.take() is None
        assert cursor.take() == 1
        assert cursor.has_more() is False

    def test_interleaved_peek_and_take(self):
        """Test interleaving peek and take operations."""
        cursor = StreamingClauseCursor(functor="test", arity=1, it=iter(range(5)))

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
