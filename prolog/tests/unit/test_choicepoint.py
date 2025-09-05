"""Tests for Choicepoint system (Stage 0)."""

import pytest
from dataclasses import FrozenInstanceError

from prolog.ast.terms import Atom, Struct
from prolog.ast.clauses import ClauseCursor
from prolog.engine.goals import Goal
from prolog.engine.choicepoint import Choicepoint, ChoiceStack


class TestChoicepoint:
    """Tests for Choicepoint dataclass."""

    def test_choicepoint_creation_with_all_fields(self):
        """Test Choicepoint creation with all required fields."""
        # Setup
        goals_snapshot = (Goal(term=Atom("a")), Goal(term=Atom("b")))
        cursor = ClauseCursor("test", 2, [1, 2, 3])
        cursor.take()  # Advance to position 1

        # Create choicepoint
        cp = Choicepoint(
            id=1,
            goals=goals_snapshot,
            cursor=cursor,
            trail_mark=42,
            cut_barrier=0,
            store_size=10,
        )

        assert cp.id == 1
        assert cp.goals == goals_snapshot
        assert cp.cursor == cursor
        assert cp.trail_mark == 42
        assert cp.cut_barrier == 0
        assert cp.store_size == 10

    def test_choicepoint_stores_immutable_goal_snapshot(self):
        """Test Choicepoint stores goals as immutable tuple."""
        goals_snapshot = (Goal(term=Atom("test")),)
        cursor = ClauseCursor("pred", 1, [0])

        cp = Choicepoint(
            id=5,
            goals=goals_snapshot,
            cursor=cursor,
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )

        # goals should be a tuple
        assert isinstance(cp.goals, tuple)

        # Cannot modify the tuple
        with pytest.raises(TypeError):
            cp.goals[0] = Goal(term=Atom("other"))

    def test_choicepoint_pre_goal_snapshot_semantics(self):
        """Test Choicepoint preserves pre-goal snapshot (goal still on top)."""
        # This is a semantic test - the goal being tried should still be
        # on top of the snapshot so backtracking can retry it
        goal_being_tried = Goal(term=Struct("test", (Atom("a"),)))
        other_goals = [Goal(term=Atom("b")), Goal(term=Atom("c"))]

        # Pre-goal snapshot: goal_being_tried is still on stack
        goals_snapshot = tuple(other_goals + [goal_being_tried])

        cursor = ClauseCursor("test", 1, [1, 2, 3])
        cursor.take()  # First clause already tried

        cp = Choicepoint(
            id=1,
            goals=goals_snapshot,
            cursor=cursor,
            trail_mark=0,
            cut_barrier=None,
            store_size=5,
        )

        # The last goal in snapshot should be the one being retried
        assert cp.goals[-1] == goal_being_tried
        # Cursor should be positioned at next clause to try
        assert cursor.peek() == 2

    def test_choicepoint_immutability(self):
        """Test Choicepoint is immutable (frozen dataclass)."""
        goals = (Goal(term=Atom("a")),)
        cursor = ClauseCursor("test", 0, [])

        cp = Choicepoint(
            id=1,
            goals=goals,
            cursor=cursor,
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )

        # Should not be able to modify fields
        with pytest.raises(FrozenInstanceError):
            cp.id = 2

        with pytest.raises(FrozenInstanceError):
            cp.trail_mark = 100

        with pytest.raises(FrozenInstanceError):
            cp.goals = ()

    def test_choicepoint_with_none_cut_barrier(self):
        """Test Choicepoint with None cut_barrier."""
        cp = Choicepoint(
            id=1,
            goals=(),
            cursor=ClauseCursor("test", 0, []),
            trail_mark=0,
            cut_barrier=None,  # No barrier
            store_size=0,
        )

        assert cp.cut_barrier is None


class TestChoiceStack:
    """Tests for ChoiceStack class."""

    def test_push_returns_unique_sequential_id(self):
        """Test push returns unique sequential IDs without mutating choicepoints."""
        stack = ChoiceStack()

        # Create frozen choicepoints with their own IDs
        cp1 = Choicepoint(
            id=99,  # Arbitrary ID, won't be changed
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )

        cp2 = Choicepoint(
            id=42,  # Another arbitrary ID
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )

        # Push and get returned IDs
        returned_id1 = stack.push(cp1)
        returned_id2 = stack.push(cp2)

        # Should return sequential IDs from the stack
        assert returned_id1 == 1
        assert returned_id2 == 2

        # Original choicepoints should be unchanged (frozen)
        assert cp1.id == 99  # Not mutated
        assert cp2.id == 42  # Not mutated

        # Stack should track next ID internally
        assert stack._next_id == 3

    def test_push_stores_choicepoint_with_original_id(self):
        """Test push stores choicepoint with its original ID field."""
        stack = ChoiceStack()

        cp = Choicepoint(
            id=123,
            goals=(),
            cursor=ClauseCursor("test", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )

        stack.push(cp)

        # Popped choicepoint should have original ID
        popped = stack.pop()
        assert popped.id == 123  # Original ID preserved

    def test_pop_returns_most_recent_choicepoint_lifo(self):
        """Test pop returns most recent choicepoint (LIFO order)."""
        stack = ChoiceStack()

        cp1 = Choicepoint(
            id=1,
            goals=(Goal(term=Atom("a")),),
            cursor=ClauseCursor("test", 1, [0]),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )

        cp2 = Choicepoint(
            id=2,
            goals=(Goal(term=Atom("b")),),
            cursor=ClauseCursor("test", 1, [1]),
            trail_mark=1,
            cut_barrier=1,
            store_size=1,
        )

        stack.push(cp1)
        stack.push(cp2)

        # Should pop in LIFO order
        popped = stack.pop()
        assert popped == cp2

        popped = stack.pop()
        assert popped == cp1

    def test_pop_returns_none_when_empty(self):
        """Test pop returns None when stack is empty."""
        stack = ChoiceStack()
        assert stack.pop() is None

        # Also after exhausting
        cp = Choicepoint(
            id=1,
            goals=(),
            cursor=ClauseCursor("test", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        stack.push(cp)
        stack.pop()
        assert stack.pop() is None

    def test_cut_to_removes_exactly_newer_choicepoints(self):
        """Test cut_to removes exactly choicepoints newer than barrier.

        Note: cut_to uses the stack-assigned IDs (returned by push),
        not the choicepoint's internal id field.
        """
        stack = ChoiceStack()

        # Push several choicepoints - push returns sequential IDs
        cp1 = Choicepoint(
            id=10,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        cp2 = Choicepoint(
            id=20,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=1,
            store_size=1,
        )
        cp3 = Choicepoint(
            id=30,
            goals=(),
            cursor=ClauseCursor("c", 0, []),
            trail_mark=2,
            cut_barrier=2,
            store_size=2,
        )
        cp4 = Choicepoint(
            id=40,
            goals=(),
            cursor=ClauseCursor("d", 0, []),
            trail_mark=3,
            cut_barrier=2,
            store_size=3,
        )

        id1 = stack.push(cp1)  # Returns 1
        id2 = stack.push(cp2)  # Returns 2
        id3 = stack.push(cp3)  # Returns 3
        id4 = stack.push(cp4)  # Returns 4

        assert id1 == 1
        assert id2 == 2
        assert id3 == 3
        assert id4 == 4

        # Cut to barrier 2 (should remove choicepoints with stack IDs > 2)
        stack.cut_to(2)

        # Should have only cp1 and cp2 left
        assert len(stack._stack) == 2
        assert stack._stack[0] == cp1
        assert stack._stack[1] == cp2

        # Popping should give cp2 then cp1
        assert stack.pop() == cp2
        assert stack.pop() == cp1
        assert stack.pop() is None

    def test_cut_to_preserves_choicepoints_at_or_before_barrier(self):
        """Test cut_to preserves choicepoints at or before barrier."""
        stack = ChoiceStack()

        cp1 = Choicepoint(
            id=1,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        cp2 = Choicepoint(
            id=2,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )
        cp3 = Choicepoint(
            id=3,
            goals=(),
            cursor=ClauseCursor("c", 0, []),
            trail_mark=2,
            cut_barrier=None,
            store_size=2,
        )

        stack.push(cp1)
        stack.push(cp2)
        stack.push(cp3)

        # Cut to 2 should keep cp1 and cp2
        stack.cut_to(2)

        assert len(stack._stack) == 2
        assert stack._stack[0] == cp1
        assert stack._stack[1] == cp2

    def test_cut_to_with_none_barrier_is_noop(self):
        """Test cut_to with None barrier does nothing."""
        stack = ChoiceStack()

        cp1 = Choicepoint(
            id=1,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        cp2 = Choicepoint(
            id=2,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )

        stack.push(cp1)
        stack.push(cp2)

        # Cut with None should do nothing
        stack.cut_to(None)

        assert len(stack._stack) == 2
        assert stack._stack[0] == cp1
        assert stack._stack[1] == cp2

    def test_cut_to_all_choicepoints(self):
        """Test cut_to(0) removes all choicepoints."""
        stack = ChoiceStack()

        cp1 = Choicepoint(
            id=1,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        cp2 = Choicepoint(
            id=2,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )

        stack.push(cp1)
        stack.push(cp2)

        # Cut to 0 should remove everything (all IDs > 0)
        stack.cut_to(0)

        assert len(stack._stack) == 0
        assert stack.pop() is None

    def test_find_locates_choicepoint_by_id(self):
        """Test find locates choicepoint by ID."""
        stack = ChoiceStack()

        cp1 = Choicepoint(
            id=10,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        cp2 = Choicepoint(
            id=20,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )
        cp3 = Choicepoint(
            id=30,
            goals=(),
            cursor=ClauseCursor("c", 0, []),
            trail_mark=2,
            cut_barrier=None,
            store_size=2,
        )

        stack._stack.append(cp1)  # Manually set IDs for this test
        stack._stack.append(cp2)
        stack._stack.append(cp3)

        # Find by ID
        assert stack.find(20) == cp2
        assert stack.find(10) == cp1
        assert stack.find(30) == cp3
        assert stack.find(999) is None  # Non-existent

    def test_top_id_returns_current_top_id_or_none(self):
        """Test top_id returns ID of top choicepoint or None."""
        stack = ChoiceStack()

        # Empty stack
        assert stack.top_id() is None

        # With choicepoints
        cp1 = Choicepoint(
            id=5,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )
        cp2 = Choicepoint(
            id=8,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )

        stack._stack.append(cp1)
        assert stack.top_id() == 5

        stack._stack.append(cp2)
        assert stack.top_id() == 8

        stack.pop()
        assert stack.top_id() == 5

        stack.pop()
        assert stack.top_id() is None

    def test_choicestack_preserves_cursor_state(self):
        """Test ChoiceStack preserves cursor state in choicepoints."""
        stack = ChoiceStack()

        # Create cursor that has been partially consumed
        cursor = ClauseCursor("test", 2, [10, 20, 30])
        cursor.take()  # Advance to position 1

        # Store in choicepoint
        cp = Choicepoint(
            id=1, goals=(), cursor=cursor, trail_mark=0, cut_barrier=None, store_size=0
        )

        stack.push(cp)

        # Pop and check cursor state preserved
        popped = stack.pop()
        assert popped.cursor.pos == 1
        assert popped.cursor.peek() == 20

    def test_multiple_cut_operations(self):
        """Test multiple cut operations work correctly."""
        stack = ChoiceStack()

        # Build a stack with IDs 1, 2, 3, 4, 5
        for i in range(1, 6):
            cp = Choicepoint(
                id=i,
                goals=(),
                cursor=ClauseCursor(f"p{i}", 0, []),
                trail_mark=i - 1,
                cut_barrier=None,
                store_size=i - 1,
            )
            stack._stack.append(cp)
            stack._next_id = i + 1

        # Cut to 3 (remove 4, 5)
        stack.cut_to(3)
        assert len(stack._stack) == 3
        assert stack.top_id() == 3

        # Cut to 1 (remove 2, 3)
        stack.cut_to(1)
        assert len(stack._stack) == 1
        assert stack.top_id() == 1

        # Cut to 0 (remove all)
        stack.cut_to(0)
        assert len(stack._stack) == 0
        assert stack.top_id() is None

    def test_current_id_returns_next_id_to_be_assigned(self):
        """Test current_id returns the next ID that will be assigned."""
        stack = ChoiceStack()

        # Initially should be 1
        assert stack.current_id() == 1

        cp1 = Choicepoint(
            id=99,
            goals=(),
            cursor=ClauseCursor("a", 0, []),
            trail_mark=0,
            cut_barrier=None,
            store_size=0,
        )

        # Push increments the internal counter
        id1 = stack.push(cp1)
        assert id1 == 1
        assert stack.current_id() == 2  # Next to be assigned

        cp2 = Choicepoint(
            id=88,
            goals=(),
            cursor=ClauseCursor("b", 0, []),
            trail_mark=1,
            cut_barrier=None,
            store_size=1,
        )

        id2 = stack.push(cp2)
        assert id2 == 2
        assert stack.current_id() == 3  # Next to be assigned

        # Pop doesn't affect current_id (it's about what's next to assign)
        stack.pop()
        assert stack.current_id() == 3  # Still 3
