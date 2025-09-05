"""Tests for Goal Stack implementation (Stage 0)."""

import pytest
from dataclasses import FrozenInstanceError

from prolog.ast.terms import Atom, Struct, Var
from prolog.engine.goals import Goal, GoalStack


class TestGoal:
    """Tests for Goal dataclass."""

    def test_goal_creation_with_term(self):
        """Test Goal creation with a term."""
        term = Atom("test")
        goal = Goal(term=term)
        assert goal.term == term

        # With Struct
        struct = Struct("parent", (Atom("john"), Atom("mary")))
        goal2 = Goal(term=struct)
        assert goal2.term == struct

        # With Var
        var = Var(0, "X")
        goal3 = Goal(term=var)
        assert goal3.term == var

    def test_goal_immutability(self):
        """Test Goal is immutable (frozen dataclass)."""
        goal = Goal(term=Atom("test"))

        # Should not be able to modify field
        with pytest.raises(FrozenInstanceError):
            goal.term = Atom("other")

    def test_goal_equality(self):
        """Test Goal equality based on term."""
        goal1 = Goal(term=Atom("test"))
        goal2 = Goal(term=Atom("test"))
        goal3 = Goal(term=Atom("other"))

        assert goal1 == goal2
        assert goal1 != goal3

    def test_goal_hashable(self):
        """Test Goal is hashable (can be used in sets/dicts)."""
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))
        goal3 = Goal(term=Atom("a"))

        # Can create a set
        goal_set = {goal1, goal2, goal3}
        assert len(goal_set) == 2  # goal1 and goal3 are equal


class TestGoalStack:
    """Tests for GoalStack class."""

    def test_stack_supports_len(self):
        """Test GoalStack supports len() for checking size."""
        stack = GoalStack()
        assert len(stack) == 0

        stack.push(Goal(term=Atom("a")))
        assert len(stack) == 1

        stack.push(Goal(term=Atom("b")))
        assert len(stack) == 2

        stack.pop()
        assert len(stack) == 1

        stack.pop()
        assert len(stack) == 0

    def test_push_adds_goal_to_stack(self):
        """Test push adds goal to stack."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("first"))
        goal2 = Goal(term=Atom("second"))

        stack.push(goal1)
        stack.push(goal2)

        # Stack should have both goals (using len instead of _stack)
        assert len(stack) == 2

    def test_pop_returns_top_goal_lifo(self):
        """Test pop returns top goal in LIFO order."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("first"))
        goal2 = Goal(term=Atom("second"))
        goal3 = Goal(term=Atom("third"))

        stack.push(goal1)
        stack.push(goal2)
        stack.push(goal3)

        # Should pop in reverse order (LIFO)
        assert stack.pop() == goal3
        assert stack.pop() == goal2
        assert stack.pop() == goal1

    def test_pop_returns_none_when_empty(self):
        """Test pop returns None when stack is empty."""
        stack = GoalStack()
        assert stack.pop() is None

        # Also after exhausting
        stack.push(Goal(term=Atom("test")))
        stack.pop()
        assert stack.pop() is None

    def test_push_body_adds_goals_in_reverse_order(self):
        """Test push_body adds goals in reverse order for left-to-right execution."""
        stack = GoalStack()

        # Simulating: a :- b, c, d.
        # Body goals should be pushed as [d, c, b] so they pop as [b, c, d]
        body = (Atom("b"), Atom("c"), Atom("d"))

        stack.push_body(body)

        # Check that we have 3 goals
        assert len(stack) == 3

        # Pop should give left-to-right execution order
        assert stack.pop().term == Atom("b")
        assert stack.pop().term == Atom("c")
        assert stack.pop().term == Atom("d")

    def test_push_body_with_empty_body(self):
        """Test push_body with empty body (fact)."""
        stack = GoalStack()
        stack.push_body(())

        assert len(stack) == 0
        assert stack.pop() is None

    def test_snapshot_creates_immutable_copy(self):
        """Test snapshot creates immutable copy as tuple."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))

        stack.push(goal1)
        stack.push(goal2)

        snapshot = stack.snapshot()

        # Should be a tuple
        assert isinstance(snapshot, tuple)
        assert len(snapshot) == 2
        assert snapshot[0] == goal1
        assert snapshot[1] == goal2

        # Cannot modify tuple
        with pytest.raises(TypeError):
            snapshot[0] = Goal(term=Atom("c"))

    def test_snapshot_unaffected_by_future_operations(self):
        """Test snapshot is unaffected by future push/pop."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))
        goal3 = Goal(term=Atom("c"))

        stack.push(goal1)
        stack.push(goal2)

        # Take snapshot
        snapshot = stack.snapshot()
        assert len(snapshot) == 2

        # Modify stack
        stack.push(goal3)
        stack.pop()
        stack.pop()

        # Snapshot unchanged
        assert len(snapshot) == 2
        assert snapshot[0] == goal1
        assert snapshot[1] == goal2

    def test_restore_from_snapshot_yields_identical_pop_order(self):
        """Test restore from snapshot yields identical pop order."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))
        goal3 = Goal(term=Atom("c"))

        stack.push(goal1)
        stack.push(goal2)
        stack.push(goal3)

        # Take snapshot
        snapshot = stack.snapshot()

        # Pop some goals
        popped1 = stack.pop()
        popped2 = stack.pop()

        # Restore from snapshot
        stack.restore(snapshot)

        # Should pop in same order as before
        assert stack.pop() == popped1  # goal3
        assert stack.pop() == popped2  # goal2
        assert stack.pop() == goal1

    def test_restore_replaces_entire_stack(self):
        """Test restore replaces entire stack contents."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))
        goal3 = Goal(term=Atom("c"))
        goal4 = Goal(term=Atom("d"))

        # Initial state
        stack.push(goal1)
        stack.push(goal2)
        snapshot1 = stack.snapshot()

        # Modify heavily
        stack.push(goal3)
        stack.push(goal4)
        assert len(stack) == 4

        # Restore
        stack.restore(snapshot1)

        # Should be back to just goal1, goal2
        assert len(stack) == 2
        assert stack.pop() == goal2
        assert stack.pop() == goal1
        assert stack.pop() is None

    def test_goalstack_with_snapshot_constructor(self):
        """Test GoalStack can be constructed from snapshot."""
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))
        snapshot = (goal1, goal2)

        # Create stack from snapshot
        stack = GoalStack(snapshot)

        assert len(stack) == 2
        assert stack.pop() == goal2
        assert stack.pop() == goal1

    def test_goalstack_constructor_copies_snapshot(self):
        """Test GoalStack constructor makes a copy of the snapshot."""
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))
        snapshot = [goal1, goal2]  # Using list to test copying

        # Create stack from snapshot
        stack = GoalStack(snapshot)

        # Modify original list
        snapshot.append(Goal(term=Atom("c")))

        # Stack should be unaffected
        assert len(stack) == 2
        assert stack.pop() == goal2
        assert stack.pop() == goal1
        assert stack.pop() is None

    def test_snapshot_of_empty_stack(self):
        """Test snapshot of empty stack."""
        stack = GoalStack()
        snapshot = stack.snapshot()

        assert snapshot == ()
        assert isinstance(snapshot, tuple)
        assert len(snapshot) == 0

    def test_restore_with_empty_snapshot(self):
        """Test restore with empty snapshot clears stack."""
        stack = GoalStack()
        stack.push(Goal(term=Atom("a")))
        stack.push(Goal(term=Atom("b")))

        assert len(stack) == 2

        # Restore empty
        stack.restore(())

        assert len(stack) == 0
        assert stack.pop() is None

    def test_peek_returns_top_without_removing(self):
        """Test peek returns top goal without removing it."""
        stack = GoalStack()
        goal1 = Goal(term=Atom("a"))
        goal2 = Goal(term=Atom("b"))

        stack.push(goal1)
        stack.push(goal2)

        # Peek should return top without removing
        assert stack.peek() == goal2
        assert stack.peek() == goal2  # Still there
        assert len(stack) == 2

        # Pop removes it
        assert stack.pop() == goal2
        assert stack.peek() == goal1
        assert len(stack) == 1

    def test_peek_returns_none_when_empty(self):
        """Test peek returns None when stack is empty."""
        stack = GoalStack()
        assert stack.peek() is None

        stack.push(Goal(term=Atom("a")))
        stack.pop()
        assert stack.peek() is None
