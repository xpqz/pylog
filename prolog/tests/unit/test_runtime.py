"""Unit tests for core runtime types (Stage 0)."""

import pytest
from prolog.engine.runtime import (
    GoalType, ChoicepointKind, Goal, Frame, Choicepoint,
    GoalStack, Trail
)
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.unify.store import Store


class TestGoal:
    """Test Goal creation and type detection."""
    
    def test_predicate_goal(self):
        """Test predicate goal detection."""
        pred = Struct("foo", (Atom("a"),))
        goal = Goal.from_term(pred)
        assert goal.type == GoalType.PREDICATE
        assert goal.term == pred
    
    def test_conjunction_goal(self):
        """Test conjunction detection."""
        conj = Struct(",", (Atom("a"), Atom("b")))
        goal = Goal.from_term(conj)
        assert goal.type == GoalType.CONJUNCTION
        assert goal.term == conj
    
    def test_disjunction_goal(self):
        """Test disjunction detection."""
        disj = Struct(";", (Atom("a"), Atom("b")))
        goal = Goal.from_term(disj)
        assert goal.type == GoalType.DISJUNCTION
        assert goal.term == disj
    
    def test_if_then_else_goal(self):
        """Test if-then-else detection."""
        # (A -> B) ; C form
        ite = Struct(";", (
            Struct("->", (Atom("test"), Atom("then"))),
            Atom("else")
        ))
        goal = Goal.from_term(ite)
        assert goal.type == GoalType.IF_THEN_ELSE
        assert goal.term == ite
    
    def test_cut_goal(self):
        """Test cut detection."""
        cut = Struct("!", ())
        goal = Goal.from_term(cut)
        assert goal.type == GoalType.CUT
        assert goal.term == cut
    
    def test_atom_as_predicate(self):
        """Test that atoms are treated as predicates."""
        atom = Atom("true")
        goal = Goal.from_term(atom)
        assert goal.type == GoalType.PREDICATE
        assert goal.term == atom
    
    def test_nested_control_structures(self):
        """Test nested control structure detection."""
        # ((A, B) ; C)
        nested = Struct(";", (
            Struct(",", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        goal = Goal.from_term(nested)
        assert goal.type == GoalType.DISJUNCTION
        
        # (A ; (B ; C))
        nested2 = Struct(";", (
            Atom("a"),
            Struct(";", (Atom("b"), Atom("c")))
        ))
        goal2 = Goal.from_term(nested2)
        assert goal2.type == GoalType.DISJUNCTION


class TestFrame:
    """Test Frame creation and properties."""
    
    def test_frame_creation(self):
        """Test basic frame creation."""
        frame = Frame(frame_id=1, cut_barrier=5, goal_height=10, pred="test/1")
        assert frame.frame_id == 1
        assert frame.cut_barrier == 5
        assert frame.goal_height == 10
        assert frame.pred == "test/1"
        assert frame.env is None
    
    def test_frame_with_env(self):
        """Test frame with environment."""
        env = {0: Atom("a"), 1: Atom("b")}
        frame = Frame(frame_id=2, cut_barrier=3, goal_height=8, pred="foo/2", env=env)
        assert frame.env == env
    
    def test_frame_repr(self):
        """Test frame string representation."""
        frame = Frame(frame_id=3, cut_barrier=2, goal_height=5, pred="test/0")
        assert "test/0" in repr(frame)
        assert "cut=2" in repr(frame)


class TestChoicepoint:
    """Test Choicepoint creation and properties."""
    
    def test_predicate_choicepoint(self):
        """Test predicate choicepoint creation."""
        cp = Choicepoint(
            kind=ChoicepointKind.PREDICATE,
            trail_top=100,
            goal_stack_height=10,
            frame_stack_height=2,
            payload={"pred_ref": "test/1", "next_clause": 1}
        )
        assert cp.kind == ChoicepointKind.PREDICATE
        assert cp.trail_top == 100
        assert cp.payload["next_clause"] == 1
    
    def test_disjunction_choicepoint(self):
        """Test disjunction choicepoint creation."""
        right_goal = Goal(GoalType.PREDICATE, Atom("b"))
        cp = Choicepoint(
            kind=ChoicepointKind.DISJUNCTION,
            trail_top=50,
            goal_stack_height=5,
            frame_stack_height=1,
            payload={"alternative": right_goal}
        )
        assert cp.kind == ChoicepointKind.DISJUNCTION
        assert cp.payload["alternative"] == right_goal
    
    def test_choicepoint_with_cut_parent(self):
        """Test choicepoint with cut parent."""
        cp = Choicepoint(
            kind=ChoicepointKind.PREDICATE,
            trail_top=0,
            goal_stack_height=0,
            frame_stack_height=0,
            payload={},
            cut_parent=3
        )
        assert cp.cut_parent == 3


class TestGoalStack:
    """Test GoalStack operations."""
    
    def test_push_pop(self):
        """Test basic push and pop operations."""
        stack = GoalStack()
        assert stack.is_empty()
        
        g1 = Goal(GoalType.PREDICATE, Atom("a"))
        g2 = Goal(GoalType.PREDICATE, Atom("b"))
        
        stack.push(g1)
        assert not stack.is_empty()
        assert stack.height() == 1
        
        stack.push(g2)
        assert stack.height() == 2
        
        popped = stack.pop()
        assert popped == g2
        assert stack.height() == 1
        
        popped = stack.pop()
        assert popped == g1
        assert stack.is_empty()
    
    def test_push_many(self):
        """Test pushing multiple goals."""
        stack = GoalStack()
        goals = [
            Goal(GoalType.PREDICATE, Atom("a")),
            Goal(GoalType.PREDICATE, Atom("b")),
            Goal(GoalType.PREDICATE, Atom("c"))
        ]
        
        stack.push_many(goals)
        assert stack.height() == 3
        
        # Should be popped in order: a, b, c
        assert stack.pop().term == Atom("a")
        assert stack.pop().term == Atom("b")
        assert stack.pop().term == Atom("c")
    
    def test_shrink_to(self):
        """Test shrinking stack to height."""
        stack = GoalStack()
        for i in range(10):
            stack.push(Goal(GoalType.PREDICATE, Int(i)))
        
        assert stack.height() == 10
        stack.shrink_to(5)
        assert stack.height() == 5
        
        # Top should be Int(4)
        assert stack.pop().term == Int(4)
    
    def test_pop_from_empty(self):
        """Test popping from empty stack returns None."""
        stack = GoalStack()
        assert stack.pop() is None


class TestTrail:
    """Test Trail operations and restoration."""
    
    def test_trail_push_and_position(self):
        """Test basic trail operations."""
        trail = Trail()
        assert trail.position() == 0
        
        trail.push(('bind', 0, Atom("old"), 0))
        assert trail.position() == 1
        
        trail.push(('parent', 1, 0, 0))
        assert trail.position() == 2
    
    def test_write_stamps(self):
        """Test write stamp generation."""
        trail = Trail()
        
        stamp1 = trail.next_stamp()
        assert stamp1 == 0
        
        stamp2 = trail.next_stamp()
        assert stamp2 == 1
        
        stamp3 = trail.next_stamp()
        assert stamp3 == 2
    
    def test_stamp_guarded_trailing(self):
        """Test that trailing only happens once per choice region."""
        trail = Trail()
        
        # First binding in choice region
        trail.push_bind(0, Atom("old"))
        assert trail.position() == 1
        
        # Try to trail same variable again in same region (shouldn't add)
        trail.push_bind(0, Atom("newer"))
        assert trail.position() == 1  # No new entry
        
        # Enter new choice region
        trail.next_stamp()
        
        # Now trailing should work again
        trail.push_bind(0, Atom("newest"))
        assert trail.position() == 2  # New entry added
    
    def test_unwind_bind(self):
        """Test unwinding variable bindings."""
        store = Store()
        trail = Trail()
        
        # Create variable
        x = store.new_var("X")
        old_cell = store.cells[x]
        
        # Record position before change
        pos = trail.position()
        
        # Bind variable with trailing
        store.cells[x] = Atom("bound")
        trail.push_bind(x, old_cell)
        
        # Verify binding
        assert store.cells[x] == Atom("bound")
        
        # Unwind
        trail.unwind_to(pos, store)
        
        # Verify restoration
        assert store.cells[x] == old_cell
    
    def test_unwind_parent(self):
        """Test unwinding union-find parent changes."""
        store = Store()
        trail = Trail()
        
        x = store.new_var("X")
        y = store.new_var("Y")
        
        pos = trail.position()
        
        # Change parent with trailing
        old_parent = store.cells[x].ref
        store.cells[x].ref = y
        trail.push_parent(x, old_parent)
        
        assert store.cells[x].ref == y
        
        # Unwind
        trail.unwind_to(pos, store)
        
        assert store.cells[x].ref == old_parent
    
    def test_unwind_attr(self):
        """Test unwinding attribute changes."""
        store = Store()
        trail = Trail()
        
        x = store.new_var("X")
        
        # Initialize attrs dict on store
        store.attrs = {}
        
        # Add attribute
        pos1 = trail.position()
        store.attrs[x] = {"test": "value"}
        trail.push_attr(x, "test", None)  # None = wasn't present
        
        # Enter new choice region for next change
        trail.next_stamp()
        
        # Modify attribute
        pos2 = trail.position()
        old_value = store.attrs[x]["test"]
        store.attrs[x]["test"] = "new_value"
        trail.push_attr(x, "test", old_value)
        
        assert store.attrs[x]["test"] == "new_value"
        
        # Unwind modification
        trail.unwind_to(pos2, store)
        assert store.attrs[x]["test"] == "value"
        
        # Unwind creation
        trail.unwind_to(pos1, store)
        assert "test" not in store.attrs.get(x, {})
    
    def test_unwind_multiple(self):
        """Test unwinding multiple changes in correct order."""
        store = Store()
        trail = Trail()
        
        x = store.new_var("X")
        y = store.new_var("Y")
        
        pos = trail.position()
        
        # Make several changes
        old_x = store.cells[x]
        old_x_parent = store.cells[x].ref  # Save parent before overwriting
        store.cells[x] = Atom("a")
        trail.push_bind(x, old_x)
        
        old_y = store.cells[y]
        store.cells[y] = Atom("b")
        trail.push_bind(y, old_y)
        
        # Can't change parent on a bound variable, skip this test
        # old_parent = store.cells[x].ref
        # store.cells[x].ref = y
        # trail.push_parent(x, old_parent)
        
        # Verify changes
        assert store.cells[x] == Atom("a")
        assert store.cells[y] == Atom("b")
        
        # Unwind all
        trail.unwind_to(pos, store)
        
        # Verify all restored
        assert store.cells[x] == old_x
        assert store.cells[y] == old_y
    
    def test_partial_unwind(self):
        """Test unwinding to intermediate position."""
        store = Store()
        trail = Trail()
        
        x = store.new_var("X")
        
        pos1 = trail.position()
        old1 = store.cells[x]
        store.cells[x] = Atom("first")
        trail.push_bind(x, old1)
        
        # Enter new choice region
        trail.next_stamp()
        
        pos2 = trail.position()
        old2 = store.cells[x]
        store.cells[x] = Atom("second")
        trail.push_bind(x, old2)
        
        # Enter another choice region
        trail.next_stamp()
        
        pos3 = trail.position()
        old3 = store.cells[x]
        store.cells[x] = Atom("third")
        trail.push_bind(x, old3)
        
        # Unwind to middle position
        trail.unwind_to(pos2, store)
        assert store.cells[x] == Atom("first")
        
        # Unwind to beginning
        trail.unwind_to(pos1, store)
        assert store.cells[x] == old1