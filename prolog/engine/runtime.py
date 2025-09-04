"""Core runtime types for the single-loop VM (Stage 0)."""

from dataclasses import dataclass, field
from typing import Optional, Any, Dict, List, Tuple, Union
from enum import IntEnum
from prolog.ast.terms import Term, Struct


class GoalType(IntEnum):
    """Goal types for dispatch optimization."""
    PREDICATE = 0
    BUILTIN = 1
    CONJUNCTION = 2  # (A, B)
    DISJUNCTION = 3  # (A ; B)
    IF_THEN_ELSE = 4  # (A -> B ; C)
    CUT = 5  # !
    POP_FRAME = 6  # Sentinel for frame cleanup
    CONTROL = 7  # Internal control goals


class ChoicepointKind(IntEnum):
    """Choicepoint types for resume dispatch."""
    PREDICATE = 0  # Try next clause
    DISJUNCTION = 1  # Try alternative branch
    IF_THEN_ELSE = 2  # Try else branch


@dataclass(frozen=True)
class Goal:
    """Goal with type tag for optimized dispatch.
    
    Attributes:
        type: Goal type for dispatch
        term: The actual term (Struct for most goals), None for internal goals
        payload: Optional type-specific data
    """
    type: GoalType
    term: Optional[Term]
    payload: Any = None
    
    @classmethod
    def from_term(cls, term: Term) -> 'Goal':
        """Create a Goal from a term, inferring its type."""
        if not isinstance(term, Struct):
            # Variables and atoms treated as predicates
            return cls(GoalType.PREDICATE, term)
        
        functor = term.functor
        
        # Check for control constructs
        if functor == ',' and len(term.args) == 2:
            return cls(GoalType.CONJUNCTION, term)
        elif functor == ';' and len(term.args) == 2:
            # Could be disjunction or if-then-else
            left = term.args[0]
            if isinstance(left, Struct) and left.functor == '->' and len(left.args) == 2:
                # (A -> B) ; C form
                return cls(GoalType.IF_THEN_ELSE, term)
            else:
                # A ; B form
                return cls(GoalType.DISJUNCTION, term)
        elif functor == '!' and len(term.args) == 0:
            return cls(GoalType.CUT, term)
        else:
            # Everything else is a predicate or builtin (determined at runtime)
            return cls(GoalType.PREDICATE, term)


@dataclass
class Frame:
    """Activation frame for a predicate call.
    
    One frame per predicate call, popped when goals exhausted.
    
    Attributes:
        frame_id: Unique monotonic ID for this frame
        cut_barrier: CP stack height at call time (for cut scope)
        goal_height: Goal stack height when frame was created
        pred: Predicate reference for debugging
        env: Local variable bindings if needed
    """
    frame_id: int  # Unique monotonic ID
    cut_barrier: int
    goal_height: int  # Goal stack height when frame was created
    pred: Optional[Any] = None  # Predicate reference
    env: Optional[Dict[int, Any]] = None  # Local environment if needed
    
    def __repr__(self) -> str:
        pred_str = f"{self.pred}" if self.pred else "?"
        return f"Frame({pred_str}, cut={self.cut_barrier})"


@dataclass
class Choicepoint:
    """Data-only choicepoint for backtracking.
    
    All choicepoints store trail position for undo.
    Type-specific data in payload determines resume behavior.
    
    Attributes:
        kind: Type of choicepoint for resume dispatch
        trail_top: Trail position to restore to
        goal_stack_height: Goal stack height to restore to
        frame_stack_height: Frame stack height to restore to
        payload: Type-specific resume data
        cut_parent: Previous choicepoint before cut barrier (for cut)
    """
    kind: ChoicepointKind
    trail_top: int
    goal_stack_height: int
    frame_stack_height: int
    payload: Dict[str, Any]
    cut_parent: Optional[int] = None  # Index of parent CP for cut
    
    def __repr__(self) -> str:
        return f"CP({self.kind.name}, trail={self.trail_top})"


class GoalStack:
    """Stack of pending goals.
    
    Goals pushed in reverse order for correct execution.
    """
    
    def __init__(self):
        self._stack: List[Goal] = []
    
    def push(self, goal: Goal) -> None:
        """Push a goal onto the stack."""
        self._stack.append(goal)
    
    def push_many(self, goals: List[Goal]) -> None:
        """Push multiple goals in reverse order."""
        # Push in reverse so first goal is popped first
        for goal in reversed(goals):
            self._stack.append(goal)
    
    def pop(self) -> Optional[Goal]:
        """Pop the next goal or None if empty."""
        return self._stack.pop() if self._stack else None
    
    def is_empty(self) -> bool:
        """Check if the stack is empty."""
        return len(self._stack) == 0
    
    def height(self) -> int:
        """Current stack height."""
        return len(self._stack)
    
    def snapshot(self) -> tuple[Goal, ...]:
        """Create an immutable snapshot of current stack state."""
        return tuple(self._stack)
    
    def shrink_to(self, height: int) -> None:
        """Shrink stack to given height (for backtracking)."""
        assert height >= 0, f"shrink_to({height}) - height must be non-negative"
        while len(self._stack) > height:
            self._stack.pop()
    
    def __len__(self) -> int:
        return len(self._stack)
    
    def __repr__(self) -> str:
        return f"GoalStack({len(self._stack)} goals)"


class Trail:
    """Trail for tracking state changes.
    
    Records all mutations for backtracking.
    Uses write stamps to prevent redundant trailing.
    """
    
    def __init__(self):
        self._entries: List[Tuple[str, ...]] = []
        self._write_stamp: int = 0
        self._var_stamps: Dict[int, int] = {}  # varid -> last trail stamp
    
    def push(self, entry: Tuple[str, ...]) -> None:
        """Record a state change."""
        self._entries.append(entry)
    
    def push_bind(self, varid: int, old_cell: Any, stamp: Optional[int] = None) -> None:
        """Trail a variable binding with stamp check.
        
        Only trails if this variable hasn't been trailed in current choice region.
        """
        # Check if already trailed in this choice region
        if varid in self._var_stamps and self._var_stamps[varid] >= self._write_stamp:
            return  # Already trailed in this region
        
        self.push(('bind', varid, old_cell))
        self._var_stamps[varid] = self._write_stamp
    
    def push_parent(self, varid: int, old_parent: int, stamp: Optional[int] = None) -> None:
        """Trail a union-find parent change with stamp check."""
        if varid in self._var_stamps and self._var_stamps[varid] >= self._write_stamp:
            return
        
        self.push(('parent', varid, old_parent))
        self._var_stamps[varid] = self._write_stamp
    
    def push_attr(self, varid: int, module: str, old_value: Any, stamp: Optional[int] = None) -> None:
        """Trail an attribute change with stamp check."""
        key = (varid, module)  # Track per variable and module
        if key in self._var_stamps and self._var_stamps[key] >= self._write_stamp:
            return
        
        self.push(('attr', varid, module, old_value))
        self._var_stamps[key] = self._write_stamp
    
    def push_domain(self, varid: int, old_domain: Any, stamp: Optional[int] = None) -> None:
        """Trail a domain change with stamp check."""
        if varid in self._var_stamps and self._var_stamps[varid] >= self._write_stamp:
            return
        
        self.push(('domain', varid, old_domain))
        self._var_stamps[varid] = self._write_stamp
    
    def push_rank(self, varid: int, old_rank: int, stamp: Optional[int] = None) -> None:
        """Trail a rank change with stamp check."""
        if varid in self._var_stamps and self._var_stamps[varid] >= self._write_stamp:
            return
        
        self.push(('rank', varid, old_rank))
        self._var_stamps[varid] = self._write_stamp
    
    def position(self) -> int:
        """Current trail position."""
        return len(self._entries)
    
    def next_stamp(self) -> int:
        """Get next write stamp and increment.
        
        Called when entering a new choice region.
        """
        stamp = self._write_stamp
        self._write_stamp += 1
        return stamp
    
    def unwind_to(self, position: int, store: Any) -> None:
        """Restore state by unwinding trail to given position.
        
        Args:
            position: Trail position to restore to
            store: The Store to restore state in
        """
        while len(self._entries) > position:
            entry = self._entries.pop()
            kind = entry[0]
            
            if kind == 'bind':
                _, varid, old_cell = entry
                store.cells[varid] = old_cell
            elif kind == 'parent':
                _, varid, old_parent = entry
                store.cells[varid].ref = old_parent
            elif kind == 'attr':
                _, varid, module, old_value = entry
                # Ensure attrs dict exists on store
                if not hasattr(store, 'attrs'):
                    store.attrs = {}
                if varid not in store.attrs:
                    store.attrs[varid] = {}
                if old_value is None:
                    # Was not present before
                    store.attrs[varid].pop(module, None)
                else:
                    store.attrs[varid][module] = old_value
            elif kind == 'domain':
                _, varid, old_domain = entry
                # Ensure domains dict exists on store
                if not hasattr(store, 'domains'):
                    store.domains = {}
                store.domains[varid] = old_domain
            elif kind == 'rank':
                # Rank changes for union-find optimization
                _, varid, old_rank = entry
                store.cells[varid].rank = old_rank
            else:
                raise ValueError(f"Unknown trail entry kind: {kind}")
    
    def __len__(self) -> int:
        return len(self._entries)
    
    def __repr__(self) -> str:
        return f"Trail({len(self._entries)} entries, stamp={self._write_stamp})"


# Tests for Trail unwind_to
def test_trail_unwind():
    """Test that trail correctly restores state."""
    from prolog.unify.store import Store
    from prolog.ast.terms import Var, Atom
    
    store = Store()
    trail = Trail()
    
    # Create some variables
    x = store.new_var("X")
    y = store.new_var("Y")
    
    # Record initial trail position
    pos0 = trail.position()
    
    # Make some changes with trailing
    old_cell_x = store.cells[x]
    store.cells[x] = Atom("a")
    trail.push_bind(x, old_cell_x)
    
    old_cell_y = store.cells[y]
    store.cells[y] = Atom("b")
    trail.push_bind(y, old_cell_y)
    
    # Verify changes took effect
    assert store.cells[x] == Atom("a")
    assert store.cells[y] == Atom("b")
    
    # Unwind to initial position
    trail.unwind_to(pos0, store)
    
    # Verify restoration
    assert store.cells[x] == old_cell_x
    assert store.cells[y] == old_cell_y
    
    print("Trail unwind test passed!")


if __name__ == "__main__":
    # Run basic tests
    test_trail_unwind()
    
    # Test goal creation
    from prolog.ast.terms import Struct, Atom
    
    # Test predicate goal
    pred = Struct("foo", (Atom("a"),))
    goal = Goal.from_term(pred)
    assert goal.type == GoalType.PREDICATE
    
    # Test conjunction
    conj = Struct(",", (Atom("a"), Atom("b")))
    goal = Goal.from_term(conj)
    assert goal.type == GoalType.CONJUNCTION
    
    # Test disjunction
    disj = Struct(";", (Atom("a"), Atom("b")))
    goal = Goal.from_term(disj)
    assert goal.type == GoalType.DISJUNCTION
    
    # Test if-then-else
    ite = Struct(";", (Struct("->", (Atom("a"), Atom("b"))), Atom("c")))
    goal = Goal.from_term(ite)
    assert goal.type == GoalType.IF_THEN_ELSE
    
    # Test cut
    cut = Struct("!", ())
    goal = Goal.from_term(cut)
    assert goal.type == GoalType.CUT
    
    print("Goal type detection tests passed!")
    
    # Test goal stack
    stack = GoalStack()
    stack.push(Goal(GoalType.PREDICATE, Atom("a")))
    stack.push(Goal(GoalType.PREDICATE, Atom("b")))
    
    assert stack.height() == 2
    g = stack.pop()
    assert g.term == Atom("b")
    assert stack.height() == 1
    
    print("Goal stack tests passed!")
    
    print("\nAll runtime type tests passed!")