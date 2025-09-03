"""Goal stack implementation for Prolog execution (Stage 0)."""

from dataclasses import dataclass
from typing import Optional, Tuple, Sequence
from prolog.ast.terms import Term


@dataclass(frozen=True)
class Goal:
    """A goal to be solved.
    
    Immutable wrapper around a term that represents a goal
    in the execution stack.
    """
    term: Term


class GoalStack:
    """Stack of goals for Prolog execution.
    
    Implements LIFO semantics with support for snapshots
    and restoration for backtracking.
    """
    
    def __init__(self, snapshot: Optional[Sequence[Goal]] = None):
        """Initialize goal stack, optionally from a snapshot.
        
        Args:
            snapshot: Optional sequence of goals to initialize with
        """
        if snapshot is None:
            self._stack: list[Goal] = []
        else:
            # Make a copy to ensure independence
            self._stack = list(snapshot)
    
    def push(self, goal: Goal) -> None:
        """Push a goal onto the stack."""
        self._stack.append(goal)
    
    def pop(self) -> Optional[Goal]:
        """Pop and return the top goal, or None if empty."""
        return self._stack.pop() if self._stack else None
    
    def peek(self) -> Optional[Goal]:
        """Return the top goal without removing it, or None if empty."""
        return self._stack[-1] if self._stack else None
    
    def push_body(self, body: Tuple[Term, ...]) -> None:
        """Push body goals in reverse order for left-to-right execution.
        
        Args:
            body: Tuple of terms representing clause body goals
        """
        # Push in reverse so they pop in left-to-right order
        for term in reversed(body):
            self._stack.append(Goal(term))
    
    def snapshot(self) -> Tuple[Goal, ...]:
        """Create an immutable snapshot of current stack state.
        
        Returns:
            Immutable tuple of current goals
        """
        return tuple(self._stack)
    
    def restore(self, snapshot: Tuple[Goal, ...]) -> None:
        """Restore stack from a snapshot.
        
        Args:
            snapshot: Previously captured snapshot
        """
        self._stack = list(snapshot)
    
    def __len__(self) -> int:
        """Return the number of goals on the stack."""
        return len(self._stack)