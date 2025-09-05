"""Choicepoint system for backtracking (Stage 0)."""

from dataclasses import dataclass
from typing import Optional, Tuple
from prolog.ast.clauses import ClauseCursor
from prolog.engine.goals import Goal


@dataclass(frozen=True)
class Choicepoint:
    """A saved state for backtracking.

    Captures all state needed to restore and try the next alternative
    when the current path fails. The choicepoint is immutable (frozen).

    Key semantics:
    - goals: Pre-goal snapshot (goal being tried is still on top)
    - cursor: Positioned at the next clause to try
    - id: Choicepoint's own ID (not mutated by stack operations)
    """

    id: int  # Choicepoint's own ID
    goals: Tuple[Goal, ...]  # Snapshot of goal stack (immutable)
    cursor: ClauseCursor  # Next clause to try
    trail_mark: int  # Position in trail for undo
    cut_barrier: Optional[int]  # Cut scope (parent choicepoint ID)
    store_size: int  # Number of variables at creation


class ChoiceStack:
    """Stack of choicepoints for backtracking.

    Manages choicepoints with sequential ID assignment. The IDs returned
    by push() are used for cut operations, not the choicepoint's internal
    id field (which is preserved unchanged).
    """

    def __init__(self):
        """Initialize empty choice stack."""
        self._stack: list[Choicepoint] = []
        self._next_id: int = 1  # Next ID to assign

    def push(self, cp: Choicepoint) -> int:
        """Push a choicepoint and return its assigned ID.

        Args:
            cp: Choicepoint to push (not mutated)

        Returns:
            Sequential ID assigned by the stack
        """
        self._stack.append(cp)
        assigned_id = self._next_id
        self._next_id += 1
        return assigned_id

    def pop(self) -> Optional[Choicepoint]:
        """Pop and return the most recent choicepoint, or None if empty."""
        return self._stack.pop() if self._stack else None

    def cut_to(self, barrier: Optional[int]) -> None:
        """Remove all choicepoints newer than the barrier.

        Args:
            barrier: Stack-assigned ID to preserve (remove all with ID > barrier)
                    None means no-op (no cut)
        """
        if barrier is None:
            return

        # Remove choicepoints with stack-assigned IDs > barrier
        # Stack-assigned IDs are sequential: 1, 2, 3, ...
        # The nth choicepoint has stack-assigned ID n
        # So we keep the first 'barrier' choicepoints
        while len(self._stack) > barrier:
            self._stack.pop()

    def find(self, cp_id: int) -> Optional[Choicepoint]:
        """Find a choicepoint by its internal ID.

        Args:
            cp_id: The choicepoint's internal id field

        Returns:
            The choicepoint if found, None otherwise
        """
        for cp in reversed(self._stack):
            if cp.id == cp_id:
                return cp
        return None

    def top_id(self) -> Optional[int]:
        """Return the internal ID of the top choicepoint, or None if empty.

        Returns:
            The top choicepoint's internal id field, or None
        """
        return self._stack[-1].id if self._stack else None

    def current_id(self) -> int:
        """Return the next ID that will be assigned by push().

        Returns:
            The next sequential ID to be assigned
        """
        return self._next_id

    def size(self) -> int:
        """Return the number of choicepoints on the stack.

        Returns:
            The current stack size
        """
        return len(self._stack)
