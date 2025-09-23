"""Clause and Program structures for Prolog (Stage 0)."""

from dataclasses import dataclass
from typing import Tuple, List, Optional, Union
from prolog.ast.terms import Atom, Struct, Term


@dataclass(frozen=True)
class Clause:
    """A Prolog clause (fact or rule).

    A clause consists of a head and optional body:
    - Fact: head with empty body tuple
    - Rule: head with non-empty body tuple

    The head can be an Atom (0-arity) or Struct.
    The body is a tuple of goals (terms).
    """

    head: Union[Atom, Struct]
    body: Tuple[Term, ...]

    @classmethod
    def from_str(cls, text: str) -> "Clause":
        """Create a Clause from a string representation.

        Args:
            text: String representation of a clause (e.g., "foo(X) :- bar(X).")

        Returns:
            Parsed Clause object

        Raises:
            ParseError: If the text cannot be parsed as a clause
        """
        from prolog.parser.parser import parse_clause
        return parse_clause(text)


@dataclass(frozen=True)
class Program:
    """A collection of Prolog clauses.

    Stores clauses in order and provides lookup by functor/arity.
    """

    clauses: Tuple[Clause, ...]

    def __post_init__(self):
        """Build index for efficient clause lookup."""
        # Create index mapping (functor, arity) -> list of clause indices
        index = {}
        for i, clause in enumerate(self.clauses):
            head = clause.head
            if isinstance(head, Atom):
                key = (head.name, 0)
            elif isinstance(head, Struct):
                key = (head.functor, len(head.args))
            else:
                # Defensive - shouldn't happen with proper types
                continue

            if key not in index:
                index[key] = []
            index[key].append(i)

        # Store as frozen attribute (can't use assignment in frozen dataclass)
        object.__setattr__(self, "_index", index)

    def clauses_for(self, functor: str, arity: int) -> List[int]:
        """Return indices of clauses matching functor/arity.

        Args:
            functor: The predicate name
            arity: The number of arguments

        Returns:
            List of clause indices in source order
        """
        key = (functor, arity)
        return self._index.get(key, [])


@dataclass
class ClauseCursor:
    """Cursor for iterating through matching clauses.

    Tracks position in a list of clause indices and provides
    clean iteration API without index manipulation.
    """

    functor: str
    arity: int
    matches: List[int]
    pos: int = 0

    def has_more(self) -> bool:
        """Check if more clauses are available."""
        return self.pos < len(self.matches)

    def peek(self) -> Optional[int]:
        """Look at next clause index without advancing."""
        return self.matches[self.pos] if self.has_more() else None

    def take(self) -> Optional[int]:
        """Get next clause index and advance position."""
        if not self.has_more():
            return None
        idx = self.matches[self.pos]
        self.pos += 1
        return idx

    def clone(self) -> "ClauseCursor":
        """Create a copy of cursor at current position.

        Note: Shares the matches list (shallow copy) since it's not mutated.
        Only the position is independent.
        """
        return ClauseCursor(
            functor=self.functor,
            arity=self.arity,
            matches=self.matches,  # Share the list
            pos=self.pos,
        )
