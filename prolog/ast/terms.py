"""Term representation for Prolog AST.

Terms are the building blocks of Prolog programs:
- Atom: Named constants (e.g., 'foo', '[]')
- Int: Integer values
- Float: Floating-point values
- Var: Logic variables with unique IDs
- Struct: Compound terms with functor and arguments
- List: Prolog lists with items and optional tail
"""

from dataclasses import dataclass, field
from typing import Tuple, Optional, Union, Any


# Type alias for any term
Term = Union["Atom", "Int", "Float", "Var", "Struct", "List", "PrologDict"]


@dataclass(frozen=True)
class Atom:
    """An atom (named constant) in Prolog.

    Atoms are immutable and can be used as dict keys or in sets.
    """

    name: str


@dataclass(frozen=True)
class Int:
    """An integer value in Prolog.

    Ints are immutable and can be used as dict keys or in sets.
    """

    value: int


@dataclass(frozen=True)
class Float:
    """A floating-point value in Prolog.

    Floats are immutable and can be used as dict keys or in sets.
    """

    value: float


@dataclass
class Var:
    """A logic variable in Prolog.

    Variables are identified by their integer ID. The hint is optional
    and used only for pretty-printing. Variable equality is based on ID only.

    Note: Vars are mutable (not frozen) because we may need to update
    references during certain operations.
    """

    id: int
    hint: Optional[str] = None

    def __eq__(self, other):
        """Variables are equal if they have the same ID."""
        if not isinstance(other, Var):
            return False
        return self.id == other.id

    def __hash__(self):
        """Hash based on ID only."""
        return hash(self.id)


@dataclass(frozen=True)
class Struct:
    """A compound term (structure) in Prolog.

    Represents terms like f(a, b, c) where 'f' is the functor
    and (a, b, c) are the arguments.

    Structs are immutable and can be used as dict keys or in sets.
    """

    functor: str
    args: Tuple[Term, ...]


@dataclass(frozen=True)
class List:
    """A list in Prolog.

    Prolog lists are represented as a sequence of items with an optional tail.
    The tail defaults to the empty list atom '[]'.

    Examples:
    - [a, b, c] -> List(items=(Atom('a'), Atom('b'), Atom('c')), tail=Atom('[]'))
    - [a, b | X] -> List(items=(Atom('a'), Atom('b')), tail=Var(id=0, hint='X'))

    Lists are immutable and can be used as dict keys or in sets.
    """

    items: Tuple[Term, ...]
    tail: Term = field(default_factory=lambda: Atom("[]"))


@dataclass(frozen=True)
class PrologDict:
    """A dictionary in Prolog.

    Dicts store key-value pairs in sorted order for canonical representation.
    Keys must be atoms or small integers. Values can be any Prolog terms.

    Dicts are immutable and can be used as dict keys or in sets.
    """

    pairs: Tuple[Tuple[Term, Term], ...]  # Sorted by key
    tag: Optional[Term] = None  # Reserved for future tagged dict support

    def __post_init__(self):
        """Validate dict invariants."""
        # Validate key types first
        for key, _ in self.pairs:
            if not isinstance(key, (Atom, Int)):
                raise ValueError("Dict keys must be atoms or integers")

        # Ensure pairs are sorted by key for canonical representation
        if not self._is_sorted():
            sorted_pairs = tuple(
                sorted(self.pairs, key=lambda pair: self._key_sort_value(pair[0]))
            )
            object.__setattr__(self, "pairs", sorted_pairs)

        # Check for duplicate keys
        keys = [pair[0] for pair in self.pairs]
        if len(keys) != len(set(keys)):
            raise ValueError("Dict keys must be unique")

    def _is_sorted(self) -> bool:
        """Check if pairs are sorted by key."""
        keys = [self._key_sort_value(pair[0]) for pair in self.pairs]
        return keys == sorted(keys)

    def _key_sort_value(self, key: Term) -> Any:
        """Get sort value for a key term."""
        if isinstance(key, Atom):
            return (0, key.name)  # Atoms sort first
        elif isinstance(key, Int):
            return (1, key.value)  # Then integers
        else:
            raise ValueError("Dict keys must be atoms or integers")
