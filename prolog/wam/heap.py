"""Heap management and cell tagging for WAM.

The heap stores WAM cells with tagged tuples representing different data types.
All heap addresses are Python list indices.

Cell Encoding:
- REF: (0, addr) - Reference cell pointing to heap address
- STR: (1, functor_addr) - Structure cell pointing to functor
- CON: (2, value) - Constant cell (atom str, int, float)
- LIST: (3, head_addr, tail_addr) - List cell with head and tail addresses

Tag constants allow efficient type checking via tuple[0].
"""

# Cell type tags
TAG_REF = 0
TAG_STR = 1
TAG_CON = 2
TAG_LIST = 3

# Type alias for heap cells
Cell = tuple[int, ...]

__all__ = [
    "TAG_REF",
    "TAG_STR",
    "TAG_CON",
    "TAG_LIST",
    "Cell",
    "make_ref",
    "make_str",
    "make_con",
    "make_list",
    "make_functor",
    "is_ref",
    "is_str",
    "is_con",
    "is_list",
]


def make_ref(addr: int) -> tuple[int, int]:
    """Create a reference cell pointing to given heap address."""
    return (TAG_REF, addr)


def make_str(functor_addr: int) -> tuple[int, int]:
    """Create a structure cell pointing to functor at given address."""
    return (TAG_STR, functor_addr)


def make_con(value) -> tuple[int, ...]:
    """Create a constant cell holding atom (str) or number (int/float)."""
    return (TAG_CON, value)


def make_list(head_addr: int, tail_addr: int) -> tuple[int, int, int]:
    """Create a list cell with head and tail addresses."""
    return (TAG_LIST, head_addr, tail_addr)


def make_functor(name: str, arity: int) -> tuple[int, tuple[str, int]]:
    """Create a functor cell (stored as CON with (name, arity) pair)."""
    return (TAG_CON, (name, arity))


def is_ref(cell: tuple) -> bool:
    """Check if cell is a reference."""
    return cell[0] == TAG_REF


def is_str(cell: tuple) -> bool:
    """Check if cell is a structure."""
    return cell[0] == TAG_STR


def is_con(cell: tuple) -> bool:
    """Check if cell is a constant."""
    return cell[0] == TAG_CON


def is_list(cell: tuple) -> bool:
    """Check if cell is a list."""
    return cell[0] == TAG_LIST
