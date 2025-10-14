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
    "new_ref",
    "new_con",
    "new_str",
    "new_list",
    "note_struct_args",
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


# Heap allocation helpers (mutate machine.heap and machine.H)


def new_ref(machine) -> int:
    """Allocate unbound reference on heap.

    Creates a self-referential REF cell (canonical unbound form).

    Args:
        machine: Machine instance with heap and H register

    Returns:
        Address of new REF cell
    """
    addr = machine.H
    cell = make_ref(addr)  # Self-referential
    machine.heap.append(cell)
    machine.H += 1
    return addr


def new_con(machine, value) -> int:
    """Allocate constant cell on heap.

    Args:
        machine: Machine instance with heap and H register
        value: Atom (str), int, float, or (name, arity) tuple for functors

    Returns:
        Address of new CON cell
    """
    addr = machine.H
    cell = make_con(value)
    machine.heap.append(cell)
    machine.H += 1
    return addr


def new_str(machine, name: str, arity: int) -> int:
    """Allocate structure cell on heap.

    Creates STR cell followed by functor cell. Caller must write
    argument cells at functor_addr+1 through functor_addr+arity.

    Note: Invariants apply after the function returns. There is a transient
    moment between the STR and functor appends where the STR points to
    an address equal to H.

    Args:
        machine: Machine instance with heap and H register
        name: Functor name
        arity: Functor arity

    Returns:
        Address of STR cell (functor is at returned_addr+1)
    """
    str_addr = machine.H
    functor_addr = machine.H + 1

    # Append STR cell pointing to functor
    machine.heap.append(make_str(functor_addr))
    machine.H += 1

    # Append functor cell
    machine.heap.append(make_functor(name, arity))
    machine.H += 1

    return str_addr


def new_list(machine, head_addr: int, tail_addr: int) -> int:
    """Allocate list cell on heap.

    Args:
        machine: Machine instance with heap and H register
        head_addr: Address of head element
        tail_addr: Address of tail element

    Returns:
        Address of new LIST cell
    """
    addr = machine.H
    cell = make_list(head_addr, tail_addr)
    machine.heap.append(cell)
    machine.H += 1
    return addr


def note_struct_args(machine, *args: int) -> None:
    """Note structure arguments for readability (no-op).

    This is an intentional no-op helper for test readability and documentation.
    Structure arguments are allocated via new_ref/new_con/etc after new_str,
    which naturally places them at functor_addr+1, functor_addr+2, etc.

    The function name 'note' (not 'write') emphasizes that this is purely
    documentary - no mutation occurs. Use in tests to clarify intent when
    building structures on the heap.

    Args:
        machine: Machine instance (unused, present for API consistency)
        *args: Heap addresses that form structure arguments (unused)

    Example:
        str_addr = new_str(m, "foo", 2)
        arg1 = new_ref(m)
        arg2 = new_con(m, "bar")
        note_struct_args(m, arg1, arg2)  # Documents that these are foo's args
    """
    pass
