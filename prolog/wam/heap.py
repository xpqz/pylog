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
    """Place structure arguments on heap by adding REF cells pointing to them.

    For each provided argument address, appends a REF cell pointing to that
    address. This allows tests to build structures where arguments are allocated
    first, then referenced from the structure.

    Args:
        machine: Machine instance with heap and H register
        *args: Heap addresses to reference as structure arguments

    Example:
        # Build f(X) where X is allocated first
        x_addr = new_ref(m)              # heap[0] = REF(0)
        f_addr = new_str(m, "f", 1)      # heap[1] = STR(2), heap[2] = CON(("f",1))
        note_struct_args(m, x_addr)      # heap[3] = REF(0) - arg points to X

        # Build f(a, b) with arguments allocated in place
        f_addr = new_str(m, "f", 2)
        arg1 = new_con(m, "a")            # Allocated at correct position
        arg2 = new_con(m, "b")            # Allocated at correct position
        note_struct_args(m, arg1, arg2)  # No-op, args already in place
    """
    start_H = machine.H
    for i, arg_addr in enumerate(args):
        # If arg was allocated before the current H, add REF pointing to it
        # If arg equals current H, it was just allocated in sequence, already in place
        if arg_addr < start_H:
            # Arg allocated earlier, add REF cell pointing to it
            machine.heap.append(make_ref(arg_addr))
            machine.H += 1
        elif arg_addr == machine.H:
            # Arg just allocated at current position, already in place
            # H was already advanced by the allocation function
            pass
        else:
            # Arg address is beyond current H - shouldn't happen in valid usage
            # Add REF anyway for robustness
            machine.heap.append(make_ref(arg_addr))
            machine.H += 1
