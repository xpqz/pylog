"""WAM type checking builtins.

Implements ISO Prolog type checking predicates:
- var/1: Check if argument is unbound variable
- nonvar/1: Check if argument is bound (non-variable)
- atom/1: Check if argument is an atom
- integer/1: Check if argument is an integer
- number/1: Check if argument is a number (integer or float)

All type check builtins are deterministic (no choicepoints) and have no side effects.
They succeed or fail based on the type of the dereferenced argument.
"""

from __future__ import annotations

from prolog.wam.heap import TAG_CON, TAG_REF
from prolog.wam.unify import deref

__all__ = [
    "builtin_var",
    "builtin_nonvar",
    "builtin_atom",
    "builtin_integer",
    "builtin_number",
    "register_type_builtins",
]


def builtin_var(machine) -> bool:
    """var(X): Succeeds if X is an unbound variable.

    Args:
        machine: Machine with X[0] containing argument address

    Returns:
        True if X[0] is unbound variable, False otherwise

    Example:
        ?- var(X).          % succeeds
        ?- var(5).          % fails
        ?- X = Y, var(X).   % fails (X bound to Y)
    """
    addr = machine.X[0]
    dereffed = deref(machine, addr)
    cell = machine.heap[dereffed]

    # Unbound if REF pointing to itself
    return cell[0] == TAG_REF and cell[1] == dereffed


def builtin_nonvar(machine) -> bool:
    """nonvar(X): Succeeds if X is not an unbound variable.

    Args:
        machine: Machine with X[0] containing argument address

    Returns:
        True if X[0] is bound (not unbound variable), False otherwise

    Example:
        ?- nonvar(5).       % succeeds
        ?- nonvar(X).       % fails
        ?- X = 5, nonvar(X). % succeeds
    """
    addr = machine.X[0]
    dereffed = deref(machine, addr)
    cell = machine.heap[dereffed]

    # Bound if not a self-referential REF
    return not (cell[0] == TAG_REF and cell[1] == dereffed)


def builtin_atom(machine) -> bool:
    """atom(X): Succeeds if X is an atom.

    Args:
        machine: Machine with X[0] containing argument address

    Returns:
        True if X[0] is an atom, False otherwise

    Example:
        ?- atom(foo).       % succeeds
        ?- atom(5).         % fails
        ?- atom(X).         % fails
        ?- atom([]).        % succeeds (empty list is atom)
    """
    addr = machine.X[0]
    dereffed = deref(machine, addr)
    cell = machine.heap[dereffed]

    # Atom if CON with string value
    if cell[0] != TAG_CON:
        return False

    value = cell[1]
    return isinstance(value, str)


def builtin_integer(machine) -> bool:
    """integer(X): Succeeds if X is an integer.

    Args:
        machine: Machine with X[0] containing argument address

    Returns:
        True if X[0] is an integer, False otherwise

    Example:
        ?- integer(5).      % succeeds
        ?- integer(5.0).    % fails
        ?- integer(foo).    % fails
        ?- integer(X).      % fails
    """
    addr = machine.X[0]
    dereffed = deref(machine, addr)
    cell = machine.heap[dereffed]

    # Integer if CON with int value (not bool, which is also int in Python)
    if cell[0] != TAG_CON:
        return False

    value = cell[1]
    return isinstance(value, int) and not isinstance(value, bool)


def builtin_number(machine) -> bool:
    """number(X): Succeeds if X is a number (integer or float).

    Args:
        machine: Machine with X[0] containing argument address

    Returns:
        True if X[0] is a number, False otherwise

    Example:
        ?- number(5).       % succeeds
        ?- number(5.0).     % succeeds
        ?- number(foo).     % fails
        ?- number(X).       % fails
    """
    addr = machine.X[0]
    dereffed = deref(machine, addr)
    cell = machine.heap[dereffed]

    # Number if CON with int or float value
    if cell[0] != TAG_CON:
        return False

    value = cell[1]
    return isinstance(value, (int, float)) and not isinstance(value, bool)


def register_type_builtins(registry: dict) -> None:
    """Register all type checking builtins in the given registry.

    Args:
        registry: Builtin registry dict to populate

    Registers:
        - system:var/1
        - system:nonvar/1
        - system:atom/1
        - system:integer/1
        - system:number/1
    """
    registry["system:var/1"] = builtin_var
    registry["system:nonvar/1"] = builtin_nonvar
    registry["system:atom/1"] = builtin_atom
    registry["system:integer/1"] = builtin_integer
    registry["system:number/1"] = builtin_number
