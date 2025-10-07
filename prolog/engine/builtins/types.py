"""Type-checking builtin predicates.

This module contains all the type-checking predicates like var/1, nonvar/1, atom/1, etc.
These predicates test the type of Prolog terms.
"""

from typing import Dict, Tuple, Callable
from prolog.ast.terms import Term, Atom, Int, Float, Var, Struct, List as PrologList


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register type-checking builtin predicates.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    registry[("var", 1)] = lambda eng, args: builtin_var(eng, args)
    registry[("nonvar", 1)] = lambda eng, args: builtin_nonvar(eng, args)
    registry[("atom", 1)] = lambda eng, args: builtin_atom(eng, args)
    registry[("integer", 1)] = lambda eng, args: builtin_integer(eng, args)
    registry[("float", 1)] = lambda eng, args: builtin_float(eng, args)
    registry[("number", 1)] = lambda eng, args: builtin_number(eng, args)
    registry[("atomic", 1)] = lambda eng, args: builtin_atomic(eng, args)
    registry[("compound", 1)] = lambda eng, args: builtin_compound(eng, args)
    registry[("callable", 1)] = lambda eng, args: builtin_callable(eng, args)
    registry[("ground", 1)] = lambda eng, args: builtin_ground(eng, args)


def builtin_var(engine, args: tuple) -> bool:
    """var(X) - true if X is an unbound variable."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        return result[0] == "UNBOUND"
    return False


def builtin_nonvar(engine, args: tuple) -> bool:
    """nonvar(X) - true if X is not an unbound variable."""
    if len(args) != 1:
        return False
    return not builtin_var(engine, args)


def builtin_atom(engine, args: tuple) -> bool:
    """atom(X) - true if X is an atom."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, Atom)
        return False
    return isinstance(term, Atom)


def builtin_integer(engine, args: tuple) -> bool:
    """integer(X) - true if X is an integer."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, Int)
        return False
    return isinstance(term, Int)


def builtin_float(engine, args: tuple) -> bool:
    """float(X) - true if X is a float."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, Float)
        return False
    return isinstance(term, Float)


def builtin_number(engine, args: tuple) -> bool:
    """number(X) - true if X is a number (integer or float)."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, (Int, Float))
        return False
    return isinstance(term, (Int, Float))


def builtin_atomic(engine, args: tuple) -> bool:
    """atomic(X) - true if X is atomic (atom, number, or other atomic term)."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, (Atom, Int, Float))
        return False
    return isinstance(term, (Atom, Int, Float))


def builtin_compound(engine, args: tuple) -> bool:
    """compound(X) - true if X is a compound term."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, (Struct, PrologList))
        return False
    return isinstance(term, (Struct, PrologList))


def builtin_callable(engine, args: tuple) -> bool:
    """callable(X) - true if X is callable (atom or compound)."""
    if len(args) != 1:
        return False
    term = args[0]
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return isinstance(bound_term, (Atom, Struct, PrologList))
        return False
    return isinstance(term, (Atom, Struct, PrologList))


def builtin_ground(engine, args: tuple) -> bool:
    """ground(X) - true if X contains no unbound variables."""
    if len(args) != 1:
        return False
    term = args[0]
    return is_ground(engine, term)


def is_ground(engine, term: Term) -> bool:
    """Helper function to check if a term is ground (contains no unbound variables)."""
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            _, _, bound_term = result
            return is_ground(engine, bound_term)
        return False
    elif isinstance(term, (Atom, Int, Float)):
        return True
    elif isinstance(term, Struct):
        return all(is_ground(engine, arg) for arg in term.args)
    elif isinstance(term, PrologList):
        return all(is_ground(engine, item) for item in term.items) and is_ground(
            engine, term.tail
        )
    return True
