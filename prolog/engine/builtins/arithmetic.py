"""Arithmetic builtins extracted from engine.py.

This module implements arithmetic predicates including:
- is/2: Arithmetic evaluation
- =:=/2: Arithmetic equality comparison
- =\\=/2: Arithmetic inequality comparison
- >/2: Arithmetic greater-than comparison
- </2: Arithmetic less-than comparison
- >=/2: Arithmetic greater-or-equal comparison
- =</2: Arithmetic less-or-equal comparison

All predicates use the eval_arithmetic function from utils.arithmetic for evaluation.
"""

from typing import Dict, Tuple, Callable
from prolog.ast.terms import Int, Float
from prolog.engine.utils.arithmetic import eval_arithmetic
from prolog.unify.unify import unify
from prolog.engine.trail_adapter import TrailAdapter


def builtin_is(engine, args: tuple) -> bool:
    """is(X, Y) - arithmetic evaluation."""
    if len(args) != 2:
        return False
    left, right = args
    # Evaluate right side
    try:
        value = eval_arithmetic(engine.store, right)
        # Unify with left side - use Float for float results, Int for integer results
        if isinstance(value, float):
            result_term = Float(value)
        else:
            result_term = Int(value)
        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)
        return unify(
            left,
            result_term,
            engine.store,
            trail_adapter,  # type: ignore
            occurs_check=engine.occurs_check,
        )
    except (ValueError, TypeError):
        return False


def builtin_num_eq(engine, args: tuple) -> bool:
    """=:=(X, Y) - arithmetic equality comparison."""
    if len(args) != 2:
        return False
    try:
        return eval_arithmetic(engine.store, args[0]) == eval_arithmetic(
            engine.store, args[1]
        )
    except (ValueError, TypeError):
        return False  # ISO: type/instantiation errors


def builtin_num_ne(engine, args: tuple) -> bool:
    """=\\=(X, Y) - arithmetic inequality comparison."""
    if len(args) != 2:
        return False
    try:
        return eval_arithmetic(engine.store, args[0]) != eval_arithmetic(
            engine.store, args[1]
        )
    except (ValueError, TypeError):
        return False  # ISO: type/instantiation errors


def builtin_gt(engine, args: tuple) -> bool:
    """>(X, Y) - arithmetic greater-than comparison."""
    if len(args) != 2:
        return False
    try:
        return eval_arithmetic(engine.store, args[0]) > eval_arithmetic(
            engine.store, args[1]
        )
    except (ValueError, TypeError):
        return False  # ISO: type/instantiation errors


def builtin_lt(engine, args: tuple) -> bool:
    """<(X, Y) - arithmetic less-than comparison."""
    if len(args) != 2:
        return False
    try:
        return eval_arithmetic(engine.store, args[0]) < eval_arithmetic(
            engine.store, args[1]
        )
    except (ValueError, TypeError):
        return False  # ISO: type/instantiation errors


def builtin_ge(engine, args: tuple) -> bool:
    """>=(X, Y) - arithmetic greater-or-equal comparison."""
    if len(args) != 2:
        return False
    try:
        return eval_arithmetic(engine.store, args[0]) >= eval_arithmetic(
            engine.store, args[1]
        )
    except (ValueError, TypeError):
        return False  # ISO: type/instantiation errors


def builtin_le(engine, args: tuple) -> bool:
    """=<(X, Y) - arithmetic less-or-equal comparison."""
    if len(args) != 2:
        return False
    try:
        return eval_arithmetic(engine.store, args[0]) <= eval_arithmetic(
            engine.store, args[1]
        )
    except (ValueError, TypeError):
        return False  # ISO: type/instantiation errors


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register arithmetic predicates with the given registry.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    registry[("is", 2)] = builtin_is
    registry[("=:=", 2)] = builtin_num_eq
    registry[("=\\=", 2)] = builtin_num_ne
    registry[(">", 2)] = builtin_gt
    registry[("<", 2)] = builtin_lt
    registry[(">=", 2)] = builtin_ge
    registry[("=<", 2)] = builtin_le
