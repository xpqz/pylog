"""Arithmetic evaluation utilities extracted from engine.py."""

from prolog.ast.terms import Term, Int, Var


def eval_int(store, t: Term) -> int:
    """Evaluate a term as an integer.

    Args:
        store: The unification store
        t: The term to evaluate.

    Returns:
        The integer value.

    Raises:
        ValueError: If the term is not an integer.
    """
    if isinstance(t, Int):
        return t.value
    if isinstance(t, Var):
        r = store.deref(t.id)
        if r[0] == "BOUND":
            _, _, b = r
            return eval_int(store, b)
    raise ValueError("non-integer")
