"""Term copying utilities extracted from engine.py."""

from typing import Dict, List
from prolog.ast.terms import Term, Atom, Int, Float, Var, Struct, List as PrologList


def copy_term_with_fresh_vars(store, term: Term) -> Term:
    """Create a copy of a term with fresh variables."""
    var_mapping = {}
    return copy_term_recursive(term, var_mapping, store)


def copy_term_recursive(term: Term, var_mapping: Dict[int, int], target_store) -> Term:
    """Recursively copy a term, mapping old var IDs to new ones."""
    if isinstance(term, Var):
        if term.id not in var_mapping:
            var_mapping[term.id] = target_store.new_var(term.hint)
        return Var(var_mapping[term.id], term.hint)
    elif isinstance(term, (Atom, Int, Float)):
        return term
    elif isinstance(term, Struct):
        new_args = tuple(
            copy_term_recursive(arg, var_mapping, target_store) for arg in term.args
        )
        return Struct(term.functor, new_args)
    elif isinstance(term, PrologList):
        new_items = tuple(
            copy_term_recursive(item, var_mapping, target_store) for item in term.items
        )
        new_tail = copy_term_recursive(term.tail, var_mapping, target_store)
        return PrologList(new_items, new_tail)
    return term


def build_prolog_list(items: List[Term]) -> Term:
    """Build a Prolog list term from a list of items."""
    if not items:
        return Atom("[]")

    # Build list from right to left
    result = Atom("[]")
    for item in reversed(items):
        result = PrologList((item,), tail=result)

    return result
