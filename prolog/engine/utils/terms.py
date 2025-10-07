"""Term utility functions extracted from engine.py.

This module contains pure utility functions for term manipulation that don't
depend on engine state beyond the store and variable name mappings.
"""

from typing import Any, Dict, Optional, List
from prolog.ast.terms import Term, Atom, Int, Float, Var, Struct, List as PrologList


def reify_var(store, qname_by_id: Dict[int, str], varid: int) -> Any:
    """Follow bindings to get the value of a variable.

    Args:
        store: The unification store
        qname_by_id: Query variable name mapping
        varid: The variable ID to reify.

    Returns:
        The ground term or a variable representation.
    """
    # Dereference to find what it's bound to
    result = store.deref(varid)

    if result[0] == "UNBOUND":
        # Unbound variable - return a representation
        _, ref = result
        # Fast lookup for query var name, default to _<id>
        hint = qname_by_id.get(ref, f"_{ref}")
        return Var(ref, hint)

    elif result[0] == "BOUND":
        # Bound to a term - reify iteratively
        _, ref, term = result
        return reify_term(store, qname_by_id, term)

    else:
        raise ValueError(f"Unknown cell tag: {result[0]}")


def reify_term(store, qname_by_id: Dict[int, str], term: Term) -> Any:
    """Reify a term, following variable bindings (iterative).

    Reified lists are always flattened: nested PrologList structures
    where the tail is also a PrologList will be combined into a single
    PrologList. Improper lists (with non-list tails) retain their tail.

    Args:
        store: The unification store
        qname_by_id: Query variable name mapping
        term: The term to reify.

    Returns:
        The ground term with variables reified.
    """
    # Use iterative approach to avoid stack overflow
    stack = [term]
    visited = set()
    all_terms = []

    # First pass: collect all terms in dependency order
    while stack:
        current = stack.pop()
        term_id = id(current)

        if term_id in visited:
            continue
        visited.add(term_id)
        all_terms.append(current)

        if isinstance(current, Var):
            # Will need to check binding
            result = store.deref(current.id)
            if result[0] == "BOUND":
                _, _, bound_term = result
                stack.append(bound_term)
        elif isinstance(current, Struct):
            for arg in reversed(current.args):
                stack.append(arg)
        elif isinstance(current, PrologList):
            stack.append(current.tail)
            for item in reversed(current.items):
                stack.append(item)

    # Second pass: reify bottom-up
    reified = {}
    for current in reversed(all_terms):
        term_id = id(current)

        if isinstance(current, Var):
            result = store.deref(current.id)
            if result[0] == "UNBOUND":
                # Unbound variable - return a representation
                _, ref = result
                # Fast lookup for query var name
                hint = qname_by_id.get(ref, f"_{ref}")
                reified[term_id] = Var(ref, hint)
            elif result[0] == "BOUND":
                # Use reified version of bound term
                _, _, bound_term = result
                reified[term_id] = reified.get(id(bound_term), bound_term)
            else:
                raise ValueError(f"Unknown cell tag: {result[0]}")

        elif isinstance(current, (Atom, Int, Float)):
            reified[term_id] = current

        elif isinstance(current, Struct):
            reified_args = tuple(reified.get(id(arg), arg) for arg in current.args)
            reified[term_id] = Struct(current.functor, reified_args)

        elif isinstance(current, PrologList):
            reified_items = list(reified.get(id(item), item) for item in current.items)
            reified_tail = reified.get(id(current.tail), current.tail)

            # Flatten if tail is also a PrologList
            if isinstance(reified_tail, PrologList):
                # Combine items from current list with items from tail list
                all_items = reified_items + list(reified_tail.items)
                final_tail = reified_tail.tail
                reified[term_id] = PrologList(tuple(all_items), tail=final_tail)
            else:
                reified[term_id] = PrologList(tuple(reified_items), tail=reified_tail)
        else:
            # Unknown term type
            reified[term_id] = current

    return reified.get(id(term), term)


def prolog_list_to_python_list(lst: PrologList) -> Optional[List[Term]]:
    """Convert a proper Prolog list to a Python list.

    Returns None if the list is improper (tail is not [] or another list).

    Args:
        lst: The Prolog list to convert

    Returns:
        Python list of terms or None if improper
    """
    result = []
    current = lst

    while isinstance(current, PrologList):
        result.extend(current.items)
        current = current.tail

    # Check if we ended with empty list (proper list)
    if isinstance(current, Atom) and current.name == "[]":
        return result

    # Improper list
    return None
