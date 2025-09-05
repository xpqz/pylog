"""Occurs check implementation for sound unification.

The occurs check prevents creating infinite/cyclic structures by detecting
when a variable would occur in its own binding. This is crucial for
maintaining logical consistency in Prolog.

The implementation is iterative (using an explicit stack) to avoid
Python's recursion limit on deeply nested structures.
"""

from typing import Any, Set

from prolog.ast.terms import Var, Struct, List as PrologList
from prolog.unify.store import Store


def occurs(vid: int, term: Any, store: Store) -> bool:
    """Check if variable vid occurs in term.

    Args:
        vid: Variable ID to check for
        term: Term to search in
        store: Variable store for dereferencing

    Returns:
        True if vid occurs in term, False otherwise

    The check follows variable chains and examines all positions
    in compound structures. It uses a seen set to handle cyclic
    terms safely and terminates even on rational trees.

    Important: This function has no side effects - it never modifies
    the store (no path compression or other mutations).
    """
    # Validate the variable ID
    if vid < 0 or vid >= len(store.cells):
        raise ValueError(f"Invalid variable ID: {vid}")

    # Stack of terms to check
    stack = [term]

    # Track seen variables to handle cycles
    seen: Set[int] = set()

    while stack:
        current = stack.pop()

        # Handle variables
        if isinstance(current, Var):
            current_vid = current.id

            # Check if this is the variable we're looking for
            if current_vid == vid:
                return True

            # Skip if we've already processed this variable (cycle detection)
            if current_vid in seen:
                continue
            seen.add(current_vid)

            # Follow the variable chain
            try:
                cell = store.cells[current_vid]
            except IndexError:
                # Invalid variable ID
                raise

            # If variable is bound, check its binding
            if cell.tag == "bound":
                stack.append(cell.term)
            else:
                # Unbound variable - follow parent chain to root
                root = current_vid
                while cell.ref != root:
                    root = cell.ref
                    try:
                        cell = store.cells[root]
                    except IndexError:
                        raise

                # Check if the root is our target
                if root == vid:
                    return True

                # If root is bound, check its binding
                if cell.tag == "bound":
                    stack.append(cell.term)

        # Handle structures
        elif isinstance(current, Struct):
            # Add all arguments to the stack
            stack.extend(current.args)

        # Handle lists
        elif isinstance(current, PrologList):
            # Add all items to the stack
            stack.extend(current.items)
            # Add tail if it exists and isn't the empty list
            if (
                current.tail is not None
                and current.tail != PrologList(())
                and not (hasattr(current.tail, "name") and current.tail.name == "[]")
            ):
                stack.append(current.tail)

        # Atoms, Ints, and other atomic terms don't contain variables
        # so we don't need to check them further

    return False
