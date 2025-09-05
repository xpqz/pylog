"""Helper functions for unification.

These functions support the main unification algorithm:
- union_vars: Union-by-rank for merging two unbound variables
- bind_root_to_term: Bind an unbound root variable to a term
- deref_term: Follow variable chains to find the term or unbound root
"""

from copy import deepcopy
from typing import Any, List, Tuple

from prolog.ast.terms import Var
from prolog.unify.store import Cell, Store


def union_vars(v1: int, v2: int, trail: List, store: Store) -> bool:
    """Union two variables using union-by-rank.

    Contract:
    - Dereferences both v1 and v2 to find their roots
    - Raises ValueError if either root is bound (union is only for unbound roots)
    - Performs union-by-rank with trail entries:
      - ("parent", child_root, old_parent): parent link change
      - ("rank", new_root, old_rank): rank increment (only if ranks were equal)
    - Returns True on success

    Args:
        v1: First variable ID
        v2: Second variable ID
        trail: Trail for recording changes
        store: Variable store

    Returns:
        True (always succeeds for valid unbound vars)

    Raises:
        ValueError: If either root is bound
    """
    # Find roots
    result1 = store.deref(v1)
    result2 = store.deref(v2)

    # Check if either is bound
    if result1[0] == "BOUND":
        raise ValueError(f"Cannot union bound variable {v1}")
    if result2[0] == "BOUND":
        raise ValueError(f"Cannot union bound variable {v2}")

    root1 = result1[1]
    root2 = result2[1]

    # If already same root, nothing to do
    if root1 == root2:
        return True

    # Get ranks
    rank1 = store.cells[root1].rank
    rank2 = store.cells[root2].rank

    if rank1 < rank2:
        # root1 joins root2
        if hasattr(trail, "push"):
            trail.push(("parent", root1, store.cells[root1].ref))
        else:
            trail.append(("parent", root1, store.cells[root1].ref))
        store.cells[root1].ref = root2
    elif rank2 < rank1:
        # root2 joins root1
        if hasattr(trail, "push"):
            trail.push(("parent", root2, store.cells[root2].ref))
        else:
            trail.append(("parent", root2, store.cells[root2].ref))
        store.cells[root2].ref = root1
    else:
        # Equal ranks: root1 joins root2, increment root2's rank
        if hasattr(trail, "push"):
            trail.push(("parent", root1, store.cells[root1].ref))
        else:
            trail.append(("parent", root1, store.cells[root1].ref))
        store.cells[root1].ref = root2
        if hasattr(trail, "push"):
            trail.push(("rank", root2, store.cells[root2].rank))
        else:
            trail.append(("rank", root2, store.cells[root2].rank))
        store.cells[root2].rank += 1

    return True


def bind_root_to_term(vid: int, term: Any, trail: List, store: Store) -> None:
    """Bind an unbound root variable to a non-var term.

    Contract:
    - Requires vid is an unbound root
    - Requires term is not a Var (must be non-var term)
    - Pushes exactly one ("bind", vid, old_cell_copy) entry to trail
    - Sets cell to Cell(tag="bound", ref=vid, term=term)

    Args:
        vid: Variable ID (must be unbound root)
        term: Term to bind to (must not be Var)
        trail: Trail for recording changes
        store: Variable store

    Raises:
        ValueError: If variable is already bound or term is a Var
    """
    cell = store.cells[vid]

    if cell.tag == "bound":
        raise ValueError(f"Variable {vid} is already bound")

    if isinstance(term, Var):
        raise ValueError(
            f"Cannot bind to Var term - use union_vars for var-var binding"
        )

    # Create a snapshot of the old cell for the trail
    old_cell = Cell(tag=cell.tag, ref=cell.ref, term=cell.term, rank=cell.rank)

    # Trail the old cell (support both list and Trail object)
    if hasattr(trail, "push"):
        trail.push(("bind", vid, old_cell))
    else:
        trail.append(("bind", vid, old_cell))

    # Bind the variable
    store.cells[vid] = Cell(tag="bound", ref=vid, term=term, rank=cell.rank)


def deref_term(term: Any, store: Store) -> Tuple[str, Any]:
    """Dereference a term to find its value or unbound root.

    Contract:
    - If term is not a Var, returns ("NONVAR", term) unchanged
    - If term is a Var:
      - Returns ("VAR", root_vid) if root is unbound
      - Returns ("NONVAR", bound_term) if root is bound
    - No path compression or side effects
    - May raise IndexError/ValueError for invalid variable IDs

    Args:
        term: Term to dereference
        store: Variable store

    Returns:
        ("NONVAR", term) if term is not a variable or is bound
        ("VAR", root_vid) if term is an unbound variable

    Raises:
        IndexError/ValueError: If Var has invalid ID
    """
    if not isinstance(term, Var):
        # Non-variable terms return as-is
        return ("NONVAR", term)

    # Dereference the variable (without compression)
    result = store.deref(term.id, compress=False, trail=None)

    if result[0] == "BOUND":
        # Bound variable: return the bound term
        return ("NONVAR", result[2])
    else:
        # Unbound variable: return the root ID
        return ("VAR", result[1])
