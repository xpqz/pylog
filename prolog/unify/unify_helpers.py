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
    
    Args:
        v1: First variable ID
        v2: Second variable ID
        trail: Trail for recording changes
        store: Variable store
        
    Returns:
        True (always succeeds for union of unbound vars)
        
    This function finds the roots of both variables and unions them.
    Union-by-rank optimization keeps trees shallow:
    - Smaller rank joins larger rank
    - Equal ranks: arbitrary choice, increment winner's rank
    """
    # Find roots
    _, root1 = store.deref(v1)
    _, root2 = store.deref(v2)
    
    # If already same root, nothing to do
    if root1 == root2:
        return True
    
    # Get ranks
    rank1 = store.cells[root1].rank
    rank2 = store.cells[root2].rank
    
    if rank1 < rank2:
        # root1 joins root2
        trail.append(("parent", root1, store.cells[root1].ref))
        store.cells[root1].ref = root2
    elif rank2 < rank1:
        # root2 joins root1
        trail.append(("parent", root2, store.cells[root2].ref))
        store.cells[root2].ref = root1
    else:
        # Equal ranks: root1 joins root2, increment root2's rank
        trail.append(("parent", root1, store.cells[root1].ref))
        store.cells[root1].ref = root2
        trail.append(("rank", root2, store.cells[root2].rank))
        store.cells[root2].rank += 1
    
    return True


def bind_root_to_term(vid: int, term: Any, trail: List, store: Store) -> None:
    """Bind an unbound root variable to a term.
    
    Args:
        vid: Variable ID (must be unbound root)
        term: Term to bind to (non-Var)
        trail: Trail for recording changes
        store: Variable store
        
    Raises:
        ValueError: If variable is already bound
        
    This function binds an unbound root to a term, trailing the old cell.
    """
    cell = store.cells[vid]
    
    if cell.tag == "bound":
        raise ValueError(f"Variable {vid} is already bound")
    
    # Create a snapshot of the old cell for the trail
    old_cell = Cell(tag=cell.tag, ref=cell.ref, term=cell.term, rank=cell.rank)
    
    # Trail the old cell
    trail.append(("bind", vid, old_cell))
    
    # Bind the variable
    store.cells[vid] = Cell(tag="bound", ref=vid, term=term, rank=cell.rank)


def deref_term(term: Any, store: Store) -> Tuple[str, Any]:
    """Dereference a term to find its value or unbound root.
    
    Args:
        term: Term to dereference
        store: Variable store
        
    Returns:
        ("NONVAR", term) if term is not a variable or is bound
        ("VAR", root_vid) if term is an unbound variable
        
    This function does not perform path compression (no side effects).
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