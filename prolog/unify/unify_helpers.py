"""Helper functions for unification.

These functions support the main unification algorithm:
- union_vars: Union-by-rank for merging two unbound variables
- bind_root_to_term: Bind an unbound root variable to a term
- deref_term: Follow variable chains to find the term or unbound root
"""

from copy import deepcopy
from typing import Any, List, Tuple

from prolog.ast.terms import Var, Int
from prolog.unify.store import Cell, Store


def union_vars(v1: int, v2: int, trail: List, store: Store) -> bool:
    """Union two variables using union-by-rank with attribute merging.

    Contract:
    - Dereferences both v1 and v2 to find their roots
    - Raises ValueError if either root is bound (union is only for unbound roots)
    - For attributed variables, calls hooks for overlapping modules
    - Merges attributes to the union-find root with proper trailing
    - Performs union-by-rank with trail entries:
      - ("parent", child_root, old_parent): parent link change
      - ("rank", new_root, old_rank): rank increment (only if ranks were equal)
      - ("attr", varid, module, old_value): attribute changes
    - Returns True on success, False if hooks reject the unification

    Args:
        v1: First variable ID
        v2: Second variable ID
        trail: Trail for recording changes
        store: Variable store

    Returns:
        True if unification succeeds, False if hooks reject it

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

    # Handle attribute merging for var-var aliasing
    if hasattr(store, 'attrs') and hasattr(trail, 'engine') and trail.engine is not None:
        if not _handle_var_var_aliasing(root1, root2, trail, store):
            return False

    # Get ranks
    rank1 = store.cells[root1].rank
    rank2 = store.cells[root2].rank

    # Determine winner and loser for attribute merging
    winner_root = None
    loser_root = None

    if rank1 < rank2:
        # root1 joins root2
        winner_root = root2
        loser_root = root1
        if hasattr(trail, "push"):
            trail.push(("parent", root1, store.cells[root1].ref))
        else:
            trail.append(("parent", root1, store.cells[root1].ref))
        store.cells[root1].ref = root2
    elif rank2 < rank1:
        # root2 joins root1
        winner_root = root1
        loser_root = root2
        if hasattr(trail, "push"):
            trail.push(("parent", root2, store.cells[root2].ref))
        else:
            trail.append(("parent", root2, store.cells[root2].ref))
        store.cells[root2].ref = root1
    else:
        # Equal ranks: root2 joins root1 (first argument becomes root), increment root1's rank
        # This ensures deterministic behavior where the first argument of unify(X, Y)
        # becomes the root when ranks are equal.
        winner_root = root1
        loser_root = root2
        if hasattr(trail, "push"):
            trail.push(("parent", root2, store.cells[root2].ref))
        else:
            trail.append(("parent", root2, store.cells[root2].ref))
        store.cells[root2].ref = root1
        if hasattr(trail, "push"):
            trail.push(("rank", root1, store.cells[root1].rank))
        else:
            trail.append(("rank", root1, store.cells[root1].rank))
        store.cells[root1].rank += 1

    # Merge attributes after union
    if hasattr(store, 'attrs') and winner_root is not None and loser_root is not None:
        _merge_attributes_to_root(winner_root, loser_root, trail, store)

        # If the merged CLP(FD) domain is a singleton, bind the root to that value.
        # This ensures queries like `X in 5..5, Y in 5..5, X = Y` ground to 5.
        try:
            attrs = store.attrs.get(winner_root, {})
            fd = attrs.get('clpfd') if isinstance(attrs, dict) else None
            dom = fd.get('domain') if isinstance(fd, dict) else None
            if dom is not None and hasattr(dom, 'is_singleton') and dom.is_singleton():
                val = dom.min()
                if val is not None:
                    # Trail and bind the union root to the integer value
                    bind_root_to_term(winner_root, Int(val), trail, store)
        except Exception:
            # Be conservative: if anything goes wrong, skip auto-binding
            pass

    return True


def _handle_var_var_aliasing(root1: int, root2: int, trail: List, store: Store) -> bool:
    """Handle attribute merging when unifying two attributed variables.

    Args:
        root1: First variable root ID
        root2: Second variable root ID
        trail: Trail for recording changes
        store: Variable store

    Returns:
        True if hooks accept unification, False if any hook rejects
    """
    # Get attributes for both roots
    attrs1 = store.attrs.get(root1, {})
    attrs2 = store.attrs.get(root2, {})

    # If neither has attributes, nothing to do
    if not attrs1 and not attrs2:
        return True

    # Find overlapping modules
    overlapping = set(attrs1.keys()) & set(attrs2.keys())

    # For overlapping modules, call hooks in sorted order
    if overlapping:
        for module in sorted(overlapping):
            if module in trail.engine._attr_hooks:
                hook = trail.engine._attr_hooks[module]
                # IMPORTANT: Hooks must be called from BOTH perspectives for var-var unification.
                # This allows each variable's hook to inspect and potentially veto the merge.
                # The hook sees its own variable ID and the other as a Var term.

                # First call: hook(engine, root1, Var(root2))
                if not hook(trail.engine, root1, Var(root2, "")):
                    return False

                # Second call: hook(engine, root2, Var(root1))
                if not hook(trail.engine, root2, Var(root1, "")):
                    return False

    return True


def _merge_attributes_to_root(winner_root: int, loser_root: int, trail: List, store: Store) -> None:
    """Merge attributes from loser to winner after union.

    Args:
        winner_root: Root that becomes the final root
        loser_root: Root that gets merged into winner
        trail: Trail for recording changes
        store: Variable store
    """
    # Get attributes
    winner_attrs = store.attrs.get(winner_root, {})
    loser_attrs = store.attrs.get(loser_root, {})

    if not loser_attrs:
        return

    # Ensure winner has attrs dict
    if winner_root not in store.attrs:
        store.attrs[winner_root] = {}

    # For each attribute in loser, merge to winner
    for module, value in loser_attrs.items():
        if module in winner_attrs:
            # For overlapping modules, keep winner's existing value.
            # This is the simplest and most predictable approach:
            # the root (winner) retains its original attribute values.
            # The loser's value is discarded (but properly trailed below).
            pass
        else:
            # Non-overlapping: trail that this attribute didn't exist on winner (None)
            _trail_attr_change(trail, winner_root, module, None)
            # Set the value from loser
            store.attrs[winner_root][module] = value

    # Clear loser's attributes and trail the removals
    for module, old_value in loser_attrs.items():
        _trail_attr_change(trail, loser_root, module, old_value)

    # Clear the loser's attributes
    if loser_root in store.attrs:
        del store.attrs[loser_root]


def _trail_attr_change(trail: List, varid: int, module: str, old_value: Any) -> None:
    """Trail an attribute change.

    Args:
        trail: Trail for recording changes
        varid: Variable ID
        module: Attribute module name
        old_value: Previous value (None if didn't exist)
    """
    if hasattr(trail, 'push_attr'):
        trail.push_attr(varid, module, old_value)
    elif hasattr(trail, 'push'):
        trail.push(("attr", varid, module, old_value))
    else:
        trail.append(("attr", varid, module, old_value))


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
