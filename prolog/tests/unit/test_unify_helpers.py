"""Unit tests for unification helper functions.

Helper functions:
- union_vars: Union-by-rank for joining two unbound roots
- bind_root_to_term: Bind an unbound root to a non-var term
- deref_term: Follow var chains to get term or unbound root
"""

import pytest
from copy import deepcopy

from prolog.unify.store import Cell, Store
from prolog.unify.unify_helpers import union_vars, bind_root_to_term, deref_term
from prolog.unify.trail import undo_to
from prolog.ast.terms import Atom, Int, Var, Struct, List


# Test union_vars()
def test_union_vars_with_equal_roots_returns_true():
    """Test union_vars with equal roots returns True."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    # Union with self should return True (no-op)
    result = union_vars(v, v, trail, store)
    assert result is True
    assert len(trail) == 0  # No changes


def test_union_vars_by_rank_smaller_joins_larger():
    """Test union by rank: smaller rank joins larger."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    # Give v2 higher rank
    store.cells[v2].rank = 3
    store.cells[v1].rank = 1
    
    result = union_vars(v1, v2, trail, store)
    assert result is True
    
    # v1 should now point to v2 (smaller joined larger)
    assert store.cells[v1].ref == v2
    assert store.cells[v2].ref == v2  # v2 remains root
    
    # Trail should have parent change for v1
    assert len(trail) == 1
    assert trail[0] == ("parent", v1, v1)


def test_union_vars_equal_ranks_increment_winner():
    """Test equal ranks increment winner's rank."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    # Both have rank 0
    assert store.cells[v1].rank == 0
    assert store.cells[v2].rank == 0
    
    result = union_vars(v1, v2, trail, store)
    assert result is True
    
    # One should point to the other
    if store.cells[v1].ref == v2:
        # v1 joined v2, v2's rank should increment
        assert store.cells[v2].rank == 1
        assert store.cells[v1].rank == 0  # Unchanged
    else:
        # v2 joined v1, v1's rank should increment
        assert store.cells[v2].ref == v1
        assert store.cells[v1].rank == 1
        assert store.cells[v2].rank == 0  # Unchanged


def test_union_vars_trail_entries():
    """Test trail entries created for parent and rank changes."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    # Both rank 0, so one will get rank increment
    mark = len(trail)
    result = union_vars(v1, v2, trail, store)
    
    # Should have 1 parent change and 1 rank change
    assert len(trail) == 2
    
    # Check we have one parent and one rank entry
    tags = {e[0] for e in trail}
    assert tags == {"parent", "rank"}
    
    # Verify undoability
    undo_to(mark, trail, store)
    assert store.cells[v1].ref == v1
    assert store.cells[v2].ref == v2
    assert store.cells[v1].rank == 0
    assert store.cells[v2].rank == 0


def test_union_vars_with_non_roots_follows_chains():
    """Test union_vars works with non-root variables."""
    store = Store()
    trail = []
    
    # Create chains
    v1 = store.new_var()
    v2 = store.new_var()
    r1 = store.new_var()
    r2 = store.new_var()
    
    # v1 -> r1, v2 -> r2
    store.cells[v1].ref = r1
    store.cells[v2].ref = r2
    
    # Union the non-roots
    result = union_vars(v1, v2, trail, store)
    assert result is True
    
    # Should have unified the roots r1 and r2
    # Check that both chains lead to same root
    _, root1 = store.deref(v1)
    _, root2 = store.deref(v2)
    assert root1 == root2


def test_union_vars_idempotent_no_trail_growth():
    """Test that union of already unified vars is a no-op."""
    store = Store()
    trail = []
    
    a = store.new_var()
    b = store.new_var()
    
    # First union
    assert union_vars(a, b, trail, store) is True
    before = len(trail)
    
    # Second union of already unified roots is a no-op
    assert union_vars(a, b, trail, store) is True
    assert len(trail) == before


def test_union_vars_raises_if_either_is_bound():
    """Test union_vars raises error if either root is bound."""
    store = Store()
    trail = []
    
    a = store.new_var()
    b = store.new_var()
    
    # Bind b to an atom
    store.cells[b] = Cell(tag="bound", ref=b, term=Atom("x"))
    
    with pytest.raises(ValueError, match="union.*bound"):
        union_vars(a, b, trail, store)
    
    # Also test with a bound
    store = Store()
    trail = []
    a = store.new_var()
    b = store.new_var()
    store.cells[a] = Cell(tag="bound", ref=a, term=Int(42))
    
    with pytest.raises(ValueError, match="union.*bound"):
        union_vars(a, b, trail, store)


def test_union_vars_equal_ranks_trail_precise():
    """Test equal ranks produce exactly one parent and one rank change."""
    store = Store()
    trail = []
    
    a = store.new_var()
    b = store.new_var()
    
    assert union_vars(a, b, trail, store) is True
    
    # Exactly 2 entries: one parent, one rank
    assert len(trail) == 2
    tags = sorted(e[0] for e in trail)
    assert tags == ["parent", "rank"]
    
    # After undo, both are pristine roots with rank 0
    mark = 0
    undo_to(mark, trail, store)
    assert store.cells[a].ref == a and store.cells[a].rank == 0
    assert store.cells[b].ref == b and store.cells[b].rank == 0


# Test bind_root_to_term()
def test_bind_root_to_atom():
    """Test bind unbound root to atom."""
    store = Store()
    trail = []
    
    v = store.new_var()
    atom = Atom("hello")
    
    bind_root_to_term(v, atom, trail, store)
    
    # Variable should now be bound
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == atom
    assert store.cells[v].ref == v  # Still points to self


def test_bind_root_to_struct():
    """Test bind unbound root to struct."""
    store = Store()
    trail = []
    
    v = store.new_var()
    struct = Struct("f", (Atom("a"), Int(42)))
    
    bind_root_to_term(v, struct, trail, store)
    
    # Variable should now be bound
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == struct
    assert store.cells[v].ref == v


def test_bind_root_trail_entry():
    """Test trail entry created with old cell copy."""
    store = Store()
    trail = []
    
    v = store.new_var()
    # Capture original state
    old_cell = deepcopy(store.cells[v])
    
    atom = Atom("test")
    bind_root_to_term(v, atom, trail, store)
    
    # Should have one trail entry
    assert len(trail) == 1
    assert trail[0][0] == "bind"
    assert trail[0][1] == v
    
    # The trailed cell should match the old state
    trailed_cell = trail[0][2]
    assert trailed_cell.tag == old_cell.tag
    assert trailed_cell.ref == old_cell.ref
    assert trailed_cell.term == old_cell.term
    assert trailed_cell.rank == old_cell.rank


def test_bind_already_bound_root_raises():
    """Test binding already bound root fails."""
    store = Store()
    trail = []
    
    v = store.new_var()
    atom1 = Atom("first")
    atom2 = Atom("second")
    
    # First bind succeeds
    bind_root_to_term(v, atom1, trail, store)
    
    # Second bind should fail
    with pytest.raises(ValueError, match="already bound"):
        bind_root_to_term(v, atom2, trail, store)


def test_bind_root_undoable():
    """Test bind can be undone via trail."""
    store = Store()
    trail = []
    
    v = store.new_var()
    mark = len(trail)
    
    atom = Atom("temporary")
    bind_root_to_term(v, atom, trail, store)
    
    # Verify bound
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == atom
    
    # Undo
    undo_to(mark, trail, store)
    
    # Should be unbound again
    assert store.cells[v].tag == "unbound"
    assert store.cells[v].term is None
    assert store.cells[v].ref == v


def test_bind_root_rejects_var_term():
    """Test bind_root_to_term rejects Var terms."""
    store = Store()
    trail = []
    
    v = store.new_var()
    w = Var(store.new_var())
    
    with pytest.raises(ValueError, match="Cannot bind to Var"):
        bind_root_to_term(v, w, trail, store)


def test_bind_trail_is_snapshot_not_reference():
    """Test trailed cell is a snapshot, not a reference."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    # Take a reference to the current cell
    old_ref = store.cells[v]
    
    bind_root_to_term(v, Atom("a"), trail, store)
    
    # Try to mutate the old_ref AFTER pushing the bind entry
    # This tests that the implementation copied the cell
    # Note: Cell is not frozen, so we can mutate it
    old_ref.ref = -999
    old_ref.tag = "bound"  # Try to corrupt it
    
    # Undo should restore to pristine state regardless of mutation
    undo_to(0, trail, store)
    
    c = store.cells[v]
    assert c.tag == "unbound"
    assert c.ref == v
    assert c.term is None
    assert c.rank == 0


# Test deref_term()
def test_deref_term_nonvar_returns_nonvar():
    """Test non-var returns ('NONVAR', term)."""
    store = Store()
    
    # Test with various non-var terms
    atom = Atom("test")
    result = deref_term(atom, store)
    assert result == ("NONVAR", atom)
    
    num = Int(42)
    result = deref_term(num, store)
    assert result == ("NONVAR", num)
    
    struct = Struct("f", (Atom("a"),))
    result = deref_term(struct, store)
    assert result == ("NONVAR", struct)
    
    lst = List((Int(1), Int(2)))
    result = deref_term(lst, store)
    assert result == ("NONVAR", lst)


def test_deref_term_unbound_var_returns_var():
    """Test unbound var returns ('VAR', root_vid)."""
    store = Store()
    
    vid = store.new_var()
    var = Var(vid)
    
    result = deref_term(var, store)
    assert result == ("VAR", vid)


def test_deref_term_bound_var_returns_nonvar():
    """Test bound var returns ('NONVAR', dereferenced_term)."""
    store = Store()
    
    vid = store.new_var()
    atom = Atom("bound_value")
    
    # Bind the variable
    store.cells[vid] = Cell(tag="bound", ref=vid, term=atom)
    
    var = Var(vid)
    result = deref_term(var, store)
    assert result == ("NONVAR", atom)


def test_deref_term_follows_chains():
    """Test deref_term follows variable chains."""
    store = Store()
    
    v1 = store.new_var()
    v2 = store.new_var()
    v3 = store.new_var()
    
    # Create chain: v1 -> v2 -> v3
    store.cells[v1].ref = v2
    store.cells[v2].ref = v3
    
    var = Var(v1)
    result = deref_term(var, store)
    assert result == ("VAR", v3)
    
    # Now bind v3
    atom = Atom("final")
    store.cells[v3] = Cell(tag="bound", ref=v3, term=atom)
    
    result = deref_term(var, store)
    assert result == ("NONVAR", atom)


def test_deref_term_no_side_effects():
    """Test no side effects (no compression)."""
    store = Store()
    
    # Create long chain
    vars = [store.new_var() for _ in range(6)]
    for i in range(5):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Snapshot store state
    snapshot = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]
    
    # Call deref_term
    var = Var(vars[0])
    result = deref_term(var, store)
    
    # Verify no changes
    current = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]
    assert snapshot == current
    
    assert result == ("VAR", vars[5])


def test_deref_term_with_hint():
    """Test deref_term works with Var hints."""
    store = Store()
    
    vid = store.new_var(hint="X")
    var = Var(vid, hint="X")
    
    result = deref_term(var, store)
    assert result == ("VAR", vid)


def test_deref_term_invalid_var_id_raises():
    """Test deref_term raises on invalid variable IDs."""
    store = Store()
    
    with pytest.raises((IndexError, ValueError)):
        deref_term(Var(-1), store)
    
    with pytest.raises((IndexError, ValueError)):
        deref_term(Var(999), store)


def test_deref_term_var_alias_chain():
    """Test deref_term follows chains of unbound variables."""
    store = Store()
    
    v1 = store.new_var()
    v2 = store.new_var()
    v3 = store.new_var()
    
    # v1 -> v2 -> v3 (all unbound)
    store.cells[v1].ref = v2
    store.cells[v2].ref = v3
    
    tag, root = deref_term(Var(v1), store)
    assert tag == "VAR" and root == v3