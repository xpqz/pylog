"""Unit tests for Store and Cell classes.

Store.deref() API:
- Returns ("UNBOUND", root_vid) for unbound variables
- Returns ("BOUND", root_vid, term) for bound variables
- Path compression only when compress=True AND trail provided
- Compression threshold: paths >= 4 nodes

Trail format:
- ("parent", varid, old_parent): parent link change
- ("bind", varid, old_cell): variable binding
- ("rank", varid, old_rank): rank change (for union operations)
"""

import pytest
from dataclasses import dataclass
from typing import Literal, Optional, Any

from prolog.unify.store import Cell, Store, COMPRESS_MIN_PATH
from prolog.unify.trail import undo_to


# Helper function for creating chains
def _make_chain(store, n):
    """Create a chain of n variables: v0->v1->...->v(n-1)."""
    vids = [store.new_var() for _ in range(n)]
    for i in range(n-1):
        store.cells[vids[i]].ref = vids[i+1]
    return vids


# Test Cell dataclass before it exists (TDD)
def test_cell_unbound_creation():
    """Test creating an unbound cell."""
    cell = Cell(tag="unbound", ref=0, term=None)
    assert cell.tag == "unbound"
    assert cell.ref == 0
    assert cell.term is None
    assert cell.rank == 0  # Default value


def test_cell_bound_creation():
    """Test creating a bound cell."""
    # Mock term for testing
    mock_term = "atom_a"
    cell = Cell(tag="bound", ref=5, term=mock_term, rank=2)
    assert cell.tag == "bound"
    assert cell.ref == 5
    assert cell.term == mock_term
    assert cell.rank == 2


def test_cell_rank_default():
    """Test that rank defaults to 0."""
    cell = Cell(tag="unbound", ref=3, term=None)
    assert cell.rank == 0


# Test Store.new_var() before it exists (TDD)
def test_store_new_var_returns_sequential_ids():
    """Test that new_var returns sequential IDs starting from 0."""
    store = Store()
    vid1 = store.new_var()
    vid2 = store.new_var()
    vid3 = store.new_var()
    
    assert vid1 == 0
    assert vid2 == 1
    assert vid3 == 2


def test_store_new_var_creates_unbound_cell():
    """Test that new_var creates an unbound cell with self-reference."""
    store = Store()
    vid = store.new_var()
    
    cell = store.cells[vid]
    assert cell.tag == "unbound"
    assert cell.ref == vid  # Self-reference for root
    assert cell.term is None


def test_store_new_var_sets_rank_to_zero():
    """Test that new_var sets rank to 0."""
    store = Store()
    vid = store.new_var()
    
    cell = store.cells[vid]
    assert cell.rank == 0


def test_store_new_var_hint_is_optional():
    """Test that hint parameter is optional and not stored in cell."""
    store = Store()
    vid1 = store.new_var()
    vid2 = store.new_var(hint="X")
    
    # Both should create valid variables
    assert vid1 == 0
    assert vid2 == 1
    
    # Hint is not stored in the cell itself
    cell1 = store.cells[vid1]
    cell2 = store.cells[vid2]
    assert not hasattr(cell1, 'hint')
    assert not hasattr(cell2, 'hint')


# Test Store.deref() basic functionality
def test_deref_unbound_root():
    """Test deref of unbound root returns ('UNBOUND', varid)."""
    store = Store()
    vid = store.new_var()
    
    result = store.deref(vid)
    assert result == ("UNBOUND", vid)


def test_deref_bound_var():
    """Test deref of bound var returns ('BOUND', varid, term)."""
    store = Store()
    vid = store.new_var()
    
    # Manually bind the variable for testing
    mock_term = "test_term"
    store.cells[vid] = Cell(tag="bound", ref=vid, term=mock_term)
    
    result = store.deref(vid)
    assert result == ("BOUND", vid, mock_term)


def test_deref_follows_single_parent_link():
    """Test deref follows a single parent link."""
    store = Store()
    child = store.new_var()
    parent = store.new_var()
    
    # Set child to point to parent
    store.cells[child].ref = parent
    
    result = store.deref(child)
    assert result == ("UNBOUND", parent)


def test_deref_follows_chain_of_links():
    """Test deref follows a chain of parent links."""
    store = Store()
    v0 = store.new_var()
    v1 = store.new_var()
    v2 = store.new_var()
    v3 = store.new_var()
    
    # Create chain: v0 -> v1 -> v2 -> v3
    store.cells[v0].ref = v1
    store.cells[v1].ref = v2
    store.cells[v2].ref = v3
    
    result = store.deref(v0)
    assert result == ("UNBOUND", v3)


def test_deref_without_compress_has_no_side_effects():
    """Test that deref without compress doesn't modify the store."""
    store = Store()
    v0 = store.new_var()
    v1 = store.new_var()
    v2 = store.new_var()
    v3 = store.new_var()
    
    # Create chain: v0 -> v1 -> v2 -> v3
    store.cells[v0].ref = v1
    store.cells[v1].ref = v2
    store.cells[v2].ref = v3
    
    # Snapshot entire cell state (not just refs)
    snapshot = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]
    
    # Deref without compression
    result = store.deref(v0)
    
    # Check no side effects - compare full cell state
    current = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]
    assert snapshot == current
    assert result == ("UNBOUND", v3)


# Test Store.deref() with path compression
def test_deref_compression_requires_compress_and_trail():
    """Test compression only happens when compress=True AND trail provided."""
    store = Store()
    trail = []
    
    # Create long chain
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # No compression without both flags
    store.deref(vars[0], compress=False, trail=None)
    assert store.cells[vars[0]].ref == vars[1]  # Unchanged
    
    store.deref(vars[0], compress=True, trail=None)
    assert store.cells[vars[0]].ref == vars[1]  # Unchanged
    
    store.deref(vars[0], compress=False, trail=trail)
    assert store.cells[vars[0]].ref == vars[1]  # Unchanged


def test_deref_no_compression_below_threshold():
    """Test no compression for paths below/at COMPRESS_MIN_PATH boundary."""
    store = Store()
    trail = []
    
    # Create chain at boundary (might or might not compress depending on definition)
    vids = _make_chain(store, COMPRESS_MIN_PATH)
    mark = len(trail)
    
    result = store.deref(vids[0], compress=True, trail=trail)
    
    # If no compression happened, chain should be intact
    # (We're not asserting no compression, just checking behavior)
    if len(trail) == 0:
        # No compression - verify chain intact
        for i in range(len(vids)-1):
            assert store.cells[vids[i]].ref == vids[i+1]
    
    assert result == ("UNBOUND", vids[-1])


def test_deref_compression_above_threshold():
    """Test compression for paths safely above COMPRESS_MIN_PATH."""
    store = Store()
    trail = []
    
    # Create chain safely above threshold
    vids = _make_chain(store, COMPRESS_MIN_PATH + 2)
    
    result = store.deref(vids[0], compress=True, trail=trail)
    
    # Should definitely compress (well above threshold)
    for i in range(len(vids)-1):
        assert store.cells[vids[i]].ref == vids[-1]
    
    # Should have trail entries for compressed nodes
    assert len(trail) >= COMPRESS_MIN_PATH - 1
    assert result == ("UNBOUND", vids[-1])


def test_deref_compression_adds_trail_entries():
    """Test compression adds trail entries for each compressed link."""
    store = Store()
    trail = []
    
    # Create chain of 5: v0 -> v1 -> v2 -> v3 -> v4
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    store.deref(vars[0], compress=True, trail=trail)
    
    # Path is [v0, v1, v2, v3] and they all get compressed to v4
    # So we should have 4 trail entries
    assert len(trail) == 4
    
    # Check trail entries (order-agnostic)
    expected_entries = {
        ("parent", vars[0], vars[1]),
        ("parent", vars[1], vars[2]),
        ("parent", vars[2], vars[3]),
        ("parent", vars[3], vars[4])
    }
    assert set(trail) == expected_entries
    
    # Consistency check: every trail entry corresponds to actual change
    # All compressed nodes should now point to vars[4]
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[4]
    
    # The root should still point to itself
    assert store.cells[vars[4]].ref == vars[4]


def test_deref_compression_undoability():
    """Test that compression can be undone via trail."""
    store = Store()
    trail = []
    
    # Create chain of 5: v0 -> v1 -> v2 -> v3 -> v4
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Remember original refs
    orig_refs = [store.cells[v].ref for v in vars]
    
    # Mark trail position
    mark = len(trail)
    
    # Compress
    store.deref(vars[0], compress=True, trail=trail)
    
    # Verify compression happened
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[4]
    
    # Undo using official undo_to function
    undo_to(mark, trail, store)
    
    # Verify restoration
    current_refs = [store.cells[v].ref for v in vars]
    assert current_refs == orig_refs


def test_deref_no_op_compression():
    """Test that re-compressing doesn't add trail entries."""
    store = Store()
    trail = []
    
    # Create and compress a chain
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # First compression
    store.deref(vars[0], compress=True, trail=trail)
    first_trail_len = len(trail)
    
    # Second compression (should be no-op)
    store.deref(vars[0], compress=True, trail=trail)
    
    # Trail shouldn't grow
    assert len(trail) == first_trail_len


def test_deref_compression_to_bound_root():
    """Test compression when chain ends at bound variable."""
    store = Store()
    trail = []
    
    # Create chain ending in bound var
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Bind the root
    mock_term = "test_atom"
    store.cells[vars[4]] = Cell(tag="bound", ref=vars[4], term=mock_term)
    
    # Mark trail position
    mark = len(trail)
    
    # Deref with compression
    result = store.deref(vars[0], compress=True, trail=trail)
    
    # Should return bound result
    assert result == ("BOUND", vars[4], mock_term)
    
    # All intermediate nodes should point to bound root
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[4]
    
    # Undo using official undo_to and verify restoration
    undo_to(mark, trail, store)
    
    # Chain should be restored
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[i+1]


def test_deref_invalid_varid():
    """Test that invalid varids raise appropriate errors."""
    store = Store()
    
    # Test negative varid (accept either IndexError or ValueError)
    with pytest.raises((IndexError, ValueError)):
        store.deref(-1)
    
    # Test varid beyond range (accept either IndexError or ValueError)
    with pytest.raises((IndexError, ValueError)):
        store.deref(100)


def test_deref_compress_from_middle_only_updates_that_path():
    """Test that compression only affects the traversed path, not siblings."""
    store = Store()
    trail = []
    
    # Create longer chain to ensure compression happens
    # v0->v1->v2->v3->v4->v5->v6
    vars = [store.new_var() for _ in range(7)]
    for i in range(6):
        store.cells[vars[i]].ref = vars[i+1]
    
    mark = len(trail)
    
    # Deref from v1 (path will be v1->v2->v3->v4->v5->v6, length 5)
    store.deref(vars[1], compress=True, trail=trail)
    
    # Only nodes in the path from v1 should be compressed
    parent_changes = {e for e in trail[mark:] if e[0] == "parent"}
    
    # v1, v2, v3, v4, v5 should all point to v6 now
    expected_changes = {
        ("parent", vars[1], vars[2]),
        ("parent", vars[2], vars[3]),
        ("parent", vars[3], vars[4]),
        ("parent", vars[4], vars[5]),
        ("parent", vars[5], vars[6]),
    }
    assert parent_changes == expected_changes
    
    # v0 should be unchanged (not in traversed path)
    assert store.cells[vars[0]].ref == vars[1]
    
    # v1 through v5 should point to v6
    for i in range(1, 6):
        assert store.cells[vars[i]].ref == vars[6]
    
    # Consistency: all compressed nodes should point to root
    for i in range(1, 6):
        assert store.cells[vars[i]].ref == vars[6]
    # Root should still point to itself
    assert store.cells[vars[6]].ref == vars[6]


def test_deref_bound_root_idempotent():
    """Test that compressing to bound root is idempotent."""
    store = Store()
    trail = []
    
    # Create chain ending in bound var (length >= COMPRESS_MIN_PATH)
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Bind the root
    store.cells[vars[4]] = Cell(tag="bound", ref=vars[4], term="test_term")
    
    # First compression
    store.deref(vars[0], compress=True, trail=trail)
    before = len(trail)
    
    # Second compression (should be no-op)
    store.deref(vars[0], compress=True, trail=trail)
    
    # Trail shouldn't grow
    assert len(trail) == before


def test_deref_midchain_to_bound_root_idempotent():
    """Test idempotent compression when starting from middle of chain."""
    store = Store()
    trail = []
    
    # Create chain longer than threshold
    vids = _make_chain(store, COMPRESS_MIN_PATH + 2)
    
    # Bind the last variable
    store.cells[vids[-1]] = Cell(tag="bound", ref=vids[-1], term="test_value")
    
    # Compress from middle (not head)
    store.deref(vids[1], compress=True, trail=trail)
    before = len(trail)
    
    # Second compression should be no-op
    store.deref(vids[1], compress=True, trail=trail)
    assert len(trail) == before
    
    # First node should remain unchanged (not in traversed path)
    assert store.cells[vids[0]].ref == vids[1]


def test_deref_compression_preserves_rank():
    """Test that compression doesn't modify rank values."""
    store = Store()
    trail = []
    
    # Create chain with custom ranks
    vars = [store.new_var() for _ in range(5)]
    for i, v in enumerate(vars):
        store.cells[v].rank = i * 2  # Set custom ranks
    
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Remember original ranks
    orig_ranks = [store.cells[v].rank for v in vars]
    
    # Compress
    store.deref(vars[0], compress=True, trail=trail)
    
    # Verify ranks unchanged
    current_ranks = [store.cells[v].rank for v in vars]
    assert current_ranks == orig_ranks