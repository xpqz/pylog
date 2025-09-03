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


# Test Cell dataclass before it exists (TDD)
def test_cell_unbound_creation():
    """Test creating an unbound cell."""
    from prolog.unify.store import Cell
    
    cell = Cell(tag="unbound", ref=0, term=None)
    assert cell.tag == "unbound"
    assert cell.ref == 0
    assert cell.term is None
    assert cell.rank == 0  # Default value


def test_cell_bound_creation():
    """Test creating a bound cell."""
    from prolog.unify.store import Cell
    
    # Mock term for testing
    mock_term = "atom_a"
    cell = Cell(tag="bound", ref=5, term=mock_term, rank=2)
    assert cell.tag == "bound"
    assert cell.ref == 5
    assert cell.term == mock_term
    assert cell.rank == 2


def test_cell_rank_default():
    """Test that rank defaults to 0."""
    from prolog.unify.store import Cell
    
    cell = Cell(tag="unbound", ref=3, term=None)
    assert cell.rank == 0


# Test Store.new_var() before it exists (TDD)
def test_store_new_var_returns_sequential_ids():
    """Test that new_var returns sequential IDs starting from 0."""
    from prolog.unify.store import Store
    
    store = Store()
    vid1 = store.new_var()
    vid2 = store.new_var()
    vid3 = store.new_var()
    
    assert vid1 == 0
    assert vid2 == 1
    assert vid3 == 2


def test_store_new_var_creates_unbound_cell():
    """Test that new_var creates an unbound cell with self-reference."""
    from prolog.unify.store import Store
    
    store = Store()
    vid = store.new_var()
    
    cell = store.cells[vid]
    assert cell.tag == "unbound"
    assert cell.ref == vid  # Self-reference for root
    assert cell.term is None


def test_store_new_var_sets_rank_to_zero():
    """Test that new_var sets rank to 0."""
    from prolog.unify.store import Store
    
    store = Store()
    vid = store.new_var()
    
    cell = store.cells[vid]
    assert cell.rank == 0


def test_store_new_var_hint_is_optional():
    """Test that hint parameter is optional and not stored in cell."""
    from prolog.unify.store import Store
    
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
    from prolog.unify.store import Store
    
    store = Store()
    vid = store.new_var()
    
    result = store.deref(vid)
    assert result == ("UNBOUND", vid)


def test_deref_bound_var():
    """Test deref of bound var returns ('BOUND', varid, term)."""
    from prolog.unify.store import Store, Cell
    
    store = Store()
    vid = store.new_var()
    
    # Manually bind the variable for testing
    mock_term = "test_term"
    store.cells[vid] = Cell(tag="bound", ref=vid, term=mock_term)
    
    result = store.deref(vid)
    assert result == ("BOUND", vid, mock_term)


def test_deref_follows_single_parent_link():
    """Test deref follows a single parent link."""
    from prolog.unify.store import Store, Cell
    
    store = Store()
    child = store.new_var()
    parent = store.new_var()
    
    # Set child to point to parent
    store.cells[child].ref = parent
    
    result = store.deref(child)
    assert result == ("UNBOUND", parent)


def test_deref_follows_chain_of_links():
    """Test deref follows a chain of parent links."""
    from prolog.unify.store import Store
    
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
    from prolog.unify.store import Store
    
    store = Store()
    v0 = store.new_var()
    v1 = store.new_var()
    v2 = store.new_var()
    v3 = store.new_var()
    
    # Create chain: v0 -> v1 -> v2 -> v3
    store.cells[v0].ref = v1
    store.cells[v1].ref = v2
    store.cells[v2].ref = v3
    
    # Remember original refs
    orig_refs = [cell.ref for cell in store.cells]
    
    # Deref without compression
    result = store.deref(v0)
    
    # Check no side effects
    new_refs = [cell.ref for cell in store.cells]
    assert orig_refs == new_refs
    assert result == ("UNBOUND", v3)


# Test Store.deref() with path compression
def test_deref_compression_requires_compress_and_trail():
    """Test compression only happens when compress=True AND trail provided."""
    from prolog.unify.store import Store
    
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


def test_deref_no_compression_short_path():
    """Test no compression for paths < 4 nodes."""
    from prolog.unify.store import Store
    
    store = Store()
    trail = []
    
    # Create chain of 3: v0 -> v1 -> v2
    v0 = store.new_var()
    v1 = store.new_var()
    v2 = store.new_var()
    
    store.cells[v0].ref = v1
    store.cells[v1].ref = v2
    
    result = store.deref(v0, compress=True, trail=trail)
    
    # Should not compress (path length < 4)
    assert store.cells[v0].ref == v1
    assert store.cells[v1].ref == v2
    assert len(trail) == 0
    assert result == ("UNBOUND", v2)


def test_deref_compression_long_path():
    """Test compression for paths >= 4 nodes updates parents."""
    from prolog.unify.store import Store
    
    store = Store()
    trail = []
    
    # Create chain of 5: v0 -> v1 -> v2 -> v3 -> v4
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    result = store.deref(vars[0], compress=True, trail=trail)
    
    # Should compress path
    assert store.cells[vars[0]].ref == vars[4]
    assert store.cells[vars[1]].ref == vars[4]
    assert store.cells[vars[2]].ref == vars[4]
    assert store.cells[vars[3]].ref == vars[4]
    assert result == ("UNBOUND", vars[4])


def test_deref_compression_adds_trail_entries():
    """Test compression adds trail entries for each compressed link."""
    from prolog.unify.store import Store
    
    store = Store()
    trail = []
    
    # Create chain of 5: v0 -> v1 -> v2 -> v3 -> v4
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    store.deref(vars[0], compress=True, trail=trail)
    
    # Should have trail entries for compressed nodes
    assert len(trail) == 4  # v0, v1, v2, v3 compressed
    
    # Check trail entries (order-agnostic)
    expected_entries = {
        ("parent", vars[0], vars[1]),
        ("parent", vars[1], vars[2]),
        ("parent", vars[2], vars[3]),
        ("parent", vars[3], vars[4])
    }
    assert set(trail) == expected_entries


def test_deref_compression_undoability():
    """Test that compression can be undone via trail."""
    from prolog.unify.store import Store
    
    store = Store()
    trail = []
    
    # Create chain of 5: v0 -> v1 -> v2 -> v3 -> v4
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Remember original refs
    orig_refs = [store.cells[v].ref for v in vars]
    
    # Compress
    store.deref(vars[0], compress=True, trail=trail)
    
    # Verify compression happened
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[4]
    
    # Undo via trail (in reverse order)
    for entry in reversed(trail):
        tag, vid, old_val = entry
        if tag == "parent":
            store.cells[vid].ref = old_val
    
    # Verify restoration
    current_refs = [store.cells[v].ref for v in vars]
    assert current_refs == orig_refs


def test_deref_no_op_compression():
    """Test that re-compressing doesn't add trail entries."""
    from prolog.unify.store import Store
    
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
    from prolog.unify.store import Store, Cell
    
    store = Store()
    trail = []
    
    # Create chain ending in bound var
    vars = [store.new_var() for _ in range(5)]
    for i in range(4):
        store.cells[vars[i]].ref = vars[i+1]
    
    # Bind the root
    mock_term = "test_atom"
    store.cells[vars[4]] = Cell(tag="bound", ref=vars[4], term=mock_term)
    
    # Deref with compression
    result = store.deref(vars[0], compress=True, trail=trail)
    
    # Should return bound result
    assert result == ("BOUND", vars[4], mock_term)
    
    # All intermediate nodes should point to bound root
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[4]
    
    # Undo and verify restoration
    for entry in reversed(trail):
        if entry[0] == "parent":
            store.cells[entry[1]].ref = entry[2]
    
    # Chain should be restored
    for i in range(4):
        assert store.cells[vars[i]].ref == vars[i+1]


def test_deref_invalid_varid():
    """Test that invalid varids raise appropriate errors."""
    from prolog.unify.store import Store
    
    store = Store()
    
    # Test negative varid
    with pytest.raises(IndexError):
        store.deref(-1)
    
    # Test varid beyond range
    with pytest.raises(IndexError):
        store.deref(100)


def test_deref_compression_preserves_rank():
    """Test that compression doesn't modify rank values."""
    from prolog.unify.store import Store
    
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