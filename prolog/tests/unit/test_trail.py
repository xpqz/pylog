"""Unit tests for Trail and undo operations.

The Trail records all mutations for backtracking:
- push/append adds entries
- mark returns current position
- undo_to restores state by applying inverse operations
- trail_guard provides automatic cleanup on exceptions
"""

import pytest

from prolog.unify.store import Cell, Store
from prolog.unify.trail import Trail, undo_to, trail_guard


# Test basic Trail operations
def test_trail_push_adds_entry():
    """Test that push adds an entry to the trail."""
    trail = Trail()
    
    assert len(trail) == 0
    
    trail.push(("parent", 1, 2))
    assert len(trail) == 1
    assert trail[0] == ("parent", 1, 2)
    
    trail.push(("bind", 3, Cell("unbound", 3, None)))
    assert len(trail) == 2
    assert trail[1][0] == "bind"


def test_trail_append_alias():
    """Test that append works as alias for push (list-like interface)."""
    trail = Trail()
    
    trail.append(("parent", 5, 6))
    assert len(trail) == 1
    assert trail[0] == ("parent", 5, 6)


def test_trail_mark_returns_position():
    """Test that mark returns current trail position."""
    trail = Trail()
    
    mark1 = trail.mark()
    assert mark1 == 0
    
    trail.push(("parent", 1, 2))
    trail.push(("parent", 3, 4))
    
    mark2 = trail.mark()
    assert mark2 == 2
    
    trail.push(("parent", 5, 6))
    
    mark3 = trail.mark()
    assert mark3 == 3


def test_trail_clear_empties():
    """Test that clear empties the trail."""
    trail = Trail()
    
    trail.push(("parent", 1, 2))
    trail.push(("bind", 3, Cell("unbound", 3, None)))
    assert len(trail) == 2
    
    trail.clear()
    assert len(trail) == 0
    # Verify it's truly empty by checking we can't index
    with pytest.raises(IndexError):
        _ = trail[0]


def test_trail_indexing():
    """Test that trail supports indexing."""
    trail = Trail()
    
    entries = [
        ("parent", 1, 2),
        ("parent", 3, 4),
        ("bind", 5, Cell("unbound", 5, None))
    ]
    
    for entry in entries:
        trail.push(entry)
    
    # Test indexing
    assert trail[0] == entries[0]
    assert trail[1] == entries[1]
    assert trail[2] == entries[2]
    assert trail[-1] == entries[2]


# Test undo_to with different entry types
def test_undo_to_parent_entry():
    """Test undo_to restores parent links."""
    store = Store()
    trail = []
    
    # Create variables
    v1 = store.new_var()
    v2 = store.new_var()
    
    mark = len(trail)
    
    # Change parent and trail it
    trail.append(("parent", v1, v1))  # Old value: pointed to self
    store.cells[v1].ref = v2  # New value: points to v2
    
    # Verify change
    assert store.cells[v1].ref == v2
    
    # Undo
    undo_to(mark, trail, store)
    
    # Verify restoration
    assert store.cells[v1].ref == v1
    assert len(trail) == mark


def test_undo_to_bind_entry():
    """Test undo_to restores entire cell for bind entries."""
    store = Store()
    trail = []
    
    v = store.new_var()
    old_cell = Cell(tag="unbound", ref=v, term=None, rank=0)
    
    mark = len(trail)
    
    # Bind variable and trail old cell
    trail.append(("bind", v, old_cell))
    store.cells[v] = Cell(tag="bound", ref=v, term="atom_a", rank=0)
    
    # Verify change
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == "atom_a"
    
    # Undo
    undo_to(mark, trail, store)
    
    # Verify restoration
    assert store.cells[v].tag == "unbound"
    assert store.cells[v].term is None
    assert store.cells[v].ref == v


def test_undo_to_rank_entry():
    """Test undo_to restores rank values."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    mark = len(trail)
    
    # Change rank and trail it
    trail.append(("rank", v, 0))  # Old rank
    store.cells[v].rank = 3  # New rank
    
    # Verify change
    assert store.cells[v].rank == 3
    
    # Undo
    undo_to(mark, trail, store)
    
    # Verify restoration
    assert store.cells[v].rank == 0
    assert len(trail) == mark


def test_undo_to_multiple_entries():
    """Test undo_to handles multiple entries correctly."""
    store = Store()
    trail = []
    
    # Create variables
    v1 = store.new_var()
    v2 = store.new_var()
    v3 = store.new_var()
    
    mark = len(trail)
    
    # Make multiple changes
    trail.append(("parent", v1, v1))
    store.cells[v1].ref = v2
    
    trail.append(("parent", v2, v2))
    store.cells[v2].ref = v3
    
    trail.append(("rank", v1, 0))
    store.cells[v1].rank = 2
    
    # Verify changes
    assert store.cells[v1].ref == v2
    assert store.cells[v2].ref == v3
    assert store.cells[v1].rank == 2
    
    # Undo all
    undo_to(mark, trail, store)
    
    # Verify restoration
    assert store.cells[v1].ref == v1
    assert store.cells[v2].ref == v2
    assert store.cells[v1].rank == 0
    assert len(trail) == mark


def test_undo_to_partial():
    """Test undo_to can undo to intermediate positions."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    mark1 = len(trail)
    
    # First change
    trail.append(("parent", v1, v1))
    store.cells[v1].ref = v2
    
    mark2 = len(trail)
    
    # Second change
    trail.append(("rank", v1, 0))
    store.cells[v1].rank = 5
    
    # Undo only the second change
    undo_to(mark2, trail, store)
    
    # First change should remain
    assert store.cells[v1].ref == v2
    # Second change should be undone
    assert store.cells[v1].rank == 0
    assert len(trail) == mark2


def test_undo_to_with_trail_object():
    """Test undo_to works with Trail objects (not just lists)."""
    store = Store()
    trail = Trail()
    
    v = store.new_var()
    mark = trail.mark()
    
    # Make change
    trail.push(("parent", v, v))
    store.cells[v].ref = 99
    
    # Undo with Trail object
    undo_to(mark, trail, store)
    
    assert store.cells[v].ref == v
    assert len(trail) == mark


def test_undo_to_unknown_tag_raises():
    """Test undo_to raises error for unknown trail tags."""
    store = Store()
    trail = []
    
    trail.append(("unknown_tag", 1, 2))
    
    with pytest.raises(ValueError, match="Unknown trail entry tag"):
        undo_to(0, trail, store)


# Test trail_guard context manager
def test_trail_guard_yields_mark():
    """Test trail_guard yields the current mark."""
    store = Store()
    trail = []
    
    trail.append(("parent", 1, 2))
    
    with trail_guard(trail, store) as mark:
        assert mark == 1
        trail.append(("parent", 3, 4))
    
    # Trail should still have both entries (no exception)
    assert len(trail) == 2


def test_trail_guard_undoes_on_exception():
    """Test trail_guard calls undo_to on exception."""
    store = Store()
    trail = []
    
    v = store.new_var()
    initial_ref = store.cells[v].ref
    
    try:
        with trail_guard(trail, store) as mark:
            # Make changes
            trail.append(("parent", v, initial_ref))
            store.cells[v].ref = 999
            
            # Verify change
            assert store.cells[v].ref == 999
            
            # Raise exception
            raise ValueError("Test exception")
    except ValueError:
        pass
    
    # Changes should be undone
    assert store.cells[v].ref == initial_ref
    assert len(trail) == 0


def test_trail_guard_no_undo_on_success():
    """Test trail_guard doesn't undo on successful completion."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    with trail_guard(trail, store):
        trail.append(("parent", v, v))
        store.cells[v].ref = 999
    
    # Changes should persist
    assert store.cells[v].ref == 999
    assert len(trail) == 1


def test_trail_guard_with_trail_object():
    """Test trail_guard works with Trail objects."""
    store = Store()
    trail = Trail()
    
    v = store.new_var()
    
    try:
        with trail_guard(trail, store) as mark:
            trail.push(("parent", v, v))
            store.cells[v].ref = 999
            raise RuntimeError("Test")
    except RuntimeError:
        pass
    
    # Should be undone
    assert store.cells[v].ref == v
    assert len(trail) == 0


def test_trail_guard_reraises_exception():
    """Test trail_guard re-raises the original exception."""
    store = Store()
    trail = []
    
    class CustomError(Exception):
        pass
    
    with pytest.raises(CustomError, match="Original"):
        with trail_guard(trail, store):
            raise CustomError("Original error")


# Additional robustness tests
def test_undo_to_idempotent():
    """Test that undoing twice to same mark is a no-op."""
    store = Store()
    trail = []
    
    v = store.new_var()
    mark = len(trail)
    
    trail.append(("parent", v, v))
    store.cells[v].ref = 42
    
    # First undo
    undo_to(mark, trail, store)
    assert store.cells[v].ref == v
    
    # Second undo should be no-op
    undo_to(mark, trail, store)
    assert store.cells[v].ref == v
    assert len(trail) == mark


def test_trail_guard_nested_marks():
    """Test nested trail_guard contexts roll back only their segment."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    with trail_guard(trail, store):
        trail.append(("parent", v, v))
        store.cells[v].ref = 111
        
        try:
            with trail_guard(trail, store):
                trail.append(("rank", v, store.cells[v].rank))
                store.cells[v].rank = 9
                raise RuntimeError("inner exception")
        except RuntimeError:
            pass
        
        # Inner undone, outer change remains
        assert store.cells[v].ref == 111
        assert store.cells[v].rank == 0
    
    # After exiting outer guard, changes persist
    assert store.cells[v].ref == 111


def test_bind_trails_full_cell_snapshot_not_reference():
    """Test that bind trail should store a snapshot, not a live reference.
    
    This test documents that the current implementation stores references,
    which means callers must pass copies when trailing bind entries.
    """
    store = Store()
    trail = []
    
    v = store.new_var()
    # When trailing a bind, we should pass a copy
    old_cell = store.cells[v]
    # Create a proper snapshot
    old_snapshot = Cell(tag=old_cell.tag, ref=old_cell.ref, 
                       term=old_cell.term, rank=old_cell.rank)
    mark = len(trail)
    
    # Push snapshot (not the live cell)
    trail.append(("bind", v, old_snapshot))
    
    # Mutate the store
    store.cells[v] = Cell(tag="bound", ref=v, term="a", rank=7)
    
    # Even if we mutate old_cell (the original reference), 
    # old_snapshot remains unchanged
    old_cell.ref = -999
    old_cell.tag = "bound"
    
    # Undo should restore from snapshot
    undo_to(mark, trail, store)
    
    # Must restore original unbound state
    assert store.cells[v].tag == "unbound"
    assert store.cells[v].ref == v
    assert store.cells[v].term is None
    assert store.cells[v].rank == 0


def test_undo_to_invalid_mark_behavior():
    """Test undo_to behavior with invalid marks."""
    store = Store()
    trail = []
    
    # Undoing to current position (0) should be no-op
    undo_to(0, trail, store)
    assert len(trail) == 0
    
    # Undoing to mark > len(trail) should be no-op or safe
    # (Implementation choice: we'll make it a no-op)
    undo_to(10, trail, store)
    assert len(trail) == 0
    
    # Add some entries
    v = store.new_var()
    trail.append(("parent", v, v))
    store.cells[v].ref = 42
    
    # Undo to mark beyond current length (should be no-op)
    undo_to(10, trail, store)
    assert len(trail) == 1  # No change
    assert store.cells[v].ref == 42  # No change


def test_undo_mixed_entries_lifo():
    """Test that undo processes mixed entry types in strict LIFO order."""
    store = Store()
    trail = []
    
    v = store.new_var()
    w = store.new_var()
    mark = len(trail)
    
    # 1) parent on v
    trail.append(("parent", v, v))
    store.cells[v].ref = w
    
    # 2) rank on v
    trail.append(("rank", v, 0))
    store.cells[v].rank = 3
    
    # 3) bind on w
    old_w = Cell("unbound", w, None, 0)
    trail.append(("bind", w, old_w))
    store.cells[w] = Cell("bound", w, "term", 0)
    
    # Verify changes took effect
    assert store.cells[v].ref == w
    assert store.cells[v].rank == 3
    assert store.cells[w].tag == "bound"
    
    # Undo in LIFO order
    undo_to(mark, trail, store)
    
    # All should be restored
    assert store.cells[v].ref == v
    assert store.cells[v].rank == 0
    assert store.cells[w].tag == "unbound"
    assert store.cells[w].term is None
    assert len(trail) == mark