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
    assert trail.entries[0] == ("parent", 1, 2)
    
    trail.push(("bind", 3, Cell("unbound", 3, None)))
    assert len(trail) == 2
    assert trail.entries[1][0] == "bind"


def test_trail_append_alias():
    """Test that append works as alias for push (list-like interface)."""
    trail = Trail()
    
    trail.append(("parent", 5, 6))
    assert len(trail) == 1
    assert trail.entries[0] == ("parent", 5, 6)


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
    assert trail.entries == []


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