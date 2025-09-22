"""Tests for attributed variables - Stage 4 Core Attribute Storage (4.0).

Tests basic attribute storage, trailing, and restoration functionality.
"""

import pytest
from prolog.unify.store import Store, Cell
from prolog.unify.trail import Trail, undo_to
from prolog.ast.terms import Atom, Int, Var


class TestStoreAttributeSupport:
    """Test Store extension with attribute support."""

    def test_store_initializes_with_empty_attrs(self):
        """Store should initialize with empty attrs dict."""
        store = Store()
        assert hasattr(store, 'attrs')
        assert store.attrs == {}

    def test_get_attrs_returns_none_for_var_without_attributes(self):
        """get_attrs should return None for variable without attributes."""
        store = Store()
        v = store.new_var()
        assert store.get_attrs(v) is None

    def test_get_attrs_follows_deref_to_root(self):
        """get_attrs should follow deref chain to find root variable."""
        store = Store()
        trail = Trail()

        # Create two variables
        v1 = store.new_var()
        v2 = store.new_var()

        # Put attribute on v1
        store.put_attr(v1, "test", Int(42), trail)

        # Create alias v1 -> v2 (make v2 the root)
        store.cells[v1].ref = v2

        # get_attrs on v1 should find attrs on root (v2)
        # But wait - attrs should be on root, need to move them during aliasing
        # For this test, manually move attrs to root
        store.attrs[v2] = store.attrs.pop(v1, {})

        assert store.get_attrs(v1) == {"test": Int(42)}

    def test_get_attr_returns_specific_module_value(self):
        """get_attr should return value for specific module."""
        store = Store()
        trail = Trail()

        v = store.new_var()
        store.put_attr(v, "color", Atom("red"), trail)
        store.put_attr(v, "size", Int(10), trail)

        assert store.get_attr(v, "color") == Atom("red")
        assert store.get_attr(v, "size") == Int(10)
        assert store.get_attr(v, "missing") is None

    def test_put_attr_sets_and_trails_value(self):
        """put_attr should set attribute and trail the change."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Put initial attribute
        store.put_attr(v, "test", Int(1), trail)

        # Check it was set
        assert store.get_attr(v, "test") == Int(1)

        # Check trail has entry
        assert len(trail) == 1
        entry = trail.entries[0]
        assert entry[0] == 'attr'
        assert entry[1] == v
        assert entry[2] == "test"
        assert entry[3] is None  # old value was None

    def test_put_attr_overwrites_with_trailing(self):
        """put_attr should overwrite existing value and trail old value."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Put initial value
        store.put_attr(v, "test", Int(1), trail)
        initial_trail_len = len(trail)

        # Overwrite with new value
        store.put_attr(v, "test", Int(2), trail)

        # Check new value
        assert store.get_attr(v, "test") == Int(2)

        # Check trail has new entry with old value
        assert len(trail) == initial_trail_len + 1
        entry = trail.entries[-1]
        assert entry[0] == 'attr'
        assert entry[1] == v
        assert entry[2] == "test"
        assert entry[3] == Int(1)  # old value

    def test_del_attr_removes_with_trailing(self):
        """del_attr should remove attribute and trail the deletion."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Put attribute
        store.put_attr(v, "test", Int(42), trail)

        # Delete it
        store.del_attr(v, "test", trail)

        # Check it's gone
        assert store.get_attr(v, "test") is None

        # Check trail has deletion entry
        assert len(trail) == 2  # put + del
        entry = trail.entries[-1]
        assert entry[0] == 'attr'
        assert entry[1] == v
        assert entry[2] == "test"
        assert entry[3] == Int(42)  # old value that was deleted

    def test_del_attr_succeeds_when_absent(self):
        """del_attr should succeed even if attribute is absent."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Delete non-existent attribute - should not raise
        store.del_attr(v, "nonexistent", trail)

        # No trail entry for no-op deletion
        assert len(trail) == 0

    def test_attributes_only_on_root_after_deref(self):
        """Attributes should only be stored on union-find roots."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Put attribute (v is its own root)
        store.put_attr(v, "test", Int(1), trail)

        # Verify stored on root
        assert v in store.attrs
        assert "test" in store.attrs[v]

    def test_put_attr_targets_deref_root(self):
        """put_attr should target the dereferenced root when var is an alias."""
        store = Store()
        trail = Trail()

        # Create two variables
        v1 = store.new_var()
        v2 = store.new_var()

        # Make v1 point to v2 (v2 is the root)
        store.cells[v1].ref = v2

        # Put attr on v1 should store on root (v2)
        store.put_attr(v1, "test", Int(42), trail)

        # Attribute should be on v2, not v1
        assert v2 in store.attrs
        assert "test" in store.attrs[v2]
        assert store.attrs[v2]["test"] == Int(42)
        assert v1 not in store.attrs  # Child should have no attrs dict

    def test_del_attr_targets_deref_root(self):
        """del_attr should target the dereferenced root when var is an alias."""
        store = Store()
        trail = Trail()

        # Create two variables
        v1 = store.new_var()
        v2 = store.new_var()

        # Make v1 point to v2 (v2 is the root)
        store.cells[v1].ref = v2

        # Put attr on v2 directly
        store.put_attr(v2, "test", Int(42), trail)
        assert v2 in store.attrs

        # Delete via v1 (alias) should delete from root (v2)
        store.del_attr(v1, "test", trail)

        # Attribute should be gone from v2
        assert v2 not in store.attrs  # Cleaned up when empty

        # v1 should never have had an attrs entry
        assert v1 not in store.attrs

    def test_no_duplicate_attrs_dicts_for_aliases(self):
        """After put_attr on alias, child should not have attrs dict."""
        store = Store()
        trail = Trail()

        # Create chain: v1 -> v2 -> v3 (v3 is root)
        v1 = store.new_var()
        v2 = store.new_var()
        v3 = store.new_var()

        store.cells[v1].ref = v2
        store.cells[v2].ref = v3

        # Put attrs via different points in the chain
        store.put_attr(v1, "a", Int(1), trail)
        store.put_attr(v2, "b", Int(2), trail)
        store.put_attr(v3, "c", Int(3), trail)

        # All attrs should be on root (v3) only
        assert v3 in store.attrs
        assert "a" in store.attrs[v3]
        assert "b" in store.attrs[v3]
        assert "c" in store.attrs[v3]

        # No attrs dicts on non-root vars
        assert v1 not in store.attrs
        assert v2 not in store.attrs


class TestTrailAttributeSupport:
    """Test trail support for attribute operations."""

    def test_trail_handles_attr_entries(self):
        """Trail should accept and store attr entries."""
        trail = Trail()

        # Push attr entry
        trail.push(('attr', 0, 'module', Int(42)))

        assert len(trail) == 1
        assert trail.entries[0] == ('attr', 0, 'module', Int(42))

    def test_undo_to_restores_attribute_value(self):
        """undo_to should restore attribute to old value."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Put initial value
        store.put_attr(v, "test", Int(1), trail)

        # Mark trail position
        mark = trail.mark()

        # Change value
        store.put_attr(v, "test", Int(2), trail)

        # Undo to mark
        undo_to(mark, trail, store)

        # Should be back to original value
        assert store.get_attr(v, "test") == Int(1)

    def test_undo_to_removes_attribute_if_was_none(self):
        """undo_to should remove attribute if it didn't exist before."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Mark trail before any attributes
        mark = trail.mark()

        # Add attribute
        store.put_attr(v, "test", Int(42), trail)
        assert store.get_attr(v, "test") == Int(42)

        # Undo to mark
        undo_to(mark, trail, store)

        # Attribute should be gone
        assert store.get_attr(v, "test") is None
        assert v not in store.attrs or "test" not in store.attrs.get(v, {})

    def test_multiple_modules_restored_correctly(self):
        """Multiple modules on same var should be restored in correct order."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Add multiple modules
        store.put_attr(v, "color", Atom("red"), trail)
        store.put_attr(v, "size", Int(10), trail)

        mark = trail.mark()

        # Modify both
        store.put_attr(v, "color", Atom("blue"), trail)
        store.put_attr(v, "size", Int(20), trail)

        # Add a new one
        store.put_attr(v, "shape", Atom("circle"), trail)

        # Undo to mark
        undo_to(mark, trail, store)

        # Original values restored
        assert store.get_attr(v, "color") == Atom("red")
        assert store.get_attr(v, "size") == Int(10)
        assert store.get_attr(v, "shape") is None

    def test_complex_backtrack_sequence(self):
        """Complex sequence of operations and backtracking."""
        store = Store()
        trail = Trail()

        v1 = store.new_var()
        v2 = store.new_var()

        # Initial state
        store.put_attr(v1, "a", Int(1), trail)
        mark1 = trail.mark()

        # More changes
        store.put_attr(v1, "b", Int(2), trail)
        store.put_attr(v2, "c", Int(3), trail)
        mark2 = trail.mark()

        # Even more changes
        store.put_attr(v1, "a", Int(10), trail)  # overwrite
        store.del_attr(v1, "b", trail)  # delete
        store.put_attr(v2, "d", Int(4), trail)  # new

        # Undo to mark2
        undo_to(mark2, trail, store)
        assert store.get_attr(v1, "a") == Int(1)
        assert store.get_attr(v1, "b") == Int(2)
        assert store.get_attr(v2, "c") == Int(3)
        assert store.get_attr(v2, "d") is None

        # Undo to mark1
        undo_to(mark1, trail, store)
        assert store.get_attr(v1, "a") == Int(1)
        assert store.get_attr(v1, "b") is None
        assert store.get_attr(v2, "c") is None


class TestRuntimeTrailVerification:
    """Verify runtime Trail already has attr support."""

    def test_runtime_trail_has_push_attr(self):
        """Runtime Trail should already have push_attr method."""
        from prolog.engine.runtime import Trail as RuntimeTrail

        trail = RuntimeTrail()

        # Verify push_attr exists
        assert hasattr(trail, 'push_attr')

        # Test it works (runtime Trail has different signature with old_value param)
        trail.push_attr(0, "module", Int(42))  # old_value is Int(42)

        # Check entry was added (runtime Trail uses _entries)
        assert len(trail._entries) == 1
        entry = trail._entries[0]
        assert entry[0] == 'attr'
        assert entry[1] == 0
        assert entry[2] == "module"
        assert entry[3] == Int(42)

    def test_runtime_trail_unwind_handles_attrs(self):
        """Runtime Trail.unwind_to should handle attr entries."""
        from prolog.engine.runtime import Trail as RuntimeTrail

        # Create a minimal store with attrs support
        store = Store()
        trail = RuntimeTrail()

        v = store.new_var()

        # Set initial attribute (no trailing needed for initial set)
        if not hasattr(store, 'attrs'):
            store.attrs = {}
        store.attrs[v] = {"test": Int(1)}

        # Mark position before changes
        mark = len(trail._entries)

        # Advance stamp to simulate new choice region
        trail.next_stamp()

        # Change attribute and trail the old value
        old_val = store.attrs[v]["test"]
        store.attrs[v]["test"] = Int(2)
        trail.push_attr(v, "test", old_val)

        # Verify change took effect
        assert store.attrs[v]["test"] == Int(2)

        # Unwind
        trail.unwind_to(mark, store)

        # Should be restored
        assert store.attrs[v]["test"] == Int(1)


class TestSparseStorage:
    """Test that attribute storage is sparse (only for vars with attrs)."""

    def test_no_attrs_dict_entry_for_vars_without_attrs(self):
        """Variables without attributes should not have entries in store.attrs."""
        store = Store()
        trail = Trail()

        # Create multiple variables
        v1 = store.new_var()
        v2 = store.new_var()
        v3 = store.new_var()

        # Only add attrs to v2
        store.put_attr(v2, "test", Int(42), trail)

        # Check sparse storage
        assert v1 not in store.attrs
        assert v2 in store.attrs
        assert v3 not in store.attrs

        # After deletion, entry should be removed
        store.del_attr(v2, "test", trail)
        assert v2 not in store.attrs  # Completely removed when empty

    def test_attrs_dict_removed_when_last_module_deleted(self):
        """When last module is deleted, var should be removed from attrs dict."""
        store = Store()
        trail = Trail()

        v = store.new_var()

        # Add multiple modules
        store.put_attr(v, "a", Int(1), trail)
        store.put_attr(v, "b", Int(2), trail)
        assert v in store.attrs

        # Delete one - var still in attrs
        store.del_attr(v, "a", trail)
        assert v in store.attrs
        assert "b" in store.attrs[v]

        # Delete last - var removed from attrs
        store.del_attr(v, "b", trail)
        assert v not in store.attrs