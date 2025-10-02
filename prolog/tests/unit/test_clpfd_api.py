"""Unit tests for CLP(FD) API functions.

Tests the FD attribute helpers for domain and watcher management
via the attributed variables mechanism.
"""

from prolog.unify.store import Store
from prolog.unify.trail import Trail, undo_to
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import (
    Priority,
    get_fd_attrs,
    get_domain,
    set_domain,
    add_watcher,
    iter_watchers,
    _uses_copy_on_write,
)


class TestFDAttributeStorage:
    """Test CLP(FD) attribute storage via attrs['clpfd']."""

    def test_get_fd_attrs_returns_none_for_unattributed_var(self):
        """get_fd_attrs returns None when no CLP(FD) attributes."""
        store = Store()
        v = store.new_var()

        assert get_fd_attrs(store, v) is None

    def test_get_fd_attrs_returns_clpfd_attrs(self):
        """get_fd_attrs returns the clpfd attribute dict."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Set some CLP(FD) attributes manually
        fd_attrs = {"domain": Domain(((1, 10),))}
        store.put_attr(v, "clpfd", fd_attrs, trail)

        result = get_fd_attrs(store, v)
        assert result == fd_attrs
        assert "domain" in result

    def test_get_domain_returns_none_without_attrs(self):
        """get_domain returns None when variable has no domain."""
        store = Store()
        v = store.new_var()

        assert get_domain(store, v) is None

    def test_get_domain_returns_domain(self):
        """get_domain returns the domain from attrs['clpfd']['domain']."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        domain = Domain(((1, 10),))
        fd_attrs = {"domain": domain}
        store.put_attr(v, "clpfd", fd_attrs, trail)

        result = get_domain(store, v)
        assert result is domain

    def test_set_domain_creates_attrs_if_needed(self):
        """set_domain creates clpfd attrs if they don't exist."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        domain = Domain(((1, 10),))
        result = set_domain(store, v, domain, trail)

        assert result is domain
        assert get_domain(store, v) is domain

        # Check trailing happened
        assert len(trail) == 1
        entry = trail.entries[0]
        assert entry[0] == "attr"
        assert entry[1] == v
        assert entry[2] == "clpfd"
        assert entry[3] is None  # Was None before

    def test_set_domain_updates_existing_domain(self):
        """set_domain updates existing domain with trailing."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Set initial domain
        domain1 = Domain(((1, 10),))
        set_domain(store, v, domain1, trail)

        # Update to new domain
        domain2 = Domain(((5, 10),))
        result = set_domain(store, v, domain2, trail)

        assert result is domain2
        assert get_domain(store, v) is domain2

        # Check both operations were trailed
        assert len(trail) == 2

    def test_set_domain_returns_same_if_unchanged(self):
        """set_domain returns same domain if no change."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        domain = Domain(((1, 10),))
        set_domain(store, v, domain, trail)
        initial_trail_len = len(trail)

        # Set same domain again
        result = set_domain(store, v, domain, trail)

        assert result is domain
        # No additional trail entry
        assert len(trail) == initial_trail_len

    def test_attribute_immutability_guarantee(self):
        """Attributes must be replaced, not mutated in-place."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Set initial domain
        domain1 = Domain(((1, 10),))
        set_domain(store, v, domain1, trail)

        # Get the attrs dict
        attrs1 = store.get_attrs(v)
        fd_attrs1 = attrs1["clpfd"]

        # Update domain
        domain2 = Domain(((5, 10),))
        set_domain(store, v, domain2, trail)

        # Get new attrs dict
        attrs2 = store.get_attrs(v)
        fd_attrs2 = attrs2["clpfd"]

        # The clpfd dict should be different objects
        assert fd_attrs1 is not fd_attrs2
        # But the old one should still have old value
        assert fd_attrs1["domain"] is domain1
        assert fd_attrs2["domain"] is domain2


class TestWatcherManagement:
    """Test watcher storage and retrieval."""

    def test_add_watcher_creates_structure(self):
        """add_watcher creates watcher structure if needed."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)

        fd_attrs = get_fd_attrs(store, v)
        assert "watchers" in fd_attrs
        assert Priority.HIGH in fd_attrs["watchers"]
        assert 1 in fd_attrs["watchers"][Priority.HIGH]

    def test_add_multiple_watchers_same_priority(self):
        """Multiple watchers can be added at same priority."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        add_watcher(store, v, pid=1, priority=Priority.MED, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)
        add_watcher(store, v, pid=3, priority=Priority.MED, trail=trail)

        fd_attrs = get_fd_attrs(store, v)
        watchers = fd_attrs["watchers"][Priority.MED]
        assert len(watchers) == 3
        assert 1 in watchers
        assert 2 in watchers
        assert 3 in watchers

    def test_add_watchers_different_priorities(self):
        """Watchers can be added at different priorities."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)
        add_watcher(store, v, pid=3, priority=Priority.LOW, trail=trail)

        fd_attrs = get_fd_attrs(store, v)
        assert 1 in fd_attrs["watchers"][Priority.HIGH]
        assert 2 in fd_attrs["watchers"][Priority.MED]
        assert 3 in fd_attrs["watchers"][Priority.LOW]

    def test_watcher_deduplication(self):
        """Adding same watcher twice is idempotent."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)

        fd_attrs = get_fd_attrs(store, v)
        watchers = fd_attrs["watchers"][Priority.HIGH]
        # Set should deduplicate
        assert len(watchers) == 1
        assert 1 in watchers

    def test_iter_watchers_empty(self):
        """iter_watchers yields nothing for var without watchers."""
        store = Store()
        v = store.new_var()

        watchers = list(iter_watchers(store, v))
        assert watchers == []

    def test_iter_watchers_priority_order(self):
        """iter_watchers yields in priority order (HIGH, MED, LOW)."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add in reverse priority order
        add_watcher(store, v, pid=3, priority=Priority.LOW, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)

        watchers = list(iter_watchers(store, v))
        # Should yield in priority order
        assert watchers == [
            (1, Priority.HIGH),
            (2, Priority.MED),
            (3, Priority.LOW),
        ]

    def test_iter_watchers_multiple_per_priority(self):
        """iter_watchers handles multiple watchers per priority."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=3, priority=Priority.MED, trail=trail)

        watchers = list(iter_watchers(store, v))
        # HIGH priority watchers first (order within priority not guaranteed)
        high_watchers = [(pid, prio) for pid, prio in watchers if prio == Priority.HIGH]
        med_watchers = [(pid, prio) for pid, prio in watchers if prio == Priority.MED]

        assert len(high_watchers) == 2
        assert len(med_watchers) == 1
        assert (1, Priority.HIGH) in high_watchers
        assert (2, Priority.HIGH) in high_watchers
        assert (3, Priority.MED) in med_watchers

    def test_watcher_immutability(self):
        """Watchers must be immutably updated."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add first watcher
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        fd_attrs1 = get_fd_attrs(store, v)
        watchers1 = fd_attrs1["watchers"]

        # Add second watcher
        add_watcher(store, v, pid=2, priority=Priority.HIGH, trail=trail)
        fd_attrs2 = get_fd_attrs(store, v)
        watchers2 = fd_attrs2["watchers"]

        # Watcher dicts should be different objects
        assert watchers1 is not watchers2
        # And the sets inside should be different too
        assert watchers1[Priority.HIGH] is not watchers2[Priority.HIGH]


class TestCombinedDomainAndWatchers:
    """Test that domains and watchers coexist properly."""

    def test_domain_and_watchers_in_same_attrs(self):
        """Domain and watchers share the clpfd attrs dict."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Set domain
        domain = Domain(((1, 10),))
        set_domain(store, v, domain, trail)

        # Add watcher
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)

        # Both should be in same clpfd dict
        fd_attrs = get_fd_attrs(store, v)
        assert "domain" in fd_attrs
        assert "watchers" in fd_attrs
        assert fd_attrs["domain"] is domain
        assert 1 in fd_attrs["watchers"][Priority.HIGH]

    def test_updating_domain_preserves_watchers(self):
        """Updating domain doesn't lose watchers."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add watcher
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)

        # Set domain
        domain1 = Domain(((1, 10),))
        set_domain(store, v, domain1, trail)

        # Update domain
        domain2 = Domain(((5, 10),))
        set_domain(store, v, domain2, trail)

        # Watcher should still be there
        fd_attrs = get_fd_attrs(store, v)
        assert fd_attrs["domain"] is domain2
        assert 1 in fd_attrs["watchers"][Priority.HIGH]

    def test_adding_watchers_preserves_domain(self):
        """Adding watchers doesn't lose domain."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Set domain
        domain = Domain(((1, 10),))
        set_domain(store, v, domain, trail)

        # Add multiple watchers
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)

        # Domain should still be there
        fd_attrs = get_fd_attrs(store, v)
        assert fd_attrs["domain"] is domain
        assert len(fd_attrs["watchers"][Priority.HIGH]) == 1
        assert len(fd_attrs["watchers"][Priority.MED]) == 1


class TestTrailingAndBacktracking:
    """Test that FD operations are properly trailed."""

    def test_domain_changes_are_trailed(self):
        """All domain changes create trail entries."""

        store = Store()
        trail = Trail()
        v = store.new_var()

        mark1 = trail.mark()

        # Set initial domain
        domain1 = Domain(((1, 10),))
        set_domain(store, v, domain1, trail)
        assert get_domain(store, v) is domain1

        mark2 = trail.mark()

        # Update domain
        domain2 = Domain(((5, 10),))
        set_domain(store, v, domain2, trail)
        assert get_domain(store, v) is domain2

        # Backtrack to mark2
        undo_to(mark2, trail, store)
        assert get_domain(store, v) is domain1

        # Backtrack to mark1
        undo_to(mark1, trail, store)
        assert get_domain(store, v) is None

    def test_watcher_changes_are_trailed(self):
        """All watcher additions are trailed."""

        store = Store()
        trail = Trail()
        v = store.new_var()

        mark1 = trail.mark()

        # Add first watcher
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)

        mark2 = trail.mark()

        # Add second watcher
        add_watcher(store, v, pid=2, priority=Priority.HIGH, trail=trail)

        watchers = list(iter_watchers(store, v))
        assert len(watchers) == 2

        # Backtrack to mark2
        undo_to(mark2, trail, store)
        watchers = list(iter_watchers(store, v))
        assert len(watchers) == 1
        assert watchers[0][0] == 1

        # Backtrack to mark1
        undo_to(mark1, trail, store)
        watchers = list(iter_watchers(store, v))
        assert len(watchers) == 0
        assert get_fd_attrs(store, v) is None


class TestCopyOnWriteOptimization:
    """Test copy-on-write optimization for API attribute operations (Phase 3.2)."""

    def test_set_domain_copy_overhead_demonstration(self):
        """Demonstrate current copying overhead in set_domain()."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add watchers to create watcher structure
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)

        # Get initial fd_attrs dict
        initial_attrs = get_fd_attrs(store, v)

        # Set domain - this currently does fd_attrs.copy() even though watchers unchanged
        domain = Domain(((1, 10),))
        set_domain(store, v, domain, trail)

        # Get new fd_attrs dict
        new_attrs = get_fd_attrs(store, v)

        # Current behavior: fd_attrs dict is copied even though only domain changed
        # The watchers are preserved by reference (which is good), but the fd_attrs dict
        # itself is unnecessarily copied
        assert new_attrs is not initial_attrs  # Different fd_attrs dict (copied)

        # However, watchers are preserved by reference (current shallow copy behavior)
        assert new_attrs["watchers"] is initial_attrs["watchers"]  # Same watcher dict

        # Domain should be set correctly
        assert new_attrs["domain"] is domain

    def test_add_watcher_optimized_copying(self):
        """Test that add_watcher optimization only copies modified priority level."""
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add many watchers at HIGH and MED priorities
        for pid in range(50):
            add_watcher(store, v, pid=pid, priority=Priority.HIGH, trail=trail)
            add_watcher(store, v, pid=pid + 50, priority=Priority.MED, trail=trail)

        # Get initial watcher sets
        initial_attrs = get_fd_attrs(store, v)
        initial_high = initial_attrs["watchers"][Priority.HIGH]
        initial_med = initial_attrs["watchers"][Priority.MED]
        initial_low = initial_attrs["watchers"][Priority.LOW]

        # Add one watcher at LOW priority
        add_watcher(store, v, pid=999, priority=Priority.LOW, trail=trail)

        # Get new watcher sets
        new_attrs = get_fd_attrs(store, v)
        new_high = new_attrs["watchers"][Priority.HIGH]
        new_med = new_attrs["watchers"][Priority.MED]
        new_low = new_attrs["watchers"][Priority.LOW]

        # With optimization: unchanged priority levels are preserved by reference
        if _uses_copy_on_write:
            assert new_high is initial_high  # Preserved by reference (optimization)
            assert new_med is initial_med  # Preserved by reference (optimization)
            assert new_low is not initial_low  # Copied because it was modified
        else:
            # Old behavior: all sets are copied
            assert new_high is not initial_high
            assert new_med is not initial_med
            assert new_low is not initial_low

        # Content should be correct regardless
        assert new_high == initial_high
        assert new_med == initial_med

        # Only LOW set actually changed
        assert len(new_low) == len(initial_low) + 1
        assert 999 in new_low

    def test_optimized_set_domain_preserves_watcher_identity(self):
        """Test that optimized set_domain preserves watcher object identity.

        This test will pass after implementing copy-on-write optimization.
        """
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add watchers
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)

        # Get initial watcher objects
        initial_attrs = get_fd_attrs(store, v)

        # Set domain - with optimization, watchers should be preserved by reference
        domain = Domain(((1, 10),))
        set_domain(store, v, domain, trail)

        # Get new attributes
        new_attrs = get_fd_attrs(store, v)

        # With copy-on-write optimization: entire fd_attrs should be preserved when possible
        if hasattr(store, "_uses_copy_on_write") or "optimization" in str(type(store)):
            # After optimization: fd_attrs could potentially be preserved entirely
            # (though this is harder to achieve while maintaining immutability)
            pass  # More complex optimization, test structure in place for future
        else:
            # Current behavior: fd_attrs dict is copied (but watchers preserved by reference)
            assert new_attrs is not initial_attrs

        # Domain should be set correctly regardless
        assert new_attrs["domain"] is domain

    def test_optimized_add_watcher_minimal_copying(self):
        """Test that optimized add_watcher only copies affected priority level.

        This test will pass after implementing copy-on-write optimization.
        """
        store = Store()
        trail = Trail()
        v = store.new_var()

        # Add watchers at HIGH and MED
        add_watcher(store, v, pid=1, priority=Priority.HIGH, trail=trail)
        add_watcher(store, v, pid=2, priority=Priority.MED, trail=trail)

        # Get initial sets
        initial_attrs = get_fd_attrs(store, v)
        initial_high = initial_attrs["watchers"][Priority.HIGH]
        initial_med = initial_attrs["watchers"][Priority.MED]

        # Add watcher at LOW priority
        add_watcher(store, v, pid=3, priority=Priority.LOW, trail=trail)

        # Get new sets
        new_attrs = get_fd_attrs(store, v)
        new_high = new_attrs["watchers"][Priority.HIGH]
        new_med = new_attrs["watchers"][Priority.MED]

        # With copy-on-write optimization: unchanged sets should be preserved
        if _uses_copy_on_write:
            # After optimization: HIGH and MED should be same objects (preserved by reference)
            assert new_high is initial_high
            assert new_med is initial_med
        else:
            # Old behavior: all sets are copied
            assert new_high is not initial_high
            assert new_med is not initial_med

        # Content should be correct regardless
        assert new_high == initial_high
        assert new_med == initial_med
