"""Comprehensive unit tests for CLP(FD) Boolean domain utilities.

Tests cover:
- Boolean domain detection
- Variable constraining to Boolean domains
- Boolean value extraction from bound and domain-constrained variables
- Edge cases: empty domains, invalid domains, already-Boolean domains
- Integration with existing domain operations
- Backtracking behavior with Boolean domain changes
"""

from prolog.unify.store import Store
from prolog.unify.trail import Trail, undo_to
from prolog.unify.unify import bind
from prolog.ast.terms import Int
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import get_domain, set_domain


class TestBooleanDomainDetection:
    """Test is_boolean_domain() function."""

    def test_empty_domain_not_boolean(self):
        """Empty domain is not considered Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        empty_dom = Domain(())
        assert not is_boolean_domain(empty_dom)

    def test_none_domain_not_boolean(self):
        """None domain is not considered Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        assert not is_boolean_domain(None)

    def test_zero_only_is_boolean(self):
        """Domain containing only 0 is Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        zero_dom = Domain(((0, 0),))
        assert is_boolean_domain(zero_dom)

    def test_one_only_is_boolean(self):
        """Domain containing only 1 is Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        one_dom = Domain(((1, 1),))
        assert is_boolean_domain(one_dom)

    def test_zero_one_is_boolean(self):
        """Domain {0, 1} is Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        bool_dom = Domain(((0, 1),))
        assert is_boolean_domain(bool_dom)

    def test_negative_values_not_boolean(self):
        """Domain with negative values is not Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        neg_dom = Domain(((-1, 1),))
        assert not is_boolean_domain(neg_dom)

    def test_large_values_not_boolean(self):
        """Domain with values > 1 is not Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        large_dom = Domain(((0, 2),))
        assert not is_boolean_domain(large_dom)

        large_dom2 = Domain(((1, 5),))
        assert not is_boolean_domain(large_dom2)

    def test_disjoint_intervals_in_boolean_range(self):
        """Multiple intervals within {0, 1} are still Boolean."""
        from prolog.clpfd.boolean import is_boolean_domain

        # This will normalize to ((0, 0), (1, 1))
        disjoint_dom = Domain(((0, 0), (1, 1)))
        assert is_boolean_domain(disjoint_dom)


class TestEnsureBooleanVar:
    """Test ensure_boolean_var() function."""

    def test_unconstrained_var_becomes_boolean(self):
        """Variable with no domain gets Boolean domain."""
        from prolog.clpfd.boolean import ensure_boolean_var, BOOL_DOMAIN

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Initially no domain
        assert get_domain(store, varid) is None

        # Ensure Boolean
        result = ensure_boolean_var(store, varid, trail)
        assert result is True

        # Check domain is now Boolean
        dom = get_domain(store, varid)
        assert dom == BOOL_DOMAIN

    def test_already_boolean_var_unchanged(self):
        """Variable already with Boolean domain stays unchanged."""
        from prolog.clpfd.boolean import ensure_boolean_var, BOOL_DOMAIN

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set Boolean domain
        set_domain(store, varid, BOOL_DOMAIN, trail)
        initial_dom = get_domain(store, varid)

        # Ensure Boolean
        result = ensure_boolean_var(store, varid, trail)
        assert result is True

        # Domain should be unchanged (same object)
        dom = get_domain(store, varid)
        assert dom is initial_dom

    def test_compatible_domain_narrows_to_boolean(self):
        """Variable with larger domain narrows to Boolean."""
        from prolog.clpfd.boolean import ensure_boolean_var

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain 0..5
        set_domain(store, varid, Domain(((0, 5),)), trail)

        # Ensure Boolean
        result = ensure_boolean_var(store, varid, trail)
        assert result is True

        # Domain should be narrowed to {0, 1}
        dom = get_domain(store, varid)
        assert dom.min() == 0
        assert dom.max() == 1
        assert dom.size() == 2

    def test_partially_compatible_domain(self):
        """Variable with domain {1, 2, 3} narrows to {1}."""
        from prolog.clpfd.boolean import ensure_boolean_var

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain 1..3
        set_domain(store, varid, Domain(((1, 3),)), trail)

        # Ensure Boolean
        result = ensure_boolean_var(store, varid, trail)
        assert result is True

        # Domain should be narrowed to {1}
        dom = get_domain(store, varid)
        assert dom.is_singleton()
        assert dom.min() == 1

    def test_incompatible_domain_fails(self):
        """Variable with incompatible domain returns False."""
        from prolog.clpfd.boolean import ensure_boolean_var

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain 2..5 (no overlap with {0, 1})
        set_domain(store, varid, Domain(((2, 5),)), trail)

        # Ensure Boolean should fail
        result = ensure_boolean_var(store, varid, trail)
        assert result is False

    def test_backtracking_restores_domain(self):
        """Backtracking restores original domain."""
        from prolog.clpfd.boolean import ensure_boolean_var

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain 0..5
        original_dom = Domain(((0, 5),))
        set_domain(store, varid, original_dom, trail)
        trail_mark = trail.mark()

        # Ensure Boolean
        result = ensure_boolean_var(store, varid, trail)
        assert result is True
        assert get_domain(store, varid).max() == 1

        # Backtrack
        undo_to(trail_mark, trail, store)

        # Domain should be restored
        dom = get_domain(store, varid)
        assert dom == original_dom


class TestGetBooleanValue:
    """Test get_boolean_value() function."""

    def test_bound_to_zero(self):
        """Variable bound to 0 returns 0."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Bind to 0
        bind(store, varid, Int(0), trail)

        value = get_boolean_value(store, varid)
        assert value == 0

    def test_bound_to_one(self):
        """Variable bound to 1 returns 1."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Bind to 1
        bind(store, varid, Int(1), trail)

        value = get_boolean_value(store, varid)
        assert value == 1

    def test_bound_to_non_boolean(self):
        """Variable bound to non-Boolean value returns None."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Bind to 5
        bind(store, varid, Int(5), trail)

        value = get_boolean_value(store, varid)
        assert value is None

    def test_domain_singleton_zero(self):
        """Variable with singleton domain {0} returns 0."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain to {0}
        set_domain(store, varid, Domain(((0, 0),)), trail)

        value = get_boolean_value(store, varid)
        assert value == 0

    def test_domain_singleton_one(self):
        """Variable with singleton domain {1} returns 1."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain to {1}
        set_domain(store, varid, Domain(((1, 1),)), trail)

        value = get_boolean_value(store, varid)
        assert value == 1

    def test_domain_boolean_undetermined(self):
        """Variable with domain {0, 1} returns None."""
        from prolog.clpfd.boolean import get_boolean_value, BOOL_DOMAIN

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set full Boolean domain
        set_domain(store, varid, BOOL_DOMAIN, trail)

        value = get_boolean_value(store, varid)
        assert value is None

    def test_no_domain_returns_none(self):
        """Variable with no domain returns None."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        varid = store.new_var("X")

        value = get_boolean_value(store, varid)
        assert value is None

    def test_domain_singleton_non_boolean(self):
        """Variable with singleton domain {5} returns None."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set domain to {5}
        set_domain(store, varid, Domain(((5, 5),)), trail)

        value = get_boolean_value(store, varid)
        assert value is None

    def test_empty_domain_returns_none(self):
        """Variable with empty domain returns None."""
        from prolog.clpfd.boolean import get_boolean_value

        store = Store()
        trail = Trail()
        varid = store.new_var("X")

        # Set empty domain
        set_domain(store, varid, Domain(()), trail)

        value = get_boolean_value(store, varid)
        assert value is None


class TestBooleanDomainConstant:
    """Test BOOL_DOMAIN constant."""

    def test_bool_domain_values(self):
        """BOOL_DOMAIN contains exactly {0, 1}."""
        from prolog.clpfd.boolean import BOOL_DOMAIN

        assert BOOL_DOMAIN.contains(0)
        assert BOOL_DOMAIN.contains(1)
        assert not BOOL_DOMAIN.contains(-1)
        assert not BOOL_DOMAIN.contains(2)
        assert BOOL_DOMAIN.size() == 2
        assert BOOL_DOMAIN.min() == 0
        assert BOOL_DOMAIN.max() == 1

    def test_bool_domain_immutable(self):
        """BOOL_DOMAIN is immutable."""
        from prolog.clpfd.boolean import BOOL_DOMAIN

        # Attempting operations that would mutate return new domains
        new_dom = BOOL_DOMAIN.remove_value(0)
        assert new_dom != BOOL_DOMAIN
        assert BOOL_DOMAIN.contains(0)  # Original unchanged


class TestIntegrationWithDomainOperations:
    """Test integration with existing domain operations."""

    def test_intersection_with_boolean(self):
        """Intersection with Boolean domain works correctly."""
        from prolog.clpfd.boolean import BOOL_DOMAIN

        # Larger domain intersected with Boolean
        large_dom = Domain(((0, 10),))
        result = large_dom.intersect(BOOL_DOMAIN)
        assert result.min() == 0
        assert result.max() == 1
        assert result.size() == 2

    def test_boolean_after_narrowing(self):
        """Domain becomes Boolean after narrowing operations."""
        from prolog.clpfd.boolean import is_boolean_domain

        # Start with 0..5
        dom = Domain(((0, 5),))
        assert not is_boolean_domain(dom)

        # Remove values to leave only {0, 1}
        dom = dom.remove_gt(1)
        assert is_boolean_domain(dom)

    def test_boolean_domain_with_holes(self):
        """Boolean detection works with domains that have holes."""
        from prolog.clpfd.boolean import is_boolean_domain

        # Domain with values removed
        dom = Domain(((0, 5),))
        dom = dom.remove_value(2)
        dom = dom.remove_value(3)
        dom = dom.remove_value(4)
        dom = dom.remove_value(5)
        # Now domain is {0, 1}
        assert is_boolean_domain(dom)
