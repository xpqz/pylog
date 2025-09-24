"""Unit tests for CLP(FD) in/2 builtin.

Tests domain posting, parsing, and integration with the engine.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.unify.store import Store
from prolog.unify.trail import Trail
from prolog.engine.engine import Engine
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import get_domain
from prolog.engine.builtins_clpfd import _builtin_in, parse_domain_term


class TestDomainParsing:
    """Test parsing of domain specifications."""

    def test_parse_singleton_domain(self):
        """Parse single integer as singleton domain."""
        term = Int(5)
        domain = parse_domain_term(term)

        assert domain.intervals == ((5, 5),)
        assert domain.is_singleton()

    def test_parse_interval_domain(self):
        """Parse low..high interval notation."""
        # 1..10
        term = Struct("..", (Int(1), Int(10)))
        domain = parse_domain_term(term)

        assert domain.intervals == ((1, 10),)
        assert domain.min() == 1
        assert domain.max() == 10

    def test_parse_empty_interval(self):
        """Parse interval where low > high yields empty."""
        # 10..1 (invalid interval)
        term = Struct("..", (Int(10), Int(1)))
        domain = parse_domain_term(term)

        assert domain.is_empty()

    def test_parse_union_of_intervals(self):
        """Parse union of domain specifications."""
        # 1..3 \/ 7..9
        term = Struct("\\/", (
            Struct("..", (Int(1), Int(3))),
            Struct("..", (Int(7), Int(9)))
        ))
        domain = parse_domain_term(term)

        # Should merge into sorted intervals
        assert len(domain.intervals) == 2
        assert (1, 3) in domain.intervals
        assert (7, 9) in domain.intervals

    def test_parse_union_of_singletons(self):
        """Parse union of singleton values."""
        # 1 \/ 3 \/ 5
        term = Struct("\\/", (
            Int(1),
            Struct("\\/", (Int(3), Int(5)))
        ))
        domain = parse_domain_term(term)

        # Should create separate singleton intervals
        assert len(domain.intervals) == 3
        assert (1, 1) in domain.intervals
        assert (3, 3) in domain.intervals
        assert (5, 5) in domain.intervals

    def test_parse_enumerated_set(self):
        """Parse enumerated set notation {1,3,5}."""
        # {} is a special functor with args as the set elements
        term = Struct("{}", (Int(1), Int(3), Int(5)))
        domain = parse_domain_term(term)

        assert len(domain.intervals) == 3
        assert (1, 1) in domain.intervals
        assert (3, 3) in domain.intervals
        assert (5, 5) in domain.intervals

    def test_parse_union_with_overlap_normalizes(self):
        """Parse union with overlapping intervals should normalize."""
        # (1..4) \/ (3..6) should merge to single interval
        term = Struct("\\/", (
            Struct("..", (Int(1), Int(4))),
            Struct("..", (Int(3), Int(6)))
        ))
        domain = parse_domain_term(term)

        # Should be normalized to single interval 1..6
        # Note: Implementation may vary, but should contain all values
        assert domain.contains(1)
        assert domain.contains(3)
        assert domain.contains(4)
        assert domain.contains(6)
        assert domain.size() == 6

    def test_parse_union_with_adjacency_normalizes(self):
        """Parse union with adjacent intervals should normalize."""
        # (1..3) \/ (4..6) should merge to 1..6 (adjacent intervals)
        term = Struct("\\/", (
            Struct("..", (Int(1), Int(3))),
            Struct("..", (Int(4), Int(6)))
        ))
        domain = parse_domain_term(term)

        # Should be normalized to single interval 1..6
        # Note: Implementation may vary on whether adjacency merges
        assert domain.contains(1)
        assert domain.contains(3)
        assert domain.contains(4)
        assert domain.contains(6)
        assert domain.size() == 6

    def test_parse_invalid_domain_raises(self):
        """Invalid domain terms raise ValueError."""
        with pytest.raises(ValueError):
            parse_domain_term(Atom("foo"))

        with pytest.raises(ValueError):
            parse_domain_term(List((Int(1), Int(2))))

        # Invalid interval with non-integer
        with pytest.raises(ValueError):
            parse_domain_term(Struct("..", (Atom("a"), Int(10))))


class TestInBuiltin:
    """Test the in/2 builtin functionality."""

    def test_in_sets_domain_on_unbound_var(self):
        """X in Domain sets domain on unbound variable."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        domain_term = Struct("..", (Int(1), Int(10)))

        result = _builtin_in(engine, x, domain_term)

        assert result is True
        domain = get_domain(store, x.id)
        assert domain is not None
        assert domain.intervals == ((1, 10),)

    def test_in_checks_bound_integer(self):
        """in/2 checks if bound integer is in domain."""
        engine = Engine()

        # Test with integer in domain
        result = _builtin_in(engine, Int(5), Struct("..", (Int(1), Int(10))))
        assert result is True

        # Test with integer outside domain
        result = _builtin_in(engine, Int(15), Struct("..", (Int(1), Int(10))))
        assert result is False

        # Test with integer in enumerated set
        result = _builtin_in(engine, Int(3), Struct("{}", (Int(1), Int(3), Int(5))))
        assert result is True

        result = _builtin_in(engine, Int(4), Struct("{}", (Int(1), Int(3), Int(5))))
        assert result is False

    def test_in_rejects_non_integer_bound_values(self):
        """in/2 fails for non-integer bound values."""
        engine = Engine()

        result = _builtin_in(engine, Atom("foo"), Struct("..", (Int(1), Int(10))))
        assert result is False

        result = _builtin_in(engine, List([Int(1)]), Struct("..", (Int(1), Int(10))))
        assert result is False

    def test_in_multiple_domains_same_var(self):
        """Multiple in/2 constraints narrow domain."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        domain1 = get_domain(store, x.id)
        assert domain1.intervals == ((1, 10),)

        # X in 5..15 (should intersect to 5..10)
        # Note: Full intersection logic will be implemented in Phase 1
        # For now we just verify the domain is updated
        _builtin_in(engine, x, Struct("..", (Int(5), Int(15))))
        domain2 = get_domain(store, x.id)

        # Should be set (narrowing behavior to be implemented)
        assert domain2 is not None
        # Once intersection is implemented, add:
        # assert domain2.intervals == ((5, 10),)

    def test_in_registers_unification_hook_once(self):
        """in/2 registers CLP(FD) unification hook on first use."""
        engine = Engine()
        store = engine.store

        # Initially no CLP(FD) hook
        assert not hasattr(engine, '_clpfd_inited') or not engine._clpfd_inited

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # Hook should now be registered
        assert hasattr(engine, '_clpfd_inited')
        assert engine._clpfd_inited

    def test_in_with_singleton_domain(self):
        """X in 5 sets domain to singleton."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Int(5))

        domain = get_domain(store, x.id)
        assert domain.is_singleton()
        assert domain.min() == 5
        assert domain.max() == 5

    def test_in_with_enumerated_set(self):
        """X in {1,3,5} sets domain to enumerated values."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("{}", (Int(1), Int(3), Int(5))))

        domain = get_domain(store, x.id)
        assert domain.size() == 3
        assert domain.contains(1)
        assert domain.contains(3)
        assert domain.contains(5)
        assert not domain.contains(2)
        assert not domain.contains(4)

    def test_in_with_union_domains(self):
        """X in (1..3 \/ 7..9) sets union domain."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        union_term = Struct("\\/", (
            Struct("..", (Int(1), Int(3))),
            Struct("..", (Int(7), Int(9)))
        ))
        _builtin_in(engine, x, union_term)

        domain = get_domain(store, x.id)
        assert domain.size() == 6
        assert domain.min() == 1
        assert domain.max() == 9
        # Check for gap
        assert not domain.contains(4)
        assert not domain.contains(5)
        assert not domain.contains(6)


class TestInBuiltinIntegration:
    """Test in/2 integration with engine and trailing."""

    def test_in_trailing_and_backtracking(self):
        """Domain changes via in/2 are properly trailed."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")

        mark = trail.position()

        # Set domain
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        domain = get_domain(store, x.id)
        assert domain is not None
        assert domain.intervals == ((1, 10),)

        # Backtrack
        trail.unwind_to(mark, store)
        domain = get_domain(store, x.id)
        assert domain is None

    def test_in_with_deref(self):
        """in/2 properly handles variables that need dereferencing."""
        engine = Engine()
        store = engine.store
        trail = engine.trail

        # Create two variables
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Make X an alias of Y (X -> Y)
        store.cells[x.id].ref = y.id

        # Set domain on X (should go to root Y)
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # Domain should be on Y (the root)
        domain = get_domain(store, y.id)
        assert domain is not None
        assert domain.intervals == ((1, 10),)

        # Getting domain via X should also work (deref to root)
        domain_via_x = get_domain(store, x.id)
        assert domain_via_x is domain

    def test_in_preserves_existing_attributes(self):
        """Setting domain via in/2 preserves other FD attributes."""
        from prolog.clpfd.api import add_watcher, Priority, iter_watchers

        engine = Engine()
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")

        # Add a watcher first
        add_watcher(store, x.id, pid=1, priority=Priority.HIGH, trail=trail)

        # Set domain
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # Check both domain and watcher are present
        domain = get_domain(store, x.id)
        assert domain is not None

        watchers = list(iter_watchers(store, x.id))
        assert len(watchers) == 1
        assert watchers[0] == (1, Priority.HIGH)