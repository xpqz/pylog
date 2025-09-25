"""Tests for CLP(FD) engine integration.

Tests unification with FD variables, constraint posting during
unification, and backtracking behavior.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine, Program
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_fd_eq, _builtin_fd_lt
from prolog.clpfd.api import get_domain


class TestUnificationIntegration:
    """Test unification with FD variables."""

    def test_unify_fd_var_with_int(self):
        """Unifying FD variable with integer should check domain."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 5..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        # Unify X with 7 (in domain)
        result = engine.unify(x, Int(7))
        assert result is True

        # X should be bound to 7
        _, _, value = store.deref(x.id)
        assert value == Int(7)

    def test_unify_fd_var_with_int_outside_domain(self):
        """Unifying FD variable with integer outside domain should fail."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 5..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        # Try to unify X with 3 (outside domain)
        result = engine.unify(x, Int(3))
        assert result is False

    def test_unify_two_fd_vars(self):
        """Unifying two FD variables should intersect domains."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 1..10, Y in 5..15
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # Unify X with Y
        result = engine.unify(x, y)
        assert result is True

        # Both should have domain 5..10 (intersection)
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((5, 10),)

        # After unification, one variable should point to the other
        # Check that they're unified (dereferencing Y should give us X's root)
        x_deref = store.deref(x.id)
        y_deref = store.deref(y.id)
        # Both should deref to the same root variable
        assert x_deref[1] == y_deref[1]

    def test_unify_fd_var_with_non_int(self):
        """Unifying FD variable with non-integer should fail."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 5..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        # Try to unify X with atom
        result = engine.unify(x, Atom("foo"))
        assert result is False

    def test_unify_with_constraint_propagation(self):
        """Unification should trigger constraint propagation."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set up domains and constraints
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(10))))

        # X #< Y
        _builtin_fd_lt(engine, x, y)

        # Unify Y with Z
        result = engine.unify(y, z)
        assert result is True

        # Now X #< Z should hold transitively
        # Check that domains are consistent
        x_dom = get_domain(store, x.id)
        z_dom = get_domain(store, z.id)
        assert x_dom.max() < z_dom.max()


class TestBacktrackingIntegration:
    """Test backtracking with CLP(FD) constraints."""

    def test_domain_restoration_on_backtrack(self):
        """Domains should be restored on backtracking."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # Save trail position
        mark = trail.position()

        # Narrow domain with constraint
        _builtin_fd_lt(engine, x, Int(5))

        # Check narrowed domain
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((1, 4),)

        # Backtrack
        trail.unwind_to(mark, store)

        # Domain should be restored
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((1, 10),)

    def test_constraint_removal_on_backtrack(self):
        """Constraints should be removed on backtracking."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))

        # Save trail position
        mark = trail.position()

        # Post constraint
        _builtin_fd_lt(engine, x, y)

        # Check that constraint affected domains
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((1, 9),)
        assert y_dom.intervals == ((2, 10),)

        # Backtrack
        trail.unwind_to(mark, store)

        # Domains should be restored
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((1, 10),)
        assert y_dom.intervals == ((1, 10),)

        # Post a different constraint - should work independently
        _builtin_fd_eq(engine, x, Int(5))
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((5, 5),)


class TestFDInspectionBuiltins:
    """Test FD variable inspection builtins."""

    def test_fd_var_builtin(self):
        """fd_var/1 should succeed for FD variables."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X has domain, Y doesn't
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        from prolog.engine.builtins_clpfd import _builtin_fd_var

        # X is FD variable
        assert _builtin_fd_var(engine, x) is True

        # Y is not FD variable
        assert _builtin_fd_var(engine, y) is False

        # Integer is not FD variable
        assert _builtin_fd_var(engine, Int(5)) is False

    def test_fd_inf_builtin(self):
        """fd_inf/2 should unify with domain minimum."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        inf = Var(store.new_var(), "Inf")

        # X in 5..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        from prolog.engine.builtins_clpfd import _builtin_fd_inf

        # fd_inf(X, Inf) should unify Inf with 5
        result = _builtin_fd_inf(engine, x, inf)
        assert result is True

        _, _, value = store.deref(inf.id)
        assert value == Int(5)

    def test_fd_sup_builtin(self):
        """fd_sup/2 should unify with domain maximum."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        sup = Var(store.new_var(), "Sup")

        # X in 5..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        from prolog.engine.builtins_clpfd import _builtin_fd_sup

        # fd_sup(X, Sup) should unify Sup with 10
        result = _builtin_fd_sup(engine, x, sup)
        assert result is True

        _, _, value = store.deref(sup.id)
        assert value == Int(10)

    def test_fd_dom_builtin(self):
        """fd_dom/2 should unify with domain representation."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        dom = Var(store.new_var(), "Dom")

        # X in 5..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        from prolog.engine.builtins_clpfd import _builtin_fd_dom

        # fd_dom(X, Dom) should unify Dom with domain term
        result = _builtin_fd_dom(engine, x, dom)
        assert result is True

        _, _, value = store.deref(dom.id)
        # Domain should be represented as 5..10
        assert isinstance(value, Struct)
        assert value.functor == ".."
        assert value.args == (Int(5), Int(10))


class TestConstraintInteraction:
    """Test interaction between constraints and unification."""

    def test_equality_constraint_forces_unification(self):
        """X #= Y should make variables equivalent."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # X #= Y
        result = _builtin_fd_eq(engine, x, y)
        assert result is True

        # Domains should be intersected
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((5, 10),)
        assert y_dom.intervals == ((5, 10),)

    def test_constraint_with_subsequent_unification(self):
        """Constraints should be maintained through unification."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(20))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(20))))
        _builtin_in(engine, z, Struct("..", (Int(10), Int(30))))

        # X #< Y
        _builtin_fd_lt(engine, x, y)

        # Unify Y with Z (should propagate constraints)
        result = engine.unify(y, z)
        assert result is True

        # Check final domains
        x_dom = get_domain(store, x.id)
        # X must be less than Y/Z, and Y/Z is in 10..20
        # So X can be at most 19
        assert x_dom.max() <= 19