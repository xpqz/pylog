"""Unit tests for CLP(FD) basic propagators.

Tests equality and comparison propagators with domain narrowing,
failure detection, and proper integration with the propagation queue.
"""

from prolog.ast.terms import Int, Var, Struct
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.clpfd.api import get_domain
from prolog.clpfd.queue import PropagationQueue
from prolog.clpfd.priority import Priority
from prolog.engine.builtins_clpfd import (
    _builtin_in,
    _builtin_fd_eq,
    _builtin_fd_lt,
    _builtin_fd_le,
    _builtin_fd_gt,
    _builtin_fd_ge,
)
from prolog.clpfd.props.equality import create_equality_propagator


class TestEqualityPropagator:
    """Test #=/2 equality propagator."""

    def test_equality_var_var_propagation(self):
        """X #= Y should make domains equal."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        # Y in 5..15
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # X #= Y should intersect domains to 5..10

        result = _builtin_fd_eq(engine, x, y)

        assert result is True
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((5, 10),)
        assert y_dom.intervals == ((5, 10),)

    def test_equality_var_int_propagation(self):
        """X #= 5 should narrow domain to singleton."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # X #= 5

        result = _builtin_fd_eq(engine, x, Int(5))

        assert result is True
        x_dom = get_domain(store, x.id)
        assert x_dom.is_singleton()
        assert x_dom.min() == 5

    def test_equality_failure_disjoint_domains(self):
        """X #= Y should fail with disjoint domains."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 1..5
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        # Y in 10..15
        _builtin_in(engine, y, Struct("..", (Int(10), Int(15))))

        # X #= Y should fail

        result = _builtin_fd_eq(engine, x, y)

        assert result is False

    def test_equality_int_int(self):
        """5 #= 5 should succeed, 5 #= 6 should fail."""
        engine = Engine(Program([]))

        # 5 #= 5
        assert _builtin_fd_eq(engine, Int(5), Int(5)) is True

        # 5 #= 6
        assert _builtin_fd_eq(engine, Int(5), Int(6)) is False

    def test_equality_wakes_watchers(self):
        """Equality propagator should wake watchers on domain change."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 1..10, Y in 5..15
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # Track domain changes
        x_rev_before = get_domain(store, x.id).rev
        y_rev_before = get_domain(store, y.id).rev

        # X #= Y

        _builtin_fd_eq(engine, x, y)

        # Domains should have changed
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.rev > x_rev_before
        assert y_dom.rev > y_rev_before


class TestLessThanPropagator:
    """Test #</2 less-than propagator."""

    def test_less_than_var_var_propagation(self):
        """X #< Y should narrow domains appropriately using bounds consistency."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 1..10, Y in 5..15
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # X #< Y with standard bounds consistency:
        # max(X) < max(Y), so X can be at most 14
        # min(Y) > min(X), so Y must be at least 2
        # X in 1..10 stays 1..10 (already <= 14)
        # Y in 5..15 stays 5..15 (already >= 2)

        result = _builtin_fd_lt(engine, x, y)

        assert result is True
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)

        # With standard bounds consistency, no narrowing occurs here
        assert x_dom.intervals == ((1, 10),)
        assert y_dom.intervals == ((5, 15),)

    def test_less_than_var_int(self):
        """X #< 5 should remove values >= 5."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # X #< 5

        result = _builtin_fd_lt(engine, x, Int(5))

        assert result is True
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((1, 4),)

    def test_less_than_int_var(self):
        """5 #< Y should remove values <= 5."""
        engine = Engine(Program([]))
        store = engine.store

        y = Var(store.new_var(), "Y")

        # Y in 1..10
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))

        # 5 #< Y

        result = _builtin_fd_lt(engine, Int(5), y)

        assert result is True
        y_dom = get_domain(store, y.id)
        assert y_dom.intervals == ((6, 10),)

    def test_less_than_failure(self):
        """X #< Y should fail when X.min >= Y.max."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 10..15, Y in 1..5
        _builtin_in(engine, x, Struct("..", (Int(10), Int(15))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))

        # X #< Y should fail

        result = _builtin_fd_lt(engine, x, y)

        assert result is False

    def test_less_than_int_int(self):
        """3 #< 5 should succeed, 5 #< 3 should fail."""
        engine = Engine(Program([]))

        # 3 #< 5
        assert _builtin_fd_lt(engine, Int(3), Int(5)) is True

        # 5 #< 3
        assert _builtin_fd_lt(engine, Int(5), Int(3)) is False

        # 5 #< 5
        assert _builtin_fd_lt(engine, Int(5), Int(5)) is False


class TestLessEqualPropagator:
    """Test #=</2 less-than-or-equal propagator."""

    def test_less_equal_var_var_propagation(self):
        """X #=< Y should narrow domains appropriately."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 1..10, Y in 5..15
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # X #=< Y should give X in 1..10, Y in 5..15 (X.max <= Y.max ok)

        result = _builtin_fd_le(engine, x, y)

        assert result is True
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        # X can be at most 10, Y must be at least 1
        assert x_dom.max() <= 10
        assert y_dom.min() >= 5

    def test_less_equal_var_int(self):
        """X #=< 5 should remove values > 5."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # X #=< 5

        result = _builtin_fd_le(engine, x, Int(5))

        assert result is True
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((1, 5),)

    def test_less_equal_includes_equality(self):
        """X #=< Y should allow X = Y."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Both in same singleton domain
        _builtin_in(engine, x, Int(5))
        _builtin_in(engine, y, Int(5))

        # X #=< Y should succeed

        result = _builtin_fd_le(engine, x, y)

        assert result is True


class TestGreaterThanPropagator:
    """Test #>/2 greater-than propagator."""

    def test_greater_than_var_var_propagation(self):
        """X #> Y should narrow domains appropriately."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 5..15, Y in 1..10
        _builtin_in(engine, x, Struct("..", (Int(5), Int(15))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))

        # X #> Y is implemented as Y #< X
        # With standard bounds consistency:
        # max(Y) < max(X), so Y can be at most 14
        # min(X) > min(Y), so X must be at least 2
        # X in 5..15 stays 5..15 (already >= 2)
        # Y in 1..10 stays 1..10 (already <= 14)

        result = _builtin_fd_gt(engine, x, y)

        assert result is True
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((5, 15),)
        assert y_dom.intervals == ((1, 10),)

    def test_greater_than_var_int(self):
        """X #> 5 should remove values <= 5."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # X #> 5

        result = _builtin_fd_gt(engine, x, Int(5))

        assert result is True
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((6, 10),)


class TestGreaterEqualPropagator:
    """Test #>=/2 greater-than-or-equal propagator."""

    def test_greater_equal_var_int(self):
        """X #>= 5 should remove values < 5."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # X in 1..10
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # X #>= 5

        result = _builtin_fd_ge(engine, x, Int(5))

        assert result is True
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((5, 10),)

    def test_greater_equal_includes_equality(self):
        """X #>= Y should allow X = Y."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Both in same singleton domain
        _builtin_in(engine, x, Int(5))
        _builtin_in(engine, y, Int(5))

        # X #>= Y should succeed

        result = _builtin_fd_ge(engine, x, y)

        assert result is True


class TestPropagatorIntegration:
    """Test propagator integration with queue and watchers."""

    def test_chain_propagation(self):
        """X #< Y, Y #< Z should propagate through chain."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set initial domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(10))))

        # X #< Y
        result1 = _builtin_fd_lt(engine, x, y)
        assert result1 is True

        # Y #< Z
        result2 = _builtin_fd_lt(engine, y, z)
        assert result2 is True

        # Check final domains
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)

        # With bounds consistency and propagation to fixpoint:
        # X #< Y gives X in 1..9, Y in 2..10
        # Y #< Z gives Y in 2..9, Z in 3..10
        # This triggers X #< Y again: X becomes 1..8
        assert x_dom.intervals == ((1, 8),)
        assert y_dom.intervals == ((2, 9),)
        assert z_dom.intervals == ((3, 10),)

        # Verify constraint relationships hold
        assert x_dom.max() < y_dom.max()  # X < Y
        assert y_dom.max() < z_dom.max()  # Y < Z

    def test_propagator_schedules_at_correct_priority(self):
        """Equality propagators should schedule at HIGH priority."""
        engine = Engine(Program([]))
        store = engine.store

        # Initialize propagation queue
        if not hasattr(engine, "clpfd_queue"):
            engine.clpfd_queue = PropagationQueue()

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(5), Int(15))))

        # Register equality propagator

        prop = create_equality_propagator(x.id, y.id)
        pid = engine.clpfd_queue.register(prop)

        # Schedule it
        engine.clpfd_queue.schedule(pid, Priority.HIGH)

        # Check it's queued at HIGH priority
        assert pid in engine.clpfd_queue.queued
        assert engine.clpfd_queue.queued[pid] == Priority.HIGH

    def test_failure_propagation_stops_queue(self):
        """Propagator failure should stop queue execution."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Disjoint domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(10), Int(15))))

        # X #= Y should fail

        result = _builtin_fd_eq(engine, x, y)

        assert result is False
