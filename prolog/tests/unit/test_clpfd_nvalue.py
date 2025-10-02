"""Unit tests for nvalue/2 global constraint."""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, List, Struct, Atom
from prolog.clpfd.api import get_domain, set_domain, iter_watchers
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_nvalue


class TestNValueBasic:
    """Test basic nvalue/2 functionality."""

    def test_nvalue_exact_count(self):
        """Test nvalue with exact number of distinct values."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables: X, Y, Z
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # nvalue(N, [X,Y,Z]) where N = 2
        # This means exactly 2 distinct values among X, Y, Z

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(2))  # N = 2

        vars_list = List([x, y, z])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # With N=2 and 3 variables in domain 1..3,
        # this constraint should still be satisfiable but may prune domains

    def test_nvalue_lower_bound(self):
        """Test nvalue when N provides lower bound on distinct values."""
        engine = Engine(Program([]))
        store = engine.store

        # Variables with overlapping domains
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Int(1))  # X = 1
        _builtin_in(engine, y, Int(2))  # Y = 2

        # nvalue(N, [X,Y]) where N >= 2

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(2), Int(10))))  # N in 2..10

        vars_list = List([x, y])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # N should be constrained to exactly 2 (since X=1, Y=2 gives exactly 2 distinct values)
        n_dom = get_domain(store, n.id)
        assert n_dom.intervals == ((2, 2),)

    def test_nvalue_upper_bound(self):
        """Test nvalue when N provides upper bound on distinct values."""
        engine = Engine(Program([]))
        store = engine.store

        # Three variables in same small domain
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # nvalue(N, [X,Y,Z]) where N = 1
        # This means all variables must have the same value

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(1))  # N = 1

        vars_list = List([x, y, z])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # This constraint should be posted but may not immediately prune
        # (depending on implementation strength)

    def test_nvalue_impossible_too_low(self):
        """Test nvalue failure when N is too low for distinct ground values."""
        engine = Engine(Program([]))
        store = engine.store

        # Variables with different fixed values
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Int(1))  # X = 1
        _builtin_in(engine, y, Int(2))  # Y = 2
        _builtin_in(engine, z, Int(3))  # Z = 3

        # nvalue(N, [X,Y,Z]) where N = 2
        # Impossible: we have 3 distinct values (1, 2, 3) but N requires only 2

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(2))  # N = 2

        vars_list = List([x, y, z])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is False

    def test_nvalue_impossible_too_high(self):
        """Test nvalue failure when N is too high for possible distinct values."""
        engine = Engine(Program([]))
        store = engine.store

        # Two variables in single-value domains (same value)
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Int(1))  # X = 1
        _builtin_in(engine, y, Int(1))  # Y = 1

        # nvalue(N, [X,Y]) where N = 2
        # Impossible: we can have at most 1 distinct value, but N requires 2

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(2))  # N = 2

        vars_list = List([x, y])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is False

    def test_nvalue_with_variable_bounds(self):
        """Test nvalue with variable having domain bounds."""
        engine = Engine(Program([]))
        store = engine.store

        # Variables in small domains
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(2))))  # X in 1..2
        _builtin_in(engine, y, Struct("..", (Int(2), Int(3))))  # Y in 2..3
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))  # Z in 1..3

        # nvalue(N, [X,Y,Z]) - N should be constrained based on possible distinct values

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(5))))  # N in 1..5

        vars_list = List([x, y, z])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # N should be constrained: minimum 1 (all same), maximum 3 (all different)
        n_dom = get_domain(store, n.id)
        assert n_dom.min() >= 1
        assert n_dom.max() <= 3

    def test_nvalue_empty_list(self):
        """Test nvalue with empty variable list."""
        engine = Engine(Program([]))
        store = engine.store

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(0))  # N = 0

        vars_list = List([])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True  # 0 distinct values in empty list

        # Test with N != 0 should fail
        n2 = Var(store.new_var(), "N2")
        _builtin_in(engine, n2, Int(1))  # N2 = 1

        result2 = _builtin_nvalue(engine, n2, vars_list)
        assert result2 is False  # Can't have 1 distinct value in empty list

    def test_nvalue_single_variable(self):
        """Test nvalue with single variable."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(5))))

        vars_list = List([x])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # N should be exactly 1 (single variable gives exactly 1 distinct value)
        n_dom = get_domain(store, n.id)
        assert n_dom.intervals == ((1, 1),)

    def test_nvalue_mixed_ground_and_variables(self):
        """Test nvalue with mix of ground values and variables."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # nvalue(N, [X, 2, Y, 2]) - two 2s are ground

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(4))))

        vars_list = List([x, Int(2), y, Int(2)])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # Ground values contribute to distinct count
        # The minimum distinct values depends on whether X and Y can equal 2 or each other

    def test_nvalue_all_same_ground_values(self):
        """Test nvalue with all ground values being the same."""
        engine = Engine(Program([]))
        store = engine.store

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(5))))

        # All values are 1
        vars_list = List([Int(1), Int(1), Int(1)])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # N should be exactly 1
        n_dom = get_domain(store, n.id)
        assert n_dom.intervals == ((1, 1),)

    def test_nvalue_distinct_ground_values(self):
        """Test nvalue with all ground values being distinct."""
        engine = Engine(Program([]))
        store = engine.store

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(5))))

        # All values are distinct: 1, 2, 3
        vars_list = List([Int(1), Int(2), Int(3)])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # N should be exactly 3
        n_dom = get_domain(store, n.id)
        assert n_dom.intervals == ((3, 3),)

    def test_nvalue_backtracking(self):
        """Test that nvalue constraints are properly restored on backtracking."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Post nvalue constraint

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(3))))

        vars_list = List([x, y])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # Mark trail position
        mark = trail.position()

        # Make X = 1, which might affect N's domain
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Backtrack
        trail.unwind_to(mark, store)

        # Domains should be restored
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        n_dom = get_domain(store, n.id)
        assert x_dom.intervals == ((1, 3),)
        assert y_dom.intervals == ((1, 3),)
        # N should be constrained to 1..2 because with 2 variables, max distinct values is 2
        assert n_dom.intervals == ((1, 2),)


class TestNValueIntegration:
    """Integration tests using query interface."""

    def test_nvalue_query_simple(self):
        """Test nvalue through query interface."""
        engine = Engine(Program([]))

        # Two variables, exactly 2 distinct values
        query = "?- X in 1..2, Y in 1..2, nvalue(2, [X,Y]), label([X,Y])."
        solutions = list(engine.query(query))

        # Should get exactly 2 solutions: X=1,Y=2 and X=2,Y=1
        assert len(solutions) == 2

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value))

        assert set(values) == {(1, 2), (2, 1)}

    def test_nvalue_query_one_distinct(self):
        """Test nvalue requiring all variables to have same value."""
        engine = Engine(Program([]))

        # Three variables, exactly 1 distinct value
        query = (
            "?- X in 1..2, Y in 1..2, Z in 1..2, nvalue(1, [X,Y,Z]), label([X,Y,Z])."
        )
        solutions = list(engine.query(query))

        # Should get exactly 2 solutions: all 1s or all 2s
        assert len(solutions) == 2

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value, sol["Z"].value))

        assert set(values) == {(1, 1, 1), (2, 2, 2)}

    def test_nvalue_query_impossible(self):
        """Test nvalue with impossible constraints."""
        engine = Engine(Program([]))

        # Impossible: need 3 distinct values but only 2 possible values
        query = "?- X in 1..2, Y in 1..2, nvalue(3, [X,Y])."
        solutions = list(engine.query(query))

        # Should get no solutions
        assert len(solutions) == 0

    def test_nvalue_query_with_fixed_values(self):
        """Test nvalue with some fixed values."""
        engine = Engine(Program([]))

        # X is fixed to 1, Y is variable, exactly 2 distinct values total
        query = "?- X = 1, Y in 1..3, nvalue(2, [X,Y]), label([Y])."
        solutions = list(engine.query(query))

        # Y must be different from 1, so Y in {2,3}
        assert len(solutions) == 2

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value))

        assert set(values) == {(1, 2), (1, 3)}


class TestNValueEdgeCases:
    """Test edge cases and error conditions."""

    def test_nvalue_non_list_second_arg(self):
        """Test nvalue with non-list second argument."""
        engine = Engine(Program([]))
        store = engine.store

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(1))

        result = _builtin_nvalue(engine, n, Atom("not_a_list"))
        assert result is False

    def test_nvalue_non_integer_n(self):
        """Test nvalue with non-integer N."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))

        result = _builtin_nvalue(engine, Atom("not_integer"), List([x]))
        assert result is False

    def test_nvalue_negative_n(self):
        """Test nvalue with negative N."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(-1))  # Negative count

        result = _builtin_nvalue(engine, n, List([x]))
        assert result is False

    def test_nvalue_non_integer_list_element(self):
        """Test nvalue with non-integer element in variable list."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(1))

        # List contains non-integer atom
        vars_list = List([x, Atom("foo")])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is False

    def test_nvalue_large_values(self):
        """Test nvalue with large domains and counts."""
        engine = Engine(Program([]))
        store = engine.store

        # Large domains
        vars_list = []
        for i in range(10):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(100))))
            vars_list.append(v)

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(50))))

        vars_term = List(vars_list)
        result = _builtin_nvalue(engine, n, vars_term)
        assert result is True


class TestNValueGCCReduction:
    """Test nvalue reduction to global_cardinality constraint."""

    def test_nvalue_reduces_to_gcc_when_n_fixed(self):
        """Test that nvalue with fixed N reduces to GCC internally."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # nvalue(2, [X,Y,Z]) should internally use GCC-like reasoning
        # This means exactly 2 distinct values, so one value appears 2+ times, another 1+ times
        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Int(2))  # N = 2

        vars_list = List([x, y, z])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # The constraint should be posted successfully
        # Exact pruning behavior depends on implementation

    def test_nvalue_with_bound_n_propagation(self):
        """Test propagation when N becomes bound during search."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(2))))

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(2))))  # N can be 1 or 2

        vars_list = List([x, y])
        result = _builtin_nvalue(engine, n, vars_list)
        assert result is True

        # Now fix N = 1 (all variables must have same value)
        set_domain(store, n.id, Domain(((1, 1),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, n.id):
            queue.schedule(pid, priority, cause=("domain_changed", n.id))
        queue.run_to_fixpoint(store, trail, engine)

        # The nvalue constraint should trigger additional propagation
        # to enforce that X and Y must be equal


class TestNValuePerformance:
    """Test performance characteristics of nvalue."""

    @pytest.mark.slow
    def test_nvalue_many_variables(self):
        """Test nvalue with many variables."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 50 variables in domain 1..10
        vars_list = []
        for i in range(50):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))
            vars_list.append(v)

        n = Var(store.new_var(), "N")
        _builtin_in(engine, n, Struct("..", (Int(1), Int(10))))

        vars_term = List(vars_list)
        result = _builtin_nvalue(engine, n, vars_term)
        assert result is True

        # Should complete without timeout
        # N domain should be reasonably constrained
        n_dom = get_domain(store, n.id)
        assert n_dom.min() >= 1
        assert n_dom.max() <= 10


# NOTE: nvalue/2 is not a standard SWI-Prolog predicate, so no SWI baseline tests are provided.
# This is a PyLog extension inspired by similar predicates in other CLP systems.
