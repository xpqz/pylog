"""Unit tests for global_cardinality/2 global constraint."""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, List, Struct, Atom
from prolog.clpfd.api import get_domain, set_domain, iter_watchers
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_global_cardinality


class TestGlobalCardinalityBasic:
    """Test basic global_cardinality/2 functionality."""

    def test_global_cardinality_simple_counts(self):
        """Test basic global_cardinality with simple value-count pairs."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables: X, Y, Z all in domain 1..3
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # global_cardinality([X,Y,Z], [1-2, 2-1, 3-0])
        # This means: value 1 appears exactly 2 times, value 2 appears exactly 1 time, value 3 appears 0 times

        vars_list = List([x, y, z])
        counts_list = List(
            [
                Struct("-", (Int(1), Int(2))),  # 1 appears 2 times
                Struct("-", (Int(2), Int(1))),  # 2 appears 1 time
                Struct("-", (Int(3), Int(0))),  # 3 appears 0 times
            ]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # After propagation, value 3 should be removed from all domains
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)

        assert not x_dom.contains(3)
        assert not y_dom.contains(3)
        assert not z_dom.contains(3)

        # Domains should be {1,2}
        assert x_dom.intervals == ((1, 2),)
        assert y_dom.intervals == ((1, 2),)
        assert z_dom.intervals == ((1, 2),)

    def test_global_cardinality_exact_match(self):
        """Test when count requirements exactly match variable assignments."""
        engine = Engine(Program([]))
        store = engine.store

        # Create three variables
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set specific values
        _builtin_in(engine, x, Int(1))  # X = 1
        _builtin_in(engine, y, Int(1))  # Y = 1
        _builtin_in(engine, z, Int(2))  # Z = 2

        # global_cardinality([X,Y,Z], [1-2, 2-1])
        # This matches exactly: two 1s, one 2

        vars_list = List([x, y, z])
        counts_list = List(
            [
                Struct("-", (Int(1), Int(2))),  # 1 appears 2 times ✓
                Struct("-", (Int(2), Int(1))),  # 2 appears 1 time ✓
            ]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

    def test_global_cardinality_failure_too_many(self):
        """Test failure when there are too many of a value."""
        engine = Engine(Program([]))
        store = engine.store

        # Create three variables all set to value 1
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Int(1))
        _builtin_in(engine, y, Int(1))
        _builtin_in(engine, z, Int(1))

        # global_cardinality([X,Y,Z], [1-2])
        # This should fail: we have 3 ones but only allow 2

        vars_list = List([x, y, z])
        counts_list = List(
            [Struct("-", (Int(1), Int(2)))]  # 1 appears 2 times, but we have 3
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is False

    def test_global_cardinality_failure_too_few(self):
        """Test failure when variables cannot provide enough of a value."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables that cannot provide enough 1s
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(2), Int(3))))  # X in 2..3
        _builtin_in(engine, y, Struct("..", (Int(2), Int(3))))  # Y in 2..3

        # global_cardinality([X,Y], [1-1])
        # This should fail: we need 1 occurrence of value 1, but no variable can be 1

        vars_list = List([x, y])
        counts_list = List(
            [Struct("-", (Int(1), Int(1)))]  # Need 1 occurrence of value 1
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is False

    def test_global_cardinality_with_ranges(self):
        """Test global_cardinality with count ranges (min-max)."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")
        w = Var(store.new_var(), "W")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, w, Struct("..", (Int(1), Int(3))))

        # global_cardinality([X,Y,Z,W], [1-(1..2), 2-(1..2), 3-(0..1)])
        # Value 1: between 1-2 times, Value 2: between 1-2 times, Value 3: between 0-1 times

        vars_list = List([x, y, z, w])
        counts_list = List(
            [
                Struct(
                    "-", (Int(1), Struct("..", (Int(1), Int(2))))
                ),  # 1 appears 1-2 times
                Struct(
                    "-", (Int(2), Struct("..", (Int(1), Int(2))))
                ),  # 2 appears 1-2 times
                Struct(
                    "-", (Int(3), Struct("..", (Int(0), Int(1))))
                ),  # 3 appears 0-1 times
            ]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # Should still allow all values initially
        x_dom = get_domain(store, x.id)
        assert x_dom.contains(1) and x_dom.contains(2) and x_dom.contains(3)

    def test_global_cardinality_pruning_on_upper_bound(self):
        """Test pruning when upper bound on count is reached."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Int(1))  # X = 1 (fixed)
        _builtin_in(engine, y, Struct("..", (Int(1), Int(2))))  # Y in 1..2
        _builtin_in(engine, z, Struct("..", (Int(1), Int(2))))  # Z in 1..2

        # global_cardinality([X,Y,Z], [1-1, 2-2])
        # Value 1 can appear exactly 1 time (already satisfied by X=1)
        # So Y and Z must be 2

        vars_list = List([x, y, z])
        counts_list = List(
            [
                Struct("-", (Int(1), Int(1))),  # 1 appears exactly 1 time
                Struct("-", (Int(2), Int(2))),  # 2 appears exactly 2 times
            ]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # Y and Z should have value 1 removed since count limit reached
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)
        assert not y_dom.contains(1)
        assert not z_dom.contains(1)
        assert y_dom.intervals == ((2, 2),)
        assert z_dom.intervals == ((2, 2),)

    def test_global_cardinality_empty_list(self):
        """Test global_cardinality with empty variable list."""
        engine = Engine(Program([]))

        vars_list = List([])
        counts_list = List([])

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

    def test_global_cardinality_no_counts(self):
        """Test global_cardinality with variables but no count constraints."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        vars_list = List([x, y])
        counts_list = List([])  # No count constraints

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # Domains should remain unchanged
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((1, 3),)
        assert y_dom.intervals == ((1, 3),)

    def test_global_cardinality_mixed_variables_and_integers(self):
        """Test global_cardinality with mix of variables and ground values."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # global_cardinality([X, 2, Y, 1], [1-1, 2-1, 3-1])
        # Ground values: one 2, one 1
        # Variables X, Y must provide: zero more 1s and 2s, one 3

        vars_list = List([x, Int(2), y, Int(1)])
        counts_list = List(
            [
                Struct(
                    "-", (Int(1), Int(1))
                ),  # 1 appears 1 time (satisfied by ground Int(1))
                Struct(
                    "-", (Int(2), Int(1))
                ),  # 2 appears 1 time (satisfied by ground Int(2))
                Struct(
                    "-", (Int(3), Int(1))
                ),  # 3 appears 1 time (must come from X or Y)
            ]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # X and Y should have 1 and 2 removed (counts already satisfied)
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert not x_dom.contains(1)
        assert not x_dom.contains(2)
        assert not y_dom.contains(1)
        assert not y_dom.contains(2)
        assert x_dom.intervals == ((3, 3),)
        assert y_dom.intervals == ((3, 3),)

    def test_global_cardinality_invalid_count_format(self):
        """Test global_cardinality with invalid count specifications."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))

        vars_list = List([x])

        # Invalid: count not using "-" functor
        invalid_counts = List([Struct("+", (Int(1), Int(2)))])
        result = _builtin_global_cardinality(engine, vars_list, invalid_counts)
        assert result is False

        # Invalid: non-integer value

        invalid_counts2 = List([Struct("-", (Atom("foo"), Int(1)))])
        result2 = _builtin_global_cardinality(engine, vars_list, invalid_counts2)
        assert result2 is False

    def test_global_cardinality_backtracking(self):
        """Test that global_cardinality constraints are properly restored on backtracking."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Post global_cardinality

        vars_list = List([x, y])
        counts_list = List(
            [
                Struct("-", (Int(1), Int(1))),  # 1 appears exactly 1 time
                Struct("-", (Int(2), Int(1))),  # 2 appears exactly 1 time
            ]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # Mark trail position
        mark = trail.position()

        # Make X = 1, which should constrain Y = 2
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Y should be constrained to 2
        y_dom = get_domain(store, y.id)
        assert y_dom.intervals == ((2, 2),)

        # Backtrack
        trail.unwind_to(mark, store)

        # Domains should be restored
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((1, 3),)
        assert y_dom.intervals == ((1, 3),)


class TestGlobalCardinalityIntegration:
    """Integration tests using query interface."""

    def test_global_cardinality_query_simple(self):
        """Test global_cardinality through query interface."""
        engine = Engine(Program([]))

        # Simple case: two variables, each value appears once
        query = "?- X in 1..2, Y in 1..2, global_cardinality([X,Y], [1-1, 2-1]), label([X,Y])."
        solutions = list(engine.query(query))

        # Should get exactly 2 solutions: X=1,Y=2 and X=2,Y=1
        assert len(solutions) == 2

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value))

        assert set(values) == {(1, 2), (2, 1)}

    def test_global_cardinality_query_with_zeros(self):
        """Test global_cardinality with zero counts."""
        engine = Engine(Program([]))

        # Variables can be 1 or 2, but value 3 must appear 0 times
        query = (
            "?- X in 1..3, Y in 1..3, global_cardinality([X,Y], [3-0]), label([X,Y])."
        )
        solutions = list(engine.query(query))

        # Should get 4 solutions: all combinations of {1,2} x {1,2}
        assert len(solutions) == 4

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value))

        assert set(values) == {(1, 1), (1, 2), (2, 1), (2, 2)}

    def test_global_cardinality_query_impossible(self):
        """Test global_cardinality with impossible constraints."""
        engine = Engine(Program([]))

        # Impossible: need 3 occurrences of value 1, but only have 2 variables
        query = "?- X in 1..2, Y in 1..2, global_cardinality([X,Y], [1-3])."
        solutions = list(engine.query(query))

        # Should get no solutions
        assert len(solutions) == 0


class TestGlobalCardinalityEdgeCases:
    """Test edge cases and error conditions."""

    def test_global_cardinality_non_list_first_arg(self):
        """Test global_cardinality with non-list first argument."""
        engine = Engine(Program([]))

        result = _builtin_global_cardinality(engine, Atom("not_a_list"), List([]))
        assert result is False

    def test_global_cardinality_non_list_second_arg(self):
        """Test global_cardinality with non-list second argument."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        result = _builtin_global_cardinality(engine, List([x]), Atom("not_a_list"))
        assert result is False

    def test_global_cardinality_negative_counts(self):
        """Test global_cardinality with negative count values."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))

        vars_list = List([x])
        counts_list = List(
            [Struct("-", (Int(1), Int(-1)))]  # Negative count should fail
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is False

    def test_global_cardinality_large_values(self):
        """Test global_cardinality with large integer values."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Large domain
        _builtin_in(engine, x, Struct("..", (Int(1000), Int(1002))))
        _builtin_in(engine, y, Struct("..", (Int(1000), Int(1002))))

        vars_list = List([x, y])
        counts_list = List(
            [Struct("-", (Int(1000), Int(1))), Struct("-", (Int(1001), Int(1)))]
        )

        result = _builtin_global_cardinality(engine, vars_list, counts_list)
        assert result is True

        # Should constrain X and Y appropriately
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        # Value 1002 is not constrained, so it should remain in the domain
        assert x_dom.contains(1002)
        assert y_dom.contains(1002)
        # Values 1000 and 1001 are explicitly constrained to appear exactly once each
        assert x_dom.contains(1000)
        assert x_dom.contains(1001)
        assert y_dom.contains(1000)
        assert y_dom.contains(1001)


class TestGlobalCardinalityPerformance:
    """Test performance characteristics of global_cardinality."""

    def test_global_cardinality_many_variables(self):
        """Test global_cardinality with many variables."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 20 variables in domain 1..5
        vars_list = []
        for i in range(20):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(5))))
            vars_list.append(v)

        # Each value 1..5 appears exactly 4 times
        counts_list = []
        for val in range(1, 6):
            counts_list.append(Struct("-", (Int(val), Int(4))))

        vars_term = List(vars_list)
        counts_term = List(counts_list)

        result = _builtin_global_cardinality(engine, vars_term, counts_term)
        assert result is True

        # All variables should still have full domains initially
        for v in vars_list:
            dom = get_domain(store, v.id)
            assert dom.intervals == ((1, 5),)

    @pytest.mark.slow
    def test_global_cardinality_large_counts(self):
        """Test global_cardinality with large count values."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 100 variables in domain 1..10
        vars_list = []
        for i in range(100):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))
            vars_list.append(v)

        # Each value 1..10 appears exactly 10 times
        counts_list = []
        for val in range(1, 11):
            counts_list.append(Struct("-", (Int(val), Int(10))))

        vars_term = List(vars_list)
        counts_term = List(counts_list)

        result = _builtin_global_cardinality(engine, vars_term, counts_term)
        assert result is True


@pytest.mark.swi_baseline
class TestGlobalCardinalitySWIBaseline:
    """Test global_cardinality/2 against SWI-Prolog baseline."""

    def test_global_cardinality_basic_swi_comparison(self, swi):
        """Compare basic global_cardinality/2 behavior with SWI-Prolog."""
        program = ":- use_module(library(clpfd))."
        goal = "(X in 1..3, Y in 1..3, Z in 1..3, global_cardinality([X,Y,Z], [1-2, 2-1, 3-0]), label([X,Y,Z]))"

        # Should find solutions where value 1 appears 2 times, value 2 appears 1 time, value 3 appears 0 times
        count = swi.count(program, goal)
        assert count > 0

        # For this specific constraint, we expect exactly 3 solutions:
        # [1,1,2], [1,2,1], and [2,1,1]
        assert count == 3

    def test_global_cardinality_impossible_swi_comparison(self, swi):
        """Compare global_cardinality/2 impossibility checking against SWI-Prolog."""
        program = ":- use_module(library(clpfd))."
        goal = "(X in 1..2, Y in 1..2, global_cardinality([X,Y], [1-3, 2-0]), label([X,Y]))"

        # Should fail - can't have value 1 appear 3 times with only 2 variables
        count = swi.count(program, goal)
        assert count == 0

    def test_global_cardinality_empty_variables_swi_comparison(self, swi):
        """Compare global_cardinality/2 with empty variable list against SWI-Prolog."""
        program = ":- use_module(library(clpfd))."
        goal = "global_cardinality([], [1-0, 2-0])"

        # Should succeed - empty list satisfies zero counts
        count = swi.count(program, goal)
        assert count == 1

    def test_global_cardinality_ground_values_swi_comparison(self, swi):
        """Compare global_cardinality/2 with ground values against SWI-Prolog."""
        program = ":- use_module(library(clpfd))."
        goal = "global_cardinality([1,2,2,1], [1-2, 2-2])"

        # Should succeed - ground values match the specified counts
        count = swi.count(program, goal)
        assert count == 1

    def test_global_cardinality_ground_values_fail_swi_comparison(self, swi):
        """Compare global_cardinality/2 failure with ground values against SWI-Prolog."""
        program = ":- use_module(library(clpfd))."
        goal = "global_cardinality([1,2,2,1], [1-3, 2-1])"

        # Should fail - ground values don't match the specified counts
        count = swi.count(program, goal)
        assert count == 0
