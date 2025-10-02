"""Unit tests for table/2 global constraint."""

from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, List, Struct, Atom
from prolog.clpfd.api import get_domain, set_domain, iter_watchers
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_table


class TestTableBasic:
    """Test basic table/2 functionality."""

    def test_table_with_valid_tuples(self):
        """table([X,Y], [[1,2], [2,3], [3,1]]) should succeed."""
        engine = Engine(Program([]))

        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")

        # Create table constraint: table([X,Y], [[1,2], [2,3], [3,1]])
        vars_list = List([x, y])
        tuples_list = List(
            [List([Int(1), Int(2)]), List([Int(2), Int(3)]), List([Int(3), Int(1)])]
        )

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

    def test_table_with_ground_values(self):
        """table([1, 2], [[1,2], [2,3]]) should succeed if ground values match a tuple."""
        engine = Engine(Program([]))

        # table([1, 2], [[1,2], [2,3]]) - [1,2] is in the table
        vars_list = List([Int(1), Int(2)])
        tuples_list = List([List([Int(1), Int(2)]), List([Int(2), Int(3)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

    def test_table_with_ground_values_fail(self):
        """table([1, 3], [[1,2], [2,3]]) should fail if ground values don't match any tuple."""
        engine = Engine(Program([]))

        # table([1, 3], [[1,2], [2,3]]) - [1,3] is not in the table
        vars_list = List([Int(1), Int(3)])
        tuples_list = List([List([Int(1), Int(2)]), List([Int(2), Int(3)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is False

    def test_table_empty_tuples(self):
        """table([X], []) should fail (no valid assignments)."""
        engine = Engine(Program([]))

        x = Var(engine.store.new_var(), "X")
        vars_list = List([x])
        tuples_list = List([])  # Empty table

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is False

    def test_table_empty_variables(self):
        """table([], [[]]) should succeed trivially."""
        engine = Engine(Program([]))

        vars_list = List([])  # No variables
        tuples_list = List([List([])])  # One empty tuple

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

    def test_table_arity_mismatch(self):
        """table([X,Y], [[1], [2,3]]) should fail due to arity mismatch."""
        engine = Engine(Program([]))

        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")
        vars_list = List([x, y])
        tuples_list = List(
            [List([Int(1)]), List([Int(2), Int(3)])]  # Wrong arity  # Correct arity
        )

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is False

    def test_table_non_integer_values(self):
        """table([X], [[atom]]) should fail with non-integer values."""
        engine = Engine(Program([]))

        x = Var(engine.store.new_var(), "X")
        vars_list = List([x])
        tuples_list = List([List([Atom("atom")])])  # Non-integer value

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is False


class TestTableDomainPruning:
    """Test table constraint domain pruning via GAC-lite."""

    def test_simple_domain_pruning(self):
        """Variables should be pruned to only values that appear in valid tuples."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains: X in 1..5, Y in 1..5
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))

        # table([X,Y], [[1,2], [3,4]]) - only values 1,2,3,4 should remain
        vars_list = List([x, y])
        tuples_list = List([List([Int(1), Int(2)]), List([Int(3), Int(4)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Check that domains are pruned correctly
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)

        # X should only contain values that appear in first position: {1, 3}
        assert x_dom.contains(1)
        assert x_dom.contains(3)
        assert not x_dom.contains(2)
        assert not x_dom.contains(4)
        assert not x_dom.contains(5)

        # Y should only contain values that appear in second position: {2, 4}
        assert y_dom.contains(2)
        assert y_dom.contains(4)
        assert not y_dom.contains(1)
        assert not y_dom.contains(3)
        assert not y_dom.contains(5)

    def test_support_based_pruning(self):
        """Values without any supporting tuple should be removed."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Post table constraint
        vars_list = List([x, y])
        tuples_list = List([List([Int(1), Int(2)]), List([Int(2), Int(1)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Initially domains should be pruned to supported values
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)

        # Both X and Y should only contain {1, 2}
        assert x_dom.contains(1) and x_dom.contains(2)
        assert not x_dom.contains(3)
        assert y_dom.contains(1) and y_dom.contains(2)
        assert not y_dom.contains(3)

        # Now narrow X to {1} and check that Y is pruned accordingly
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # When X=1, only tuple [1,2] is valid, so Y must be 2
        y_dom = get_domain(store, y.id)
        assert y_dom.is_singleton()
        assert y_dom.min() == 2

    def test_gac_lite_filtering(self):
        """Test GAC-lite: remove values that have no support in current domains."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # table([X,Y,Z], [[1,2,3], [2,1,3], [2,3,1]])
        vars_list = List([x, y, z])
        tuples_list = List(
            [
                List([Int(1), Int(2), Int(3)]),
                List([Int(2), Int(1), Int(3)]),
                List([Int(2), Int(3), Int(1)]),
            ]
        )

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Now narrow Y to {1} - only [2,1,3] tuple remains valid
        set_domain(store, y.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, y.id):
            queue.schedule(pid, priority, cause=("domain_changed", y.id))
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # X must be 2, Z must be 3
        x_dom = get_domain(store, x.id)
        z_dom = get_domain(store, z.id)
        assert x_dom.is_singleton() and x_dom.min() == 2
        assert z_dom.is_singleton() and z_dom.min() == 3

    def test_no_valid_support_failure(self):
        """Constraint should fail when no valid tuples remain."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set initial domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(2))))

        # table([X,Y], [[1,2], [2,1]])
        vars_list = List([x, y])
        tuples_list = List([List([Int(1), Int(2)]), List([Int(2), Int(1)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Now force X=1 and Y=1 - this has no support in the table
        set_domain(store, x.id, Domain(((1, 1),)), trail)
        set_domain(store, y.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        for pid, priority in iter_watchers(store, y.id):
            queue.schedule(pid, priority, cause=("domain_changed", y.id))

        # Should fail because [1,1] is not a valid tuple
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is False


class TestTableEdgeCases:
    """Test edge cases and error conditions."""

    def test_table_single_variable(self):
        """table([X], [[1], [3], [5]]) should work for single variable."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        # Set domain
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # table([X], [[1], [3], [5]])
        vars_list = List([x])
        tuples_list = List([List([Int(1)]), List([Int(3)]), List([Int(5)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # X should be pruned to {1, 3, 5}
        x_dom = get_domain(store, x.id)
        assert x_dom.contains(1)
        assert x_dom.contains(3)
        assert x_dom.contains(5)
        assert not x_dom.contains(2)
        assert not x_dom.contains(4)

    def test_table_large_arity(self):
        """table with many variables should work efficiently."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 5 variables
        vars = []
        for i in range(5):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(2))))
            vars.append(v)

        # table([X0,X1,X2,X3,X4], [[1,1,1,1,1], [2,2,2,2,2]])
        vars_list = List(vars)
        tuples_list = List(
            [
                List([Int(1), Int(1), Int(1), Int(1), Int(1)]),
                List([Int(2), Int(2), Int(2), Int(2), Int(2)]),
            ]
        )

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # All variables should still have domain {1, 2}
        for v in vars:
            dom = get_domain(store, v.id)
            assert dom.contains(1) and dom.contains(2)

    def test_table_duplicate_tuples(self):
        """Duplicate tuples should be handled correctly."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # table([X,Y], [[1,2], [1,2], [2,3]]) - [1,2] appears twice
        vars_list = List([x, y])
        tuples_list = List(
            [
                List([Int(1), Int(2)]),
                List([Int(1), Int(2)]),  # Duplicate
                List([Int(2), Int(3)]),
            ]
        )

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Should work correctly despite duplicates
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.contains(1) and x_dom.contains(2)
        assert y_dom.contains(2) and y_dom.contains(3)

    def test_table_mixed_variables_and_integers(self):
        """table([X, 2, Y], [[1,2,3], [4,2,5]]) with mixed vars and constants."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))

        # table([X, 2, Y], [[1,2,3], [4,2,5]]) - middle position fixed to 2
        vars_list = List([x, Int(2), y])
        tuples_list = List(
            [List([Int(1), Int(2), Int(3)]), List([Int(4), Int(2), Int(5)])]
        )

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # X should be {1, 4}, Y should be {3, 5}
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.contains(1) and x_dom.contains(4)
        assert not x_dom.contains(2) and not x_dom.contains(3) and not x_dom.contains(5)
        assert y_dom.contains(3) and y_dom.contains(5)
        assert not y_dom.contains(1) and not y_dom.contains(2) and not y_dom.contains(4)

    def test_table_invalid_tuple_format(self):
        """table([X], [atom]) should fail with invalid tuple format."""
        engine = Engine(Program([]))

        x = Var(engine.store.new_var(), "X")
        vars_list = List([x])
        tuples_list = List([Atom("not_a_list")])  # Invalid - not a list

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is False

    def test_table_variable_appears_twice(self):
        """table([X, X], [[1,1], [2,2]]) should work when same variable appears multiple times."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))

        # table([X, X], [[1,1], [2,2]]) - X appears twice, must have same value
        vars_list = List([x, x])
        tuples_list = List([List([Int(1), Int(1)]), List([Int(2), Int(2)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # X should be pruned to {1, 2}
        x_dom = get_domain(store, x.id)
        assert x_dom.contains(1) and x_dom.contains(2)
        assert not x_dom.contains(3)


class TestTablePerformance:
    """Test performance characteristics of table constraint."""

    def test_table_large_domain_pruning(self):
        """Table should efficiently prune large domains."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Large domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(1000))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(1000))))

        # Small table
        vars_list = List([x, y])
        tuples_list = List([List([Int(5), Int(10)]), List([Int(15), Int(20)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Should efficiently prune to small domains
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.size() == 2  # {5, 15}
        assert y_dom.size() == 2  # {10, 20}

    def test_table_many_tuples(self):
        """Table should handle many tuples efficiently."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(50))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(50))))

        # Create many tuples: (i, i+1) for i = 1..25
        tuples = []
        for i in range(1, 26):
            tuples.append(List([Int(i), Int(i + 1)]))

        vars_list = List([x, y])
        tuples_list = List(tuples)

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # X should be {1..25}, Y should be {2..26}
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.min() == 1 and x_dom.max() == 25
        assert y_dom.min() == 2 and y_dom.max() == 26


class TestTableBacktracking:
    """Test table constraint with backtracking."""

    def test_table_backtracking_restoration(self):
        """Domains should be restored correctly on backtracking."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Post table constraint
        vars_list = List([x, y])
        tuples_list = List([List([Int(1), Int(2)]), List([Int(2), Int(3)])])

        result = _builtin_table(engine, vars_list, tuples_list)
        assert result is True

        # Domains should be pruned
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.contains(1) and x_dom.contains(2) and not x_dom.contains(3)
        assert y_dom.contains(2) and y_dom.contains(3) and not y_dom.contains(1)

        # Mark trail position
        mark = trail.position()

        # Make X singleton, triggering further propagation
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Y should be forced to 2
        y_dom = get_domain(store, y.id)
        assert y_dom.is_singleton() and y_dom.min() == 2

        # Backtrack
        trail.unwind_to(mark, store)

        # Domains should be restored to post-table state
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.contains(1) and x_dom.contains(2) and not x_dom.contains(3)
        assert y_dom.contains(2) and y_dom.contains(3) and not y_dom.contains(1)


class TestTableIntegration:
    """Integration tests using query interface."""

    def test_simple_table_query(self):
        """Test table through query interface."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..3, Y in 1..3,
           table([X,Y], [[1,2], [2,1], [3,3]]),
           label([X,Y]).
        """
        solutions = list(engine.query(query))

        # Should get exactly the 3 tuples from the table
        assert len(solutions) == 3

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value))

        expected = {(1, 2), (2, 1), (3, 3)}
        assert set(values) == expected

    def test_table_with_arithmetic(self):
        """Test table combined with arithmetic constraints."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..5, Y in 1..5, Z in 1..10,
           table([X,Y], [[1,2], [2,3], [3,4]]),
           Z #= X + Y,
           label([X,Y,Z]).
        """
        solutions = list(engine.query(query))

        # Should get solutions where Z = X + Y and [X,Y] is in table
        assert len(solutions) == 3

        values = []
        for sol in solutions:
            x, y, z = sol["X"].value, sol["Y"].value, sol["Z"].value
            values.append((x, y, z))
            assert z == x + y  # Check arithmetic constraint

        expected = {(1, 2, 3), (2, 3, 5), (3, 4, 7)}
        assert set(values) == expected
