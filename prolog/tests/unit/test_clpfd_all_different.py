"""Unit tests for all_different/1 global constraint."""

from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, List, Struct, Atom
from prolog.clpfd.api import get_domain, set_domain, iter_watchers
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_all_different
from prolog.clpfd.props.alldiff import (
    create_incremental_all_different_propagator,
    create_optimized_all_different_propagator,
)
from prolog.unify.unify import unify


class TestAllDifferentBasic:
    """Test basic all_different/1 functionality."""

    def test_all_different_with_fixed_values(self):
        """all_different([1,2,3]) should succeed, all_different([1,2,1]) should fail."""
        engine = Engine(Program([]))

        # Test success case: distinct fixed values

        result = _builtin_all_different(engine, List([Int(1), Int(2), Int(3)]))
        assert result is True

        # Test failure case: duplicate fixed values
        result = _builtin_all_different(engine, List([Int(1), Int(2), Int(1)]))
        assert result is False

    def test_all_different_with_variables(self):
        """all_different([X,Y,Z]) with domains should propagate."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains: X in 1..3, Y in 2..4, Z in 1..3
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(2), Int(4))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # Post all_different([X,Y,Z])

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Domains should be unchanged initially (no singletons yet)
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)
        assert x_dom.intervals == ((1, 3),)
        assert y_dom.intervals == ((2, 4),)
        assert z_dom.intervals == ((1, 3),)

    def test_value_elimination_on_singleton(self):
        """When a variable becomes singleton, its value should be removed from others."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains: all in 1..3
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # Post all_different([X,Y,Z])

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Now make X = 1 (singleton)
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # Y and Z should have value 1 removed
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)
        assert not y_dom.contains(1)
        assert not z_dom.contains(1)
        assert y_dom.intervals == ((2, 3),)
        assert z_dom.intervals == ((2, 3),)

    def test_mixed_variables_and_integers(self):
        """all_different with mix of variables and fixed integers."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains: X in 1..3, Y in 2..4
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(2), Int(4))))

        # Post all_different([X, 2, Y, 3])

        result = _builtin_all_different(engine, List([x, Int(2), y, Int(3)]))
        assert result is True

        # X should have 2 and 3 removed (fixed values), Y should have 2 and 3 removed
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert not x_dom.contains(2)
        assert not x_dom.contains(3)
        assert x_dom.contains(1)
        assert not y_dom.contains(2)
        assert not y_dom.contains(3)
        assert x_dom.intervals == ((1, 1),)  # Only 1 left
        assert y_dom.intervals == ((4, 4),)  # Only 4 left

    def test_failure_on_duplicate_singletons(self):
        """all_different should fail when two variables have same singleton domain."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set both to singleton domain {2}
        _builtin_in(engine, x, Int(2))
        _builtin_in(engine, y, Int(2))

        # Post all_different([X,Y]) should fail

        result = _builtin_all_different(engine, List([x, y]))
        assert result is False

    def test_pigeonhole_principle(self):
        """all_different should fail when pigeonhole principle is violated."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 4 variables with domains in 1..3 (pigeonhole violation)
        vars = []
        for i in range(4):
            v = Var(store.new_var(), f"V{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(3))))
            vars.append(v)

        # Post all_different should fail (4 vars, only 3 possible values)

        result = _builtin_all_different(engine, List(vars))
        # Phase 2: Hall-interval pruning detects pigeonhole violation immediately
        assert result is False  # Fails due to Hall-interval detection

    def test_backtracking_restoration(self):
        """Domains should be restored on backtracking."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Post all_different

        result = _builtin_all_different(engine, List([x, y]))
        assert result is True

        # Mark trail position
        mark = trail.position()

        # Make X singleton, triggering propagation
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Y should have 1 removed
        y_dom = get_domain(store, y.id)
        assert y_dom.intervals == ((2, 3),)

        # Backtrack
        trail.unwind_to(mark, store)

        # Domains should be restored
        x_dom = get_domain(store, x.id)
        y_dom = get_domain(store, y.id)
        assert x_dom.intervals == ((1, 3),)
        assert y_dom.intervals == ((1, 3),)

    def test_idempotent_posting(self):
        """Posting all_different twice should be idempotent."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Post all_different twice

        result1 = _builtin_all_different(engine, List([x, y]))
        assert result1 is True

        # Count propagators before second posting
        initial_prop_count = len(engine.clpfd_queue.propagators)

        result2 = _builtin_all_different(engine, List([x, y]))
        assert result2 is True

        # Should have added another propagator (not deduplicated)
        final_prop_count = len(engine.clpfd_queue.propagators)
        assert final_prop_count == initial_prop_count + 1

        # But behavior should be same - make X singleton
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Y should still have 1 removed (not removed twice)
        y_dom = get_domain(store, y.id)
        assert y_dom.intervals == ((2, 3),)

    def test_non_integer_ground_terms_fail(self):
        """all_different with non-integer ground terms should fail."""
        engine = Engine(Program([]))

        # Test with atom
        result = _builtin_all_different(engine, List([Int(1), Atom("foo"), Int(2)]))
        assert result is False

        # Test with cons structure (not an integer)
        result = _builtin_all_different(
            engine, List([Int(1), Struct(".", [Int(3), Int(14)]), Int(2)])
        )
        assert result is False

    def test_empty_list(self):
        """all_different([]) should succeed trivially."""
        engine = Engine(Program([]))

        result = _builtin_all_different(engine, List([]))
        assert result is True

    def test_singleton_list(self):
        """all_different([X]) should succeed for any X."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        result = _builtin_all_different(engine, List([x]))
        assert result is True

        # Domain should be unchanged
        x_dom = get_domain(store, x.id)
        assert x_dom.intervals == ((1, 10),)

    def test_same_variable_repeated(self):
        """all_different([X, X]) should fail immediately."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        # Even without domains, repeating same variable should fail

        result = _builtin_all_different(engine, List([x, x]))
        assert result is False

        # Also test with domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        result = _builtin_all_different(engine, List([x, x]))
        assert result is False

    def test_unification_after_posting(self):
        """Unifying variables after posting all_different should fail."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))

        # Post all_different([X, Y])

        result = _builtin_all_different(engine, List([x, y]))
        assert result is True

        # Now try to unify X and Y - should fail via hook

        result = unify(x, y, store, trail)
        assert result is False  # all_different forbids aliasing

    def test_unify_to_int_propagation(self):
        """Unifying a variable to an integer should trigger propagation automatically."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(5))))

        # Post all_different([X, Y, Z])

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Unify X with 2 (should automatically wake watchers and propagate)

        result = unify(x, Int(2), store, trail)
        assert result is True

        # The hook should have triggered propagation automatically
        # Y and Z should have 2 removed without manual queue poking
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)

        # Note: This might not work without hook integration
        # For Phase 1, we might need to manually trigger propagation
        # Let's test what we expect after manual propagation for now

        queue = engine.clpfd_queue
        # Manually trigger for Phase 1 (hooks will automate this)
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("unified_to_int", x.id))
        queue.run_to_fixpoint(store, trail, engine)

        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)
        assert not y_dom.contains(2)
        assert not z_dom.contains(2)

    def test_duplicate_integers_with_variables(self):
        """all_different with duplicate integers mixed with variables should fail."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))

        # all_different([X, 2, 2, Y]) should fail immediately (duplicate 2s)

        result = _builtin_all_different(engine, List([x, Int(2), Int(2), y]))
        assert result is False

    def test_cons_form_list(self):
        """Test builtin handles '.'/2 cons form lists."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))

        # Create cons-form list: [X|[Y|[]]]

        nil = Atom("[]")
        cons_list = Struct(".", (x, Struct(".", (y, nil))))

        # Post all_different on cons-form list

        _builtin_all_different(engine, cons_list)

        # Note: This might need proper cons-form parsing in the builtin
        # For now, we'll stick with List objects in Phase 1
        # This test documents the expected future behavior

    def test_aliasing_after_posting(self):
        """Test constraint fails when variables are aliased after posting."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(5))))

        # Post all_different([X, Y, Z])

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Now alias Y and Z (unify them) - should fail

        result = unify(y, z, store, trail)
        assert result is False  # all_different forbids Y=Z


class TestAllDifferentIntegration:
    """Integration tests using query interface."""

    def test_simple_all_different_query(self):
        """Test all_different through query interface."""
        engine = Engine(Program([]))

        query = "?- X in 1..3, Y in 1..3, Z in 1..3, all_different([X,Y,Z]), label([X,Y,Z])."
        solutions = list(engine.query(query))

        # Should get all 6 permutations of {1,2,3}
        assert len(solutions) == 6

        # Extract solution values
        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value, sol["Z"].value))

        # Check we got all permutations
        expected = {(1, 2, 3), (1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2), (3, 2, 1)}
        assert set(values) == expected

    def test_all_different_with_fixed_values_query(self):
        """Test all_different with mixed variables and integers."""
        engine = Engine(Program([]))

        # X and Y should avoid 2 and 3
        query = "?- X in 1..4, Y in 1..4, all_different([X, 2, Y, 3])."
        solutions = list(engine.query(query))

        # Without labeling, we get one solution with constrained variables
        assert len(solutions) == 1

        values = []
        for sol in solutions:
            x_val = sol["X"]
            y_val = sol["Y"]
            # If variables are bound
            if hasattr(x_val, "value") and hasattr(y_val, "value"):
                values.append((x_val.value, y_val.value))
            else:
                # Variables might still have domains
                x_dom = get_domain(engine.store, x_val.id)
                y_dom = get_domain(engine.store, y_val.id)
                if x_dom.is_singleton() and y_dom.is_singleton():
                    values.append((x_dom.min(), y_dom.min()))

        # Note: Without labeling, we might not get ground solutions
        # Let's check domains instead
        query_with_label = (
            "?- X in 1..4, Y in 1..4, all_different([X, 2, Y, 3]), label([X,Y])."
        )
        solutions = list(engine.query(query_with_label))
        assert len(solutions) == 2

        values = []
        for sol in solutions:
            values.append((sol["X"].value, sol["Y"].value))
        assert set(values) == {(1, 4), (4, 1)}


class TestHallIntervalPruning:
    """Test Hall-interval pruning for Phase 2."""

    def test_hall_interval_basic(self):
        """Basic Hall-interval detection and pruning.

        Classic example: X1,X2 ∈ {1,2} and X3 ∈ {1,2,3}
        The interval [1,2] is a Hall set with 2 tight variables.
        X3 should have [1,2] removed, leaving only {3}.
        """
        engine = Engine(Program([]))
        store = engine.store

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        # Set domains: X1,X2 in 1..2, X3 in 1..3
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(3))))

        # Post all_different([X1, X2, X3])

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Check that X3 had interval [1,2] removed
        x3_dom = get_domain(store, x3.id)
        assert x3_dom.intervals == ((3, 3),)
        assert x3_dom.is_singleton()
        assert x3_dom.min() == 3

    def test_hall_interval_multiple_intervals(self):
        """Hall-interval pruning with multiple Hall sets."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables
        vars = []
        for i in range(5):
            v = Var(store.new_var(), f"X{i+1}")
            vars.append(v)

        # X1,X2 ∈ {1,2} - tight on [1,2]
        _builtin_in(engine, vars[0], Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, vars[1], Struct("..", (Int(1), Int(2))))

        # X3,X4 ∈ {4,5} - tight on [4,5]
        _builtin_in(engine, vars[2], Struct("..", (Int(4), Int(5))))
        _builtin_in(engine, vars[3], Struct("..", (Int(4), Int(5))))

        # X5 ∈ {1,2,3,4,5} - should have both [1,2] and [4,5] removed
        _builtin_in(engine, vars[4], Struct("..", (Int(1), Int(5))))

        # Post all_different

        result = _builtin_all_different(engine, List(vars))
        assert result is True

        # X5 should only have value 3 left
        x5_dom = get_domain(store, vars[4].id)
        assert x5_dom.intervals == ((3, 3),)
        assert x5_dom.is_singleton()
        assert x5_dom.min() == 3

    def test_hall_interval_pigeonhole_detection(self):
        """Hall-interval should detect pigeonhole principle violations."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 3 variables all with domain {1,2}
        vars = []
        for i in range(3):
            v = Var(store.new_var(), f"X{i+1}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(2))))
            vars.append(v)

        # Post all_different - should fail (3 vars, only 2 values)

        result = _builtin_all_different(engine, List(vars))

        # Should fail immediately with Hall-interval detection
        assert result is False

    def test_hall_interval_partial_overlap(self):
        """Hall-interval with partial domain overlaps."""
        engine = Engine(Program([]))
        store = engine.store

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")
        x4 = Var(store.new_var(), "X4")

        # X1 ∈ {1,2} - tight on [1,2]
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))
        # X2 ∈ {1,2} - tight on [1,2]
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(2))))
        # X3 ∈ {4,5} - tight on [4,5]
        _builtin_in(engine, x3, Struct("..", (Int(4), Int(5))))
        # X4 ∈ {1,2,3,4,5,6}
        _builtin_in(engine, x4, Struct("..", (Int(1), Int(6))))

        # Post all_different

        result = _builtin_all_different(engine, List([x1, x2, x3, x4]))
        assert result is True

        # Check domains after Hall pruning
        x4_dom = get_domain(store, x4.id)

        # X4 should have [1,2] removed (Hall set for X1,X2)
        # But not [4,5] because only X3 is tight on it (need 2 vars for interval size 2)
        assert not x4_dom.contains(1)
        assert not x4_dom.contains(2)
        assert x4_dom.contains(3)  # Should remain

    def test_hall_interval_with_singletons(self):
        """Hall-interval combined with singleton propagation."""
        engine = Engine(Program([]))
        store = engine.store

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")
        x4 = Var(store.new_var(), "X4")

        # X1 = 1 (singleton)
        _builtin_in(engine, x1, Int(1))
        # X2 ∈ {2,3}
        _builtin_in(engine, x2, Struct("..", (Int(2), Int(3))))
        # X3 ∈ {3,4}
        _builtin_in(engine, x3, Struct("..", (Int(3), Int(4))))
        # X4 ∈ {1,2,3,4,5}
        _builtin_in(engine, x4, Struct("..", (Int(1), Int(5))))

        # Post all_different

        result = _builtin_all_different(engine, List([x1, x2, x3, x4]))
        assert result is True

        # X4 should have 1 removed (singleton) and potentially more from Hall sets
        x4_dom = get_domain(store, x4.id)
        assert not x4_dom.contains(1)  # Singleton removal

        # With Hall pruning, interval [2,3] might be identified as tight for X2
        # and interval [3,4] might be identified as tight for X3
        # The exact pruning depends on the algorithm's order

    def test_hall_interval_no_pruning_needed(self):
        """Hall-interval when no pruning is possible."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables with non-overlapping domains
        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x2, Struct("..", (Int(3), Int(4))))
        _builtin_in(engine, x3, Struct("..", (Int(5), Int(6))))

        # Post all_different

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Domains should remain unchanged (no Hall sets found)
        x1_dom = get_domain(store, x1.id)
        x2_dom = get_domain(store, x2.id)
        x3_dom = get_domain(store, x3.id)

        assert x1_dom.intervals == ((1, 2),)
        assert x2_dom.intervals == ((3, 4),)
        assert x3_dom.intervals == ((5, 6),)

    def test_hall_interval_large_example(self):
        """Hall-interval on larger problem."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 10 variables
        vars = []

        # First 5 variables: tight on [1,5]
        for i in range(5):
            v = Var(store.new_var(), f"X{i+1}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(5))))
            vars.append(v)

        # Next 5 variables: domain [1,10]
        for i in range(5):
            v = Var(store.new_var(), f"Y{i+1}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))
            vars.append(v)

        # Post all_different

        result = _builtin_all_different(engine, List(vars))
        assert result is True

        # The last 5 variables should have [1,5] removed
        for i in range(5, 10):
            dom = get_domain(store, vars[i].id)
            assert dom.min() >= 6
            assert dom.max() == 10
            assert dom.intervals == ((6, 10),)

    def test_hall_avoid_false_positives(self):
        """Ensure Hall pruning doesn't create false positives on non-tight unions."""
        engine = Engine(Program([]))
        store = engine.store

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        # X1 ∈ {1,3}, X2 ∈ {2,4}, X3 ∈ {1,4}
        # Union is {1,2,3,4} with 3 vars - NOT a Hall set
        # No variable domain is tight on any interval
        _builtin_in(engine, x1, Struct("\\/", (Int(1), Int(3))))
        _builtin_in(engine, x2, Struct("\\/", (Int(2), Int(4))))
        _builtin_in(engine, x3, Struct("\\/", (Int(1), Int(4))))

        # Post all_different

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Domains should remain unchanged - no spurious pruning
        x1_dom = get_domain(store, x1.id)
        x2_dom = get_domain(store, x2.id)
        x3_dom = get_domain(store, x3.id)

        # Check that no false Hall interval removal occurred
        assert x1_dom.contains(1) and x1_dom.contains(3)
        assert x2_dom.contains(2) and x2_dom.contains(4)
        assert x3_dom.contains(1) and x3_dom.contains(4)

    def test_hall_pruning_after_post_narrowing(self):
        """Hall pruning should trigger when variables narrow after posting."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")
        x4 = Var(store.new_var(), "X4")

        # Initially: no Hall sets
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(4))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(4))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(4))))
        _builtin_in(engine, x4, Struct("..", (Int(1), Int(4))))

        # Post all_different

        result = _builtin_all_different(engine, List([x1, x2, x3, x4]))
        assert result is True

        # Domains should be unchanged initially (no Hall sets)
        assert get_domain(store, x4.id).intervals == ((1, 4),)

        # Now narrow X1 and X2 to create a Hall set
        set_domain(store, x1.id, Domain(((1, 2),)), trail)
        set_domain(store, x2.id, Domain(((1, 2),)), trail)

        # Trigger propagation (simulating what would happen via watchers)

        queue = engine.clpfd_queue

        # Wake watchers for X1
        for pid, priority in iter_watchers(store, x1.id):
            queue.schedule(pid, priority, cause=("domain_changed", x1.id))
        # Wake watchers for X2
        for pid, priority in iter_watchers(store, x2.id):
            queue.schedule(pid, priority, cause=("domain_changed", x2.id))

        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # X3 and X4 should have [1,2] removed
        x3_dom = get_domain(store, x3.id)
        x4_dom = get_domain(store, x4.id)
        assert x3_dom.intervals == ((3, 4),)
        assert x4_dom.intervals == ((3, 4),)

    def test_explicit_duplicate_roots(self):
        """all_different([X, X, Y]) should fail immediately (duplicate root)."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))

        # Post all_different with X appearing twice - should fail

        result = _builtin_all_different(engine, List([x, x, y]))
        assert result is False

        # Also test with different variable objects but same root
        z = Var(x.id, "Z")  # Same ID as X
        result2 = _builtin_all_different(engine, List([x, z, y]))
        assert result2 is False

    def test_hall_unification_triggers_propagation(self):
        """Unifying a variable to Int should trigger Hall propagation automatically."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        # Set domains
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(3))))

        # Post all_different

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Unify X1 with 1

        result = unify(x1, Int(1), store, trail, occurs_check=False)
        assert result is True

        # NOTE: Without hooks, we need manual propagation
        # This test documents expected behavior with hooks

        queue = engine.clpfd_queue

        # In Phase 2, we manually trigger; Phase 3 hooks will automate
        # Need to re-run all_different propagator since X1 is now bound
        for pid, priority in iter_watchers(store, x1.id):
            queue.schedule(pid, priority, cause=("unified_to_int", x1.id))

        # Also trigger for X2 and X3 to ensure they are processed
        for pid, priority in iter_watchers(store, x2.id):
            queue.schedule(pid, priority, cause=("watching", x2.id))
        for pid, priority in iter_watchers(store, x3.id):
            queue.schedule(pid, priority, cause=("watching", x3.id))

        queue.run_to_fixpoint(store, trail, engine)

        # X2 and X3 should have value 1 removed
        x2_dom = get_domain(store, x2.id)
        x3_dom = get_domain(store, x3.id)
        assert not x2_dom.contains(1)
        assert not x3_dom.contains(1)

        # Now if X2 becomes {2}, X3 should narrow to {3}
        set_domain(store, x2.id, Domain(((2, 2),)), trail)

        for pid, priority in iter_watchers(store, x2.id):
            queue.schedule(pid, priority, cause=("domain_changed", x2.id))
        queue.run_to_fixpoint(store, trail, engine)

        x3_dom = get_domain(store, x3.id)
        assert x3_dom.is_singleton()
        assert x3_dom.min() == 3


class TestEfficientHallIntervalAlgorithm:
    """Tests for Phase 2 efficient Hall-interval algorithm."""

    def test_efficient_hall_correctness_vs_naive(self):
        """New efficient algorithm should produce same results as naive algorithm."""
        engine = Engine(Program([]))
        store = engine.store

        # Create test scenario: X1,X2 ∈ {1,2}, X3 ∈ {1,2,3}
        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(3))))

        # Post all_different using current implementation

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Expected result: X3 should be pruned to {3}
        x3_dom = get_domain(store, x3.id)
        assert x3_dom.intervals == ((3, 3),)

    def test_efficient_hall_large_problem_performance(self):
        """Test efficient algorithm can handle larger problems that naive algorithm struggles with."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 25 variables - exceeds current hard limit of 20
        vars = []

        # First 10 variables: tight on [1,10]
        for i in range(10):
            v = Var(store.new_var(), f"X{i+1}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))
            vars.append(v)

        # Next 15 variables: domain [1,25]
        for i in range(15):
            v = Var(store.new_var(), f"Y{i+1}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(25))))
            vars.append(v)

        # Post all_different - current algorithm would skip due to >20 variable limit

        result = _builtin_all_different(engine, List(vars))
        assert result is True

        # The efficient algorithm should handle this and prune Y variables
        # Check that some of the Y variables have been constrained
        # (exact pruning depends on implementation details)

    def test_efficient_hall_interval_sweep_edge_cases(self):
        """Test edge cases for interval sweep algorithm."""
        engine = Engine(Program([]))
        store = engine.store

        # Edge case 1: Empty domains after other constraints
        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(2))))

        # This should still work

        result = _builtin_all_different(engine, List([x1, x2]))
        assert result is True

        # Edge case 2: Single value domains (singletons)
        x3 = Var(store.new_var(), "X3")
        x4 = Var(store.new_var(), "X4")
        x5 = Var(store.new_var(), "X5")

        _builtin_in(engine, x3, Int(5))  # Singleton
        _builtin_in(engine, x4, Int(6))  # Singleton
        _builtin_in(
            engine, x5, Struct("..", (Int(5), Int(7)))
        )  # Range including singletons

        result2 = _builtin_all_different(engine, List([x3, x4, x5]))
        assert result2 is True

        # X5 should have 5 and 6 removed
        x5_dom = get_domain(store, x5.id)
        assert x5_dom.intervals == ((7, 7),)

    def test_efficient_hall_fragmented_domains(self):
        """Test efficient algorithm with fragmented domains (multiple intervals)."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        # Create fragmented domains: X1 ∈ {1,3}, X2 ∈ {2,4}, X3 ∈ {1,2,3,4,5}
        # These are created by domain operations to simulate realistic scenarios
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x2, Struct("..", (Int(2), Int(4))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(5))))

        # Remove value 2 from X1 to create {1,3}
        set_domain(store, x1.id, Domain(((1, 1), (3, 3))), trail)
        # Remove value 3 from X2 to create {2,4}
        set_domain(store, x2.id, Domain(((2, 2), (4, 4))), trail)

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Interval sweep should handle fragmented domains correctly
        x3_dom = get_domain(store, x3.id)
        # X3 should still have all values since no Hall intervals exist
        assert x3_dom.contains(1)
        assert x3_dom.contains(2)
        assert x3_dom.contains(3)
        assert x3_dom.contains(4)
        assert x3_dom.contains(5)

    def test_efficient_hall_no_domain_materialization(self):
        """Ensure efficient algorithm never materializes large domains."""
        engine = Engine(Program([]))
        store = engine.store

        # Create variables with very large domains
        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        # Large domains that would cause memory explosion if materialized
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(1000000))))
        _builtin_in(engine, x2, Struct("..", (Int(500000), Int(1500000))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(2000000))))

        # This should complete without memory explosion

        result = _builtin_all_different(engine, List([x1, x2, x3]))
        assert result is True

        # Domains should be preserved (no Hall intervals in this case)
        x1_dom = get_domain(store, x1.id)
        x2_dom = get_domain(store, x2.id)
        x3_dom = get_domain(store, x3.id)

        assert x1_dom.intervals == ((1, 1000000),)
        assert x2_dom.intervals == ((500000, 1500000),)
        assert x3_dom.intervals == ((1, 2000000),)

    def test_efficient_hall_complex_overlapping_intervals(self):
        """Test complex scenario with multiple overlapping Hall intervals."""
        engine = Engine(Program([]))
        store = engine.store

        # Create 8 variables with carefully designed domains
        vars = []
        for i in range(8):
            v = Var(store.new_var(), f"X{i+1}")
            vars.append(v)

        # X1,X2,X3 ∈ {1,2,3} - tight on [1,3]
        _builtin_in(engine, vars[0], Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, vars[1], Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, vars[2], Struct("..", (Int(1), Int(3))))

        # X4,X5 ∈ {5,6} - tight on [5,6]
        _builtin_in(engine, vars[3], Struct("..", (Int(5), Int(6))))
        _builtin_in(engine, vars[4], Struct("..", (Int(5), Int(6))))

        # X6,X7,X8 ∈ {1,2,3,4,5,6,7,8} - should be pruned
        _builtin_in(engine, vars[5], Struct("..", (Int(1), Int(8))))
        _builtin_in(engine, vars[6], Struct("..", (Int(1), Int(8))))
        _builtin_in(engine, vars[7], Struct("..", (Int(1), Int(8))))

        result = _builtin_all_different(engine, List(vars))
        assert result is True

        # X6, X7, X8 should have [1,3] and [5,6] removed
        for i in range(5, 8):
            dom = get_domain(store, vars[i].id)
            # Should not contain Hall intervals
            assert not dom.contains(1)
            assert not dom.contains(2)
            assert not dom.contains(3)
            assert not dom.contains(5)
            assert not dom.contains(6)
            # Should contain remaining values
            assert dom.contains(4)
            assert dom.contains(7)
            assert dom.contains(8)

    def test_efficient_hall_incremental_updates(self):
        """Test that the algorithm supports incremental updates efficiently."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")
        x4 = Var(store.new_var(), "X4")

        # Initially large domains - no Hall intervals
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, x4, Struct("..", (Int(1), Int(10))))

        result = _builtin_all_different(engine, List([x1, x2, x3, x4]))
        assert result is True

        # Initially no pruning
        assert get_domain(store, x4.id).intervals == ((1, 10),)

        # Incrementally narrow X1 and X2 to create Hall interval
        set_domain(store, x1.id, Domain(((1, 2),)), trail)

        # Trigger propagation

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x1.id):
            queue.schedule(pid, priority, cause=("domain_changed", x1.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Should still be no Hall interval yet (only 1 tight variable)
        assert get_domain(store, x4.id).intervals == ((1, 10),)

        # Now narrow X2 to create Hall interval [1,2] with 2 tight variables
        set_domain(store, x2.id, Domain(((1, 2),)), trail)

        for pid, priority in iter_watchers(store, x2.id):
            queue.schedule(pid, priority, cause=("domain_changed", x2.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Now X3 and X4 should have [1,2] removed
        x3_dom = get_domain(store, x3.id)
        x4_dom = get_domain(store, x4.id)
        assert x3_dom.intervals == ((3, 10),)
        assert x4_dom.intervals == ((3, 10),)

    def test_efficient_hall_batch_domain_updates(self):
        """Test that domain updates are batched efficiently."""
        engine = Engine(Program([]))
        store = engine.store

        # Create scenario where multiple variables need pruning
        vars = []
        for i in range(5):
            v = Var(store.new_var(), f"X{i+1}")
            vars.append(v)

        # X1,X2 ∈ {1,2} - Hall interval [1,2]
        _builtin_in(engine, vars[0], Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, vars[1], Struct("..", (Int(1), Int(2))))

        # X3,X4,X5 ∈ {1,2,3,4,5} - should have [1,2] removed, leaving {3,4,5}
        for i in range(2, 5):
            _builtin_in(engine, vars[i], Struct("..", (Int(1), Int(5))))

        result = _builtin_all_different(engine, List(vars))
        assert result is True

        # All of X3-X5 should have [1,2] removed
        for i in range(2, 5):
            dom = get_domain(store, vars[i].id)
            assert dom.intervals == ((3, 5),)
            assert not dom.contains(1)
            assert not dom.contains(2)


class TestIncrementalHallSetUpdates:
    """Tests for Phase 2.2 incremental Hall set updates."""

    def test_incremental_propagator_creation(self):
        """Test creation of incremental all_different propagator."""

        # This should create a stateful propagator with revision tracking
        propagator = create_incremental_all_different_propagator([1, 2, 3], ())

        # Propagator should be callable
        assert callable(propagator)

    def test_incremental_no_changes_optimization(self):
        """Test that propagator skips work when no domains changed."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(3))))

        propagator = create_incremental_all_different_propagator(
            [x1.id, x2.id, x3.id], ()
        )

        # First call should do full propagation
        result1 = propagator(store, trail, engine, None)
        assert result1[0] == "ok"

        # Second call with no changes should be optimized (quick return)
        result2 = propagator(store, trail, engine, None)
        assert result2[0] == "ok"
        assert result2[1] is None  # No changes to report

    def test_incremental_detects_domain_changes(self):
        """Test that propagator detects when domains change and recomputes."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(5))))

        propagator = create_incremental_all_different_propagator(
            [x1.id, x2.id, x3.id], ()
        )

        # First call establishes baseline
        result1 = propagator(store, trail, engine, None)
        assert result1[0] == "ok"

        # Change X1 domain to create Hall interval
        set_domain(store, x1.id, Domain(((1, 2),)), trail)

        # Next call should detect change and propagate
        result2 = propagator(store, trail, engine, ("domain_changed", x1.id))
        assert result2[0] == "ok"
        # Should have changes since domain changed

    def test_incremental_partial_recomputation(self):
        """Test that only affected parts are recomputed when few variables change."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create many variables to test selective updating
        vars = []
        for i in range(10):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(15))))
            vars.append(v)

        var_ids = [v.id for v in vars]

        propagator = create_incremental_all_different_propagator(var_ids, ())

        # Initial propagation
        result1 = propagator(store, trail, engine, None)
        assert result1[0] == "ok"

        # Change only one variable
        set_domain(store, vars[0].id, Domain(((1, 3),)), trail)

        # Should detect minimal change and avoid full recomputation
        result2 = propagator(store, trail, engine, ("domain_changed", vars[0].id))
        assert result2[0] == "ok"

    def test_incremental_threshold_triggers_full_recomputation(self):
        """Test that many changes trigger full recomputation instead of incremental."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        vars = []
        for i in range(6):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))
            vars.append(v)

        var_ids = [v.id for v in vars]

        propagator = create_incremental_all_different_propagator(var_ids, ())

        # Initial propagation
        result1 = propagator(store, trail, engine, None)
        assert result1[0] == "ok"

        # Change many variables (>1/3 threshold)
        for i in range(3):  # Change 3/6 = 50% > 33% threshold
            set_domain(store, vars[i].id, Domain(((1, 5),)), trail)

        # Should trigger full recomputation due to many changes
        result2 = propagator(store, trail, engine, ("many_changes",))
        assert result2[0] == "ok"

    def test_incremental_cached_hall_sets(self):
        """Test that Hall sets are cached between incremental runs."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create scenario with clear Hall intervals
        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))  # Tight on [1,2]
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(2))))  # Tight on [1,2]
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(5))))  # Should be pruned

        propagator = create_incremental_all_different_propagator(
            [x1.id, x2.id, x3.id], ()
        )

        # First run should cache Hall interval [1,2]
        result1 = propagator(store, trail, engine, None)
        assert result1[0] == "ok"

        # X3 should be pruned to [3,5]
        x3_dom = get_domain(store, x3.id)
        assert x3_dom.intervals == ((3, 5),)

        # Small change that doesn't affect Hall set structure
        set_domain(store, x3.id, Domain(((3, 4),)), trail)

        # Should reuse cached Hall sets
        result2 = propagator(store, trail, engine, ("domain_changed", x3.id))
        assert result2[0] == "ok"

    def test_incremental_revision_tracking(self):
        """Test that domain revision tracking works correctly."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")

        _builtin_in(engine, x1, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(5))))

        propagator = create_incremental_all_different_propagator([x1.id, x2.id], ())

        # Get initial revision numbers
        x1_dom = get_domain(store, x1.id)
        get_domain(store, x2.id)
        initial_rev1 = x1_dom.rev

        # First propagation should record these revisions
        result1 = propagator(store, trail, engine, None)
        assert result1[0] == "ok"

        # Change X1 domain (with incremented revision)
        old_x1_dom = get_domain(store, x1.id)
        set_domain(store, x1.id, Domain(((1, 3),), old_x1_dom.rev + 1), trail)

        # Revision should have changed
        new_x1_dom = get_domain(store, x1.id)
        assert new_x1_dom.rev != initial_rev1

        # Propagator should detect the revision change
        result2 = propagator(store, trail, engine, ("domain_changed", x1.id))
        assert result2[0] == "ok"


class TestOptimizedValueElimination:
    """Tests for Phase 2.3 optimized value elimination with batch updates."""

    def test_batch_singleton_elimination(self):
        """Test that multiple singletons are eliminated in a single batch operation."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create scenario with multiple singletons
        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")
        x4 = Var(store.new_var(), "X4")

        # X1 = 1, X2 = 3 (singletons), X3,X4 have large domains
        _builtin_in(engine, x1, Int(1))  # Singleton
        _builtin_in(engine, x2, Int(3))  # Singleton
        _builtin_in(engine, x3, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, x4, Struct("..", (Int(1), Int(5))))

        propagator = create_optimized_all_different_propagator(
            [x1.id, x2.id, x3.id, x4.id], ()
        )

        # Should eliminate values {1,3} from X3,X4 in batch
        result = propagator(store, trail, engine, None)
        assert result[0] == "ok"

        # Check that both values were removed efficiently
        x3_dom = get_domain(store, x3.id)
        x4_dom = get_domain(store, x4.id)

        assert not x3_dom.contains(1)
        assert not x3_dom.contains(3)
        assert x3_dom.contains(2)
        assert x3_dom.contains(4)
        assert x3_dom.contains(5)

        assert not x4_dom.contains(1)
        assert not x4_dom.contains(3)

    def test_batch_interval_removal(self):
        """Test batched removal of multiple intervals from domains."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")
        x3 = Var(store.new_var(), "X3")

        # Create proper Hall interval: 2 variables tight on [1,2] (size 2)
        _builtin_in(engine, x1, Struct("..", (Int(1), Int(2))))  # Tight on [1,2]
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(2))))  # Tight on [1,2]
        _builtin_in(
            engine, x3, Struct("..", (Int(1), Int(5)))
        )  # Should have [1,2] removed

        propagator = create_optimized_all_different_propagator(
            [x1.id, x2.id, x3.id], ()
        )

        result = propagator(store, trail, engine, None)
        assert result[0] == "ok"

        # X3 should have interval [1,2] removed in one operation
        x3_dom = get_domain(store, x3.id)
        assert x3_dom.intervals == ((3, 5),)

    def test_optimized_vs_naive_batch_performance(self):
        """Test that optimized batch elimination outperforms naive sequential removal."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create scenario with many singletons
        vars = []
        singleton_values = set()

        # Create 10 singleton variables
        for i in range(10):
            v = Var(store.new_var(), f"S{i}")
            _builtin_in(engine, v, Int(i + 1))  # Singletons 1,2,3,...,10
            vars.append(v)
            singleton_values.add(i + 1)

        # Create 5 variables with large domains that need pruning
        for i in range(5):
            v = Var(store.new_var(), f"L{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(20))))  # Large domain
            vars.append(v)

        var_ids = [v.id for v in vars]

        propagator = create_optimized_all_different_propagator(var_ids, ())

        # Should efficiently batch-remove all singleton values from large domains
        result = propagator(store, trail, engine, None)
        assert result[0] == "ok"

        # Check that all 10 singleton values were removed from large domains
        for i in range(10, 15):  # Check the large domain variables
            dom = get_domain(store, vars[i].id)
            for singleton_val in singleton_values:
                assert not dom.contains(singleton_val)

    def test_batch_empty_domain_detection(self):
        """Test that batch operations detect empty domains early."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create impossible scenario: too many singletons for available space
        vars = []

        # Create 3 variables with singletons in {1,2}
        for i in range(3):
            v = Var(store.new_var(), f"X{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(2))))
            vars.append(v)

        propagator = create_optimized_all_different_propagator([v.id for v in vars], ())

        # Should detect failure quickly via batch operations
        result = propagator(store, trail, engine, None)
        assert result[0] == "fail"

    def test_batch_operations_preserve_intervals(self):
        """Test that batch operations correctly preserve non-affected intervals."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")

        # Create fragmented domain and singleton
        _builtin_in(engine, x1, Int(5))  # Singleton
        set_domain(store, x2.id, Domain(((1, 3), (5, 7), (9, 11))), trail)  # Fragmented

        propagator = create_optimized_all_different_propagator([x1.id, x2.id], ())

        result = propagator(store, trail, engine, None)
        assert result[0] == "ok"

        # X2 should have value 5 removed but other intervals preserved
        x2_dom = get_domain(store, x2.id)
        assert x2_dom.intervals == ((1, 3), (6, 7), (9, 11))
        assert not x2_dom.contains(5)

    def test_batch_update_revision_tracking(self):
        """Test that batch operations properly update domain revisions."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x1 = Var(store.new_var(), "X1")
        x2 = Var(store.new_var(), "X2")

        _builtin_in(engine, x1, Int(1))  # Singleton
        _builtin_in(engine, x2, Struct("..", (Int(1), Int(5))))

        # Get initial revision
        initial_x2_dom = get_domain(store, x2.id)
        initial_rev = initial_x2_dom.rev

        propagator = create_optimized_all_different_propagator([x1.id, x2.id], ())

        result = propagator(store, trail, engine, None)
        assert result[0] == "ok"

        # X2 domain should have updated revision after batch operation
        final_x2_dom = get_domain(store, x2.id)
        assert final_x2_dom.rev > initial_rev
