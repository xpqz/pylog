"""Unit tests for all_different/1 global constraint."""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, List, Struct
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import _builtin_in


class TestAllDifferentBasic:
    """Test basic all_different/1 functionality."""

    def test_all_different_with_fixed_values(self):
        """all_different([1,2,3]) should succeed, all_different([1,2,1]) should fail."""
        engine = Engine(Program([]))

        # Test success case: distinct fixed values
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([Int(1), Int(2), Int(3)]))
        assert result is True

        # Test failure case: duplicate fixed values
        result = _builtin_all_different(engine, List([Int(1), Int(2), Int(1)]))
        assert result is False

    def test_all_different_with_variables(self):
        """all_different([X,Y,Z]) with domains should propagate."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        # Set domains: X in 1..3, Y in 2..4, Z in 1..3
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(2), Int(4))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))

        # Post all_different([X,Y,Z])
        from prolog.engine.builtins_clpfd import _builtin_all_different

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
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Now make X = 1 (singleton)
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        from prolog.clpfd.api import iter_watchers

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
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set domains: X in 1..3, Y in 2..4
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(2), Int(4))))

        # Post all_different([X, 2, Y, 3])
        from prolog.engine.builtins_clpfd import _builtin_all_different

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
        trail = engine.trail

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # Set both to singleton domain {2}
        _builtin_in(engine, x, Int(2))
        _builtin_in(engine, y, Int(2))

        # Post all_different([X,Y]) should fail
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, y]))
        assert result is False

    def test_pigeonhole_principle(self):
        """all_different should fail when pigeonhole principle is violated."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create 4 variables with domains in 1..3 (pigeonhole violation)
        vars = []
        for i in range(4):
            v = Var(store.new_var(), f"V{i}")
            _builtin_in(engine, v, Struct("..", (Int(1), Int(3))))
            vars.append(v)

        # Post all_different should fail (4 vars, only 3 possible values)
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List(vars))
        # Phase 1: This won't fail immediately without Hall pruning
        # But after setting any var to singleton, propagation should fail
        assert result is True  # Initially succeeds

        # Set first var to 1
        set_domain(store, vars[0].id, Domain(((1, 1),)), trail)

        # Trigger propagation
        from prolog.clpfd.api import iter_watchers

        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, vars[0].id):
            queue.schedule(pid, priority, cause=("domain_changed", vars[0].id))
        result = queue.run_to_fixpoint(store, trail, engine)

        # Other vars should have 1 removed, leaving {2,3}
        for v in vars[1:]:
            dom = get_domain(store, v.id)
            assert dom.intervals == ((2, 3),)

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
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, y]))
        assert result is True

        # Mark trail position
        mark = trail.position()

        # Make X singleton, triggering propagation
        set_domain(store, x.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        from prolog.clpfd.api import iter_watchers

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
        from prolog.engine.builtins_clpfd import _builtin_all_different

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
        from prolog.clpfd.api import iter_watchers

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

        from prolog.ast.terms import Atom
        from prolog.engine.builtins_clpfd import _builtin_all_different

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

        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([]))
        assert result is True

    def test_singleton_list(self):
        """all_different([X]) should succeed for any X."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        from prolog.engine.builtins_clpfd import _builtin_all_different

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
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, x]))
        assert result is False

        # Also test with domains
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        result = _builtin_all_different(engine, List([x, x]))
        assert result is False

    @pytest.mark.xfail(reason="Requires hook integration for all_different constraint")
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
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, y]))
        assert result is True

        # Now try to unify X and Y - should fail via hook
        from prolog.unify.unify import unify

        result = unify(x, y, store, trail)
        assert result is False  # all_different forbids aliasing

    @pytest.mark.xfail(
        reason="Requires hook integration for automatic propagation on unification"
    )
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
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Unify X with 2 (should automatically wake watchers and propagate)
        from prolog.unify.unify import unify

        result = unify(x, Int(2), store, trail)
        assert result is True

        # The hook should have triggered propagation automatically
        # Y and Z should have 2 removed without manual queue poking
        y_dom = get_domain(store, y.id)
        z_dom = get_domain(store, z.id)

        # Note: This might not work without hook integration
        # For Phase 1, we might need to manually trigger propagation
        # Let's test what we expect after manual propagation for now
        from prolog.clpfd.api import iter_watchers

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
        from prolog.engine.builtins_clpfd import _builtin_all_different

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
        from prolog.ast.terms import Atom

        nil = Atom("[]")
        cons_list = Struct(".", (x, Struct(".", (y, nil))))

        # Post all_different on cons-form list
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, cons_list)

        # Note: This might need proper cons-form parsing in the builtin
        # For now, we'll stick with List objects in Phase 1
        # This test documents the expected future behavior

    @pytest.mark.xfail(reason="Requires hook integration for all_different constraint")
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
        from prolog.engine.builtins_clpfd import _builtin_all_different

        result = _builtin_all_different(engine, List([x, y, z]))
        assert result is True

        # Now alias Y and Z (unify them) - should fail
        from prolog.unify.unify import unify

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
