"""Tests for Stage 4 Phase 3: Hook Registry & Var-Nonvar Integration.

Tests hook registration, dispatch, and integration with unification.
"""

from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.ast.terms import Atom, Int, Struct


class TestHookRegistry:
    """Test hook registry functionality in Engine."""

    def test_engine_initializes_with_empty_hooks(self):
        """Engine should initialize with empty _attr_hooks dict."""
        program = Program(())
        engine = Engine(program)

        assert hasattr(engine, "_attr_hooks")
        assert isinstance(engine._attr_hooks, dict)
        assert len(engine._attr_hooks) == 0

    def test_register_attr_hook_stores_function(self):
        """register_attr_hook should store hook function."""
        program = Program(())
        engine = Engine(program)

        def test_hook(engine, varid, other):
            return True

        engine.register_attr_hook("test_module", test_hook)

        assert "test_module" in engine._attr_hooks
        assert engine._attr_hooks["test_module"] == test_hook

    def test_register_attr_hook_overwrites_existing(self):
        """register_attr_hook should overwrite existing hook for same module."""
        program = Program(())
        engine = Engine(program)

        def hook1(engine, varid, other):
            return True

        def hook2(engine, varid, other):
            return False

        engine.register_attr_hook("test", hook1)
        assert engine._attr_hooks["test"] == hook1

        engine.register_attr_hook("test", hook2)
        assert engine._attr_hooks["test"] == hook2

    def test_hook_signature_three_params(self):
        """Hook functions should accept (engine, varid, other) and return bool."""
        program = Program(())
        engine = Engine(program)

        # This is a signature test - the hook itself won't be called in this test
        def valid_hook(engine, varid, other) -> bool:
            # engine: Engine instance
            # varid: integer variable ID
            # other: Term being unified with
            return True

        # Should not raise any errors
        engine.register_attr_hook("test", valid_hook)
        assert engine._attr_hooks["test"] == valid_hook


class TestHookDispatch:
    """Test hook dispatch mechanism."""

    def test_dispatch_returns_true_when_no_attrs(self):
        """dispatch_attr_hooks should return True when variable has no attributes."""
        program = Program(())
        engine = Engine(program)

        # Create a variable without attributes
        varid = engine.store.new_var("X")

        # Dispatch should return True (no hooks to call)
        result = engine.dispatch_attr_hooks(varid, Int(42))
        assert result is True

    def test_dispatch_calls_relevant_hooks(self):
        """dispatch_attr_hooks should call hooks for modules with attributes."""
        program = Program(())
        engine = Engine(program)

        # Track hook calls
        calls = []

        def hook1(eng, vid, other):
            calls.append(("hook1", vid, other))
            return True

        def hook2(eng, vid, other):
            calls.append(("hook2", vid, other))
            return True

        # Register hooks
        engine.register_attr_hook("mod1", hook1)
        engine.register_attr_hook("mod2", hook2)

        # Create variable with attribute for mod1 only
        varid = engine.store.new_var("X")
        engine.store.attrs[varid] = {"mod1": "value1"}

        # Dispatch should only call hook1
        result = engine.dispatch_attr_hooks(varid, Int(42))
        assert result is True
        assert len(calls) == 1
        assert calls[0][0] == "hook1"
        assert calls[0][1] == varid
        assert calls[0][2] == Int(42)

    def test_hooks_called_in_sorted_order(self):
        """Hooks should be called in sorted module name order for determinism."""
        program = Program(())
        engine = Engine(program)

        call_order = []

        def make_hook(name):
            def hook(eng, vid, other):
                call_order.append(name)
                return True

            return hook

        # Register hooks in non-alphabetical order
        engine.register_attr_hook("zebra", make_hook("zebra"))
        engine.register_attr_hook("apple", make_hook("apple"))
        engine.register_attr_hook("monkey", make_hook("monkey"))

        # Create variable with all three attributes
        varid = engine.store.new_var("X")
        engine.store.attrs[varid] = {"zebra": "z", "apple": "a", "monkey": "m"}

        # Dispatch should call in alphabetical order
        result = engine.dispatch_attr_hooks(varid, Int(42))
        assert result is True
        assert call_order == ["apple", "monkey", "zebra"]

    def test_dispatch_dereferences_variable(self):
        """dispatch_attr_hooks should dereference variable to get root."""
        program = Program(())
        engine = Engine(program)

        received_varid = None

        def hook(eng, vid, other):
            nonlocal received_varid
            received_varid = vid
            return True

        engine.register_attr_hook("test", hook)

        # Create a variable and put attribute on it
        x = engine.store.new_var("X")
        engine.store.attrs[x] = {"test": "value"}

        # Create an alias that points to X
        y = engine.store.new_var("Y")
        # Make Y point to X manually (simulating partial deref chain)
        # Note: In Stage 4, attributes live on roots. Attribute migration
        # during var-var unification will be handled in Phase 4.3.
        # This test isolates the dispatch dereferencing behavior.
        engine.store.cells[y].ref = x

        # Dispatch on Y should dereference to X (the root) and pass X to hook
        result = engine.dispatch_attr_hooks(y, Int(42))
        assert result is True
        assert received_varid == x  # Hook should receive the root

    def test_fast_path_skips_vars_without_attrs(self):
        """dispatch_attr_hooks should skip vars not in store.attrs (fast path)."""
        program = Program(())
        engine = Engine(program)

        hook_called = False

        def hook(eng, vid, other):
            nonlocal hook_called
            hook_called = True
            return True

        engine.register_attr_hook("test", hook)

        # Variable without attributes
        varid = engine.store.new_var("X")
        # Ensure no attrs entry for this variable
        assert varid not in engine.store.attrs

        # Dispatch should not call hook
        result = engine.dispatch_attr_hooks(varid, Int(42))
        assert result is True
        assert not hook_called

    def test_short_circuit_on_first_false(self):
        """dispatch_attr_hooks should stop calling hooks after first False."""
        program = Program(())
        engine = Engine(program)

        calls = []

        def hook1(eng, vid, other):
            calls.append("hook1")
            return True

        def hook2(eng, vid, other):
            calls.append("hook2")
            return False  # This one fails

        def hook3(eng, vid, other):
            calls.append("hook3")
            return True

        # Register hooks (alphabetical order: a, b, c)
        engine.register_attr_hook("a_mod", hook1)
        engine.register_attr_hook("b_mod", hook2)
        engine.register_attr_hook("c_mod", hook3)

        # Variable with all three attributes
        varid = engine.store.new_var("X")
        engine.store.attrs[varid] = {"a_mod": "val1", "b_mod": "val2", "c_mod": "val3"}

        # Should call hook1, hook2 (returns False), then stop
        result = engine.dispatch_attr_hooks(varid, Int(42))
        assert result is False
        assert calls == ["hook1", "hook2"]  # hook3 not called

    def test_dispatch_skips_modules_without_hooks(self):
        """dispatch_attr_hooks should skip modules without registered hooks."""
        program = Program(())
        engine = Engine(program)

        hook_called = False

        def hook(eng, vid, other):
            nonlocal hook_called
            hook_called = True
            return True

        # Only register hook for mod1
        engine.register_attr_hook("mod1", hook)

        # Variable has attributes for mod1 and mod2
        varid = engine.store.new_var("X")
        engine.store.attrs[varid] = {
            "mod1": "value1",
            "mod2": "value2",  # No hook for mod2
        }

        # Should call hook for mod1, skip mod2
        result = engine.dispatch_attr_hooks(varid, Int(42))
        assert result is True
        assert hook_called  # mod1 hook was called


class TestTrailAdapterIntegration:
    """Test TrailAdapter carries engine reference."""

    def test_trail_adapter_carries_engine(self):
        """TrailAdapter should carry engine reference."""
        program = Program(())
        engine = Engine(program)

        from prolog.engine.trail_adapter import TrailAdapter

        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

        assert hasattr(trail_adapter, "engine")
        assert trail_adapter.engine is engine

    def test_trail_adapter_engine_accessible_in_unify(self):
        """TrailAdapter.engine should be accessible during unification."""
        program = Program(())
        engine = Engine(program)

        # This will be tested indirectly through var-nonvar unification
        # when hooks are integrated. For now, verify the structure exists.
        from prolog.engine.trail_adapter import TrailAdapter

        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

        # Verify we can access engine from trail_adapter
        assert trail_adapter.engine is engine
        assert trail_adapter.engine.store is engine.store

    def test_trail_adapter_with_none_engine(self):
        """TrailAdapter with None engine should not crash."""
        program = Program(())
        engine = Engine(program)

        from prolog.engine.trail_adapter import TrailAdapter

        # Create adapter without engine
        trail_adapter = TrailAdapter(engine.trail, engine=None, store=engine.store)

        assert trail_adapter.engine is None

        # Basic operations should still work (use a valid tag)
        trail_adapter.push(("rank", 0, 0))
        assert len(engine.trail._entries) == 1


class TestVarNonvarUnification:
    """Test hook integration with var-nonvar unification."""

    def test_hook_called_on_attributed_var_nonvar(self):
        """Hook should be called when unifying attributed var with nonvar."""
        program = Program(())
        engine = Engine(program)

        hook_calls = []

        def test_hook(eng, varid, other):
            hook_calls.append((varid, other))
            return True

        engine.register_attr_hook("test", test_hook)

        # Put attribute on X and unify with integer
        query = "?- put_attr(X, test, data), X = 42."
        solutions = list(engine.query(query))

        # Hook should have been called
        assert len(hook_calls) == 1
        assert hook_calls[0][1] == Int(42)

        # Unification should succeed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(42)

    def test_hook_not_called_for_vars_without_attrs(self):
        """Hook should NOT be called for vars without attributes (fast path)."""
        program = Program(())
        engine = Engine(program)

        hook_calls = []

        def test_hook(eng, varid, other):
            hook_calls.append((varid, other))
            return True

        engine.register_attr_hook("test", test_hook)

        # Variable without attributes
        query = "?- X = 42."
        solutions = list(engine.query(query))

        # Hook should NOT have been called
        assert len(hook_calls) == 0

        # Unification should succeed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(42)

    def test_hook_rejection_causes_unification_failure(self):
        """Hook returning False should cause unification to fail."""
        program = Program(())
        engine = Engine(program)

        def reject_hook(eng, varid, other):
            return False  # Always reject

        engine.register_attr_hook("test", reject_hook)

        # Put attribute and try to unify
        query = "?- put_attr(X, test, data), X = 42."
        solutions = list(engine.query(query))

        # Unification should fail due to hook rejection
        assert len(solutions) == 0

    def test_multiple_hooks_all_called(self):
        """Multiple hooks should all be called for different modules."""
        program = Program(())
        engine = Engine(program)

        calls = []

        def hook1(eng, varid, other):
            calls.append("hook1")
            return True

        def hook2(eng, varid, other):
            calls.append("hook2")
            return True

        engine.register_attr_hook("mod1", hook1)
        engine.register_attr_hook("mod2", hook2)

        # Put attributes for both modules
        query = "?- put_attr(X, mod1, data1), put_attr(X, mod2, data2), X = 42."
        solutions = list(engine.query(query))

        # Both hooks should have been called
        assert "hook1" in calls
        assert "hook2" in calls

        # Unification should succeed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(42)


class TestExampleConstraint:
    """Test example 'must_be_even' constraint."""

    def test_must_be_even_rejects_odd(self):
        """must_be_even hook should reject odd integers."""
        program = Program(())
        engine = Engine(program)

        def must_be_even(eng, varid, other):
            # Accept non-integers
            if not isinstance(other, Int):
                return True
            # Check if even
            return other.value % 2 == 0

        engine.register_attr_hook("must_be_even", must_be_even)

        # Try to unify with odd number
        query = "?- put_attr(X, must_be_even, true), X = 3."
        solutions = list(engine.query(query))

        # Should fail
        assert len(solutions) == 0

    def test_must_be_even_accepts_even(self):
        """must_be_even hook should accept even integers."""
        program = Program(())
        engine = Engine(program)

        def must_be_even(eng, varid, other):
            # Accept non-integers
            if not isinstance(other, Int):
                return True
            # Check if even
            return other.value % 2 == 0

        engine.register_attr_hook("must_be_even", must_be_even)

        # Try to unify with even number
        query = "?- put_attr(X, must_be_even, true), X = 4."
        solutions = list(engine.query(query))

        # Should succeed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(4)

    def test_must_be_even_accepts_non_integers(self):
        """must_be_even hook should accept non-integers."""
        program = Program(())
        engine = Engine(program)

        def must_be_even(eng, varid, other):
            # Accept non-integers
            if not isinstance(other, Int):
                return True
            # Check if even
            return other.value % 2 == 0

        engine.register_attr_hook("must_be_even", must_be_even)

        # Try to unify with atom
        query = "?- put_attr(X, must_be_even, true), X = foo."
        solutions = list(engine.query(query))

        # Should succeed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("foo")

        # Try to unify with structure
        query = "?- put_attr(X, must_be_even, true), X = f(a, b)."
        solutions = list(engine.query(query))

        # Should succeed
        assert len(solutions) == 1
        assert isinstance(solutions[0]["X"], Struct)


class TestVarVarUnification:
    """Test var-var unification attribute handling (Phase 4.3)."""

    def test_attributes_merge_during_var_var_unification(self):
        """Attributes from both variables should merge during var-var unification.

        When two attributed variables are unified, their attributes are merged
        correctly through the var-var aliasing mechanism implemented in PR #118.
        """
        program = Program(())
        engine = Engine(program)

        # Register hooks for both modules
        hook_calls = []

        def hook_a(eng, varid, other):
            hook_calls.append(("a", varid, other))
            return True

        def hook_b(eng, varid, other):
            hook_calls.append(("b", varid, other))
            return True

        engine.register_attr_hook("mod_a", hook_a)
        engine.register_attr_hook("mod_b", hook_b)

        # This query puts different attributes on X and Y, then unifies them
        # After unification, both attributes should be on the root
        query = (
            "?- put_attr(X, mod_a, data_a), put_attr(Y, mod_b, data_b), X = Y, X = 42."
        )
        solutions = list(engine.query(query))

        # The query should succeed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(42)

        # Both hooks should have been called (after proper merging)
        assert len(hook_calls) == 2, f"Expected 2 hooks, got {len(hook_calls)}"
        modules_called = {call[0] for call in hook_calls}
        assert modules_called == {"a", "b"}, "Both module hooks should be called"


class TestBacktracking:
    """Test that hooks work correctly with backtracking."""

    def test_backtrack_restores_state_after_hook_failure(self):
        """Backtracking should restore state after hook rejection."""
        program = Program(())
        engine = Engine(program)

        def even_hook(eng, varid, other):
            if not isinstance(other, Int):
                return True
            return other.value % 2 == 0

        engine.register_attr_hook("even", even_hook)

        # Try odd (fails), then even (succeeds)
        query = "?- put_attr(X, even, true), (X = 3 ; X = 4)."
        solutions = list(engine.query(query))

        # Should get one solution with X = 4
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(4)

    def test_hook_state_preserved_across_backtrack(self):
        """Hooks should maintain correct state across backtracking."""
        program = Program(())
        engine = Engine(program)

        def range_hook(eng, varid, other):
            # Accept values 1-5
            if not isinstance(other, Int):
                return True
            return 1 <= other.value <= 5

        engine.register_attr_hook("range", range_hook)

        # Try multiple values through backtracking
        query = "?- put_attr(X, range, true), (X = 0 ; X = 3 ; X = 6 ; X = 5)."
        solutions = list(engine.query(query))

        # Should get X = 3 and X = 5
        assert len(solutions) == 2
        values = [sol["X"].value for sol in solutions]
        assert 3 in values
        assert 5 in values
