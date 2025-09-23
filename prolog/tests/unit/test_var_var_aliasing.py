"""Tests for Stage 4 Phase 4: Var-Var Aliasing with Attribute Merging.

Tests attribute merging when unifying two variables, ensuring attributes
migrate to the union-find root with proper trailing.
"""

import pytest
from prolog.engine.engine import Engine
from prolog.ast.clauses import Clause, Program
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.unify.store import Store, Cell
from prolog.unify.unify import unify


class TestAttributeVisibilityThroughAliases:
    """Test that attributes are visible through variable aliases."""

    def test_self_unification_with_attributes(self):
        """X=X should succeed when X has attributes (edge case)."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, test, value), X = X, get_attr(X, test, V).
        solutions = list(engine.query(
            "?- put_attr(X, test, value), X = X, get_attr(X, test, V)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["V"] == Atom("value")
        # X should still have its attribute after self-unification

    def test_attribute_visible_after_alias(self):
        """Attributes should be visible through alias after X = Y."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), X = Y, get_attr(Y, color, C).
        solutions = list(engine.query(
            "?- put_attr(X, color, red), X = Y, get_attr(Y, color, C)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("red")

    def test_put_on_x_get_on_y_after_unify(self):
        """put_attr on X, unify X=Y, get_attr on Y should work."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, size, 10), X = Y, get_attr(Y, size, S).
        solutions = list(engine.query(
            "?- put_attr(X, size, 10), X = Y, get_attr(Y, size, S)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["S"] == Int(10)

    def test_attributes_follow_union_find_root(self):
        """Attributes should follow the union-find root after aliasing."""
        program = Program(())
        engine = Engine(program)

        # Create chain: X=Y, Y=Z with attribute on X
        # ?- put_attr(X, test, value), X = Y, Y = Z, get_attr(Z, test, V).
        solutions = list(engine.query(
            "?- put_attr(X, test, value), X = Y, Y = Z, get_attr(Z, test, V)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["V"] == Atom("value")

    def test_only_root_carries_attributes(self):
        """After aliasing, only the union-find root should carry attributes."""
        store = Store()
        trail = []

        # Create two variables
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # Add attribute to X
        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"color": Atom("red")}

        # Unify X and Y
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Find the root
        x_result = store.deref(x_id)
        y_result = store.deref(y_id)

        # Both should deref to same root
        assert x_result[1] == y_result[1]
        root_id = x_result[1]

        # Only root should have attributes
        assert root_id in store.attrs
        assert "color" in store.attrs[root_id]
        assert store.attrs[root_id]["color"] == Atom("red")

        # Non-root should not have attributes
        non_root = x_id if x_id != root_id else y_id
        assert non_root not in store.attrs or not store.attrs[non_root]


class TestAttributeMerging:
    """Test attribute merging when unifying two attributed variables."""

    def test_merge_non_overlapping_attributes(self):
        """Non-overlapping attributes should merge to root."""
        program = Program(())
        engine = Engine(program)

        # X has color, Y has size, after X=Y both accessible
        # ?- put_attr(X, color, red), put_attr(Y, size, 10),
        #    X = Y, get_attr(X, color, C), get_attr(X, size, S).
        solutions = list(engine.query(
            "?- put_attr(X, color, red), put_attr(Y, size, 10), " +
            "X = Y, get_attr(X, color, C), get_attr(X, size, S)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("red")
        assert solutions[0]["S"] == Int(10)

    def test_merge_overlapping_attributes_calls_hook(self):
        """Overlapping attributes should trigger hook during merge."""
        program = Program(())
        engine = Engine(program)

        # Register a hook that allows merging if both values are the same
        def same_value_hook(engine, varid, other):
            # When merging two variables, 'other' will be a Var
            if isinstance(other, Var):
                other_result = engine.store.deref(other.id)
                if other_result[0] == "UNBOUND":
                    other_id = other_result[1]
                    if other_id in engine.store.attrs and "test" in engine.store.attrs[other_id]:
                        my_value = engine.store.attrs[varid]["test"]
                        other_value = engine.store.attrs[other_id]["test"]
                        # Only allow merge if values are the same
                        return my_value == other_value
            return True

        engine.register_attr_hook("test", same_value_hook)

        # Same values should merge successfully
        # ?- put_attr(X, test, same), put_attr(Y, test, same), X = Y.
        solutions = list(engine.query(
            "?- put_attr(X, test, same), put_attr(Y, test, same), X = Y."
        ))
        assert len(solutions) == 1

        # Different values should fail merge
        # ?- put_attr(X, test, val1), put_attr(Y, test, val2), X = Y.
        solutions = list(engine.query(
            "?- put_attr(X, test, val1), put_attr(Y, test, val2), X = Y."
        ))
        assert len(solutions) == 0

    def test_child_attributes_cleared_after_merge(self):
        """Child's attributes should be cleared after merge."""
        store = Store()
        trail = []

        # Create two variables
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # Add different attributes to each
        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"color": Atom("red")}
        store.attrs[y_id] = {"size": Int(10)}

        # Unify X and Y
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Find root and child
        x_result = store.deref(x_id)
        y_result = store.deref(y_id)
        root_id = x_result[1]

        # Determine which was the child
        child_id = x_id if x_id != root_id else y_id

        # Root should have both attributes
        assert "color" in store.attrs[root_id]
        assert "size" in store.attrs[root_id]

        # Child should have no attributes
        assert child_id not in store.attrs or not store.attrs[child_id]

    def test_overlapping_modules_properly_trailed(self):
        """Overlapping modules should be properly trailed for backtracking."""
        program = Program(())
        engine = Engine(program)

        # Register hook that always accepts
        engine.register_attr_hook("overlap", lambda e, v, o: True)

        # Test backtracking restores original attributes
        # ?- put_attr(X, overlap, xval), put_attr(Y, overlap, yval),
        #    ((X = Y, fail) ; (get_attr(X, overlap, Xv), get_attr(Y, overlap, Yv))).
        query = """?- put_attr(X, overlap, xval), put_attr(Y, overlap, yval),
                     ((X = Y, fail) ; (get_attr(X, overlap, Xv), get_attr(Y, overlap, Yv)))."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["Xv"] == Atom("xval")
        assert solutions[0]["Yv"] == Atom("yval")


class TestHookDispatchForAliasing:
    """Test hook dispatch when aliasing attributed variables."""

    def test_hooks_called_for_overlapping_modules(self):
        """Hooks should be called for overlapping modules during aliasing."""
        program = Program(())
        engine = Engine(program)

        hook_called = []

        def track_hook(engine, varid, other):
            hook_called.append((varid, other))
            return True

        engine.register_attr_hook("tracked", track_hook)

        # ?- put_attr(X, tracked, x), put_attr(Y, tracked, y), X = Y.
        solutions = list(engine.query(
            "?- put_attr(X, tracked, x), put_attr(Y, tracked, y), X = Y."
        ))
        assert len(solutions) == 1
        # Hook should have been called for the overlapping module
        assert len(hook_called) > 0

    def test_hook_can_veto_aliasing(self):
        """Either hook can veto the aliasing operation."""
        program = Program(())
        engine = Engine(program)

        def veto_hook(engine, varid, other):
            # Veto if merging with another variable that has this attribute
            if isinstance(other, Var):
                other_result = engine.store.deref(other.id)
                if other_result[0] == "UNBOUND":
                    other_id = other_result[1]
                    if other_id in engine.store.attrs and "veto" in engine.store.attrs[other_id]:
                        return False  # Veto the merge
            return True

        engine.register_attr_hook("veto", veto_hook)

        # Should fail due to veto
        # ?- put_attr(X, veto, x), put_attr(Y, veto, y), X = Y.
        solutions = list(engine.query(
            "?- put_attr(X, veto, x), put_attr(Y, veto, y), X = Y."
        ))
        assert len(solutions) == 0

    def test_non_overlapping_modules_dont_interfere(self):
        """Non-overlapping modules should not interfere with aliasing."""
        program = Program(())
        engine = Engine(program)

        blocking_called = False

        def blocking_hook(engine, varid, other):
            nonlocal blocking_called
            blocking_called = True
            return False  # Would block if called

        engine.register_attr_hook("blocker", blocking_hook)

        # Should succeed because modules don't overlap
        # ?- put_attr(X, color, red), put_attr(Y, size, 10), X = Y.
        solutions = list(engine.query(
            "?- put_attr(X, color, red), put_attr(Y, size, 10), X = Y."
        ))
        assert len(solutions) == 1
        assert not blocking_called  # Hook should not have been called

    def test_hooks_see_both_variables(self):
        """Hooks should see both variables during aliasing."""
        program = Program(())
        engine = Engine(program)

        seen_vars = []

        def observer_hook(engine, varid, other):
            seen_vars.append((varid, other))
            return True

        engine.register_attr_hook("observer", observer_hook)

        # ?- put_attr(X, observer, x), put_attr(Y, observer, y), X = Y.
        solutions = list(engine.query(
            "?- put_attr(X, observer, x), put_attr(Y, observer, y), X = Y."
        ))
        assert len(solutions) == 1

        # Hook should have seen the variables
        assert len(seen_vars) > 0
        # The 'other' should be a Var in var-var unification
        assert any(isinstance(other, Var) for _, other in seen_vars)


class TestComplexAliasingScenarios:
    """Test complex aliasing scenarios with attributes."""

    def test_chain_of_aliases_with_attrs(self):
        """Chains of aliases X=Y, Y=Z should handle attributes correctly."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, a, 1), put_attr(Y, b, 2), put_attr(Z, c, 3),
        #    X = Y, Y = Z,
        #    get_attr(Z, a, A), get_attr(Z, b, B), get_attr(Z, c, C).
        query = """?- put_attr(X, a, 1), put_attr(Y, b, 2), put_attr(Z, c, 3),
                     X = Y, Y = Z,
                     get_attr(Z, a, A), get_attr(Z, b, B), get_attr(Z, c, C)."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["A"] == Int(1)
        assert solutions[0]["B"] == Int(2)
        assert solutions[0]["C"] == Int(3)

    def test_multiple_modules_across_multiple_variables(self):
        """Multiple modules across multiple variables should merge correctly."""
        program = Program(())
        engine = Engine(program)

        # Complex scenario with 3 variables, multiple attributes each
        # ?- put_attr(X, m1, x1), put_attr(X, m2, x2),
        #    put_attr(Y, m2, y2), put_attr(Y, m3, y3),
        #    put_attr(Z, m3, z3), put_attr(Z, m4, z4),
        #    X = Y, Y = Z,
        #    get_attr(X, m1, V1), get_attr(X, m2, V2),
        #    get_attr(X, m3, V3), get_attr(X, m4, V4).
        query = """?- put_attr(X, m1, x1), put_attr(X, m2, x2),
                     put_attr(Y, m2, y2), put_attr(Y, m3, y3),
                     put_attr(Z, m3, z3), put_attr(Z, m4, z4),
                     X = Y, Y = Z,
                     get_attr(X, m1, V1), get_attr(X, m2, V2),
                     get_attr(X, m3, V3), get_attr(X, m4, V4)."""

        # For overlapping modules, we need hooks that allow merge
        for module in ["m2", "m3"]:
            engine.register_attr_hook(module, lambda e, v, o: True)

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["V1"] == Atom("x1")
        # m2 was on both X and Y, one value should win (implementation dependent)
        assert solutions[0]["V2"] in [Atom("x2"), Atom("y2")]
        # m3 was on both Y and Z, one value should win
        assert solutions[0]["V3"] in [Atom("y3"), Atom("z3")]
        assert solutions[0]["V4"] == Atom("z4")

    def test_circular_aliasing_with_attributes(self):
        """Circular aliasing should maintain attributes on current root."""
        program = Program(())
        engine = Engine(program)

        # Create a circular reference through aliasing
        # ?- put_attr(X, test, xval), X = Y, Y = Z, Z = X,
        #    get_attr(X, test, V1), get_attr(Y, test, V2), get_attr(Z, test, V3).
        query = """?- put_attr(X, test, xval), X = Y, Y = Z, Z = X,
                     get_attr(X, test, V1), get_attr(Y, test, V2), get_attr(Z, test, V3)."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        # All should see the same value through the root
        assert solutions[0]["V1"] == Atom("xval")
        assert solutions[0]["V2"] == Atom("xval")
        assert solutions[0]["V3"] == Atom("xval")

    def test_attributes_always_on_current_root(self):
        """Attributes should always be on the current union-find root."""
        store = Store()
        trail = []

        # Create three variables
        x_id = store.new_var("X")
        y_id = store.new_var("Y")
        z_id = store.new_var("Z")

        # Add attributes to each
        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"a": Int(1)}
        store.attrs[y_id] = {"b": Int(2)}
        store.attrs[z_id] = {"c": Int(3)}

        # Unify X and Y
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Find current root
        root1 = store.deref(x_id)[1]
        assert "a" in store.attrs[root1]
        assert "b" in store.attrs[root1]

        # Now unify with Z
        z_var = Var(z_id, "Z")
        assert unify(x_var, z_var, store, trail)

        # Find new root (might have changed)
        final_root = store.deref(x_id)[1]
        assert "a" in store.attrs[final_root]
        assert "b" in store.attrs[final_root]
        assert "c" in store.attrs[final_root]

        # No other variable should have attributes
        for vid in [x_id, y_id, z_id]:
            if vid != final_root:
                assert vid not in store.attrs or not store.attrs[vid]


class TestBacktrackingThroughAliases:
    """Test that backtracking correctly restores attribute state."""

    def test_backtrack_restores_separation(self):
        """Backtracking should restore variable separation and attributes."""
        program = Program(())
        engine = Engine(program)

        # After backtracking, X and Y should be separate with original attributes
        # ?- put_attr(X, test, xval), put_attr(Y, test, yval),
        #    ((X = Y, get_attr(X, test, Merged), fail) ;
        #     (get_attr(X, test, Xv), get_attr(Y, test, Yv))).
        query = """?- put_attr(X, test, xval), put_attr(Y, test, yval),
                     ((X = Y, get_attr(X, test, Merged), fail) ;
                      (get_attr(X, test, Xv), get_attr(Y, test, Yv)))."""

        # Register hook that allows merging
        engine.register_attr_hook("test", lambda e, v, o: True)

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["Xv"] == Atom("xval")
        assert solutions[0]["Yv"] == Atom("yval")
        # After backtracking, X and Y are separate with their original attributes

    def test_complex_backtrack_through_chain(self):
        """Complex backtracking through alias chains should restore state."""
        program = Program(())
        engine = Engine(program)

        # Test complex scenario with multiple unifications and backtrack
        query = """?- put_attr(X, a, 1), put_attr(Y, b, 2), put_attr(Z, c, 3),
                     ((X = Y, Y = Z, fail) ;
                      (get_attr(X, a, A), get_attr(Y, b, B), get_attr(Z, c, C)))."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["A"] == Int(1)
        assert solutions[0]["B"] == Int(2)
        assert solutions[0]["C"] == Int(3)


class TestIntegrationWithBuiltins:
    """Test var-var aliasing integration with existing builtins."""

    def test_aliasing_with_put_get_del(self):
        """Aliasing should work correctly with put_attr, get_attr, del_attr."""
        program = Program(())
        engine = Engine(program)

        # Complex scenario mixing aliasing with attribute operations
        # ?- put_attr(X, test, initial),
        #    X = Y,  % Now Y is alias of X
        #    get_attr(Y, test, V1),  % Should see initial
        #    put_attr(Y, test, modified),  % Modify through alias
        #    get_attr(X, test, V2),  % X should see modification
        #    del_attr(X, test),  % Delete through X
        #    (get_attr(Y, test, _) -> fail ; true).  % Y shouldn't have it
        query = """?- put_attr(X, test, initial),
                     X = Y,
                     get_attr(Y, test, V1),
                     put_attr(Y, test, modified),
                     get_attr(X, test, V2),
                     del_attr(X, test),
                     (get_attr(Y, test, _) -> fail ; true)."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["V1"] == Atom("initial")
        assert solutions[0]["V2"] == Atom("modified")

    def test_aliasing_in_predicate_context(self):
        """Aliasing should work in predicate context with attributes."""
        program = Program((
            Clause.from_str("merge_colors(X, Y) :- "
                          "put_attr(X, color, red), "
                          "put_attr(Y, color, blue), "
                          "X = Y."),
            Clause.from_str("check_merge(X, Y, C) :- "
                          "merge_colors(X, Y), "
                          "get_attr(X, color, C)."),
        ))
        engine = Engine(program)

        # Register hook that takes first value
        engine.register_attr_hook("color", lambda e, v, o: True)

        # ?- check_merge(A, B, Color).
        solutions = list(engine.query("?- check_merge(A, B, Color)."))
        assert len(solutions) == 1
        # Should have one of the colors (implementation dependent which wins)
        assert solutions[0]["Color"] in [Atom("red"), Atom("blue")]


class TestRankBasedMerging:
    """Test that union-by-rank determines merge direction."""

    def test_rank_determines_root(self):
        """Union-by-rank should determine which variable becomes root."""
        store = Store()
        trail = []

        # Create two variables
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # Give Y higher rank by creating a chain under it
        z_id = store.new_var("Z")
        z_var = Var(z_id, "Z")
        y_var = Var(y_id, "Y")
        assert unify(z_var, y_var, store, trail)

        # Now Y has rank 1, X has rank 0
        # Add attributes
        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"xattr": Atom("xval")}
        y_root = store.deref(y_id)[1]
        store.attrs[y_root] = {"yattr": Atom("yval")}

        # Unify X with Y - Y should become root due to higher rank
        x_var = Var(x_id, "X")
        assert unify(x_var, y_var, store, trail)

        # Check that Y's root is the final root
        final_root = store.deref(x_id)[1]
        assert final_root == y_root

        # Attributes should be merged at Y's root
        assert "xattr" in store.attrs[final_root]
        assert "yattr" in store.attrs[final_root]

    def test_equal_rank_uses_first_as_root(self):
        """When ranks are equal, first variable should become root."""
        store = Store()
        trail = []

        # Create two variables with equal rank
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # Add attributes
        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"xattr": Atom("xval")}
        store.attrs[y_id] = {"yattr": Atom("yval")}

        # Unify X with Y - X should become root (first arg)
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Check that X is the root
        final_root = store.deref(x_id)[1]
        assert final_root == x_id

        # Attributes should be merged at X
        assert "xattr" in store.attrs[final_root]
        assert "yattr" in store.attrs[final_root]


class TestTrailCorrectness:
    """Test that all attribute operations are properly trailed."""

    def test_deletion_from_child_trailed(self):
        """Deletion of attributes from child should be trailed."""
        store = Store()
        trail = []

        # Mock trail that tracks operations
        class RecordingTrail(list):
            def push_attr(self, varid, module, old_value, stamp=None):
                self.append(("attr", varid, module, old_value))

        trail = RecordingTrail()

        # Create two variables with attributes
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"test": Atom("xval")}
        store.attrs[y_id] = {"test": Atom("yval")}

        # Unify - this should trail the deletion from child
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Check that deletion was trailed
        # Should have entry for removing attribute from child
        attr_trails = [e for e in trail if e[0] == "attr"]
        assert len(attr_trails) > 0

    def test_overwrite_on_root_trailed(self):
        """Overwriting attribute on root should be trailed."""
        store = Store()

        class RecordingTrail(list):
            def push_attr(self, varid, module, old_value, stamp=None):
                self.append(("attr", varid, module, old_value))

        trail = RecordingTrail()

        # Create two variables with same module
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"overlap": Atom("xval")}
        store.attrs[y_id] = {"overlap": Atom("yval")}

        # Unify - root's value should be trailed before overwrite
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Should have trailed the old values
        attr_trails = [e for e in trail if e[0] == "attr"]
        assert len(attr_trails) > 0

    def test_undo_restores_exact_state(self):
        """Undo should restore exact attribute state before unification."""
        from prolog.unify.trail import undo_to

        store = Store()
        trail = []

        # Create variables with attributes
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        if not hasattr(store, "attrs"):
            store.attrs = {}
        store.attrs[x_id] = {"a": Int(1), "b": Int(2)}
        store.attrs[y_id] = {"b": Int(3), "c": Int(4)}

        # Save original state
        orig_x_attrs = dict(store.attrs[x_id])
        orig_y_attrs = dict(store.attrs[y_id])

        # Mark trail position
        mark = len(trail)

        # Unify variables
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")
        assert unify(x_var, y_var, store, trail)

        # Undo to mark
        undo_to(mark, trail, store)

        # Check attributes restored
        assert store.attrs[x_id] == orig_x_attrs
        assert store.attrs[y_id] == orig_y_attrs

        # Variables should be separate again
        x_result = store.deref(x_id)
        y_result = store.deref(y_id)
        assert x_result[1] != y_result[1]