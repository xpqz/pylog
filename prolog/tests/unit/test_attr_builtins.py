"""Tests for Stage 4 Phase 2: Attribute builtin predicates.

Tests put_attr/3, get_attr/3, and del_attr/2 builtins.
"""

import pytest
from prolog.engine.engine import Engine
from prolog.ast.clauses import Clause, Program
from prolog.ast.terms import Atom, Int, Var, Struct


class TestPutAttr:
    """Test put_attr/3 builtin predicate."""

    def test_put_attr_unbound_var_succeeds(self):
        """put_attr/3 with unbound variable should succeed."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red).
        solutions = list(engine.query("?- put_attr(X, color, red)."))
        assert len(solutions) == 1
        assert solutions[0] == {}  # Success with no visible bindings

    def test_put_attr_bound_var_fails(self):
        """put_attr/3 with variable that derefs to bound term should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- X = 42, put_attr(X, color, red).
        solutions = list(engine.query("?- X = 42, put_attr(X, color, red)."))
        assert len(solutions) == 0  # Fails because X is bound to 42

    def test_put_attr_non_atom_module_fails(self):
        """put_attr/3 with non-atom module should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, 123, red).  (module must be atom)
        solutions = list(engine.query("?- put_attr(X, 123, red)."))
        assert len(solutions) == 0

        # ?- put_attr(X, [a,b], red).  (module must be atom)
        solutions = list(engine.query("?- put_attr(X, [a,b], red)."))
        assert len(solutions) == 0

    def test_put_attr_trails_change(self):
        """put_attr/3 should trail attribute changes for backtracking."""
        program = Program(())
        engine = Engine(program)

        # Test backtracking restores state
        # ?- (put_attr(X, color, red), fail) ; (get_attr(X, color, _) -> fail ; true).
        # This should succeed because after backtrack, X has no color attribute
        query = "?- (put_attr(X, color, red), fail) ; (get_attr(X, color, _) -> fail ; true)."
        solutions = list(engine.query(query))
        assert len(solutions) == 1

    def test_put_attr_wrong_arity_fails(self):
        """put_attr with wrong arity should fail."""
        program = Program(())
        engine = Engine(program)

        # put_attr/2 doesn't exist
        solutions = list(engine.query("?- put_attr(X, color)."))
        assert len(solutions) == 0

        # put_attr/4 doesn't exist
        solutions = list(engine.query("?- put_attr(X, color, red, extra)."))
        assert len(solutions) == 0

    def test_put_attr_variable_module_fails(self):
        """put_attr/3 with variable module should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, M, value).  (module must be ground atom)
        solutions = list(engine.query("?- put_attr(X, M, value)."))
        assert len(solutions) == 0

    def test_put_attr_overwrites_existing(self):
        """put_attr/3 should overwrite existing attribute value."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), put_attr(X, color, blue), get_attr(X, color, C).
        solutions = list(engine.query(
            "?- put_attr(X, color, red), put_attr(X, color, blue), get_attr(X, color, C)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("blue")


class TestGetAttr:
    """Test get_attr/3 builtin predicate."""

    def test_get_attr_retrieves_existing(self):
        """get_attr/3 should retrieve existing attribute."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), get_attr(X, color, C).
        solutions = list(engine.query("?- put_attr(X, color, red), get_attr(X, color, C)."))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("red")

    def test_get_attr_fails_for_missing(self):
        """get_attr/3 should fail for missing attribute."""
        program = Program(())
        engine = Engine(program)

        # ?- get_attr(X, color, C).  (X has no attributes)
        solutions = list(engine.query("?- get_attr(X, color, C)."))
        assert len(solutions) == 0

        # ?- put_attr(X, size, 10), get_attr(X, color, C).  (X has size but not color)
        solutions = list(engine.query("?- put_attr(X, size, 10), get_attr(X, color, C)."))
        assert len(solutions) == 0

    def test_get_attr_unifies_third_arg(self):
        """get_attr/3 should unify third arg with attribute value."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), get_attr(X, color, red).  (check mode)
        solutions = list(engine.query("?- put_attr(X, color, red), get_attr(X, color, red)."))
        assert len(solutions) == 1

        # ?- put_attr(X, color, red), get_attr(X, color, blue).  (fails to unify)
        solutions = list(engine.query("?- put_attr(X, color, red), get_attr(X, color, blue)."))
        assert len(solutions) == 0

    def test_get_attr_bound_var_fails(self):
        """get_attr/3 with variable that derefs to bound term should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- X = 42, get_attr(X, color, C).
        solutions = list(engine.query("?- X = 42, get_attr(X, color, C)."))
        assert len(solutions) == 0

    def test_get_attr_non_atom_module_fails(self):
        """get_attr/3 with non-atom module should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), get_attr(X, 123, C).
        solutions = list(engine.query("?- put_attr(X, color, red), get_attr(X, 123, C)."))
        assert len(solutions) == 0

    def test_get_attr_wrong_arity_fails(self):
        """get_attr with wrong arity should fail."""
        program = Program(())
        engine = Engine(program)

        # get_attr/2 doesn't exist
        solutions = list(engine.query("?- get_attr(X, color)."))
        assert len(solutions) == 0

        # get_attr/4 doesn't exist
        solutions = list(engine.query("?- get_attr(X, color, C, extra)."))
        assert len(solutions) == 0


class TestDelAttr:
    """Test del_attr/2 builtin predicate."""

    def test_del_attr_removes_existing(self):
        """del_attr/2 should remove existing attribute."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), del_attr(X, color), get_attr(X, color, C).
        # Should fail because color was deleted
        solutions = list(engine.query(
            "?- put_attr(X, color, red), del_attr(X, color), get_attr(X, color, C)."
        ))
        assert len(solutions) == 0

    def test_del_attr_succeeds_for_missing(self):
        """del_attr/2 should succeed even for missing attribute."""
        program = Program(())
        engine = Engine(program)

        # ?- del_attr(X, color).  (X has no attributes)
        solutions = list(engine.query("?- del_attr(X, color)."))
        assert len(solutions) == 1

        # ?- put_attr(X, size, 10), del_attr(X, color).  (X has size but not color)
        solutions = list(engine.query("?- put_attr(X, size, 10), del_attr(X, color)."))
        assert len(solutions) == 1

    def test_del_attr_trails_deletion(self):
        """del_attr/2 should trail deletion for backtracking."""
        program = Program(())
        engine = Engine(program)

        # Test backtracking restores deleted attribute
        # ?- put_attr(X, color, red), (del_attr(X, color), fail) ; get_attr(X, color, C).
        query = "?- put_attr(X, color, red), (del_attr(X, color), fail) ; get_attr(X, color, C)."
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("red")

    def test_del_attr_bound_var_fails(self):
        """del_attr/2 with variable that derefs to bound term should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- X = 42, del_attr(X, color).
        solutions = list(engine.query("?- X = 42, del_attr(X, color)."))
        assert len(solutions) == 0

    def test_del_attr_non_atom_module_fails(self):
        """del_attr/2 with non-atom module should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- del_attr(X, 123).
        solutions = list(engine.query("?- del_attr(X, 123)."))
        assert len(solutions) == 0

        # ?- del_attr(X, [a,b]).
        solutions = list(engine.query("?- del_attr(X, [a,b])."))
        assert len(solutions) == 0

    def test_del_attr_wrong_arity_fails(self):
        """del_attr with wrong arity should fail."""
        program = Program(())
        engine = Engine(program)

        # del_attr/1 doesn't exist
        solutions = list(engine.query("?- del_attr(X)."))
        assert len(solutions) == 0

        # del_attr/3 doesn't exist
        solutions = list(engine.query("?- del_attr(X, color, extra)."))
        assert len(solutions) == 0


class TestAttributeIntegration:
    """Integration tests for attribute builtins working together."""

    def test_put_then_get_retrieves_value(self):
        """put_attr then get_attr should retrieve the value."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, module1, value1), get_attr(X, module1, V).
        solutions = list(engine.query(
            "?- put_attr(X, module1, value1), get_attr(X, module1, V)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["V"] == Atom("value1")

    def test_put_del_get_fails(self):
        """put_attr then del_attr then get_attr should fail."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, m, v), del_attr(X, m), get_attr(X, m, V).
        solutions = list(engine.query(
            "?- put_attr(X, m, v), del_attr(X, m), get_attr(X, m, V)."
        ))
        assert len(solutions) == 0

    def test_multiple_modules_on_same_variable(self):
        """Multiple modules should coexist on same variable."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), put_attr(X, size, 10),
        #    get_attr(X, color, C), get_attr(X, size, S).
        solutions = list(engine.query(
            "?- put_attr(X, color, red), put_attr(X, size, 10), " +
            "get_attr(X, color, C), get_attr(X, size, S)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("red")
        assert solutions[0]["S"] == Int(10)

    def test_backtrack_restores_attributes(self):
        """Backtracking should restore attribute state."""
        program = Program(())
        engine = Engine(program)

        # Test complex backtrack scenario
        # ?- put_attr(X, a, 1),
        #    (put_attr(X, b, 2), put_attr(X, a, 3), fail) ;
        #    (get_attr(X, a, A), (get_attr(X, b, _) -> fail ; true)).
        query = """?- put_attr(X, a, 1),
                     (put_attr(X, b, 2), put_attr(X, a, 3), fail) ;
                     (get_attr(X, a, A), (get_attr(X, b, _) -> fail ; true))."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["A"] == Int(1)  # Original value restored

    def test_attributes_visible_through_aliases(self):
        """Attributes should be visible through variable aliases."""
        program = Program(())
        engine = Engine(program)

        # ?- put_attr(X, color, red), Y = X, get_attr(Y, color, C).
        solutions = list(engine.query(
            "?- put_attr(X, color, red), Y = X, get_attr(Y, color, C)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("red")

        # Also test setting through alias
        # ?- X = Y, put_attr(X, color, blue), get_attr(Y, color, C).
        solutions = list(engine.query(
            "?- X = Y, put_attr(X, color, blue), get_attr(Y, color, C)."
        ))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Atom("blue")

    def test_del_attr_through_alias(self):
        """del_attr should work through variable aliases."""
        program = Program(())
        engine = Engine(program)

        # Set attribute on X, unify Y = X, delete through Y, verify X has no attribute
        # ?- put_attr(X, test, value), Y = X, del_attr(Y, test),
        #    (get_attr(X, test, _) -> fail ; true).
        query = """?- put_attr(X, test, value), Y = X, del_attr(Y, test),
                     (get_attr(X, test, _) -> fail ; true)."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1  # Deletion through alias succeeded

    def test_all_builtins_recognized(self):
        """All three builtins should be recognized in queries."""
        program = Program(())
        engine = Engine(program)

        # Create a test that uses all three in sequence
        query = """?- put_attr(X, test, initial),
                     get_attr(X, test, V1),
                     V1 = initial,
                     put_attr(X, test, modified),
                     get_attr(X, test, V2),
                     V2 = modified,
                     del_attr(X, test),
                     (get_attr(X, test, _) -> fail ; true)."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1

    def test_attributes_with_complex_terms(self):
        """Attributes can be complex terms, not just atoms."""
        program = Program(())
        engine = Engine(program)

        # Test with list attribute
        solutions = list(engine.query(
            "?- put_attr(X, data, [1,2,3]), get_attr(X, data, D)."
        ))
        assert len(solutions) == 1
        # D should be the list [1,2,3]

        # Test with structure attribute
        solutions = list(engine.query(
            "?- put_attr(X, info, point(3,4)), get_attr(X, info, I)."
        ))
        assert len(solutions) == 1
        # I should be point(3,4)

        # Test with variable in attribute (should work)
        solutions = list(engine.query(
            "?- put_attr(X, ref, Y), get_attr(X, ref, R), R = 42, Y = Z."
        ))
        assert len(solutions) == 1
        assert solutions[0]["Y"] == Int(42)
        assert solutions[0]["Z"] == Int(42)
        assert solutions[0]["R"] == Int(42)