"""Tests for dict unification implementation.

This module tests dict unification following SWI-Prolog semantics:
- Dicts unify if they have identical key sets and all corresponding values unify
- Key sets must match exactly (no partial matching)
- Values are unified recursively
"""

from prolog.ast.terms import PrologDict, Atom, Int, Var, Struct
from prolog.unify.unify import unify, bind
from prolog.unify.store import Store


class TestDictUnificationBasics:
    """Test basic dict unification behavior."""

    def test_identical_dicts_unify(self):
        """Test that identical dicts unify successfully."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True

    def test_empty_dicts_unify(self):
        """Test that empty dicts unify successfully."""
        d1 = PrologDict(())
        d2 = PrologDict(())
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True

    def test_dict_order_independence(self):
        """Test that dict creation order doesn't affect unification."""
        # Same pairs, different creation order
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("b"), Int(2)), (Atom("a"), Int(1))))
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True


class TestDictUnificationFailures:
    """Test cases where dict unification should fail."""

    def test_different_key_sets_fail(self):
        """Test that dicts with different key sets fail to unify."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("a"), Int(1)), (Atom("c"), Int(3))))
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is False

    def test_different_lengths_fail(self):
        """Test that dicts of different lengths fail to unify."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("a"), Int(1)),))
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is False

    def test_subset_keys_fail(self):
        """Test that dict with subset of keys fails to unify."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("a"), Int(1)),))  # Only has 'a', missing 'b'
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is False

    def test_same_keys_different_values_fail(self):
        """Test that dicts with same keys but different values fail."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(
            ((Atom("a"), Int(1)), (Atom("b"), Int(99)))
        )  # Different value for 'b'
        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is False


class TestDictVariableUnification:
    """Test dict unification with variables."""

    def test_dict_unifies_with_variable(self):
        """Test that dict can unify with an unbound variable."""
        d = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        store = Store()
        var_id = store.new_var("X")
        var = Var(var_id, "X")
        trail = []

        result = unify(d, var, store, trail)
        assert result is True

        # Variable should be bound to the dict
        deref_result = store.deref(var_id)
        assert deref_result[0] == "BOUND"
        assert deref_result[2] == d

    def test_variable_unifies_with_dict(self):
        """Test symmetry: variable can unify with dict."""
        d = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        store = Store()
        var_id = store.new_var("X")
        var = Var(var_id, "X")
        trail = []

        result = unify(var, d, store, trail)  # Reversed order
        assert result is True

        # Variable should be bound to the dict
        deref_result = store.deref(var_id)
        assert deref_result[0] == "BOUND"
        assert deref_result[2] == d

    def test_dict_values_with_variables(self):
        """Test that dict values can contain variables that unify."""
        store = Store()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")
        x_var = Var(x_id, "X")
        y_var = Var(y_id, "Y")

        d1 = PrologDict(((Atom("a"), x_var), (Atom("b"), y_var)))
        d2 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True

        # Variables should be bound to the corresponding values
        x_deref = store.deref(x_id)
        y_deref = store.deref(y_id)
        assert x_deref[0] == "BOUND" and x_deref[2] == Int(1)
        assert y_deref[0] == "BOUND" and y_deref[2] == Int(2)

    def test_dict_values_variable_unification_failure(self):
        """Test that unification fails when dict values can't unify."""
        store = Store()
        x_id = store.new_var("X")
        x_var = Var(x_id, "X")

        # Bind X to a value that conflicts with dict unification
        bind(store, x_id, Int(999), trail=[])

        d1 = PrologDict(((Atom("a"), x_var),))  # X is bound to 999
        d2 = PrologDict(((Atom("a"), Int(1)),))  # Trying to unify with 1
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is False


class TestDictComplexUnification:
    """Test complex dict unification scenarios."""

    def test_nested_dicts_unify(self):
        """Test that nested dicts unify recursively."""
        inner1 = PrologDict(((Atom("x"), Int(1)),))
        inner2 = PrologDict(((Atom("x"), Int(1)),))
        outer1 = PrologDict(((Atom("nested"), inner1),))
        outer2 = PrologDict(((Atom("nested"), inner2),))

        store = Store()
        trail = []

        result = unify(outer1, outer2, store, trail)
        assert result is True

    def test_nested_dicts_different_values_fail(self):
        """Test that nested dicts with different values fail to unify."""
        inner1 = PrologDict(((Atom("x"), Int(1)),))
        inner2 = PrologDict(((Atom("x"), Int(2)),))  # Different value
        outer1 = PrologDict(((Atom("nested"), inner1),))
        outer2 = PrologDict(((Atom("nested"), inner2),))

        store = Store()
        trail = []

        result = unify(outer1, outer2, store, trail)
        assert result is False

    def test_dict_with_struct_values(self):
        """Test dict unification with compound term values."""
        struct1 = Struct("f", (Int(1), Int(2)))
        struct2 = Struct("f", (Int(1), Int(2)))
        d1 = PrologDict(((Atom("data"), struct1),))
        d2 = PrologDict(((Atom("data"), struct2),))

        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True

    def test_dict_mixed_key_types(self):
        """Test dict unification with mixed atom/integer keys."""
        d1 = PrologDict(((Atom("name"), Atom("alice")), (Int(42), Atom("answer"))))
        d2 = PrologDict(((Atom("name"), Atom("alice")), (Int(42), Atom("answer"))))

        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True

    def test_dict_mixed_keys_wrong_order_still_unify(self):
        """Test that canonical ordering ensures unification works."""
        # Create with different key ordering - should still unify due to canonical sorting
        d1 = PrologDict(((Int(42), Atom("answer")), (Atom("name"), Atom("alice"))))
        d2 = PrologDict(((Atom("name"), Atom("alice")), (Int(42), Atom("answer"))))

        store = Store()
        trail = []

        result = unify(d1, d2, store, trail)
        assert result is True


class TestDictUnificationTrailing:
    """Test that dict unification properly handles backtracking."""

    def test_dict_unification_trails_properly(self):
        """Test that failed dict unification can be undone."""
        store = Store()
        x_id = store.new_var("X")
        x_var = Var(x_id, "X")
        trail = []

        # First, unify X with a dict
        d1 = PrologDict(((Atom("a"), Int(1)),))
        result1 = unify(x_var, d1, store, trail)
        assert result1 is True

        # X should be bound
        deref_result = store.deref(x_id)
        assert deref_result[0] == "BOUND"

        # Now try to unify X with an incompatible dict - should fail and undo
        d2 = PrologDict(((Atom("b"), Int(2)),))  # Different key
        result2 = unify(x_var, d2, store, trail)
        assert result2 is False

        # X should still be bound to d1 (unification failure undoes changes)
        deref_result = store.deref(x_id)
        assert deref_result[0] == "BOUND"
        assert deref_result[2] == d1
