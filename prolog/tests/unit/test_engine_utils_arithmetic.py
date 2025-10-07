"""Unit tests for prolog.engine.utils.arithmetic module."""

import pytest
from prolog.ast.terms import Atom, Int, Float, Var
from prolog.engine.utils.arithmetic import eval_int
from prolog.unify.store import Store
from prolog.unify.unify_helpers import bind_root_to_term, union_vars
from prolog.engine.runtime import Trail


class TestEvalInt:
    """Test eval_int function."""

    def test_eval_int_literal(self):
        """Test evaluating integer literal."""
        store = Store()
        result = eval_int(store, Int(42))
        assert result == 42

    def test_eval_int_negative(self):
        """Test evaluating negative integer."""
        store = Store()
        result = eval_int(store, Int(-17))
        assert result == -17

    def test_eval_int_zero(self):
        """Test evaluating zero."""
        store = Store()
        result = eval_int(store, Int(0))
        assert result == 0

    def test_eval_bound_variable_to_int(self):
        """Test evaluating variable bound to integer."""
        store = Store()
        trail = Trail()
        varid = store.new_var("X")
        bind_root_to_term(varid, Int(99), trail, store)

        result = eval_int(store, Var(varid, "X"))
        assert result == 99

    def test_eval_chain_of_bound_variables(self):
        """Test evaluating chain of bound variables through union-find."""
        store = Store()
        trail = Trail()
        var1_id = store.new_var("X")
        var2_id = store.new_var("Y")

        # Unite X with Y, then bind the unified root to 123
        union_vars(var1_id, var2_id, trail, store)

        # Find the root and bind it
        root_result = store.deref(var1_id)
        if root_result[0] == "UNBOUND":
            _, root_id = root_result
            bind_root_to_term(root_id, Int(123), trail, store)

        result = eval_int(store, Var(var1_id, "X"))
        assert result == 123

    def test_eval_unbound_variable_fails(self):
        """Test that evaluating unbound variable raises ValueError."""
        store = Store()
        varid = store.new_var("X")

        with pytest.raises(ValueError, match="non-integer"):
            eval_int(store, Var(varid, "X"))

    def test_eval_atom_fails(self):
        """Test that evaluating atom raises ValueError."""
        store = Store()

        with pytest.raises(ValueError, match="non-integer"):
            eval_int(store, Atom("hello"))

    def test_eval_float_fails(self):
        """Test that evaluating float raises ValueError."""
        store = Store()

        with pytest.raises(ValueError, match="non-integer"):
            eval_int(store, Float(3.14))

    def test_eval_variable_bound_to_atom_fails(self):
        """Test that variable bound to non-integer fails."""
        store = Store()
        trail = Trail()
        varid = store.new_var("X")
        bind_root_to_term(varid, Atom("not_an_int"), trail, store)

        with pytest.raises(ValueError, match="non-integer"):
            eval_int(store, Var(varid, "X"))

    def test_eval_variable_bound_to_float_fails(self):
        """Test that variable bound to float fails."""
        store = Store()
        trail = Trail()
        varid = store.new_var("X")
        bind_root_to_term(varid, Float(2.5), trail, store)

        with pytest.raises(ValueError, match="non-integer"):
            eval_int(store, Var(varid, "X"))

    def test_eval_large_integer(self):
        """Test evaluating large integer values."""
        store = Store()
        large_int = 999999999999999

        result = eval_int(store, Int(large_int))
        assert result == large_int

    def test_eval_deeply_bound_variables(self):
        """Test evaluating deeply nested variable bindings."""
        store = Store()
        trail = Trail()
        var_ids = [store.new_var(f"X{i}") for i in range(5)]

        # Create chain: X0 -> X1 -> X2 -> X3 -> X4 -> 777
        for i in range(4):
            union_vars(var_ids[i], var_ids[i + 1], trail, store)

        # Find the root and bind it to 777
        root_result = store.deref(var_ids[0])
        if root_result[0] == "UNBOUND":
            _, root_id = root_result
            bind_root_to_term(root_id, Int(777), trail, store)

        result = eval_int(store, Var(var_ids[0], "X0"))
        assert result == 777
