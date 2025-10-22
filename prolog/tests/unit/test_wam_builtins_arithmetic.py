"""Tests for WAM arithmetic builtins."""

import pytest

from prolog.wam.builtins_arithmetic import (
    builtin_is,
    builtin_arith_eq,
    builtin_arith_ne,
    builtin_arith_lt,
    builtin_arith_le,
    builtin_arith_gt,
    builtin_arith_ge,
)
from prolog.wam.errors import EvaluationError, InstantiationError
from prolog.wam.heap import TAG_CON, new_con, new_ref, new_str, note_struct_args
from prolog.wam.machine import Machine
from prolog.wam.unify import deref


def init_machine_with_registers(num_registers=2) -> Machine:
    """Create Machine with initialized X registers.

    Args:
        num_registers: Number of X registers to allocate

    Returns:
        Machine with X initialized to correct size
    """
    machine = Machine()
    machine.X = [0] * num_registers
    return machine


class TestIsBuiltin:
    """Test is/2 builtin for arithmetic evaluation."""

    def test_is_evaluates_and_binds(self):
        """X is 2 + 3 → X = 5."""
        machine = init_machine_with_registers()
        # X is 2 + 3
        # X[0] = X (unbound)
        # X[1] = 2 + 3 expression
        x_var = new_ref(machine)
        machine.X[0] = x_var

        # Build 2 + 3
        expr_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)
        machine.X[1] = expr_addr

        result = builtin_is(machine)

        assert result is True
        # X should be bound to 5

        x_val_addr = deref(machine, x_var)
        x_cell = machine.heap[x_val_addr]

        assert x_cell[0] == TAG_CON
        assert x_cell[1] == 5

    def test_is_checks_when_lhs_bound(self):
        """5 is 2 + 3 → success."""
        machine = init_machine_with_registers()
        # 5 is 2 + 3
        machine.X[0] = new_con(machine, 5)

        expr_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)
        machine.X[1] = expr_addr

        result = builtin_is(machine)
        assert result is True

    def test_is_fails_when_lhs_bound_mismatch(self):
        """6 is 2 + 3 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 6)

        expr_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)
        machine.X[1] = expr_addr

        result = builtin_is(machine)
        assert result is False

    def test_is_with_unbound_variable_raises(self):
        """X is Y → instantiation_error."""
        machine = init_machine_with_registers()
        machine.X[0] = new_ref(machine)
        machine.X[1] = new_ref(machine)

        with pytest.raises(InstantiationError):
            builtin_is(machine)

    def test_is_with_division_by_zero_raises(self):
        """X is 5 // 0 → evaluation_error."""
        machine = init_machine_with_registers()
        machine.X[0] = new_ref(machine)

        # Build 5 // 0
        expr_addr = new_str(machine, "//", 2)
        new_con(machine, 5)
        new_con(machine, 0)
        machine.X[1] = expr_addr

        with pytest.raises(EvaluationError) as exc_info:
            builtin_is(machine)
        assert exc_info.value.kwargs["error"] == "zero_divisor"

    def test_is_with_float_result(self):
        """X is 10 / 4 → X = 2.5."""
        machine = init_machine_with_registers()
        x_var = new_ref(machine)
        machine.X[0] = x_var

        # Build 10 / 4
        expr_addr = new_str(machine, "/", 2)
        new_con(machine, 10)
        new_con(machine, 4)
        machine.X[1] = expr_addr

        result = builtin_is(machine)

        assert result is True

        x_val_addr = deref(machine, x_var)
        x_cell = machine.heap[x_val_addr]

        assert x_cell[0] == TAG_CON
        assert x_cell[1] == 2.5


class TestArithmeticEquality:
    """Test =:=/2 arithmetic equality."""

    def test_arith_eq_success(self):
        """5 =:= 2 + 3 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)

        expr_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)
        machine.X[1] = expr_addr

        result = builtin_arith_eq(machine)
        assert result is True

    def test_arith_eq_failure(self):
        """5 =:= 4 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)
        machine.X[1] = new_con(machine, 4)

        result = builtin_arith_eq(machine)
        assert result is False

    def test_arith_eq_with_floats(self):
        """2.5 =:= 5 / 2 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 2.5)

        expr_addr = new_str(machine, "/", 2)
        new_con(machine, 5)
        new_con(machine, 2)
        machine.X[1] = expr_addr

        result = builtin_arith_eq(machine)
        assert result is True


class TestArithmeticInequality:
    """Test =\\=/2 arithmetic inequality."""

    def test_arith_ne_success(self):
        """5 =\\= 4 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)
        machine.X[1] = new_con(machine, 4)

        result = builtin_arith_ne(machine)
        assert result is True

    def test_arith_ne_failure(self):
        """5 =\\= 2 + 3 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)

        expr_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)
        machine.X[1] = expr_addr

        result = builtin_arith_ne(machine)
        assert result is False


class TestArithmeticLessThan:
    """Test </2 arithmetic less than."""

    def test_arith_lt_success(self):
        """3 < 5 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 3)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_lt(machine)
        assert result is True

    def test_arith_lt_failure_equal(self):
        """5 < 5 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_lt(machine)
        assert result is False

    def test_arith_lt_failure_greater(self):
        """7 < 5 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 7)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_lt(machine)
        assert result is False


class TestArithmeticLessEqual:
    """Test =</2 arithmetic less than or equal."""

    def test_arith_le_success_less(self):
        """3 =< 5 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 3)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_le(machine)
        assert result is True

    def test_arith_le_success_equal(self):
        """5 =< 5 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_le(machine)
        assert result is True

    def test_arith_le_failure(self):
        """7 =< 5 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 7)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_le(machine)
        assert result is False


class TestArithmeticGreaterThan:
    """Test >/2 arithmetic greater than."""

    def test_arith_gt_success(self):
        """7 > 5 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 7)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_gt(machine)
        assert result is True

    def test_arith_gt_failure_equal(self):
        """5 > 5 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_gt(machine)
        assert result is False

    def test_arith_gt_failure_less(self):
        """3 > 5 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 3)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_gt(machine)
        assert result is False


class TestArithmeticGreaterEqual:
    """Test >=/2 arithmetic greater than or equal."""

    def test_arith_ge_success_greater(self):
        """7 >= 5 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 7)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_ge(machine)
        assert result is True

    def test_arith_ge_success_equal(self):
        """5 >= 5 → success."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 5)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_ge(machine)
        assert result is True

    def test_arith_ge_failure(self):
        """3 >= 5 → failure."""
        machine = init_machine_with_registers()
        machine.X[0] = new_con(machine, 3)
        machine.X[1] = new_con(machine, 5)

        result = builtin_arith_ge(machine)
        assert result is False


class TestArithmeticBuiltinsWithExpressions:
    """Test arithmetic comparison builtins with complex expressions."""

    def test_arith_lt_with_expressions(self):
        """2 + 3 < 10 → success."""
        machine = init_machine_with_registers()

        # Build 2 + 3
        left_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)
        machine.X[0] = left_addr

        machine.X[1] = new_con(machine, 10)

        result = builtin_arith_lt(machine)
        assert result is True

    def test_arith_eq_with_nested_expressions(self):
        """(2 + 3) * 2 =:= 10 → success."""
        machine = init_machine_with_registers()

        # Build inner: 2 + 3
        inner_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)

        # Build outer: inner * 2

        outer_addr = new_str(machine, "*", 2)
        note_struct_args(machine, inner_addr)
        new_con(machine, 2)
        machine.X[0] = outer_addr

        machine.X[1] = new_con(machine, 10)

        result = builtin_arith_eq(machine)
        assert result is True
