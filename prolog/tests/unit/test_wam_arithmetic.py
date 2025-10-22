"""Tests for WAM arithmetic evaluation engine."""

import pytest

from prolog.wam.arithmetic import eval_arithmetic
from prolog.wam.errors import EvaluationError, InstantiationError, TypeError
from prolog.wam.heap import new_con, new_ref, new_str, note_struct_args
from prolog.wam.machine import Machine


def build_const_binary_op(machine, op: str, left_val, right_val) -> int:
    """Build binary operation with constant operands.

    Args:
        machine: Machine instance
        op: Operator name ("+", "-", etc.)
        left_val: Left constant value (int/float)
        right_val: Right constant value (int/float)

    Returns:
        Address of structure
    """
    # Create structure first (STR + functor cells)
    struct_addr = new_str(machine, op, 2)
    # Now create argument cells - they'll be at functor+1 and functor+2
    new_con(machine, left_val)
    new_con(machine, right_val)
    return struct_addr


def build_binary_op(machine, op: str, left_addr, right_addr) -> int:
    """Build binary operation with existing operands.

    Args:
        machine: Machine instance
        op: Operator name ("+", "-", etc.)
        left_addr: Left operand address (already on heap)
        right_addr: Right operand address (already on heap)

    Returns:
        Address of structure
    """
    # Create structure first (STR + functor cells)
    struct_addr = new_str(machine, op, 2)
    # Copy the operand cells to argument positions
    machine.heap.append(machine.heap[left_addr])
    machine.heap.append(machine.heap[right_addr])
    return struct_addr


def build_const_unary_op(machine, op: str, arg_val) -> int:
    """Build unary operation with constant operand.

    Args:
        machine: Machine instance
        op: Operator name ("-", "+")
        arg_val: Argument constant value

    Returns:
        Address of structure
    """
    struct_addr = new_str(machine, op, 1)
    new_con(machine, arg_val)
    return struct_addr


class TestArithmeticConstants:
    """Test arithmetic evaluation of constants."""

    def test_integer_constant(self):
        """Evaluate integer constant."""
        machine = Machine()
        addr = new_con(machine, 42)

        result = eval_arithmetic(addr, machine)

        assert result == 42
        assert isinstance(result, int)

    def test_negative_integer_constant(self):
        """Evaluate negative integer constant."""
        machine = Machine()
        addr = new_con(machine, -17)

        result = eval_arithmetic(addr, machine)

        assert result == -17

    def test_zero_constant(self):
        """Evaluate zero."""
        machine = Machine()
        addr = new_con(machine, 0)

        result = eval_arithmetic(addr, machine)

        assert result == 0

    def test_float_constant(self):
        """Evaluate float constant."""
        machine = Machine()
        addr = new_con(machine, 3.14)

        result = eval_arithmetic(addr, machine)

        assert result == 3.14
        assert isinstance(result, float)


class TestArithmeticBinaryOperators:
    """Test binary arithmetic operators."""

    def test_addition(self):
        """Evaluate 2 + 3 → 5."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "+", 2, 3)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 5

    def test_subtraction(self):
        """Evaluate 10 - 3 → 7."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "-", 10, 3)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 7

    def test_multiplication(self):
        """Evaluate 4 * 5 → 20."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "*", 4, 5)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 20

    def test_float_division(self):
        """Evaluate 7 / 2 → 3.5."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "/", 7, 2)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 3.5
        assert isinstance(result, float)

    def test_integer_division(self):
        """Evaluate 7 // 2 → 3."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "//", 7, 2)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 3
        assert isinstance(result, int)

    def test_integer_division_negative(self):
        """Evaluate -7 // 2 → -4 (floor division)."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "//", -7, 2)
        result = eval_arithmetic(struct_addr, machine)
        assert result == -4

    def test_modulo(self):
        """Evaluate 7 mod 3 → 1."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "mod", 7, 3)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 1

    def test_modulo_negative(self):
        """Evaluate -7 mod 3 → 2 (Python semantics)."""
        machine = Machine()
        struct_addr = build_const_binary_op(machine, "mod", -7, 3)
        result = eval_arithmetic(struct_addr, machine)
        assert result == 2


class TestArithmeticUnaryOperators:
    """Test unary arithmetic operators."""

    def test_unary_minus(self):
        """Evaluate -(5) → -5."""
        machine = Machine()

        struct_addr = build_const_unary_op(machine, "-", 5)

        result = eval_arithmetic(struct_addr, machine)

        assert result == -5

    def test_unary_minus_negative(self):
        """Evaluate -(-5) → 5."""
        machine = Machine()

        struct_addr = build_const_unary_op(machine, "-", -5)

        result = eval_arithmetic(struct_addr, machine)

        assert result == 5

    def test_unary_plus(self):
        """Evaluate +(5) → 5."""
        machine = Machine()

        struct_addr = build_const_unary_op(machine, "+", 5)

        result = eval_arithmetic(struct_addr, machine)

        assert result == 5


class TestArithmeticNested:
    """Test nested arithmetic expressions."""

    def test_nested_addition(self):
        """Evaluate (2 + 3) + 4 → 9."""
        machine = Machine()

        # Build inner: +(2, 3)
        inner_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        new_con(machine, 3)

        # Build outer: +(inner, 4) - inner is referenced via REF
        outer_addr = new_str(machine, "+", 2)
        note_struct_args(machine, inner_addr)  # Append REF(inner) as arg1
        new_con(machine, 4)  # Allocate 4 as arg2

        result = eval_arithmetic(outer_addr, machine)

        assert result == 9

    def test_precedence_multiplication_addition(self):
        """Evaluate 2 + 3 * 4 → 14 (assuming correct structure)."""
        machine = Machine()

        # Build inner: *(3, 4)
        mult_addr = new_str(machine, "*", 2)
        new_con(machine, 3)
        new_con(machine, 4)

        # Build outer: +(2, mult)
        add_addr = new_str(machine, "+", 2)
        new_con(machine, 2)  # arg1: constant 2
        note_struct_args(machine, mult_addr)  # arg2: REF(mult)

        result = eval_arithmetic(add_addr, machine)

        assert result == 14

    def test_deeply_nested(self):
        """Evaluate ((1 + 2) * (3 + 4)) → 21."""
        machine = Machine()

        # Build inner1: +(1, 2)
        inner1_addr = new_str(machine, "+", 2)
        new_con(machine, 1)
        new_con(machine, 2)

        # Build inner2: +(3, 4)
        inner2_addr = new_str(machine, "+", 2)
        new_con(machine, 3)
        new_con(machine, 4)

        # Build outer: *(inner1, inner2)
        outer_addr = new_str(machine, "*", 2)
        note_struct_args(machine, inner1_addr)  # arg1: REF(inner1)
        note_struct_args(machine, inner2_addr)  # arg2: REF(inner2)

        result = eval_arithmetic(outer_addr, machine)

        assert result == 21


class TestArithmeticErrors:
    """Test arithmetic error handling."""

    def test_unbound_variable_raises_instantiation_error(self):
        """Unbound variable in expression raises InstantiationError."""
        machine = Machine()

        unbound_addr = new_ref(machine)

        with pytest.raises(InstantiationError):
            eval_arithmetic(unbound_addr, machine)

    def test_unbound_in_binary_op_raises_instantiation_error(self):
        """Unbound variable in binary operation raises InstantiationError."""
        machine = Machine()

        unbound_addr = new_ref(machine)

        # Build: +(2, X) where X is unbound
        struct_addr = new_str(machine, "+", 2)
        new_con(machine, 2)
        note_struct_args(machine, unbound_addr)  # REF to unbound

        with pytest.raises(InstantiationError):
            eval_arithmetic(struct_addr, machine)

    def test_division_by_zero_raises_evaluation_error(self):
        """Division by zero raises EvaluationError."""
        machine = Machine()

        struct_addr = build_const_binary_op(machine, "/", 5, 0)

        with pytest.raises(EvaluationError) as exc_info:
            eval_arithmetic(struct_addr, machine)

        assert exc_info.value.kwargs["error"] == "zero_divisor"

    def test_integer_division_by_zero_raises_evaluation_error(self):
        """Integer division by zero raises EvaluationError."""
        machine = Machine()

        struct_addr = build_const_binary_op(machine, "//", 5, 0)

        with pytest.raises(EvaluationError) as exc_info:
            eval_arithmetic(struct_addr, machine)

        assert exc_info.value.kwargs["error"] == "zero_divisor"

    def test_modulo_by_zero_raises_evaluation_error(self):
        """Modulo by zero raises EvaluationError."""
        machine = Machine()

        struct_addr = build_const_binary_op(machine, "mod", 5, 0)

        with pytest.raises(EvaluationError) as exc_info:
            eval_arithmetic(struct_addr, machine)

        assert exc_info.value.kwargs["error"] == "zero_divisor"

    def test_non_numeric_constant_raises_type_error(self):
        """Non-numeric constant raises TypeError."""
        machine = Machine()

        atom_addr = new_con(machine, "foo")

        with pytest.raises(TypeError) as exc_info:
            eval_arithmetic(atom_addr, machine)

        assert exc_info.value.kwargs["expected"] == "number"

    def test_boolean_constant_raises_type_error(self):
        """Boolean constant raises TypeError."""
        machine = Machine()

        bool_addr = new_con(machine, True)

        with pytest.raises(TypeError) as exc_info:
            eval_arithmetic(bool_addr, machine)

        assert exc_info.value.kwargs["expected"] == "number"

    def test_unknown_operator_raises_type_error(self):
        """Unknown operator raises TypeError."""
        machine = Machine()

        struct_addr = build_const_binary_op(machine, "unknown_op", 1, 2)

        with pytest.raises(TypeError) as exc_info:
            eval_arithmetic(struct_addr, machine)

        assert exc_info.value.kwargs["expected"] == "evaluable"
