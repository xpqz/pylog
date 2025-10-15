"""Unit tests for WAM instruction validation."""

import pytest

from prolog.wam.instructions import (
    OP_DBG_SNAP,
    OP_HALT,
    OP_NOOP,
    OP_SET_X,
    name_to_opcode,
    opcode_name,
    validate_instruction,
)


class TestOpcodeNameMapping:
    """Test opcode name conversion functions."""

    def test_opcode_name_for_known_opcodes(self):
        """Convert known opcodes to names."""
        assert opcode_name(OP_NOOP) == "noop"
        assert opcode_name(OP_HALT) == "halt"
        assert opcode_name(OP_SET_X) == "set_x"
        assert opcode_name(OP_DBG_SNAP) == "dbg_snap"

    def test_opcode_name_for_unknown_opcode(self):
        """Unknown opcodes return formatted string."""
        assert opcode_name(999) == "<unknown:999>"

    def test_name_to_opcode_for_known_names(self):
        """Convert known names to opcodes."""
        assert name_to_opcode("noop") == OP_NOOP
        assert name_to_opcode("halt") == OP_HALT
        assert name_to_opcode("set_x") == OP_SET_X
        assert name_to_opcode("dbg_snap") == OP_DBG_SNAP

    def test_name_to_opcode_raises_for_unknown_name(self):
        """Unknown names raise ValueError."""
        with pytest.raises(ValueError, match="Unknown instruction name: foo"):
            name_to_opcode("foo")


class TestValidateInstruction:
    """Test instruction validation."""

    def test_validate_noop_passes(self):
        """NOOP with no args is valid."""
        validate_instruction((OP_NOOP,))  # Should not raise

    def test_validate_halt_passes(self):
        """HALT with no args is valid."""
        validate_instruction((OP_HALT,))  # Should not raise

    def test_validate_set_x_passes(self):
        """SET_X with 2 args is valid."""
        validate_instruction((OP_SET_X, 0, 42))  # Should not raise

    def test_validate_dbg_snap_passes(self):
        """DBG_SNAP with no args is valid."""
        validate_instruction((OP_DBG_SNAP,))  # Should not raise

    def test_validate_empty_instruction_fails(self):
        """Empty tuple raises ValueError."""
        with pytest.raises(ValueError, match="Empty instruction tuple"):
            validate_instruction(())

    def test_validate_unknown_opcode_fails(self):
        """Unknown opcode raises ValueError."""
        with pytest.raises(ValueError, match="Unknown opcode: 999"):
            validate_instruction((999,))

    def test_validate_wrong_arity_too_few_fails(self):
        """Instruction with too few args raises ValueError."""
        with pytest.raises(
            ValueError, match="Instruction set_x expects 2 arguments, got 0"
        ):
            validate_instruction((OP_SET_X,))

    def test_validate_wrong_arity_too_many_fails(self):
        """Instruction with too many args raises ValueError."""
        with pytest.raises(
            ValueError, match="Instruction noop expects 0 arguments, got 2"
        ):
            validate_instruction((OP_NOOP, 1, 2))

    def test_validate_set_x_with_one_arg_fails(self):
        """SET_X with 1 arg raises ValueError."""
        with pytest.raises(
            ValueError, match="Instruction set_x expects 2 arguments, got 1"
        ):
            validate_instruction((OP_SET_X, 0))
