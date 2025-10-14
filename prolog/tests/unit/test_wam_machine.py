"""Unit tests for WAM machine state and execution."""

import pytest

from prolog.wam.instructions import OP_HALT, OP_NOOP, OP_SET_X
from prolog.wam.machine import Machine


class TestMachineInitialization:
    """Test Machine initialization."""

    def test_machine_initializes_with_empty_state(self):
        """Machine starts with all registers and stacks empty."""
        m = Machine()
        assert m.P == 0
        assert m.H == 0
        assert m.HB == 0
        assert m.B is None
        assert m.E is None
        assert m.CP is None
        assert m.TR == 0
        assert m.S is None
        assert m.EF == -1
        assert m.unify_mode is None
        assert m.X == []
        assert m.heap == []
        assert m.frames == []
        assert m.cp_stack == []
        assert m.trail == []
        assert m.halted is False
        assert m.code == []


class TestMachineInvariants:
    """Test Machine.check_invariants()."""

    def test_check_invariants_passes_on_empty_machine(self):
        """Invariants hold for freshly initialized machine."""
        m = Machine()
        m.check_invariants()  # Should not raise

    def test_check_invariants_accepts_valid_heap(self):
        """Invariants pass with valid heap cells."""
        m = Machine()
        # Add REF cell pointing to itself (canonical unbound)
        m.heap.append((0, 0))  # REF pointing to self
        m.H = 1
        m.check_invariants()

    def test_check_invariants_rejects_invalid_heap_pointer(self):
        """Invariant check fails if H exceeds heap size."""
        m = Machine()
        m.H = 10
        with pytest.raises(AssertionError, match="Heap pointer H=10 out of range"):
            m.check_invariants()

    def test_check_invariants_rejects_ref_out_of_bounds(self):
        """Invariant check fails if REF points outside heap."""
        m = Machine()
        m.heap.append((0, 5))  # REF pointing to non-existent cell
        m.H = 1
        with pytest.raises(
            AssertionError, match="REF cell at 0 points to invalid address 5"
        ):
            m.check_invariants()


class TestMachineReset:
    """Test Machine.reset()."""

    def test_reset_clears_all_state(self):
        """Reset returns machine to initial state."""
        m = Machine()
        # Modify state
        m.P = 10
        m.H = 5
        m.halted = True
        m.X = [1, 2, 3]
        m.heap = [(0, 0)]
        m.code = [(OP_NOOP,)]

        # Reset
        m.reset()

        # State cleared except code
        assert m.P == 0
        assert m.H == 0
        assert m.halted is False
        assert m.X == []
        assert m.heap == []
        assert m.code == [(OP_NOOP,)]  # Code not cleared by reset


class TestInstructionExecution:
    """Test instruction dispatch and execution."""

    def test_run_with_empty_code_returns_immediately(self):
        """Running with no code does nothing."""
        m = Machine()
        m.run(max_steps=10)
        assert m.P == 0
        assert not m.halted

    def test_noop_advances_program_counter(self):
        """NOOP increments P and continues."""
        m = Machine()
        m.code = [(OP_NOOP,), (OP_HALT,)]
        m.step()
        assert m.P == 1
        assert not m.halted

    def test_halt_stops_execution(self):
        """HALT sets halt flag and stops."""
        m = Machine()
        m.code = [(OP_HALT,)]
        m.step()
        assert m.halted
        assert m.P == 0  # P not advanced after halt

    def test_set_x_writes_register(self):
        """SET_X writes value to X register."""
        m = Machine()
        m.code = [(OP_SET_X, 0, 42), (OP_HALT,)]
        m.step()
        assert m.X[0] == 42
        assert m.P == 1

    def test_set_x_extends_x_register_bank(self):
        """SET_X extends X list if necessary."""
        m = Machine()
        m.code = [(OP_SET_X, 5, 99), (OP_HALT,)]
        m.step()
        assert len(m.X) == 6
        assert m.X[5] == 99
        assert m.P == 1

    def test_run_executes_multiple_steps(self):
        """run() executes until halt or max_steps."""
        m = Machine()
        m.code = [
            (OP_NOOP,),
            (OP_NOOP,),
            (OP_SET_X, 0, 10),
            (OP_HALT,),
        ]
        m.run(max_steps=10)
        assert m.halted
        assert m.P == 3
        assert m.X[0] == 10

    def test_run_respects_max_steps(self):
        """run() stops after max_steps even if not halted."""
        m = Machine()
        m.code = [(OP_NOOP,)] * 100
        m.run(max_steps=10)
        assert m.P == 10
        assert not m.halted

    def test_step_returns_false_when_halted(self):
        """step() returns False after halt."""
        m = Machine()
        m.code = [(OP_HALT,)]
        result = m.step()
        assert result is False
        assert m.halted

    def test_step_returns_true_when_continuing(self):
        """step() returns True for non-halt instructions."""
        m = Machine()
        m.code = [(OP_NOOP,), (OP_HALT,)]
        result = m.step()
        assert result is True
        assert not m.halted

    def test_unknown_opcode_halts_execution(self):
        """Unknown opcodes halt the machine."""
        m = Machine()
        m.code = [(999,)]  # Invalid opcode
        m.step()
        assert m.halted


class TestExecutionScenarios:
    """Test realistic execution scenarios."""

    def test_program_with_register_operations(self):
        """Execute program that manipulates registers."""
        m = Machine()
        m.code = [
            (OP_SET_X, 0, 1),
            (OP_SET_X, 1, 2),
            (OP_SET_X, 2, 3),
            (OP_NOOP,),
            (OP_HALT,),
        ]
        m.run()
        assert m.X == [1, 2, 3]
        assert m.halted

    def test_program_counter_advances_correctly(self):
        """P advances through instruction sequence."""
        m = Machine()
        m.code = [(OP_NOOP,)] * 5 + [(OP_HALT,)]

        for expected_p in range(5):
            assert m.P == expected_p
            m.step()

        # After 5 NOOPs, P should be at 5 (HALT instruction)
        assert m.P == 5
        assert not m.halted

        # Execute HALT
        m.step()
        assert m.halted
