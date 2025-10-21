"""Unit tests for WAM catch/3 instructions (catch_setup/catch_cleanup).

Tests catch/3 exception handling:
- catch_setup instruction (push exception frame)
- catch_cleanup instruction (pop exception frame on normal exit)
- Integration with throw/1 instruction
- Nested catch scenarios
- Backtracking interaction
"""

from prolog.wam.heap import new_con, new_ref
from prolog.wam.instructions import OP_CATCH_CLEANUP, OP_CATCH_SETUP, OP_HALT
from prolog.wam.machine import (
    Machine,
    UnhandledPrologException,
    instr_catch_cleanup,
    instr_catch_setup,
    instr_throw,
    push_exception_frame,
)


class TestCatchSetup:
    """Test catch_setup instruction."""

    def test_catch_setup_pushes_exception_frame(self):
        """catch_setup creates new exception frame."""

        machine = Machine()

        # Create pattern on heap
        pattern_addr = new_con(machine, "error")

        # Initially no exception frame
        assert machine.EF is None
        assert len(machine.exception_frames) == 0

        # Execute catch_setup
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        # Frame pushed
        assert machine.EF == 0
        assert len(machine.exception_frames) == 1

        frame = machine.exception_frames[0]
        assert frame.prev_frame is None
        assert frame.ball_pattern == pattern_addr
        assert frame.handler_label == 100

    def test_catch_setup_saves_machine_state(self):
        """catch_setup captures current machine state."""

        machine = Machine()
        pattern_addr = new_con(machine, "err")

        # Set machine state
        machine.CP = 42
        machine.E = 100
        machine.B = 50
        for _ in range(10):
            new_ref(machine)
        machine.TR = 5
        machine.HB = 8

        saved_H = machine.H
        saved_TR = machine.TR

        # Execute catch_setup
        instr_catch_setup(machine, handler_label=200, ball_pattern_addr=pattern_addr)

        # State captured in frame
        frame = machine.exception_frames[0]
        assert frame.CP == 42
        assert frame.E == 100
        assert frame.B == 50
        assert frame.H == saved_H
        assert frame.TR == saved_TR
        assert frame.HB == 8

    def test_catch_setup_advances_P(self):
        """catch_setup advances P to next instruction."""

        machine = Machine()
        pattern_addr = new_con(machine, "err")

        machine.P = 10
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        assert machine.P == 11

    def test_nested_catch_setup(self):
        """Multiple catch_setup creates nested frames."""

        machine = Machine()

        # Outer catch
        outer_pattern = new_con(machine, "outer")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=outer_pattern)
        outer_ef = machine.EF

        # Inner catch
        inner_pattern = new_con(machine, "inner")
        instr_catch_setup(machine, handler_label=200, ball_pattern_addr=inner_pattern)
        inner_ef = machine.EF

        # Two frames exist
        assert len(machine.exception_frames) == 2
        assert machine.EF == inner_ef

        # Inner frame links to outer
        inner_frame = machine.exception_frames[inner_ef]
        assert inner_frame.prev_frame == outer_ef

        outer_frame = machine.exception_frames[outer_ef]
        assert outer_frame.prev_frame is None


class TestCatchCleanup:
    """Test catch_cleanup instruction."""

    def test_catch_cleanup_pops_exception_frame(self):
        """catch_cleanup removes top exception frame."""

        machine = Machine()
        pattern_addr = new_con(machine, "err")

        # Push frame
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)
        assert machine.EF == 0

        # Pop frame
        instr_catch_cleanup(machine)

        # Frame popped (EF now None)
        assert machine.EF is None

    def test_catch_cleanup_with_nested_frames(self):
        """catch_cleanup pops innermost frame, keeps outer."""

        machine = Machine()

        # Push outer
        outer_pattern = new_con(machine, "outer")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=outer_pattern)
        outer_ef = machine.EF

        # Push inner
        inner_pattern = new_con(machine, "inner")
        instr_catch_setup(machine, handler_label=200, ball_pattern_addr=inner_pattern)

        # Pop inner
        instr_catch_cleanup(machine)

        # Now at outer frame
        assert machine.EF == outer_ef

    def test_catch_cleanup_advances_P(self):
        """catch_cleanup advances P to next instruction."""

        machine = Machine()
        pattern_addr = new_con(machine, "err")

        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        machine.P = 20
        instr_catch_cleanup(machine)

        assert machine.P == 21

    def test_catch_cleanup_with_no_frames(self):
        """catch_cleanup is safe when no frames exist."""

        machine = Machine()
        assert machine.EF is None

        machine.P = 10
        instr_catch_cleanup(machine)

        # No error, P still advances
        assert machine.P == 11
        assert machine.EF is None


class TestCatchThrowIntegration:
    """Test catch/3 integration with throw/1."""

    def test_catch_throw_match_jumps_to_handler(self):
        """throw with matching catch jumps to handler."""

        machine = Machine()

        # Set up catch
        ball_addr = new_con(machine, "my_error")
        pattern_addr = new_con(machine, "my_error")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        # Throw ball
        machine.X = [ball_addr]
        instr_throw(machine)

        # Jumped to handler
        assert machine.P == 100
        # Frame popped
        assert machine.EF is None

    def test_catch_throw_no_match_propagates(self):
        """throw with non-matching catch raises exception."""

        machine = Machine()

        # Set up catch for "error_a"
        pattern_addr = new_con(machine, "error_a")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        # Throw "error_b"
        ball_addr = new_con(machine, "error_b")
        machine.X = [ball_addr]

        # Should raise UnhandledPrologException
        try:
            instr_throw(machine)
            assert False, "Expected UnhandledPrologException"
        except UnhandledPrologException as e:
            assert e.ball_addr == ball_addr

    def test_nested_catch_innermost_match(self):
        """throw matches innermost catch first."""

        machine = Machine()

        # Outer catch (matches "outer")
        outer_pattern = new_con(machine, "outer")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=outer_pattern)

        # Inner catch (matches "inner")
        inner_pattern = new_con(machine, "inner")
        instr_catch_setup(machine, handler_label=200, ball_pattern_addr=inner_pattern)

        # Throw "inner"
        ball_addr = new_con(machine, "inner")
        machine.X = [ball_addr]
        instr_throw(machine)

        # Jumped to inner handler
        assert machine.P == 200
        # Popped to outer frame
        assert machine.EF == 0

    def test_nested_catch_outer_match(self):
        """throw skips inner, matches outer catch."""

        machine = Machine()

        # Outer catch (matches "target")
        outer_pattern = new_con(machine, "target")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=outer_pattern)

        # Inner catch (matches "other")
        inner_pattern = new_con(machine, "other")
        instr_catch_setup(machine, handler_label=200, ball_pattern_addr=inner_pattern)

        # Throw "target"
        ball_addr = new_con(machine, "target")
        machine.X = [ball_addr]
        instr_throw(machine)

        # Jumped to outer handler
        assert machine.P == 100
        # All frames popped
        assert machine.EF is None

    def test_catch_with_variable_pattern_unifies(self):
        """catch with variable pattern unifies with any ball."""

        machine = Machine()

        # Ball created first
        ball_addr = new_con(machine, "anything")

        # Pattern is unbound variable (created after ball)
        pattern_addr = new_ref(machine)
        saved_H = machine.H

        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        # Put ball in X[0] and throw
        machine.X = [ball_addr]
        instr_throw(machine)

        # Matched (variable unifies with anything)
        assert machine.P == 100
        assert machine.EF is None
        # Heap restored to saved_H (both ball and pattern still exist)
        assert machine.H == saved_H


class TestCatchNormalExit:
    """Test catch cleanup on normal (non-throwing) exit."""

    def test_catch_normal_success_cleans_up(self):
        """catch with successful goal cleans up frame."""

        machine = Machine()
        pattern_addr = new_con(machine, "err")

        # Set up catch
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)
        assert machine.EF == 0

        # Simulate successful goal completion
        # (no throw, just cleanup)
        instr_catch_cleanup(machine)

        # Frame cleaned up
        assert machine.EF is None

    def test_nested_catch_normal_exit(self):
        """Nested catch with normal exit cleans up both frames."""

        machine = Machine()

        # Outer catch
        outer_pattern = new_con(machine, "outer")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=outer_pattern)

        # Inner catch
        inner_pattern = new_con(machine, "inner")
        instr_catch_setup(machine, handler_label=200, ball_pattern_addr=inner_pattern)

        # Inner goal succeeds
        instr_catch_cleanup(machine)
        assert machine.EF == 0  # Back to outer

        # Outer goal succeeds
        instr_catch_cleanup(machine)
        assert machine.EF is None  # All cleaned up


class TestCatchStateRestoration:
    """Test that throw restores state correctly when catching."""

    def test_catch_restores_heap(self):
        """throw truncates heap to frame.H."""

        machine = Machine()

        # Create pattern
        pattern_addr = new_con(machine, "err")
        saved_H = machine.H

        # Set up catch
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        # Allocate more heap after catch
        for _ in range(10):
            new_ref(machine)

        # Create and throw ball
        ball_addr = new_con(machine, "err")
        machine.X = [ball_addr]
        instr_throw(machine)

        # Heap restored to saved_H
        assert machine.H == saved_H

    def test_catch_restores_all_registers(self):
        """throw restores CP, E, B, HB from exception frame."""

        machine = Machine()

        # Set initial state
        machine.CP = 42
        machine.E = 100
        machine.B = 50
        machine.HB = 35

        pattern_addr = new_con(machine, "err")
        instr_catch_setup(machine, handler_label=100, ball_pattern_addr=pattern_addr)

        # Modify state after catch_setup
        machine.CP = 999
        machine.E = 888
        machine.B = 777
        machine.HB = 666

        # Throw
        ball_addr = new_con(machine, "err")
        machine.X = [ball_addr]
        instr_throw(machine)

        # State restored
        assert machine.CP == 42
        assert machine.E == 100
        assert machine.B == 50
        assert machine.HB == 35


class TestCatchDispatch:
    """Test catch_setup and catch_cleanup execution through Machine.step()."""

    def test_catch_setup_via_step(self):
        """catch_setup executes correctly via step() dispatcher."""
        machine = Machine()

        # Create pattern and handler
        pattern_addr = new_con(machine, "error")
        handler_label = 100

        # Load catch_setup instruction
        machine.code = [(OP_CATCH_SETUP, handler_label, pattern_addr)]
        machine.P = 0

        # Execute via step
        result = machine.step()

        # Should succeed
        assert result is True
        assert machine.halted is False

        # Exception frame should be pushed
        assert machine.EF == 0
        assert len(machine.exception_frames) == 1

        # P should advance
        assert machine.P == 1

    def test_catch_cleanup_via_step(self):
        """catch_cleanup executes correctly via step() dispatcher."""
        machine = Machine()

        # Create and push exception frame first
        pattern_addr = new_con(machine, "error")
        push_exception_frame(machine, pattern_addr, 100)

        # Load catch_cleanup instruction
        machine.code = [(OP_CATCH_CLEANUP,)]
        machine.P = 0

        # Execute via step
        result = machine.step()

        # Should succeed
        assert result is True
        assert machine.halted is False

        # Exception frame should be popped
        assert machine.EF is None

        # P should advance
        assert machine.P == 1

    def test_catch_setup_cleanup_sequence_via_step(self):
        """catch_setup followed by catch_cleanup via step()."""
        machine = Machine()

        # Create pattern
        pattern_addr = new_con(machine, "my_error")
        handler_label = 200

        # Load sequence: catch_setup, catch_cleanup
        machine.code = [
            (OP_CATCH_SETUP, handler_label, pattern_addr),
            (OP_CATCH_CLEANUP,),
        ]
        machine.P = 0

        # Execute catch_setup
        result = machine.step()
        assert result is True
        assert machine.EF == 0
        assert machine.P == 1

        # Execute catch_cleanup
        result = machine.step()
        assert result is True
        assert machine.EF is None
        assert machine.P == 2

    def test_catch_throw_integration_via_step(self):
        """catch_setup + throw integration via step()."""
        machine = Machine()

        # Create ball and pattern (matching)
        ball_addr = new_con(machine, "test_error")
        pattern_addr = new_con(machine, "test_error")

        # Put ball in X[0] for throw
        machine.X = [ball_addr]

        # Load code: catch_setup, halt (handler location)
        machine.code = [
            (OP_CATCH_SETUP, 2, pattern_addr),  # Handler at index 2
            (OP_HALT,),  # Index 1 - shouldn't execute
            (OP_HALT,),  # Index 2 - handler (halt)
        ]
        machine.P = 0

        # Execute catch_setup
        result = machine.step()
        assert result is True
        assert machine.EF == 0
        assert machine.P == 1

        # Now throw (manually call instr_throw since we're testing catch dispatch)
        instr_throw(machine)

        # Should have jumped to handler
        assert machine.P == 2
        assert machine.EF is None

        # Execute handler (halt)
        result = machine.step()
        assert result is False
        assert machine.halted is True
