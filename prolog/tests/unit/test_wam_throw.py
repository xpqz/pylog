"""Unit tests for WAM throw/1 instruction and exception unwinding.

Tests throw/1 instruction behavior:
- Exception frame search and matching
- State restoration on match
- Untrailing during unwind
- UnhandledPrologException when no match
- Nested exception handlers
"""

from prolog.wam.heap import new_con, new_ref, new_str
from prolog.wam.machine import (
    Machine,
    UnhandledPrologException,
    instr_throw,
    push_exception_frame,
    untrail_to,
)


class TestThrowBasic:
    """Test basic throw/1 behavior."""

    def test_throw_with_no_exception_frames_raises(self):
        """throw with no active exception frames raises UnhandledPrologException."""

        machine = Machine()

        # Put ball in X[0]
        ball_addr = new_con(machine, "error")
        machine.X = [ball_addr]

        # No exception frames
        assert machine.EF is None

        # throw should raise UnhandledPrologException
        try:
            instr_throw(machine)
            assert False, "Expected UnhandledPrologException"
        except UnhandledPrologException as e:
            assert e.ball_addr == ball_addr

    def test_throw_with_matching_frame_jumps_to_handler(self):
        """throw with matching pattern jumps to handler."""

        machine = Machine()

        # Create ball and pattern (same atom)
        ball_addr = new_con(machine, "my_error")
        pattern_addr = new_con(machine, "my_error")

        # Set up exception frame
        handler_label = 100
        machine.CP = 42
        machine.E = 50
        machine.B = 25
        machine.H = 10
        machine.TR = 5
        machine.HB = 8

        push_exception_frame(machine, pattern_addr, handler_label)

        # Put ball in X[0]
        machine.X = [ball_addr]

        # Execute throw
        instr_throw(machine)

        # Should jump to handler
        assert machine.P == handler_label

        # State should be restored from frame
        assert machine.CP == 42
        assert machine.E == 50
        assert machine.B == 25
        assert machine.H == 10
        assert machine.TR == 5
        assert machine.HB == 8

        # Exception frame popped
        assert machine.EF is None

    def test_throw_with_non_matching_frame_raises(self):
        """throw with non-matching pattern raises UnhandledPrologException."""

        machine = Machine()

        # Create ball and different pattern
        ball_addr = new_con(machine, "error_a")
        pattern_addr = new_con(machine, "error_b")

        # Set up exception frame
        push_exception_frame(machine, pattern_addr, 100)

        # Put ball in X[0]
        machine.X = [ball_addr]

        # Execute throw
        try:
            instr_throw(machine)
            assert False, "Expected UnhandledPrologException"
        except UnhandledPrologException as e:
            assert e.ball_addr == ball_addr

        # Frame should have been tried and discarded
        assert machine.EF is None


class TestThrowStateRestoration:
    """Test state restoration during unwind."""

    def test_throw_restores_all_registers(self):
        """throw restores CP, E, B, H, TR, HB from exception frame."""

        machine = Machine()

        # Set initial state
        machine.CP = 10
        machine.E = 20
        machine.B = 30
        machine.H = 40
        machine.TR = 50
        machine.HB = 35

        # Create matching frame
        ball_addr = new_con(machine, "err")
        pattern_addr = new_con(machine, "err")
        push_exception_frame(machine, pattern_addr, 200)

        # Modify state after push
        machine.CP = 999
        machine.E = 888
        machine.B = 777
        machine.H = 666
        machine.TR = 555
        machine.HB = 444

        # throw
        machine.X = [ball_addr]
        instr_throw(machine)

        # Original state restored
        assert machine.CP == 10
        assert machine.E == 20
        assert machine.B == 30
        assert machine.H == 40
        assert machine.TR == 50
        assert machine.HB == 35
        assert machine.P == 200

    def test_throw_untrails_to_saved_tr(self):
        """throw untrails from current TR to saved TR."""

        machine = Machine()

        # Create some heap cells
        ref1 = new_ref(machine)
        ref2 = new_ref(machine)
        con_addr = new_con(machine, "value")

        # Bind ref1 (trail it)
        machine.trail.append(ref1)
        machine.heap[ref1] = (1, con_addr)  # TAG_REF pointing to con
        machine.TR = 1

        # Save state in exception frame
        ball_addr = new_con(machine, "err")
        pattern_addr = new_con(machine, "err")
        push_exception_frame(machine, pattern_addr, 100)

        # Bind ref2 (trail it)
        machine.trail.append(ref2)
        machine.heap[ref2] = (1, con_addr)
        machine.TR = 2

        # throw should untrail ref2 but not ref1
        machine.X = [ball_addr]
        instr_throw(machine)

        # TR restored to 1
        assert machine.TR == 1

        # ref2 should be unbound (restored)
        assert machine.heap[ref2] == (0, ref2)  # TAG_REF self-ref

        # ref1 should still be bound
        assert machine.heap[ref1] == (1, con_addr)


class TestThrowNestedFrames:
    """Test throw with nested exception frames."""

    def test_throw_matches_innermost_frame(self):
        """throw matches innermost (most recent) exception frame."""

        machine = Machine()

        # Create ball
        ball_addr = new_con(machine, "err")

        # Outer frame (matches)
        pattern_outer = new_con(machine, "err")
        push_exception_frame(machine, pattern_outer, 100)

        # Inner frame (also matches)
        pattern_inner = new_con(machine, "err")
        push_exception_frame(machine, pattern_inner, 200)

        # throw should match inner frame
        machine.X = [ball_addr]
        instr_throw(machine)

        assert machine.P == 200  # Inner handler

        # Should pop back to outer frame
        assert machine.EF == 0

    def test_throw_skips_non_matching_frames(self):
        """throw skips non-matching frames to find matching one."""

        machine = Machine()

        # Create ball
        ball_addr = new_con(machine, "target_error")

        # Outer frame (matches)
        pattern_outer = new_con(machine, "target_error")
        machine.H = 100
        push_exception_frame(machine, pattern_outer, 300)
        saved_H = 100

        # Inner frame (doesn't match)
        pattern_inner = new_con(machine, "other_error")
        machine.H = 200
        push_exception_frame(machine, pattern_inner, 400)

        # throw should skip inner, match outer
        machine.X = [ball_addr]
        instr_throw(machine)

        assert machine.P == 300  # Outer handler
        assert machine.H == saved_H  # Restored from outer frame

        # Should have popped to before outer frame
        assert machine.EF is None

    def test_throw_tries_all_frames_before_raising(self):
        """throw tries all frames before raising UnhandledPrologException."""

        machine = Machine()

        # Create ball
        ball_addr = new_con(machine, "no_match")

        # Create several non-matching frames
        for i in range(3):
            pattern = new_con(machine, f"error_{i}")
            push_exception_frame(machine, pattern, 100 * (i + 1))

        # All frames are non-matching
        machine.X = [ball_addr]

        try:
            instr_throw(machine)
            assert False, "Expected UnhandledPrologException"
        except UnhandledPrologException as e:
            assert e.ball_addr == ball_addr

        # All frames should have been popped
        assert machine.EF is None


class TestThrowUnification:
    """Test throw with variable patterns and unification."""

    def test_throw_unifies_with_variable_pattern(self):
        """throw unifies ball with variable pattern."""

        machine = Machine()

        # Create ball (concrete)
        ball_addr = new_con(machine, "my_error")

        # Create pattern (variable)
        pattern_addr = new_ref(machine)

        # Set up exception frame
        push_exception_frame(machine, pattern_addr, 100)

        # throw
        machine.X = [ball_addr]
        instr_throw(machine)

        # Should match (unify variable with concrete)
        assert machine.P == 100

        # Pattern variable should now be bound to ball
        # (Note: unification happens before restoration, so binding is temporary)

    def test_throw_unifies_complex_terms(self):
        """throw unifies complex structured terms."""

        machine = Machine()

        # Create ball: error(type, culprit)
        ball_addr = new_str(machine, "error", 2)  # Returns STR address
        new_con(machine, "type")  # arg1
        new_con(machine, "foo")  # arg2

        # Create pattern: error(X, Y) where X and Y are variables
        pattern_addr = new_str(machine, "error", 2)  # Returns STR address
        new_ref(machine)  # arg1 (variable X)
        new_ref(machine)  # arg2 (variable Y)

        # Set up exception frame
        push_exception_frame(machine, pattern_addr, 100)

        # throw
        machine.X = [ball_addr]
        instr_throw(machine)

        # Should match
        assert machine.P == 100


class TestThrowUntrail:
    """Test untrail helper function."""

    def test_untrail_restores_bindings(self):
        """untrail restores heap cells from trail."""

        machine = Machine()

        # Create refs
        ref1 = new_ref(machine)
        ref2 = new_ref(machine)

        # Modify and trail
        machine.heap[ref1] = (1, 999)
        machine.trail.append(ref1)
        machine.TR = 1

        machine.heap[ref2] = (1, 888)
        machine.trail.append(ref2)
        machine.TR = 2

        # Untrail to TR=1 (undo ref2 but not ref1)
        untrail_to(machine, 1)

        assert machine.TR == 1
        assert machine.heap[ref2] == (0, ref2)  # Restored to self-ref
        assert machine.heap[ref1] == (1, 999)  # Still modified

    def test_untrail_to_zero(self):
        """untrail to TR=0 undoes all bindings."""

        machine = Machine()

        # Create and bind several refs
        refs = []
        for i in range(5):
            ref = new_ref(machine)
            refs.append(ref)
            machine.heap[ref] = (1, 100 + i)
            machine.trail.append(ref)
            machine.TR = i + 1

        # Untrail all
        untrail_to(machine, 0)

        assert machine.TR == 0

        # All refs should be restored to self-refs
        for i, ref in enumerate(refs):
            assert machine.heap[ref] == (0, ref)


class TestUnhandledPrologException:
    """Test UnhandledPrologException class."""

    def test_exception_has_ball_addr(self):
        """UnhandledPrologException stores ball address."""
        machine = Machine()
        ball_addr = new_con(machine, "error")

        exc = UnhandledPrologException(ball_addr)

        assert exc.ball_addr == ball_addr

    def test_exception_message(self):
        """UnhandledPrologException has informative message."""
        ball_addr = 42

        exc = UnhandledPrologException(ball_addr)

        assert "42" in str(exc)
        assert "Unhandled" in str(exc) or "unhandled" in str(exc)


class TestThrowEdgeCases:
    """Test edge cases and corner scenarios."""

    def test_throw_with_empty_heap(self):
        """throw works with minimal heap."""

        machine = Machine()

        # Ball at heap[0]
        ball_addr = 0
        machine.heap.append((2, "error"))  # TAG_CON

        # Pattern at heap[1]
        pattern_addr = 1
        machine.heap.append((2, "error"))

        push_exception_frame(machine, pattern_addr, 50)

        machine.X = [ball_addr]
        instr_throw(machine)

        assert machine.P == 50

    def test_throw_restores_state_before_unification_side_effects(self):
        """throw restores state even if unification modified heap."""

        machine = Machine()

        # Create pattern with variable
        pattern_addr = new_ref(machine)

        # Save H before frame
        saved_H = machine.H
        push_exception_frame(machine, pattern_addr, 100)

        # Add more heap after frame
        ball_addr = new_con(machine, "error")
        new_H = machine.H

        # throw will unify (binding pattern var), then restore H
        machine.X = [ball_addr]
        instr_throw(machine)

        # H should be restored to frame's saved value
        assert machine.H == saved_H
        # Not the current H
        assert machine.H != new_H
