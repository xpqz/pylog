"""Unit tests for WAM exception frame data structure and operations.

Tests exception frame management for catch/3 support:
- ExceptionFrame structure
- Push/pop operations
- Frame chaining
- State snapshot capture
"""

from prolog.wam.heap import new_ref
from prolog.wam.machine import (
    ExceptionFrame,
    Machine,
    pop_exception_frame,
    push_exception_frame,
)


class TestExceptionFrameStructure:
    """Test ExceptionFrame data structure."""

    def test_exception_frame_has_required_fields(self):
        """ExceptionFrame includes all required state fields."""

        # Create frame with all fields
        frame = ExceptionFrame(
            prev_frame=None,
            ball_pattern=10,
            handler_label=200,
            CP=42,
            E=100,
            B=50,
            H=75,
            TR=5,
            HB=70,
        )

        assert frame.prev_frame is None
        assert frame.ball_pattern == 10
        assert frame.handler_label == 200
        assert frame.CP == 42
        assert frame.E == 100
        assert frame.B == 50
        assert frame.H == 75
        assert frame.TR == 5
        assert frame.HB == 70

    def test_exception_frame_with_previous(self):
        """ExceptionFrame can chain to previous frame."""

        frame = ExceptionFrame(
            prev_frame=3,  # Index of previous frame
            ball_pattern=20,
            handler_label=300,
            CP=50,
            E=200,
            B=100,
            H=150,
            TR=10,
            HB=140,
        )

        assert frame.prev_frame == 3


class TestExceptionFrameStack:
    """Test exception frame stack operations."""

    def test_machine_has_exception_frames_list(self):
        """Machine has exception_frames list."""
        machine = Machine()
        assert hasattr(machine, "exception_frames")
        assert isinstance(machine.exception_frames, list)
        assert len(machine.exception_frames) == 0

    def test_machine_has_ef_register(self):
        """Machine has EF register (exception frame pointer)."""
        machine = Machine()
        assert hasattr(machine, "EF")
        assert machine.EF is None  # No active exception frame initially

    def test_push_exception_frame_basic(self):
        """Push exception frame saves current machine state."""

        machine = Machine()
        machine.P = 10
        machine.CP = 42
        machine.E = 100
        machine.B = 50
        # Pre-allocate heap to H=75
        for _ in range(75):
            new_ref(machine)
        machine.TR = 5
        machine.HB = 70

        push_exception_frame(machine, ball_pattern=20, handler_label=200)

        # EF now points to first frame
        assert machine.EF == 0
        assert len(machine.exception_frames) == 1

        # Frame captured state
        frame = machine.exception_frames[0]
        assert frame.prev_frame is None
        assert frame.ball_pattern == 20
        assert frame.handler_label == 200
        assert frame.CP == 42
        assert frame.E == 100
        assert frame.B == 50
        assert frame.H == 75
        assert frame.TR == 5
        assert frame.HB == 70

    def test_push_multiple_exception_frames(self):
        """Multiple pushes create frame chain."""

        machine = Machine()
        machine.CP, machine.E = 10, 50
        for _ in range(25):
            new_ref(machine)

        # Push first frame
        push_exception_frame(machine, ball_pattern=10, handler_label=100)
        assert machine.EF == 0

        # Change state and push second frame
        machine.CP, machine.E = 20, 60
        for _ in range(5):
            new_ref(machine)
        push_exception_frame(machine, ball_pattern=20, handler_label=200)
        assert machine.EF == 1

        # Frames form chain
        assert machine.exception_frames[0].prev_frame is None
        assert machine.exception_frames[1].prev_frame == 0

        # Each frame captured different state
        assert machine.exception_frames[0].CP == 10
        assert machine.exception_frames[1].CP == 20

    def test_pop_exception_frame_basic(self):
        """Pop exception frame restores previous EF."""

        machine = Machine()
        push_exception_frame(machine, 10, 100)

        assert machine.EF == 0

        pop_exception_frame(machine)

        assert machine.EF is None
        # Frame still in list (not removed, just no longer active)

    def test_pop_exception_frame_nested(self):
        """Popping nested frame restores previous."""

        machine = Machine()
        push_exception_frame(machine, 10, 100)
        push_exception_frame(machine, 20, 200)

        assert machine.EF == 1

        pop_exception_frame(machine)

        assert machine.EF == 0  # Back to first frame

        pop_exception_frame(machine)

        assert machine.EF is None

    def test_pop_empty_stack_safe(self):
        """Popping empty exception stack is safe."""

        machine = Machine()
        assert machine.EF is None

        # Should not raise
        pop_exception_frame(machine)

        assert machine.EF is None


class TestExceptionFrameChaining:
    """Test exception frame linked list behavior."""

    def test_frame_chain_traversal(self):
        """Can traverse exception frames via prev_frame."""

        machine = Machine()

        # Create chain of 3 frames
        push_exception_frame(machine, 10, 100)
        push_exception_frame(machine, 20, 200)
        push_exception_frame(machine, 30, 300)

        # Traverse from current to bottom
        frames_visited = []
        ef = machine.EF
        while ef is not None:
            frames_visited.append(ef)
            frame = machine.exception_frames[ef]
            ef = frame.prev_frame

        assert frames_visited == [2, 1, 0]

    def test_nested_catch_frame_structure(self):
        """Nested catch creates proper frame hierarchy."""

        machine = Machine()

        # Outer catch
        for _ in range(10):
            new_ref(machine)
        push_exception_frame(machine, ball_pattern=5, handler_label=100)
        outer_ef = machine.EF

        # Inner catch (nested)
        for _ in range(10):
            new_ref(machine)
        push_exception_frame(machine, ball_pattern=15, handler_label=200)
        inner_ef = machine.EF

        # Inner frame links to outer
        inner_frame = machine.exception_frames[inner_ef]
        assert inner_frame.prev_frame == outer_ef

        # Each frame captured different heap state
        outer_frame = machine.exception_frames[outer_ef]
        assert outer_frame.H == 10
        assert inner_frame.H == 20


class TestExceptionFrameStateSnapshot:
    """Test that exception frames capture complete machine state."""

    def test_frame_captures_all_registers(self):
        """Frame captures P, CP, E, B, H, TR, HB."""

        machine = Machine()

        # Set all registers to distinct values
        machine.P = 1
        machine.CP = 2
        machine.E = 3
        machine.B = 4
        for _ in range(5):
            new_ref(machine)
        machine.TR = 6
        machine.HB = 7

        push_exception_frame(machine, ball_pattern=10, handler_label=100)

        frame = machine.exception_frames[0]
        # Note: P is not saved in exception frame (only in choicepoints)
        assert frame.CP == 2
        assert frame.E == 3
        assert frame.B == 4
        assert frame.H == 5
        assert frame.TR == 6
        assert frame.HB == 7

    def test_frame_independent_of_later_changes(self):
        """Frame snapshot is independent of later machine state changes."""

        machine = Machine()
        for _ in range(10):
            new_ref(machine)
        machine.TR = 5

        push_exception_frame(machine, 20, 200)
        saved_H = 10

        # Modify machine state after push
        for _ in range(40):
            new_ref(machine)
        machine.TR = 25

        # Frame still has old values
        frame = machine.exception_frames[0]
        assert frame.H == saved_H
        assert frame.TR == 5


class TestExceptionFrameEdgeCases:
    """Test edge cases and corner scenarios."""

    def test_many_nested_frames(self):
        """Can handle many nested exception frames."""

        machine = Machine()

        # Push 100 frames
        for i in range(100):
            push_exception_frame(machine, i, i * 10)

        assert machine.EF == 99
        assert len(machine.exception_frames) == 100

        # Pop all
        for i in range(100):
            pop_exception_frame(machine)

        assert machine.EF is None

    def test_frame_with_none_values(self):
        """Frame handles None values for optional fields."""

        frame = ExceptionFrame(
            prev_frame=None,
            ball_pattern=10,
            handler_label=100,
            CP=None,  # Can be None
            E=None,  # Can be None
            B=None,  # Can be None
            H=0,
            TR=0,
            HB=0,
        )

        assert frame.CP is None
        assert frame.E is None
        assert frame.B is None
