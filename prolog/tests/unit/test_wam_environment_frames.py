"""Unit tests for WAM environment frame operations.

Tests for allocate/deallocate instructions and Y register access.

Frame layout:
    [prev_E, saved_CP, n_slots, Y0, Y1, ..., Y_{N-1}]

Where:
- prev_E: Previous environment frame pointer (int or None)
- saved_CP: Continuation pointer to restore on return (int or None)
- n_slots: Number of Y register slots in this frame (int)
- Y0, Y1, ...: Permanent variable slots (N slots)
"""

import pytest
from prolog.wam.heap import new_ref
from prolog.wam.instructions import OP_ALLOCATE, OP_DEALLOCATE, OP_HALT
from prolog.wam.machine import Machine


class TestAllocateInstruction:
    """Test allocate N instruction."""

    def test_allocate_creates_frame_with_correct_size(self):
        """allocate N creates frame with N permanent variable slots."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 3), (OP_HALT,)]
        m.CP = 42  # Some return address

        m.step()

        # Frame should have: [prev_E, saved_CP, n_slots, Y0, Y1, Y2]
        assert len(m.frames) == 6
        assert m.frames[0] is None  # prev_E (no previous frame)
        assert m.frames[1] == 42  # saved_CP
        assert m.frames[2] == 3  # n_slots
        assert m.frames[3] is None  # Y0
        assert m.frames[4] is None  # Y1
        assert m.frames[5] is None  # Y2

    def test_allocate_sets_E_register(self):
        """allocate sets E to point to new frame base."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_HALT,)]

        assert m.E is None  # Initially no frame
        m.step()
        assert m.E == 0  # E points to start of frame

    def test_allocate_saves_previous_E(self):
        """allocate saves previous E in new frame."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_ALLOCATE, 1), (OP_HALT,)]
        m.CP = 10

        # First allocate
        m.step()
        first_E = m.E
        assert m.frames[first_E] is None  # No previous frame

        # Second allocate (nested)
        m.CP = 20
        m.step()
        second_E = m.E
        assert m.frames[second_E] == first_E  # Saves first frame pointer

    def test_allocate_with_zero_slots(self):
        """allocate 0 creates frame with no Y variables."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 0), (OP_HALT,)]
        m.CP = 99

        m.step()

        # Frame should have: [prev_E, saved_CP, n_slots]
        assert len(m.frames) == 3
        assert m.frames[0] is None
        assert m.frames[1] == 99
        assert m.frames[2] == 0  # n_slots = 0

    def test_allocate_advances_program_counter(self):
        """allocate advances P by 1."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_HALT,)]

        assert m.P == 0
        m.step()
        assert m.P == 1


class TestDeallocateInstruction:
    """Test deallocate instruction."""

    def test_deallocate_restores_CP(self):
        """deallocate restores CP from frame."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_DEALLOCATE,), (OP_HALT,)]
        m.CP = 42

        m.step()  # allocate
        m.CP = 99  # Change CP after allocate
        m.step()  # deallocate

        assert m.CP == 42  # Restored from frame

    def test_deallocate_restores_E(self):
        """deallocate restores E to previous frame."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_DEALLOCATE,), (OP_HALT,)]

        m.step()  # allocate
        assert m.E == 0
        m.step()  # deallocate
        assert m.E is None  # Restored to no frame

    def test_deallocate_with_nested_frames(self):
        """deallocate correctly unwinds nested frames."""
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 1),  # Frame 1
            (OP_ALLOCATE, 2),  # Frame 2 (nested)
            (OP_DEALLOCATE,),  # Pop Frame 2
            (OP_DEALLOCATE,),  # Pop Frame 1
            (OP_HALT,),
        ]
        m.CP = 10

        m.step()  # allocate frame 1
        frame1_E = m.E
        assert frame1_E == 0

        m.CP = 20
        m.step()  # allocate frame 2
        frame2_E = m.E
        assert frame2_E > frame1_E

        m.step()  # deallocate frame 2
        assert m.E == frame1_E
        assert m.CP == 20  # CP from frame 2 (restored)

        m.step()  # deallocate frame 1
        assert m.E is None
        assert m.CP == 10  # CP from frame 1 (restored)

    def test_deallocate_advances_program_counter(self):
        """deallocate advances P by 1."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_DEALLOCATE,), (OP_HALT,)]
        m.CP = 10

        m.step()  # allocate
        assert m.P == 1
        m.step()  # deallocate
        assert m.P == 2


class TestYRegisterAccess:
    """Test Y register read/write operations."""

    def test_get_y_reads_from_current_frame(self):
        """get_y reads Y register from current frame."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 3), (OP_HALT,)]
        m.step()  # allocate

        # Manually set Y registers (frame: [prev_E, saved_CP, n_slots, Y0, Y1, Y2])
        m.frames[m.E + 3] = 100  # Y0
        m.frames[m.E + 4] = 200  # Y1
        m.frames[m.E + 5] = 300  # Y2

        assert m.get_y(0) == 100
        assert m.get_y(1) == 200
        assert m.get_y(2) == 300

    def test_set_y_writes_to_current_frame(self):
        """set_y writes Y register to current frame."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_HALT,)]
        m.step()  # allocate

        m.set_y(0, 42)
        m.set_y(1, 99)

        assert m.frames[m.E + 3] == 42
        assert m.frames[m.E + 4] == 99

    def test_y_registers_persist_across_nested_frames(self):
        """Y registers in outer frame remain accessible after inner frame."""
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 2),  # Outer frame
            (OP_ALLOCATE, 1),  # Inner frame
            (OP_DEALLOCATE,),  # Pop inner
            (OP_HALT,),
        ]
        m.CP = 10

        m.step()  # allocate outer
        m.set_y(0, 111)
        m.set_y(1, 222)

        m.CP = 20
        m.step()  # allocate inner
        m.set_y(0, 333)  # Inner Y0

        m.step()  # deallocate inner

        # Back to outer frame, Y registers should be intact
        assert m.get_y(0) == 111
        assert m.get_y(1) == 222

    def test_y_registers_with_heap_addresses(self):
        """Y registers can store heap addresses (REF cells)."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_HALT,)]
        m.step()  # allocate

        # Create REF cells on heap
        addr1 = new_ref(m)
        addr2 = new_ref(m)

        # Store in Y registers
        m.set_y(0, addr1)
        m.set_y(1, addr2)

        assert m.get_y(0) == addr1
        assert m.get_y(1) == addr2


class TestFrameInvariants:
    """Test environment frame invariants."""

    def test_E_points_to_valid_frame_after_allocate(self):
        """E points to valid frame base after allocate."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 3), (OP_HALT,)]
        m.step()

        assert m.E is not None
        assert 0 <= m.E < len(m.frames)
        # Should have at least prev_E and saved_CP
        assert m.E + 1 < len(m.frames)

    def test_frame_chain_integrity(self):
        """Frame chain via prev_E maintains stack discipline."""
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 1),  # A
            (OP_ALLOCATE, 1),  # B
            (OP_ALLOCATE, 1),  # C
            (OP_HALT,),
        ]
        m.CP = 10

        m.step()  # A
        E_A = m.E
        assert m.frames[E_A] is None  # No previous

        m.CP = 20
        m.step()  # B
        E_B = m.E
        assert m.frames[E_B] == E_A  # Points to A

        m.CP = 30
        m.step()  # C
        E_C = m.E
        assert m.frames[E_C] == E_B  # Points to B

        # Walk chain: C -> B -> A -> None
        chain = []
        E = m.E
        while E is not None:
            chain.append(E)
            E = m.frames[E]
        assert chain == [E_C, E_B, E_A]

    def test_E_is_None_after_all_deallocations(self):
        """E is None after all frames are deallocated."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_DEALLOCATE,), (OP_HALT,)]
        m.CP = 10

        m.step()  # allocate
        assert m.E is not None
        m.step()  # deallocate
        assert m.E is None


class TestFrameEdgeCases:
    """Test edge cases and error conditions."""

    def test_allocate_multiple_times_extends_frames(self):
        """Multiple allocates extend frames list correctly."""
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 2),
            (OP_ALLOCATE, 3),
            (OP_ALLOCATE, 1),
            (OP_HALT,),
        ]
        m.CP = 10

        m.step()  # 5 slots (prev_E, CP, n_slots, Y0, Y1)
        assert len(m.frames) == 5

        m.CP = 20
        m.step()  # +6 slots (prev_E, CP, n_slots, Y0, Y1, Y2)
        assert len(m.frames) == 11

        m.CP = 30
        m.step()  # +4 slots (prev_E, CP, n_slots, Y0)
        assert len(m.frames) == 15

    def test_y_register_bounds_check(self):
        """Accessing Y register out of bounds raises error."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_HALT,)]
        m.step()  # 2 Y slots: Y0, Y1

        # Valid accesses
        m.set_y(0, 42)
        m.set_y(1, 99)

        # Out of bounds (Y2 doesn't exist in this frame)
        with pytest.raises((IndexError, AssertionError)):
            m.get_y(2)

    def test_deallocate_without_frame_is_error(self):
        """Deallocate when E is None should raise error or halt."""
        m = Machine()
        m.code = [(OP_DEALLOCATE,), (OP_HALT,)]

        # E is None initially
        assert m.E is None

        # Should either halt or raise error
        result = m.step()
        assert result is False or m.halted

    def test_y_register_bounds_after_nested_deallocation(self):
        """Y register access must validate against current frame size, not total frames length.

        Regression test: After allocating nested frames and deallocating the inner one,
        accessing Y registers beyond the outer frame's size should raise an error,
        even though the frames list still contains the deallocated inner frame's data.
        """
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 2),  # Outer: Y0, Y1 (2 slots)
            (OP_ALLOCATE, 1),  # Inner: Y0 (1 slot)
            (OP_DEALLOCATE,),  # Pop inner, back to outer
            (OP_HALT,),
        ]
        m.CP = 10

        m.step()  # Allocate outer (2 Y slots)
        outer_E = m.E
        m.set_y(0, 100)
        m.set_y(1, 200)

        m.CP = 20
        m.step()  # Allocate inner (1 Y slot)

        m.step()  # Deallocate inner, back to outer
        assert m.E == outer_E

        # Outer frame has only Y0 and Y1 (indices 0 and 1)
        assert m.get_y(0) == 100  # Valid
        assert m.get_y(1) == 200  # Valid

        # Y2 is out of bounds for outer frame (even though frames list is longer)
        with pytest.raises((IndexError, AssertionError)):
            m.get_y(2)

        with pytest.raises((IndexError, AssertionError)):
            m.set_y(2, 999)


class TestFrameIntegration:
    """Integration tests combining multiple operations."""

    def test_allocate_set_deallocate_sequence(self):
        """Complete sequence: allocate, use Y registers, deallocate."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_DEALLOCATE,), (OP_HALT,)]
        m.CP = 100

        # Allocate
        m.step()
        assert m.E is not None

        # Use Y registers
        m.set_y(0, 42)
        m.set_y(1, 99)
        assert m.get_y(0) == 42
        assert m.get_y(1) == 99

        # Deallocate
        m.step()
        assert m.E is None
        assert m.CP == 100

    def test_three_level_nesting(self):
        """Three levels of nested frames."""
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 1),  # Level 1
            (OP_ALLOCATE, 1),  # Level 2
            (OP_ALLOCATE, 1),  # Level 3
            (OP_DEALLOCATE,),  # Pop 3
            (OP_DEALLOCATE,),  # Pop 2
            (OP_DEALLOCATE,),  # Pop 1
            (OP_HALT,),
        ]
        m.CP = 10

        m.step()
        E1 = m.E
        m.set_y(0, 111)

        m.CP = 20
        m.step()
        E2 = m.E
        m.set_y(0, 222)

        m.CP = 30
        m.step()
        E3 = m.E
        m.set_y(0, 333)

        # At level 3
        assert m.get_y(0) == 333
        assert m.E == E3

        m.step()  # Pop 3
        assert m.E == E2
        assert m.get_y(0) == 222

        m.step()  # Pop 2
        assert m.E == E1
        assert m.get_y(0) == 111

        m.step()  # Pop 1
        assert m.E is None

    def test_frame_operations_preserve_heap(self):
        """Frame operations don't corrupt heap."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 2), (OP_DEALLOCATE,), (OP_HALT,)]
        m.CP = 10

        # Create some heap cells before frames
        new_ref(m)
        new_ref(m)
        initial_H = m.H

        m.step()  # allocate
        m.step()  # deallocate

        # Heap should be unchanged
        assert m.H == initial_H
        assert len(m.heap) == initial_H

    def test_deallocate_actually_frees_frame_memory(self):
        """deallocate must actually remove frame from frames list to prevent memory leak.

        Regression test: deallocate was only restoring CP/E but leaving stale data in
        frames list, causing unbounded growth in tail-recursive/loop scenarios.
        """
        m = Machine()
        # Simulate tail-recursive loop: allocate then deallocate repeatedly
        m.code = [
            (OP_ALLOCATE, 2),
            (OP_DEALLOCATE,),
            (OP_ALLOCATE, 2),
            (OP_DEALLOCATE,),
            (OP_ALLOCATE, 2),
            (OP_DEALLOCATE,),
            (OP_HALT,),
        ]
        m.CP = 10

        # First iteration
        m.step()  # allocate
        assert m.E == 0
        first_frame_size = len(m.frames)
        assert first_frame_size == 5  # [prev_E, CP, n_slots, Y0, Y1]

        m.step()  # deallocate
        assert m.E is None
        # Frame should be removed from frames list
        assert len(m.frames) == 0, "deallocate should free frame memory"

        # Second iteration
        m.step()  # allocate
        assert m.E == 0
        assert len(m.frames) == 5

        m.step()  # deallocate
        assert m.E is None
        assert len(m.frames) == 0, "frames should not grow on repeated alloc/dealloc"

        # Third iteration
        m.step()  # allocate
        assert m.E == 0
        assert len(m.frames) == 5

        m.step()  # deallocate
        assert m.E is None
        assert len(m.frames) == 0, "frames should remain bounded"
