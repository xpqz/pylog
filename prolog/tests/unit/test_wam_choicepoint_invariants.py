"""Tests for WAM choicepoint invariant checks.

These tests verify that _check_choicepoint_invariants() properly detects
invalid choicepoint stack states that could indicate bugs in the
choicepoint operations.
"""

import pytest

from prolog.wam.instructions import (
    OP_HALT,
    OP_RETRY_ME_ELSE,
    OP_TRUST_ME,
    OP_TRY_ME_ELSE,
)
from prolog.wam.machine import Machine


class TestChoicepointInvariantChecks:
    """Test choicepoint invariant validation."""

    def test_valid_empty_cp_stack(self):
        """Empty cp_stack with B=None is valid."""
        m = Machine()
        m.code = [(OP_HALT,)]
        # Empty cp_stack, B=None
        assert len(m.cp_stack) == 0
        assert m.B is None
        # Should not raise
        m._check_choicepoint_invariants()

    def test_valid_single_choicepoint(self):
        """Single choicepoint with correct structure is valid."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 2),  # 0
            (OP_HALT,),  # 1
            (OP_HALT,),  # 2
        ]
        m.step()  # Create choicepoint
        # Should have 8 fields, B=0
        assert len(m.cp_stack) == 8
        assert m.B == 0
        # Should not raise
        m._check_choicepoint_invariants()

    def test_valid_nested_choicepoints(self):
        """Nested choicepoints with valid prev_B chain."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0: Outer
            (OP_TRY_ME_ELSE, 6),  # 1: Inner
            (OP_HALT,),  # 2
            (OP_HALT,),  # 3
            (OP_HALT,),  # 4
            (OP_HALT,),  # 5
            (OP_HALT,),  # 6
        ]
        m.step()  # Outer choicepoint at 0
        m.step()  # Inner choicepoint at 8
        assert len(m.cp_stack) == 16
        assert m.B == 8
        assert m.cp_stack[8] == 0  # prev_B points to outer
        # Should not raise
        m._check_choicepoint_invariants()

    def test_invalid_cp_stack_not_multiple_of_8(self):
        """Detect cp_stack length not multiple of 8."""
        m = Machine()
        # Manually corrupt cp_stack
        m.cp_stack = [None] * 7  # 7 fields, not 8
        m.B = None
        with pytest.raises(AssertionError, match="not a multiple of 8"):
            m._check_choicepoint_invariants()

    def test_invalid_B_not_aligned(self):
        """Detect B not aligned to choicepoint boundary."""
        m = Machine()
        m.cp_stack = [None] * 16  # 2 choicepoints
        m.B = 5  # Not aligned to 8
        with pytest.raises(AssertionError, match="not multiple of 8"):
            m._check_choicepoint_invariants()

    def test_invalid_B_beyond_cp_stack(self):
        """Detect B pointing beyond cp_stack."""
        m = Machine()
        m.cp_stack = [None] * 8  # 1 choicepoint
        m.B = 16  # Beyond end
        with pytest.raises(AssertionError, match="beyond cp_stack"):
            m._check_choicepoint_invariants()

    def test_invalid_B_negative(self):
        """Detect negative B value."""
        m = Machine()
        m.cp_stack = [None] * 8
        m.B = -8
        with pytest.raises(AssertionError, match="negative"):
            m._check_choicepoint_invariants()

    def test_invalid_prev_B_not_aligned(self):
        """Detect prev_B not aligned to choicepoint boundary."""
        m = Machine()
        m.cp_stack = [None] * 16
        m.cp_stack[0] = None  # Outer prev_B
        m.cp_stack[8] = 5  # Inner prev_B not aligned
        m.B = 8
        with pytest.raises(AssertionError, match="not aligned"):
            m._check_choicepoint_invariants()

    def test_invalid_prev_B_not_backward(self):
        """Detect prev_B not pointing backward."""
        m = Machine()
        m.cp_stack = [None] * 16
        m.cp_stack[0] = None  # Outer prev_B
        m.cp_stack[8] = 16  # Inner prev_B points forward (invalid)
        m.B = 8
        with pytest.raises(AssertionError, match="not point backward"):
            m._check_choicepoint_invariants()

    def test_invalid_prev_B_negative(self):
        """Detect negative prev_B value."""
        m = Machine()
        m.cp_stack = [None] * 16
        m.cp_stack[0] = None
        m.cp_stack[8] = -8  # Negative prev_B
        m.B = 8
        with pytest.raises(AssertionError, match="negative"):
            m._check_choicepoint_invariants()

    def test_invalid_prev_B_cycle(self):
        """Detect cycle in prev_B chain.

        The cycle is caught either by "doesn't point backward" check
        or explicit cycle detection, both are valid.
        """
        m = Machine()
        # Create 3 choicepoints
        m.cp_stack = [None] * 24
        m.cp_stack[0] = 16  # Outer points to third (creates cycle)
        m.cp_stack[8] = 0  # Middle points to outer
        m.cp_stack[16] = 8  # Third points to middle
        m.B = 16
        with pytest.raises(AssertionError, match="(Cycle detected|not point backward)"):
            m._check_choicepoint_invariants()

    def test_check_invariants_includes_choicepoint_checks(self):
        """check_invariants() includes choicepoint invariant checks."""
        m = Machine()
        m.cp_stack = [None] * 7  # Invalid length
        m.B = None
        # check_invariants should call _check_choicepoint_invariants
        with pytest.raises(AssertionError, match="not a multiple of 8"):
            m.check_invariants()


class TestChoicepointInvariantsAfterOperations:
    """Test that choicepoint operations maintain invariants."""

    def test_try_me_else_maintains_invariants(self):
        """try_me_else creates valid choicepoint structure."""
        m = Machine()
        m.code = [(OP_TRY_ME_ELSE, 2), (OP_HALT,), (OP_HALT,)]
        m.step()
        # Should not raise
        m.check_invariants()

    def test_retry_me_else_maintains_invariants(self):
        """retry_me_else maintains valid choicepoint structure."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_HALT,),  # 1
            (OP_HALT,),  # 2
            (OP_RETRY_ME_ELSE, 5),  # 3
            (OP_HALT,),  # 4
            (OP_HALT,),  # 5
        ]
        m.step()  # Create choicepoint
        m.P = 3  # Jump to retry
        m.step()  # Retry
        # Should not raise
        m.check_invariants()

    def test_trust_me_maintains_invariants(self):
        """trust_me maintains valid structure after popping."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_HALT,),  # 1
            (OP_HALT,),  # 2
            (OP_TRUST_ME,),  # 3
            (OP_HALT,),  # 4
        ]
        m.step()  # Create choicepoint
        m.P = 3  # Jump to trust
        m.step()  # Trust (pops choicepoint)
        # Should not raise
        m.check_invariants()
        # Should have empty cp_stack, B=None
        assert len(m.cp_stack) == 0
        assert m.B is None

    def test_nested_choicepoints_maintain_invariants(self):
        """Nested operations maintain invariants."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0: Outer
            (OP_TRY_ME_ELSE, 6),  # 1: Inner
            (OP_HALT,),  # 2
            (OP_TRUST_ME,),  # 3: Pop inner
            (OP_HALT,),  # 4
            (OP_HALT,),  # 5
            (OP_TRUST_ME,),  # 6: Pop outer
            (OP_HALT,),  # 7
        ]
        m.step()  # Outer
        m.check_invariants()
        m.step()  # Inner
        m.check_invariants()
        m.P = 3  # Jump to first trust
        m.step()  # Pop inner
        m.check_invariants()
        # Should have outer choicepoint only
        assert len(m.cp_stack) == 8
        assert m.B == 0
        m.P = 6  # Jump to second trust
        m.step()  # Pop outer
        m.check_invariants()
        # Should be empty
        assert len(m.cp_stack) == 0
        assert m.B is None
