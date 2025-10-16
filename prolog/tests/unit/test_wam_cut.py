"""Tests for WAM cut operations (get_level, cut, neck_cut).

Cut operations prune choicepoints to prevent backtracking:
- get_level Yk: Save current B (choicepoint pointer) into Y register
- cut Yk: Prune all choicepoints above the level saved in Yk
- neck_cut: Prune choicepoints created by current clause (optimization)

Tests verify:
- Cut level saving and retrieval
- Choicepoint pruning above cut level
- HB (heap backtrack boundary) updates after cut
- Cut determinism (no backtracking through cut)
- Neck-cut optimization behavior
"""

import pytest
from prolog.wam.machine import Machine
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_DEALLOCATE,
    OP_GET_LEVEL,
    OP_CUT,
    OP_NECK_CUT,
    OP_TRY_ME_ELSE,
    OP_HALT,
)


class TestGetLevel:
    """Test get_level instruction for saving cut level."""

    def test_get_level_saves_current_B(self):
        """get_level Yk saves current B into Y register."""
        m = Machine()
        # Allocate environment with 1 slot
        m.code = [(OP_ALLOCATE, 1), (OP_GET_LEVEL, 0), (OP_HALT,)]
        m.P = 0

        # Create choicepoint (set B to some value)
        m.B = 16  # Simulated choicepoint address

        # Execute allocate
        m.step()
        assert m.E is not None
        assert m.P == 1

        # Execute get_level Y0
        m.step()
        assert m.P == 2

        # Check Y0 contains current B
        saved_b = m.get_y(0)
        assert saved_b == 16

    def test_get_level_saves_None_when_no_choicepoint(self):
        """get_level saves None when B is None (no choicepoints)."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_GET_LEVEL, 0), (OP_HALT,)]
        m.P = 0
        m.B = None  # No choicepoints

        # Execute allocate
        m.step()
        # Execute get_level Y0
        m.step()

        # Y0 should contain None
        saved_b = m.get_y(0)
        assert saved_b is None

    def test_get_level_multiple_choicepoints(self):
        """get_level captures current B with nested choicepoints."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),  # Create first choicepoint
            (OP_ALLOCATE, 1),
            (OP_GET_LEVEL, 0),  # Save level
            (OP_HALT,),
        ]
        m.P = 0

        # Execute try_me_else (creates choicepoint at B=0)
        m.step()
        first_b = m.B
        assert first_b == 0

        # Execute allocate
        m.step()
        # Execute get_level
        m.step()

        # Y0 should contain first_b
        saved_b = m.get_y(0)
        assert saved_b == first_b


class TestCut:
    """Test cut instruction for pruning choicepoints."""

    def test_cut_prunes_single_choicepoint(self):
        """cut Yk prunes choicepoints above saved level."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),  # Create choicepoint
            (OP_ALLOCATE, 1),
            (OP_GET_LEVEL, 0),  # Save level (B=0)
            (OP_TRY_ME_ELSE, 20),  # Create another choicepoint above
            (OP_CUT, 0),  # Cut to Y0 level
            (OP_HALT,),
        ]
        m.P = 0

        # Execute try_me_else (B=0)
        m.step()
        first_b = m.B
        assert first_b == 0

        # Execute allocate
        m.step()
        # Execute get_level Y0 (saves B=0)
        m.step()

        # Execute second try_me_else (B=8, nested choicepoint)
        m.step()
        second_b = m.B
        assert second_b == 8
        assert len(m.cp_stack) == 16  # 2 choicepoints

        # Execute cut Y0
        m.step()

        # B should be restored to first_b (level saved in Y0)
        assert m.B == first_b
        # Second choicepoint should still exist (cut sets B, doesn't remove stack entries)
        # WAM cut only updates B pointer, not cp_stack

    def test_cut_updates_HB_correctly(self):
        """cut updates HB to match new B's saved_HB."""
        m = Machine()
        # Create choicepoint structure manually for controlled testing
        m.cp_stack = [
            None,
            100,
            200,
            300,
            50,
            10,
            50,
            999,  # First choicepoint: saved_HB=50
        ]
        m.B = 0
        m.HB = 50

        m.code = [(OP_ALLOCATE, 1), (OP_GET_LEVEL, 0), (OP_CUT, 0), (OP_HALT,)]
        m.P = 0

        # Execute allocate and get_level
        m.step()
        m.step()

        # Execute cut
        m.step()

        # B should be 0 (saved level)
        assert m.B == 0
        # HB should be restored from choicepoint's saved_HB (index 6)
        assert m.HB == 50

    def test_cut_with_None_level_sets_B_None(self):
        """cut with None saved level sets B to None."""
        m = Machine()
        m.B = 8  # Current choicepoint
        m.cp_stack = [None] * 8

        m.code = [(OP_ALLOCATE, 1), (OP_GET_LEVEL, 0), (OP_CUT, 0), (OP_HALT,)]
        m.P = 0

        # Manually set Y0 to None (simulating no choicepoint at get_level time)
        m.step()  # allocate
        m.set_y(0, None)

        # Execute cut Y0
        m.P = 2
        m.step()

        # B should be None
        assert m.B is None
        # HB should be 0 when no choicepoints
        assert m.HB == 0

    def test_cut_determinism_no_backtrack(self):
        """After cut, backtracking stops at cut level."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 100),  # Outer choice
            (OP_ALLOCATE, 1),
            (OP_GET_LEVEL, 0),
            (OP_TRY_ME_ELSE, 200),  # Inner choice (will be pruned)
            (OP_CUT, 0),  # Cut to outer level
            (OP_HALT,),
        ]
        m.P = 0

        # Create outer choicepoint
        m.step()
        outer_b = m.B
        assert outer_b == 0

        # Allocate and save level
        m.step()
        m.step()
        saved_level = m.get_y(0)
        assert saved_level == outer_b

        # Create inner choicepoint
        m.step()
        inner_b = m.B
        assert inner_b == 8

        # Execute cut - should restore to outer level
        m.step()
        assert m.B == outer_b
        # Further backtracking would only reach outer_b, not beyond


class TestNeckCut:
    """Test neck_cut instruction (cut at clause entry)."""

    def test_neck_cut_prunes_clause_choicepoint(self):
        """neck_cut prunes choicepoint created by try_me_else."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),  # Create choicepoint for clause alternatives
            (OP_NECK_CUT,),  # Prune it immediately
            (OP_HALT,),
        ]
        m.P = 0

        # Execute try_me_else
        m.step()
        assert m.B == 0  # Choicepoint created

        # Execute neck_cut
        m.step()

        # B should be restored to prev_B (which is None for first choicepoint)
        assert m.B is None
        assert m.HB == 0

    def test_neck_cut_with_nested_choicepoints(self):
        """neck_cut only prunes topmost choicepoint."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),  # First choicepoint
            (OP_TRY_ME_ELSE, 20),  # Second choicepoint (nested)
            (OP_NECK_CUT,),  # Prune second only
            (OP_HALT,),
        ]
        m.P = 0

        # Create first choicepoint
        m.step()
        first_b = m.B
        assert first_b == 0

        # Create second choicepoint
        m.step()
        second_b = m.B
        assert second_b == 8

        # Execute neck_cut
        m.step()

        # B should be restored to first_b
        assert m.B == first_b
        # HB should be updated from first choicepoint
        assert m.HB == m.cp_stack[first_b + 6]

    def test_neck_cut_with_no_choicepoint_is_noop(self):
        """neck_cut when B is None does nothing."""
        m = Machine()
        m.code = [(OP_NECK_CUT,), (OP_HALT,)]
        m.P = 0
        m.B = None
        m.HB = 0

        # Execute neck_cut
        m.step()

        # Should remain unchanged
        assert m.B is None
        assert m.HB == 0


class TestCutScenarios:
    """Integration tests for cut in realistic scenarios."""

    def test_cut_prevents_backtracking_through_alternatives(self):
        """Cut prevents trying later alternatives after first succeeds."""
        m = Machine()
        # Simulate: p :- q, !, r.  with alternatives for q
        m.code = [
            # Alternative 1 for q
            (OP_TRY_ME_ELSE, 100),
            (OP_ALLOCATE, 1),
            (OP_GET_LEVEL, 0),  # Save cut level before alternatives
            # (would call q here)
            (OP_CUT, 0),  # Cut after first q succeeds
            # (would call r here)
            (OP_DEALLOCATE,),
            (OP_HALT,),
        ]
        m.P = 0

        # Try first alternative
        m.step()
        outer_b = m.B

        # Allocate, get_level
        m.step()
        m.step()

        # Suppose we had nested choices here - cut removes them
        # For this test, just verify cut restores B
        m.step()  # cut
        assert m.B == outer_b

    def test_multiple_cuts_in_sequence(self):
        """Multiple cuts work correctly."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),
            (OP_ALLOCATE, 2),
            (OP_GET_LEVEL, 0),
            (OP_TRY_ME_ELSE, 20),
            (OP_CUT, 0),  # First cut
            (OP_GET_LEVEL, 1),
            (OP_TRY_ME_ELSE, 30),
            (OP_CUT, 1),  # Second cut
            (OP_HALT,),
        ]
        m.P = 0

        # Create outer choice
        m.step()
        first_b = m.B

        # Allocate 2 slots, save first level
        m.step()
        m.step()

        # Create inner choice
        m.step()

        # First cut
        m.step()
        assert m.B == first_b

        # Save second level
        m.step()
        second_level = m.get_y(1)
        assert second_level == first_b

        # Create another choice
        m.step()

        # Second cut
        m.step()
        assert m.B == second_level


class TestCutEdgeCases:
    """Edge cases for cut operations."""

    def test_cut_with_invalid_Y_register(self):
        """cut with out-of-range Y register should fail gracefully."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_CUT, 5), (OP_HALT,)]  # Y5 doesn't exist
        m.P = 0

        m.step()  # allocate

        # Attempt to cut with invalid Y register
        # Should raise error or handle gracefully
        with pytest.raises((IndexError, AssertionError, RuntimeError)):
            m.step()

    def test_get_level_without_environment_fails(self):
        """get_level without allocate should fail."""
        m = Machine()
        m.code = [(OP_GET_LEVEL, 0), (OP_HALT,)]
        m.P = 0

        # get_level requires environment (E must not be None)
        with pytest.raises((AssertionError, RuntimeError)):
            m.step()

    def test_cut_with_stale_level_beyond_cp_stack(self):
        """cut with saved level beyond cp_stack should fail gracefully."""
        m = Machine()
        m.code = [(OP_ALLOCATE, 1), (OP_CUT, 0), (OP_HALT,)]
        m.P = 0

        m.step()  # allocate

        # Manually set Y0 to invalid level (beyond cp_stack)
        m.set_y(0, 100)  # Stale/invalid pointer

        # Attempt to cut with invalid level
        # Should raise error when trying to access cp_stack[100 + 6]
        with pytest.raises((IndexError, AssertionError, RuntimeError)):
            m.step()

    def test_neck_cut_updates_HB_from_prev_choicepoint(self):
        """neck_cut correctly updates HB from previous choicepoint."""
        m = Machine()
        # Manually create choicepoint chain
        m.cp_stack = [
            None,
            10,
            20,
            30,
            40,
            50,
            60,
            70,  # First: saved_HB=60
            0,
            11,
            21,
            31,
            41,
            51,
            80,
            71,  # Second: prev_B=0, saved_HB=80
        ]
        m.B = 8  # Points to second choicepoint
        m.HB = 80

        m.code = [(OP_NECK_CUT,), (OP_HALT,)]
        m.P = 0

        # Execute neck_cut
        m.step()

        # B should point to first choicepoint
        assert m.B == 0
        # HB should be from first choicepoint's saved_HB (index 6)
        assert m.HB == 60


class TestCutInvariantMaintenance:
    """Verify cut operations maintain machine invariants."""

    def test_cut_maintains_valid_B_pointer(self):
        """After cut, B points to valid choicepoint or None."""
        m = Machine()
        m.cp_stack = [None] * 8
        m.B = 0

        m.code = [(OP_ALLOCATE, 1), (OP_GET_LEVEL, 0), (OP_CUT, 0), (OP_HALT,)]
        m.P = 0

        m.step()  # allocate
        m.step()  # get_level
        m.step()  # cut

        # B should be valid
        assert m.B is None or (isinstance(m.B, int) and m.B % 8 == 0)

        # Verify machine invariants after cut
        m.check_invariants()

    def test_neck_cut_maintains_choicepoint_chain_integrity(self):
        """neck_cut maintains valid prev_B chain."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),
            (OP_TRY_ME_ELSE, 20),
            (OP_NECK_CUT,),
            (OP_HALT,),
        ]
        m.P = 0

        m.step()  # First choicepoint
        m.step()  # Second choicepoint
        m.step()  # neck_cut

        # After neck_cut, B should point to valid choicepoint
        if m.B is not None:
            assert m.B % 8 == 0
            assert m.B < len(m.cp_stack)

        # Verify machine invariants after neck_cut
        m.check_invariants()
