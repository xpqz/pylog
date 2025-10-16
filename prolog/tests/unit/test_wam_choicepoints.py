"""Tests for WAM choicepoint operations (Phase 2).

Choicepoints enable backtracking by saving machine state at choice points.
This test suite covers try_me_else, retry_me_else, and trust_me instructions.

Choicepoint Layout:
    [prev_B, saved_CP, saved_E, saved_P, saved_H, saved_TR, saved_HB, alt_ptr]

Instructions:
- try_me_else L: Create choicepoint with alternative at L, continue to next instruction
- retry_me_else L: Restore state from current choicepoint, update alternative to L
- trust_me: Restore state from current choicepoint, pop it (no more alternatives)
"""

from prolog.wam.heap import TAG_CON, TAG_REF, new_con, new_ref
from prolog.wam.unify import bind
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_CALL,
    OP_DEALLOCATE,
    OP_HALT,
    OP_PROCEED,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_PUT_VARIABLE,
    OP_RETRY_ME_ELSE,
    OP_TRUST_ME,
    OP_TRY_ME_ELSE,
)
from prolog.wam.machine import Machine


class TestChoicepointBasics:
    """Basic choicepoint creation and structure tests."""

    def test_try_me_else_creates_choicepoint(self):
        """try_me_else creates a choicepoint record and continues execution."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 5),  # 0: Create choicepoint, alternative at 5
            (OP_HALT,),  # 1: First clause body
            (OP_HALT,),  # 2: (pad)
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_HALT,),  # 5: Alternative clause
        ]

        # Set up some initial state
        m.CP = 100
        m.E = None
        m.H = 0
        m.TR = 0

        # Execute try_me_else
        m.step()

        # Should have created a choicepoint
        assert m.B is not None, "B should point to new choicepoint"
        assert m.B == 0, "First choicepoint should be at index 0"
        assert len(m.cp_stack) == 8, "Choicepoint record has 8 fields"

        # Verify choicepoint fields: [prev_B, saved_CP, saved_E, saved_P, saved_H, saved_TR, saved_HB, alt_ptr]
        assert m.cp_stack[0] is None, "prev_B should be None (first choicepoint)"
        assert m.cp_stack[1] == 100, "saved_CP should be 100"
        assert m.cp_stack[2] is None, "saved_E should be None"
        assert m.cp_stack[3] == 0, "saved_P should be 0 (before try_me_else)"
        assert m.cp_stack[4] == 0, "saved_H should be 0"
        assert m.cp_stack[5] == 0, "saved_TR should be 0"
        assert m.cp_stack[6] == 0, "saved_HB should be 0"
        assert m.cp_stack[7] == 5, "alt_ptr should be 5"

        # P should advance to next instruction (not jump to alternative)
        assert m.P == 1, "P should advance to next instruction"

        # HB should be updated to current H
        assert m.HB == 0, "HB should be set to H at choice time"

    def test_try_me_else_with_active_environment(self):
        """try_me_else saves current E register in choicepoint."""
        m = Machine()
        m.code = [
            (OP_ALLOCATE, 2),  # 0: Create frame
            (OP_TRY_ME_ELSE, 5),  # 1: Create choicepoint
            (OP_HALT,),  # 2: First clause
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_HALT,),  # 5: Alternative
        ]
        m.CP = 100

        m.step()  # allocate
        frame_addr = m.E
        assert frame_addr == 0

        m.step()  # try_me_else

        # Choicepoint should save E
        assert m.cp_stack[2] == frame_addr, "saved_E should point to frame"

    def test_try_me_else_with_heap_and_trail(self):
        """try_me_else saves H and TR at choice time."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 2),
            (OP_HALT,),
            (OP_HALT,),
        ]

        # Modify heap and trail before choice
        new_ref(m)
        new_con(m, 42)
        m.trail.extend([("bind", 0, (TAG_REF, 0))])
        m.TR = len(m.trail)

        m.step()

        # Choicepoint should snapshot H and TR
        assert m.cp_stack[4] == 2, "saved_H should be 2"
        assert m.cp_stack[5] == 1, "saved_TR should be 1"
        assert m.cp_stack[6] == 2, "saved_HB should equal saved_H"

    def test_nested_choicepoints(self):
        """Multiple try_me_else create a linked choicepoint chain."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 5),  # 0: First choice
            (OP_TRY_ME_ELSE, 10),  # 1: Nested choice
            (OP_HALT,),  # 2: (pad)
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_HALT,),  # 5: First alternative
            (OP_HALT,),  # 6: (pad)
            (OP_HALT,),  # 7: (pad)
            (OP_HALT,),  # 8: (pad)
            (OP_HALT,),  # 9: (pad)
            (OP_HALT,),  # 10: Second alternative
        ]

        m.step()  # First try_me_else
        first_B = m.B
        assert first_B == 0
        assert m.cp_stack[0] is None, "First choicepoint has no prev_B"

        m.step()  # Second try_me_else
        second_B = m.B
        assert second_B == 8, "Second choicepoint starts at index 8"
        assert m.cp_stack[8] == first_B, "Second choicepoint's prev_B points to first"


class TestRetryMeElse:
    """Tests for retry_me_else instruction (restore and update alternative)."""

    def test_retry_me_else_restores_state(self):
        """retry_me_else restores machine state from current choicepoint."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 10),  # 0: Create choicepoint
            (OP_HALT,),  # 1: (pad)
            (OP_HALT,),  # 2: (pad)
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_HALT,),  # 5: (pad)
            (OP_HALT,),  # 6: (pad)
            (OP_HALT,),  # 7: (pad)
            (OP_HALT,),  # 8: (pad)
            (OP_HALT,),  # 9: (pad)
            (OP_RETRY_ME_ELSE, 20),  # 10: Alternative - restore state, next alt at 20
            (OP_HALT,),  # 11: (pad)
        ]
        m.CP = 100

        # Execute first clause attempt
        m.step()  # try_me_else - saves P=0, CP=100, H=0, TR=0
        choicepoint_B = m.B
        assert m.P == 1

        # Modify machine state
        new_ref(m)
        new_con(m, 99)
        m.trail.append(("bind", 0, (TAG_REF, 0)))
        m.TR = len(m.trail)
        m.CP = 200  # Changed CP

        # Now backtrack with retry_me_else
        m.P = 10  # Jump to retry_me_else manually
        m.step()

        # State should be restored from choicepoint
        assert m.CP == 100, "CP should be restored to saved value"
        assert m.E is None, "E should be restored"
        assert m.H == 0, "H should be restored (heap unwound)"
        assert len(m.heap) == 0, "Heap should be truncated to saved_H"
        assert m.TR == 0, "TR should be restored"
        assert len(m.trail) == 0, "Trail should be truncated to saved_TR"
        assert m.HB == 0, "HB should be restored to saved_HB"

        # Choicepoint should still exist but alt_ptr updated
        assert m.B == choicepoint_B, "B should still point to same choicepoint"
        assert m.cp_stack[7] == 20, "alt_ptr should be updated to 20"

        # P should advance to next instruction after retry_me_else
        assert m.P == 11, "P should advance to instruction after retry_me_else"

    def test_retry_me_else_unwinds_trail(self):
        """retry_me_else must unwind bindings from trail."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 5),  # 0: Create choicepoint
            (OP_PUT_VARIABLE, 0, 0),  # 1: Create var in X0
            (OP_HALT,),  # 2: (pad)
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_RETRY_ME_ELSE, 10),  # 5: Backtrack
            (OP_HALT,),  # 6: Second clause
        ]

        m.step()  # try_me_else - H=0, TR=0
        saved_H = m.H
        saved_TR = m.TR

        m.step()  # put_variable X0, A0 - creates ref on heap
        ref_addr = 0
        assert m.H == 1, "Heap should have one cell"
        assert m.X[0] == ref_addr

        # Bind the variable and trail it
        const_addr = new_con(m, 42)
        bind(m, ref_addr, const_addr)
        assert len(m.trail) > saved_TR, "Trail should have binding"
        assert m.heap[ref_addr] == (TAG_CON, 42), "Variable should be bound"

        # Backtrack with retry_me_else
        m.P = 5
        m.step()

        # Binding should be undone
        assert m.H == saved_H, "H restored"
        assert m.TR == saved_TR, "TR restored"
        assert len(m.heap) == saved_H, "Heap truncated"
        assert len(m.trail) == saved_TR, "Trail truncated"

    def test_retry_me_else_preserves_earlier_bindings(self):
        """retry_me_else only unwinds trail entries after saved_TR."""
        m = Machine()
        m.code = [
            (OP_PUT_VARIABLE, 0, 0),  # 0: Create var before choice (X0 -> heap[0])
            (OP_TRY_ME_ELSE, 5),  # 1: Create choicepoint
            (OP_PUT_VARIABLE, 1, 1),  # 2: Create var after choice (X1 -> heap[1])
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_RETRY_ME_ELSE, 10),  # 5: Backtrack
            (OP_HALT,),  # 6: Alternative
        ]

        m.step()  # put_variable X0 - heap[0]
        early_ref_addr = 0
        assert m.H == 1

        # Bind before choice and trail it
        early_const = new_con(m, 10)
        bind(m, early_ref_addr, early_const)
        early_trail_len = len(m.trail)
        assert early_trail_len > 0

        m.step()  # try_me_else - saves TR (with early binding)
        saved_TR = m.cp_stack[5]
        assert saved_TR == early_trail_len

        m.step()  # put_variable X1 - heap[2] (after early const at 1)
        late_ref_addr = 2
        assert m.H == 3

        # Bind after choice
        late_const = new_con(m, 20)
        bind(m, late_ref_addr, late_const)
        assert len(m.trail) > early_trail_len

        # Backtrack
        m.P = 5
        m.step()

        # Late binding undone, early binding preserved
        assert len(m.trail) == saved_TR, "Trail truncated to saved_TR"
        assert len(m.heap) == 2, "Heap truncated to saved_H (2: ref + const)"
        # Early binding should still be in trail (before saved_TR)
        assert m.trail[0][1] == early_ref_addr


class TestTrustMe:
    """Tests for trust_me instruction (restore and pop choicepoint)."""

    def test_trust_me_restores_and_pops(self):
        """trust_me restores state and removes current choicepoint."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 5),  # 0: Create choicepoint
            (OP_HALT,),  # 1: First clause
            (OP_HALT,),  # 2: (pad)
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_TRUST_ME,),  # 5: Last alternative
            (OP_HALT,),  # 6: Final clause
        ]
        m.CP = 100

        m.step()  # try_me_else
        choicepoint_B = m.B
        assert choicepoint_B == 0
        assert len(m.cp_stack) == 8

        # Modify state
        new_ref(m)
        new_con(m, 5)
        m.trail.append(("bind", 0, (TAG_REF, 0)))
        m.TR = len(m.trail)
        m.CP = 200

        # Backtrack with trust_me
        m.P = 5
        m.step()

        # State restored
        assert m.CP == 100, "CP restored"
        assert m.H == 0, "H restored"
        assert m.TR == 0, "TR restored"
        assert len(m.heap) == 0, "Heap truncated"
        assert len(m.trail) == 0, "Trail truncated"

        # Choicepoint removed
        assert m.B is None, "B should be None (no choicepoints)"
        assert len(m.cp_stack) == 0, "cp_stack should be empty"

        # P advances
        assert m.P == 6, "P should advance to next instruction"

    def test_trust_me_restores_previous_choicepoint(self):
        """trust_me pops current choicepoint and restores B to prev_B."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 5),  # 0: Outer choice
            (OP_TRY_ME_ELSE, 10),  # 1: Inner choice
            (OP_HALT,),  # 2: (pad)
            (OP_HALT,),  # 3: (pad)
            (OP_HALT,),  # 4: (pad)
            (OP_HALT,),  # 5: Outer alt
            (OP_HALT,),  # 6: (pad)
            (OP_HALT,),  # 7: (pad)
            (OP_HALT,),  # 8: (pad)
            (OP_HALT,),  # 9: (pad)
            (OP_TRUST_ME,),  # 10: Inner final alt
            (OP_HALT,),  # 11: (pad)
        ]

        m.step()  # Outer try_me_else
        outer_B = m.B
        assert outer_B == 0

        m.step()  # Inner try_me_else
        inner_B = m.B
        assert inner_B == 8
        assert m.cp_stack[8] == outer_B, "Inner prev_B points to outer"

        # Backtrack inner with trust_me
        m.P = 10
        m.step()

        # Inner choicepoint removed, outer restored
        assert m.B == outer_B, "B should point to outer choicepoint"
        assert len(m.cp_stack) == 8, "Only outer choicepoint remains"

    def test_trust_me_unwinds_trail_correctly(self):
        """trust_me must unwind trail entries created after choicepoint."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0: Create choice
            (OP_PUT_VARIABLE, 0, 0),  # 1: Create var after choice
            (OP_HALT,),  # 2: (pad)
            (OP_TRUST_ME,),  # 3: Final alternative
            (OP_HALT,),  # 4: (pad)
        ]

        m.step()  # try_me_else - saves H=0, TR=0
        saved_H = m.cp_stack[4]
        saved_TR = m.cp_stack[5]

        m.step()  # put_variable - heap[0]
        ref_addr = 0

        # Bind and trail
        const_addr = new_con(m, 99)
        bind(m, ref_addr, const_addr)
        assert len(m.trail) > saved_TR

        # Backtrack with trust_me
        m.P = 3
        m.step()

        # Trail unwound, heap truncated
        assert len(m.trail) == saved_TR
        assert len(m.heap) == saved_H
        assert m.B is None


class TestBacktrackingScenarios:
    """Integration tests for backtracking through multiple clauses."""

    def test_two_clause_predicate(self):
        """Simulate a simple two-clause predicate with backtracking."""
        m = Machine()
        # Simulated predicate with 2 clauses
        # Code layout:
        # 0: try_me_else 3        # Clause 1 prologue
        # 1: proceed              # Clause 1 body (simplified)
        # 2: (pad)
        # 3: trust_me             # Clause 2 prologue
        # 4: proceed              # Clause 2 body (simplified)
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_PROCEED,),  # 1
            (OP_HALT,),  # 2
            (OP_TRUST_ME,),  # 3
            (OP_PROCEED,),  # 4
        ]
        m.CP = 100  # Simulated return address

        # First solution
        m.step()  # try_me_else - creates choicepoint
        assert m.B == 0
        assert m.cp_stack[7] == 3, "alt_ptr points to trust_me"
        m.step()  # proceed - returns to CP=100
        assert m.P == 100
        assert m.halted is False

        # Backtrack manually by jumping to alternative
        alt_ptr = m.cp_stack[7]
        m.P = alt_ptr  # Jump to trust_me

        # Second solution
        m.step()  # trust_me - restores state, pops choicepoint
        assert m.B is None, "Choicepoint removed"
        m.step()  # proceed
        assert m.P == 100

    def test_three_clause_predicate(self):
        """Simulate three-clause predicate with try/retry/trust pattern."""
        m = Machine()
        # Code layout:
        # 0: try_me_else 3        # Clause 1
        # 1: proceed
        # 2: (pad)
        # 3: retry_me_else 6      # Clause 2
        # 4: proceed
        # 5: (pad)
        # 6: trust_me             # Clause 3
        # 7: proceed
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_PROCEED,),  # 1
            (OP_HALT,),  # 2
            (OP_RETRY_ME_ELSE, 6),  # 3
            (OP_PROCEED,),  # 4
            (OP_HALT,),  # 5
            (OP_TRUST_ME,),  # 6
            (OP_PROCEED,),  # 7
        ]
        m.CP = 100

        # Solution 1
        m.step()  # try_me_else
        choicepoint_B = m.B
        assert choicepoint_B == 0
        m.step()  # proceed
        assert m.P == 100

        # Backtrack to solution 2
        m.P = m.cp_stack[choicepoint_B + 7]  # Jump to alt_ptr (3)
        m.step()  # retry_me_else
        assert m.B == choicepoint_B, "Choicepoint still exists"
        assert m.cp_stack[choicepoint_B + 7] == 6, "alt_ptr updated to 6"
        m.step()  # proceed
        assert m.P == 100

        # Backtrack to solution 3
        m.P = m.cp_stack[choicepoint_B + 7]  # Jump to alt_ptr (6)
        m.step()  # trust_me
        assert m.B is None, "Choicepoint removed"
        m.step()  # proceed
        assert m.P == 100

    def test_backtracking_with_heap_recovery(self):
        """Backtracking must restore heap to saved_H."""
        m = Machine()
        # Predicate that builds structure in clause 1
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_PUT_STRUCTURE, "f/1", 0),  # 1: Build f/1 structure on heap
            (OP_HALT,),  # 2
            (OP_TRUST_ME,),  # 3
            (OP_PROCEED,),  # 4
        ]
        m.CP = 100

        # Clause 1
        m.step()  # try_me_else - saves H=0
        saved_H = m.cp_stack[4]
        assert saved_H == 0
        m.step()  # put_structure f/1, A0 - creates STR and functor on heap
        assert m.H > saved_H, "Heap grew"

        # Backtrack
        m.P = 3
        m.step()  # trust_me - restores H
        assert m.H == saved_H, "Heap restored to saved_H"
        assert len(m.heap) == saved_H, "Heap truncated"

    def test_nested_choice_backtracking(self):
        """Backtracking through nested choicepoints."""
        m = Machine()
        # Outer predicate calls inner predicate with its own choices
        # We simulate: outer :- inner. with multiple clauses for both
        m.code = [
            (OP_TRY_ME_ELSE, 10),  # 0: Outer clause 1
            (OP_ALLOCATE, 0),  # 1: Need frame for call
            (OP_CALL, 20),  # 2: Call inner predicate at 20
            (OP_DEALLOCATE,),  # 3
            (OP_PROCEED,),  # 4
            (OP_HALT,),  # 5-9: (pad)
            (OP_HALT,),
            (OP_HALT,),
            (OP_HALT,),
            (OP_HALT,),
            (OP_TRUST_ME,),  # 10: Outer clause 2
            (OP_ALLOCATE, 0),  # 11
            (OP_CALL, 20),  # 12: Call inner again
            (OP_DEALLOCATE,),  # 13
            (OP_PROCEED,),  # 14
            (OP_HALT,),  # 15-19: (pad)
            (OP_HALT,),
            (OP_HALT,),
            (OP_HALT,),
            (OP_HALT,),
            (OP_TRY_ME_ELSE, 25),  # 20: Inner clause 1
            (OP_PROCEED,),  # 21
            (OP_HALT,),  # 22-24: (pad)
            (OP_HALT,),
            (OP_HALT,),
            (OP_TRUST_ME,),  # 25: Inner clause 2
            (OP_PROCEED,),  # 26
        ]
        m.CP = 100

        # Execute outer clause 1, which calls inner clause 1
        m.step()  # try_me_else - outer choicepoint at 0
        outer_B = m.B
        assert outer_B == 0

        m.step()  # allocate
        m.step()  # call 20 - CP=3
        assert m.P == 20

        m.step()  # try_me_else - inner choicepoint at 8
        inner_B = m.B
        assert inner_B == 8
        assert m.cp_stack[inner_B] == outer_B, "Inner prev_B points to outer"

        m.step()  # proceed from inner clause 1 - returns to CP=3
        assert m.P == 3
        m.step()  # deallocate
        m.step()  # proceed - returns to CP=100
        assert m.P == 100

        # Now backtrack inner: jump to inner alt
        inner_alt = m.cp_stack[inner_B + 7]
        assert inner_alt == 25
        m.P = inner_alt
        m.step()  # trust_me - pops inner choicepoint
        assert m.B == outer_B, "Back to outer choicepoint"

        # Now backtrack outer: jump to outer alt
        outer_alt = m.cp_stack[outer_B + 7]
        assert outer_alt == 10
        m.P = outer_alt
        m.step()  # trust_me - pops outer choicepoint
        assert m.B is None, "All choicepoints removed"


class TestEdgeCases:
    """Edge cases and error conditions."""

    def test_retry_me_else_without_choicepoint_fails(self):
        """retry_me_else with B=None should fail gracefully."""
        m = Machine()
        m.code = [(OP_RETRY_ME_ELSE, 5), (OP_HALT,)]
        m.B = None

        # Should either halt or raise error
        # Implementation choice: halt machine
        m.step()
        assert m.halted, "Machine should halt on retry_me_else without choicepoint"

    def test_trust_me_without_choicepoint_fails(self):
        """trust_me with B=None should fail gracefully."""
        m = Machine()
        m.code = [(OP_TRUST_ME,), (OP_HALT,)]
        m.B = None

        m.step()
        assert m.halted, "Machine should halt on trust_me without choicepoint"

    def test_choicepoint_with_empty_trail(self):
        """Choicepoint operations work when trail is empty."""
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_PROCEED,),  # 1
            (OP_HALT,),  # 2
            (OP_TRUST_ME,),  # 3
            (OP_PROCEED,),  # 4
        ]
        m.CP = 100

        m.step()  # try_me_else
        assert m.TR == 0
        assert len(m.trail) == 0

        m.P = 3
        m.step()  # trust_me
        # Should work without errors
        assert m.B is None

    def test_backtrack_preserves_X_registers(self):
        """X registers (arguments) are NOT automatically restored by retry/trust.

        In WAM, argument registers (A0-An, aliased as X0-Xn during calls) are
        NOT explicitly saved in the choicepoint. Instead, the predicate itself is
        responsible for reconstructing arguments when retrying. This test verifies
        that X registers are NOT automatically restored by retry/trust.
        """
        m = Machine()
        m.code = [
            (OP_TRY_ME_ELSE, 3),  # 0
            (OP_PUT_CONSTANT, 1, 0),  # 1: X0 = address of constant 1 on heap
            (OP_HALT,),  # 2
            (OP_RETRY_ME_ELSE, 10),  # 3
            (OP_PUT_CONSTANT, 2, 0),  # 4: X0 = address of constant 2 on heap
            (OP_HALT,),  # 5
        ]

        m.step()  # try_me_else
        m.step()  # put_constant 1, X0 - creates constant on heap, stores addr in X[0]
        x0_val = m.X[0]
        assert isinstance(x0_val, int), "X0 should contain heap address"
        assert m.heap[x0_val] == (TAG_CON, 1)

        # Modify X0 directly
        dummy_addr = new_con(m, 99)
        m.X[0] = dummy_addr

        # Backtrack
        m.P = 3
        m.step()  # retry_me_else - heap restored to H=0

        # X0 should NOT be automatically restored - it still points to dummy
        # But heap was truncated, so dummy_addr is now invalid
        # Clause must rebuild X0
        assert m.X[0] == dummy_addr, "X registers not auto-restored by retry"
        assert len(m.heap) == 0, "Heap was truncated, so X0 points to nothing"

        m.step()  # put_constant 2, X0 - creates new constant at heap[0]
        assert m.X[0] == 0, "X0 now points to heap[0]"
        assert m.heap[0] == (TAG_CON, 2)
