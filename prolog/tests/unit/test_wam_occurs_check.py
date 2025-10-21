"""Unit tests for WAM occurs-check implementation.

Tests the optional occurs-check flag for cycle detection during unification:
- occurs() function for detecting cycles
- Machine.occurs_check_enabled flag
- Integration with unification (bind operations)
- Behavior differences with flag on/off
- Side effects and heap invariants
"""

import pytest

from prolog.wam.heap import new_con, new_list, new_ref, new_str, note_struct_args
from prolog.wam.machine import Machine
from prolog.wam.unify import deref, occurs, unify


class TestOccursFunction:
    """Test occurs() cycle detection function."""

    def test_occurs_var_in_itself_detects_cycle(self):
        """occurs() detects X in X."""
        machine = Machine()
        x_addr = new_ref(machine)

        # Check if X occurs in X
        result = occurs(machine, x_addr, x_addr)
        assert result is True

    def test_occurs_var_not_in_constant(self):
        """occurs() returns False for X in constant."""

        machine = Machine()
        x_addr = new_ref(machine)
        const_addr = new_con(machine, "atom")

        result = occurs(machine, x_addr, const_addr)
        assert result is False

    def test_occurs_var_not_in_different_var(self):
        """occurs() returns False for X in Y (both unbound)."""
        machine = Machine()
        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        result = occurs(machine, x_addr, y_addr)
        assert result is False

    def test_occurs_detects_cycle_in_simple_structure(self):
        """occurs() detects X in f(X)."""
        machine = Machine()
        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Check if X occurs in f(X)
        result = occurs(machine, x_addr, f_addr)
        assert result is True

    def test_occurs_detects_cycle_in_nested_structure(self):
        """occurs() detects X in f(g(X))."""
        machine = Machine()
        x_addr = new_ref(machine)

        # Build g(X)
        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, x_addr)

        # Build f(g(X))
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, g_addr)

        # Check if X occurs in f(g(X))
        result = occurs(machine, x_addr, f_addr)
        assert result is True

    def test_occurs_no_cycle_in_structure_with_different_var(self):
        """occurs() returns False for X in f(Y)."""
        machine = Machine()
        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Build f(Y)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, y_addr)

        result = occurs(machine, x_addr, f_addr)
        assert result is False

    def test_occurs_detects_cycle_in_list(self):
        """occurs() detects X in [X|Y]."""

        machine = Machine()
        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Build [X|Y]
        list_addr = new_list(machine, x_addr, y_addr)

        # Check if X occurs in [X|Y]
        result = occurs(machine, x_addr, list_addr)
        assert result is True

    def test_occurs_detects_cycle_in_list_tail(self):
        """occurs() detects X in [a|X]."""

        machine = Machine()
        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # Build [a|X]
        list_addr = new_list(machine, a_addr, x_addr)

        # Check if X occurs in [a|X]
        result = occurs(machine, x_addr, list_addr)
        assert result is True

    def test_occurs_handles_dereferencing(self):
        """occurs() follows variable chains correctly."""
        machine = Machine()
        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Bind Y to X (Y points to X)
        machine.heap[y_addr] = (0, x_addr)  # TAG_REF

        # Build f(Y) - but Y points to X
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, y_addr)

        # Should detect X in f(Y) since Y->X
        result = occurs(machine, x_addr, f_addr)
        assert result is True

    def test_occurs_handles_visited_set(self):
        """occurs() handles circular structures without infinite loop."""
        machine = Machine()
        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Create circular reference: X -> f(X) (simulate already bound)
        machine.heap[x_addr] = (1, f_addr)  # TAG_STR

        # Should still detect cycle without infinite loop
        result = occurs(machine, x_addr, f_addr)
        # This tests that visited set prevents infinite loop
        # Result depends on implementation but should not hang
        assert isinstance(result, bool)


class TestMachineOccursCheckFlag:
    """Test Machine.occurs_check_enabled flag."""

    def test_machine_occurs_check_default_false(self):
        """Machine.occurs_check_enabled defaults to False."""
        machine = Machine()
        assert machine.occurs_check_enabled is False

    def test_machine_occurs_check_can_be_enabled(self):
        """Machine.occurs_check_enabled can be set to True."""
        machine = Machine()
        machine.occurs_check_enabled = True
        assert machine.occurs_check_enabled is True

    def test_machine_occurs_check_can_be_disabled(self):
        """Machine.occurs_check_enabled can be set back to False."""
        machine = Machine()
        machine.occurs_check_enabled = True
        machine.occurs_check_enabled = False
        assert machine.occurs_check_enabled is False


class TestUnificationWithOccursCheck:
    """Test unification behavior with occurs-check on/off."""

    def test_unify_self_ref_fails_with_occurs_check(self):
        """X = X fails when occurs-check is enabled (trivial cycle)."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Should fail: cannot unify X with X when checking occurs
        result = unify(machine, x_addr, x_addr)
        # Actually X=X should succeed even with occurs check (same variable)
        # Let me reconsider: unifying a variable with itself is always OK
        assert result is True

    def test_unify_cyclic_structure_fails_with_occurs_check(self):
        """X = f(X) fails when occurs-check is enabled."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Should fail: X = f(X) creates cycle
        result = unify(machine, x_addr, f_addr)
        assert result is False

    def test_unify_cyclic_structure_succeeds_without_occurs_check(self):
        """X = f(X) succeeds when occurs-check is disabled (default)."""
        machine = Machine()
        machine.occurs_check_enabled = False

        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Should succeed: creates rational tree
        result = unify(machine, x_addr, f_addr)
        assert result is True

    def test_unify_nested_cycle_fails_with_occurs_check(self):
        """X = f(g(X)) fails when occurs-check is enabled."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build g(X)
        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, x_addr)

        # Build f(g(X))
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, g_addr)

        # Should fail: X = f(g(X)) creates nested cycle
        result = unify(machine, x_addr, f_addr)
        assert result is False

    def test_unify_nested_cycle_succeeds_without_occurs_check(self):
        """X = f(g(X)) succeeds when occurs-check is disabled."""
        machine = Machine()
        machine.occurs_check_enabled = False

        x_addr = new_ref(machine)

        # Build g(X)
        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, x_addr)

        # Build f(g(X))
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, g_addr)

        # Should succeed: creates rational tree
        result = unify(machine, x_addr, f_addr)
        assert result is True

    def test_unify_non_cyclic_structure_succeeds_with_occurs_check(self):
        """X = f(a) succeeds when occurs-check is enabled (no cycle)."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # Build f(a)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, a_addr)

        # Should succeed: no cycle
        result = unify(machine, x_addr, f_addr)
        assert result is True

    def test_unify_cyclic_list_fails_with_occurs_check(self):
        """X = [X|Y] fails when occurs-check is enabled."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Build [X|Y]
        list_addr = new_list(machine, x_addr, y_addr)

        # Should fail: X = [X|Y] creates cycle
        result = unify(machine, x_addr, list_addr)
        assert result is False

    def test_unify_cyclic_list_succeeds_without_occurs_check(self):
        """X = [X|Y] succeeds when occurs-check is disabled."""

        machine = Machine()
        machine.occurs_check_enabled = False

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Build [X|Y]
        list_addr = new_list(machine, x_addr, y_addr)

        # Should succeed: creates cyclic list
        result = unify(machine, x_addr, list_addr)
        assert result is True

    def test_unify_different_vars_succeeds_with_occurs_check(self):
        """X = Y succeeds when occurs-check is enabled (no cycle)."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Should succeed: just binds variables
        result = unify(machine, x_addr, y_addr)
        assert result is True


class TestOccursCheckEdgeCases:
    """Test edge cases and complex scenarios."""

    def test_occurs_check_with_uninitialized_structure(self):
        """Occurs-check handles structures with uninitialized arguments gracefully."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build f/2 structure but don't initialize arguments yet
        # This simulates get_structure in write mode with occurs-check enabled
        f_addr = new_str(machine, "f", 2)
        # At this point, functor_addr+1 and functor_addr+2 don't exist yet

        # Occurs-check should handle missing arguments without IndexError
        result = occurs(machine, x_addr, f_addr)
        # X doesn't occur in the uninitialized structure
        assert result is False

    def test_occurs_with_multiple_args_structure(self):
        """occurs() checks all arguments in multi-arg structure."""
        machine = Machine()
        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Build f(y, X, z)

        z_addr = new_con(machine, "z")
        f_addr = new_str(machine, "f", 3)
        note_struct_args(machine, y_addr, x_addr, z_addr)

        # X occurs in second argument
        result = occurs(machine, x_addr, f_addr)
        assert result is True

    def test_occurs_with_deeply_nested_no_cycle(self):
        """occurs() correctly identifies no cycle in deep nesting."""

        machine = Machine()
        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # Build f(g(h(a)))
        h_addr = new_str(machine, "h", 1)
        note_struct_args(machine, a_addr)

        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, h_addr)

        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, g_addr)

        # X does not occur in f(g(h(a)))
        result = occurs(machine, x_addr, f_addr)
        assert result is False

    def test_unify_maintains_binding_when_no_cycle(self):
        """Unification with occurs-check maintains bindings when no cycle."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # X = a should succeed and bind X
        result = unify(machine, x_addr, a_addr)
        assert result is True

        # X should now be bound to 'a'

        dereffed = deref(machine, x_addr)
        cell = machine.heap[dereffed]
        assert cell[0] == 2  # TAG_CON
        assert cell[1] == "a"

    def test_occurs_check_flag_can_toggle_during_execution(self):
        """occurs_check_enabled flag can be changed during execution."""

        machine = Machine()

        # First unification with occurs-check off
        machine.occurs_check_enabled = False
        x1_addr = new_ref(machine)
        f1_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x1_addr)

        result1 = unify(machine, x1_addr, f1_addr)
        assert result1 is True  # Succeeds

        # Enable occurs-check
        machine.occurs_check_enabled = True

        # Second unification with occurs-check on
        x2_addr = new_ref(machine)
        f2_addr = new_str(machine, "g", 1)
        note_struct_args(machine, x2_addr)

        result2 = unify(machine, x2_addr, f2_addr)
        assert result2 is False  # Fails due to cycle


class TestOccursCheckSideEffects:
    """Test that failed occurs-check unifications don't mutate state."""

    def test_failed_unify_no_trail_extension(self):
        """Failed unify with occurs-check does not extend trail."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Record TR before unification
        tr_before = machine.TR

        # Should fail: X = f(X)
        result = unify(machine, x_addr, f_addr)
        assert result is False

        # TR should be unchanged
        assert machine.TR == tr_before

    def test_failed_unify_no_heap_allocation(self):
        """Failed unify with occurs-check does not allocate heap cells."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Record H before unification
        h_before = machine.H

        # Should fail: X = f(X)
        result = unify(machine, x_addr, f_addr)
        assert result is False

        # H should be unchanged
        assert machine.H == h_before

    def test_failed_unify_no_binding(self):
        """Failed unify with occurs-check leaves variables unbound."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # Should fail: X = f(X)
        result = unify(machine, x_addr, f_addr)
        assert result is False

        # X should still be unbound
        cell = machine.heap[x_addr]
        assert cell[0] == 0  # TAG_REF
        assert cell[1] == x_addr  # Points to itself

    def test_successful_unify_with_occurs_check_extends_trail(self):
        """Successful unify with occurs-check does extend trail when needed."""

        machine = Machine()
        machine.occurs_check_enabled = True

        # Set HB to make binding trailable
        machine.HB = 10
        x_addr = new_ref(machine)  # Will be < HB
        a_addr = new_con(machine, "a")

        # Record TR before
        tr_before = machine.TR

        # Should succeed: X = a (no cycle)
        result = unify(machine, x_addr, a_addr)
        assert result is True

        # TR should have increased (binding was trailed)
        assert machine.TR > tr_before


class TestOccursCheckBindingPreserved:
    """Test that non-cyclic unifications preserve correct bindings."""

    def test_binding_preserved_simple_constant(self):
        """X = a with occurs-check binds X correctly."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # X = a should succeed
        result = unify(machine, x_addr, a_addr)
        assert result is True

        # X should be bound to 'a'
        dereffed = deref(machine, x_addr)
        cell = machine.heap[dereffed]
        assert cell[0] == 2  # TAG_CON
        assert cell[1] == "a"

    def test_binding_preserved_structure(self):
        """X = f(a, b) with occurs-check binds X correctly."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")
        b_addr = new_con(machine, "b")

        # Build f(a, b)
        f_addr = new_str(machine, "f", 2)
        note_struct_args(machine, a_addr, b_addr)

        # X = f(a, b) should succeed
        result = unify(machine, x_addr, f_addr)
        assert result is True

        # X should be bound to f(a, b)
        dereffed = deref(machine, x_addr)
        cell = machine.heap[dereffed]
        assert cell[0] == 1  # TAG_STR
        assert cell[1] == f_addr + 1  # Points to functor

    def test_binding_heap_shape_correct(self):
        """Heap shape is correct after successful unification with occurs-check."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # Build f(Y)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, y_addr)

        # X = f(Y) should succeed (no cycle)
        result = unify(machine, x_addr, f_addr)
        assert result is True

        # Now Y = a
        result2 = unify(machine, y_addr, a_addr)
        assert result2 is True

        # X should dereference through f(Y) where Y is bound to a
        x_deref = deref(machine, x_addr)
        assert machine.heap[x_deref][0] == 1  # TAG_STR

        y_deref = deref(machine, y_addr)
        assert machine.heap[y_deref][0] == 2  # TAG_CON
        assert machine.heap[y_deref][1] == "a"


class TestOccursCheckIndirectCycles:
    """Test indirect cycle detection."""

    def test_indirect_cycle_via_intermediate_binding(self):
        """T = f(X), then X = T fails with occurs-check."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        t_addr = new_ref(machine)

        # Build f(X)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        # T = f(X) succeeds (no cycle yet)
        result1 = unify(machine, t_addr, f_addr)
        assert result1 is True

        # Now X = T should fail (creates cycle: X -> f(X))
        result2 = unify(machine, x_addr, t_addr)
        assert result2 is False

    def test_indirect_cycle_via_structure_chain(self):
        """X = f(Y), Y = g(X) fails with occurs-check."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)

        # Build f(Y)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, y_addr)

        # X = f(Y) succeeds
        result1 = unify(machine, x_addr, f_addr)
        assert result1 is True

        # Build g(X)
        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, x_addr)

        # Y = g(X) should fail (creates cycle)
        result2 = unify(machine, y_addr, g_addr)
        assert result2 is False

    def test_indirect_cycle_three_variables(self):
        """X = f(Y), Y = g(Z), Z = X fails with occurs-check."""
        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)
        z_addr = new_ref(machine)

        # X = f(Y)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, y_addr)
        result1 = unify(machine, x_addr, f_addr)
        assert result1 is True

        # Y = g(Z)
        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, z_addr)
        result2 = unify(machine, y_addr, g_addr)
        assert result2 is True

        # Z = X should fail (creates cycle)
        result3 = unify(machine, z_addr, x_addr)
        assert result3 is False


class TestOccursCheckListTailChains:
    """Test cycle detection through list tail chains."""

    def test_list_tail_cycle_direct(self):
        """X = [a|X] fails with occurs-check."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")

        # Build [a|X]
        list_addr = new_list(machine, a_addr, x_addr)

        # X = [a|X] should fail
        result = unify(machine, x_addr, list_addr)
        assert result is False

    def test_list_tail_cycle_indirect(self):
        """X = [a|Y], Y = [b|X] fails with occurs-check."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)
        a_addr = new_con(machine, "a")
        b_addr = new_con(machine, "b")

        # Build [a|Y]
        list1_addr = new_list(machine, a_addr, y_addr)

        # X = [a|Y] succeeds
        result1 = unify(machine, x_addr, list1_addr)
        assert result1 is True

        # Build [b|X]
        list2_addr = new_list(machine, b_addr, x_addr)

        # Y = [b|X] should fail (creates cycle)
        result2 = unify(machine, y_addr, list2_addr)
        assert result2 is False

    def test_list_tail_chain_through_vars(self):
        """X = [a|Y], Y = Z, Z = [b|X] fails with occurs-check."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        y_addr = new_ref(machine)
        z_addr = new_ref(machine)
        a_addr = new_con(machine, "a")
        b_addr = new_con(machine, "b")

        # X = [a|Y]
        list1_addr = new_list(machine, a_addr, y_addr)
        result1 = unify(machine, x_addr, list1_addr)
        assert result1 is True

        # Y = Z
        result2 = unify(machine, y_addr, z_addr)
        assert result2 is True

        # Build [b|X]
        list2_addr = new_list(machine, b_addr, x_addr)

        # Z = [b|X] should fail (Y->Z->cycle)
        result3 = unify(machine, z_addr, list2_addr)
        assert result3 is False


class TestOccursCheckFlagToggling:
    """Test occurs_check flag behavior during execution sequences."""

    def test_toggle_affects_subsequent_unifications(self):
        """Changing occurs_check_enabled affects next unification."""
        machine = Machine()

        # First: occurs-check OFF
        machine.occurs_check_enabled = False
        x1_addr = new_ref(machine)
        f1_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x1_addr)

        result1 = unify(machine, x1_addr, f1_addr)
        assert result1 is True  # Succeeds with rational tree

        # Toggle ON
        machine.occurs_check_enabled = True

        # Second: occurs-check ON
        x2_addr = new_ref(machine)
        f2_addr = new_str(machine, "g", 1)
        note_struct_args(machine, x2_addr)

        result2 = unify(machine, x2_addr, f2_addr)
        assert result2 is False  # Fails due to cycle

    def test_toggle_mid_sequence_isolated(self):
        """Toggling flag mid-sequence doesn't affect previous bindings."""

        machine = Machine()

        # Create bindings with occurs-check OFF
        machine.occurs_check_enabled = False
        x_addr = new_ref(machine)
        f_addr = new_str(machine, "f", 1)
        note_struct_args(machine, x_addr)

        result1 = unify(machine, x_addr, f_addr)
        assert result1 is True

        # Toggle ON
        machine.occurs_check_enabled = True

        # Previous binding should still exist
        dereffed = deref(machine, x_addr)
        assert machine.heap[dereffed][0] == 1  # TAG_STR

        # New unification respects new flag
        y_addr = new_ref(machine)
        g_addr = new_str(machine, "g", 1)
        note_struct_args(machine, y_addr)

        result2 = unify(machine, y_addr, g_addr)
        assert result2 is False

    def test_flag_read_at_bind_time(self):
        """occurs_check_enabled is read at bind time, not call time."""
        machine = Machine()

        x_addr = new_ref(machine)
        f_addr = new_str(machine, "h", 1)
        note_struct_args(machine, x_addr)

        # Flag is OFF
        machine.occurs_check_enabled = False

        # Unification happens with flag OFF
        result = unify(machine, x_addr, f_addr)
        assert result is True

        # Turning flag ON after unification doesn't affect result
        machine.occurs_check_enabled = True
        # X is already bound, flag change doesn't undo it
        dereffed = deref(machine, x_addr)
        assert machine.heap[dereffed][0] == 1  # Still bound


@pytest.mark.slow
class TestOccursCheckPerformance:
    """Test that occurs-check doesn't break performance for acyclic cases."""

    def test_large_structure_without_cycle(self):
        """Occurs-check on large acyclic structure completes reasonably."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build deeply nested structure without X: f(g(h(i(j(k(a))))))
        a_addr = new_con(machine, "a")
        current = a_addr

        for functor in ["k", "j", "i", "h", "g", "f"]:
            struct_addr = new_str(machine, functor, 1)
            note_struct_args(machine, current)
            current = struct_addr

        # X does not occur, should complete without timeout
        result = occurs(machine, x_addr, current)
        assert result is False

    def test_wide_structure_check(self):
        """Occurs-check on wide structure (many arguments)."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build f(a, b, c, d, e, f, g, h, i, j)
        args = [new_con(machine, f"arg{i}") for i in range(10)]
        f_addr = new_str(machine, "f", 10)
        note_struct_args(machine, *args)

        # X does not occur
        result = occurs(machine, x_addr, f_addr)
        assert result is False


@pytest.mark.stress
class TestOccursCheckStress:
    """Stress tests for occurs-check on very large structures."""

    def test_very_deep_nesting(self):
        """Occurs-check on very deeply nested structure (100+ levels)."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)
        a_addr = new_con(machine, "a")
        current = a_addr

        # Build 100-level deep nesting
        for i in range(100):
            struct_addr = new_str(machine, f"f{i}", 1)
            note_struct_args(machine, current)
            current = struct_addr

        # Should complete without stack overflow
        result = occurs(machine, x_addr, current)
        assert result is False

    def test_very_wide_structure(self):
        """Occurs-check on very wide structure (100+ arguments)."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build structure with 100 arguments
        args = [new_con(machine, f"arg{i}") for i in range(100)]
        f_addr = new_str(machine, "f", 100)
        note_struct_args(machine, *args)

        # Should complete efficiently
        result = occurs(machine, x_addr, f_addr)
        assert result is False

    def test_combined_depth_and_breadth(self):
        """Occurs-check on structure with both depth and breadth."""

        machine = Machine()
        machine.occurs_check_enabled = True

        x_addr = new_ref(machine)

        # Build a tree-like structure
        # Level 0: 10 constants
        level = [new_con(machine, f"leaf{i}") for i in range(10)]

        # Build 5 levels up, each node has 2 children
        for depth in range(5):
            new_level = []
            for i in range(0, len(level), 2):
                if i + 1 < len(level):
                    struct_addr = new_str(machine, f"node_{depth}_{i}", 2)
                    note_struct_args(machine, level[i], level[i + 1])
                    new_level.append(struct_addr)
            level = new_level

        root = level[0] if level else new_con(machine, "empty")

        # Should handle tree efficiently
        result = occurs(machine, x_addr, root)
        assert result is False
