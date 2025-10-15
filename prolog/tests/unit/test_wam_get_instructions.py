"""Unit tests for WAM get instruction family."""

from prolog.wam.heap import (
    TAG_CON,
    TAG_REF,
    TAG_STR,
    new_con,
    new_list,
    new_ref,
    new_str,
)
from prolog.wam.instructions import (
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_GET_VALUE,
    OP_GET_VARIABLE,
)
from prolog.wam.machine import Machine


class TestGetVariable:
    """Test get_variable instruction."""

    def test_get_variable_copies_aj_to_xi(self):
        """get_variable Xi, Aj copies Aj to Xi."""
        m = Machine()
        m.X = [None, 42, None]
        m.code = [(OP_GET_VARIABLE, 0, 1)]  # get_variable X0, A1

        m.step()

        # X0 should now have value from A1 (X[1])
        assert m.X[0] == 42
        assert m.X[1] == 42  # A1 unchanged
        assert m.P == 1

    def test_get_variable_copies_heap_address(self):
        """get_variable copies heap addresses (pointers)."""
        m = Machine()
        addr = new_con(m, "foo")
        m.X = [None, addr]
        m.code = [(OP_GET_VARIABLE, 2, 1)]  # get_variable X2, A1

        m.step()

        # Both X2 and A1 point to same heap address
        assert m.X[2] == addr
        assert m.X[1] == addr

    def test_get_variable_extends_x_registers(self):
        """get_variable extends X register bank as needed."""
        m = Machine()
        m.X = [10, 20]
        m.code = [(OP_GET_VARIABLE, 5, 0)]  # get_variable X5, A0

        assert len(m.X) == 2

        m.step()

        # X should extend to accommodate X5
        assert len(m.X) >= 6
        assert m.X[5] == 10  # Copied from A0

    def test_get_variable_does_not_modify_heap(self):
        """get_variable only copies pointers, doesn't touch heap."""
        m = Machine()
        m.X = [None, 5]
        m.code = [(OP_GET_VARIABLE, 0, 1)]

        old_h = m.H
        old_heap = m.heap[:]

        m.step()

        # Heap unchanged
        assert m.H == old_h
        assert m.heap == old_heap

    def test_get_variable_sequential_copies(self):
        """Sequential get_variable instructions work correctly."""
        m = Machine()
        m.X = [1, 2, 3, None, None]
        m.code = [
            (OP_GET_VARIABLE, 3, 0),  # X3 = A0
            (OP_GET_VARIABLE, 4, 1),  # X4 = A1
        ]

        m.step()
        m.step()

        assert m.X[3] == 1
        assert m.X[4] == 2


class TestGetValue:
    """Test get_value instruction."""

    def test_get_value_unifies_two_variables(self):
        """get_value Xi, Aj unifies contents of Xi and Aj."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)
        m.X = [var1, var2]
        m.code = [(OP_GET_VALUE, 0, 1)]  # get_value X0, A1

        m.step()

        # Both should now point to same root after unification
        assert m.heap[var1][1] == var2 or m.heap[var2][1] == var1
        assert m.P == 1
        assert not m.halted

    def test_get_value_unifies_variable_with_constant(self):
        """get_value unifies variable with constant."""
        m = Machine()
        var = new_ref(m)
        const = new_con(m, 42)
        m.X = [var, const]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        # Variable should be bound to constant
        assert m.heap[var] == (TAG_REF, const)
        assert not m.halted

    def test_get_value_succeeds_on_matching_constants(self):
        """get_value succeeds when both are same constant."""
        m = Machine()
        const1 = new_con(m, "foo")
        const2 = new_con(m, "foo")
        m.X = [const1, const2]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        assert m.P == 1
        assert not m.halted

    def test_get_value_fails_on_different_constants(self):
        """get_value fails when constants differ."""
        m = Machine()
        const1 = new_con(m, "foo")
        const2 = new_con(m, "bar")
        m.X = [const1, const2]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        # Machine should halt on unification failure
        assert m.halted

    def test_get_value_unifies_structures(self):
        """get_value unifies matching structures."""
        m = Machine()

        # f(a)
        str1 = new_str(m, "f", 1)
        new_con(m, "a")

        # f(a)
        str2 = new_str(m, "f", 1)
        new_con(m, "a")

        m.X = [str1, str2]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        assert not m.halted
        assert m.P == 1

    def test_get_value_fails_on_structure_mismatch(self):
        """get_value fails when structures don't match."""
        m = Machine()

        # f(a)
        str1 = new_str(m, "f", 1)
        new_con(m, "a")

        # g(b)
        str2 = new_str(m, "g", 1)
        new_con(m, "b")

        m.X = [str1, str2]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        assert m.halted

    def test_get_value_with_ref_chains(self):
        """get_value derefs both operands before unification."""
        m = Machine()

        # Create chains: var1 -> var2 -> var3, var4 -> var5 -> var3
        var3 = new_ref(m)
        var2 = new_ref(m)
        var1 = new_ref(m)
        var5 = new_ref(m)
        var4 = new_ref(m)

        m.heap[var1] = (TAG_REF, var2)
        m.heap[var2] = (TAG_REF, var3)
        m.heap[var4] = (TAG_REF, var5)
        m.heap[var5] = (TAG_REF, var3)

        m.X = [var1, var4]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        # Both should deref to var3 and unify successfully
        assert not m.halted

    def test_get_value_variable_with_structure(self):
        """get_value unifies REF with STR (binds variable to structure)."""
        m = Machine()

        var = new_ref(m)
        # f(a)
        str_addr = new_str(m, "f", 1)
        new_con(m, "a")

        m.X = [var, str_addr]
        m.code = [(OP_GET_VALUE, 0, 1)]

        initial_h = m.H

        m.step()

        # Variable should be bound to structure, no new allocation
        assert m.heap[var] == (TAG_REF, str_addr)
        assert m.H == initial_h  # No new allocations
        assert not m.halted

    def test_get_value_matching_structures_no_allocation(self):
        """get_value on matching structures doesn't allocate."""
        m = Machine()

        # f(a)
        str1 = new_str(m, "f", 1)
        new_con(m, "a")

        # f(a)
        str2 = new_str(m, "f", 1)
        new_con(m, "a")

        m.X = [str1, str2]
        m.code = [(OP_GET_VALUE, 0, 1)]

        initial_h = m.H

        m.step()

        # Unification succeeds, no new allocation
        assert m.H == initial_h
        assert not m.halted


class TestGetConstant:
    """Test get_constant instruction."""

    def test_get_constant_unifies_variable_with_constant(self):
        """get_constant C, Aj unifies Aj with new constant."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_CONSTANT, 42, 0)]  # get_constant 42, A0

        initial_h = m.H

        m.step()

        # Should allocate constant and bind variable to it
        assert m.H == initial_h + 1
        new_const_addr = initial_h
        assert m.heap[new_const_addr] == (TAG_CON, 42)
        assert m.heap[var] == (TAG_REF, new_const_addr)
        assert not m.halted

    def test_get_constant_succeeds_on_matching_constant(self):
        """get_constant succeeds when Aj already has matching constant."""
        m = Machine()
        const = new_con(m, "foo")
        m.X = [const]
        m.code = [(OP_GET_CONSTANT, "foo", 0)]

        initial_h = m.H

        m.step()

        # No new allocation needed, unification succeeds
        assert m.H == initial_h  # No allocation on match
        assert not m.halted
        assert m.P == 1

    def test_get_constant_fails_on_different_constant(self):
        """get_constant fails when Aj has different constant without allocating."""
        m = Machine()
        const = new_con(m, "foo")
        m.X = [const]
        m.code = [(OP_GET_CONSTANT, "bar", 0)]

        initial_h = m.H

        m.step()

        # Unification should fail
        assert m.halted
        # Should not allocate on known mismatch
        assert m.H == initial_h

    def test_get_constant_with_integer(self):
        """get_constant works with integer constants."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_CONSTANT, 99, 0)]

        m.step()

        # Variable bound to new integer constant
        const_addr = m.H - 1
        assert m.heap[const_addr] == (TAG_CON, 99)
        assert not m.halted

    def test_get_constant_with_atom(self):
        """get_constant works with atom constants."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_CONSTANT, "atom", 0)]

        m.step()

        const_addr = m.H - 1
        assert m.heap[const_addr] == (TAG_CON, "atom")
        assert not m.halted

    def test_get_constant_fails_on_type_mismatch(self):
        """get_constant fails on type mismatch (STR/LIST) without allocating."""
        m = Machine()
        str_addr = new_str(m, "f", 1)
        new_ref(m)  # arg
        m.X = [str_addr]
        m.code = [(OP_GET_CONSTANT, 42, 0)]

        initial_h = m.H

        m.step()

        # Should fail on type mismatch
        assert m.halted
        # Should not allocate
        assert m.H == initial_h


class TestGetStructure:
    """Test get_structure instruction."""

    def test_get_structure_with_unbound_variable_builds(self):
        """get_structure with unbound variable builds new structure."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_STRUCTURE, ("f", 2), 0)]  # get_structure f/2, A0

        initial_h = m.H

        m.step()

        # Should allocate structure and bind variable
        assert m.H == initial_h + 2  # STR + functor
        str_addr = initial_h  # STR allocated at current H
        functor_addr = str_addr + 1

        assert m.heap[str_addr] == (TAG_STR, functor_addr)
        assert m.heap[functor_addr] == (TAG_CON, ("f", 2))

        # Variable should be bound to structure
        assert m.heap[var] == (TAG_REF, str_addr)

        # Should be in write mode
        assert m.unify_mode == "write"
        # S should point to first arg slot
        assert m.S == functor_addr + 1

        assert not m.halted
        assert m.P == 1

    def test_get_structure_with_matching_structure_reads(self):
        """get_structure with matching structure enters read mode."""
        m = Machine()

        # Build f(X, Y)
        str_addr = new_str(m, "f", 2)
        arg1 = new_ref(m)
        new_ref(m)  # arg2

        m.X = [str_addr]
        m.code = [(OP_GET_STRUCTURE, ("f", 2), 0)]

        m.step()

        # Should enter read mode
        assert m.unify_mode == "read"
        # S should point to first argument
        functor_addr = str_addr + 1
        assert m.S == functor_addr + 1
        assert m.S == arg1  # Points to first arg

        assert not m.halted
        assert m.P == 1

    def test_get_structure_with_mismatched_functor_fails(self):
        """get_structure fails when functor doesn't match."""
        m = Machine()

        # Build g/1
        str_addr = new_str(m, "g", 1)
        new_ref(m)

        m.X = [str_addr]
        m.code = [(OP_GET_STRUCTURE, ("f", 2), 0)]  # Looking for f/2

        m.step()

        # Should fail
        assert m.halted

    def test_get_structure_with_mismatched_arity_fails(self):
        """get_structure fails when arity doesn't match."""
        m = Machine()

        # Build f/1
        str_addr = new_str(m, "f", 1)
        new_ref(m)

        m.X = [str_addr]
        m.code = [(OP_GET_STRUCTURE, ("f", 2), 0)]  # Looking for f/2

        m.step()

        assert m.halted

    def test_get_structure_with_constant_fails(self):
        """get_structure with constant (non-structure) fails."""
        m = Machine()
        const = new_con(m, "atom")
        m.X = [const]
        m.code = [(OP_GET_STRUCTURE, ("f", 1), 0)]

        m.step()

        assert m.halted

    def test_get_structure_with_list_fails(self):
        """get_structure with list fails."""
        m = Machine()
        # Build simple list
        nil = new_con(m, "[]")
        list_addr = new_list(m, new_con(m, 1), nil)

        m.X = [list_addr]
        m.code = [(OP_GET_STRUCTURE, ("f", 1), 0)]

        m.step()

        assert m.halted

    def test_get_structure_zero_arity(self):
        """get_structure handles zero-arity functors."""
        m = Machine()

        # Build atom/0 as structure
        str_addr = new_str(m, "atom", 0)

        m.X = [str_addr]
        m.code = [(OP_GET_STRUCTURE, ("atom", 0), 0)]

        m.step()

        assert m.unify_mode == "read"
        # For zero-arity, S points to slot after functor (no args)
        functor_addr = str_addr + 1
        assert m.S == functor_addr + 1
        assert not m.halted

    def test_get_structure_derefs_argument(self):
        """get_structure derefs Aj before checking."""
        m = Machine()

        # Create chain: var1 -> var2 (unbound)
        var2 = new_ref(m)
        var1 = new_ref(m)
        m.heap[var1] = (TAG_REF, var2)

        m.X = [var1]
        m.code = [(OP_GET_STRUCTURE, ("f", 1), 0)]

        m.step()

        # Should build structure and bind to var2 (the root)
        assert m.unify_mode == "write"
        # var2 should be bound to new structure
        assert m.heap[var2][0] == TAG_REF
        assert m.heap[m.heap[var2][1]][0] == TAG_STR


class TestGetIntegration:
    """Integration tests for get instruction sequences."""

    def test_get_variable_then_get_value(self):
        """Sequence: get_variable then get_value."""
        m = Machine()
        const = new_con(m, 42)
        m.X = [const, None]
        m.code = [
            (OP_GET_VARIABLE, 1, 0),  # X1 = A0
            (OP_GET_VALUE, 1, 0),  # Unify X1 with A0 (should succeed)
        ]

        m.step()
        assert m.X[1] == const

        m.step()
        assert not m.halted  # Unification succeeds (same address)

    def test_mixed_get_sequence(self):
        """Mixed sequence of get instructions."""
        m = Machine()

        var1 = new_ref(m)
        var2 = new_ref(m)

        m.X = [var1, var2, None]
        m.code = [
            (OP_GET_VARIABLE, 2, 0),  # X2 = A0
            (OP_GET_CONSTANT, "a", 1),  # Unify A1 with 'a'
            (OP_GET_VALUE, 2, 0),  # Unify X2 with A0 (should succeed)
        ]

        m.run()

        assert not m.halted
        assert m.X[2] == var1  # Copied from A0

    def test_get_structure_build_then_read(self):
        """Build structure with unbound, then read with bound."""
        m = Machine()

        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_STRUCTURE, ("f", 2), 0)]

        m.step()

        # Structure built, now try reading same structure
        str_addr = m.heap[var][1]  # Address variable is bound to
        # Extend X to have space for X[1]
        while len(m.X) <= 1:
            m.X.append(None)
        m.X[1] = str_addr
        m.code.append((OP_GET_STRUCTURE, ("f", 2), 1))
        m.P = 1

        m.step()

        assert m.unify_mode == "read"
        assert not m.halted


class TestGetEdgeCases:
    """Edge case tests for get instructions."""

    def test_get_variable_from_none(self):
        """get_variable copies None value."""
        m = Machine()
        m.X = [None, None]
        m.code = [(OP_GET_VARIABLE, 1, 0)]

        m.step()

        assert m.X[1] is None

    def test_get_value_with_none_halts(self):
        """get_value with None in registers halts (invalid heap address)."""
        m = Machine()
        m.X = [None, None]
        m.code = [(OP_GET_VALUE, 0, 1)]

        m.step()

        # Registers should hold valid heap addresses only
        # None is invalid, so this should halt
        assert m.halted

    def test_get_constant_with_float(self):
        """get_constant works with float constants."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_CONSTANT, 3.14, 0)]

        m.step()

        const_addr = m.H - 1
        assert m.heap[const_addr] == (TAG_CON, 3.14)
        assert not m.halted

    def test_get_structure_with_special_functor_name(self):
        """get_structure with special characters in functor."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_STRUCTURE, ("@>", 2), 0)]

        m.step()

        functor_addr = m.H - 1
        assert m.heap[functor_addr] == (TAG_CON, ("@>", 2))
        assert m.unify_mode == "write"


class TestGetMachineState:
    """Tests verifying machine state after get instructions."""

    def test_get_instructions_increment_p(self):
        """All get instructions increment P on success."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)
        var3 = new_ref(m)
        var4 = new_ref(m)

        m.X = [var1, var2, var3, var4]
        m.code = [
            (OP_GET_VARIABLE, 0, 1),
            (OP_GET_VALUE, 0, 1),
            (OP_GET_CONSTANT, "a", 2),
            (OP_GET_STRUCTURE, ("f", 1), 3),
        ]

        for i in range(4):
            assert m.P == i
            m.step()
            if m.halted:
                break

        assert m.P == 4

    def test_get_value_failure_halts_machine(self):
        """Failed get_value halts machine and doesn't increment P."""
        m = Machine()
        const1 = new_con(m, "foo")
        const2 = new_con(m, "bar")
        m.X = [const1, const2]
        m.code = [
            (OP_GET_VALUE, 0, 1),  # Should fail
            (OP_GET_VARIABLE, 0, 1),  # Should not execute
        ]

        m.step()

        assert m.halted
        assert m.P == 0  # P not incremented on failure

    def test_get_structure_failure_halts_machine(self):
        """Failed get_structure halts machine and doesn't increment P."""
        m = Machine()

        # f(a)
        str_addr = new_str(m, "f", 1)
        new_con(m, "a")

        m.X = [str_addr]
        m.code = [
            (OP_GET_STRUCTURE, ("g", 1), 0),  # Wrong functor, should fail
            (OP_GET_VARIABLE, 0, 1),  # Should not execute
        ]

        m.step()

        assert m.halted
        assert m.P == 0  # P not incremented on failure

    def test_get_does_not_modify_unrelated_registers(self):
        """Get instructions don't affect unrelated registers."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_CONSTANT, 42, 0)]

        # Set some initial state
        m.CP = 10
        m.E = 5
        m.B = 3
        m.TR = 0
        m.HB = 0

        m.step()

        # Only P, H, and possibly unify_mode should change
        assert m.CP == 10
        assert m.E == 5
        assert m.B == 3


class TestGetStructureModeSetting:
    """Tests for get_structure mode and S register setting."""

    def test_get_structure_write_mode_sets_s_correctly(self):
        """get_structure in write mode sets S to first arg slot."""
        m = Machine()
        var = new_ref(m)
        m.X = [var]
        m.code = [(OP_GET_STRUCTURE, ("f", 3), 0)]

        m.step()

        # STR at H-2, functor at H-1
        # S should point to first arg after functor
        functor_addr = m.H - 1
        assert m.S == functor_addr + 1

    def test_get_structure_read_mode_sets_s_correctly(self):
        """get_structure in read mode sets S to first arg."""
        m = Machine()

        # Build f(a, b, c)
        str_addr = new_str(m, "f", 3)
        arg1 = new_con(m, "a")
        new_con(m, "b")  # arg2
        new_con(m, "c")  # arg3

        m.X = [str_addr]
        m.code = [(OP_GET_STRUCTURE, ("f", 3), 0)]

        m.step()

        # S should point to first argument
        assert m.S == arg1

    def test_get_structure_mode_transitions(self):
        """Test mode transitions between write and read."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)

        m.X = [var1, var2]
        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),  # Write mode
            (OP_GET_STRUCTURE, ("g", 1), 1),  # Write mode again
        ]

        m.step()
        assert m.unify_mode == "write"

        m.step()
        assert m.unify_mode == "write"  # Both in write mode
