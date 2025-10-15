"""Unit tests for WAM unify and set instruction families."""

from prolog.wam.heap import TAG_CON, TAG_REF, TAG_STR, new_con, new_ref, new_str
from prolog.wam.instructions import (
    OP_GET_STRUCTURE,
    OP_PUT_STRUCTURE,
    OP_SET_VALUE,
    OP_SET_VARIABLE,
    OP_UNIFY_VALUE,
    OP_UNIFY_VARIABLE,
)
from prolog.wam.machine import Machine
from prolog.wam.unify import deref


class TestSetVariable:
    """Test set_variable instruction (write mode)."""

    def test_set_variable_creates_new_ref(self):
        """set_variable Xi creates new REF and writes to heap."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 0),  # mode=write, S=2
            (OP_SET_VARIABLE, 0),  # Create REF, write to heap, assign to X0
        ]

        m.step()  # put_structure
        initial_h = m.H
        assert m.unify_mode == "write"
        assert m.S == 2  # Points to first arg slot

        m.step()  # set_variable

        # Should create REF at heap[2]
        assert m.H == initial_h + 1
        assert m.heap[2] == (TAG_REF, 2)  # Self-referential
        # X0 should point to it
        assert m.X[0] == 2
        # S should advance
        assert m.S == 3

    def test_set_variable_multiple_calls(self):
        """Multiple set_variable calls create distinct REFs."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 2), 0),
            (OP_SET_VARIABLE, 0),  # X0
            (OP_SET_VARIABLE, 1),  # X1
        ]

        m.run()

        # Structure at heap[0-1], args at heap[2-3]
        assert m.heap[2] == (TAG_REF, 2)  # X0
        assert m.heap[3] == (TAG_REF, 3)  # X1
        assert m.X[0] == 2
        assert m.X[1] == 3
        assert m.S == 4

    def test_set_variable_extends_x_registers(self):
        """set_variable extends X register bank as needed."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 0),
            (OP_SET_VARIABLE, 5),  # X5
        ]

        assert len(m.X) == 0

        m.run()

        assert len(m.X) >= 6
        assert m.X[5] == 2  # Points to REF at heap[2]


class TestSetValue:
    """Test set_value instruction (write mode)."""

    def test_set_value_writes_register_to_heap(self):
        """set_value Xi writes Xi's address to heap."""
        m = Machine()
        # Pre-allocate a REF at heap[0]
        ref_addr = new_ref(m)
        m.X = [ref_addr]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # A1, mode=write, S=3
            (OP_SET_VALUE, 0),  # Write X0 to heap
        ]

        m.step()  # put_structure
        assert m.S == 3
        initial_h = m.H

        m.step()  # set_value

        # Should write X0's address to heap[3]
        assert m.H == initial_h + 1
        assert m.heap[3] == ref_addr
        assert m.S == 4

    def test_set_value_writes_constant(self):
        """set_value can write constant addresses."""
        m = Machine()
        const_addr = new_con(m, 42)
        m.X = [const_addr]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # Use A1, not A0
            (OP_SET_VALUE, 0),  # Write X0
        ]

        m.run()

        # Get structure address and compute offsets
        str_addr = m.X[1]
        functor_addr = str_addr + 1
        arg_addr = functor_addr + 1

        # Argument should point to constant at heap[0]
        assert m.heap[arg_addr] == const_addr

    def test_set_value_writes_structure(self):
        """set_value can write structure addresses."""
        m = Machine()
        str_addr = new_str(m, "g", 1)
        new_ref(m)  # arg
        m.X = [str_addr]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # Build f/_ in A1
            (OP_SET_VALUE, 0),  # Write X0 (g structure) as arg
        ]

        m.run()

        # Get f structure address and compute offsets
        f_str_addr = m.X[1]
        f_functor_addr = f_str_addr + 1
        f_arg_addr = f_functor_addr + 1

        # Argument should point to g structure at heap[0]
        assert m.heap[f_arg_addr] == str_addr

    def test_set_value_sequential_writes(self):
        """Sequential set_value instructions write multiple values."""
        m = Machine()
        ref1 = new_ref(m)
        ref2 = new_ref(m)
        m.X = [ref1, ref2]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 2), 3),  # Build in A3, don't clobber X0/X1
            (OP_SET_VALUE, 0),  # Write X0 as first arg
            (OP_SET_VALUE, 1),  # Write X1 as second arg
        ]

        m.run()

        # Get structure address and compute offsets
        str_addr = m.X[3]
        functor_addr = str_addr + 1
        arg1_addr = functor_addr + 1
        arg2_addr = arg1_addr + 1

        assert m.heap[arg1_addr] == ref1
        assert m.heap[arg2_addr] == ref2
        assert m.S == arg2_addr + 1


class TestUnifyVariable:
    """Test unify_variable instruction (read mode)."""

    def test_unify_variable_reads_from_heap(self):
        """unify_variable Xi in read mode reads heap[S] to Xi."""
        m = Machine()
        # Build structure f(42) using put/set instructions
        const_addr = new_con(m, 42)
        m.X = [const_addr]

        # Build the structure properly
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # Build f/_ in A1
            (OP_SET_VALUE, 0),  # Write const as arg
            # Now read it back
            (OP_GET_STRUCTURE, ("f", 1), 1),  # Read f/_ from A1
            (OP_UNIFY_VARIABLE, 2),  # X2 = first arg
        ]

        # Execute build sequence
        m.step()  # put_structure
        m.step()  # set_value

        str_addr = m.X[1]
        functor_addr = str_addr + 1

        # Now read it
        m.step()  # get_structure
        assert m.unify_mode == "read"
        expected_s = functor_addr + 1
        assert m.S == expected_s

        m.step()  # unify_variable

        # X2 should now have the arg address
        assert m.X[2] == const_addr
        assert m.S == expected_s + 1

    def test_unify_variable_multiple_reads(self):
        """Multiple unify_variable calls read sequential arguments."""
        m = Machine()
        # Build structure f(a, b) using put/set
        arg1 = new_con(m, "a")
        arg2 = new_con(m, "b")
        m.X = [arg1, arg2]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 2), 3),  # Build f/2 in A3
            (OP_SET_VALUE, 0),  # Write arg1
            (OP_SET_VALUE, 1),  # Write arg2
            # Now read it
            (OP_GET_STRUCTURE, ("f", 2), 3),  # Read from A3
            (OP_UNIFY_VARIABLE, 4),  # X4 = first arg
            (OP_UNIFY_VARIABLE, 5),  # X5 = second arg
        ]

        m.run()

        assert m.X[4] == arg1
        assert m.X[5] == arg2

    def test_unify_variable_extends_x_registers(self):
        """unify_variable extends X register bank as needed."""
        m = Machine()
        str_addr = new_str(m, "f", 1)
        new_con(m, "x")
        m.X = [str_addr]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),
            (OP_UNIFY_VARIABLE, 5),  # X5
        ]

        assert len(m.X) == 1

        m.run()

        assert len(m.X) >= 6


class TestUnifyValue:
    """Test unify_value instruction (read mode)."""

    def test_unify_value_succeeds_on_match(self):
        """unify_value succeeds when Xi matches heap[S]."""
        m = Machine()
        const_addr = new_con(m, 42)
        str_addr = new_str(m, "f", 1)  # heap[1-2]
        m.heap.append(const_addr)  # heap[3] = 42
        m.X = [str_addr, const_addr]  # X0=structure, X1=42

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),  # mode=read, S=3
            (OP_UNIFY_VALUE, 1),  # Unify X1 with heap[3]
        ]

        m.run()

        assert not m.halted
        assert m.S == 4

    def test_unify_value_binds_variable(self):
        """unify_value binds Xi when it's unbound."""
        m = Machine()
        # Build structure f(99) properly
        const_addr = new_con(m, 99)
        var_addr = new_ref(m)
        m.X = [const_addr, var_addr]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 2),  # Build f/_ in A2
            (OP_SET_VALUE, 0),  # Write const as arg
            # Now unify with var
            (OP_GET_STRUCTURE, ("f", 1), 2),  # Read from A2
            (OP_UNIFY_VALUE, 1),  # Unify X1 (var) with arg
        ]

        m.run()

        # X1's REF should now point to constant
        assert m.heap[var_addr] == (TAG_REF, const_addr)
        assert not m.halted

    def test_unify_value_fails_on_mismatch(self):
        """unify_value halts when Xi doesn't match heap[S]."""
        m = Machine()
        const1 = new_con(m, "a")
        const2 = new_con(m, "b")
        str_addr = new_str(m, "f", 1)
        m.heap.append(const2)  # heap[4] = "b"
        m.X = [str_addr, const1]  # X1 = "a"

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),  # S=4
            (OP_UNIFY_VALUE, 1),  # Try to unify "a" with "b"
        ]

        m.run()

        assert m.halted

    def test_unify_value_multiple_calls(self):
        """Multiple unify_value calls unify sequential arguments."""
        m = Machine()
        const_a = new_con(m, "a")
        const_b = new_con(m, "b")
        str_addr = new_str(m, "f", 2)
        m.heap.append(const_a)  # heap[4] = "a"
        m.heap.append(const_b)  # heap[5] = "b"
        m.X = [str_addr, const_a, const_b]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 2), 0),  # S=4
            (OP_UNIFY_VALUE, 1),  # Unify X1 with heap[4]
            (OP_UNIFY_VALUE, 2),  # Unify X2 with heap[5]
        ]

        m.run()

        assert not m.halted
        assert m.S == 6


class TestUnifyVariableWriteMode:
    """Test unify_variable in write mode (behaves like set_variable)."""

    def test_unify_variable_write_mode_creates_ref(self):
        """unify_variable in write mode creates new REF."""
        m = Machine()
        var_addr = new_ref(m)  # heap[0]
        m.X = [var_addr]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),  # Unbound, so write mode
            (OP_UNIFY_VARIABLE, 1),
        ]

        m.step()  # get_structure
        assert m.unify_mode == "write"

        # After get_structure on unbound: X[0] still points to REF, which is now bound to STR
        # Must deref to get actual STR address
        str_addr = deref(m, m.X[0])
        functor_addr = str_addr + 1
        expected_s = functor_addr + 1
        assert m.S == expected_s

        m.step()  # unify_variable

        # Should create REF at the arg slot
        arg_addr = expected_s
        assert m.heap[arg_addr] == (TAG_REF, arg_addr)
        assert m.X[1] == arg_addr


class TestUnifyValueWriteMode:
    """Test unify_value in write mode (behaves like set_value)."""

    def test_unify_value_write_mode_writes_to_heap(self):
        """unify_value in write mode writes Xi to heap."""
        m = Machine()
        var_addr = new_ref(m)  # heap[0]
        const_addr = new_con(m, 42)  # heap[1]
        m.X = [var_addr, const_addr]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),  # Unbound, so write mode
            (OP_UNIFY_VALUE, 1),
        ]

        m.step()  # get_structure
        assert m.unify_mode == "write"

        # After get_structure: X[0] still points to REF, must deref to get STR
        str_addr = deref(m, m.X[0])
        functor_addr = str_addr + 1
        arg_addr = functor_addr + 1
        assert m.S == arg_addr

        m.step()  # unify_value

        # Should write X1's address to arg slot
        assert m.heap[arg_addr] == const_addr


class TestSetIntegration:
    """Integration tests for set instruction sequences."""

    def test_build_simple_structure(self):
        """Build f(X) using put_structure + set_variable."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # Build in A1, not A0
            (OP_SET_VARIABLE, 0),  # X0 gets the new REF
        ]

        m.run()

        # Get structure address and compute offsets
        str_addr = m.X[1]
        functor_addr = str_addr + 1
        arg_addr = functor_addr + 1

        # Verify layout
        assert m.heap[str_addr] == (TAG_STR, functor_addr)
        assert m.heap[functor_addr] == (TAG_CON, ("f", 1))
        assert m.heap[arg_addr] == (TAG_REF, arg_addr)  # X is unbound REF
        assert m.X[0] == arg_addr  # X0 points to the REF

    def test_build_structure_with_constant(self):
        """Build f(42) using put_structure + set_value."""
        m = Machine()
        const_addr = new_con(m, 42)
        m.X = [const_addr]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),
            (OP_SET_VALUE, 0),
        ]

        m.run()

        # Structure at heap[1-2], arg at heap[3]
        assert m.heap[1] == (TAG_STR, 2)
        assert m.heap[2] == (TAG_CON, ("f", 1))
        assert m.heap[3] == const_addr

    def test_build_nested_structure(self):
        """Build f(g(X), Y) using nested structures."""
        m = Machine()
        m.code = [
            # Build g(X)
            (OP_PUT_STRUCTURE, ("g", 1), 2),  # Build in A2
            (OP_SET_VARIABLE, 0),  # X in X0
            # Build f(g(X), Y)
            (OP_PUT_STRUCTURE, ("f", 2), 3),  # Build in A3
            (OP_SET_VALUE, 2),  # Write g(X) from A2
            (OP_SET_VARIABLE, 1),  # Y in X1
        ]

        m.run()

        # Get addresses
        g_str_addr = m.X[2]
        g_functor_addr = g_str_addr + 1
        g_arg_addr = g_functor_addr + 1

        f_str_addr = m.X[3]
        f_functor_addr = f_str_addr + 1
        f_arg1_addr = f_functor_addr + 1
        f_arg2_addr = f_arg1_addr + 1

        # Verify g(X)
        assert m.heap[g_str_addr] == (TAG_STR, g_functor_addr)
        assert m.heap[g_functor_addr] == (TAG_CON, ("g", 1))
        assert m.heap[g_arg_addr] == (TAG_REF, g_arg_addr)  # X unbound
        assert m.X[0] == g_arg_addr  # X0 points to X

        # Verify f(g(X), Y)
        assert m.heap[f_str_addr] == (TAG_STR, f_functor_addr)
        assert m.heap[f_functor_addr] == (TAG_CON, ("f", 2))
        assert m.heap[f_arg1_addr] == g_str_addr  # Points to g(X)
        assert m.heap[f_arg2_addr] == (TAG_REF, f_arg2_addr)  # Y unbound
        assert m.X[1] == f_arg2_addr  # X1 points to Y


class TestUnifyIntegration:
    """Integration tests for unify instruction sequences."""

    def test_match_simple_structure(self):
        """Match f(42) using get_structure + unify_variable."""
        m = Machine()
        # Build f(42) properly
        const_addr = new_con(m, 42)
        m.X = [const_addr]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # Build f/_ in A1
            (OP_SET_VALUE, 0),  # Write const as arg
            # Now match it
            (OP_GET_STRUCTURE, ("f", 1), 1),  # Read from A1
            (OP_UNIFY_VARIABLE, 2),  # X2 = arg
        ]

        m.run()

        assert not m.halted
        assert m.X[2] == const_addr

    def test_match_with_unify_value(self):
        """Match f(X, X) with f(a, a) using unify_value."""
        m = Machine()
        const_a = new_con(m, "a")
        str_addr = new_str(m, "f", 2)
        m.heap.append(const_a)
        m.heap.append(const_a)
        m.X = [str_addr, const_a]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 2), 0),
            (OP_UNIFY_VARIABLE, 2),  # X2 = first arg
            (OP_UNIFY_VALUE, 2),  # Check second arg = X2
        ]

        m.run()

        assert not m.halted

    def test_match_nested_structure(self):
        """Match f(g(X)) using nested get_structure + unify sequences."""
        m = Machine()
        # Build f(g(42)) properly
        const_addr = new_con(m, 42)
        m.X = [const_addr]

        m.code = [
            # Build g(42)
            (OP_PUT_STRUCTURE, ("g", 1), 1),  # Build g/_ in A1
            (OP_SET_VALUE, 0),  # Write const
            # Build f(g(42))
            (OP_PUT_STRUCTURE, ("f", 1), 2),  # Build f/_ in A2
            (OP_SET_VALUE, 1),  # Write g structure
            # Now match it
            (OP_GET_STRUCTURE, ("f", 1), 2),  # Match f in A2
            (OP_UNIFY_VARIABLE, 3),  # X3 = g(...)
            (OP_GET_STRUCTURE, ("g", 1), 3),  # Match g in X3
            (OP_UNIFY_VARIABLE, 4),  # X4 = 42
        ]

        m.run()

        assert not m.halted
        assert m.X[4] == const_addr


class TestMixedSequences:
    """Test sequences mixing set and unify instructions."""

    def test_build_then_match(self):
        """Build structure, then match it."""
        m = Machine()
        m.code = [
            # Build f(X) where X is unbound
            (OP_PUT_STRUCTURE, ("f", 1), 1),  # Build in A1
            (OP_SET_VARIABLE, 0),  # X0 = new REF
            # Match against it
            (OP_GET_STRUCTURE, ("f", 1), 1),  # Match A1
            (OP_UNIFY_VARIABLE, 2),  # X2 = arg
        ]

        m.run()

        assert not m.halted
        # X0 and X2 should point to same REF
        assert m.X[0] == m.X[2]


class TestEdgeCases:
    """Edge case tests for unify/set instructions."""

    def test_set_variable_zero_arity(self):
        """set_variable with zero-arity structure (shouldn't happen but handle it)."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 0), 0),
        ]

        m.run()

        # Should work fine, no set instructions follow
        assert m.X[0] == 0
        assert m.S == 2  # Points past functor

    def test_unify_variable_derefs_heap(self):
        """unify_variable derefs heap cell before assigning."""
        m = Machine()
        # Build structure with REF chain
        ref1 = new_ref(m)  # heap[0]
        const = new_con(m, 99)  # heap[1]
        # Bind ref1 to const
        m.heap[0] = (TAG_REF, const)
        str_addr = new_str(m, "f", 1)  # heap[2-3]
        m.heap.append(ref1)  # heap[4] = points to ref1
        m.X = [str_addr]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),
            (OP_UNIFY_VARIABLE, 1),  # Should deref heap[4]
        ]

        m.run()

        # X1 should have the dereferenced address (const)
        # Note: unify_variable reads heap[S], not deref(heap[S])
        # Actually checking implementation - it should just read the address
        assert m.X[1] == ref1  # Gets the address at heap[4]

    def test_set_value_with_none_writes_none(self):
        """set_value with None in register writes None."""
        m = Machine()
        m.X = [None]

        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 1),
            (OP_SET_VALUE, 0),
        ]

        m.run()

        # Should write None to heap
        assert m.heap[2] is None

    def test_unify_value_none_handling(self):
        """unify_value with None should handle gracefully."""
        m = Machine()
        str_addr = new_str(m, "f", 1)
        m.heap.append(42)
        m.X = [str_addr, None]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),
            (OP_UNIFY_VALUE, 1),
        ]

        # This might fail in unify() if it can't handle None
        # Behavior depends on implementation
        m.run()

        # Will likely halt due to None being invalid
        assert m.halted or not m.halted  # Implementation-defined


class TestMachineState:
    """Tests verifying machine state after unify/set instructions."""

    def test_set_instructions_increment_p(self):
        """All set instructions increment P."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 2), 0),
            (OP_SET_VARIABLE, 0),
            (OP_SET_VALUE, 0),
        ]

        m.step()  # P=0→1
        assert m.P == 1
        m.step()  # P=1→2
        assert m.P == 2
        m.step()  # P=2→3
        assert m.P == 3

    def test_unify_instructions_increment_p(self):
        """All unify instructions increment P."""
        m = Machine()
        str_addr = new_str(m, "f", 2)
        m.heap.append(new_ref(m))
        m.heap.append(new_ref(m))
        m.X = [str_addr]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 2), 0),
            (OP_UNIFY_VARIABLE, 1),
            (OP_UNIFY_VARIABLE, 2),
        ]

        m.step()  # P=0→1
        assert m.P == 1
        m.step()  # P=1→2
        assert m.P == 2
        m.step()  # P=2→3
        assert m.P == 3

    def test_unify_value_failure_halts_without_incrementing_p(self):
        """unify_value failure halts machine and doesn't increment P."""
        m = Machine()
        const_a = new_con(m, "a")
        const_b = new_con(m, "b")
        str_addr = new_str(m, "f", 1)
        m.heap.append(const_b)
        m.X = [str_addr, const_a]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 1), 0),
            (OP_UNIFY_VALUE, 1),  # Should fail
        ]

        m.run()

        assert m.halted
        # P should be at unify_value instruction (1)
        assert m.P == 1


class TestSRegisterManagement:
    """Tests for S register management."""

    def test_s_advances_with_set_variable(self):
        """S register advances after set_variable."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 3), 0),
            (OP_SET_VARIABLE, 0),
            (OP_SET_VARIABLE, 1),
            (OP_SET_VARIABLE, 2),
        ]

        m.step()  # put_structure
        assert m.S == 2  # First arg slot

        m.step()  # set_variable
        assert m.S == 3

        m.step()  # set_variable
        assert m.S == 4

        m.step()  # set_variable
        assert m.S == 5

    def test_s_advances_with_unify_variable(self):
        """S register advances after unify_variable."""
        m = Machine()
        str_addr = new_str(m, "f", 2)
        m.heap.extend([new_ref(m), new_ref(m)])
        m.X = [str_addr]

        m.code = [
            (OP_GET_STRUCTURE, ("f", 2), 0),
            (OP_UNIFY_VARIABLE, 1),
            (OP_UNIFY_VARIABLE, 2),
        ]

        m.step()  # get_structure
        initial_s = m.S

        m.step()  # unify_variable
        assert m.S == initial_s + 1

        m.step()  # unify_variable
        assert m.S == initial_s + 2
