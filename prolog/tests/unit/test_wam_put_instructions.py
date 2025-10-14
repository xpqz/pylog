"""Unit tests for WAM put instruction family."""

from prolog.wam.heap import TAG_CON, TAG_REF, TAG_STR
from prolog.wam.instructions import (
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_PUT_VALUE,
    OP_PUT_VARIABLE,
)
from prolog.wam.machine import Machine


class TestPutVariable:
    """Test put_variable instruction."""

    def test_put_variable_creates_new_ref(self):
        """put_variable Xi, Aj creates new unbound REF."""
        m = Machine()
        m.code = [(OP_PUT_VARIABLE, 0, 1)]  # put_variable X0, A1

        m.step()

        # Should create new REF and store in both X0 and A1 (X[1])
        assert len(m.X) >= 2
        assert m.X[0] == m.X[1]  # Same address in both registers
        addr = m.X[0]
        assert isinstance(addr, int)
        assert m.heap[addr] == (TAG_REF, addr)  # Self-referential (unbound)
        assert m.P == 1

    def test_put_variable_extends_x_registers(self):
        """put_variable extends X register bank as needed."""
        m = Machine()
        m.code = [(OP_PUT_VARIABLE, 5, 3)]  # put_variable X5, A3

        assert len(m.X) == 0

        m.step()

        # X should extend to accommodate X5
        assert len(m.X) >= 6
        assert m.X[5] == m.X[3]

    def test_put_variable_multiple_calls(self):
        """Multiple put_variable calls create distinct REFs."""
        m = Machine()
        m.code = [
            (OP_PUT_VARIABLE, 0, 0),  # put_variable X0, A0
            (OP_PUT_VARIABLE, 1, 1),  # put_variable X1, A1
        ]

        m.step()
        m.step()

        # X0 and X1 should have different addresses
        assert m.X[0] != m.X[1]
        # But each should be unbound
        assert m.heap[m.X[0]] == (TAG_REF, m.X[0])
        assert m.heap[m.X[1]] == (TAG_REF, m.X[1])

    def test_put_variable_with_existing_x_register(self):
        """put_variable overwrites existing X register value."""
        m = Machine()
        m.X = [42, 99]  # Pre-populate X registers
        m.code = [(OP_PUT_VARIABLE, 0, 1)]

        old_h = m.H

        m.step()

        # X0 should be replaced with new REF address
        assert m.X[0] != 42
        assert m.X[0] >= old_h  # New heap address


class TestPutValue:
    """Test put_value instruction."""

    def test_put_value_copies_register(self):
        """put_value Xi, Aj copies Xi value to Aj."""
        m = Machine()
        m.X = [100, None, None]
        m.code = [(OP_PUT_VALUE, 0, 2)]  # put_value X0, A2

        m.step()

        # A2 (X[2]) should now have same value as X0
        assert m.X[2] == 100
        assert m.X[0] == 100  # X0 unchanged
        assert m.P == 1

    def test_put_value_copies_heap_address(self):
        """put_value copies heap addresses correctly."""
        m = Machine()
        m.heap.append((TAG_CON, 42))
        m.H = 1
        addr = 0

        m.X = [addr, None]
        m.code = [(OP_PUT_VALUE, 0, 1)]

        m.step()

        # Both registers point to same heap address
        assert m.X[1] == addr
        assert m.X[0] == addr

    def test_put_value_does_not_modify_heap(self):
        """put_value only copies pointers, doesn't touch heap."""
        m = Machine()
        m.X = [5, None]
        m.code = [(OP_PUT_VALUE, 0, 1)]

        old_h = m.H
        old_heap = m.heap[:]

        m.step()

        # Heap unchanged
        assert m.H == old_h
        assert m.heap == old_heap

    def test_put_value_sequential_copies(self):
        """Sequential put_value instructions work correctly."""
        m = Machine()
        m.X = [10, 20, None, None]
        m.code = [
            (OP_PUT_VALUE, 0, 2),  # A2 = X0
            (OP_PUT_VALUE, 1, 3),  # A3 = X1
        ]

        m.step()
        m.step()

        assert m.X[2] == 10
        assert m.X[3] == 20

    def test_put_value_extends_x_for_aj(self):
        """put_value extends X register bank when Aj >= len(X)."""
        m = Machine()
        m.X = [42]  # Only X0 initialized
        m.code = [(OP_PUT_VALUE, 0, 5)]  # A5 = X0

        assert len(m.X) == 1

        m.step()

        # X should extend to accommodate A5 (X[5])
        assert len(m.X) >= 6
        assert m.X[5] == 42


class TestPutConstant:
    """Test put_constant instruction."""

    def test_put_constant_integer(self):
        """put_constant C, Aj allocates integer constant."""
        m = Machine()
        m.code = [(OP_PUT_CONSTANT, 42, 0)]  # put_constant 42, A0

        m.step()

        # Should allocate CON cell on heap
        assert m.H == 1
        assert len(m.heap) == 1
        assert m.heap[0] == (TAG_CON, 42)

        # A0 should point to it
        assert m.X[0] == 0
        assert m.P == 1

    def test_put_constant_atom(self):
        """put_constant C, Aj allocates atom constant."""
        m = Machine()
        m.code = [(OP_PUT_CONSTANT, "foo", 1)]  # put_constant foo, A1

        m.step()

        assert m.heap[0] == (TAG_CON, "foo")
        assert m.X[1] == 0

    def test_put_constant_multiple_constants(self):
        """Multiple put_constant calls allocate separate cells."""
        m = Machine()
        m.code = [
            (OP_PUT_CONSTANT, 1, 0),
            (OP_PUT_CONSTANT, 2, 1),
            (OP_PUT_CONSTANT, 3, 2),
        ]

        m.step()
        m.step()
        m.step()

        # Three separate CON cells
        assert m.H == 3
        assert m.heap[0] == (TAG_CON, 1)
        assert m.heap[1] == (TAG_CON, 2)
        assert m.heap[2] == (TAG_CON, 3)

        # Registers point to respective cells
        assert m.X[0] == 0
        assert m.X[1] == 1
        assert m.X[2] == 2

    def test_put_constant_extends_x_registers(self):
        """put_constant extends X register bank if needed."""
        m = Machine()
        m.code = [(OP_PUT_CONSTANT, 99, 5)]

        assert len(m.X) == 0

        m.step()

        assert len(m.X) >= 6
        assert m.X[5] == 0  # Points to heap[0]


class TestPutStructure:
    """Test put_structure instruction."""

    def test_put_structure_allocates_functor_and_str(self):
        """put_structure F/N, Aj allocates STR then functor cell."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("f", 2), 0)]  # put_structure f/2, A0

        m.step()

        # Should allocate STR at heap[0], functor at heap[1]
        assert m.H == 2
        assert m.heap[0] == (TAG_STR, 1)  # STR pointing to functor
        assert m.heap[1] == (TAG_CON, ("f", 2))  # Functor

        # A0 points to STR
        assert m.X[0] == 0
        assert m.P == 1

    def test_put_structure_sets_write_mode(self):
        """put_structure sets unify_mode to 'write'."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("g", 1), 0)]

        assert m.unify_mode is None

        m.step()

        assert m.unify_mode == "write"

    def test_put_structure_sets_s_register(self):
        """put_structure sets S to first argument slot."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("h", 3), 1)]

        m.step()

        # STR at heap[0], functor at heap[1]
        # S should point to heap[2] (first arg slot after functor)
        functor_addr = 1
        assert m.S == functor_addr + 1

    def test_put_structure_zero_arity(self):
        """put_structure handles zero-arity functors."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("atom", 0), 0)]

        m.step()

        assert m.heap[0] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("atom", 0))
        assert m.X[0] == 0
        assert m.unify_mode == "write"
        assert m.S == 2  # First arg slot (even though no args)

    def test_put_structure_multiple_structures(self):
        """Multiple put_structure calls allocate separate structures."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 0),
            (OP_PUT_STRUCTURE, ("g", 2), 1),
        ]

        m.step()

        # First structure: STR at 0, functor at 1
        assert m.heap[0] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("f", 1))
        assert m.X[0] == 0

        m.step()

        # Second structure: STR at 2, functor at 3
        assert m.heap[2] == (TAG_STR, 3)
        assert m.heap[3] == (TAG_CON, ("g", 2))
        assert m.X[1] == 2

    def test_put_structure_large_arity(self):
        """put_structure handles large arity correctly."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("big", 10), 2)]

        m.step()

        assert m.heap[0] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("big", 10))
        assert m.S == 2  # Points to first arg slot


class TestPutIntegration:
    """Integration tests for put instruction sequences."""

    def test_build_simple_term(self):
        """Build f(42) using put instructions."""
        m = Machine()
        m.code = [
            (OP_PUT_STRUCTURE, ("f", 1), 0),  # A0 = f/_
            (OP_PUT_CONSTANT, 42, 1),  # A1 = 42
            # In real code, would follow with set_value to fill argument
        ]

        m.step()  # put_structure
        assert m.X[0] == 0  # Points to STR
        assert m.unify_mode == "write"
        assert m.S == 2  # Ready for arg

        m.step()  # put_constant
        assert m.X[1] == 2  # Points to CON(42)

    def test_mixed_put_sequence(self):
        """Sequence of different put instructions."""
        m = Machine()
        m.code = [
            (OP_PUT_VARIABLE, 0, 0),  # X0 = A0 = new var
            (OP_PUT_CONSTANT, "a", 1),  # A1 = atom 'a'
            (OP_PUT_VALUE, 0, 2),  # A2 = X0
            (OP_PUT_STRUCTURE, ("f", 2), 3),  # A3 = f/2
        ]

        m.run()

        # X0 and A0 (X[0]) point to same unbound REF
        var_addr = m.X[0]
        assert m.heap[var_addr] == (TAG_REF, var_addr)

        # A1 points to CON('a')
        assert m.heap[m.X[1]] == (TAG_CON, "a")

        # A2 same as A0
        assert m.X[2] == var_addr

        # A3 points to STR for f/2
        assert m.heap[m.X[3]][0] == TAG_STR

    def test_put_preserves_heap_invariants(self):
        """Put instructions maintain H == len(heap)."""
        m = Machine()
        m.code = [
            (OP_PUT_VARIABLE, 0, 0),
            (OP_PUT_CONSTANT, 1, 1),
            (OP_PUT_STRUCTURE, ("f", 1), 2),
        ]

        for _ in range(3):
            m.step()
            assert m.H == len(m.heap)

        m.check_invariants()


class TestPutEdgeCases:
    """Edge case tests for put instructions."""

    def test_put_to_same_register(self):
        """put_variable Xi, Ai where i is same."""
        m = Machine()
        m.code = [(OP_PUT_VARIABLE, 2, 2)]  # put_variable X2, A2

        m.step()

        # Should work fine, X2 = A2 = new REF
        assert len(m.X) >= 3
        addr = m.X[2]
        assert m.heap[addr] == (TAG_REF, addr)

    def test_put_value_from_uninitialized_register(self):
        """put_value from register with None."""
        m = Machine()
        m.X = [None, None]
        m.code = [(OP_PUT_VALUE, 0, 1)]

        m.step()

        # Should copy None
        assert m.X[1] is None

    def test_put_value_xi_out_of_range(self):
        """put_value with Xi beyond current X length extends and copies None."""
        m = Machine()
        m.X = [10]  # Only X0 initialized
        m.code = [(OP_PUT_VALUE, 3, 0)]  # A0 = X3 (doesn't exist)

        m.step()

        # X should extend to include X3, defaulting to None
        assert len(m.X) >= 4
        # X[3] should be None (default)
        assert m.X[3] is None
        # A0 should copy None from X3
        assert m.X[0] is None

    def test_put_constant_float(self):
        """put_constant with float value."""
        m = Machine()
        m.code = [(OP_PUT_CONSTANT, 3.14, 0)]

        m.step()

        assert m.heap[0] == (TAG_CON, 3.14)
        assert m.X[0] == 0

    def test_put_structure_with_special_functor_name(self):
        """put_structure with special characters in functor."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("@>", 2), 0)]

        m.step()

        assert m.heap[0] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("@>", 2))


class TestPutMachineState:
    """Tests verifying machine state after put instructions."""

    def test_put_variable_increments_p(self):
        """All put instructions increment P."""
        m = Machine()
        m.code = [
            (OP_PUT_VARIABLE, 0, 0),
            (OP_PUT_VALUE, 0, 1),
            (OP_PUT_CONSTANT, 1, 2),
            (OP_PUT_STRUCTURE, ("f", 1), 3),
        ]

        for i in range(4):
            assert m.P == i
            m.step()

        assert m.P == 4

    def test_put_does_not_modify_other_registers(self):
        """Put instructions don't affect unrelated registers."""
        m = Machine()
        m.code = [(OP_PUT_CONSTANT, 42, 0)]

        # Set some initial state
        m.CP = 10
        m.E = 5
        m.B = 3
        m.TR = 0
        m.HB = 0

        m.step()

        # Only P, H, and X should change
        assert m.CP == 10
        assert m.E == 5
        assert m.B == 3
        assert m.TR == 0
        assert m.HB == 0

    def test_put_structure_does_not_trail(self):
        """put_structure doesn't add to trail."""
        m = Machine()
        m.code = [(OP_PUT_STRUCTURE, ("f", 2), 0)]

        m.step()

        assert m.TR == 0
        assert len(m.trail) == 0
