"""Tests for WAM type checking builtins."""


from prolog.wam.builtins import WAM_BUILTINS
from prolog.wam.heap import new_con, new_ref, new_str
from prolog.wam.instructions import OP_CALL_BUILTIN
from prolog.wam.machine import Machine


class TestVarBuiltin:
    """Test var/1 builtin."""

    def test_var_with_unbound_variable(self):
        """var(X) succeeds when X is unbound."""
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        result = WAM_BUILTINS["system:var/1"](machine)
        assert result is True

    def test_var_with_integer_fails(self):
        """var(5) fails."""
        machine = Machine()
        int_addr = new_con(machine, 5)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:var/1"](machine)
        assert result is False

    def test_var_with_atom_fails(self):
        """var(foo) fails."""
        machine = Machine()
        atom_addr = new_con(machine, "foo")
        machine.X = [atom_addr]

        result = WAM_BUILTINS["system:var/1"](machine)
        assert result is False

    def test_var_with_structure_fails(self):
        """var(f(a)) fails."""
        machine = Machine()
        f_addr = new_str(machine, "f", 1)
        a_addr = new_con(machine, "a")
        machine.heap.append((0, a_addr))  # Add argument

        machine.X = [f_addr]

        result = WAM_BUILTINS["system:var/1"](machine)
        assert result is False

    def test_var_with_bound_variable_fails(self):
        """var(X) fails when X is bound."""
        machine = Machine()
        x_addr = new_ref(machine)
        val_addr = new_con(machine, 42)

        # Bind X to 42
        machine.heap[x_addr] = (0, val_addr)  # TAG_REF

        machine.X = [x_addr]

        result = WAM_BUILTINS["system:var/1"](machine)
        assert result is False


class TestNonvarBuiltin:
    """Test nonvar/1 builtin."""

    def test_nonvar_with_integer_succeeds(self):
        """nonvar(5) succeeds."""
        machine = Machine()
        int_addr = new_con(machine, 5)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:nonvar/1"](machine)
        assert result is True

    def test_nonvar_with_atom_succeeds(self):
        """nonvar(foo) succeeds."""
        machine = Machine()
        atom_addr = new_con(machine, "foo")
        machine.X = [atom_addr]

        result = WAM_BUILTINS["system:nonvar/1"](machine)
        assert result is True

    def test_nonvar_with_unbound_variable_fails(self):
        """nonvar(X) fails when X is unbound."""
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        result = WAM_BUILTINS["system:nonvar/1"](machine)
        assert result is False

    def test_nonvar_with_bound_variable_succeeds(self):
        """nonvar(X) succeeds when X is bound."""
        machine = Machine()
        x_addr = new_ref(machine)
        val_addr = new_con(machine, 42)

        # Bind X to 42
        machine.heap[x_addr] = (0, val_addr)  # TAG_REF

        machine.X = [x_addr]

        result = WAM_BUILTINS["system:nonvar/1"](machine)
        assert result is True


class TestAtomBuiltin:
    """Test atom/1 builtin."""

    def test_atom_with_atom_succeeds(self):
        """atom(foo) succeeds."""
        machine = Machine()
        atom_addr = new_con(machine, "foo")
        machine.X = [atom_addr]

        result = WAM_BUILTINS["system:atom/1"](machine)
        assert result is True

    def test_atom_with_empty_list_succeeds(self):
        """atom([]) succeeds (empty list is atom)."""
        machine = Machine()
        nil_addr = new_con(machine, "[]")
        machine.X = [nil_addr]

        result = WAM_BUILTINS["system:atom/1"](machine)
        assert result is True

    def test_atom_with_integer_fails(self):
        """atom(5) fails."""
        machine = Machine()
        int_addr = new_con(machine, 5)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:atom/1"](machine)
        assert result is False

    def test_atom_with_float_fails(self):
        """atom(5.0) fails."""
        machine = Machine()
        float_addr = new_con(machine, 5.0)
        machine.X = [float_addr]

        result = WAM_BUILTINS["system:atom/1"](machine)
        assert result is False

    def test_atom_with_variable_fails(self):
        """atom(X) fails."""
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        result = WAM_BUILTINS["system:atom/1"](machine)
        assert result is False

    def test_atom_with_structure_fails(self):
        """atom(f(a)) fails."""
        machine = Machine()
        f_addr = new_str(machine, "f", 1)
        a_addr = new_con(machine, "a")
        machine.heap.append((0, a_addr))

        machine.X = [f_addr]

        result = WAM_BUILTINS["system:atom/1"](machine)
        assert result is False


class TestIntegerBuiltin:
    """Test integer/1 builtin."""

    def test_integer_with_positive_integer_succeeds(self):
        """integer(5) succeeds."""
        machine = Machine()
        int_addr = new_con(machine, 5)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:integer/1"](machine)
        assert result is True

    def test_integer_with_negative_integer_succeeds(self):
        """integer(-5) succeeds."""
        machine = Machine()
        int_addr = new_con(machine, -5)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:integer/1"](machine)
        assert result is True

    def test_integer_with_zero_succeeds(self):
        """integer(0) succeeds."""
        machine = Machine()
        int_addr = new_con(machine, 0)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:integer/1"](machine)
        assert result is True

    def test_integer_with_float_fails(self):
        """integer(5.0) fails."""
        machine = Machine()
        float_addr = new_con(machine, 5.0)
        machine.X = [float_addr]

        result = WAM_BUILTINS["system:integer/1"](machine)
        assert result is False

    def test_integer_with_atom_fails(self):
        """integer(foo) fails."""
        machine = Machine()
        atom_addr = new_con(machine, "foo")
        machine.X = [atom_addr]

        result = WAM_BUILTINS["system:integer/1"](machine)
        assert result is False

    def test_integer_with_variable_fails(self):
        """integer(X) fails."""
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        result = WAM_BUILTINS["system:integer/1"](machine)
        assert result is False


class TestNumberBuiltin:
    """Test number/1 builtin."""

    def test_number_with_integer_succeeds(self):
        """number(5) succeeds."""
        machine = Machine()
        int_addr = new_con(machine, 5)
        machine.X = [int_addr]

        result = WAM_BUILTINS["system:number/1"](machine)
        assert result is True

    def test_number_with_float_succeeds(self):
        """number(5.0) succeeds."""
        machine = Machine()
        float_addr = new_con(machine, 5.0)
        machine.X = [float_addr]

        result = WAM_BUILTINS["system:number/1"](machine)
        assert result is True

    def test_number_with_negative_float_succeeds(self):
        """number(-3.14) succeeds."""
        machine = Machine()
        float_addr = new_con(machine, -3.14)
        machine.X = [float_addr]

        result = WAM_BUILTINS["system:number/1"](machine)
        assert result is True

    def test_number_with_atom_fails(self):
        """number(foo) fails."""
        machine = Machine()
        atom_addr = new_con(machine, "foo")
        machine.X = [atom_addr]

        result = WAM_BUILTINS["system:number/1"](machine)
        assert result is False

    def test_number_with_variable_fails(self):
        """number(X) fails."""
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        result = WAM_BUILTINS["system:number/1"](machine)
        assert result is False


class TestInstructionIntegration:
    """Test integration with OP_CALL_BUILTIN instruction."""

    def test_var_via_instruction(self):
        """Test var/1 via OP_CALL_BUILTIN instruction."""
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        machine.code = [
            (OP_CALL_BUILTIN, "system:var/1"),
        ]
        machine.P = 0

        # Execute instruction
        result = machine.step()

        assert result is True
        assert machine.P == 1  # Advanced to next instruction

    def test_atom_via_instruction_succeeds(self):
        """Test atom/1 via OP_CALL_BUILTIN instruction (success)."""
        machine = Machine()
        atom_addr = new_con(machine, "foo")
        machine.X = [atom_addr]

        machine.code = [
            (OP_CALL_BUILTIN, "system:atom/1"),
        ]
        machine.P = 0

        result = machine.step()

        assert result is True
        assert machine.P == 1

    def test_atom_via_instruction_fails(self):
        """Test atom/1 via OP_CALL_BUILTIN instruction (failure)."""
        machine = Machine()
        int_addr = new_con(machine, 42)
        machine.X = [int_addr]

        machine.code = [
            (OP_CALL_BUILTIN, "system:atom/1"),
        ]
        machine.P = 0

        # Execute instruction
        machine.step()

        # Should have halted on failure
        assert machine.halted


class TestBuiltinRegistration:
    """Test that type builtins are registered on module import."""

    def test_var_registered(self):
        """var/1 is registered in WAM_BUILTINS."""
        assert "system:var/1" in WAM_BUILTINS

    def test_nonvar_registered(self):
        """nonvar/1 is registered in WAM_BUILTINS."""
        assert "system:nonvar/1" in WAM_BUILTINS

    def test_atom_registered(self):
        """atom/1 is registered in WAM_BUILTINS."""
        assert "system:atom/1" in WAM_BUILTINS

    def test_integer_registered(self):
        """integer/1 is registered in WAM_BUILTINS."""
        assert "system:integer/1" in WAM_BUILTINS

    def test_number_registered(self):
        """number/1 is registered in WAM_BUILTINS."""
        assert "system:number/1" in WAM_BUILTINS
