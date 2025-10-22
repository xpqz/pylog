"""Tests for WAM meta-call builtins."""

import pytest

from prolog.wam.builtins import WAM_BUILTINS
from prolog.wam.errors import InstantiationError, TypeError
from prolog.wam.heap import new_con, new_ref, new_str
from prolog.wam.instructions import OP_CALL_BUILTIN, OP_HALT, OP_PROCEED
from prolog.wam.machine import Machine


class TestCallBuiltin:
    """Test call/1 builtin."""

    def test_call_atom_succeeds(self):
        """call(Atom) succeeds when Atom is a 0-arity predicate."""
        machine = Machine()

        # Register a simple predicate user:foo/0 that just proceeds
        machine.code = [
            # user:foo/0 at address 0
            (OP_PROCEED,),
            # Main code at address 1
            (OP_CALL_BUILTIN, "system:call/1"),
            (OP_HALT,),
        ]
        machine.register_predicate("user:foo/0", 0)

        # Set up call argument: foo
        foo_addr = new_con(machine, "foo")
        machine.X = [foo_addr]
        machine.P = 1

        # Execute call/1
        result = machine.step()

        assert result is True
        # P should now point to user:foo/0 (address 0)
        assert machine.P == 0
        # CP should be saved as return address (pointing to HALT)
        assert machine.CP == 2

    def test_call_structure_succeeds(self):
        """call(f(Args)) succeeds and loads arguments into X registers."""
        machine = Machine()

        # Register user:append/3
        machine.code = [
            # user:append/3 at address 0 (stub: just proceed)
            (OP_PROCEED,),
            # Main code at address 1
            (OP_CALL_BUILTIN, "system:call/1"),
            (OP_HALT,),
        ]
        machine.register_predicate("user:append/3", 0)

        # Build append([1],[2],X) structure
        # new_str creates: STR cell at H, functor cell at H+1
        # We need to append the 3 arguments after the functor cell

        f_addr = new_str(machine, "append", 3)
        # f_addr points to STR cell
        # functor is at heap[f_addr][1]
        functor_addr = machine.heap[f_addr][1]

        # Create arguments and append them after functor cell
        list1_addr = new_con(machine, "[1_placeholder]")  # Simplified
        list2_addr = new_con(machine, "[2_placeholder]")
        x_addr = new_ref(machine)

        # Arguments should be at functor_addr+1, +2, +3
        machine.heap.append(list1_addr)
        machine.heap.append(list2_addr)
        machine.heap.append(x_addr)

        machine.X = [f_addr]
        machine.P = 1

        # Execute call/1
        result = machine.step()

        assert result is True
        # X registers should now hold the arguments
        assert len(machine.X) == 3
        assert machine.X[0] == functor_addr + 1  # [1]
        assert machine.X[1] == functor_addr + 2  # [2]
        assert machine.X[2] == functor_addr + 3  # X
        # P should point to user:append/3
        assert machine.P == 0
        # CP saved
        assert machine.CP == 2

    def test_call_with_unbound_variable_raises_instantiation_error(self):
        """call(X) raises instantiation_error when X is unbound."""
        machine = Machine()

        # Create unbound variable
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        # call/1 should raise InstantiationError
        with pytest.raises(InstantiationError):
            WAM_BUILTINS["system:call/1"](machine)

    def test_call_with_integer_raises_type_error(self):
        """call(123) raises type_error(callable, 123)."""
        machine = Machine()

        # Create integer
        int_addr = new_con(machine, 123)
        machine.X = [int_addr]

        # call/1 should raise TypeError
        with pytest.raises(TypeError) as exc_info:
            WAM_BUILTINS["system:call/1"](machine)

        assert exc_info.value.kwargs["expected"] == "callable"

    def test_call_with_integer_via_instruction(self):
        """call(123) via OP_CALL_BUILTIN properly converts TypeError to throw."""
        machine = Machine()

        # Set up code that calls call/1 with an integer
        machine.code = [
            (OP_CALL_BUILTIN, "system:call/1"),
            (OP_HALT,),
        ]

        # Create integer goal
        int_addr = new_con(machine, 123)
        machine.X = [int_addr]
        machine.P = 0

        # Execute - should convert TypeError to PrologError and throw
        # The exception handling will halt the machine
        machine.step()

        # Machine should halt due to unhandled exception
        assert machine.halted is True

    def test_call_with_undefined_predicate_fails(self):
        """call(undefined_pred) fails when predicate not registered."""
        machine = Machine()

        # Create atom for undefined predicate
        pred_addr = new_con(machine, "undefined_pred")
        machine.X = [pred_addr]

        # call/1 should return False (predicate not found)
        result = WAM_BUILTINS["system:call/1"](machine)
        assert result is False

    def test_call_via_instruction(self):
        """call/1 works via OP_CALL_BUILTIN instruction."""
        machine = Machine()

        # Register user:succeed/0
        machine.code = [
            # user:succeed/0 at address 0
            (OP_PROCEED,),
            # Main code at address 1
            (OP_CALL_BUILTIN, "system:call/1"),
            (OP_HALT,),
        ]
        machine.register_predicate("user:succeed/0", 0)

        # Set up call argument
        succeed_addr = new_con(machine, "succeed")
        machine.X = [succeed_addr]
        machine.P = 1

        # Execute call_builtin instruction
        result = machine.step()

        assert result is True
        assert machine.P == 0  # Jumped to user:succeed/0

        # Execute proceed
        result = machine.step()

        assert result is True
        assert machine.P == 2  # Returned to HALT

    def test_call_builtin_registered(self):
        """call/1 is registered in WAM_BUILTINS."""
        assert "system:call/1" in WAM_BUILTINS


class TestCallIntegration:
    """Integration tests for call/1 with real predicates."""

    def test_call_with_true_succeeds(self):
        """call(true) succeeds."""
        machine = Machine()

        # Register user:true/0 that succeeds
        machine.code = [
            # user:true/0 at address 0
            (OP_PROCEED,),
            # Main at address 1
            (OP_CALL_BUILTIN, "system:call/1"),
            (OP_HALT,),
        ]
        machine.register_predicate("user:true/0", 0)

        true_addr = new_con(machine, "true")
        machine.X = [true_addr]
        machine.P = 1

        # Execute call/1
        machine.step()
        assert machine.P == 0

        # Execute user:true/0
        machine.step()
        assert machine.P == 2  # Returned

    def test_call_preserves_cp(self):
        """call/1 properly saves and restores continuation pointer."""
        machine = Machine()

        machine.code = [
            # user:foo/0 at address 0
            (OP_PROCEED,),
            # Main at address 1
            (OP_CALL_BUILTIN, "system:call/1"),
            (OP_HALT,),
        ]
        machine.register_predicate("user:foo/0", 0)

        foo_addr = new_con(machine, "foo")
        machine.X = [foo_addr]
        machine.P = 1
        machine.CP = None

        # Execute call/1
        machine.step()

        # CP should be set to return address
        assert machine.CP == 2

        # Execute foo/0 (proceed)
        machine.step()

        # Should have returned to HALT
        assert machine.P == 2
