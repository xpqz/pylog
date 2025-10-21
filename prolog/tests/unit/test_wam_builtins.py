"""Tests for WAM builtin registry and dispatcher."""

import pytest

from prolog.ast.terms import Atom
from prolog.wam.builtins import WAM_BUILTINS, dispatch_builtin, register_builtin
from prolog.wam.errors import (
    DomainError,
    EvaluationError,
    InstantiationError,
    TypeError,
)
from prolog.wam.heap import is_ref, new_con, new_ref
from prolog.wam.instructions import OP_CALL_BUILTIN
from prolog.wam.machine import Machine
from prolog.wam.unify import deref


class TestBuiltinRegistry:
    """Test builtin registration and lookup."""

    def test_registry_can_be_cleared_and_restored(self):
        """Builtin registry can be cleared and restored for test isolation."""
        # Save current state (may include pre-registered builtins)
        original_builtins = WAM_BUILTINS.copy()
        original_count = len(WAM_BUILTINS)

        # Clear registry
        WAM_BUILTINS.clear()
        assert len(WAM_BUILTINS) == 0

        # Restore original state
        WAM_BUILTINS.update(original_builtins)
        assert len(WAM_BUILTINS) == original_count

    def test_register_builtin(self):
        """register_builtin() adds handler to registry."""

        def dummy_handler(machine):
            return True

        register_builtin("test:dummy/0", dummy_handler)

        assert "test:dummy/0" in WAM_BUILTINS
        assert WAM_BUILTINS["test:dummy/0"] is dummy_handler

        # Cleanup
        del WAM_BUILTINS["test:dummy/0"]

    def test_register_multiple_builtins(self):
        """Multiple builtins can be registered."""

        def handler1(machine):
            return True

        def handler2(machine):
            return False

        register_builtin("test:foo/1", handler1)
        register_builtin("test:bar/2", handler2)

        assert "test:foo/1" in WAM_BUILTINS
        assert "test:bar/2" in WAM_BUILTINS

        # Cleanup
        del WAM_BUILTINS["test:foo/1"]
        del WAM_BUILTINS["test:bar/2"]


class TestBuiltinDispatcher:
    """Test dispatch_builtin() function."""

    def test_dispatch_calls_handler(self):
        """dispatch_builtin() calls registered handler."""
        called = []

        def test_handler(machine):
            called.append(machine)
            return True

        register_builtin("test:called/0", test_handler)

        machine = Machine()
        result = dispatch_builtin(machine, "test:called/0")

        assert result is True
        assert len(called) == 1
        assert called[0] is machine

        # Cleanup
        del WAM_BUILTINS["test:called/0"]

    def test_dispatch_returns_handler_result(self):
        """dispatch_builtin() returns handler's boolean result."""

        def success_handler(machine):
            return True

        def failure_handler(machine):
            return False

        register_builtin("test:success/0", success_handler)
        register_builtin("test:failure/0", failure_handler)

        machine = Machine()

        assert dispatch_builtin(machine, "test:success/0") is True
        assert dispatch_builtin(machine, "test:failure/0") is False

        # Cleanup
        del WAM_BUILTINS["test:success/0"]
        del WAM_BUILTINS["test:failure/0"]

    def test_dispatch_undefined_builtin_raises_runtime_error(self):
        """dispatch_builtin() raises RuntimeError for undefined builtin."""
        machine = Machine()

        with pytest.raises(RuntimeError, match="Undefined builtin: test:undefined/0"):
            dispatch_builtin(machine, "test:undefined/0")

    def test_dispatch_propagates_prolog_error(self):
        """dispatch_builtin() re-raises PrologError from handler."""

        def error_handler(machine):
            raise InstantiationError()

        register_builtin("test:error/0", error_handler)

        machine = Machine()

        with pytest.raises(InstantiationError):
            dispatch_builtin(machine, "test:error/0")

        # Cleanup
        del WAM_BUILTINS["test:error/0"]

    def test_dispatch_converts_python_exception_to_prolog_error(self):
        """dispatch_builtin() converts Python exceptions using python_exception_to_prolog()."""

        def python_error_handler(machine):
            raise ValueError("test error message")

        register_builtin("test:pyerror/0", python_error_handler)

        machine = Machine()

        # ValueError should be converted to DomainError
        with pytest.raises(DomainError) as exc_info:
            dispatch_builtin(machine, "test:pyerror/0")

        # Check it's a proper DomainError
        assert exc_info.value.kwargs["domain"] == "valid_value"

        # Cleanup
        del WAM_BUILTINS["test:pyerror/0"]

    def test_dispatch_converts_zerodivision_to_evaluation_error(self):
        """dispatch_builtin() converts ZeroDivisionError to evaluation_error(zero_divisor)."""

        def division_error_handler(machine):
            return 1 // 0  # ZeroDivisionError

        register_builtin("test:divzero/0", division_error_handler)

        machine = Machine()

        # ZeroDivisionError should be converted to EvaluationError
        with pytest.raises(EvaluationError) as exc_info:
            dispatch_builtin(machine, "test:divzero/0")

        # Check it's evaluation_error(zero_divisor)
        assert exc_info.value.kwargs["error"] == "zero_divisor"

        # Cleanup
        del WAM_BUILTINS["test:divzero/0"]


class TestBuiltinInstruction:
    """Test OP_CALL_BUILTIN instruction integration."""

    def test_call_builtin_success_continues(self):
        """OP_CALL_BUILTIN continues to next instruction on success."""

        def success_builtin(machine):
            return True

        register_builtin("test:succeed/0", success_builtin)

        machine = Machine()
        machine.code = [
            (OP_CALL_BUILTIN, "test:succeed/0"),
            (1,),  # OP_HALT
        ]
        machine.P = 0

        # Execute call_builtin
        result = machine.step()

        assert result is True
        assert machine.P == 1  # Advanced to next instruction
        assert not machine.halted

        # Cleanup
        del WAM_BUILTINS["test:succeed/0"]

    def test_call_builtin_failure_backtracks(self):
        """OP_CALL_BUILTIN backtracks on failure."""

        def failure_builtin(machine):
            return False

        register_builtin("test:fail/0", failure_builtin)

        machine = Machine()
        machine.code = [
            (OP_CALL_BUILTIN, "test:fail/0"),
        ]
        machine.P = 0

        # Execute call_builtin
        machine.step()

        # Machine should have called fail() which halts with no choicepoint
        assert machine.halted

        # Cleanup
        del WAM_BUILTINS["test:fail/0"]

    def test_call_builtin_error_throws(self):
        """OP_CALL_BUILTIN throws PrologError from handler."""

        def error_builtin(machine):
            raise TypeError("integer", Atom("atom"))

        register_builtin("test:typerror/0", error_builtin)

        machine = Machine()
        machine.code = [
            (OP_CALL_BUILTIN, "test:typerror/0"),
        ]
        machine.P = 0

        # Execute call_builtin - should throw error
        # With no exception frame, this should halt
        machine.step()

        # Should have executed throw protocol which halts if no frame
        assert machine.halted

        # Cleanup
        del WAM_BUILTINS["test:typerror/0"]

    def test_call_builtin_undefined_halts(self):
        """OP_CALL_BUILTIN halts on undefined builtin."""
        machine = Machine()
        machine.code = [
            (OP_CALL_BUILTIN, "test:undefined/0"),
        ]
        machine.P = 0

        # Execute call_builtin with undefined builtin
        result = machine.step()

        # Should halt on RuntimeError
        assert result is False
        assert machine.halted


class TestBuiltinExamples:
    """Example builtin implementations for documentation."""

    def test_example_var_builtin(self):
        """Example: var/1 builtin implementation pattern."""

        def example_var_impl(machine):
            """Check if X0 is unbound variable."""
            addr = machine.X[0]
            dereffed = deref(machine, addr)
            cell = machine.heap[dereffed]
            # Unbound if REF pointing to itself
            return cell[0] == 0 and cell[1] == dereffed

        register_builtin("test:example_var/1", example_var_impl)

        # Test with unbound variable
        machine = Machine()
        x_addr = new_ref(machine)
        machine.X = [x_addr]

        result = dispatch_builtin(machine, "test:example_var/1")
        assert result is True

        # Test with bound variable
        machine2 = Machine()
        bound_addr = new_con(machine2, 42)
        machine2.X = [bound_addr]

        result2 = dispatch_builtin(machine2, "test:example_var/1")
        assert result2 is False

        # Cleanup
        del WAM_BUILTINS["test:example_var/1"]

    def test_example_builtin_with_error(self):
        """Example: Builtin that raises error on wrong type."""

        def builtin_needs_int(machine):
            """Require X0 to be integer."""
            addr = deref(machine, machine.X[0])
            cell = machine.heap[addr]

            if is_ref(cell):
                # Unbound variable
                raise InstantiationError()

            if cell[0] != 2:  # TAG_CON
                raise TypeError("integer", addr)

            value = cell[1]
            if not isinstance(value, int):
                raise TypeError("integer", addr)

            return True

        register_builtin("test:needs_int/1", builtin_needs_int)

        # Test with integer succeeds
        machine = Machine()
        int_addr = new_con(machine, 42)
        machine.X = [int_addr]

        result = dispatch_builtin(machine, "test:needs_int/1")
        assert result is True

        # Test with unbound raises InstantiationError
        machine2 = Machine()
        unbound_addr = new_ref(machine2)
        machine2.X = [unbound_addr]

        with pytest.raises(InstantiationError):
            dispatch_builtin(machine2, "test:needs_int/1")

        # Test with atom raises TypeError
        machine3 = Machine()
        atom_addr = new_con(machine3, "atom")
        machine3.X = [atom_addr]

        with pytest.raises(TypeError):
            dispatch_builtin(machine3, "test:needs_int/1")

        # Cleanup
        del WAM_BUILTINS["test:needs_int/1"]
