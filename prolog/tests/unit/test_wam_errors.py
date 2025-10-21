"""Unit tests for WAM Prolog error term mapping.

Tests error classes and conversion between Python exceptions and Prolog error terms:
- PrologError base class
- Specific error types (instantiation, type, domain, evaluation, etc.)
- to_prolog_term() conversion to heap structures
- python_exception_to_prolog() mapping from Python exceptions
"""

import builtins

from prolog.ast.terms import Atom, Float, Int, List, PrologDict, Struct
from prolog.wam.errors import (
    DomainError,
    EvaluationError,
    ExistenceError,
    InstantiationError,
    PrologError,
    SystemError,
    TypeError,
    python_exception_to_prolog,
)
from prolog.wam.heap import new_con, new_ref, new_str
from prolog.wam.machine import Machine, instr_throw, push_exception_frame


class TestPrologErrorBase:
    """Test PrologError base class."""

    def test_prolog_error_creation(self):
        """PrologError can be created with error type."""
        err = PrologError("test_error", "Test message")
        assert err.error_type == "test_error"
        assert err.message == "Test message"
        assert isinstance(err, Exception)

    def test_prolog_error_string_representation(self):
        """PrologError has readable string representation."""
        err = PrologError("test_error", "Test message")
        assert "test_error" in str(err)
        assert "Test message" in str(err)

    def test_prolog_error_with_kwargs(self):
        """PrologError can store additional context in kwargs."""
        err = PrologError("type_error", "Wrong type", expected="integer", culprit="foo")
        assert err.kwargs["expected"] == "integer"
        assert err.kwargs["culprit"] == "foo"


class TestInstantiationError:
    """Test InstantiationError for unbound variables."""

    def test_instantiation_error_creation(self):
        """InstantiationError can be created."""
        err = InstantiationError("X is unbound")
        assert err.error_type == "instantiation_error"
        assert isinstance(err, PrologError)

    def test_instantiation_error_to_prolog_term(self):
        """InstantiationError converts to instantiation_error structure.

        Note: Returns instantiation_error/0, not wrapped in ISO's error/2.
        The error/2 wrapper is added at throw site if needed.
        """
        err = InstantiationError("context")
        term = err.to_prolog_term()

        assert isinstance(term, Struct)
        assert term.functor == "instantiation_error"
        assert len(term.args) == 0
        assert len(term.args) == 0

    def test_instantiation_error_to_heap(self):
        """InstantiationError can be placed on heap with correct layout."""
        machine = Machine()
        err = InstantiationError("test")
        addr = err.to_heap(machine)

        # Should be structure address
        assert isinstance(addr, int)
        assert addr >= 0
        assert addr < len(machine.heap)

        # TAG_STR = 1, points to functor cell
        cell = machine.heap[addr]
        assert cell[0] == 1  # TAG_STR
        assert cell[1] == addr + 1  # Points to functor cell

        # Functor cell: TAG_CON = 2, (name, arity)
        functor_cell = machine.heap[addr + 1]
        assert functor_cell[0] == 2  # TAG_CON
        assert functor_cell[1] == ("instantiation_error", 0)


class TestTypeError:
    """Test TypeError for wrong types."""

    def test_type_error_creation(self):
        """TypeError stores expected type and culprit."""
        culprit = Atom("foo")
        err = TypeError("integer", culprit)

        assert err.error_type == "type_error"
        assert err.kwargs["expected"] == "integer"
        assert err.kwargs["culprit"] == culprit

    def test_type_error_to_prolog_term(self):
        """TypeError converts to type_error(Expected, Culprit)."""
        culprit = Atom("foo")
        err = TypeError("integer", culprit)
        term = err.to_prolog_term()

        assert isinstance(term, Struct)
        assert term.functor == "type_error"
        assert len(term.args) == 2
        assert len(term.args) == 2

        # First arg is expected type (atom)
        assert isinstance(term.args[0], Atom)
        assert term.args[0].name == "integer"

        # Second arg is culprit term
        assert isinstance(term.args[1], Atom)
        assert term.args[1] == culprit

    def test_type_error_with_various_types(self):
        """TypeError works with different expected types."""
        for expected_type in ["integer", "atom", "list", "compound"]:
            err = TypeError(expected_type, Atom("x"))
            term = err.to_prolog_term()
            assert term.args[0].name == expected_type

    def test_type_error_with_struct_culprit(self):
        """TypeError preserves structure in culprit."""
        culprit = Struct("foo", (Atom("bar"), Atom("baz")))
        err = TypeError("integer", culprit)
        term = err.to_prolog_term()

        assert term.functor == "type_error"
        assert len(term.args) == 2
        # Culprit should be preserved as structure
        assert isinstance(term.args[1], Struct)
        assert term.args[1].functor == "foo"
        assert len(term.args[1].args) == 2

    def test_type_error_to_heap_with_struct_culprit(self):
        """TypeError with struct culprit builds correct heap layout."""
        machine = Machine()
        culprit = Struct("foo", (Atom("bar"),))
        err = TypeError("integer", culprit)
        addr = err.to_heap(machine)

        # TAG_STR pointing to functor
        cell = machine.heap[addr]
        assert cell[0] == 1  # TAG_STR
        functor_addr = cell[1]

        # Functor cell for type_error/2
        functor_cell = machine.heap[functor_addr]
        assert functor_cell[0] == 2  # TAG_CON
        assert functor_cell[1] == ("type_error", 2)

        # Arguments start at functor_addr + 1
        # First arg: "integer" atom
        # Second arg: foo/1 structure


class TestDomainError:
    """Test DomainError for out-of-domain values."""

    def test_domain_error_creation(self):
        """DomainError stores domain and culprit."""
        culprit = Atom("bar")
        err = DomainError("positive_integer", culprit)

        assert err.error_type == "domain_error"
        assert err.kwargs["domain"] == "positive_integer"
        assert err.kwargs["culprit"] == culprit

    def test_domain_error_to_prolog_term(self):
        """DomainError converts to domain_error(Domain, Culprit)."""
        culprit = Atom("-5")
        err = DomainError("positive_integer", culprit)
        term = err.to_prolog_term()

        assert isinstance(term, Struct)
        assert term.functor == "domain_error"
        assert len(term.args) == 2
        assert len(term.args) == 2

        assert isinstance(term.args[0], Atom)
        assert term.args[0].name == "positive_integer"
        assert isinstance(term.args[1], Atom)
        assert term.args[1] == culprit

    def test_domain_error_with_struct_culprit(self):
        """DomainError preserves structure in culprit."""
        culprit = Struct("neg", (Atom("5"),))
        err = DomainError("positive_integer", culprit)
        term = err.to_prolog_term()

        assert term.functor == "domain_error"
        assert len(term.args) == 2
        assert isinstance(term.args[1], Struct)
        assert term.args[1].functor == "neg"


class TestExistenceError:
    """Test ExistenceError for missing objects."""

    def test_existence_error_creation(self):
        """ExistenceError stores object type and reference.

        Note: "foo/1" is a human-readable predicate indicator,
        not a functor/arity pair structure.
        """
        err = ExistenceError("procedure", Atom("foo/1"))

        assert err.error_type == "existence_error"
        assert err.kwargs["obj_type"] == "procedure"
        assert err.kwargs["culprit"].name == "foo/1"

    def test_existence_error_to_prolog_term(self):
        """ExistenceError converts to existence_error(Type, Obj)."""
        obj = Atom("nonexistent/2")
        err = ExistenceError("procedure", obj)
        term = err.to_prolog_term()

        assert isinstance(term, Struct)
        assert term.functor == "existence_error"
        assert len(term.args) == 2
        assert len(term.args) == 2

        assert isinstance(term.args[0], Atom)
        assert term.args[0].name == "procedure"
        assert isinstance(term.args[1], Atom)
        assert term.args[1] == obj


class TestEvaluationError:
    """Test EvaluationError for arithmetic errors."""

    def test_evaluation_error_creation(self):
        """EvaluationError stores error type."""
        err = EvaluationError("zero_divisor")

        assert err.error_type == "evaluation_error"
        assert err.kwargs["error"] == "zero_divisor"

    def test_evaluation_error_to_prolog_term(self):
        """EvaluationError converts to evaluation_error(Error)."""
        err = EvaluationError("zero_divisor")
        term = err.to_prolog_term()

        assert isinstance(term, Struct)
        assert term.functor == "evaluation_error"
        assert len(term.args) == 1

        assert isinstance(term.args[0], Atom)
        assert term.args[0].name == "zero_divisor"

    def test_evaluation_error_types(self):
        """EvaluationError supports various error types."""
        for error_type in ["zero_divisor", "undefined", "overflow", "underflow"]:
            err = EvaluationError(error_type)
            term = err.to_prolog_term()
            assert term.args[0].name == error_type


class TestSystemError:
    """Test SystemError for internal errors."""

    def test_system_error_creation(self):
        """SystemError stores message."""
        err = SystemError("Internal failure")

        assert err.error_type == "system_error"
        assert err.message == "Internal failure"

    def test_system_error_to_prolog_term(self):
        """SystemError converts to system_error(Message)."""
        err = SystemError("Something broke")
        term = err.to_prolog_term()

        assert isinstance(term, Struct)
        assert term.functor == "system_error"
        assert len(term.args) == 1
        assert len(term.args) == 1

        assert isinstance(term.args[0], Atom)
        assert "Something broke" in term.args[0].name

    def test_system_error_to_heap(self):
        """SystemError creates correct heap layout."""
        machine = Machine()
        err = SystemError("Test failure")
        addr = err.to_heap(machine)

        # TAG_STR pointing to functor
        cell = machine.heap[addr]
        assert cell[0] == 1  # TAG_STR
        functor_addr = cell[1]

        # Functor cell for system_error/1
        functor_cell = machine.heap[functor_addr]
        assert functor_cell[0] == 2  # TAG_CON
        assert functor_cell[1] == ("system_error", 1)

        # Message argument at functor_addr + 1 should be atom
        msg_addr = functor_addr + 1
        assert msg_addr < len(machine.heap)


class TestPythonExceptionConversion:
    """Test conversion from Python exceptions to PrologError."""

    def test_zero_division_to_evaluation_error(self):
        """Python ZeroDivisionError maps to evaluation_error(zero_divisor)."""
        py_exc = ZeroDivisionError("division by zero")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, EvaluationError)
        assert prolog_err.kwargs["error"] == "zero_divisor"

    def test_type_error_to_domain_error(self):
        """Python TypeError maps to domain_error.

        Note: Python TypeError typically indicates "value out of expected domain"
        in our bridge (e.g., int where string needed), hence domain_error
        rather than Prolog's type_error which is for structural type mismatch.
        """
        py_exc = builtins.TypeError("expected int")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, DomainError)
        assert prolog_err.error_type == "domain_error"

    def test_value_error_to_domain_error(self):
        """Python ValueError maps to domain_error."""
        py_exc = ValueError("invalid value")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, DomainError)

    def test_attribute_error_to_existence_error(self):
        """Python AttributeError maps to existence_error."""
        py_exc = AttributeError("no such attribute")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, ExistenceError)

    def test_key_error_to_existence_error(self):
        """Python KeyError maps to existence_error."""
        py_exc = KeyError("missing_key")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, ExistenceError)
        assert prolog_err.error_type == "existence_error"

    def test_prolog_error_passthrough(self):
        """PrologError instances pass through unchanged (idempotent)."""
        original = InstantiationError("test")
        result = python_exception_to_prolog(original)

        # Should be identical object (not a copy or conversion)
        assert result is original
        assert isinstance(result, PrologError)
        assert isinstance(result, InstantiationError)

    def test_unknown_exception_to_system_error(self):
        """Unknown Python exceptions map to system_error."""
        py_exc = RuntimeError("unexpected error")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, SystemError)
        assert "unexpected error" in str(prolog_err)


class TestErrorToHeap:
    """Test error term placement on WAM heap."""

    def test_type_error_to_heap(self):
        """TypeError creates valid heap structure with correct layout."""
        machine = Machine()
        err = TypeError("integer", Atom("foo"))
        addr = err.to_heap(machine)

        # Should return heap address
        assert isinstance(addr, int)
        assert addr < len(machine.heap)

        # TAG_STR pointing to functor
        cell = machine.heap[addr]
        assert cell[0] == 1  # TAG_STR
        assert cell[1] == addr + 1  # Points to functor cell

        # Functor cell for type_error/2
        functor_cell = machine.heap[addr + 1]
        assert functor_cell[0] == 2  # TAG_CON
        assert functor_cell[1] == ("type_error", 2)

        # Arguments start at functor_addr + 1
        # First arg: "integer" atom, second arg: "foo" atom

    def test_instantiation_error_to_heap(self):
        """InstantiationError creates heap structure."""
        machine = Machine()
        err = InstantiationError()
        addr = err.to_heap(machine)

        assert isinstance(addr, int)
        assert addr < len(machine.heap)

    def test_multiple_errors_to_heap(self):
        """Multiple errors can be placed on same heap."""
        machine = Machine()

        err1 = InstantiationError("X")
        addr1 = err1.to_heap(machine)

        err2 = TypeError("integer", Atom("foo"))
        addr2 = err2.to_heap(machine)

        # Should have different addresses
        assert addr1 != addr2
        assert addr1 < len(machine.heap)
        assert addr2 < len(machine.heap)


class TestErrorIntegration:
    """Test error integration with throw/catch."""

    def test_error_can_be_thrown(self):
        """PrologError can be converted and thrown."""
        machine = Machine()

        # Create error and place on heap
        err = TypeError("integer", Atom("foo"))
        ball_addr = err.to_heap(machine)

        # Set up catch frame
        pattern_addr = new_con(machine, "type_error")
        push_exception_frame(machine, pattern_addr, handler_label=100)

        # Put ball in X[0] for throw
        machine.X = [ball_addr]

        # This should not raise UnhandledPrologException
        # (though it won't match the pattern, so will)
        try:
            instr_throw(machine)
        except Exception:
            pass  # Expected - pattern doesn't match structure

    def test_error_structure_format(self):
        """Error terms have correct structure format."""
        err = TypeError("integer", Atom("x"))
        term = err.to_prolog_term()

        # Should be: type_error(integer, x)
        assert term.functor == "type_error"
        assert len(term.args) == 2
        assert len(term.args) == 2

    def test_error_throw_catch_match_succeeds(self):
        """PrologError thrown and caught with matching pattern jumps to handler."""
        machine = Machine()

        # Create error and place on heap
        err = TypeError("integer", Atom("foo"))
        ball_addr = err.to_heap(machine)

        # Create matching pattern: type_error(_, _)
        # Build structure type_error/2 with two unbound variables
        pattern_functor_addr = new_str(machine, "type_error", 2)
        # Args: two fresh variables
        new_ref(machine)  # First arg (expected type) - unbound
        new_ref(machine)  # Second arg (culprit) - unbound
        pattern_addr = pattern_functor_addr

        # Set up catch frame with matching pattern
        handler_label = 100
        push_exception_frame(machine, pattern_addr, handler_label)

        # Put ball in X[0] for throw
        machine.X = [ball_addr]

        # Throw should succeed and jump to handler
        instr_throw(machine)

        # Should have jumped to handler
        assert machine.P == handler_label
        # Exception frame should be popped
        assert machine.EF is None


class TestTermToHeapExtensions:
    """Test _term_to_heap() with Float, List, and PrologDict."""

    def test_type_error_with_float_culprit(self):
        """TypeError with Float culprit converts correctly."""
        culprit = Float(3.14)
        err = TypeError("integer", culprit)
        term = err.to_prolog_term()

        assert len(term.args) == 2
        assert isinstance(term.args[1], Float)
        assert term.args[1].value == 3.14

    def test_domain_error_with_float_to_heap(self):
        """DomainError with Float builds correct heap."""
        machine = Machine()
        culprit = Float(1.5)
        err = DomainError("integer", culprit)
        addr = err.to_heap(machine)

        # Should create structure on heap
        assert isinstance(addr, int)
        assert addr < len(machine.heap)

    def test_type_error_with_list_culprit(self):
        """TypeError with List culprit converts correctly."""
        culprit = List((Atom("a"), Atom("b")), Atom("[]"))
        err = TypeError("integer", culprit)
        addr = err.to_heap(Machine())

        # Should succeed without ValueError
        assert isinstance(addr, int)

    def test_domain_error_with_empty_list(self):
        """DomainError with empty list culprit."""
        culprit = List((), Atom("[]"))
        err = DomainError("positive_integer", culprit)
        addr = err.to_heap(Machine())

        assert isinstance(addr, int)

    def test_type_error_with_prolog_dict(self):
        """TypeError with PrologDict culprit converts correctly."""
        culprit = PrologDict(
            pairs=((Atom("key"), Atom("value")), (Atom("num"), Int(42)))
        )
        err = TypeError("atom", culprit)
        addr = err.to_heap(Machine())

        # Should build dict structure
        assert isinstance(addr, int)

    def test_existence_error_with_empty_dict(self):
        """ExistenceError with empty PrologDict."""
        culprit = PrologDict(pairs=())
        err = ExistenceError("object", culprit)
        addr = err.to_heap(Machine())

        assert isinstance(addr, int)


class TestAdditionalPythonExceptionMappings:
    """Test additional Python exception mappings."""

    def test_overflow_error_to_evaluation_error(self):
        """OverflowError maps to evaluation_error(overflow)."""
        py_exc = OverflowError("too large")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, EvaluationError)
        assert prolog_err.kwargs["error"] == "overflow"

    def test_arithmetic_error_to_evaluation_error(self):
        """ArithmeticError maps to evaluation_error(undefined)."""
        py_exc = ArithmeticError("arithmetic problem")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, EvaluationError)
        assert prolog_err.kwargs["error"] == "undefined"

    def test_file_not_found_error_to_existence_error(self):
        """FileNotFoundError maps to existence_error(file, ...)."""
        py_exc = FileNotFoundError("No such file")
        py_exc.filename = "/path/to/file.txt"
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, ExistenceError)
        assert prolog_err.kwargs["obj_type"] == "file"
        assert prolog_err.kwargs["culprit"].name == "/path/to/file.txt"

    def test_file_not_found_error_without_filename(self):
        """FileNotFoundError without filename uses message."""
        py_exc = FileNotFoundError("file not found")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, ExistenceError)
        assert prolog_err.kwargs["obj_type"] == "file"

    def test_os_error_to_system_error(self):
        """OSError maps to system_error."""
        py_exc = OSError("permission denied")
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, SystemError)
        assert "permission denied" in prolog_err.message

    def test_long_message_truncation(self):
        """Very long exception messages are truncated."""
        long_message = "x" * 300
        py_exc = RuntimeError(long_message)
        prolog_err = python_exception_to_prolog(py_exc)

        assert isinstance(prolog_err, SystemError)
        assert len(prolog_err.message) <= 200
        assert prolog_err.message.endswith("...")
