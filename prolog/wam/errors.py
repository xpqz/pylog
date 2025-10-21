"""WAM Prolog error term mapping.

Provides error classes and utilities to convert between Python exceptions
and ISO Prolog error terms for the WAM runtime.

Error Term Format:
- instantiation_error/0 - Unbound variable where ground term required
- type_error/2 - type_error(Expected, Culprit)
- domain_error/2 - domain_error(Domain, Culprit)
- existence_error/2 - existence_error(Type, Object)
- evaluation_error/1 - evaluation_error(Error)
- system_error/1 - system_error(Message)

Note: Error terms are returned unwrapped. The error/2 wrapper can be
added at throw site if needed for ISO compliance.

Naming: This module exports TypeError (Prolog error class). When working
with Python's built-in TypeError, use builtins.TypeError explicitly.

Handler Bindings: Current throw/catch implementation untrails to frame.TR
on success, so handler won't observe Ball variable bindings. This differs
from ISO Prolog where Ball=Term may be visible in the handler.
"""

from __future__ import annotations

import builtins

from prolog.ast.terms import Atom, Float, Int, List, PrologDict, Struct, Term, Var
from prolog.wam.heap import new_con, new_list, new_ref, new_str, note_struct_args


def _term_to_heap(machine, term: Term) -> int:
    """Convert AST term to heap structure.

    Handles all Prolog term types: Atom, Int, Float, Var, Struct, List, PrologDict.
    Arguments are allocated sequentially right after functor to form valid structure segments.
    Strings become atoms (not Prolog strings) in error terms.

    Args:
        machine: WAM machine instance
        term: AST term to convert

    Returns:
        Heap address of term

    Raises:
        ValueError: If term type is not recognized
    """

    if isinstance(term, Atom):
        return new_con(machine, term.name)
    elif isinstance(term, Int):
        return new_con(machine, term.value)
    elif isinstance(term, Float):
        return new_con(machine, term.value)
    elif isinstance(term, Var):
        return new_ref(machine)
    elif isinstance(term, Struct):
        # Create structure with functor
        arity = len(term.args)
        addr = new_str(machine, term.functor, arity)
        # Convert arguments to heap - they're allocated sequentially in place
        for arg in term.args:
            _term_to_heap(machine, arg)
        return addr
    elif isinstance(term, List):
        # Convert Prolog list to heap list cells
        # Build list from items + tail
        if not term.items:
            # Empty list or just tail
            return _term_to_heap(machine, term.tail)

        # Build list cells right-to-left
        tail_addr = _term_to_heap(machine, term.tail)
        # Process items in reverse to build proper list structure
        for item in reversed(term.items):
            item_addr = _term_to_heap(machine, item)
            tail_addr = new_list(machine, item_addr, tail_addr)
        return tail_addr
    elif isinstance(term, PrologDict):
        # Convert PrologDict to structure dict{K1:V1, K2:V2, ...}
        # Use SWI-compatible dict format: dict functor with key-value pairs
        if not term.pairs:
            # Empty dict - use atom 'dict'
            return new_con(machine, "dict")

        # Build dict as structure with pairs
        # Format: dict(Key1-Value1, Key2-Value2, ...)
        pair_addrs = []
        for key, value in term.pairs:
            # Allocate key and value on heap (sequentially in place)
            _term_to_heap(machine, key)
            _term_to_heap(machine, value)
            # Create pair as -(Key, Value) structure
            pair_addr = new_str(machine, "-", 2)
            # Key and value already allocated at correct positions
            pair_addrs.append(pair_addr)

        # Create dict structure
        addr = new_str(machine, "dict", len(pair_addrs))
        # Add REFs to pair structures (they're not contiguous)
        note_struct_args(machine, *pair_addrs)
        return addr
    else:
        raise ValueError(f"Unknown term type: {type(term)}")


class PrologError(Exception):
    """Base class for WAM Prolog errors.

    Attributes:
        error_type: Type of error (e.g., "instantiation_error", "type_error")
        message: Human-readable error message
        kwargs: Additional error-specific context (expected, culprit, domain, etc.)
    """

    def __init__(self, error_type: str, message: str | None = None, **kwargs):
        """Create PrologError.

        Args:
            error_type: Error type identifier
            message: Optional error message
            **kwargs: Error-specific fields (expected, culprit, domain, etc.)
        """
        self.error_type = error_type
        self.message = message
        self.kwargs = kwargs
        super().__init__(f"{error_type}: {message}" if message else error_type)

    def to_prolog_term(self) -> Struct:
        """Convert to Prolog error term (AST structure).

        Returns:
            Struct representing the error term

        Raises:
            NotImplementedError: Must be overridden by subclasses
        """
        raise NotImplementedError(f"{self.__class__.__name__}.to_prolog_term()")

    def to_heap(self, machine) -> int:
        """Place error term on WAM heap.

        Args:
            machine: WAM machine instance

        Returns:
            Heap address of error structure
        """

        term = self.to_prolog_term()
        return _term_to_heap(machine, term)


class InstantiationError(PrologError):
    """Unbound variable where ground term required.

    ISO: instantiation_error/0
    """

    def __init__(self, context: str | None = None):
        """Create InstantiationError.

        Args:
            context: Optional context describing the unbound variable
        """
        super().__init__("instantiation_error", context)

    def to_prolog_term(self) -> Struct:
        """Convert to instantiation_error/0 structure.

        Returns:
            Struct("instantiation_error", ())
        """
        return Struct("instantiation_error", ())


class TypeError(PrologError):
    """Wrong type for operation.

    ISO: type_error(Expected, Culprit)
    Expected is the expected type (atom), Culprit is the offending term.
    """

    def __init__(self, expected: str, culprit: Term):
        """Create TypeError.

        Args:
            expected: Expected type name (e.g., "integer", "atom", "list")
            culprit: The term that has the wrong type
        """
        super().__init__(
            "type_error", f"Expected {expected}", expected=expected, culprit=culprit
        )

    def to_prolog_term(self) -> Struct:
        """Convert to type_error(Expected, Culprit) structure.

        Returns:
            Struct("type_error", (Atom(expected), culprit))
        """
        expected = Atom(self.kwargs["expected"])
        culprit = self.kwargs["culprit"]
        return Struct("type_error", (expected, culprit))


class DomainError(PrologError):
    """Value outside valid domain.

    ISO: domain_error(Domain, Culprit)
    Domain is the expected domain (atom), Culprit is the offending value.
    """

    def __init__(self, domain: str, culprit: Term):
        """Create DomainError.

        Args:
            domain: Expected domain (e.g., "positive_integer", "valid_value")
            culprit: The value outside the domain
        """
        super().__init__(
            "domain_error",
            f"Value outside domain {domain}",
            domain=domain,
            culprit=culprit,
        )

    def to_prolog_term(self) -> Struct:
        """Convert to domain_error(Domain, Culprit) structure.

        Returns:
            Struct("domain_error", (Atom(domain), culprit))
        """
        domain = Atom(self.kwargs["domain"])
        culprit = self.kwargs["culprit"]
        return Struct("domain_error", (domain, culprit))


class ExistenceError(PrologError):
    """Reference to non-existent object.

    ISO: existence_error(Type, Object)
    Type is the object type (atom), Object is the reference.
    """

    def __init__(self, obj_type: str, culprit: Term):
        """Create ExistenceError.

        Args:
            obj_type: Type of object (e.g., "procedure", "file", "key")
            culprit: Reference to non-existent object
        """
        super().__init__(
            "existence_error",
            f"{obj_type} does not exist",
            obj_type=obj_type,
            culprit=culprit,
        )

    def to_prolog_term(self) -> Struct:
        """Convert to existence_error(Type, Object) structure.

        Returns:
            Struct("existence_error", (Atom(obj_type), culprit))
        """
        obj_type = Atom(self.kwargs["obj_type"])
        culprit = self.kwargs["culprit"]
        return Struct("existence_error", (obj_type, culprit))


class EvaluationError(PrologError):
    """Arithmetic evaluation error.

    ISO: evaluation_error(Error)
    Error is the type of arithmetic error (atom).
    Common errors: zero_divisor, undefined, overflow, underflow.
    """

    def __init__(self, error: str):
        """Create EvaluationError.

        Args:
            error: Error type (e.g., "zero_divisor", "overflow", "undefined")
        """
        super().__init__("evaluation_error", f"Arithmetic error: {error}", error=error)

    def to_prolog_term(self) -> Struct:
        """Convert to evaluation_error(Error) structure.

        Returns:
            Struct("evaluation_error", (Atom(error),))
        """
        error = Atom(self.kwargs["error"])
        return Struct("evaluation_error", (error,))


class SystemError(PrologError):
    """Internal system error.

    ISO: system_error/1
    Non-standard but useful for implementation errors.
    """

    def __init__(self, message: str):
        """Create SystemError.

        Args:
            message: Error message describing the internal failure
        """
        super().__init__("system_error", message)

    def to_prolog_term(self) -> Struct:
        """Convert to system_error(Message) structure.

        Returns:
            Struct("system_error", (Atom(message),))
        """
        message_atom = Atom(self.message if self.message else "unknown error")
        return Struct("system_error", (message_atom,))


def python_exception_to_prolog(exc: Exception) -> PrologError:
    """Convert Python exception to PrologError.

    Provides bridge between Python runtime errors and Prolog error terms.

    Mapping:
    - PrologError subclasses → passthrough unchanged
    - ZeroDivisionError → evaluation_error(zero_divisor)
    - TypeError → domain_error (Python type mismatch is usually domain issue)
    - ValueError → domain_error
    - AttributeError → existence_error
    - KeyError → existence_error
    - Other exceptions → system_error

    Args:
        exc: Python exception to convert

    Returns:
        PrologError instance
    """
    # Already a PrologError - pass through unchanged
    if isinstance(exc, PrologError):
        return exc

    # Map specific Python exceptions
    if isinstance(exc, ZeroDivisionError):
        return EvaluationError("zero_divisor")

    if isinstance(exc, OverflowError):
        return EvaluationError("overflow")

    if isinstance(exc, ArithmeticError):
        # Generic arithmetic error (base class for ZeroDivisionError, OverflowError, etc.)
        return EvaluationError("undefined")

    # Python TypeError (not our TypeError which is a PrologError)
    if isinstance(exc, builtins.TypeError):
        return DomainError("valid_value", Atom(str(exc)))

    if isinstance(exc, ValueError):
        return DomainError("valid_value", Atom(str(exc)))

    if isinstance(exc, AttributeError):
        return ExistenceError("attribute", Atom(str(exc)))

    if isinstance(exc, KeyError):
        return ExistenceError("key", Atom(str(exc)))

    if isinstance(exc, FileNotFoundError):
        # Use filename if available, otherwise message
        filename = getattr(exc, "filename", None)
        culprit = Atom(filename) if filename else Atom(str(exc))
        return ExistenceError("file", culprit)

    if isinstance(exc, OSError):
        # Generic OS error - use system_error
        return SystemError(str(exc))

    # Default: wrap unknown exceptions as system_error
    message = str(exc)
    # Truncate very long messages
    if len(message) > 200:
        message = message[:197] + "..."
    return SystemError(message)


__all__ = [
    "PrologError",
    "InstantiationError",
    "TypeError",
    "DomainError",
    "ExistenceError",
    "EvaluationError",
    "SystemError",
    "python_exception_to_prolog",
]
