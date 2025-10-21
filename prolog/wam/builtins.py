"""WAM builtin registry and dispatcher.

Provides infrastructure for registering and dispatching native WAM builtins
with consistent interface and error handling.

Design:
- WAM_BUILTINS: Registry dict mapping "module:name/arity" to handler functions
- dispatch_builtin(): Central dispatcher with error handling
- Builtin handlers: Functions that take Machine, return bool (success/failure)

Integration:
- Called from OP_CALL when predicate not in predicate_table
- Builtins can raise PrologError for error conditions
- Python exceptions converted to system_error

Example builtin handler:
    def builtin_var(machine) -> bool:
        '''Check if X0 is unbound variable.'''
        addr = machine.X[0]
        cell = machine.heap[deref(machine, addr)]
        return cell[0] == TAG_REF and cell[1] == addr
"""

from __future__ import annotations

from prolog.wam.errors import python_exception_to_prolog

__all__ = [
    "WAM_BUILTINS",
    "dispatch_builtin",
    "register_builtin",
]


# Builtin registry: maps "module:name/arity" to handler function
# Handler signature: (machine) -> bool
WAM_BUILTINS: dict[str, callable] = {}


def register_builtin(symbol: str, handler: callable) -> None:
    """Register a builtin handler.

    Args:
        symbol: Builtin identifier ("module:name/arity")
        handler: Function taking (machine) -> bool

    Handler contract:
        - Return True on success, False on failure
        - Raise PrologError for ISO-style errors
        - Don't mutate machine state unless succeeding
        - Machine state is unspecified after raising errors

    Example:
        register_builtin("system:var/1", builtin_var)
    """
    WAM_BUILTINS[symbol] = handler


def dispatch_builtin(machine, symbol: str) -> bool:
    """Dispatch to builtin handler.

    Args:
        machine: Machine instance
        symbol: Builtin identifier ("module:name/arity")

    Returns:
        True if builtin succeeded, False if failed

    Raises:
        PrologError: On error conditions (instantiation_error, type_error, etc.)
        RuntimeError: If builtin not found (should be caught by caller)

    Examples:
        # Success case
        if dispatch_builtin(machine, "system:var/1"):
            machine.P += 1  # Continue
        else:
            # Failure: backtrack

        # Error case (raises PrologError)
        try:
            dispatch_builtin(machine, "system:is/2")
        except PrologError:
            # throw/1 will handle it
    """
    # Check if builtin exists
    if symbol not in WAM_BUILTINS:
        raise RuntimeError(f"Undefined builtin: {symbol}")

    # Get handler
    handler = WAM_BUILTINS[symbol]

    try:
        # Call handler
        result = handler(machine)
        return result
    except Exception as e:
        # Convert Python exceptions to PrologError using standard mapping
        # (ZeroDivisionError -> evaluation_error, TypeError -> domain_error, etc.)
        # PrologError instances pass through unchanged
        prolog_error = python_exception_to_prolog(e)
        raise prolog_error
