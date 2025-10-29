"""ISO Prolog I/O builtin predicates.

Implements the predicates that interface with the stream system:
- open/3: Open a stream
- close/1-2: Close a stream
- read/1-2: Read terms from streams
- write/1-2: Write terms to streams
- nl/0-1: Write newlines
- current_output/1: Get current output stream
- stream_property/2: Query stream properties
"""

from __future__ import annotations

from typing import Dict, Tuple, Callable

from prolog.ast.terms import Atom, Var, Struct, Term, List as PrologList
from prolog.engine.errors import PrologThrow
from prolog.engine.builtins.streams import get_stream_manager


def deref_term(engine, term: Term) -> Term:
    """Dereference a term using the engine's store."""
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "UNBOUND":
            return term  # Return the unbound variable
        elif result[0] == "BOUND":
            # result is ("BOUND", root_vid, bound_term)
            return result[2]  # Return the bound term
        else:
            # Shouldn't happen but just in case
            return term
    return term


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register I/O builtin predicates.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    registry[("open", 3)] = builtin_open
    registry[("close", 1)] = builtin_close
    registry[("write", 1)] = builtin_write_1
    registry[("write", 2)] = builtin_write_2
    registry[("read", 1)] = builtin_read_1
    registry[("read", 2)] = builtin_read_2
    registry[("nl", 0)] = builtin_nl_0
    registry[("nl", 1)] = builtin_nl_1
    registry[("current_output", 1)] = builtin_current_output
    # Keep the builtin for now, but the Prolog wrapper provides nondeterministic behavior
    registry[("stream_property", 2)] = builtin_stream_property
    registry[("$stream_properties", 2)] = builtin_stream_properties


def builtin_open(engine, args: tuple) -> bool:
    """open(+File, +Mode, -Stream).

    Open a file as a stream.
    """
    if len(args) != 3:
        return False

    file_arg = deref_term(engine, args[0])
    mode_arg = deref_term(engine, args[1])
    stream_arg = deref_term(engine, args[2])

    # Check instantiation
    if isinstance(file_arg, Var):
        raise PrologThrow(
            Struct("error", (Atom("instantiation_error"), Atom("open/3")))
        )

    if isinstance(mode_arg, Var):
        raise PrologThrow(
            Struct("error", (Atom("instantiation_error"), Atom("open/3")))
        )

    # Check types
    if not isinstance(file_arg, Atom):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("type_error", (Atom("atom"), file_arg)), Atom("open/3")),
            )
        )

    if not isinstance(mode_arg, Atom):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("type_error", (Atom("atom"), mode_arg)), Atom("open/3")),
            )
        )

    # Check valid modes
    if mode_arg.name not in ("read", "write", "append"):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("domain_error", (Atom("io_mode"), mode_arg)), Atom("open/3")),
            )
        )

    # Try to open the stream
    manager = get_stream_manager()

    try:
        stream_id = manager.open_stream(file_arg.name, mode_arg.name)

        # Unify with stream ID as an atom
        return engine.unify(stream_arg, Atom(stream_id))

    except FileNotFoundError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct("existence_error", (Atom("source_sink"), file_arg)),
                    Atom("open/3"),
                ),
            )
        )

    except PermissionError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct(
                        "permission_error",
                        (Atom("open"), Atom("source_sink"), file_arg),
                    ),
                    Atom("open/3"),
                ),
            )
        )


def builtin_close(engine, args: tuple) -> bool:
    """close(+Stream).

    Close a stream.
    """
    if len(args) != 1:
        return False

    stream_arg = deref_term(engine, args[0])

    # Check instantiation
    if isinstance(stream_arg, Var):
        raise PrologThrow(
            Struct("error", (Atom("instantiation_error"), Atom("close/1")))
        )

    # Check type
    if not isinstance(stream_arg, Atom):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("type_error", (Atom("atom"), stream_arg)), Atom("close/1")),
            )
        )

    # Try to close the stream
    manager = get_stream_manager()

    try:
        manager.close_stream(stream_arg.name)
        return True

    except ValueError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct("existence_error", (Atom("stream"), stream_arg)),
                    Atom("close/1"),
                ),
            )
        )


def builtin_write_1(engine, args: tuple) -> bool:
    """write(+Term).

    Write a term to the current output stream.
    """
    if len(args) != 1:
        return False

    term_arg = deref_term(engine, args[0])

    manager = get_stream_manager()
    current_output = manager.aliases.get(manager.current_output)

    if current_output:
        try:
            manager.write_term(current_output, term_arg)
            return True
        except Exception:
            return False
    return False


def builtin_write_2(engine, args: tuple) -> bool:
    """write(+Stream, +Term).

    Write a term to a specific stream.
    """
    if len(args) != 2:
        return False

    stream_arg = deref_term(engine, args[0])
    term_arg = deref_term(engine, args[1])

    # Check instantiation
    if isinstance(stream_arg, Var):
        raise PrologThrow(
            Struct("error", (Atom("instantiation_error"), Atom("write/2")))
        )

    # Check type
    if not isinstance(stream_arg, Atom):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("type_error", (Atom("atom"), stream_arg)), Atom("write/2")),
            )
        )

    manager = get_stream_manager()

    try:
        manager.write_term(stream_arg.name, term_arg)
        return True

    except ValueError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct("existence_error", (Atom("stream"), stream_arg)),
                    Atom("write/2"),
                ),
            )
        )

    except PermissionError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct(
                        "permission_error", (Atom("output"), Atom("stream"), stream_arg)
                    ),
                    Atom("write/2"),
                ),
            )
        )


def builtin_read_1(engine, args: tuple) -> bool:
    """read(-Term).

    Read a term from the current input stream.
    """
    if len(args) != 1:
        return False

    term_arg = deref_term(engine, args[0])

    manager = get_stream_manager()
    current_input = manager.aliases.get(manager.current_input)

    if current_input:
        try:
            term = manager.read_term(current_input)
            return engine.unify(term_arg, term)
        except Exception:
            return False
    return False


def builtin_read_2(engine, args: tuple) -> bool:
    """read(+Stream, -Term).

    Read a term from a specific stream.
    """
    if len(args) != 2:
        return False

    stream_arg = deref_term(engine, args[0])
    term_arg = deref_term(engine, args[1])

    # Check instantiation
    if isinstance(stream_arg, Var):
        raise PrologThrow(
            Struct("error", (Atom("instantiation_error"), Atom("read/2")))
        )

    # Check type
    if not isinstance(stream_arg, Atom):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("type_error", (Atom("atom"), stream_arg)), Atom("read/2")),
            )
        )

    manager = get_stream_manager()

    try:
        term = manager.read_term(stream_arg.name)
        return engine.unify(term_arg, term)

    except ValueError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct("existence_error", (Atom("stream"), stream_arg)),
                    Atom("read/2"),
                ),
            )
        )

    except PermissionError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct(
                        "permission_error", (Atom("input"), Atom("stream"), stream_arg)
                    ),
                    Atom("read/2"),
                ),
            )
        )


def builtin_nl_0(engine, args: tuple) -> bool:
    """nl.

    Write a newline to the current output stream.
    """
    if len(args) != 0:
        return False

    manager = get_stream_manager()
    current_output = manager.aliases.get(manager.current_output)

    if current_output:
        try:
            manager.write_newline(current_output)
            return True
        except Exception:
            return False
    return False


def builtin_nl_1(engine, args: tuple) -> bool:
    """nl(+Stream).

    Write a newline to a specific stream.
    """
    if len(args) != 1:
        return False

    stream_arg = deref_term(engine, args[0])

    # Check instantiation
    if isinstance(stream_arg, Var):
        raise PrologThrow(Struct("error", (Atom("instantiation_error"), Atom("nl/1"))))

    # Check type
    if not isinstance(stream_arg, Atom):
        raise PrologThrow(
            Struct(
                "error",
                (Struct("type_error", (Atom("atom"), stream_arg)), Atom("nl/1")),
            )
        )

    manager = get_stream_manager()

    try:
        manager.write_newline(stream_arg.name)
        return True

    except ValueError:
        raise PrologThrow(
            Struct(
                "error",
                (Struct("existence_error", (Atom("stream"), stream_arg)), Atom("nl/1")),
            )
        )

    except PermissionError:
        raise PrologThrow(
            Struct(
                "error",
                (
                    Struct(
                        "permission_error", (Atom("output"), Atom("stream"), stream_arg)
                    ),
                    Atom("nl/1"),
                ),
            )
        )


def builtin_current_output(engine, args: tuple) -> bool:
    """current_output(-Stream).

    Unify with the current output stream.
    """
    if len(args) != 1:
        return False

    stream_arg = deref_term(engine, args[0])

    manager = get_stream_manager()
    current_output = manager.aliases.get(manager.current_output)

    if current_output:
        return engine.unify(stream_arg, Atom(current_output))
    return False


def builtin_stream_property(engine, args: tuple) -> bool:
    """stream_property(?Stream, ?Property).

    Query or enumerate stream properties.
    NOTE: This is a deterministic builtin. For nondeterministic backtracking,
    load the Prolog wrapper from streams.pl which uses member/2.
    """
    if len(args) != 2:
        return False

    stream_arg = deref_term(engine, args[0])
    property_arg = deref_term(engine, args[1])

    manager = get_stream_manager()

    # For simplicity in this initial implementation, we'll handle
    # only the deterministic cases where both stream and property
    # are specified or where stream is specified.

    if isinstance(stream_arg, Atom):
        stream_id = stream_arg.name

        # Check if stream exists
        if stream_id not in manager.streams:
            return False

        properties = manager.get_stream_properties(stream_id)

        # If property is specified, check if it matches
        if not isinstance(property_arg, Var):
            for prop in properties:
                if engine.unify(property_arg, prop):
                    return True
            return False
        else:
            # For now, unify with the first property
            # A proper implementation would need to support backtracking
            if properties:
                return engine.unify(property_arg, properties[0])
            return False
    else:
        # If stream not specified, enumerate streams
        # For now, just fail - proper implementation needs backtracking
        return False


def builtin_stream_properties(engine, args: tuple) -> bool:
    """$stream_properties(+Stream, -Properties).

    Internal builtin to get all properties as a list.
    This supports the Prolog-level stream_property/2 predicate.
    """
    if len(args) != 2:
        return False

    stream_arg = deref_term(engine, args[0])
    properties_arg = deref_term(engine, args[1])

    # Stream must be specified
    if not isinstance(stream_arg, Atom):
        return False

    manager = get_stream_manager()
    stream_id = stream_arg.name

    # Check if stream exists
    if stream_id not in manager.streams:
        return False

    # Get all properties as a list
    properties = manager.get_stream_properties(stream_id)

    # Convert to Prolog list
    if properties:
        # Build proper list from properties
        result = PrologList(items=tuple(properties), tail=Atom("[]"))
    else:
        result = Atom("[]")

    return engine.unify(properties_arg, result)
