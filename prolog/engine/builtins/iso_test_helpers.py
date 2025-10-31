"""ISO test helper predicates.

This module provides auxiliary predicates used by the ISO Prolog test suite:
- iso_test_ensure_loaded/1: Load a Prolog source file
- iso_test_variant/2: Check if two terms are variants (same structure, different variables)
- iso_test_os/1: Unify with current operating system identifier
- iso_test_non_repositionable_stream/1: Create a non-seekable stream
- iso_test_same_members/2: Check if two lists have the same set of members

Additionally provides utility predicates needed by the helpers:
- subsumes_term/2: Term subsumption check
- sort/2: Sort and deduplicate a list
"""

from typing import Dict, Tuple, Callable
import platform
import io
from pathlib import Path

from prolog.ast.terms import Term, Atom, Var, Struct, List as PrologList, Int
from prolog.unify.unify import unify
from prolog.engine.trail_adapter import TrailAdapter
from prolog.engine.runtime import Trail

__all__ = [
    "register",
    "builtin_iso_test_ensure_loaded",
    "builtin_iso_test_variant",
    "builtin_iso_test_os",
    "builtin_iso_test_non_repositionable_stream",
    "builtin_iso_test_same_members",
    "builtin_subsumes_term",
    "builtin_sort",
]


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register ISO test helper builtin predicates.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    # ISO test helper predicates
    registry[("iso_test_ensure_loaded", 1)] = builtin_iso_test_ensure_loaded
    registry[("iso_test_variant", 2)] = builtin_iso_test_variant
    registry[("iso_test_os", 1)] = builtin_iso_test_os
    registry[("iso_test_non_repositionable_stream", 1)] = (
        builtin_iso_test_non_repositionable_stream
    )
    registry[("iso_test_same_members", 2)] = builtin_iso_test_same_members

    # Utility predicates needed by the helpers
    registry[("subsumes_term", 2)] = builtin_subsumes_term
    registry[("sort", 2)] = builtin_sort


def builtin_iso_test_ensure_loaded(engine, args: tuple) -> bool:
    """iso_test_ensure_loaded(+File) - Load a Prolog source file.

    Loads clauses from the specified file into the current engine.
    Similar to consult/1 but specifically for ISO test suite usage.

    Args:
        engine: The Prolog engine
        args: Tuple containing the file path (as an Atom)

    Returns:
        True if file loaded successfully, False otherwise
    """
    if len(args) != 1:
        return False

    file_arg = args[0]

    # Dereference if variable
    if isinstance(file_arg, Var):
        result = engine.store.deref(file_arg.id)
        if result[0] == "BOUND":
            file_arg = result[2]
        else:
            return False

    # File path must be an atom
    if not isinstance(file_arg, Atom):
        return False

    file_path = Path(file_arg.name)

    # Check if file exists
    if not file_path.exists():
        return False

    try:
        # Read the file
        with open(file_path, "r") as f:
            content = f.read()

        # Load into engine using consult_string
        engine.consult_string(content)
        return True

    except Exception:
        # Any error during loading fails the predicate
        return False


def builtin_iso_test_variant(engine, args: tuple) -> bool:
    """iso_test_variant(?Term1, ?Term2) - Check if two terms are variants.

    Two terms are variants if they have the same structure with variables
    in corresponding positions. Two terms are variants iff they're structurally
    identical modulo consistent variable renaming.

    Args:
        engine: The Prolog engine
        args: Tuple containing two terms to compare

    Returns:
        True if terms are variants, False otherwise
    """
    if len(args) != 2:
        return False

    term1, term2 = args

    # Dereference both terms
    term1 = _deref_term(engine, term1)
    term2 = _deref_term(engine, term2)

    # Check if terms are variants
    return _is_variant(term1, term2, engine, {}, {})


def builtin_iso_test_os(engine, args: tuple) -> bool:
    """iso_test_os(?OS) - Unify with current operating system identifier.

    Returns an atom identifying the current operating system:
    - 'darwin' for macOS
    - 'windows' for Windows
    - 'linux' for Linux
    - 'unix' for other Unix-like systems

    Args:
        engine: The Prolog engine
        args: Tuple containing the OS variable/atom

    Returns:
        True if unification succeeds, False otherwise
    """
    if len(args) != 1:
        return False

    os_arg = args[0]
    trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

    # Determine the OS
    system = platform.system().lower()

    if system == "darwin":
        os_atom = Atom("darwin")
    elif system == "windows":
        os_atom = Atom("windows")
    elif system == "linux":
        os_atom = Atom("linux")
    else:
        # Default to 'unix' for other Unix-like systems
        os_atom = Atom("unix")

    # Unify with the argument
    return unify(os_arg, os_atom, engine.store, trail_adapter, engine.occurs_check)


def builtin_iso_test_non_repositionable_stream(engine, args: tuple) -> bool:
    """iso_test_non_repositionable_stream(-Stream) - Create a non-repositionable stream.

    Creates a stream that cannot be repositioned (seeked). This is typically
    used for testing stream positioning errors. The stream is created from a
    StringIO object which is not seekable in the same way as file streams.

    Args:
        engine: The Prolog engine
        args: Tuple containing the stream variable

    Returns:
        True if stream created and unified successfully, False otherwise
    """
    if len(args) != 1:
        return False

    stream_arg = args[0]
    trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

    # Create a non-repositionable stream using StringIO
    # This creates a stream from a string buffer which is non-seekable
    # in the context of Prolog stream positioning
    content = "abc"
    stream = io.StringIO(content)

    # Wrap the stream in a Prolog term
    # For now, we use a structure to represent the stream
    # Format: stream(id, io_object)
    # We'll use a unique ID based on object id
    stream_id = id(stream)
    stream_term = Struct("$stream", (Int(stream_id),))

    # Store the stream in the engine's stream table
    # (this assumes engine has a stream management system)
    # For basic implementation, we just create the term
    # A full implementation would register this stream properly

    # Unify with the argument
    return unify(
        stream_arg, stream_term, engine.store, trail_adapter, engine.occurs_check
    )


def builtin_iso_test_same_members(engine, args: tuple) -> bool:
    """iso_test_same_members(?List1, ?List2) - Check if lists have same members.

    Two lists have the same members if they contain the same elements after
    sorting and deduplication. Order and duplicates are ignored.
    Implemented as: sort(List1, S1), sort(List2, S2), S1 == S2.

    Args:
        engine: The Prolog engine
        args: Tuple containing two lists to compare

    Returns:
        True if lists have same members, False otherwise
    """
    if len(args) != 2:
        return False

    list1, list2 = args

    # Dereference both lists
    list1 = _deref_term(engine, list1)
    list2 = _deref_term(engine, list2)

    # Both must be lists
    if not isinstance(list1, PrologList) or not isinstance(list2, PrologList):
        return False

    # Sort both lists (this also removes duplicates)
    sorted1 = _sort_list(engine, list1)
    sorted2 = _sort_list(engine, list2)

    if sorted1 is None or sorted2 is None:
        return False

    # Check structural equality (==)
    return _structural_equal(sorted1, sorted2)


def builtin_subsumes_term(engine, args: tuple) -> bool:
    """subsumes_term(@General, @Specific) - Check if General subsumes Specific.

    General subsumes Specific if there exists a substitution that makes
    General identical to Specific, without binding variables in Specific.

    This is used to check if one term is more general than another.

    Args:
        engine: The Prolog engine
        args: Tuple containing (General, Specific)

    Returns:
        True if General subsumes Specific, False otherwise
    """
    if len(args) != 2:
        return False

    general, specific = args

    # Dereference both terms
    general = _deref_term(engine, general)
    specific = _deref_term(engine, specific)

    return _subsumes_term_check(engine, general, specific)


def builtin_sort(engine, args: tuple) -> bool:
    """sort(+List, ?Sorted) - Sort list and remove duplicates.

    Sorts the list in standard order and removes duplicate elements.
    Standard order is defined as:
    1. Variables < Numbers < Atoms < Compound terms
    2. Within each category, use natural ordering

    Args:
        engine: The Prolog engine
        args: Tuple containing (List, Sorted)

    Returns:
        True if sorting and unification succeed, False otherwise
    """
    if len(args) != 2:
        return False

    list_arg, sorted_arg = args
    trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

    # Dereference list
    list_arg = _deref_term(engine, list_arg)

    # Must be a list
    if not isinstance(list_arg, PrologList):
        return False

    # Sort the list
    sorted_list = _sort_list(engine, list_arg)
    if sorted_list is None:
        return False

    # Unify with result
    return unify(
        sorted_arg, sorted_list, engine.store, trail_adapter, engine.occurs_check
    )


# Helper functions


def _deref_term(engine, term: Term) -> Term:
    """Dereference a term if it's a variable.

    Args:
        engine: The Prolog engine
        term: Term to dereference

    Returns:
        Dereferenced term
    """
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            return result[2]
    return term


def _is_variant(
    term1: Term, term2: Term, engine, var_map1: dict, var_map2: dict
) -> bool:
    """Check if two terms are variants.

    Two terms are variants if they have the same structure and variables
    map consistently between them.

    Args:
        term1: First term
        term2: Second term
        engine: The Prolog engine
        var_map1: Mapping from term1 variables to canonical IDs
        var_map2: Mapping from term2 variables to canonical IDs

    Returns:
        True if terms are variants
    """
    # Dereference
    term1 = _deref_term(engine, term1)
    term2 = _deref_term(engine, term2)

    # Both variables - check consistent renaming
    if isinstance(term1, Var) and isinstance(term2, Var):
        # Get or create canonical ID for term1's variable
        if term1.id not in var_map1:
            var_map1[term1.id] = len(var_map1)
        # Get or create canonical ID for term2's variable
        if term2.id not in var_map2:
            var_map2[term2.id] = len(var_map2)
        # Variables must map to same canonical position
        return var_map1[term1.id] == var_map2[term2.id]

    # One variable, one non-variable - not variants
    if isinstance(term1, Var) or isinstance(term2, Var):
        return False

    # Both atoms - must be identical
    if isinstance(term1, Atom) and isinstance(term2, Atom):
        return term1.name == term2.name

    # Both integers - must be identical
    if isinstance(term1, Int) and isinstance(term2, Int):
        return term1.value == term2.value

    # Both structures - must have same functor/arity and variant args
    if isinstance(term1, Struct) and isinstance(term2, Struct):
        if term1.functor != term2.functor:
            return False
        if len(term1.args) != len(term2.args):
            return False
        return all(
            _is_variant(a1, a2, engine, var_map1, var_map2)
            for a1, a2 in zip(term1.args, term2.args)
        )

    # Both lists - must have same length and variant elements
    if isinstance(term1, PrologList) and isinstance(term2, PrologList):
        if len(term1.items) != len(term2.items):
            return False
        if not all(
            _is_variant(i1, i2, engine, var_map1, var_map2)
            for i1, i2 in zip(term1.items, term2.items)
        ):
            return False
        return _is_variant(term1.tail, term2.tail, engine, var_map1, var_map2)

    # Different types - not variants
    return False


def _collect_vars(term: Term, engine) -> set:
    """Collect all variable IDs in a term.

    Args:
        engine: The Prolog engine
        term: Term to collect variables from

    Returns:
        Set of variable IDs
    """
    vars = set()
    term = _deref_term(engine, term)

    if isinstance(term, Var):
        vars.add(term.id)
    elif isinstance(term, Struct):
        for arg in term.args:
            vars.update(_collect_vars(arg, engine))
    elif isinstance(term, PrologList):
        for item in term.items:
            vars.update(_collect_vars(item, engine))
        vars.update(_collect_vars(term.tail, engine))

    return vars


def _subsumes_term_check(engine, general: Term, specific: Term) -> bool:
    """Check if general subsumes specific.

    General subsumes Specific if there exists a substitution θ such that
    General·θ = Specific, where only variables in General are substituted.

    Implementation: We rename variables in General to fresh ones, then try
    to unify with Specific. If unification succeeds without binding any
    variables from Specific, then General subsumes Specific.

    Args:
        engine: The Prolog engine
        general: The more general term
        specific: The more specific term

    Returns:
        True if general subsumes specific
    """
    # Collect all variables in specific (these should NOT be bound during unification)
    specific_vars = _collect_vars(specific, engine)

    # Create a fresh renaming for general's variables
    var_map = {}
    general_renamed = _rename_vars(general, var_map, engine)

    # Save the current state of variables in specific
    specific_var_states = {}
    for var_id in specific_vars:
        result = engine.store.deref(var_id)
        specific_var_states[var_id] = result

    # Try to unify in the current store
    # We'll use a temporary trail to undo any bindings
    temp_trail = Trail()
    temp_adapter = TrailAdapter(temp_trail, engine=engine, store=engine.store)

    # Attempt unification
    result = unify(general_renamed, specific, engine.store, temp_adapter, False)

    # Check if any variables from specific were bound
    if result:
        for var_id in specific_vars:
            new_state = engine.store.deref(var_id)
            if new_state != specific_var_states[var_id]:
                # A variable from specific was bound - not a valid subsumption
                result = False
                break

    # Undo all bindings
    temp_trail.unwind_to(0, engine.store)

    return result


def _rename_vars(term: Term, var_map: dict, engine) -> Term:
    """Rename all variables in a term using a mapping.

    Args:
        term: Term to rename
        var_map: Dictionary mapping old var IDs to new var IDs
        engine: The Prolog engine

    Returns:
        Term with renamed variables
    """
    term = _deref_term(engine, term)

    if isinstance(term, Var):
        if term.id not in var_map:
            # Create a new variable ID
            new_id = engine.store.new_var()
            var_map[term.id] = new_id
        return Var(var_map[term.id], term.hint)

    elif isinstance(term, Struct):
        new_args = tuple(_rename_vars(arg, var_map, engine) for arg in term.args)
        return Struct(term.functor, new_args)

    elif isinstance(term, PrologList):
        new_items = tuple(_rename_vars(item, var_map, engine) for item in term.items)
        new_tail = _rename_vars(term.tail, var_map, engine)
        return PrologList(new_items, new_tail)

    else:
        # Atoms, Ints, Floats - return as-is
        return term


def _copy_to_store(term: Term, store, engine) -> Term:
    """Copy a term into a different store, creating fresh variables.

    Args:
        term: Term to copy
        store: Destination store
        engine: The Prolog engine

    Returns:
        Copy of term with variables in the new store
    """
    var_map = {}
    return _copy_to_store_helper(term, store, engine, var_map)


def _copy_to_store_helper(term: Term, store, engine, var_map: dict) -> Term:
    """Helper for copying terms to a new store."""
    term = _deref_term(engine, term)

    if isinstance(term, Var):
        if term.id not in var_map:
            new_id = store.new_var()
            var_map[term.id] = new_id
        return Var(var_map[term.id], term.hint)

    elif isinstance(term, Struct):
        new_args = tuple(
            _copy_to_store_helper(arg, store, engine, var_map) for arg in term.args
        )
        return Struct(term.functor, new_args)

    elif isinstance(term, PrologList):
        new_items = tuple(
            _copy_to_store_helper(item, store, engine, var_map) for item in term.items
        )
        new_tail = _copy_to_store_helper(term.tail, store, engine, var_map)
        return PrologList(new_items, new_tail)

    else:
        return term


def _sort_list(engine, prolog_list: PrologList) -> PrologList:
    """Sort a Prolog list and remove duplicates.

    Args:
        engine: The Prolog engine
        prolog_list: List to sort

    Returns:
        Sorted list with duplicates removed, or None on error
    """
    # Convert to Python list
    python_list = []
    current = prolog_list

    while True:
        current = _deref_term(engine, current)

        if isinstance(current, Atom) and current.name == "[]":
            # End of list
            break

        if not isinstance(current, PrologList):
            # Invalid list
            return None

        # Add items
        for item in current.items:
            item = _deref_term(engine, item)
            python_list.append(item)

        # Move to tail
        current = current.tail

    # Sort using standard term ordering
    try:
        sorted_list = sorted(python_list, key=lambda t: _term_sort_key(t))

        # Remove duplicates while preserving order
        deduplicated = []

        for term in sorted_list:
            # Use structural equality for deduplication
            is_duplicate = False
            for seen_term in deduplicated:
                if _structural_equal(term, seen_term):
                    is_duplicate = True
                    break

            if not is_duplicate:
                deduplicated.append(term)

        # Convert back to Prolog list
        return _python_list_to_prolog(deduplicated)

    except Exception:
        return None


def _term_sort_key(term: Term) -> tuple:
    """Generate a sort key for standard term ordering.

    Standard order: Variables < Numbers < Atoms < Compound terms

    Args:
        term: Term to generate key for

    Returns:
        Tuple that can be used for sorting
    """
    if isinstance(term, Var):
        return (0, term.id)
    elif isinstance(term, Int):
        return (1, 0, term.value)
    elif isinstance(term, Atom):
        return (2, term.name)
    elif isinstance(term, Struct):
        # Sort by arity, then functor, then args
        return (
            3,
            len(term.args),
            term.functor,
            tuple(_term_sort_key(arg) for arg in term.args),
        )
    elif isinstance(term, PrologList):
        # Lists are compound terms
        return (
            3,
            len(term.items) + 1,
            ".",
            tuple(_term_sort_key(item) for item in term.items),
        )
    else:
        # Fallback
        return (4, str(term))


def _python_list_to_prolog(items: list) -> PrologList:
    """Convert a Python list to a Prolog list.

    Args:
        items: Python list of terms

    Returns:
        Prolog list
    """
    if not items:
        return Atom("[]")

    # Build list from right to left
    result = Atom("[]")
    for item in reversed(items):
        result = PrologList((item,), result)

    return result


def _structural_equal(term1: Term, term2: Term) -> bool:
    """Check if two terms are structurally equal (==).

    Args:
        term1: First term
        term2: Second term

    Returns:
        True if structurally equal
    """
    # Same type check
    if type(term1) is not type(term2):
        return False

    if isinstance(term1, Var):
        return term1.id == term2.id

    elif isinstance(term1, Atom):
        return term1.name == term2.name

    elif isinstance(term1, Int):
        return term1.value == term2.value

    elif isinstance(term1, Struct):
        if term1.functor != term2.functor:
            return False
        if len(term1.args) != len(term2.args):
            return False
        return all(_structural_equal(a1, a2) for a1, a2 in zip(term1.args, term2.args))

    elif isinstance(term1, PrologList):
        if len(term1.items) != len(term2.items):
            return False
        if not all(
            _structural_equal(i1, i2) for i1, i2 in zip(term1.items, term2.items)
        ):
            return False
        return _structural_equal(term1.tail, term2.tail)

    else:
        # Fallback to Python equality
        return term1 == term2
