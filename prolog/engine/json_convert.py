"""
JSON ↔ Prolog term conversion functionality.

This module provides the core infrastructure for bidirectional conversion
between JSON objects and Prolog terms using both classical and modern dict
representations, following SWI-Prolog's dual representation approach.
"""

import math
from typing import Any, Optional, Mapping

from prolog.ast.terms import Term, Atom, Int, Float, Struct, List, PrologDict

# Constants for JSON representation modes
CLASSIC_MODE = "classic"
DICT_MODE = "dict"

# Configurable constant representations
DEFAULT_CLASSIC_CONSTANTS = {
    "null": Struct("@", (Atom("null"),)),
    "true": Struct("@", (Atom("true"),)),
    "false": Struct("@", (Atom("false"),)),
}

DEFAULT_DICT_CONSTANTS = {
    "null": Atom("null"),
    "true": Atom("true"),
    "false": Atom("false"),
}


def json_to_prolog(
    json_obj: Any,
    mode: str = CLASSIC_MODE,
    constants: Optional[Mapping[str, Term]] = None,
) -> Term:
    """Convert JSON object to Prolog term.

    Args:
        json_obj: JSON value to convert (dict, list, str, int, float, bool, None)
        mode: Conversion mode - either CLASSIC_MODE or DICT_MODE
        constants: Optional custom constant mappings for null/true/false
                  (accepts any Mapping, not just Dict)

    Returns:
        Corresponding Prolog term

    Raises:
        ValueError: If mode is invalid, JSON contains unsupported values,
                   or JSON object has non-string keys

    Note:
        In DICT_MODE, JSON strings "true"/"false" are indistinguishable from
        JSON booleans true/false after conversion (both become Atom("true")/Atom("false")).
    """
    if mode not in (CLASSIC_MODE, DICT_MODE):
        raise ValueError(f"Unknown mode: {mode}")

    # Use default constants if none provided
    if constants is None:
        constants = (
            DEFAULT_CLASSIC_CONSTANTS
            if mode == CLASSIC_MODE
            else DEFAULT_DICT_CONSTANTS
        )

    return _json_to_prolog_recursive(json_obj, mode, constants)


def prolog_to_json(
    term: Term, mode: str = CLASSIC_MODE, constants: Optional[Mapping[str, Term]] = None
) -> Any:
    """Convert Prolog term to JSON object.

    Args:
        term: Prolog term to convert
        mode: Conversion mode - either CLASSIC_MODE or DICT_MODE
        constants: Optional custom constant mappings for null/true/false
                  (accepts any Mapping, not just Dict)

    Returns:
        Corresponding JSON value

    Raises:
        ValueError: If mode is invalid, term cannot be converted to JSON,
                   or term contains invalid structures

    Note:
        PrologDict with Int keys converts them to string keys in JSON.
        This is asymmetric with JSON→Prolog which rejects non-string keys.
    """
    if mode not in (CLASSIC_MODE, DICT_MODE):
        raise ValueError(f"Unknown mode: {mode}")

    # Use default constants if none provided
    if constants is None:
        constants = (
            DEFAULT_CLASSIC_CONSTANTS
            if mode == CLASSIC_MODE
            else DEFAULT_DICT_CONSTANTS
        )

    return _prolog_to_json_recursive(term, mode, constants)


def _json_to_prolog_recursive(
    json_obj: Any, mode: str, constants: Mapping[str, Term]
) -> Term:
    """Recursively convert JSON object to Prolog term."""

    # Handle None (null)
    if json_obj is None:
        return constants["null"]

    # Handle booleans
    if isinstance(json_obj, bool):
        return constants["true"] if json_obj else constants["false"]

    # Handle numbers
    if isinstance(json_obj, int):
        return Int(json_obj)

    if isinstance(json_obj, float):
        # Check for special float values that JSON doesn't support
        if math.isinf(json_obj) or math.isnan(json_obj):
            raise ValueError(f"Cannot convert special float value {json_obj} to JSON")
        return Float(json_obj)

    # Handle strings
    if isinstance(json_obj, str):
        return Atom(json_obj)

    # Handle arrays/lists
    if isinstance(json_obj, list):
        converted_items = tuple(
            _json_to_prolog_recursive(item, mode, constants) for item in json_obj
        )
        return List(converted_items)

    # Handle objects/dicts
    if isinstance(json_obj, dict):
        # Validate that all keys are strings (JSON requirement)
        for key in json_obj.keys():
            if not isinstance(key, str):
                raise ValueError("JSON object keys must be strings")

        # Sort keys for canonical representation
        sorted_items = sorted(json_obj.items())

        if mode == CLASSIC_MODE:
            # Convert to json(...) struct with key-value pairs
            pairs = []
            for key, value in sorted_items:
                key_term = Atom(key)
                value_term = _json_to_prolog_recursive(value, mode, constants)
                pair = Struct("=", (key_term, value_term))
                pairs.append(pair)

            return Struct("json", (List(tuple(pairs)),))

        else:  # DICT_MODE
            # Convert to PrologDict
            pairs = []
            for key, value in sorted_items:
                key_term = Atom(key)
                value_term = _json_to_prolog_recursive(value, mode, constants)
                pairs.append((key_term, value_term))

            return PrologDict(tuple(pairs))

    # Unsupported type
    raise ValueError(f"Cannot convert JSON type {type(json_obj)} to Prolog term")


def _prolog_to_json_recursive(
    term: Term, mode: str, constants: Mapping[str, Term]
) -> Any:
    """Recursively convert Prolog term to JSON object."""

    # Create reverse mapping for constants (term -> json_value)
    reverse_constants = {v: k for k, v in constants.items()}
    json_constants = {"null": None, "true": True, "false": False}

    # Check if term matches a constant
    if term in reverse_constants:
        const_name = reverse_constants[term]
        return json_constants[const_name]

    # Handle basic types
    if isinstance(term, Int):
        return term.value

    if isinstance(term, Float):
        # Check for special float values
        if math.isinf(term.value) or math.isnan(term.value):
            raise ValueError(f"Cannot convert special float value {term.value} to JSON")
        return term.value

    if isinstance(term, Atom):
        # Check if this atom represents a constant in the current mode
        if term in reverse_constants:
            const_name = reverse_constants[term]
            return json_constants[const_name]
        # Otherwise, treat as regular string
        return term.name

    # Handle lists
    if isinstance(term, List):
        # Check for proper list (empty tail)
        if term.tail != Atom("[]"):
            raise ValueError("Cannot convert list with non-empty tail to JSON")

        return [_prolog_to_json_recursive(item, mode, constants) for item in term.items]

    # Handle PrologDict (always converts to JSON object)
    if isinstance(term, PrologDict):
        result = {}
        for key_term, value_term in term.pairs:
            # Validate key type
            if isinstance(key_term, Atom):
                key_str = key_term.name
            elif isinstance(key_term, Int):
                key_str = str(key_term.value)
            else:
                raise ValueError(f"Invalid dict key type: {type(key_term)}")

            result[key_str] = _prolog_to_json_recursive(value_term, mode, constants)

        return result

    # Handle classic json(...) structures
    if isinstance(term, Struct) and term.functor == "json":
        if mode != CLASSIC_MODE:
            raise ValueError("json/1 structures only supported in classic mode")

        if len(term.args) != 1:
            raise ValueError("Invalid json structure: wrong arity")

        list_arg = term.args[0]
        if not isinstance(list_arg, List):
            raise ValueError("Invalid json structure: argument must be a list")

        result = {}
        seen_keys = set()

        for pair in list_arg.items:
            if (
                not isinstance(pair, Struct)
                or pair.functor != "="
                or len(pair.args) != 2
            ):
                raise ValueError("Invalid json structure: malformed key-value pair")

            key_term, value_term = pair.args

            if not isinstance(key_term, Atom):
                raise ValueError("Invalid json structure: keys must be atoms")

            key_str = key_term.name

            # Check for duplicate keys
            if key_str in seen_keys:
                raise ValueError(f"Duplicate key '{key_str}' in classic json structure")
            seen_keys.add(key_str)

            result[key_str] = _prolog_to_json_recursive(value_term, mode, constants)

        return result

    # Handle constant structures in classic mode
    if isinstance(term, Struct) and mode == CLASSIC_MODE:
        if term in reverse_constants:
            const_name = reverse_constants[term]
            return json_constants[const_name]

    # Unsupported term type
    raise ValueError(f"Cannot convert Prolog term {type(term)} to JSON")
