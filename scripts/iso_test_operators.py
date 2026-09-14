"""
Operator context manager for ISO test suite.

Temporarily adds ISO test operators to the parser operator table
without polluting global state.
"""

from contextlib import contextmanager
from typing import List
from prolog.parser.operators import _OPERATOR_TABLE_MUTABLE
from prolog.parser.reader import Reader
from prolog.ast.clauses import Clause


# ISO test operators with precedence matching harness.pl
# Precedence 1110 ensures patterns like (X=1;X=2) should_give ...
# group the disjunction (;/2 at 1100) before the test operator
# Note: underscore in operator names (should_fail not "should fail")
ISO_TEST_OPERATORS = {
    ("fixme", "prefix"): (1200, "fy", "fixme"),
    ("should_fail", "postfix"): (1110, "xf", "should_fail"),
    ("should_give", "infix"): (1110, "xfx", "should_give"),
    ("should_throw", "infix"): (1110, "xfx", "should_throw"),
}


@contextmanager
def iso_test_operators():
    """
    Context manager to temporarily add ISO test operators.

    Usage:
        with iso_test_operators():
            clauses = parse_iso_tests(iso_test_content)
            # ISO operators available here

    Ensures operators are removed even if parsing fails.
    """
    # Save original state
    original_keys = set(_OPERATOR_TABLE_MUTABLE.keys())

    try:
        # Add ISO test operators
        _OPERATOR_TABLE_MUTABLE.update(ISO_TEST_OPERATORS)
        yield
    finally:
        # Remove only the operators we added
        for key in ISO_TEST_OPERATORS.keys():
            if key in _OPERATOR_TABLE_MUTABLE:
                del _OPERATOR_TABLE_MUTABLE[key]

        # Verify we're back to original state
        assert set(_OPERATOR_TABLE_MUTABLE.keys()) == original_keys


def parse_iso_tests(text: str) -> List[Clause]:
    """
    Parse ISO test patterns with ISO test operators enabled.

    Must be called within iso_test_operators() context or operators
    must be pre-added to the global table.

    Args:
        text: ISO test pattern text

    Returns:
        List of Clause objects where heads contain test patterns

    Example:
        with iso_test_operators():
            clauses = parse_iso_tests("call(fail) should_fail.")
    """
    # Create Reader AFTER operators are added (within context)
    # This ensures the Tokenizer sees the ISO operators
    reader = Reader()
    return reader.read_program(text)
