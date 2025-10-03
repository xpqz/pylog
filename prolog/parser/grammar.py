"""
Grammar module for Stage 1.5 - exports the grammar text for tests.
"""

import pathlib

# Path to the grammar file
GRAMMAR_PATH = pathlib.Path(__file__).parent / "grammar.lark"

# Export the grammar text
grammar_text = GRAMMAR_PATH.read_text()


def get_grammar_text() -> str:
    """Get the grammar text."""
    return grammar_text


# Also export the path for reference
__all__ = ["grammar_text", "GRAMMAR_PATH", "get_grammar_text"]
