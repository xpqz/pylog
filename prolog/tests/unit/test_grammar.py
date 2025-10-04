"""Tests for the grammar module."""

from prolog.parser.grammar import get_grammar_text, grammar_text, GRAMMAR_PATH


def test_get_grammar_text():
    """Test that get_grammar_text returns the grammar."""
    text = get_grammar_text()
    assert isinstance(text, str)
    assert len(text) > 0
    assert "start:" in text  # Grammar should have a start rule
    assert "term:" in text  # Grammar should define terms


def test_grammar_text_export():
    """Test that grammar_text is exported."""
    assert isinstance(grammar_text, str)
    assert len(grammar_text) > 0
    assert grammar_text == get_grammar_text()


def test_grammar_path():
    """Test that GRAMMAR_PATH points to the grammar file."""
    assert GRAMMAR_PATH.exists()
    assert GRAMMAR_PATH.name == "grammar.lark"
    assert GRAMMAR_PATH.read_text() == get_grammar_text()
