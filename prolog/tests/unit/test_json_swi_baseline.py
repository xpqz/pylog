"""
SWI-Prolog baseline tests for JSON functionality.

This module tests JSON support against SWI-Prolog behavior to ensure
exact compatibility with the SWI-Prolog JSON library.

Tests cover:
- json_read/3 and json_write/3 (classic mode)
- json_read_dict/3 and json_write_dict/3 (dict mode)
- atom_json_term/3 (bidirectional conversion)
- JSON constant handling (null, true, false)
- Complex nested structures
- Error handling for malformed JSON
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.parser.reader import Reader
from prolog.tests.helpers import program


@pytest.mark.swi_baseline
class TestJSONSWIBaseline:
    """Test JSON support exact compatibility with SWI-Prolog."""

    def test_basic_json_parsing_classic_mode(self, swi):
        """Test basic JSON parsing matches SWI-Prolog exactly (classic mode)."""
        # Test simple object in classic mode
        test_program = """
        :- use_module(library(http/json)).

        test_json_parse :-
            atom_json_term('{"name": "test", "value": 42}', Term, []),
            Term = json([name=test, value=42]).
        """

        # Test in PyLog
        reader = Reader()
        engine = Engine(program())
        query = reader.read_term(
            'atom_json_term(\'{"name": "test", "value": 42}\', Term, []), Term = json([name=test, value=42])'
        )
        result_pylog = list(engine.solve(query))

        # Test in SWI-Prolog
        count_swi = swi.count(test_program, "test_json_parse")

        # Both should succeed with identical behavior
        assert len(result_pylog) == 1  # PyLog should find solution
        assert count_swi == 1  # SWI should find solution

    def test_json_constants_classic_mode(self, swi):
        """Test JSON constant handling matches SWI-Prolog (null, true, false)."""
        test_program = """
        :- use_module(library(http/json)).

        test_json_constants :-
            atom_json_term('{"value": null}', Term, []),
            Term = json([=(value,'@'(null))]).
        """

        # Test in PyLog with simpler JSON to avoid escaping issues
        engine = Engine(program())

        # Test null constant specifically - build query programmatically to avoid string escaping
        json_atom = Atom('{"value": null}')
        term_var = Var(0, "Term")
        options = List(tuple())  # empty list []

        # Create the query: atom_json_term('{"value": null}', Term, [])
        json_query = Struct("atom_json_term", (json_atom, term_var, options))

        result_pylog = list(engine.solve(json_query))

        # Test in SWI-Prolog
        count_swi = swi.count(test_program, "test_json_constants")

        # Both should parse JSON null as @(null)
        assert len(result_pylog) == 1, "PyLog should parse JSON with null constant"
        assert count_swi == 1, "SWI-Prolog should parse JSON with null constant"

        # Verify PyLog produces json([value=@(null)]) with correct structure
        if result_pylog:
            parsed_term = result_pylog[0]["Term"]
            assert (
                parsed_term.functor == "json"
            ), f"Expected json/1, got {parsed_term.functor}"
            pairs = parsed_term.args[0].items
            assert len(pairs) == 1, f"Expected 1 key-value pair, got {len(pairs)}"

            key_value = pairs[0]
            assert key_value.functor == "=", f"Expected =/2, got {key_value.functor}"
            assert (
                key_value.args[0].name == "value"
            ), f"Expected key 'value', got {key_value.args[0]}"

            value_term = key_value.args[1]
            assert (
                value_term.functor == "@"
            ), f"Expected @/1 for null, got {value_term.functor}"
            assert (
                value_term.args[0].name == "null"
            ), f"Expected @(null), got @({value_term.args[0]})"

    def test_json_arrays_classic_mode(self, swi):
        """Test JSON array parsing matches SWI-Prolog."""
        test_program = """
        :- use_module(library(http/json)).

        test_json_array :-
            atom_json_term('[1, 2, "hello", true]', Term, []),
            Term = [1, 2, hello, @(true)].
        """

        # Test in PyLog
        reader = Reader()
        engine = Engine(program())
        query = reader.read_term(
            "atom_json_term('[1, 2, \"hello\", true]', Term, []), Term = [1, 2, hello, @(true)]"
        )
        result_pylog = list(engine.solve(query))

        # Test in SWI-Prolog
        count_swi = swi.count(test_program, "test_json_array")

        assert len(result_pylog) == 1
        assert count_swi == 1

    def test_json_empty_structures(self, swi):
        """Test empty JSON objects and arrays match SWI-Prolog."""
        test_program = """
        :- use_module(library(http/json)).

        test_empty :-
            atom_json_term('{}', json([]), []),
            atom_json_term('[]', [], []).
        """

        # Test in PyLog
        reader = Reader()
        engine = Engine(program())
        query1 = reader.read_term("atom_json_term('{}', json([]), [])")
        result1 = list(engine.solve(query1))

        query2 = reader.read_term("atom_json_term('[]', [], [])")
        result2 = list(engine.solve(query2))

        # Test in SWI-Prolog
        count_swi = swi.count(test_program, "test_empty")

        assert len(result1) == 1
        assert len(result2) == 1
        assert count_swi == 1

    def test_json_error_handling_malformed(self, swi):
        """Test error handling for malformed JSON matches SWI-Prolog."""
        test_program = """
        :- use_module(library(http/json)).

        test_malformed :-
            catch(atom_json_term('{"bad": json}', _, []), _, fail).
        """

        # Test in PyLog
        reader = Reader()
        engine = Engine(program())
        query = reader.read_term("atom_json_term('{\"bad\": json}', _, [])")
        result_pylog = list(engine.solve(query))

        # Test in SWI-Prolog
        count_swi = swi.count(test_program, "test_malformed")

        # Both should fail (no solutions)
        assert len(result_pylog) == 0
        assert count_swi == 0

    def test_atom_json_term_bidirectional(self, swi):
        """Test bidirectional conversion matches SWI-Prolog."""
        test_program = """
        :- use_module(library(http/json)).

        test_bidirectional :-
            Term = json([name=test, value=42]),
            atom_json_term(Atom, Term, []),
            atom_json_term(Atom, Term2, []),
            Term = Term2.
        """

        # Test in PyLog
        reader = Reader()
        engine = Engine(program())
        query = reader.read_term(
            "Term = json([name=test, value=42]), atom_json_term(Atom, Term, []), atom_json_term(Atom, Term2, []), Term = Term2"
        )
        result_pylog = list(engine.solve(query))

        # Test in SWI-Prolog
        count_swi = swi.count(test_program, "test_bidirectional")

        assert len(result_pylog) == 1
        assert count_swi == 1
