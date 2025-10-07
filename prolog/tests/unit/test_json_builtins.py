"""
Tests for JSON I/O builtin predicates (Phase 2).

This module tests the stream-based JSON I/O builtins that work with both
classical json(...) representation and modern PrologDict representation:
- json_read/3 - read JSON from stream (classic mode)
- json_write/3 - write JSON to stream (classic mode)
- json_read_dict/3 - read JSON as dict (dict mode)
- json_write_dict/3 - write dict as JSON (dict mode)
- atom_json_term/3 - bidirectional atom ↔ term conversion
"""

import json
import tempfile
import os
from io import StringIO

import pytest
from prolog.ast.terms import Atom, Int, Float, Struct, List, PrologDict, Var
from prolog.engine.engine import Engine
from prolog.tests.helpers import program


class TestJSONReadBuiltin:
    """Test json_read/3 builtin (classic mode)."""

    def test_json_read_simple_object_from_string(self):
        """Test json_read/3 with simple JSON object from string stream."""
        engine = Engine(program())

        # Create string stream with JSON
        json_str = '{"name": "test", "value": 42}'
        stream = StringIO(json_str)

        # Variables for unification
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())  # Empty options list

        # Call json_read(Stream, Term, Options)
        result = engine._builtin_json_read((stream, term_var, options))

        # Should succeed
        assert result is True

        # Term should be bound to json([name-test, value-42])
        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, Struct)
        assert bound_term.functor == "json"
        assert len(bound_term.args) == 1

        pairs_list = bound_term.args[0]
        assert isinstance(pairs_list, List)
        assert len(pairs_list.items) == 2

        # Check pairs are sorted: name-test, value-42
        pair1, pair2 = pairs_list.items
        assert isinstance(pair1, Struct) and pair1.functor == "="
        assert pair1.args == (Atom("name"), Atom("test"))
        assert isinstance(pair2, Struct) and pair2.functor == "="
        assert pair2.args == (Atom("value"), Int(42))

    def test_json_read_array_from_string(self):
        """Test json_read/3 with JSON array."""
        engine = Engine(program())

        json_str = '[1, 2, "hello"]'
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, List)
        assert len(bound_term.items) == 3
        assert bound_term.items == (Int(1), Int(2), Atom("hello"))

    def test_json_read_constants(self):
        """Test json_read/3 with JSON constants (null, true, false)."""
        engine = Engine(program())

        json_str = "[null, true, false]"
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, List)
        assert len(bound_term.items) == 3

        # Check constants use @ wrapper in classic mode
        null_term, true_term, false_term = bound_term.items
        assert null_term == Struct("@", (Atom("null"),))
        assert true_term == Struct("@", (Atom("true"),))
        assert false_term == Struct("@", (Atom("false"),))

    def test_json_read_empty_object(self):
        """Test json_read/3 with empty JSON object."""
        engine = Engine(program())

        json_str = "{}"
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, Struct)
        assert bound_term.functor == "json"
        assert bound_term.args == (List(()),)

    def test_json_read_nested_structure(self):
        """Test json_read/3 with nested JSON structure."""
        engine = Engine(program())

        json_str = '{"level1": {"level2": {"value": 42}}}'
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, Struct)
        assert bound_term.functor == "json"

        # Verify nested structure content explicitly
        pairs_list = bound_term.args[0]
        assert isinstance(pairs_list, List)
        assert len(pairs_list.items) == 1

        level1_pair = pairs_list.items[0]
        assert isinstance(level1_pair, Struct)
        assert level1_pair.functor == "="
        assert level1_pair.args[0] == Atom("level1")

        # The value should be another json(...) structure
        level1_value = level1_pair.args[1]
        assert isinstance(level1_value, Struct)
        assert level1_value.functor == "json"

        # Drill down to level2 and verify final value
        level1_pairs = level1_value.args[0]
        assert len(level1_pairs.items) == 1
        level2_pair = level1_pairs.items[0]
        assert level2_pair.args[0] == Atom("level2")

        level2_value = level2_pair.args[1]
        assert isinstance(level2_value, Struct)
        assert level2_value.functor == "json"

        # Final level should contain value: 42
        level2_pairs = level2_value.args[0]
        assert len(level2_pairs.items) == 1
        final_pair = level2_pairs.items[0]
        assert final_pair.args == (Atom("value"), Int(42))

    def test_json_read_malformed_json_fails(self):
        """Test json_read/3 fails with malformed JSON."""
        engine = Engine(program())

        json_str = '{"incomplete": '  # Malformed JSON
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        # Should fail with malformed JSON
        result = engine._builtin_json_read((stream, term_var, options))
        assert result is False

    def test_json_read_file_stream(self):
        """Test json_read/3 with file stream."""
        engine = Engine(program())

        # Create temporary file with JSON
        json_data = {"test": "file", "number": 123}
        with tempfile.NamedTemporaryFile(
            mode="w", delete=False, suffix=".json", encoding="utf-8"
        ) as f:
            json.dump(json_data, f)
            temp_path = f.name

        try:
            # Open file for reading
            with open(temp_path, "r", encoding="utf-8") as stream:
                term_var = Var(engine.store.new_var("Term"), "Term")
                options = List(())

                result = engine._builtin_json_read((stream, term_var, options))
                assert result is True

                deref_result = engine.store.deref(term_var.id)
                assert deref_result[0] == "BOUND"
                bound_term = deref_result[2]
                assert isinstance(bound_term, Struct)
                assert bound_term.functor == "json"
        finally:
            os.unlink(temp_path)

    def test_json_read_special_floats_fail(self):
        """Test json_read/3 with special float values (should fail)."""
        engine = Engine(program())

        # JSON doesn't support NaN or infinity
        for special_val in ["NaN", "Infinity", "-Infinity"]:
            json_str = f"[{special_val}]"
            stream = StringIO(json_str)

            term_var = Var(engine.store.new_var("Term"), "Term")
            options = List(())

            # Should fail - JSON spec doesn't allow these values
            result = engine._builtin_json_read((stream, term_var, options))
            assert result is False

    def test_json_read_unicode_strings(self):
        """Test json_read/3 with Unicode strings."""
        engine = Engine(program())

        json_str = '{"greeting": "héllo", "emoji": "🌟", "japanese": "こんにちは"}'
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, Struct)
        assert bound_term.functor == "json"


class TestJSONWriteBuiltin:
    """Test json_write/3 builtin (classic mode)."""

    def test_json_write_simple_object_to_string(self):
        """Test json_write/3 with simple json structure to string stream."""
        engine = Engine(program())

        # Create json([name-test, value-42]) term
        pairs = List(
            (
                Struct("=", (Atom("name"), Atom("test"))),
                Struct("=", (Atom("value"), Int(42))),
            )
        )
        json_term = Struct("json", (pairs,))

        # String stream to capture output
        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, json_term, options))
        assert result is True

        # Check written JSON
        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == {"name": "test", "value": 42}

    def test_json_write_array_to_string(self):
        """Test json_write/3 with array (List) to string stream."""
        engine = Engine(program())

        array_term = List((Int(1), Int(2), Atom("hello")))
        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, array_term, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == [1, 2, "hello"]

    def test_json_write_constants(self):
        """Test json_write/3 with JSON constants."""
        engine = Engine(program())

        # Array with constants: [@(null), @(true), @(false)]
        constants_array = List(
            (
                Struct("@", (Atom("null"),)),
                Struct("@", (Atom("true"),)),
                Struct("@", (Atom("false"),)),
            )
        )

        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, constants_array, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == [None, True, False]

    def test_json_write_empty_object(self):
        """Test json_write/3 with empty json object."""
        engine = Engine(program())

        empty_json = Struct("json", (List(()),))
        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, empty_json, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == {}

    def test_json_write_nested_structure(self):
        """Test json_write/3 with nested structures."""
        engine = Engine(program())

        # Create nested json structure
        inner_pairs = List((Struct("=", (Atom("value"), Int(42))),))
        inner_json = Struct("json", (inner_pairs,))

        outer_pairs = List((Struct("=", (Atom("inner"), inner_json)),))
        outer_json = Struct("json", (outer_pairs,))

        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, outer_json, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == {"inner": {"value": 42}}

    def test_json_write_invalid_term_fails(self):
        """Test json_write/3 fails with invalid term."""
        engine = Engine(program())

        # Invalid term that can't be converted to JSON
        invalid_term = Struct("invalid", (Atom("data"),))
        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, invalid_term, options))
        assert result is False

    def test_json_write_improper_list_fails(self):
        """Test json_write/3 fails with improper list (non-empty tail)."""
        engine = Engine(program())

        # Create improper list with non-empty tail
        improper_list = List((Int(1), Int(2)), tail=Atom("tail"))
        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, improper_list, options))
        assert result is False

    def test_json_write_special_floats_fail(self):
        """Test json_write/3 fails with special float values."""
        engine = Engine(program())

        # Test special float values that JSON can't represent
        special_floats = [
            Float(float("nan")),
            Float(float("inf")),
            Float(float("-inf")),
        ]

        for special_float in special_floats:
            # Try writing array containing special float
            array_with_special = List((Int(1), special_float, Int(3)))
            stream = StringIO()
            options = List(())

            result = engine._builtin_json_write((stream, array_with_special, options))
            assert result is False

    def test_json_write_file_stream(self):
        """Test json_write/3 with file stream."""
        engine = Engine(program())

        # Create json term
        pairs = List((Struct("=", (Atom("test"), Atom("file"))),))
        json_term = Struct("json", (pairs,))

        # Create temporary file
        with tempfile.NamedTemporaryFile(
            mode="w", delete=False, suffix=".json", encoding="utf-8"
        ) as f:
            temp_path = f.name

        try:
            # Write to file
            with open(temp_path, "w", encoding="utf-8") as stream:
                options = List(())
                result = engine._builtin_json_write((stream, json_term, options))
                assert result is True

            # Verify file contents
            with open(temp_path, "r", encoding="utf-8") as f:
                content = f.read()
                parsed = json.loads(content)
                assert parsed == {"test": "file"}
        finally:
            os.unlink(temp_path)


class TestJSONReadDictBuiltin:
    """Test json_read_dict/3 builtin (dict mode)."""

    def test_json_read_dict_simple_object_from_string(self):
        """Test json_read_dict/3 with simple JSON object."""
        engine = Engine(program())

        json_str = '{"name": "test", "value": 42}'
        stream = StringIO(json_str)

        dict_var = Var(engine.store.new_var("Dict"), "Dict")
        options = List(())

        result = engine._builtin_json_read_dict((stream, dict_var, options))
        assert result is True

        deref_result = engine.store.deref(dict_var.id)
        assert deref_result[0] == "BOUND"
        bound_dict = deref_result[2]
        assert isinstance(bound_dict, PrologDict)
        assert len(bound_dict.pairs) == 2

        # Check pairs (should be sorted)
        pairs = dict(bound_dict.pairs)
        assert pairs[Atom("name")] == Atom("test")
        assert pairs[Atom("value")] == Int(42)

    def test_json_read_dict_array_from_string(self):
        """Test json_read_dict/3 with JSON array."""
        engine = Engine(program())

        json_str = '[1, 2, "hello"]'
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read_dict((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, List)
        assert bound_term.items == (Int(1), Int(2), Atom("hello"))

    def test_json_read_dict_constants(self):
        """Test json_read_dict/3 with JSON constants in dict mode."""
        engine = Engine(program())

        json_str = '{"null_val": null, "true_val": true, "false_val": false}'
        stream = StringIO(json_str)

        dict_var = Var(engine.store.new_var("Dict"), "Dict")
        options = List(())

        result = engine._builtin_json_read_dict((stream, dict_var, options))
        assert result is True

        deref_result = engine.store.deref(dict_var.id)
        assert deref_result[0] == "BOUND"
        bound_dict = deref_result[2]
        assert isinstance(bound_dict, PrologDict)

        pairs = dict(bound_dict.pairs)
        # In dict mode, constants are atoms, not @ structures
        assert pairs[Atom("null_val")] == Atom("null")
        assert pairs[Atom("true_val")] == Atom("true")
        assert pairs[Atom("false_val")] == Atom("false")

    def test_json_read_dict_nested_structure(self):
        """Test json_read_dict/3 with nested JSON structure."""
        engine = Engine(program())

        json_str = '{"level1": {"level2": {"value": 42}}}'
        stream = StringIO(json_str)

        dict_var = Var(engine.store.new_var("Dict"), "Dict")
        options = List(())

        result = engine._builtin_json_read_dict((stream, dict_var, options))
        assert result is True

        deref_result = engine.store.deref(dict_var.id)
        assert deref_result[0] == "BOUND"
        bound_dict = deref_result[2]
        assert isinstance(bound_dict, PrologDict)

        # Should have nested PrologDict structures
        pairs = dict(bound_dict.pairs)
        level1 = pairs[Atom("level1")]
        assert isinstance(level1, PrologDict)

        level1_pairs = dict(level1.pairs)
        level2 = level1_pairs[Atom("level2")]
        assert isinstance(level2, PrologDict)

        level2_pairs = dict(level2.pairs)
        assert level2_pairs[Atom("value")] == Int(42)


class TestJSONWriteDictBuiltin:
    """Test json_write_dict/3 builtin (dict mode)."""

    def test_json_write_dict_simple_dict_to_string(self):
        """Test json_write_dict/3 with simple PrologDict."""
        engine = Engine(program())

        prolog_dict = PrologDict(
            ((Atom("name"), Atom("test")), (Atom("value"), Int(42)))
        )

        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write_dict((stream, prolog_dict, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == {"name": "test", "value": 42}

    def test_json_write_dict_with_constants(self):
        """Test json_write_dict/3 with dict mode constants."""
        engine = Engine(program())

        prolog_dict = PrologDict(
            (
                (Atom("null_val"), Atom("null")),
                (Atom("true_val"), Atom("true")),
                (Atom("false_val"), Atom("false")),
            )
        )

        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write_dict((stream, prolog_dict, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == {"null_val": None, "true_val": True, "false_val": False}

    def test_json_write_dict_array(self):
        """Test json_write_dict/3 with List (array)."""
        engine = Engine(program())

        array_term = List((Int(1), Int(2), Atom("hello")))
        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write_dict((stream, array_term, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        assert parsed == [1, 2, "hello"]

    def test_json_write_dict_special_floats_fail(self):
        """Test json_write_dict/3 fails with special float values."""
        engine = Engine(program())

        # Test special float values in dict mode
        special_floats = [
            Float(float("nan")),
            Float(float("inf")),
            Float(float("-inf")),
        ]

        for special_float in special_floats:
            # Try writing PrologDict containing special float
            dict_with_special = PrologDict(
                ((Atom("normal"), Int(42)), (Atom("special"), special_float))
            )
            stream = StringIO()
            options = List(())

            result = engine._builtin_json_write_dict(
                (stream, dict_with_special, options)
            )
            assert result is False

    def test_json_write_dict_int_keys(self):
        """Test json_write_dict/3 with integer keys in PrologDict."""
        engine = Engine(program())

        prolog_dict = PrologDict(
            (
                (Int(1), Atom("first")),
                (Int(2), Atom("second")),
                (Atom("name"), Atom("test")),
            )
        )

        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write_dict((stream, prolog_dict, options))
        assert result is True

        written_json = stream.getvalue()
        parsed = json.loads(written_json)
        # Integer keys should be converted to strings
        assert parsed == {"1": "first", "2": "second", "name": "test"}


class TestAtomJSONTermBuiltin:
    """Test atom_json_term/3 builtin (bidirectional conversion)."""

    def test_atom_json_term_atom_to_term_classic(self):
        """Test atom_json_term/3 converting atom to term (classic mode)."""
        engine = Engine(program())

        json_atom = Atom('{"name": "test", "value": 42}')
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())  # Default to classic mode

        result = engine._builtin_atom_json_term((json_atom, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, Struct)
        assert bound_term.functor == "json"

    def test_atom_json_term_term_to_atom_classic(self):
        """Test atom_json_term/3 converting term to atom (classic mode)."""
        engine = Engine(program())

        pairs = List((Struct("=", (Atom("name"), Atom("test"))),))
        json_term = Struct("json", (pairs,))
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        options = List(())

        result = engine._builtin_atom_json_term((atom_var, json_term, options))
        assert result is True

        deref_result = engine.store.deref(atom_var.id)
        assert deref_result[0] == "BOUND"
        bound_atom = deref_result[2]
        assert isinstance(bound_atom, Atom)

        # Should be valid JSON when parsed
        parsed = json.loads(bound_atom.name)
        assert parsed == {"name": "test"}

    def test_atom_json_term_bidirectional_classic(self):
        """Test atom_json_term/3 both directions work (classic mode)."""
        engine = Engine(program())

        original_atom = Atom('{"test": "value"}')
        term_var = Var(engine.store.new_var("Term"), "Term")
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        options = List(())

        # First direction: atom -> term
        result1 = engine._builtin_atom_json_term((original_atom, term_var, options))
        assert result1 is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]

        # Second direction: term -> atom
        result2 = engine._builtin_atom_json_term((atom_var, bound_term, options))
        assert result2 is True

        deref_result = engine.store.deref(atom_var.id)
        assert deref_result[0] == "BOUND"
        bound_atom = deref_result[2]
        assert isinstance(bound_atom, Atom)

        # Should parse to same JSON structure
        original_json = json.loads(original_atom.name)
        result_json = json.loads(bound_atom.name)
        assert original_json == result_json

    def test_atom_json_term_atom_to_term_dict(self):
        """Test atom_json_term/3 converting atom to term (dict mode)."""
        engine = Engine(program())

        json_atom = Atom('{"name": "test", "value": 42}')
        term_var = Var(engine.store.new_var("Term"), "Term")
        # Options with dict mode specified
        options = List((Struct("mode", (Atom("dict"),)),))

        result = engine._builtin_atom_json_term((json_atom, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, PrologDict)

    def test_atom_json_term_term_to_atom_dict(self):
        """Test atom_json_term/3 converting term to atom (dict mode)."""
        engine = Engine(program())

        prolog_dict = PrologDict(((Atom("name"), Atom("test")),))
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        options = List((Struct("mode", (Atom("dict"),)),))

        result = engine._builtin_atom_json_term((atom_var, prolog_dict, options))
        assert result is True

        deref_result = engine.store.deref(atom_var.id)
        assert deref_result[0] == "BOUND"
        bound_atom = deref_result[2]
        assert isinstance(bound_atom, Atom)

        parsed = json.loads(bound_atom.name)
        assert parsed == {"name": "test"}

    def test_atom_json_term_both_ground_unify(self):
        """Test atom_json_term/3 with both args ground - should unify."""
        engine = Engine(program())

        # JSON atom and corresponding term
        json_atom = Atom('{"test": "value"}')
        pairs = List((Struct("=", (Atom("test"), Atom("value"))),))
        json_term = Struct("json", (pairs,))
        options = List(())

        # Both ground - should unify/check consistency
        result = engine._builtin_atom_json_term((json_atom, json_term, options))
        assert result is True

    def test_atom_json_term_both_ground_no_unify(self):
        """Test atom_json_term/3 with inconsistent ground args - should fail."""
        engine = Engine(program())

        json_atom = Atom('{"test": "value"}')
        # Different term that doesn't match
        pairs = List((Struct("=", (Atom("other"), Atom("data"))),))
        json_term = Struct("json", (pairs,))
        options = List(())

        result = engine._builtin_atom_json_term((json_atom, json_term, options))
        assert result is False

    def test_atom_json_term_malformed_json_fails(self):
        """Test atom_json_term/3 fails with malformed JSON atom."""
        engine = Engine(program())

        malformed_atom = Atom('{"incomplete": ')
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_atom_json_term((malformed_atom, term_var, options))
        assert result is False

    def test_atom_json_term_invalid_term_fails(self):
        """Test atom_json_term/3 fails with term that can't convert to JSON."""
        engine = Engine(program())

        invalid_term = Struct("invalid", (Atom("term"),))
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        options = List(())

        result = engine._builtin_atom_json_term((atom_var, invalid_term, options))
        assert result is False


class TestJSONBuiltinRoundTrips:
    """Test round-trip conversions through JSON builtins."""

    def test_classic_mode_roundtrip_via_builtins(self):
        """Test JSON classic mode roundtrip through read/write builtins."""
        engine = Engine(program())

        # Original JSON
        original_json = {"name": "test", "values": [1, 2, 3], "active": True}
        json_str = json.dumps(original_json)

        # Read JSON -> term
        read_stream = StringIO(json_str)
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        read_result = engine._builtin_json_read((read_stream, term_var, options))
        assert read_result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]

        # Write term -> JSON
        write_stream = StringIO()
        write_result = engine._builtin_json_write((write_stream, bound_term, options))
        assert write_result is True

        # Parse result and compare
        result_json = json.loads(write_stream.getvalue())
        assert result_json == original_json

    def test_dict_mode_roundtrip_via_builtins(self):
        """Test JSON dict mode roundtrip through read_dict/write_dict builtins."""
        engine = Engine(program())

        original_json = {"name": "test", "values": [1, 2, 3], "active": True}
        json_str = json.dumps(original_json)

        # Read JSON -> dict
        read_stream = StringIO(json_str)
        dict_var = Var(engine.store.new_var("Dict"), "Dict")
        options = List(())

        read_result = engine._builtin_json_read_dict((read_stream, dict_var, options))
        assert read_result is True

        deref_result = engine.store.deref(dict_var.id)
        assert deref_result[0] == "BOUND"
        bound_dict = deref_result[2]

        # Write dict -> JSON
        write_stream = StringIO()
        write_result = engine._builtin_json_write_dict(
            (write_stream, bound_dict, options)
        )
        assert write_result is True

        result_json = json.loads(write_stream.getvalue())
        assert result_json == original_json

    def test_atom_json_term_roundtrip_classic(self):
        """Test atom_json_term/3 roundtrip in classic mode."""
        engine = Engine(program())

        original_json = {"test": "data", "number": 42}
        json_atom = Atom(json.dumps(original_json))

        # Atom -> Term
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result1 = engine._builtin_atom_json_term((json_atom, term_var, options))
        assert result1 is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]

        # Term -> Atom
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        result2 = engine._builtin_atom_json_term((atom_var, bound_term, options))
        assert result2 is True

        deref_result = engine.store.deref(atom_var.id)
        assert deref_result[0] == "BOUND"
        bound_atom = deref_result[2]
        result_json = json.loads(bound_atom.name)
        assert result_json == original_json

    def test_atom_json_term_roundtrip_dict(self):
        """Test atom_json_term/3 roundtrip in dict mode."""
        engine = Engine(program())

        original_json = {"test": "data", "number": 42}
        json_atom = Atom(json.dumps(original_json))

        # Atom -> Dict
        dict_var = Var(engine.store.new_var("Dict"), "Dict")
        dict_options = List((Struct("mode", (Atom("dict"),)),))

        result1 = engine._builtin_atom_json_term((json_atom, dict_var, dict_options))
        assert result1 is True

        deref_result = engine.store.deref(dict_var.id)
        assert deref_result[0] == "BOUND"
        bound_dict = deref_result[2]
        assert isinstance(bound_dict, PrologDict)

        # Dict -> Atom
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        result2 = engine._builtin_atom_json_term((atom_var, bound_dict, dict_options))
        assert result2 is True

        deref_result = engine.store.deref(atom_var.id)
        assert deref_result[0] == "BOUND"
        bound_atom = deref_result[2]
        result_json = json.loads(bound_atom.name)
        assert result_json == original_json


class TestJSONBuiltinErrorHandling:
    """Test error handling in JSON builtins."""

    def test_json_read_with_non_stream_fails(self):
        """Test JSON read builtins fail with non-stream first argument."""
        engine = Engine(program())

        non_stream = Atom("not_a_stream")
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        # json_read/3 should fail
        result1 = engine._builtin_json_read((non_stream, term_var, options))
        assert result1 is False

        # json_read_dict/3 should fail
        result2 = engine._builtin_json_read_dict((non_stream, term_var, options))
        assert result2 is False

    def test_json_write_with_non_stream_fails(self):
        """Test JSON write builtins fail with non-stream first argument."""
        engine = Engine(program())

        non_stream = Atom("not_a_stream")
        valid_term = List((Int(1), Int(2)))
        options = List(())

        # json_write/3 should fail
        result1 = engine._builtin_json_write((non_stream, valid_term, options))
        assert result1 is False

        # json_write_dict/3 should fail
        result2 = engine._builtin_json_write_dict((non_stream, valid_term, options))
        assert result2 is False

    def test_atom_json_term_with_non_atom_fails(self):
        """Test atom_json_term/3 fails when first arg should be atom but isn't."""
        engine = Engine(program())

        non_atom = Int(42)
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_atom_json_term((non_atom, term_var, options))
        assert result is False

    def test_json_builtins_with_invalid_options_fail(self):
        """Test JSON builtins handle invalid options gracefully."""
        engine = Engine(program())

        stream = StringIO('{"test": "value"}')
        term_var = Var(engine.store.new_var("Term"), "Term")
        invalid_options = Struct("invalid", (Atom("option"),))

        # Should fail with invalid options (or succeed and ignore them)
        # Implementation dependent - at minimum shouldn't crash
        try:
            result = engine._builtin_json_read((stream, term_var, invalid_options))
            # Either fails gracefully or succeeds ignoring bad options
            assert isinstance(result, bool)
        except Exception:
            # Should not raise unhandled exceptions
            pytest.fail("JSON builtin should handle invalid options gracefully")


class TestJSONBuiltinIntegration:
    """Integration tests combining JSON builtins with other engine features."""

    def test_json_builtins_with_variables_and_unification(self):
        """Test JSON builtins work with variables and unification."""
        engine = Engine(program())

        # Test that variables in terms are properly handled
        json_str = '{"name": "test"}'
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        # Read JSON
        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        # The term should be properly bound and dereferenceable
        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]
        assert isinstance(bound_term, Struct)
        assert bound_term.functor == "json"

    def test_json_builtins_preserve_store_state(self):
        """Test JSON builtins don't corrupt the variable store."""
        engine = Engine(program())

        # Create some variables before JSON operations
        before_var_id = engine.store.new_var("Before")
        engine.store.new_var()  # Ensure store has some state

        # Perform JSON operation
        json_str = '{"test": "value"}'
        stream = StringIO(json_str)
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        # Verify original state preserved
        assert isinstance(before_var_id, int)
        # Store should still be valid
        new_var_id = engine.store.new_var("New")
        assert isinstance(new_var_id, int)

    def test_json_operations_in_sequence(self):
        """Test multiple JSON operations in sequence."""
        engine = Engine(program())

        # Read -> modify -> write sequence
        original_json = {"counter": 1, "name": "test"}

        # Step 1: Read
        read_stream = StringIO(json.dumps(original_json))
        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        read_result = engine._builtin_json_read((read_stream, term_var, options))
        assert read_result is True

        # Step 2: Convert to dict mode for easier manipulation
        atom_var = Var(engine.store.new_var("Atom"), "Atom")
        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]

        atom_result = engine._builtin_atom_json_term((atom_var, bound_term, options))
        assert atom_result is True

        # Step 3: Read back as dict
        deref_result = engine.store.deref(atom_var.id)
        assert deref_result[0] == "BOUND"
        bound_atom = deref_result[2]
        dict_stream = StringIO(bound_atom.name)
        dict_var = Var(engine.store.new_var("Dict"), "Dict")

        dict_result = engine._builtin_json_read_dict((dict_stream, dict_var, options))
        assert dict_result is True

        # Step 4: Write dict back to JSON
        write_stream = StringIO()
        deref_result = engine.store.deref(dict_var.id)
        assert deref_result[0] == "BOUND"
        bound_dict = deref_result[2]

        write_result = engine._builtin_json_write_dict(
            (write_stream, bound_dict, options)
        )
        assert write_result is True

        # Verify final result
        final_json = json.loads(write_stream.getvalue())
        assert final_json == original_json


class TestJSONBuiltinKeyOrderingAndDuplicates:
    """Test JSON object key ordering and duplicate handling."""

    def test_json_object_key_sorting(self):
        """Test that JSON object keys are sorted canonically."""
        engine = Engine(program())

        # JSON with keys in non-alphabetical order
        json_str = '{"zebra": 3, "apple": 1, "banana": 2}'
        stream = StringIO(json_str)

        term_var = Var(engine.store.new_var("Term"), "Term")
        options = List(())

        result = engine._builtin_json_read((stream, term_var, options))
        assert result is True

        deref_result = engine.store.deref(term_var.id)
        assert deref_result[0] == "BOUND"
        bound_term = deref_result[2]

        # Check that pairs are sorted alphabetically
        pairs_list = bound_term.args[0]
        assert len(pairs_list.items) == 3

        # Should be sorted: apple, banana, zebra
        assert pairs_list.items[0].args[0] == Atom("apple")
        assert pairs_list.items[1].args[0] == Atom("banana")
        assert pairs_list.items[2].args[0] == Atom("zebra")

    def test_classic_json_struct_duplicate_keys_fails(self):
        """Test that classic json struct with duplicate keys fails on write."""
        engine = Engine(program())

        # Create malformed classic struct with duplicate keys
        malformed_json = Struct(
            "json",
            (
                List(
                    (
                        Struct("=", (Atom("key"), Atom("value1"))),
                        Struct("=", (Atom("key"), Atom("value2"))),  # Duplicate key
                    )
                ),
            ),
        )

        stream = StringIO()
        options = List(())

        result = engine._builtin_json_write((stream, malformed_json, options))
        assert result is False
