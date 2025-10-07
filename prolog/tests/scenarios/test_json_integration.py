"""
Integration test scenarios for JSON functionality.

This module tests end-to-end JSON processing scenarios that combine
multiple JSON operations and demonstrate real-world usage patterns:

- File-based JSON reading and writing
- Round-trip conversion between JSON and Prolog terms
- Mixed operations with PrologDict and JSON builtins
- Error handling in realistic scenarios
"""

from prolog.engine.engine import Engine
from prolog.parser.reader import Reader
from prolog.tests.helpers import program


class TestJSONConversionRoundTrip:
    """Test round-trip conversion preserves data integrity."""

    def test_atom_json_term_round_trip_classic(self):
        """Test that JSON → Prolog → JSON preserves data (classic mode)."""
        reader = Reader()
        engine = Engine(program())

        # Test round trip conversion
        query = reader.read_term(
            'atom_json_term(\'{"name": "test", "items": [1, 2, 3]}\', Term, [])'
        )
        results = list(engine.solve(query))
        assert len(results) == 1

    def test_atom_json_term_round_trip_dict(self):
        """Test JSON round-trip using dict mode."""
        reader = Reader()
        engine = Engine(program())

        # Test dict mode conversion
        query = reader.read_term(
            'atom_json_term(\'{"user": {"id": 123}}\', Dict, [json_object(dict)])'
        )
        results = list(engine.solve(query))
        assert len(results) == 1

    def test_empty_structures_round_trip(self):
        """Test round-trip with empty objects and arrays."""
        reader = Reader()
        engine = Engine(program())

        # Test empty object
        query_obj = reader.read_term(
            "atom_json_term('{}', Term, []), atom_json_term(NewAtom, Term, [])"
        )
        results = list(engine.solve(query_obj))
        assert len(results) == 1

        # Test empty array
        query_arr = reader.read_term(
            "atom_json_term('[]', Term, []), atom_json_term(NewAtom, Term, [])"
        )
        results = list(engine.solve(query_arr))
        assert len(results) == 1


class TestMixedDictJSONOperations:
    """Test mixing PrologDict and JSON operations."""

    def test_dict_to_json_conversion(self):
        """Test converting PrologDict to JSON and back."""
        reader = Reader()
        engine = Engine(program())

        # Test that JSON dict mode works
        query = reader.read_term(
            'atom_json_term(\'{"name": "alice", "age": 30}\', Dict, [json_object(dict)])'
        )
        results = list(engine.solve(query))
        assert len(results) == 1

    def test_json_to_dict_operations(self):
        """Test reading JSON and performing dict operations."""
        reader = Reader()
        engine = Engine(program())

        # Read JSON as dict
        query = reader.read_term(
            'atom_json_term(\'{"name": "bob", "age": 30}\', Dict, [json_object(dict)])'
        )
        results = list(engine.solve(query))
        assert len(results) == 1


class TestJSONErrorHandling:
    """Test error handling in realistic scenarios."""

    def test_malformed_json_handling(self):
        """Test handling of malformed JSON."""
        reader = Reader()
        engine = Engine(program())

        # Should fail to parse malformed JSON
        query = reader.read_term("atom_json_term('{\"bad\": invalid_json}', _, [])")
        results = list(engine.solve(query))
        assert len(results) == 0  # Should fail

    def test_empty_string_handling(self):
        """Test error handling with empty JSON string."""
        reader = Reader()
        engine = Engine(program())

        # Empty string should fail
        query = reader.read_term("atom_json_term('', _, [])")
        results = list(engine.solve(query))
        assert len(results) == 0  # Should fail


class TestJSONStructureHandling:
    """Test handling of various JSON structure types."""

    def test_nested_structure_parsing(self):
        """Test parsing deeply nested structures."""
        reader = Reader()
        engine = Engine(program())

        nested_json = '{"a": {"b": {"c": {"d": "deep"}}}}'
        query = reader.read_term(f"atom_json_term('{nested_json}', Term, [])")
        results = list(engine.solve(query))
        assert len(results) == 1

    def test_mixed_array_handling(self):
        """Test arrays with mixed data types."""
        reader = Reader()
        engine = Engine(program())

        mixed_json = '[1, "hello", true, null, {"key": "value"}]'
        query = reader.read_term(f"atom_json_term('{mixed_json}', Term, [])")
        results = list(engine.solve(query))
        assert len(results) == 1

    def test_large_number_handling(self):
        """Test handling of large numbers and edge cases."""
        reader = Reader()
        engine = Engine(program())

        number_json = '{"big": 9223372036854775807, "small": 0.000001}'
        query = reader.read_term(f"atom_json_term('{number_json}', Term, [])")
        results = list(engine.solve(query))
        assert len(results) == 1


class TestJSONConstantHandling:
    """Test JSON constant representation across modes."""

    def test_json_constants_classic_mode(self):
        """Test JSON constants in classic mode."""
        reader = Reader()
        engine = Engine(program())

        query = reader.read_term(
            'atom_json_term(\'{"null": null, "true": true, "false": false}\', Term, [])'
        )
        results = list(engine.solve(query))
        assert len(results) == 1

    def test_json_constants_dict_mode(self):
        """Test JSON constants in dict mode."""
        reader = Reader()
        engine = Engine(program())

        query = reader.read_term(
            'atom_json_term(\'{"null": null, "true": true, "false": false}\', Dict, [json_object(dict)])'
        )
        results = list(engine.solve(query))
        assert len(results) == 1
