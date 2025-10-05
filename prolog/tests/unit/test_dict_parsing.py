"""Tests for PrologDict parsing functionality.

This module tests the parser integration for dict syntax including:
- Empty dict parsing: {}
- Simple dict parsing: {a:1, b:2}
- Nested dict parsing: {a:{x:1}, b:2}
- Error cases: duplicate keys, malformed syntax
- Round-trip parsing and pretty printing
- ParseError conversion from ValueError
"""

import pytest
from prolog.ast.terms import PrologDict, Atom, Int, Var, Struct, List
from prolog.ast.pretty import pretty
from prolog.parser.parser import parse_term, ParseError


class TestDictParsingBasic:
    """Test basic dict parsing functionality."""

    def test_parse_empty_dict(self):
        """Test parsing empty dict {}."""
        result = parse_term("{}")
        expected = PrologDict(())
        assert result == expected
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 0

    def test_parse_single_pair_dict(self):
        """Test parsing dict with single key-value pair."""
        result = parse_term("{a:1}")
        expected = PrologDict(((Atom("a"), Int(1)),))
        assert result == expected
        assert len(result.pairs) == 1
        assert result.pairs[0] == (Atom("a"), Int(1))

    def test_parse_multiple_pairs_dict(self):
        """Test parsing dict with multiple key-value pairs."""
        result = parse_term("{a:1, b:2}")
        expected = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        assert result == expected
        assert len(result.pairs) == 2

    def test_parse_dict_with_spaces(self):
        """Test parsing dict with various whitespace."""
        # Extra spaces should be ignored
        result = parse_term("{ a : 1 , b : 2 }")
        expected = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        assert result == expected

    def test_parse_dict_automatic_sorting(self):
        """Test that parsed dicts are automatically sorted."""
        # Input order: z, a, m should become a, m, z
        result = parse_term("{z:3, a:1, m:2}")
        expected = PrologDict(
            ((Atom("a"), Int(1)), (Atom("m"), Int(2)), (Atom("z"), Int(3)))
        )
        assert result == expected
        # Verify order is correct
        keys = [pair[0].name for pair in result.pairs]
        assert keys == ["a", "m", "z"]


class TestDictParsingKeyTypes:
    """Test dict parsing with different key types."""

    def test_parse_atom_keys(self):
        """Test parsing with atom keys."""
        result = parse_term("{foo:1, bar:2}")
        expected = PrologDict(((Atom("bar"), Int(2)), (Atom("foo"), Int(1))))
        assert result == expected

    def test_parse_quoted_atom_keys(self):
        """Test parsing with quoted atom keys."""
        result = parse_term("{'hello world':1, 'key with spaces':2}")
        expected = PrologDict(
            ((Atom("hello world"), Int(1)), (Atom("key with spaces"), Int(2)))
        )
        assert result == expected

    def test_parse_integer_keys(self):
        """Test parsing with integer keys."""
        result = parse_term("{42:answer, 1:first}")
        # Should be sorted: atoms before integers, but let's check actual result
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2

    def test_parse_mixed_key_types(self):
        """Test parsing with mixed atom and integer keys."""
        result = parse_term("{name:alice, 42:answer}")
        expected = PrologDict(
            ((Atom("name"), Atom("alice")), (Int(42), Atom("answer")))
        )
        assert result == expected
        # Verify atoms sort before integers
        first_key = result.pairs[0][0]
        assert isinstance(first_key, Atom)


class TestDictParsingValueTypes:
    """Test dict parsing with different value types."""

    def test_parse_atom_values(self):
        """Test parsing with atom values."""
        result = parse_term("{type:person, status:active}")
        expected = PrologDict(
            ((Atom("status"), Atom("active")), (Atom("type"), Atom("person")))
        )
        assert result == expected

    def test_parse_integer_values(self):
        """Test parsing with integer values."""
        result = parse_term("{count:5, total:100}")
        expected = PrologDict(((Atom("count"), Int(5)), (Atom("total"), Int(100))))
        assert result == expected

    def test_parse_variable_values(self):
        """Test parsing with variable values."""
        result = parse_term("{name:X, age:Y}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2
        # Check that values are variables
        for _, value in result.pairs:
            assert isinstance(value, Var)

    def test_parse_struct_values(self):
        """Test parsing with structure values."""
        result = parse_term("{person:name(alice), data:point(1, 2)}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2
        # Check that values are structures
        for _, value in result.pairs:
            assert isinstance(value, Struct)

    def test_parse_list_values(self):
        """Test parsing with list values."""
        result = parse_term("{items:[a, b, c], empty:[]}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2
        # Check that values are lists
        for _, value in result.pairs:
            assert isinstance(value, List)


class TestDictParsingNested:
    """Test parsing of nested dictionaries."""

    def test_parse_nested_dict_values(self):
        """Test parsing dicts with nested dict values."""
        result = parse_term("{outer:{inner:value}}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 1

        outer_key, outer_value = result.pairs[0]
        assert outer_key == Atom("outer")
        assert isinstance(outer_value, PrologDict)
        assert len(outer_value.pairs) == 1

        inner_key, inner_value = outer_value.pairs[0]
        assert inner_key == Atom("inner")
        assert inner_value == Atom("value")

    def test_parse_complex_nested_structure(self):
        """Test parsing complex nested dict structure."""
        result = parse_term("{a:{x:1, y:2}, b:simple}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2

        # Check first pair (a: {x:1, y:2})
        a_key, a_value = result.pairs[0]
        assert a_key == Atom("a")
        assert isinstance(a_value, PrologDict)
        assert len(a_value.pairs) == 2

        # Check second pair (b: simple)
        b_key, b_value = result.pairs[1]
        assert b_key == Atom("b")
        assert b_value == Atom("simple")


class TestDictParsingErrors:
    """Test dict parsing error cases."""

    def test_parse_duplicate_keys_error(self):
        """Test that duplicate keys raise ParseError."""
        with pytest.raises(ParseError, match="Duplicate keys"):
            parse_term("{a:1, a:2}")

    def test_parse_malformed_syntax_errors(self):
        """Test various malformed dict syntax errors."""
        # Missing closing brace
        with pytest.raises(ParseError):
            parse_term("{a:1")

        # Missing opening brace
        with pytest.raises(ParseError):
            parse_term("a:1}")

        # Missing colon
        with pytest.raises(ParseError):
            parse_term("{a 1}")

        # Missing value
        with pytest.raises(ParseError):
            parse_term("{a:}")

        # Missing key
        with pytest.raises(ParseError):
            parse_term("{:1}")

    def test_parse_trailing_comma_error(self):
        """Test that trailing comma is handled properly."""
        # This may or may not be an error depending on grammar design
        # For now, assume it should be an error
        with pytest.raises(ParseError):
            parse_term("{a:1,}")

    def test_parse_empty_pair_error(self):
        """Test parsing with empty pairs."""
        with pytest.raises(ParseError):
            parse_term("{,}")

    def test_quoted_numeric_keys(self):
        """Test that quoted numeric keys are treated as atoms."""
        result = parse_term("{'123':value, '456':other}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2
        # All keys should be atoms, not integers
        for key, _ in result.pairs:
            assert isinstance(key, Atom)
            assert key.name in ["123", "456"]

    def test_duplicate_keys_different_forms(self):
        """Test duplicate detection for same atom in different forms."""
        # Both 'a' and a represent the same atom
        with pytest.raises(ParseError, match="Duplicate keys"):
            parse_term("{'a':1, a:2}")


class TestDictParsingRoundTrip:
    """Test round-trip parsing and pretty printing."""

    def test_round_trip_empty_dict(self):
        """Test round-trip for empty dict."""
        original = "{}"
        parsed = parse_term(original)
        pretty_str = pretty(parsed)
        assert pretty_str == "{}"

    def test_round_trip_simple_dict(self):
        """Test round-trip for simple dict."""
        # Note: may not be exact due to sorting
        parsed = parse_term("{b:2, a:1}")
        pretty_str = pretty(parsed)
        assert pretty_str == "{a:1, b:2}"  # Should be sorted

        # Re-parse the pretty output
        reparsed = parse_term(pretty_str)
        assert reparsed == parsed

    def test_round_trip_complex_dict(self):
        """Test round-trip for complex dict with various types."""
        parsed = parse_term("{name:alice, count:5, items:[x, y]}")
        pretty_str = pretty(parsed)

        # Re-parse and compare
        reparsed = parse_term(pretty_str)
        assert reparsed == parsed

    def test_round_trip_nested_dict(self):
        """Test round-trip for nested dict."""
        parsed = parse_term("{outer:{inner:value, other:data}}")
        pretty_str = pretty(parsed)

        # Re-parse and compare
        reparsed = parse_term(pretty_str)
        assert reparsed == parsed


class TestDictParsingEdgeCases:
    """Test edge cases and special scenarios."""

    def test_parse_dict_with_reserved_tokens(self):
        """Test parsing dict with reserved token values."""
        result = parse_term("{empty:[], cut:!}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2

        # Check that reserved tokens are parsed correctly
        for key, value in result.pairs:
            if key == Atom("empty"):
                assert isinstance(value, List)
            elif key == Atom("cut"):
                assert value == Atom("!")

    def test_parse_single_character_keys(self):
        """Test parsing with single character keys."""
        result = parse_term("{a:1, b:2, c:3}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 3

    def test_parse_numeric_string_values(self):
        """Test parsing with quoted numeric values."""
        result = parse_term("{num:'123', text:'hello'}")
        assert isinstance(result, PrologDict)
        # Both values should be atoms (quoted)
        for _, value in result.pairs:
            assert isinstance(value, Atom)

    def test_parse_dict_in_larger_structure(self):
        """Test parsing dict as part of larger structure."""
        result = parse_term("container({key:value})")
        assert isinstance(result, Struct)
        assert result.functor == "container"
        assert len(result.args) == 1
        assert isinstance(result.args[0], PrologDict)


class TestDictParsingGrammarIntegration:
    """Test integration with existing grammar elements."""

    def test_dict_as_struct_argument(self):
        """Test dict used as structure argument."""
        result = parse_term("process({input:data, output:result})")
        assert isinstance(result, Struct)
        assert result.functor == "process"
        assert len(result.args) == 1
        assert isinstance(result.args[0], PrologDict)

    def test_dict_as_list_element(self):
        """Test dict used as list element."""
        result = parse_term("[{a:1}, {b:2}]")
        assert isinstance(result, List)
        assert len(result.items) == 2
        for item in result.items:
            assert isinstance(item, PrologDict)

    def test_compound_term_keys_rejected(self):
        """Test that compound term keys are rejected with ParseError."""
        # Compound terms not allowed as keys per Phase 1 policy
        with pytest.raises(ParseError, match="Dict keys must be atoms or integers"):
            parse_term("{func(a):result}")

    def test_variable_keys_rejected(self):
        """Test that variable keys are rejected with ParseError."""
        # Variables not allowed as keys per Phase 1 policy
        with pytest.raises(ParseError, match="Dict keys must be atoms or integers"):
            parse_term("{X:value}")

    def test_dict_with_compound_values(self):
        """Test dict with compound term values (allowed)."""
        # Values can be any term type, only keys are restricted
        result = parse_term("{key:value(x, y), other:func(a)}")
        assert isinstance(result, PrologDict)
        assert len(result.pairs) == 2
        # All values should be structures
        for _, value in result.pairs:
            assert isinstance(value, Struct)
