"""Tests for Dict term type implementation.

This module tests the core Dict dataclass including:
- Dict creation with sorted key-value pairs
- Immutability and hashability verification
- Duplicate key detection and error handling
- Canonical ordering verification
- Pretty printing round-trip tests
"""

import pytest
from prolog.ast.terms import PrologDict, Atom, Int, Var
from prolog.ast.pretty import pretty


class TestDictBasicCreation:
    """Test basic Dict creation and validation."""

    def test_empty_dict(self):
        """Test creating an empty dictionary."""
        d = PrologDict(())
        assert d.pairs == ()
        assert d.tag is None

    def test_single_pair_dict(self):
        """Test creating a dict with one key-value pair."""
        d = PrologDict(((Atom("a"), Int(1)),))
        assert len(d.pairs) == 1
        assert d.pairs[0] == (Atom("a"), Int(1))

    def test_multiple_pairs_dict(self):
        """Test creating a dict with multiple key-value pairs."""
        d = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        assert len(d.pairs) == 2
        assert d.pairs[0] == (Atom("a"), Int(1))
        assert d.pairs[1] == (Atom("b"), Int(2))

    def test_automatic_sorting(self):
        """Test that pairs are automatically sorted by key."""
        # Create dict with unsorted pairs - should be auto-sorted
        d = PrologDict(((Atom("z"), Int(3)), (Atom("a"), Int(1)), (Atom("m"), Int(2))))

        # Should be sorted alphabetically
        expected_pairs = ((Atom("a"), Int(1)), (Atom("m"), Int(2)), (Atom("z"), Int(3)))
        assert d.pairs == expected_pairs

    def test_atoms_sort_before_integers(self):
        """Test that atom keys sort before integer keys."""
        d = PrologDict(((Int(5), Atom("five")), (Atom("a"), Int(1))))

        # Atom should come first
        expected_pairs = ((Atom("a"), Int(1)), (Int(5), Atom("five")))
        assert d.pairs == expected_pairs

    def test_integer_key_sorting(self):
        """Test that integer keys are sorted numerically."""
        d = PrologDict(
            (
                (Int(100), Atom("big")),
                (Int(5), Atom("small")),
                (Int(50), Atom("medium")),
            )
        )

        expected_pairs = (
            (Int(5), Atom("small")),
            (Int(50), Atom("medium")),
            (Int(100), Atom("big")),
        )
        assert d.pairs == expected_pairs


class TestDictValidation:
    """Test Dict validation and error handling."""

    def test_duplicate_key_rejection(self):
        """Test that duplicate keys raise ValueError."""
        with pytest.raises(ValueError, match="Dict keys must be unique"):
            PrologDict(((Atom("a"), Int(1)), (Atom("a"), Int(2))))

    def test_invalid_key_type_rejection(self):
        """Test that invalid key types raise ValueError."""
        with pytest.raises(ValueError, match="Dict keys must be atoms or integers"):
            PrologDict(((Var(id=0), Int(1)),))  # Variables not allowed as keys

    def test_duplicate_int_keys(self):
        """Test duplicate integer keys are also rejected."""
        with pytest.raises(ValueError, match="Dict keys must be unique"):
            PrologDict(((Int(42), Atom("first")), (Int(42), Atom("second"))))


class TestDictImmutability:
    """Test Dict immutability properties."""

    def test_dict_is_frozen(self):
        """Test that Dict instances are immutable."""
        d = PrologDict(((Atom("a"), Int(1)),))

        # Should not be able to modify pairs
        with pytest.raises(AttributeError):
            d.pairs = ((Atom("b"), Int(2)),)

    def test_dict_is_hashable(self):
        """Test that Dict instances can be used as dict keys."""
        d1 = PrologDict(((Atom("a"), Int(1)),))
        d2 = PrologDict(((Atom("b"), Int(2)),))

        # Should be able to use as dict keys
        lookup = {d1: "first", d2: "second"}
        assert lookup[d1] == "first"
        assert lookup[d2] == "second"

    def test_dict_equality(self):
        """Test Dict equality comparison."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d3 = PrologDict(((Atom("a"), Int(1)), (Atom("c"), Int(3))))

        assert d1 == d2  # Same pairs
        assert d1 != d3  # Different pairs

    def test_dict_ordering_independence(self):
        """Test that creation order doesn't affect equality."""
        d1 = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        d2 = PrologDict(((Atom("b"), Int(2)), (Atom("a"), Int(1))))  # Different order

        assert d1 == d2  # Should be equal due to canonical ordering


class TestDictPrettyPrinting:
    """Test Dict pretty printing functionality."""

    def test_empty_dict_pretty(self):
        """Test pretty printing of empty dict."""
        d = PrologDict(())
        assert pretty(d) == "{}"

    def test_single_pair_pretty(self):
        """Test pretty printing of single-pair dict."""
        d = PrologDict(((Atom("a"), Int(1)),))
        assert pretty(d) == "{a:1}"

    def test_multiple_pairs_pretty(self):
        """Test pretty printing of multi-pair dict."""
        d = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        assert pretty(d) == "{a:1, b:2}"

    def test_mixed_key_types_pretty(self):
        """Test pretty printing with mixed atom/int keys."""
        d = PrologDict(((Atom("name"), Atom("alice")), (Int(42), Atom("answer"))))
        assert pretty(d) == "{name:alice, 42:answer}"

    def test_complex_values_pretty(self):
        """Test pretty printing with complex values."""
        d = PrologDict(((Atom("list"), Atom("[]")), (Atom("var"), Var(0, "X"))))
        result = pretty(d)
        # Should contain both pairs in sorted order
        # Note: [] is a reserved token so it gets quoted as '[]'
        assert "list:'[]'" in result
        assert "var:_G0" in result or "var:X" in result
        assert result.startswith("{")
        assert result.endswith("}")


class TestDictRoundTrip:
    """Test round-trip properties with pretty printing."""

    def test_simple_round_trip_structure(self):
        """Test that pretty printing produces parseable syntax."""
        d = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        pretty_str = pretty(d)

        # Should look like valid dict syntax
        assert pretty_str == "{a:1, b:2}"
        assert pretty_str.startswith("{")
        assert pretty_str.endswith("}")
        assert ":" in pretty_str


class TestDictAdvancedFeatures:
    """Test advanced Dict features and edge cases."""

    def test_tag_field_reserved(self):
        """Test that tag field is present and defaults to None."""
        d = PrologDict(((Atom("a"), Int(1)),))
        assert hasattr(d, "tag")
        assert d.tag is None

    def test_nested_dict_values(self):
        """Test dict with nested dict values."""
        inner = PrologDict(((Atom("x"), Int(1)),))
        outer = PrologDict(((Atom("nested"), inner),))

        assert len(outer.pairs) == 1
        assert outer.pairs[0][0] == Atom("nested")
        assert outer.pairs[0][1] == inner

    def test_variable_values_allowed(self):
        """Test that variables are allowed as values (just not keys)."""
        d = PrologDict(((Atom("var"), Var(0, "X")),))
        assert len(d.pairs) == 1
        assert d.pairs[0] == (Atom("var"), Var(0, "X"))
