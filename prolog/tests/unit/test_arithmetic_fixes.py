"""Tests for arithmetic fixes from PR review feedback.

These tests verify:
1. is/2 returns Float for floating-point results
2. Integer division (//) and mod require integer operands
"""

from prolog.ast.terms import Int, Float
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine


class TestArithmeticIsFixes:
    """Test is/2 fixes for proper Float handling."""

    def test_is_with_float_division_result(self):
        """Test that is/2 returns Float for division results."""
        engine = Engine(Program([]))

        # Test float division: 7 / 2 should give 3.5 as Float
        result = list(engine.query("X is 7 / 2"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Float)
        assert result[0]["X"].value == 3.5

    def test_is_with_integer_result(self):
        """Test that is/2 returns Int for integer results."""
        engine = Engine(Program([]))

        # Test integer addition: 3 + 4 should give 7 as Int
        result = list(engine.query("X is 3 + 4"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Int)
        assert result[0]["X"].value == 7

    def test_is_with_float_from_division(self):
        """Test that division produces float results."""
        engine = Engine(Program([]))

        # Even integer division should return Float when using /
        result = list(engine.query("X is 4 / 2"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Float)
        assert result[0]["X"].value == 2.0

    def test_is_with_nested_division(self):
        """Test nested arithmetic with division."""
        engine = Engine(Program([]))

        # Nested arithmetic with division
        result = list(engine.query("X is 1 + 3 / 2"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Float)
        assert result[0]["X"].value == 2.5


class TestIntegerOperationTypeguards:
    """Test type guards for integer-only operations."""

    def test_integer_division_with_float_results_fails(self):
        """Test that integer division (//) fails when operands become floats."""
        engine = Engine(Program([]))

        # Create float operands through division, then try integer division
        # This should fail because 5/1 creates a float
        result = list(engine.query("X is (5 / 1) // 2"))
        assert len(result) == 0  # Should fail

        # Another way: divide then try to use in integer division
        result = list(engine.query("Y is 6 / 2, X is Y // 2"))
        assert len(result) == 0  # Should fail because Y is a float

    def test_integer_division_with_integers_succeeds(self):
        """Test that integer division (//) works with integer operands."""
        engine = Engine(Program([]))

        result = list(engine.query("X is 7 // 2"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Int)
        assert result[0]["X"].value == 3

    def test_mod_with_float_results_fails(self):
        """Test that mod operation fails when operands become floats."""
        engine = Engine(Program([]))

        # Create float operands through division, then try mod
        result = list(engine.query("X is (5 / 1) mod 2"))
        assert len(result) == 0  # Should fail

        # Another way: divide then try to use in mod
        result = list(engine.query("Y is 6 / 2, X is Y mod 2"))
        assert len(result) == 0  # Should fail because Y is a float

    def test_mod_with_integers_succeeds(self):
        """Test that mod operation works with integer operands."""
        engine = Engine(Program([]))

        result = list(engine.query("X is 7 mod 3"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Int)
        assert result[0]["X"].value == 1

    def test_division_with_computed_operands(self):
        """Test that regular division (/) works with computed operands."""
        engine = Engine(Program([]))

        # Division should work with any numeric operands
        result = list(engine.query("Y is 10, X is Y / 2"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Float)
        assert result[0]["X"].value == 5.0

    def test_float_division_even_with_integers(self):
        """Test that / always returns float even with integer operands."""
        engine = Engine(Program([]))

        # Integer / integer should still return Float
        result = list(engine.query("X is 6 / 2"))
        assert len(result) == 1
        assert isinstance(result[0]["X"], Float)
        assert result[0]["X"].value == 3.0
