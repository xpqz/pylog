"""Tests for max() and min() functions in arithmetic and CLP(FD) contexts.

This module tests the implementation of max() and min() functions that were
added to fix issue #191.
"""

import pytest
from prolog.engine.engine import Engine


class TestArithmeticMaxMin:
    """Test max() and min() with the 'is' operator (pure arithmetic)."""

    def test_max_with_integers(self):
        """Test max() with integer literals."""
        engine = Engine("")

        result = list(engine.query("X is max(3, 7)"))
        assert len(result) == 1
        assert result[0]["X"].value == 7

    def test_min_with_integers(self):
        """Test min() with integer literals."""
        engine = Engine("")

        result = list(engine.query("X is min(3, 7)"))
        assert len(result) == 1
        assert result[0]["X"].value == 3

    def test_max_equal_values(self):
        """Test max() with equal values."""
        engine = Engine("")

        result = list(engine.query("X is max(5, 5)"))
        assert len(result) == 1
        assert result[0]["X"].value == 5

    def test_min_equal_values(self):
        """Test min() with equal values."""
        engine = Engine("")

        result = list(engine.query("X is min(5, 5)"))
        assert len(result) == 1
        assert result[0]["X"].value == 5

    def test_max_with_negative_numbers(self):
        """Test max() with negative numbers."""
        engine = Engine("")

        result = list(engine.query("X is max(-3, -7)"))
        assert len(result) == 1
        assert result[0]["X"].value == -3

    def test_min_with_negative_numbers(self):
        """Test min() with negative numbers."""
        engine = Engine("")

        result = list(engine.query("X is min(-3, -7)"))
        assert len(result) == 1
        assert result[0]["X"].value == -7

    def test_nested_max_expressions(self):
        """Test nested max() expressions."""
        engine = Engine("")

        result = list(engine.query("X is max(1, max(2, 3))"))
        assert len(result) == 1
        assert result[0]["X"].value == 3

    def test_nested_min_expressions(self):
        """Test nested min() expressions."""
        engine = Engine("")

        result = list(engine.query("X is min(5, min(2, 3))"))
        assert len(result) == 1
        assert result[0]["X"].value == 2

    def test_max_in_complex_expression(self):
        """Test max() as part of a larger arithmetic expression."""
        engine = Engine("")

        result = list(engine.query("X is max(2, 3) + max(4, 5)"))
        assert len(result) == 1
        assert result[0]["X"].value == 8  # 3 + 5

    def test_min_in_complex_expression(self):
        """Test min() as part of a larger arithmetic expression."""
        engine = Engine("")

        result = list(engine.query("X is min(2, 3) * min(4, 5)"))
        assert len(result) == 1
        assert result[0]["X"].value == 8  # 2 * 4


class TestCLPFDMaxMin:
    """Test max() and min() with CLP(FD) constraints."""

    def test_max_with_fd_integers(self):
        """Test max() with FD constraint on integer literals."""
        engine = Engine("")

        result = list(engine.query("X #= max(3, 7), label([X])"))
        assert len(result) == 1
        assert result[0]["X"].value == 7

    def test_min_with_fd_integers(self):
        """Test min() with FD constraint on integer literals."""
        engine = Engine("")

        result = list(engine.query("X #= min(3, 7), label([X])"))
        assert len(result) == 1
        assert result[0]["X"].value == 3

    def test_max_with_constrained_variables(self):
        """Test max() with variables constrained to specific values."""
        engine = Engine("")

        result = list(engine.query("A #= 5, B #= 8, X #= max(A, B), label([A, B, X])"))
        assert len(result) == 1
        assert result[0]["A"].value == 5
        assert result[0]["B"].value == 8
        assert result[0]["X"].value == 8

    def test_min_with_constrained_variables(self):
        """Test min() with variables constrained to specific values."""
        engine = Engine("")

        result = list(engine.query("A #= 5, B #= 8, X #= min(A, B), label([A, B, X])"))
        assert len(result) == 1
        assert result[0]["A"].value == 5
        assert result[0]["B"].value == 8
        assert result[0]["X"].value == 5

    def test_nested_max_with_fd(self):
        """Test nested max() with FD constraints."""
        engine = Engine("")

        result = list(
            engine.query(
                "A #= 2, B #= 5, C #= 3, X #= max(A, max(B, C)), label([A, B, C, X])"
            )
        )
        assert len(result) == 1
        assert result[0]["X"].value == 5

    def test_nested_min_with_fd(self):
        """Test nested min() with FD constraints."""
        engine = Engine("")

        result = list(
            engine.query(
                "A #= 2, B #= 5, C #= 3, X #= min(A, min(B, C)), label([A, B, C, X])"
            )
        )
        assert len(result) == 1
        assert result[0]["X"].value == 2

    def test_max_with_arithmetic_expressions(self):
        """Test max() with arithmetic expressions as arguments."""
        engine = Engine("")

        result = list(
            engine.query("A #= 3, B #= 4, X #= max(A + 1, B * 2), label([A, B, X])")
        )
        assert len(result) == 1
        assert result[0]["X"].value == 8  # max(3+1, 4*2) = max(4, 8) = 8

    def test_min_with_arithmetic_expressions(self):
        """Test min() with arithmetic expressions as arguments."""
        engine = Engine("")

        result = list(
            engine.query("A #= 3, B #= 4, X #= min(A + 5, B + 1), label([A, B, X])")
        )
        assert len(result) == 1
        assert result[0]["X"].value == 5  # min(3+5, 4+1) = min(8, 5) = 5

    def test_max_min_combination(self):
        """Test combining max() and min() in the same expression."""
        engine = Engine("")

        result = list(
            engine.query(
                "A #= 2, B #= 7, C #= 4, X #= max(A, min(B, C)), label([A, B, C, X])"
            )
        )
        assert len(result) == 1
        assert result[0]["X"].value == 4  # max(2, min(7, 4)) = max(2, 4) = 4

    @pytest.mark.skip(
        reason="max() with non-singleton domain variables not yet implemented"
    )
    def test_max_with_domain_variables(self):
        """Test max() where variables have domains rather than fixed values.

        NOTE: This test is skipped because the current implementation only handles
        ground arithmetic expressions (where all variables have singleton domains).
        Supporting max() with general domain variables would require a specialized
        max constraint propagator.
        """
        engine = Engine("")

        # A can be 1 or 2, B is fixed at 3, so max should be 3
        result = list(
            engine.query("A in 1..2, B #= 3, X #= max(A, B), label([A, B, X])")
        )
        assert len(result) == 2
        for sol in result:
            assert sol["B"].value == 3
            assert sol["X"].value == 3  # max(1|2, 3) = 3

    def test_scheduling_scenario(self):
        """Test max() in a scheduling-like scenario."""
        engine = Engine("")

        # Task end times
        query = """
        S1 #= 0, S2 #= 5,
        D1 #= 3, D2 #= 2,
        E1 #= S1 + D1,
        E2 #= S2 + D2,
        MaxEnd #= max(E1, E2),
        label([S1, S2, D1, D2, E1, E2, MaxEnd])
        """

        result = list(engine.query(query))
        assert len(result) == 1
        assert result[0]["E1"].value == 3  # 0 + 3
        assert result[0]["E2"].value == 7  # 5 + 2
        assert result[0]["MaxEnd"].value == 7  # max(3, 7)


class TestMaxMinEdgeCases:
    """Test edge cases and error conditions for max() and min()."""

    def test_max_with_zero(self):
        """Test max() with zero."""
        engine = Engine("")

        result = list(engine.query("X is max(0, -5)"))
        assert len(result) == 1
        assert result[0]["X"].value == 0

    def test_min_with_zero(self):
        """Test min() with zero."""
        engine = Engine("")

        result = list(engine.query("X is min(0, 5)"))
        assert len(result) == 1
        assert result[0]["X"].value == 0

    def test_max_with_large_numbers(self):
        """Test max() with large numbers."""
        engine = Engine("")

        result = list(engine.query("X is max(1000000, 999999)"))
        assert len(result) == 1
        assert result[0]["X"].value == 1000000

    def test_deeply_nested_max(self):
        """Test deeply nested max() expressions."""
        engine = Engine("")

        result = list(engine.query("X is max(1, max(2, max(3, max(4, 5))))"))
        assert len(result) == 1
        assert result[0]["X"].value == 5

    def test_deeply_nested_min(self):
        """Test deeply nested min() expressions."""
        engine = Engine("")

        result = list(engine.query("X is min(5, min(4, min(3, min(2, 1))))"))
        assert len(result) == 1
        assert result[0]["X"].value == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
