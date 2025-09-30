"""Tests for issue #180: Arithmetic constraints with comparison operators bug.

This module demonstrates that arithmetic constraints using comparison operators
(#>=, #>, #<, #=<) with arithmetic expressions return ALL domain combinations
instead of filtering for solutions that satisfy the constraint.
"""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


class TestArithmeticComparisonBug:
    """Test cases demonstrating the arithmetic comparison bug."""

    def test_simple_comparison_works(self):
        """Baseline: Simple comparisons without arithmetic work correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # X #< Y should give only X=1, Y=2
        query = reader.read_term("X in 1..2, Y in 1..2, X #< Y, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 1
        sol = solutions[0]
        assert sol.get("X").value == 1
        assert sol.get("Y").value == 2

    def test_arithmetic_equality_works(self):
        """Baseline: Arithmetic equality constraints work correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # X + Y #= 3 should give X=1,Y=2 and X=2,Y=1
        query = reader.read_term("X in 1..2, Y in 1..2, X + Y #= 3, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 2
        solution_set = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert (1, 2) in solution_set
        assert (2, 1) in solution_set

    def test_arithmetic_gte_bug(self):
        """FIXED: Y #>= X + 2 now works correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Y #>= X + 2 with X in 0..2, Y in 0..4
        # Expected: 6 solutions where constraint is satisfied
        query = reader.read_term("X in 0..2, Y in 0..4, Y #>= X + 2, label([X, Y])")
        solutions = list(engine.solve(query))

        # Verify the constraint works correctly
        assert len(solutions) == 6, f"Expected 6 solutions, got {len(solutions)}"

        # All solutions should satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert (
                y_val >= x_val + 2
            ), f"Solution ({x_val}, {y_val}) violates Y >= X + 2"

        # Verify we get exactly the expected solutions
        expected_solutions = {(0, 2), (0, 3), (0, 4), (1, 3), (1, 4), (2, 4)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_arithmetic_gt_bug(self):
        """FIXED: Y #> X + 1 now works correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Y #> X + 1 with X in 0..2, Y in 0..3
        query = reader.read_term("X in 0..2, Y in 0..3, Y #> X + 1, label([X, Y])")
        solutions = list(engine.solve(query))

        # Should return exactly 3 solutions where constraint is satisfied
        assert len(solutions) == 3, f"Expected 3 solutions, got {len(solutions)}"

        # All solutions should satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert y_val > x_val + 1, f"Solution ({x_val}, {y_val}) violates Y > X + 1"

        # Verify we get exactly the expected solutions
        expected_solutions = {(0, 2), (0, 3), (1, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_arithmetic_lt_bug(self):
        """FIXED: X #< Y - 1 now works correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # X #< Y - 1 with X in 0..2, Y in 1..3
        query = reader.read_term("X in 0..2, Y in 1..3, X #< Y - 1, label([X, Y])")
        solutions = list(engine.solve(query))

        # Should return exactly 3 solutions where constraint is satisfied
        assert len(solutions) == 3, f"Expected 3 solutions, got {len(solutions)}"

        # All solutions should satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert x_val < y_val - 1, f"Solution ({x_val}, {y_val}) violates X < Y - 1"

        # Verify we get exactly the expected solutions
        expected_solutions = {(0, 2), (0, 3), (1, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_arithmetic_le_bug(self):
        """FIXED: X #=< Y - 2 now works correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # X #=< Y - 2 with X in 0..2, Y in 2..4
        query = reader.read_term("X in 0..2, Y in 2..4, X #=< Y - 2, label([X, Y])")
        solutions = list(engine.solve(query))

        # Should return exactly 6 solutions where constraint is satisfied
        assert len(solutions) == 6, f"Expected 6 solutions, got {len(solutions)}"

        # All solutions should satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert (
                x_val <= y_val - 2
            ), f"Solution ({x_val}, {y_val}) violates X <= Y - 2"

        # Verify we get exactly the expected solutions
        expected_solutions = {(0, 2), (0, 3), (0, 4), (1, 3), (1, 4), (2, 4)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_reified_arithmetic_works(self):
        """Baseline: Reified arithmetic constraints work correctly (fixed in #179)."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # B #<=> (Y #>= X + 2) should correctly identify valid solutions
        query = reader.read_term(
            "X in 0..2, Y in 0..4, B #<=> (Y #>= X + 2), label([X, Y, B])"
        )
        solutions = list(engine.solve(query))

        # Should have 15 total solutions (all X,Y combinations with B value)
        assert len(solutions) == 15

        # Filter for cases where B=1 (constraint satisfied)
        true_solutions = [
            (s.get("X").value, s.get("Y").value)
            for s in solutions
            if s.get("B").value == 1
        ]

        # Should have exactly the 6 cases where Y >= X + 2
        expected_true = {(0, 2), (0, 3), (0, 4), (1, 3), (1, 4), (2, 4)}
        assert set(true_solutions) == expected_true


@pytest.mark.xfail(reason="Issue #180: Direct arithmetic comparison constraints broken")
class TestArithmeticComparisonFixed:
    """Test cases for what should work once issue #180 is fixed."""

    def test_arithmetic_gte_fixed(self):
        """Y #>= X + 2 should return only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 0..4, Y #>= X + 2, label([X, Y])")
        solutions = list(engine.solve(query))

        # Should return exactly 6 solutions
        assert len(solutions) == 6

        expected_solutions = {(0, 2), (0, 3), (0, 4), (1, 3), (1, 4), (2, 4)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_arithmetic_gt_fixed(self):
        """Y #> X + 1 should return only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 0..3, Y #> X + 1, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 3
        expected_solutions = {(0, 2), (0, 3), (1, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_arithmetic_lt_fixed(self):
        """X #< Y - 1 should return only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 1..3, X #< Y - 1, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 3
        expected_solutions = {(0, 2), (0, 3), (1, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

    def test_arithmetic_le_fixed(self):
        """X #=< Y - 2 should return only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 2..4, X #=< Y - 2, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 6
        expected_solutions = {(0, 2), (0, 3), (0, 4), (1, 3), (1, 4), (2, 4)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions
