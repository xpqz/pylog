"""Tests for issue #180: Arithmetic constraints with comparison operators - FIXED.

This module demonstrates that arithmetic constraints using comparison operators
(#>=, #>, #<, #=<) with arithmetic expressions now work correctly after the fix.
"""

from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


class TestArithmeticComparisonFixed:
    """Test cases demonstrating that arithmetic comparison constraints work correctly."""

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

    def test_arithmetic_gte_fixed(self):
        """FIXED: Y #>= X + 2 returns only solutions where constraint is satisfied."""
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

        # Verify all solutions satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert (
                y_val >= x_val + 2
            ), f"Solution ({x_val}, {y_val}) violates Y >= X + 2"

    def test_arithmetic_gt_fixed(self):
        """FIXED: Y #> X + 1 returns only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 0..3, Y #> X + 1, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 3
        expected_solutions = {(0, 2), (0, 3), (1, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

        # Verify all solutions satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert y_val > x_val + 1, f"Solution ({x_val}, {y_val}) violates Y > X + 1"

    def test_arithmetic_lt_fixed(self):
        """FIXED: X #< Y - 1 returns only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 1..3, X #< Y - 1, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 3
        expected_solutions = {(0, 2), (0, 3), (1, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

        # Verify all solutions satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert x_val < y_val - 1, f"Solution ({x_val}, {y_val}) violates X < Y - 1"

    def test_arithmetic_le_fixed(self):
        """FIXED: X #=< Y - 2 returns only solutions where constraint is satisfied."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term("X in 0..2, Y in 2..4, X #=< Y - 2, label([X, Y])")
        solutions = list(engine.solve(query))

        assert len(solutions) == 6
        expected_solutions = {(0, 2), (0, 3), (0, 4), (1, 3), (1, 4), (2, 4)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

        # Verify all solutions satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert (
                x_val <= y_val - 2
            ), f"Solution ({x_val}, {y_val}) violates X <= Y - 2"

    def test_reified_arithmetic_still_works(self):
        """Baseline: Reified arithmetic constraints continue to work correctly."""
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

    def test_complex_arithmetic_expressions(self):
        """Test more complex arithmetic expressions in constraints."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Test: 2*X + Y #>= 5
        query = reader.read_term("X in 1..2, Y in 1..3, 2*X + Y #>= 5, label([X, Y])")
        solutions = list(engine.solve(query))

        expected_solutions = {
            (1, 3),
            (2, 1),
            (2, 2),
            (2, 3),
        }  # Solutions where 2*X + Y >= 5
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions

        # Verify all solutions satisfy the constraint
        for sol in solutions:
            x_val = sol.get("X").value
            y_val = sol.get("Y").value
            assert (
                2 * x_val + y_val >= 5
            ), f"Solution ({x_val}, {y_val}) violates 2*X + Y >= 5"

    def test_mixed_arithmetic_and_simple_constraints(self):
        """Test mixing arithmetic and simple constraints."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # X #< Y, Y #>= X + 1 (should be equivalent to X #< Y since Y >= X+1 is weaker)
        query = reader.read_term(
            "X in 1..3, Y in 1..3, X #< Y, Y #>= X + 1, label([X, Y])"
        )
        solutions = list(engine.solve(query))

        # Same as X #< Y: (1,2), (1,3), (2,3)
        expected_solutions = {(1, 2), (1, 3), (2, 3)}
        actual_solutions = {(s.get("X").value, s.get("Y").value) for s in solutions}
        assert actual_solutions == expected_solutions
