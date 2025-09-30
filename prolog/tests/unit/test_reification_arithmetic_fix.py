"""
Tests for issue #179 fix: Reification of arithmetic constraints.

This module tests that arithmetic expressions in reified constraints
work correctly after the fix for issue #179.
"""

from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


class TestReificationArithmeticFix:
    """Tests for reification arithmetic constraint fix."""

    def test_simple_arithmetic_addition(self):
        """Test B #<=> (Y #>= X + 2) works correctly."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 0..2, Y in 0..4,
            B #<=> (Y #>= X + 2),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # Should find solutions (was 0 before fix)
        assert len(solutions) > 0

        # Verify some expected solutions
        solution_set = {(s["X"].value, s["Y"].value, s["B"].value) for s in solutions}

        # When X=0, Y>=2: B should be 1
        assert (0, 2, 1) in solution_set
        assert (0, 3, 1) in solution_set
        assert (0, 4, 1) in solution_set

        # When X=0, Y<2: B should be 0
        assert (0, 0, 0) in solution_set
        assert (0, 1, 0) in solution_set

    def test_arithmetic_subtraction(self):
        """Test B #<=> (Y #>= X - 1) works correctly."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 1..3, Y in 0..2,
            B #<=> (Y #>= X - 1),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # Should find solutions (was 0 before fix)
        assert len(solutions) > 0

        # Verify constraint logic
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if y >= x - 1 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

    def test_nested_arithmetic(self):
        """Test B #<=> (X + 1 #= Y + 2) works correctly."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 0..3, Y in 0..3,
            B #<=> (X + 1 #= Y + 2),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # Should find solutions (was 0 before fix)
        assert len(solutions) > 0

        # Verify constraint logic: X + 1 = Y + 2 ⟺ X = Y + 1
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x == y + 1 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

    def test_arithmetic_vs_simple_consistency(self):
        """Test that arithmetic and simple reification give same results when equivalent."""
        reader = Reader()

        # Simple case: B #<=> (X #= Y)
        simple_code = """
        test_simple(X, Y, B) :-
            X in 0..2, Y in 0..2,
            B #<=> (X #= Y),
            label([X, Y, B]).
        """

        # Arithmetic case: B #<=> (X + 0 #= Y + 0)
        arith_code = """
        test_arith(X, Y, B) :-
            X in 0..2, Y in 0..2,
            B #<=> (X + 0 #= Y + 0),
            label([X, Y, B]).
        """

        # Test simple case
        clauses = reader.read_program(simple_code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        simple_solutions = set(
            (s["X"].value, s["Y"].value, s["B"].value)
            for s in engine.run(reader.read_query("?- test_simple(X, Y, B)."))
        )

        # Test arithmetic case
        clauses = reader.read_program(arith_code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        arith_solutions = set(
            (s["X"].value, s["Y"].value, s["B"].value)
            for s in engine.run(reader.read_query("?- test_arith(X, Y, B)."))
        )

        # Both should produce the same solutions
        assert simple_solutions == arith_solutions
        assert len(simple_solutions) > 0  # Sanity check
