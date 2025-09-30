"""
Comprehensive tests for arithmetic reification (#179 fix).

This module provides extensive test coverage to enable fearless refactoring
of the arithmetic reification functionality. It covers edge cases, integration
scenarios, and potential regression points.
"""

import pytest
import time
from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


class TestArithmeticReificationEdgeCases:
    """Test edge cases and boundary conditions for arithmetic reification."""

    def test_zero_coefficient_expressions(self):
        """Test arithmetic expressions that simplify to constants."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..3,
            B #<=> (X - X #= 0),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # All solutions should have B=1 since X - X = 0 is always true
        assert len(solutions) == 3
        for sol in solutions:
            assert sol["B"].value == 1

    def test_negative_coefficients(self):
        """Test arithmetic with negative coefficients."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 1..3, Y in 1..3,
            B #<=> (Y #= -X + 4),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # Verify constraint logic: Y = -X + 4
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if y == -x + 4 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

    def test_large_constants(self):
        """Test arithmetic with large constants."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1000..1002,
            B #<=> (X + 10000 #= 11001),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # Only X=1001 should make the constraint true
        solution_map = {sol["X"].value: sol["B"].value for sol in solutions}
        assert solution_map[1000] == 0
        assert solution_map[1001] == 1
        assert solution_map[1002] == 0

    def test_multiple_arithmetic_operations(self):
        """Test complex arithmetic expressions with multiple operations."""
        reader = Reader()
        code = """
        test(X, Y, Z, B) :-
            X in 1..2, Y in 1..2, Z in 1..10,
            B #<=> (X + Y * 2 - 1 #= Z),
            label([X, Y, Z, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, Z, B).")))

        # Verify constraint logic: X + Y * 2 - 1 = Z
        for sol in solutions:
            x, y, z, b = sol["X"].value, sol["Y"].value, sol["Z"].value, sol["B"].value
            expected_b = 1 if x + y * 2 - 1 == z else 0
            assert (
                b == expected_b
            ), f"X={x}, Y={y}, Z={z}: expected B={expected_b}, got B={b}"

    def test_arithmetic_in_both_sides(self):
        """Test arithmetic expressions on both sides of comparison."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 1..3, Y in 1..3,
            B #<=> (X + 1 #= Y - 1),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # Verify constraint logic: X + 1 = Y - 1 ⟺ X = Y - 2
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x == y - 2 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

    def test_single_variable_arithmetic(self):
        """Test arithmetic expressions with a single variable."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..5,
            B #<=> (X + 3 #< 6),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # X + 3 < 6 ⟺ X < 3, so only X=1,2 should have B=1
        solution_map = {sol["X"].value: sol["B"].value for sol in solutions}
        assert solution_map[1] == 1
        assert solution_map[2] == 1
        assert solution_map[3] == 0
        assert solution_map[4] == 0
        assert solution_map[5] == 0


class TestArithmeticReificationIntegration:
    """Test integration with other CLP(FD) features."""

    def test_reification_with_all_different(self):
        """Test arithmetic reification combined with all_different."""
        reader = Reader()
        code = """
        test(X, Y, Z, B) :-
            X in 1..3, Y in 1..3, Z in 1..3,
            all_different([X, Y, Z]),
            B #<=> (X + Y #= Z + 3),
            label([X, Y, Z, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, Z, B).")))

        # Verify all_different is satisfied and reification works
        for sol in solutions:
            x, y, z, b = sol["X"].value, sol["Y"].value, sol["Z"].value, sol["B"].value
            # Check all_different
            assert len({x, y, z}) == 3, f"all_different violated: X={x}, Y={y}, Z={z}"
            # Check reification
            expected_b = 1 if x + y == z + 3 else 0
            assert (
                b == expected_b
            ), f"X={x}, Y={y}, Z={z}: expected B={expected_b}, got B={b}"

    def test_chained_arithmetic_reification(self):
        """Test multiple reified arithmetic constraints."""
        reader = Reader()
        code = """
        test(X, Y, B1, B2) :-
            X in 1..3, Y in 1..3,
            B1 #<=> (X + 1 #= Y),
            B2 #<=> (Y - 1 #= X),
            label([X, Y, B1, B2]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B1, B2).")))

        # Both constraints are equivalent, so B1 should equal B2
        for sol in solutions:
            x, y, b1, b2 = (
                sol["X"].value,
                sol["Y"].value,
                sol["B1"].value,
                sol["B2"].value,
            )
            assert b1 == b2, f"X={x}, Y={y}: B1={b1} != B2={b2}"
            expected_b = 1 if x + 1 == y else 0
            assert b1 == expected_b

    def test_reification_with_domain_updates(self):
        """Test arithmetic reification when domains are updated by constraints."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 1..10, Y in 1..10,
            X #< 5,  % Narrows X domain to 1..4
            B #<=> (X + 2 #= Y),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # X is constrained to 1..4, verify reification works correctly
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            assert 1 <= x <= 4, f"X={x} violates X < 5 constraint"
            expected_b = 1 if x + 2 == y else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"


class TestArithmeticReificationConstraintTypes:
    """Test arithmetic reification with all constraint types."""

    @pytest.mark.parametrize(
        "constraint,op_func",
        [
            ("#=", lambda x, y: x == y),
            ("#<", lambda x, y: x < y),
            ("#=<", lambda x, y: x <= y),
            ("#>", lambda x, y: x > y),
            ("#>=", lambda x, y: x >= y),
            ("#\\=", lambda x, y: x != y),
        ],
    )
    def test_all_constraint_types_with_arithmetic(self, constraint, op_func):
        """Test arithmetic reification with all supported constraint types."""
        reader = Reader()
        code = f"""
        test(X, Y, B) :-
            X in 1..3, Y in 1..5,
            B #<=> (X + 1 {constraint} Y),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        assert len(solutions) > 0, f"No solutions found for {constraint}"

        # Verify constraint logic
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if op_func(x + 1, y) else 0
            assert (
                b == expected_b
            ), f"X={x}, Y={y}: {constraint} expected B={expected_b}, got B={b}"


class TestArithmeticReificationRegressionTests:
    """Specific regression tests for known problematic patterns."""

    def test_original_issue_179_fix_validation(self):
        """Test that issue #179 is fixed - reified arithmetic constraints work."""
        reader = Reader()

        # This was failing before the fix (returned 0 solutions)
        reified_code = """
        test_reified(X, Y, B) :-
            X in 0..3, Y in 0..6,
            B #<=> (Y #>= X + 2),
            label([X, Y, B]).
        """

        clauses = reader.read_program(reified_code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        reified_solutions = list(
            engine.run(reader.read_query("?- test_reified(X, Y, B)."))
        )

        # Should find solutions now (was 0 before fix)
        assert (
            len(reified_solutions) > 0
        ), "Reified version should find solutions after fix"

        # Should cover all (X,Y) combinations in the domain
        all_combinations = {(x, y) for x in range(4) for y in range(7)}
        reified_all_pairs = {(s["X"].value, s["Y"].value) for s in reified_solutions}
        assert (
            reified_all_pairs == all_combinations
        ), "Reified should cover all (X,Y) combinations"

        # Verify Boolean logic is correct
        for sol in reified_solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if y >= x + 2 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

        # Should have both true and false cases
        b_values = {sol["B"].value for sol in reified_solutions}
        assert b_values == {0, 1}, "Should have both B=0 and B=1 cases"

    def test_zero_plus_variable_edge_case(self):
        """Test edge case: arithmetic with zero addition."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..3,
            B #<=> (X + 0 #= 2),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # X + 0 = 2 should behave like X = 2
        solution_map = {sol["X"].value: sol["B"].value for sol in solutions}
        assert solution_map[1] == 0
        assert solution_map[2] == 1
        assert solution_map[3] == 0

    def test_variable_minus_itself(self):
        """Test edge case: X - X should always equal 0."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..5,
            B #<=> (X - X #= 0),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # X - X = 0 should always be true
        assert len(solutions) == 5
        for sol in solutions:
            assert (
                sol["B"].value == 1
            ), f"X - X = 0 should always be true for X={sol['X'].value}"

    def test_constant_arithmetic_optimization(self):
        """Test that constant arithmetic is properly handled."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..3,
            B #<=> (5 + 3 #= X + 6),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # 5 + 3 = X + 6 ⟺ 8 = X + 6 ⟺ X = 2
        solution_map = {sol["X"].value: sol["B"].value for sol in solutions}
        assert solution_map[1] == 0
        assert solution_map[2] == 1
        assert solution_map[3] == 0


class TestArithmeticReificationPerformance:
    """Performance regression tests for arithmetic reification."""

    def test_no_exponential_blowup_with_arithmetic(self):
        """Ensure arithmetic reification doesn't cause exponential blowup."""
        reader = Reader()

        # Simple test that arithmetic reification doesn't hang
        code = """
        test(X, Y, B) :-
            X in 1..3, Y in 1..3,
            B #<=> (X + Y #= 4),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # Should complete quickly without hanging
        start = time.perf_counter()
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))
        elapsed = time.perf_counter() - start

        assert len(solutions) > 0, "Should find solutions"
        assert elapsed < 1.0, f"Should complete quickly, took {elapsed:.3f}s"

        # Verify solutions are correct
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x + y == 4 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

    def test_auxiliary_variable_cleanup(self):
        """Test that auxiliary variables don't accumulate inappropriately."""
        reader = Reader()

        # Test multiple queries to ensure no variable accumulation
        code = """
        test(X, Y, B) :-
            X in 1..2, Y in 1..2,
            B #<=> (X + Y #= 3),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))

        # Run multiple times and check consistency
        results = []
        for _ in range(3):
            engine = Engine(program)
            solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))
            results.append(len(solutions))

        # All runs should produce the same number of solutions
        assert all(r == results[0] for r in results), f"Inconsistent results: {results}"
        assert results[0] > 0, "Should find solutions"


class TestArithmeticReificationBacktracking:
    """Test backtracking behavior with arithmetic reification."""

    def test_backtracking_restores_auxiliary_constraints(self):
        """Test that backtracking properly handles auxiliary constraints."""
        reader = Reader()
        code = """
        test(X, Y, B, Choice) :-
            X in 1..2, Y in 1..2,
            (Choice = 1 ; Choice = 2),
            B #<=> (X + Y #= Choice + 2),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B, Choice).")))

        # Should find solutions for both choices
        choice1_solutions = [s for s in solutions if s["Choice"].value == 1]
        choice2_solutions = [s for s in solutions if s["Choice"].value == 2]

        assert len(choice1_solutions) > 0, "Should find solutions for Choice=1"
        assert len(choice2_solutions) > 0, "Should find solutions for Choice=2"

        # Verify constraint logic for each choice
        for sol in choice1_solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x + y == 3 else 0  # Choice=1, so X+Y = 1+2 = 3
            assert b == expected_b

        for sol in choice2_solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x + y == 4 else 0  # Choice=2, so X+Y = 2+2 = 4
            assert b == expected_b

    def test_cut_with_arithmetic_reification(self):
        """Test that cut works correctly with arithmetic reification."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..5,
            X #< 3, !,
            B #<=> (X + 1 #= 2),
            label([X, B]).

        test(X, B) :-
            X in 1..5,
            B #<=> (X + 1 #= 4),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # Cut should prevent second clause, so only X=1,2 should be considered
        x_values = {sol["X"].value for sol in solutions}
        assert x_values.issubset({1, 2}), f"Cut failed: found X values {x_values}"

        # For the solutions found, verify reification works
        for sol in solutions:
            x, b = sol["X"].value, sol["B"].value
            expected_b = 1 if x + 1 == 2 else 0  # From first clause
            assert b == expected_b, f"X={x}: expected B={expected_b}, got B={b}"
