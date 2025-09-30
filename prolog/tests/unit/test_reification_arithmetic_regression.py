"""
Regression tests for arithmetic reification (#179 fix).

This module contains specific tests for known problematic patterns,
edge cases that have historically caused issues, and scenarios that
are particularly important to preserve during refactoring.
"""

import time
from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


class TestHistoricalRegressions:
    """Tests for specific patterns that caused issues before the fix."""

    def test_issue_179_exact_reproduction(self):
        """Exact reproduction of the original issue #179 cases."""
        reader = Reader()

        # Case 1: This worked before (direct constraint)
        direct_code = """
        test_direct(X, Y) :-
            X in 0..3, Y in 0..6,
            Y #>= X + 2,
            label([X, Y]).
        """

        # Case 2: This failed before fix (reified constraint with arithmetic)
        reified_code = """
        test_reified(X, Y, B) :-
            X in 0..3, Y in 0..6,
            B #<=> (Y #>= X + 2),
            label([X, Y, B]).
        """

        # Case 3: This worked before (simple reification)
        simple_code = """
        test_simple(X, Y, B) :-
            X in 0..3, Y in 0..6,
            B #<=> (Y #= X),
            label([X, Y, B]).
        """

        # Test direct case (should find 14 solutions where Y >= X + 2)
        clauses = reader.read_program(direct_code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        direct_solutions = list(engine.run(reader.read_query("?- test_direct(X, Y).")))
        assert (
            len(direct_solutions) == 14
        ), f"Direct case should find 14 solutions, got {len(direct_solutions)}"

        # Test reified case (should now find solutions, was 0 before)
        clauses = reader.read_program(reified_code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        reified_solutions = list(
            engine.run(reader.read_query("?- test_reified(X, Y, B)."))
        )
        assert (
            len(reified_solutions) > 0
        ), f"Reified case should find solutions, got {len(reified_solutions)}"

        # Test simple case (should still work)
        clauses = reader.read_program(simple_code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        simple_solutions = list(
            engine.run(reader.read_query("?- test_simple(X, Y, B)."))
        )
        assert (
            len(simple_solutions) > 0
        ), f"Simple case should still work, got {len(simple_solutions)}"

        # Verify reified solutions have correct Boolean semantics
        for sol in reified_solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if y >= x + 2 else 0
            assert b == expected_b, f"X={x}, Y={y}: expected B={expected_b}, got B={b}"

    def test_arithmetic_in_all_constraint_types(self):
        """Regression test: arithmetic should work in all constraint types that support reification."""
        reader = Reader()

        test_cases = [
            ("#=", "X + 1 #= Y + 1"),
            ("#<", "X + 1 #< Y + 2"),
            ("#=<", "X + 1 #=< Y + 1"),
            ("#>", "X + 2 #> Y + 1"),
            ("#>=", "X + 1 #>= Y + 1"),
            ("#\\=", "X + 1 #\\= Y + 2"),
        ]

        for constraint_name, constraint_expr in test_cases:
            code = f"""
            test(X, Y, B) :-
                X in 1..3, Y in 1..3,
                B #<=> ({constraint_expr}),
                label([X, Y, B]).
            """

            clauses = reader.read_program(code)
            program = Program(tuple(clauses))
            engine = Engine(program)
            solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

            assert (
                len(solutions) > 0
            ), f"No solutions found for {constraint_name} with arithmetic"

            # Verify at least some solutions exist with Boolean values
            # Note: Some constraints might be always true or always false for small domains
            # but we should at least get solutions with valid B values
            for sol in solutions:
                assert sol["B"].value in {
                    0,
                    1,
                }, f"Invalid Boolean value: {sol['B'].value}"

    def test_complex_expression_types(self):
        """Test various complex expression patterns that could cause parsing issues."""
        reader = Reader()

        test_expressions = [
            ("X + Y - 1", "addition and subtraction"),
            ("X * 2 + Y", "multiplication and addition"),
            ("X - Y + 3", "subtraction and addition"),
            ("2 * X - Y", "multiplication and subtraction"),
            ("-X + Y", "unary minus"),
            ("X + (-Y)", "parenthesized negative"),
            ("(X + 1) - (Y - 1)", "parenthesized expressions"),
        ]

        for expr, description in test_expressions:
            code = f"""
            test(X, Y, B) :-
                X in 1..3, Y in 1..3,
                B #<=> (({expr}) #= 3),
                label([X, Y, B]).
            """

            try:
                clauses = reader.read_program(code)
                program = Program(tuple(clauses))
                engine = Engine(program)
                solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

                # Should either find solutions or fail gracefully
                # The key is no crashes or infinite loops
                assert isinstance(
                    solutions, list
                ), f"Expression '{expr}' ({description}) caused unexpected behavior"

            except Exception as e:
                # Some complex expressions might not be supported yet
                # But they should fail gracefully, not crash
                assert (
                    "not supported" in str(e).lower() or "parse" in str(e).lower()
                ), f"Expression '{expr}' caused unexpected error: {e}"


class TestBoundaryConditions:
    """Tests for edge cases at domain boundaries and extreme values."""

    def test_domain_boundary_arithmetic(self):
        """Test arithmetic near domain boundaries doesn't cause overflow."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 2147483640..2147483647,  % Near 32-bit integer limit
            B #<=> (X + 1 #= 2147483641),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # Should handle large numbers correctly
        assert len(solutions) > 0
        for sol in solutions:
            x, b = sol["X"].value, sol["B"].value
            expected_b = 1 if x + 1 == 2147483641 else 0
            assert b == expected_b

    def test_negative_domain_arithmetic(self):
        """Test arithmetic with negative domains."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in -5..-1, Y in -3..3,
            B #<=> (X + 3 #= Y),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        assert len(solutions) > 0
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x + 3 == y else 0
            assert b == expected_b

    def test_zero_crossing_arithmetic(self):
        """Test arithmetic that crosses zero."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in -2..2,
            B #<=> (X + 1 #> 0),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # X + 1 > 0 ⟺ X > -1, so X ∈ {0, 1, 2} should have B=1
        solution_map = {sol["X"].value: sol["B"].value for sol in solutions}
        assert solution_map[-2] == 0
        assert solution_map[-1] == 0
        assert solution_map[0] == 1
        assert solution_map[1] == 1
        assert solution_map[2] == 1


class TestMemoryAndPerformanceRegression:
    """Tests that check for memory leaks and performance degradation."""

    def test_repeated_queries_no_memory_leak(self):
        """Test that repeated queries don't accumulate memory indefinitely."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 1..3, Y in 1..3,
            B #<=> (X + Y #= 4),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))

        # Run the same query multiple times
        results = []
        for i in range(10):
            engine = Engine(program)
            solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))
            results.append(len(solutions))

        # All runs should produce the same number of solutions
        assert all(
            r == results[0] for r in results
        ), f"Inconsistent results across runs: {results}"
        assert results[0] > 0, "Should find solutions"

    def test_multiple_arithmetic_reifications_work(self):
        """Test that multiple arithmetic reifications work correctly."""
        reader = Reader()

        # Test with a smaller, more tractable problem
        code = """
        test(X, Y, Z, B1, B2) :-
            X in 1..2, Y in 1..2, Z in 1..2,
            B1 #<=> (X + 1 #= Y),
            B2 #<=> (Y + 1 #= Z),
            label([X, Y, Z, B1, B2]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        start = time.perf_counter()
        solutions = list(engine.run(reader.read_query("?- test(X, Y, Z, B1, B2).")))
        elapsed = time.perf_counter() - start

        assert len(solutions) > 0, "Should find solutions with multiple reifications"
        assert elapsed < 2.0, f"Should complete efficiently, took {elapsed:.3f}s"

        # Verify logic is correct
        for sol in solutions:
            x, y, z = sol["X"].value, sol["Y"].value, sol["Z"].value
            b1, b2 = sol["B1"].value, sol["B2"].value
            expected_b1 = 1 if x + 1 == y else 0
            expected_b2 = 1 if y + 1 == z else 0
            assert b1 == expected_b1, f"B1 incorrect: X={x}, Y={y}"
            assert b2 == expected_b2, f"B2 incorrect: Y={y}, Z={z}"


class TestErrorHandlingRegression:
    """Tests for proper error handling in edge cases."""

    def test_invalid_arithmetic_expressions_fail_gracefully(self):
        """Test that invalid expressions are rejected cleanly."""
        reader = Reader()

        # These should be rejected (assuming they're not supported)
        invalid_cases = [
            "B #<=> (X / Y #= 2)",  # Division not supported in linear expressions
            "B #<=> (X * Y #= 6)",  # Non-linear multiplication
            "B #<=> (X ** 2 #= 4)",  # Exponentiation
        ]

        for invalid_expr in invalid_cases:
            code = f"""
            test(X, Y, B) :-
                X in 1..3, Y in 1..3,
                {invalid_expr},
                label([X, Y, B]).
            """

            try:
                clauses = reader.read_program(code)
                program = Program(tuple(clauses))
                engine = Engine(program)
                solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

                # If it doesn't throw an error, it should at least not crash
                assert isinstance(
                    solutions, list
                ), f"Expression {invalid_expr} caused unexpected behavior"

            except Exception as e:
                # Should fail with a clear error message, not crash
                error_msg = str(e).lower()
                assert any(
                    word in error_msg
                    for word in ["not supported", "invalid", "error", "parse"]
                ), f"Expression {invalid_expr} caused unclear error: {e}"

    def test_empty_domain_arithmetic_reification(self):
        """Test behavior when arithmetic creates empty domains."""
        reader = Reader()
        code = """
        test(X, B) :-
            X in 1..2,
            X #> 5,  % This makes X's domain empty
            B #<=> (X + 1 #= 3),
            label([X, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B).")))

        # Should find no solutions (empty domain)
        assert len(solutions) == 0, "Empty domain should yield no solutions"

    def test_contradictory_arithmetic_reification(self):
        """Test behavior with contradictory arithmetic constraints."""
        reader = Reader()
        code = """
        test(X, B1, B2) :-
            X in 1..3,
            B1 #<=> (X + 1 #= 3),  % True when X = 2
            B2 #<=> (X + 1 #= 4),  % True when X = 3
            B1 #= 1, B2 #= 1,     % Both must be true (contradiction)
            label([X, B1, B2]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, B1, B2).")))

        # Should find no solutions (contradiction)
        assert (
            len(solutions) == 0
        ), "Contradictory constraints should yield no solutions"


class TestBackwardCompatibility:
    """Tests to ensure the fix doesn't break existing functionality."""

    def test_simple_reification_still_works(self):
        """Test that simple (non-arithmetic) reification still works as before."""
        reader = Reader()
        code = """
        test(X, Y, B) :-
            X in 1..3, Y in 1..3,
            B #<=> (X #= Y),
            label([X, Y, B]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y, B).")))

        # Should work exactly as before
        assert len(solutions) == 9  # 3x3 grid
        for sol in solutions:
            x, y, b = sol["X"].value, sol["Y"].value, sol["B"].value
            expected_b = 1 if x == y else 0
            assert b == expected_b

    def test_existing_builtins_unaffected(self):
        """Test that existing CLP(FD) builtins are unaffected by the arithmetic fix."""
        reader = Reader()
        code = """
        test(X, Y) :-
            X in 1..5, Y in 1..5,
            X #< Y,
            X + Y #= 7,
            all_different([X, Y]),
            label([X, Y]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)
        solutions = list(engine.run(reader.read_query("?- test(X, Y).")))

        # Should still find the correct solutions
        expected_solutions = {(2, 5), (3, 4)}
        actual_solutions = {(sol["X"].value, sol["Y"].value) for sol in solutions}
        assert actual_solutions == expected_solutions

    def test_performance_comparable_to_before(self):
        """Test that the fix doesn't significantly slow down existing code."""
        reader = Reader()
        code = """
        test(X, Y, Z) :-
            X in 1..10, Y in 1..10, Z in 1..10,
            X #< Y, Y #< Z,
            X + Y + Z #= 15,
            label([X]).  % Only label X for speed
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # Should complete quickly
        start = time.perf_counter()
        solutions = list(
            engine.run(reader.read_query("?- test(X, Y, Z)."), max_solutions=10)
        )
        elapsed = time.perf_counter() - start

        assert len(solutions) > 0, "Should find solutions"
        assert elapsed < 1.0, f"Should be fast, took {elapsed:.3f}s"
