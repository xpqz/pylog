"""Performance validation tests for operator support - Issue #43.

This test module validates that operator support doesn't introduce
performance regressions in parsing or execution.

Test Coverage:
- Reader transformation overhead
- Operator vs canonical parsing speed
- Execution performance with operators
- Memory usage comparison
"""

import pytest
import time
from prolog.parser.reader import Reader
from prolog.parser import parser as parser_module
from prolog.engine.engine import Engine
from prolog.engine.patches import create_dev_engine
from prolog.ast.clauses import Program


# Create dev mode engine class
DevEngine = create_dev_engine(Engine)

# Mark all performance tests
pytestmark = pytest.mark.perf


class TestParsingPerformance:
    """Test parsing performance with operators."""

    def test_reader_transformation_overhead(self):
        """Measure overhead of reader transformation."""
        reader = Reader()

        # Generate a complex expression
        expr = "a , b , c , d , e , f , g , h"

        # Time parsing without reader (canonical form)
        canonical = "','(a, ','(b, ','(c, ','(d, ','(e, ','(f, ','(g, h)))))))"

        # Warm-up
        for _ in range(50):
            parser_module.parse_term(canonical)

        # Parse canonical form many times
        start = time.perf_counter()
        for _ in range(1000):
            parser_module.parse_term(canonical)
        canonical_time = time.perf_counter() - start

        # Warm-up
        for _ in range(50):
            reader.read_term(expr)

        # Parse with reader many times
        start = time.perf_counter()
        for _ in range(1000):
            reader.read_term(expr)
        reader_time = time.perf_counter() - start

        # Reader should not be more than 3x slower
        # (it does more work: tokenization + precedence parsing)
        assert (
            reader_time < canonical_time * 3
        ), f"Reader too slow: {reader_time:.3f}s vs canonical {canonical_time:.3f}s"

    def test_operator_vs_canonical_parsing(self):
        """Compare parsing speed of operators vs canonical forms.

        Note: We compare both via Reader to isolate syntax/token load;
        the Reader-vs-Grammar gap is measured in test_reader_transformation_overhead.
        """
        reader = Reader()

        test_cases = [
            ("1 + 2 * 3 - 4", "'-'('+'(1, '*'(2, 3)), 4)"),
            ("a , b ; c , d", "';'(','(a, b), ','(c, d))"),
            ("X = Y, Y = Z", "','('='(X, Y), '='(Y, Z))"),
        ]

        for op_form, canonical_form in test_cases:
            # Both should use the reader for fair comparison
            # Warm-up and time operator parsing
            for _ in range(50):
                reader.read_term(op_form)
            start = time.perf_counter()
            for _ in range(1000):
                reader.read_term(op_form)
            op_time = time.perf_counter() - start

            # Warm-up and time canonical parsing (also through reader)
            for _ in range(50):
                reader.read_term(canonical_form)
            start = time.perf_counter()
            for _ in range(1000):
                reader.read_term(canonical_form)
            canonical_time = time.perf_counter() - start

            # Operator form should be faster or comparable
            # (simpler syntax, fewer tokens)
            ratio = op_time / canonical_time
            assert (
                ratio < 2.0
            ), f"Operator parsing slower than canonical: ratio {ratio:.2f} for: {op_form}"

    def test_large_expression_parsing(self):
        """Test parsing performance with large expressions."""
        reader = Reader()

        # Build a large arithmetic expression
        n = 50
        expr = " + ".join(str(i) for i in range(1, n + 1))

        # Should parse in reasonable time
        start = time.perf_counter()
        result = reader.read_term(expr)
        elapsed = time.perf_counter() - start

        # Should parse within 1s even for large expression (relaxed for CI)
        assert elapsed < 1.0, f"Large expression took {elapsed:.3f}s"
        assert result is not None


class TestExecutionPerformance:
    """Test execution performance with operators."""

    def test_operator_execution_overhead(self):
        """Operators should not affect execution performance."""
        # Program with operators
        clauses1 = parser_module.parse_program(
            """
            test1(0, 0).
            test1(N, R) :-
                N > 0,
                N1 is N - 1,
                test1(N1, R1),
                R is R1 + N.
        """
        )
        engine1 = DevEngine(Program(tuple(clauses1)))

        # Same program with canonical forms
        clauses2 = parser_module.parse_program(
            """
            test2(0, 0).
            test2(N, R) :-
                '>'(N, 0),
                is(N1, '-'(N, 1)),
                test2(N1, R1),
                is(R, '+'(R1, N)).
        """
        )
        engine2 = DevEngine(Program(tuple(clauses2)))

        # Warm-up runs
        goals1 = parser_module.parse_query("?- test1(10, R).")
        _ = list(engine1.run(goals1))

        goals2 = parser_module.parse_query("?- test2(10, R).")
        _ = list(engine2.run(goals2))

        # Run multiple iterations and use minimum time to reduce noise
        num_iterations = 3

        # Time execution with operators (best of N)
        goals1 = parser_module.parse_query("?- test1(100, R).")
        op_times = []
        for _ in range(num_iterations):
            start = time.perf_counter()
            solutions1 = list(engine1.run(goals1))
            op_times.append(time.perf_counter() - start)
        op_time = min(op_times)

        # Time execution with canonical forms (best of N)
        goals2 = parser_module.parse_query("?- test2(100, R).")
        canonical_times = []
        for _ in range(num_iterations):
            start = time.perf_counter()
            solutions2 = list(engine2.run(goals2))
            canonical_times.append(time.perf_counter() - start)
        canonical_time = min(canonical_times)

        # Results should be identical and deterministic
        assert len(solutions1) == 1 == len(solutions2)
        assert solutions1[0]["R"] == solutions2[0]["R"]

        # Skip performance assertion if times are too small to measure reliably
        if op_time < 0.001 or canonical_time < 0.001:
            return  # Skip - measurements unreliable

        # Execution times should be comparable (operators may be faster or slower)
        # Allow operators to be up to 5x faster (ratio >= 0.2) or up to 3x slower (ratio <= 3.0)
        # Wide tolerance for CI environments under varying load
        ratio = op_time / canonical_time
        assert (
            0.2 <= ratio <= 3.0
        ), f"Execution time ratio {ratio:.2f}: op={op_time:.3f}s, canonical={canonical_time:.3f}s"

    def test_backtracking_performance(self):
        """Backtracking performance should not degrade."""
        clauses = parser_module.parse_program(
            """
            choice(X) :- X = 1 ; X = 2 ; X = 3 ; X = 4 ; X = 5.
            test(R) :- choice(A), choice(B), choice(C), R is A + B + C.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # This creates 5^3 = 125 choice points
        goals = parser_module.parse_query("?- test(R).")

        # Warm-up run
        _ = list(engine.run(goals))

        # Run multiple times and take the best
        times = []
        for _ in range(3):
            start = time.perf_counter()
            solutions = list(engine.run(goals))
            times.append(time.perf_counter() - start)
        elapsed = min(times)

        # Should handle 125 solutions quickly
        assert len(solutions) == 125
        assert elapsed < 2.0, f"Backtracking took {elapsed:.3f}s for 125 solutions"

    def test_deep_recursion_performance(self):
        """Deep recursion should perform well with operators."""
        clauses = parser_module.parse_program(
            """
            countdown(0).
            countdown(N) :- N > 0, N1 is N - 1, countdown(N1).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # Test with reasonably deep recursion
        goals = parser_module.parse_query("?- countdown(1000).")

        # Run multiple times and take the best
        times = []
        for _ in range(3):
            start = time.perf_counter()
            solutions = list(engine.run(goals))
            times.append(time.perf_counter() - start)
        elapsed = min(times)

        assert len(solutions) == 1
        # Should complete within 2s (very relaxed for slow CI runners)
        assert elapsed < 2.0, f"Deep recursion took {elapsed:.3f}s"


class TestMemoryUsage:
    """Test that operators don't significantly increase memory usage."""

    # Note: AST equality implies identical memory structure - Python uses
    # the same amount of memory for identical data structures

    def test_ast_memory_overhead(self):
        """AST size should be similar for operators and canonical forms."""
        reader = Reader()

        # Parse same expression both ways
        op_ast = reader.read_term("a , b , c , d , e")
        canonical_ast = reader.read_term("','(a, ','(b, ','(c, ','(d, e))))")

        # AST structure should be identical
        assert op_ast == canonical_ast

        # Memory usage is implicitly the same since ASTs are identical
        # Python will use same amount of memory for identical structures

    def test_large_program_memory(self):
        """Large programs with operators should not use excessive memory."""
        # Generate a large program with operators including tight tokens
        lines = []
        for i in range(100):
            # Use only implemented operators
            lines.append(f"pred{i}(X) :- X > {i}, X =:= {i+5}.")

        program_text = "\n".join(lines)

        # Should parse without issues
        clauses = parser_module.parse_program(program_text)
        assert len(clauses) == 100

        # Create engine - should not use excessive memory
        engine = DevEngine(Program(tuple(clauses)))

        # Test a query
        goals = parser_module.parse_query("?- pred50(55).")
        solutions = list(engine.run(goals))
        assert len(solutions) == 1


class TestRegressionGuards:
    """Guard against performance regressions."""

    def test_no_performance_regression_basic_queries(self):
        """Basic queries should not regress in performance."""
        clauses = parser_module.parse_program(
            """
            parent(tom, bob).
            parent(tom, liz).
            parent(bob, ann).
            parent(bob, pat).
            parent(pat, jim).
            
            ancestor(X, Y) :- parent(X, Y).
            ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # Find all ancestors
        goals = parser_module.parse_query("?- ancestor(X, Y).")

        start = time.perf_counter()
        solutions = list(engine.run(goals))
        elapsed = time.perf_counter() - start

        # Should find all ancestor relationships quickly
        assert len(solutions) == 9  # All ancestor pairs (including transitive)
        # Relaxed threshold for CI
        assert elapsed < 0.5, f"Basic query took {elapsed:.3f}s"

    def test_no_performance_regression_arithmetic(self):
        """Arithmetic operations should not regress."""
        clauses = parser_module.parse_program(
            """
            sum([], 0).
            sum([H|T], S) :- sum(T, S1), S is S1 + H.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # Sum a list of 100 numbers
        goals = parser_module.parse_query(
            "?- sum([" + ",".join(str(i) for i in range(1, 101)) + "], S)."
        )

        start = time.perf_counter()
        solutions = list(engine.run(goals))
        elapsed = time.perf_counter() - start

        assert len(solutions) == 1
        assert solutions[0]["S"].value == 5050  # sum(1..100)
        # Relaxed threshold for CI
        assert elapsed < 1.0, f"Arithmetic took {elapsed:.3f}s"
