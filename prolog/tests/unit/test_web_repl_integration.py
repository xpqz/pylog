"""Integration tests for Web REPL functionality.

These tests verify that the Web REPL correctly processes queries and returns
variable bindings, not just 'true' for successful queries.
"""

from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.parser.parser import parse_query, parse_program
from prolog.ast.pretty import pretty


class TestWebREPLVariableBindings:
    """Test that Web REPL correctly returns variable bindings."""

    def setup_method(self):
        """Set up test environment with standard library."""
        # Load standard library
        stdlib_content = """
        % Lists library
        append([], L, L).
        append([H|T], L2, [H|R]) :- append(T, L2, R).

        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).

        length([], 0).
        length([_|T], N) :- length(T, N1), N is N1 + 1.
        """

        # Parse standard library clauses
        clauses = parse_program(stdlib_content)
        self.program = Program(clauses)

    def test_append_returns_variable_binding(self):
        """Test that append/3 query returns X binding, not just empty dict."""
        engine = Engine(self.program)

        # Query: append([1, 2], [3, 4], X)
        query_text = "?- append([1, 2], [3, 4], X)."
        goals = parse_query(query_text)

        # Run query
        solutions = engine.run(goals, max_solutions=10)

        # Verify we get exactly one solution
        assert len(solutions) == 1

        # The solution MUST contain the variable binding
        solution = solutions[0]
        assert solution is not None
        assert isinstance(solution, dict)
        assert (
            len(solution) > 0
        ), "Solution should not be empty dict - should contain X binding"
        assert "X" in solution, "Solution must contain binding for variable X"

        # Verify the binding is correct
        x_value = solution["X"]
        pretty_x = pretty(x_value, operator_mode=True)
        assert (
            pretty_x == "[1, 2, 3, 4]"
        ), f"Expected X = [1, 2, 3, 4], got X = {pretty_x}"

    def test_member_returns_all_bindings(self):
        """Test that member/2 returns all variable bindings."""
        engine = Engine(self.program)

        # Query: member(X, [a, b, c])
        query_text = "?- member(X, [a, b, c])."
        goals = parse_query(query_text)

        # Run query
        solutions = engine.run(goals, max_solutions=10)

        # Should get 3 solutions
        assert len(solutions) == 3

        # Each solution should contain X binding
        expected_values = ["a", "b", "c"]
        for i, solution in enumerate(solutions):
            assert solution is not None
            assert isinstance(solution, dict)
            assert len(solution) > 0, f"Solution {i+1} should not be empty"
            assert "X" in solution, f"Solution {i+1} must contain X binding"

            x_value = solution["X"]
            pretty_x = pretty(x_value, operator_mode=True)
            assert pretty_x == expected_values[i]

    def test_multiple_variables_all_bound(self):
        """Test query with multiple variables returns all bindings."""
        engine = Engine(self.program)

        # Query: append(X, Y, [1, 2, 3])
        query_text = "?- append(X, Y, [1, 2, 3])."
        goals = parse_query(query_text)

        # Run query
        solutions = engine.run(goals, max_solutions=10)

        # Should get 4 solutions: ([], [1,2,3]), ([1], [2,3]), ([1,2], [3]), ([1,2,3], [])
        assert len(solutions) == 4

        expected_pairs = [
            ("[]", "[1, 2, 3]"),
            ("[1]", "[2, 3]"),
            ("[1, 2]", "[3]"),
            ("[1, 2, 3]", "[]"),
        ]

        for i, solution in enumerate(solutions):
            assert solution is not None
            assert isinstance(solution, dict)
            assert (
                len(solution) == 2
            ), f"Solution {i+1} should have 2 bindings (X and Y)"
            assert "X" in solution, f"Solution {i+1} must contain X"
            assert "Y" in solution, f"Solution {i+1} must contain Y"

            x_pretty = pretty(solution["X"], operator_mode=True)
            y_pretty = pretty(solution["Y"], operator_mode=True)

            # Handle both [] and '[]' representations
            x_normalized = x_pretty.strip("'") if x_pretty == "'[]'" else x_pretty
            y_normalized = y_pretty.strip("'") if y_pretty == "'[]'" else y_pretty
            expected_x, expected_y = expected_pairs[i]
            expected_x_norm = (
                expected_x.strip("'") if expected_x == "'[]'" else expected_x
            )
            expected_y_norm = (
                expected_y.strip("'") if expected_y == "'[]'" else expected_y
            )

            assert (x_normalized, y_normalized) == (expected_x_norm, expected_y_norm)

    def test_query_without_variables_returns_empty_dict(self):
        """Test that queries with no variables return empty dict, not None."""
        engine = Engine(self.program)

        # Query: append([1], [2], [1, 2])  (no variables, just checking)
        query_text = "?- append([1], [2], [1, 2])."
        goals = parse_query(query_text)

        # Run query
        solutions = engine.run(goals, max_solutions=10)

        # Should succeed with one solution
        assert len(solutions) == 1

        # Solution should be empty dict (not None, not missing)
        solution = solutions[0]
        assert solution is not None
        assert isinstance(solution, dict)
        assert (
            len(solution) == 0
        ), "Solution for query without variables should be empty dict"

    def test_failing_query_returns_no_solutions(self):
        """Test that failing queries return empty list."""
        engine = Engine(self.program)

        # Query: append([1], [2], [3, 4])  (will fail)
        query_text = "?- append([1], [2], [3, 4])."
        goals = parse_query(query_text)

        # Run query
        solutions = engine.run(goals, max_solutions=10)

        # Should get no solutions
        assert len(solutions) == 0

    def test_complex_query_with_arithmetic(self):
        """Test query combining list operations with arithmetic."""
        engine = Engine(self.program)

        # Query: length([1, 2, 3], N)
        query_text = "?- length([1, 2, 3], N)."
        goals = parse_query(query_text)

        # Run query
        solutions = engine.run(goals, max_solutions=10)

        # Should get exactly one solution
        assert len(solutions) == 1

        solution = solutions[0]
        assert solution is not None
        assert isinstance(solution, dict)
        assert "N" in solution

        n_value = solution["N"]
        pretty_n = pretty(n_value, operator_mode=True)
        assert pretty_n == "3"


class TestWebWorkerSimulation:
    """Test that simulates what the Web worker does with solutions."""

    def setup_method(self):
        """Set up test environment."""
        stdlib_content = """
        append([], L, L).
        append([H|T], L2, [H|R]) :- append(T, L2, R).
        """
        clauses = parse_program(stdlib_content)
        self.program = Program(clauses)

    def test_python_dict_iteration_preserves_bindings(self):
        """Test that Python dict iteration works as expected in worker."""
        engine = Engine(self.program)

        query_text = "?- append([1, 2], [3, 4], X)."
        goals = parse_query(query_text)

        # This is what the worker gets
        python_solutions = engine.run(goals, max_solutions=10)

        # Simulate worker processing
        results = []

        # This simulates the JavaScript for...of loop
        for solution in python_solutions:
            if solution is not None and solution is not None:  # Simulates JS check
                pretty_solution = {}

                # This is the critical part - solution.items() must work
                if hasattr(solution, "items"):
                    for key, value in solution.items():
                        pretty_str = pretty(value, operator_mode=True)
                        pretty_solution[key] = str(pretty_str)

                results.append(pretty_solution)

        # Verify results are correct
        assert len(results) == 1
        assert results[0] == {"X": "[1, 2, 3, 4]"}

    def test_empty_solution_handling(self):
        """Test that empty solutions (true queries) are handled correctly."""
        engine = Engine(self.program)

        # Query with no variables
        query_text = "?- append([], [1], [1])."
        goals = parse_query(query_text)

        python_solutions = engine.run(goals, max_solutions=10)

        results = []
        for solution in python_solutions:
            if solution is not None:
                pretty_solution = {}
                if hasattr(solution, "items"):
                    for key, value in solution.items():
                        pretty_str = pretty(value, operator_mode=True)
                        pretty_solution[key] = str(pretty_str)
                results.append(pretty_solution)

        assert len(results) == 1
        assert results[0] == {}  # Empty dict for 'true' result
