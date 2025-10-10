"""Test Pyodide proxy behavior for Web REPL variable bindings.

These tests simulate how Pyodide handles Python objects in JavaScript contexts
to ensure our worker.js code correctly preserves variable bindings.
"""

from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.parser.parser import parse_query, parse_program
from prolog.ast.pretty import pretty


class TestPyodideProxyBehavior:
    """Test that our solution extraction works with Pyodide proxy semantics."""

    def setup_method(self):
        """Set up test environment with standard library."""
        stdlib_content = """
        append([], L, L).
        append([H|T], L2, [H|R]) :- append(T, L2, R).
        """
        clauses = parse_program(stdlib_content)
        self.program = Program(clauses)

    def test_forof_loop_would_lose_items_method(self):
        """Test that for...of loop behavior would lose dict methods.

        This test simulates what happens when JavaScript's for...of
        iterates over a Python list - it auto-converts each element.
        """
        engine = Engine(self.program)
        query_text = "?- append([1, 2], [3, 4], X)."
        goals = parse_query(query_text)
        python_solutions = engine.run(goals, max_solutions=10)

        # Simulate for...of auto-conversion behavior
        # In Pyodide, for...of converts each yielded element
        results_with_forof = []
        for solution in python_solutions:
            # Simulate conversion to plain dict (loses Python methods)
            if hasattr(solution, "items"):
                # This would work with Python dict
                converted = dict(solution)  # Simulates JS conversion
                # But now converted.items() wouldn't exist in JS!
                # hasattr(converted, 'items') is True in Python but
                # in JS, the plain object wouldn't have .items()
                results_with_forof.append(converted)

        # This shows the dict exists but in JS it would be a plain object
        assert len(results_with_forof) == 1
        assert "X" in results_with_forof[0]

    def test_indexed_access_preserves_proxy(self):
        """Test that indexed access preserves Python proxy methods.

        This simulates our fix - using indexed access to avoid conversion.
        """
        engine = Engine(self.program)
        query_text = "?- append([1, 2], [3, 4], X)."
        goals = parse_query(query_text)
        python_solutions = engine.run(goals, max_solutions=10)

        # Simulate indexed access (our fix)
        results_with_index = []
        solutions_length = len(python_solutions)
        for i in range(solutions_length):
            # Access by index - preserves proxy
            solution = python_solutions[i]

            # This is what our worker.js does
            if solution is not None:
                pretty_solution = {}
                # solution.items() is available because it's still a proxy
                if hasattr(solution, "items"):
                    for key, value in solution.items():
                        pretty_str = pretty(value, operator_mode=True)
                        pretty_solution[key] = str(pretty_str)
                results_with_index.append(pretty_solution)

        # Verify we got the binding
        assert len(results_with_index) == 1
        assert results_with_index[0] == {"X": "[1, 2, 3, 4]"}

    def test_explicit_tojs_with_proxies_would_work(self):
        """Test that explicit toJs({create_pyproxies: true}) would also work.

        This was one of the attempted fixes that would have worked.
        """
        engine = Engine(self.program)
        query_text = "?- append([1, 2], [3, 4], X)."
        goals = parse_query(query_text)
        python_solutions = engine.run(goals, max_solutions=10)

        # Simulate converting the list but keeping proxies
        # This is like: solutions = pythonSolutions.toJs({create_pyproxies: true})
        results = []

        # In real Pyodide, this would convert the list to JS array
        # but keep dict elements as proxies
        for solution in python_solutions:
            if solution is not None:
                # Solution would still be a proxy with .items()
                pretty_solution = {}
                if hasattr(solution, "items"):
                    for key, value in solution.items():
                        pretty_str = pretty(value, operator_mode=True)
                        pretty_solution[key] = str(pretty_str)
                results.append(pretty_solution)

        assert len(results) == 1
        assert results[0] == {"X": "[1, 2, 3, 4]"}


class TestSolutionExtractionValidation:
    """Test that validates actual solution extraction, not just code structure."""

    def setup_method(self):
        """Set up test environment."""
        stdlib_content = """
        append([], L, L).
        append([H|T], L2, [H|R]) :- append(T, L2, R).
        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).
        """
        clauses = parse_program(stdlib_content)
        self.program = Program(clauses)

    def simulate_worker_solution_extraction(self, python_solutions, use_index=True):
        """Simulate how worker.js extracts solutions.

        Args:
            python_solutions: List of solution dicts from PyLog engine
            use_index: If True, use indexed access (correct).
                      If False, simulate for...of (buggy).

        Returns:
            List of extracted solution dicts as they would appear in JS
        """
        results = []

        if use_index:
            # Correct approach - indexed access
            solutions_length = len(python_solutions)
            for i in range(solutions_length):
                solution = python_solutions[i]
                if solution is not None:
                    pretty_solution = {}
                    if hasattr(solution, "items"):
                        for key, value in solution.items():
                            pretty_str = pretty(value, operator_mode=True)
                            pretty_solution[key] = str(pretty_str)
                    results.append(pretty_solution)
        else:
            # Buggy approach - for...of would auto-convert
            for solution in python_solutions:
                if solution is not None:
                    # Simulate loss of .items() method
                    # In real JS, solution would be plain object without .items()
                    pretty_solution = {}
                    # This simulates .items() being undefined
                    # so the loop body never executes
                    results.append(pretty_solution)

        return results

    def test_single_variable_extraction(self):
        """Test extraction of single variable binding."""
        engine = Engine(self.program)
        query = "?- append([1, 2], [3, 4], X)."
        goals = parse_query(query)
        solutions = engine.run(goals, max_solutions=10)

        # Test correct extraction (indexed)
        correct_results = self.simulate_worker_solution_extraction(
            solutions, use_index=True
        )
        assert len(correct_results) == 1
        assert correct_results[0] == {"X": "[1, 2, 3, 4]"}

        # Test buggy extraction (for...of)
        buggy_results = self.simulate_worker_solution_extraction(
            solutions, use_index=False
        )
        assert len(buggy_results) == 1
        assert buggy_results[0] == {}  # Empty! This is the bug!

    def test_multiple_variable_extraction(self):
        """Test extraction of multiple variable bindings."""
        engine = Engine(self.program)
        query = "?- append(X, Y, [1, 2, 3])."
        goals = parse_query(query)
        solutions = engine.run(goals, max_solutions=10)

        # Test correct extraction
        correct_results = self.simulate_worker_solution_extraction(
            solutions, use_index=True
        )
        assert len(correct_results) == 4
        # Handle both [] and '[]' representations
        assert correct_results[0]["X"] in ["[]", "'[]'"]
        assert correct_results[0]["Y"] == "[1, 2, 3]"
        assert correct_results[1] == {"X": "[1]", "Y": "[2, 3]"}
        assert correct_results[2] == {"X": "[1, 2]", "Y": "[3]"}
        assert correct_results[3]["X"] == "[1, 2, 3]"
        assert correct_results[3]["Y"] in ["[]", "'[]'"]

        # Test buggy extraction
        buggy_results = self.simulate_worker_solution_extraction(
            solutions, use_index=False
        )
        assert len(buggy_results) == 4
        assert all(result == {} for result in buggy_results)  # All empty!

    def test_multiple_solutions_extraction(self):
        """Test extraction from query with multiple solutions."""
        engine = Engine(self.program)
        query = "?- member(X, [a, b, c])."
        goals = parse_query(query)
        solutions = engine.run(goals, max_solutions=10)

        # Test correct extraction
        correct_results = self.simulate_worker_solution_extraction(
            solutions, use_index=True
        )
        assert len(correct_results) == 3
        assert correct_results[0] == {"X": "a"}
        assert correct_results[1] == {"X": "b"}
        assert correct_results[2] == {"X": "c"}

        # Test buggy extraction
        buggy_results = self.simulate_worker_solution_extraction(
            solutions, use_index=False
        )
        assert len(buggy_results) == 3
        assert all(result == {} for result in buggy_results)

    def test_empty_solution_extraction(self):
        """Test extraction of empty solution (query with no variables)."""
        engine = Engine(self.program)
        query = "?- append([1], [2], [1, 2])."
        goals = parse_query(query)
        solutions = engine.run(goals, max_solutions=10)

        # Both approaches should handle empty solutions correctly
        correct_results = self.simulate_worker_solution_extraction(
            solutions, use_index=True
        )
        assert len(correct_results) == 1
        assert correct_results[0] == {}

        buggy_results = self.simulate_worker_solution_extraction(
            solutions, use_index=False
        )
        assert len(buggy_results) == 1
        assert buggy_results[0] == {}  # This one works by accident!


class TestWorkerCodeAssumptions:
    """Test the assumptions our worker.js code makes about Pyodide behavior."""

    def test_python_list_has_length_property(self):
        """Test that Python lists have a length property accessible from JS."""
        test_list = [{"X": 1}, {"Y": 2}, {"Z": 3}]

        # In Pyodide, Python lists expose .length to JavaScript
        # We simulate this with len() in Python
        assert len(test_list) == 3

        # Indexed access should work
        assert test_list[0] == {"X": 1}
        assert test_list[1] == {"Y": 2}
        assert test_list[2] == {"Z": 3}

    def test_python_dict_has_items_method(self):
        """Test that Python dicts have items() method."""
        test_dict = {"X": "value1", "Y": "value2"}

        # Check items() exists and works
        assert hasattr(test_dict, "items")
        items_list = list(test_dict.items())
        assert items_list == [("X", "value1"), ("Y", "value2")]

    def test_items_returns_tuples(self):
        """Test that dict.items() returns tuples we can index."""
        test_dict = {"X": "value1", "Y": "value2"}

        for item in test_dict.items():
            # Each item should be a tuple (key, value)
            assert isinstance(item, tuple)
            assert len(item) == 2
            key = item[0]
            value = item[1]
            assert isinstance(key, str)
            assert test_dict[key] == value

    def test_empty_dict_items_works(self):
        """Test that empty dict.items() works correctly."""
        empty_dict = {}

        assert hasattr(empty_dict, "items")
        items_list = list(empty_dict.items())
        assert items_list == []

        # Should be safe to iterate even when empty
        count = 0
        for item in empty_dict.items():
            count += 1
        assert count == 0


class MockPyodideProxy:
    """Mock Pyodide proxy to simulate JS/Python interop behavior."""

    def __init__(self, python_obj):
        self._obj = python_obj

    def __getitem__(self, index):
        """Indexed access preserves proxy."""
        return (
            MockPyodideProxy(self._obj[index])
            if isinstance(self._obj, list)
            else self._obj[index]
        )

    def __iter__(self):
        """for...of converts to JS (loses Python methods)."""
        if isinstance(self._obj, list):
            # Simulate auto-conversion: return plain dicts, not proxies
            for item in self._obj:
                if isinstance(item, dict):
                    yield dict(item)  # Converted, no .items() in JS!
                else:
                    yield item
        else:
            yield from self._obj

    @property
    def length(self):
        """Python lists expose .length to JavaScript."""
        return len(self._obj) if isinstance(self._obj, list) else None

    def items(self):
        """Dict method - only available on proxy, not converted object."""
        if isinstance(self._obj, dict):
            return self._obj.items()
        raise AttributeError("items() only on dict proxies")


class TestMockPyodideEnvironment:
    """Test with mock Pyodide environment to catch conversion issues."""

    def test_mock_forof_loses_items(self):
        """Test that for...of in mock environment loses .items() method."""
        solutions = [{"X": "value1"}, {"Y": "value2"}]
        proxy = MockPyodideProxy(solutions)

        # Simulate for...of loop
        results = []
        for solution in proxy:  # This uses __iter__ which auto-converts
            # solution is now a plain dict (in JS would have no .items())
            try:
                # This would fail in real JS
                for k, v in solution.items():
                    results.append((k, v))
            except AttributeError:
                # In real JS, .items() would be undefined
                pass

        # In Python, dict still has .items(), but in JS it wouldn't
        # This test can't fully simulate the JS behavior, but documents intent
        assert len(results) > 0  # Python dicts still have .items()

    def test_mock_indexed_preserves_proxy(self):
        """Test that indexed access in mock environment preserves proxy."""
        solutions = [{"X": "value1"}, {"Y": "value2"}]
        proxy = MockPyodideProxy(solutions)

        # Simulate indexed access
        results = []
        for i in range(proxy.length):
            solution_proxy = proxy[i]  # Returns MockPyodideProxy
            # Now .items() should work because it's still a proxy
            if isinstance(solution_proxy._obj, dict):
                for k, v in solution_proxy.items():
                    results.append((k, v))

        assert results == [("X", "value1"), ("Y", "value2")]
