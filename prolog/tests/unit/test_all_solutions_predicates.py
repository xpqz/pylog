"""Tests for all-solutions predicates: findall/3, bagof/3, setof/3.

These tests verify the implementation of the ISO Prolog all-solutions predicates
using proper query execution through the engine, not direct method calls.
"""

from prolog.ast.terms import Atom, List as PrologList
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.parser.parser import parse_program


class TestFindallPredicate:
    """Test findall/3 predicate."""

    def test_findall_basic_success(self):
        """Test findall/3 with basic facts."""
        program_text = """
        value(1).
        value(2).
        value(3).
        """
        clauses = parse_program(program_text)
        program = Program(clauses)
        engine = Engine(program)

        # Query: findall(X, value(X), L)
        query_text = "findall(X, value(X), L)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # findall always succeeds once

        # Check the result list contains the expected values
        result_list = solutions[0]["L"]
        assert isinstance(result_list, PrologList)
        assert len(result_list.items) == 3
        # Check that it contains the right integers (order may vary)
        values = [item.value for item in result_list.items if hasattr(item, "value")]
        assert sorted(values) == [1, 2, 3]

    def test_findall_empty_result(self):
        """Test findall/3 with no solutions returns empty list."""
        program = Program([])  # Empty program
        engine = Engine(program)

        # Query: findall(X, nonexistent(X), L)
        query_text = "findall(X, nonexistent(X), L)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # findall always succeeds

        # Check that result is empty list
        result_list = solutions[0]["L"]
        assert result_list == Atom("[]")

    def test_findall_wrong_arity(self):
        """Test findall with wrong number of arguments fails."""
        program = Program([])
        engine = Engine(program)

        # Test with 2 arguments - should fail to parse or execute
        query_text = "findall(X, goal)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # Should fail


class TestBagofPredicate:
    """Test bagof/3 predicate."""

    def test_bagof_basic_success(self):
        """Test bagof/3 with basic facts."""
        program_text = """
        color(red).
        color(blue).
        color(green).
        """
        clauses = parse_program(program_text)
        program = Program(clauses)
        engine = Engine(program)

        # Query: bagof(X, color(X), L)
        query_text = "bagof(X, color(X), L)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # bagof succeeds when there are solutions

        # Check that result is a list with the colors
        result_list = solutions[0]["L"]
        assert isinstance(result_list, PrologList)
        assert len(result_list.items) == 3

    def test_bagof_fails_on_empty(self):
        """Test bagof/3 fails when no solutions exist."""
        program = Program([])  # Empty program
        engine = Engine(program)

        # Query: bagof(X, nonexistent(X), L)
        query_text = "bagof(X, nonexistent(X), L)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # bagof fails on no solutions

    def test_bagof_wrong_arity(self):
        """Test bagof with wrong number of arguments fails."""
        program = Program([])
        engine = Engine(program)

        # Test with 2 arguments
        query_text = "bagof(X, goal)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # Should fail


class TestSetofPredicate:
    """Test setof/3 predicate."""

    def test_setof_basic_success(self):
        """Test setof/3 with basic facts."""
        program_text = """
        item(apple).
        item(banana).
        item(apple).
        """
        clauses = parse_program(program_text)
        program = Program(clauses)
        engine = Engine(program)

        # Query: setof(X, item(X), L)
        query_text = "setof(X, item(X), L)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # setof succeeds when there are solutions

        # Check that result is a list with unique, sorted items
        result_list = solutions[0]["L"]
        assert isinstance(result_list, PrologList)
        # Should be deduplicated (apple appears only once) and sorted
        assert len(result_list.items) == 2  # apple, banana

    def test_setof_fails_on_empty(self):
        """Test setof/3 fails when no solutions exist."""
        program = Program([])
        engine = Engine(program)

        # Query: setof(X, nonexistent(X), L)
        query_text = "setof(X, nonexistent(X), L)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # setof fails on no solutions

    def test_setof_wrong_arity(self):
        """Test setof with wrong number of arguments fails."""
        program = Program([])
        engine = Engine(program)

        query_text = "setof(X, goal)."
        query_clauses = parse_program(query_text)
        query = query_clauses[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # Should fail


class TestAllSolutionsIntegration:
    """Integration tests for all-solutions predicates."""

    def test_findall_vs_bagof_empty_behavior(self):
        """Test difference between findall and bagof on empty results."""
        program = Program([])
        engine = Engine(program)

        # findall should succeed with empty list
        findall_query = parse_program("findall(X, nonexistent(X), L).")[0].head
        findall_solutions = list(engine.solve(findall_query))
        assert len(findall_solutions) == 1
        assert findall_solutions[0]["L"] == Atom("[]")

        # bagof should fail
        bagof_query = parse_program("bagof(X, nonexistent(X), L).")[0].head
        bagof_solutions = list(engine.solve(bagof_query))
        assert len(bagof_solutions) == 0

    def test_builtin_registration(self):
        """Test that all-solutions predicates are properly registered."""
        engine = Engine(Program([]))

        # Verify that predicates are registered in the builtin system
        assert ("findall", 3) in engine._builtins
        assert ("bagof", 3) in engine._builtins
        assert ("setof", 3) in engine._builtins

    def test_comprehensive_comparison(self):
        """Test all three predicates with the same data to show differences."""
        program_text = """
        item(apple).
        item(banana).
        item(apple).
        item(cherry).
        """
        clauses = parse_program(program_text)
        program = Program(clauses)
        engine = Engine(program)

        # Test findall - should include duplicates
        findall_query = parse_program("findall(X, item(X), L).")[0].head
        findall_solutions = list(engine.solve(findall_query))
        assert len(findall_solutions) == 1
        findall_list = findall_solutions[0]["L"]
        assert len(findall_list.items) == 4  # Includes duplicates

        # Test bagof - should include duplicates (same as findall for this case)
        bagof_query = parse_program("bagof(X, item(X), L).")[0].head
        bagof_solutions = list(engine.solve(bagof_query))
        assert len(bagof_solutions) == 1
        bagof_list = bagof_solutions[0]["L"]
        assert len(bagof_list.items) == 4  # Includes duplicates

        # Test setof - should deduplicate and sort
        setof_query = parse_program("setof(X, item(X), L).")[0].head
        setof_solutions = list(engine.solve(setof_query))
        assert len(setof_solutions) == 1
        setof_list = setof_solutions[0]["L"]
        assert len(setof_list.items) == 3  # Deduplicated: apple, banana, cherry
