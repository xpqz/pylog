"""Tests for all-solutions builtins extraction.

This test verifies that the extracted all-solutions predicates (findall/3, bagof/3, setof/3)
in builtins/solutions.py work correctly through the builtin registry system.

This follows the TDD approach for Phase 5 of the engine refactoring plan.
"""

import pytest
from prolog.ast.terms import Atom
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.parser.parser import parse_program
from prolog.engine.builtins import solutions
from prolog.engine.builtins import register_all


@pytest.fixture
def sample_program():
    """Create a sample program for testing."""
    program_text = """
    color(red).
    color(blue).
    color(green).

    number(1).
    number(2).
    number(3).
    number(2).

    person(alice).
    person(bob).
    """
    clauses = parse_program(program_text)
    return Program(clauses)


class TestSolutionsExtraction:
    """Test that extracted solutions module works correctly through builtin registry."""

    def test_findall_proper_integration(self, sample_program):
        """Test that findall/3 works through proper builtin registry execution."""
        engine = Engine(sample_program)

        # Test findall functionality through normal query execution
        query_text = "findall(X, color(X), Colors)."
        query = parse_program(query_text)[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # findall always succeeds

        # Verify the result contains the expected colors
        colors_list = solutions[0]["Colors"]
        assert len(colors_list.items) == 3
        color_names = [item.name for item in colors_list.items]
        assert set(color_names) == {"red", "blue", "green"}

    def test_bagof_proper_integration(self, sample_program):
        """Test that bagof/3 works through proper builtin registry execution."""
        engine = Engine(sample_program)

        # Test bagof functionality with color facts (which work correctly)
        query_text = "bagof(X, color(X), Colors)."
        query = parse_program(query_text)[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # bagof succeeds when solutions exist

        # Verify the result includes all colors
        colors_list = solutions[0]["Colors"]
        assert len(colors_list.items) == 3
        color_names = [item.name for item in colors_list.items]
        assert set(color_names) == {"red", "blue", "green"}

    def test_setof_proper_integration(self, sample_program):
        """Test setof/3 works through proper builtin registry execution with deduplication."""
        # Create program with duplicates to test setof deduplication
        program_text = """
        item(apple).
        item(banana).
        item(apple).
        item(cherry).
        """
        clauses = parse_program(program_text)
        program = Program(clauses)
        engine = Engine(program)

        # Test setof functionality (deduplication)
        query_text = "setof(X, item(X), UniqueItems)."
        query = parse_program(query_text)[0].head

        solutions = list(engine.solve(query))
        assert len(solutions) == 1  # setof succeeds when solutions exist

        # Verify the result is deduplicated and sorted
        items_list = solutions[0]["UniqueItems"]
        assert len(items_list.items) == 3  # duplicates removed: apple, banana, cherry
        item_names = [item.name for item in items_list.items]
        # Should be sorted alphabetically
        assert item_names == ["apple", "banana", "cherry"]

    def test_empty_case_behavior(self):
        """Test different behavior of findall vs bagof/setof on empty results."""
        engine = Engine(Program([]))

        # findall should succeed with empty list
        findall_query = parse_program("findall(X, nonexistent(X), L).")[0].head
        findall_solutions = list(engine.solve(findall_query))
        assert len(findall_solutions) == 1
        assert findall_solutions[0]["L"] == Atom("[]")

        # bagof should fail on empty
        bagof_query = parse_program("bagof(X, nonexistent(X), L).")[0].head
        bagof_solutions = list(engine.solve(bagof_query))
        assert len(bagof_solutions) == 0

        # setof should fail on empty
        setof_query = parse_program("setof(X, nonexistent(X), L).")[0].head
        setof_solutions = list(engine.solve(setof_query))
        assert len(setof_solutions) == 0

    def test_registration_compatibility(self):
        """Test that extracted predicates are properly registered."""
        engine = Engine(Program([]))

        # Verify registrations exist in builtin system
        assert ("findall", 3) in engine._builtins
        assert ("bagof", 3) in engine._builtins
        assert ("setof", 3) in engine._builtins

        # Verify they are callable functions
        findall_fn = engine._builtins[("findall", 3)]
        bagof_fn = engine._builtins[("bagof", 3)]
        setof_fn = engine._builtins[("setof", 3)]

        assert callable(findall_fn)
        assert callable(bagof_fn)
        assert callable(setof_fn)

    def test_error_handling_compatibility(self):
        """Test that extracted predicates handle errors correctly."""
        engine = Engine(Program([]))

        # Test wrong arity handling through normal query execution
        # These should fail to match or execute properly

        # findall with wrong arity
        wrong_arity_query = parse_program("findall(X, goal).")[0].head
        solutions = list(engine.solve(wrong_arity_query))
        assert len(solutions) == 0  # Should fail

    def test_builtin_module_extraction(self):
        """Test that the extraction properly moved code to builtins module."""
        # Test that we can import the extracted module
        # (imported at module top)

        # Verify the module has the expected exports
        assert hasattr(solutions, "register")
        assert hasattr(solutions, "builtin_findall")
        assert hasattr(solutions, "builtin_bagof")
        assert hasattr(solutions, "builtin_setof")
        assert hasattr(solutions, "collect_all_solutions")

        # Verify registration function works
        test_registry = {}
        solutions.register(test_registry)

        assert ("findall", 3) in test_registry
        assert ("bagof", 3) in test_registry
        assert ("setof", 3) in test_registry


class TestPhase5Compliance:
    """Test compliance with Phase 5 requirements."""

    def test_no_engine_private_methods(self):
        """Verify that old private methods are removed from engine."""
        engine = Engine(Program([]))

        # These methods should no longer exist after extraction
        assert not hasattr(engine, "_builtin_findall")
        assert not hasattr(engine, "_builtin_bagof")
        assert not hasattr(engine, "_builtin_setof")
        assert not hasattr(engine, "_collect_all_solutions")
        assert not hasattr(engine, "_sort_and_deduplicate")
        assert not hasattr(engine, "_extract_existential_vars")
        assert not hasattr(engine, "_find_free_variables")
        assert not hasattr(engine, "_reify_term_with_solution")

    def test_builtin_registry_integration(self):
        """Test that builtins integrate properly with registry system."""
        # Test that register_all includes solutions (imported at module top)
        test_registry = {}
        register_all(test_registry)

        # Should include all-solutions predicates
        assert ("findall", 3) in test_registry
        assert ("bagof", 3) in test_registry
        assert ("setof", 3) in test_registry

        # Should still include other builtins from previous phases
        assert ("var", 1) in test_registry  # from types
        assert ("is", 2) in test_registry  # from arithmetic
        assert ("=..", 2) in test_registry  # from terms

    def test_semantic_preservation(self, sample_program):
        """Test that semantics are preserved after extraction."""
        engine = Engine(sample_program)

        # Test that all three predicates work with identical goals
        # and produce expected different behaviors

        template_goal = "person(X)"

        # findall: always succeeds, collects all
        findall_query = parse_program(f"findall(X, {template_goal}, L).")[0].head
        findall_solutions = list(engine.solve(findall_query))
        assert len(findall_solutions) == 1
        findall_result = findall_solutions[0]["L"]

        # bagof: succeeds when solutions exist, collects all
        bagof_query = parse_program(f"bagof(X, {template_goal}, L).")[0].head
        bagof_solutions = list(engine.solve(bagof_query))
        assert len(bagof_solutions) == 1
        bagof_result = bagof_solutions[0]["L"]

        # setof: succeeds when solutions exist, deduplicates
        setof_query = parse_program(f"setof(X, {template_goal}, L).")[0].head
        setof_solutions = list(engine.solve(setof_query))
        assert len(setof_solutions) == 1
        setof_result = setof_solutions[0]["L"]

        # For this case, findall and bagof should be identical
        assert len(findall_result.items) == len(bagof_result.items) == 2
        assert len(setof_result.items) == 2  # no duplicates in this case
