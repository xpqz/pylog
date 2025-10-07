"""Tests for all-solutions builtins extraction.

This test verifies that the extracted all-solutions predicates (findall/3, bagof/3, setof/3)
in builtins/solutions.py work exactly the same as the original engine implementations.

This follows the TDD approach for Phase 5 of the engine refactoring plan.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, List as PrologList
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.parser.parser import parse_program
from prolog.engine.builtins.solutions import collect_all_solutions
from prolog.tests.unit.test_all_solutions_predicates import (
    TestFindallPredicate,
    TestBagofPredicate,
    TestSetofPredicate,
    TestAllSolutionsIntegration,
)


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
    """Test that extracted solutions module works identically to engine implementation."""

    def test_findall_extraction_compatibility(self, sample_program):
        """Test that extracted findall/3 works same as original engine implementation."""
        engine = Engine(sample_program)

        # Test basic findall functionality using builtin registry
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("color", (template,))
        result_list = Var(engine.store.new_var("L"), "L")

        # Call extracted implementation through builtin registry
        findall_builtin = engine._builtins[("findall", 3)]
        result = findall_builtin(engine, (template, goal, result_list))
        assert result is True

        # Get the bound result
        deref_result = engine.store.deref(result_list.id)
        assert deref_result[0] == "BOUND"

        # Verify the result contains expected colors
        bound_list = deref_result[2]
        assert isinstance(bound_list, PrologList)

        # Count items in nested list structure
        def count_list_items(plist):
            if not isinstance(plist, PrologList):
                return 0
            if plist.tail == Atom("[]"):
                return len(plist.items)
            else:
                return len(plist.items) + count_list_items(plist.tail)

        assert count_list_items(bound_list) == 3  # red, blue, green

    def test_bagof_extraction_compatibility(self, sample_program):
        """Test that extracted bagof/3 works same as original engine implementation."""
        engine = Engine(sample_program)

        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("person", (template,))  # Use existing person predicate
        result_bag = Var(engine.store.new_var("L"), "L")

        # Call extracted implementation through builtin registry
        bagof_builtin = engine._builtins[("bagof", 3)]
        result = bagof_builtin(engine, (template, goal, result_bag))
        assert result is True

        # Get the bound result
        deref_result = engine.store.deref(result_bag.id)
        assert deref_result[0] == "BOUND"

        # Verify the result contains expected persons
        bound_list = deref_result[2]
        assert isinstance(bound_list, PrologList)

        # Count items in nested list structure
        def count_list_items(plist):
            if not isinstance(plist, PrologList):
                return 0
            if plist.tail == Atom("[]"):
                return len(plist.items)
            else:
                return len(plist.items) + count_list_items(plist.tail)

        assert count_list_items(bound_list) == 2  # alice, bob

    def test_setof_extraction_compatibility(self, sample_program):
        """Test that extracted setof/3 works same as original engine implementation."""
        engine = Engine(sample_program)

        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("color", (template,))  # Use existing color predicate
        result_set = Var(engine.store.new_var("L"), "L")

        # Call extracted implementation through builtin registry
        setof_builtin = engine._builtins[("setof", 3)]
        result = setof_builtin(engine, (template, goal, result_set))
        assert result is True

        # Get the bound result
        deref_result = engine.store.deref(result_set.id)
        assert deref_result[0] == "BOUND"

        # Verify the result contains expected colors (sorted)
        bound_list = deref_result[2]
        assert isinstance(bound_list, PrologList)

        # Count items in nested list structure
        def count_list_items(plist):
            if not isinstance(plist, PrologList):
                return 0
            if plist.tail == Atom("[]"):
                return len(plist.items)
            else:
                return len(plist.items) + count_list_items(plist.tail)

        assert count_list_items(bound_list) == 3  # red, blue, green (sorted)

    def test_collect_all_solutions_helper_extraction(self, sample_program):
        """Test that extracted collect_all_solutions helper works as expected."""
        engine = Engine(sample_program)

        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("person", (template,))

        # Call extracted implementation directly from solutions module
        solutions = collect_all_solutions(engine, template, goal, existential_all=False)
        assert isinstance(solutions, list)
        assert len(solutions) == 2  # Should find alice and bob

    def test_registration_compatibility(self):
        """Test that extracted predicates are properly registered."""
        # Create empty program for basic registration test
        engine = Engine(Program([]))

        # Verify current registrations exist
        assert ("findall", 3) in engine._builtins
        assert ("bagof", 3) in engine._builtins
        assert ("setof", 3) in engine._builtins

        # TODO: After extraction, verify that new registration system works
        # and that predicates are still registered with same keys

    def test_error_handling_compatibility(self):
        """Test that extracted predicates handle errors same as original."""
        engine = Engine(Program([]))

        # Test wrong arity handling through builtin registry
        findall_builtin = engine._builtins[("findall", 3)]
        bagof_builtin = engine._builtins[("bagof", 3)]
        setof_builtin = engine._builtins[("setof", 3)]

        assert (
            findall_builtin(engine, (Atom("x"), Atom("goal"))) is False
        )  # 2 args instead of 3
        assert (
            bagof_builtin(engine, (Atom("x"), Atom("goal"))) is False
        )  # 2 args instead of 3
        assert (
            setof_builtin(engine, (Atom("x"), Atom("goal"))) is False
        )  # 2 args instead of 3


class TestBuiltinInfrastructureIntegration:
    """Test that extraction integrates properly with builtin infrastructure."""

    def test_builtins_init_includes_solutions(self):
        """Test that builtins/__init__.py properly imports and registers solutions module."""
        # TODO: After creating solutions.py, verify it's imported in __init__.py
        # and that register_all() calls register_solutions()
        pass

    def test_engine_delegates_to_extracted_predicates(self):
        """Test that engine properly delegates to extracted predicates."""
        # TODO: After extraction, verify that engine methods are thin wrappers
        # that delegate to the extracted implementations
        pass


class TestBackwardCompatibility:
    """Test that extraction maintains backward compatibility."""

    def test_existing_tests_still_pass(self):
        """Verify that all existing all-solutions tests still pass after extraction."""
        # This test serves as a placeholder to remind us to run the existing
        # test_all_solutions_predicates.py after extraction to ensure no regressions

        # The existing test module is imported at top level
        # The actual tests will be run by pytest when the full suite is executed
        assert TestFindallPredicate is not None
        assert TestBagofPredicate is not None
        assert TestSetofPredicate is not None
        assert TestAllSolutionsIntegration is not None
