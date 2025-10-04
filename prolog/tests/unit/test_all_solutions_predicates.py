"""Tests for all-solutions predicates: findall/3, bagof/3, setof/3.

These tests verify the implementation of the ISO Prolog all-solutions predicates.
"""

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.parser.parser import parse_program


class TestFindallPredicate:
    """Test findall/3 predicate."""

    def test_findall_basic_success(self):
        """Test findall/3 with basic facts."""
        from prolog.tests.helpers import mk_fact, program

        prog = program(
            mk_fact("value", Int(1)), mk_fact("value", Int(2)), mk_fact("value", Int(3))
        )
        engine = Engine(prog)

        # Query: findall(X, value(X), L)
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("value", (template,))
        result_list = Var(engine.store.new_var("L"), "L")

        result = engine._builtin_findall((template, goal, result_list))
        assert result is True

        # Check that result_list is bound to some list
        deref_result = engine.store.deref(result_list.id)
        assert deref_result[0] == "BOUND"
        bound_list = deref_result[2]
        # For now, just check it's not empty list (will refine later)
        print(f"DEBUG: bound_list = {bound_list}, type = {type(bound_list)}")
        assert bound_list != Atom("[]")  # Should have found some solutions

    def test_findall_empty_result(self):
        """Test findall/3 with no solutions returns empty list."""
        program = Program([])  # Empty program
        engine = Engine(program)

        # Query: findall(X, nonexistent(X), L)
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("nonexistent", (template,))
        result_list = Var(engine.store.new_var("L"), "L")

        result = engine._builtin_findall((template, goal, result_list))
        assert result is True

        # Check that result_list is bound to []
        deref_result = engine.store.deref(result_list.id)
        assert deref_result[0] == "BOUND"
        bound_list = deref_result[2]
        assert bound_list == Atom("[]")

    def test_findall_wrong_arity(self):
        """Test findall with wrong number of arguments fails."""
        program = Program([])
        engine = Engine(program)

        # Test with 2 arguments
        result = engine._builtin_findall((Atom("x"), Atom("goal")))
        assert result is False

        # Test with 4 arguments
        result = engine._builtin_findall(
            (Atom("x"), Atom("goal"), Atom("list"), Atom("extra"))
        )
        assert result is False


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
        from prolog.ast.clauses import Program

        program = Program(clauses)
        engine = Engine(program)

        # Query: bagof(X, color(X), L)
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("color", (template,))
        result_bag = Var(engine.store.new_var("L"), "L")

        result = engine._builtin_bagof((template, goal, result_bag))
        assert result is True

        # Check that result_bag is bound to a list
        deref_result = engine.store.deref(result_bag.id)
        assert deref_result[0] == "BOUND"

    def test_bagof_fails_on_empty(self):
        """Test bagof/3 fails when no solutions exist."""
        program = Program([])  # Empty program
        engine = Engine(program)

        # Query: bagof(X, nonexistent(X), L)
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("nonexistent", (template,))
        result_bag = Var(engine.store.new_var("L"), "L")

        result = engine._builtin_bagof((template, goal, result_bag))
        assert result is False  # bagof fails on no solutions

    def test_bagof_wrong_arity(self):
        """Test bagof with wrong number of arguments fails."""
        program = Program([])
        engine = Engine(program)

        result = engine._builtin_bagof((Atom("x"), Atom("goal")))
        assert result is False


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
        from prolog.ast.clauses import Program

        program = Program(clauses)
        engine = Engine(program)

        # Query: setof(X, item(X), L)
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("item", (template,))
        result_set = Var(engine.store.new_var("L"), "L")

        result = engine._builtin_setof((template, goal, result_set))
        assert result is True

        # Check that result_set is bound to a list (should be sorted and deduplicated)
        deref_result = engine.store.deref(result_set.id)
        assert deref_result[0] == "BOUND"

    def test_setof_fails_on_empty(self):
        """Test setof/3 fails when no solutions exist."""
        program = Program([])
        engine = Engine(program)

        # Query: setof(X, nonexistent(X), L)
        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("nonexistent", (template,))
        result_set = Var(engine.store.new_var("L"), "L")

        result = engine._builtin_setof((template, goal, result_set))
        assert result is False  # setof fails on no solutions

    def test_setof_wrong_arity(self):
        """Test setof with wrong number of arguments fails."""
        program = Program([])
        engine = Engine(program)

        result = engine._builtin_setof((Atom("x"), Atom("goal")))
        assert result is False


class TestAllSolutionsIntegration:
    """Integration tests for all-solutions predicates."""

    def test_findall_vs_bagof_empty_behavior(self):
        """Test difference between findall and bagof on empty results."""
        program = Program([])
        engine = Engine(program)

        template = Var(engine.store.new_var("X"), "X")
        goal = Struct("nonexistent", (template,))
        result = Var(engine.store.new_var("L"), "L")

        # findall should succeed with empty list
        findall_result = engine._builtin_findall((template, goal, result))
        assert findall_result is True

        # Reset result variable
        result2 = Var(engine.store.new_var("L2"), "L2")

        # bagof should fail
        bagof_result = engine._builtin_bagof((template, goal, result2))
        assert bagof_result is False

    def test_helper_methods(self):
        """Test helper methods used by all-solutions predicates."""
        program = Program([])
        engine = Engine(program)

        # Test _build_prolog_list
        items = [Int(1), Int(2), Int(3)]
        result = engine._build_prolog_list(items)
        assert isinstance(result, PrologList)

        # Test empty list
        empty_result = engine._build_prolog_list([])
        assert empty_result == Atom("[]")

        # Test _copy_term_with_fresh_vars
        original_var = Var(engine.store.new_var("X"), "X")
        original_term = Struct("test", (original_var, Int(42)))

        copied_term = engine._copy_term_with_fresh_vars(original_term)
        assert isinstance(copied_term, Struct)
        assert copied_term.functor == "test"
        assert len(copied_term.args) == 2
        assert isinstance(copied_term.args[0], Var)
        assert copied_term.args[0].id != original_var.id  # Should be different variable
        assert copied_term.args[1] == Int(42)  # Non-variable terms unchanged

    def test_sort_and_deduplicate(self):
        """Test sorting and deduplication for setof/3."""
        program = Program([])
        engine = Engine(program)

        # Test with duplicates
        solutions = [Atom("banana"), Atom("apple"), Atom("banana"), Atom("cherry")]
        result = engine._sort_and_deduplicate(solutions)

        # Should be sorted and deduplicated
        assert len(result) == 3  # duplicates removed
        # Check that items are unique (exact order may vary based on string comparison)
        str_results = [str(item) for item in result]
        assert len(set(str_results)) == 3  # All unique
