"""Unit tests for findall/3 handling of free variables from outer context.

Tests for issue #427: findall/3 doesn't properly handle free variables
that appear in the goal but not in the template when those variables
are bound in the outer context.
"""

from __future__ import annotations

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.parser.parser import parse_program


@pytest.mark.iso
class TestFindallFreeVariables:
    """Test findall/3 handling of variables from outer context."""

    def test_findall_with_outer_bound_variable_simple(self):
        """findall/3 should use outer binding for variables in goal but not template."""
        # Define simple facts for testing
        program_text = """
        color(red).
        color(blue).
        color(green).

        has_color(apple, red).
        has_color(sky, blue).
        has_color(grass, green).
        has_color(fire, red).
        """

        clauses = parse_program(program_text)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # X = red, findall(Y, has_color(Y, X), Things)
        # X is bound in outer context, should find all things with color red
        query = Struct(
            ",",
            (
                Struct("=", (Var(0, "X"), Atom("red"))),
                Struct(
                    "findall",
                    (
                        Var(1, "Y"),
                        Struct("has_color", (Var(1, "Y"), Var(0, "X"))),
                        Var(2, "Things"),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

        things = solutions[0]["Things"]
        assert isinstance(things, List)
        # Should find apple and fire (both red) - order not guaranteed
        assert len(things.items) == 2
        found_items = set(things.items)
        assert found_items == {Atom("apple"), Atom("fire")}

    def test_findall_with_multiple_outer_bindings(self):
        """findall/3 should handle multiple outer variable bindings."""
        program_text = """
        between(Low, High, Low) :- Low =< High.
        between(Low, High, X) :-
            Low < High,
            Low1 is Low + 1,
            between(Low1, High, X).
        """

        clauses = parse_program(program_text)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # Min = 3, Max = 5, findall(N, between(Min, Max, N), Numbers)
        # Both Min and Max are bound in outer context
        query = Struct(
            ",",
            (
                Struct("=", (Var(0, "Min"), Int(3))),
                Struct(
                    ",",
                    (
                        Struct("=", (Var(1, "Max"), Int(5))),
                        Struct(
                            "findall",
                            (
                                Var(2, "N"),
                                Struct(
                                    "between",
                                    (Var(0, "Min"), Var(1, "Max"), Var(2, "N")),
                                ),
                                Var(3, "Numbers"),
                            ),
                        ),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

        numbers = solutions[0]["Numbers"]
        assert isinstance(numbers, List)
        # Should find 3, 4, 5
        assert len(numbers.items) == 3
        assert Int(3) in numbers.items
        assert Int(4) in numbers.items
        assert Int(5) in numbers.items

    def test_findall_preserves_outer_bindings_after_call(self):
        """Outer variable bindings should remain unchanged after findall/3."""
        program_text = """
        fact(one).
        fact(two).
        fact(three).
        """

        clauses = parse_program(program_text)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # X = bound_value, findall(Y, fact(Y), List), X = bound_value
        # X should still be bound to bound_value after findall
        query = Struct(
            ",",
            (
                Struct("=", (Var(0, "X"), Atom("bound_value"))),
                Struct(
                    ",",
                    (
                        Struct(
                            "findall",
                            (
                                Var(1, "Y"),
                                Struct("fact", (Var(1, "Y"),)),
                                Var(2, "List"),
                            ),
                        ),
                        # Check X is still bound to same value
                        Struct("=", (Var(0, "X"), Atom("bound_value"))),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1
        assert solutions[0]["X"] == Atom("bound_value")

    def test_findall_with_compound_goal_using_outer_var(self):
        """findall/3 should handle compound goals with outer variables."""
        program_text = """
        parent(john, mary).
        parent(john, tom).
        parent(mary, ann).
        parent(tom, bob).

        male(john).
        male(tom).
        male(bob).

        female(mary).
        female(ann).
        """

        clauses = parse_program(program_text)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # P = john, findall(C, (parent(P, C), male(C)), Sons)
        # P is bound in outer context, find all male children of john
        query = Struct(
            ",",
            (
                Struct("=", (Var(0, "P"), Atom("john"))),
                Struct(
                    "findall",
                    (
                        Var(1, "C"),
                        Struct(
                            ",",
                            (
                                Struct("parent", (Var(0, "P"), Var(1, "C"))),
                                Struct("male", (Var(1, "C"),)),
                            ),
                        ),
                        Var(2, "Sons"),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

        sons = solutions[0]["Sons"]
        assert isinstance(sons, List)
        # Should find only tom (male child of john)
        assert len(sons.items) == 1
        assert Atom("tom") in sons.items

    def test_findall_nested_with_outer_variables(self):
        """Nested findall calls should each access their outer context correctly."""
        program_text = """
        num(1).
        num(2).
        num(3).

        less_than(X, Y) :- X < Y.
        """

        clauses = parse_program(program_text)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # X = 2, findall(Y, (num(Y), less_than(X, Y)), GreaterNums)
        # X is bound to 2, should find all numbers greater than 2
        query = Struct(
            ",",
            (
                Struct("=", (Var(0, "X"), Int(2))),
                Struct(
                    "findall",
                    (
                        Var(1, "Y"),
                        Struct(
                            ",",
                            (
                                Struct("num", (Var(1, "Y"),)),
                                Struct("less_than", (Var(0, "X"), Var(1, "Y"))),
                            ),
                        ),
                        Var(2, "GreaterNums"),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

        greater_nums = solutions[0]["GreaterNums"]
        assert isinstance(greater_nums, List)
        # Should find only 3 (greater than 2)
        assert len(greater_nums.items) == 1
        assert Int(3) in greater_nums.items

    def test_findall_with_no_outer_variables(self):
        """findall/3 should work normally when no outer variables are involved."""
        program_text = """
        item(a).
        item(b).
        item(c).
        """

        clauses = parse_program(program_text)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # findall(X, item(X), Items)
        # No outer variables, should collect all items
        query = Struct(
            "findall", (Var(0, "X"), Struct("item", (Var(0, "X"),)), Var(1, "Items"))
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

        items = solutions[0]["Items"]
        assert isinstance(items, List)
        assert len(items.items) == 3
        assert Atom("a") in items.items
        assert Atom("b") in items.items
        assert Atom("c") in items.items

    @pytest.mark.swi_baseline
    def test_findall_free_vars_swi_baseline(self, swi):
        """Compare findall free variable behavior with SWI-Prolog."""
        program = """
        has_color(apple, red).
        has_color(sky, blue).
        has_color(fire, red).
        """

        # Test with outer bound variable - X is bound in outer context
        # This should find all items with color red
        goal = "X = red, findall(Y, has_color(Y, X), Things)"
        count = swi.count(program, goal)
        # SWI should succeed with one solution
        assert count == 1

        # Now extract the Things list to verify it contains apple and fire
        # We'll use a modified goal to extract just the list elements
        goal2 = "X = red, findall(Y, has_color(Y, X), Things), member(Item, Things)"
        items = swi.onevar(program, goal2, "Item")
        # Should find apple and fire (both have color red)
        assert len(items) == 2
        assert "apple" in items
        assert "fire" in items
