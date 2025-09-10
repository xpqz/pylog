"""
Tests for once/1 builtin predicate.
once(Goal) succeeds at most once, cutting away remaining choicepoints.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact, mk_rule


@pytest.fixture
def multi_solution_engine():
    """Create an engine with predicates that have multiple solutions."""
    p = program(
        mk_fact("color", Atom("red")),
        mk_fact("color", Atom("green")),
        mk_fact("color", Atom("blue")),
        mk_fact("number", Int(1)),
        mk_fact("number", Int(2)),
        mk_fact("number", Int(3)),
        mk_fact("single", Atom("only")),
    )
    return Engine(p)


class TestOnceBasic:
    """Basic tests for once/1 builtin."""

    def test_once_with_single_solution(self, multi_solution_engine):
        """Test once/1 with predicate that has single solution."""
        X = Var(0, "X")
        query = Struct("once", (Struct("single", (X,)),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("only")

    def test_once_limits_multiple_solutions(self, multi_solution_engine):
        """Test once/1 returns only first solution."""
        X = Var(0, "X")
        query = Struct("once", (Struct("color", (X,)),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("red")  # First solution only

    def test_once_with_failing_goal(self, multi_solution_engine):
        """Test once/1 fails when goal fails."""
        query = Struct("once", (Struct("color", (Atom("yellow"),)),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 0

    def test_once_with_conjunction(self, multi_solution_engine):
        """Test once/1 with conjunction inside."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # once((color(X), number(Y))) - should get first combination only
        conj = Struct(",", (
            Struct("color", (X,)),
            Struct("number", (Y,))
        ))
        query = Struct("once", (conj,))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("red")
        assert solutions[0]["Y"] == Int(1)

    def test_once_with_disjunction(self, multi_solution_engine):
        """Test once/1 with disjunction inside."""
        X = Var(0, "X")
        # once((color(X) ; number(X))) - should get first solution from first branch
        disj = Struct(";", (
            Struct("color", (X,)),
            Struct("number", (X,))
        ))
        query = Struct("once", (disj,))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("red")


class TestOnceWithVariables:
    """Tests for once/1 with variable goals."""

    def test_once_with_unbound_goal_fails(self, multi_solution_engine):
        """Test once/1 fails with unbound variable as goal."""
        X = Var(0, "X")
        query = Struct("once", (X,))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 0

    def test_once_with_bound_variable_goal(self, multi_solution_engine):
        """Test once/1 with variable bound to goal."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = color(Y), once(X)
        query = Struct(",", (
            Struct("=", (X, Struct("color", (Y,)))),
            Struct("once", (X,))
        ))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["Y"] == Atom("red")

    def test_once_with_integer_goal_fails(self, multi_solution_engine):
        """Test once/1 fails with non-callable term."""
        query = Struct("once", (Int(42),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 0


class TestOnceNested:
    """Tests for nested once/1 calls."""

    def test_nested_once(self, multi_solution_engine):
        """Test nested once/1 calls."""
        X = Var(0, "X")
        # once(once(color(X))) - should be same as once(color(X))
        query = Struct("once", (Struct("once", (Struct("color", (X,)),)),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("red")

    def test_once_inside_disjunction(self, multi_solution_engine):
        """Test once/1 inside disjunction branches."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # (once(color(X)) ; number(Y))
        # First branch gives one solution, second branch gives three
        query = Struct(";", (
            Struct("once", (Struct("color", (X,)),)),
            Struct("number", (Y,))
        ))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 4  # 1 from once(color) + 3 from number
        # First solution from first branch
        assert solutions[0]["X"] == Atom("red")
        # Rest from second branch
        assert solutions[1]["Y"] == Int(1)
        assert solutions[2]["Y"] == Int(2)
        assert solutions[3]["Y"] == Int(3)


class TestOnceWithBacktracking:
    """Tests for once/1 interaction with backtracking."""

    def test_once_cuts_choicepoints(self, multi_solution_engine):
        """Test that once/1 removes choicepoints like cut."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # color(X), once(number(Y)), fail ; single(X)
        # The once should prevent backtracking to other numbers
        query = Struct(";", (
            Struct(",", (
                Struct("color", (X,)),
                Struct(",", (
                    Struct("once", (Struct("number", (Y,)),)),
                    Atom("fail")
                ))
            )),
            Struct("single", (X,))
        ))
        solutions = multi_solution_engine.run([query])
        # All color attempts fail after once(number(1)), so we get single
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("only")

    def test_once_in_conjunction_with_backtracking(self):
        """Test once/1 in conjunction with later failure."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("q", Atom("a")),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        # once(p(X)), q(X)
        # once gives X=a, q(a) succeeds
        query = Struct(",", (
            Struct("once", (Struct("p", (X,)),)),
            Struct("q", (X,))
        ))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")
        
        # Without once, we'd try p(b) on backtrack, but q(b) would fail
        # Let's verify this behaves differently without once:
        query_no_once = Struct(",", (
            Struct("p", (X,)),
            Struct("q", (X,))
        ))
        solutions_no_once = engine.run([query_no_once])
        assert len(solutions_no_once) == 1  # Same result, but different search


class TestOnceWithBuiltins:
    """Tests for once/1 with other builtins."""

    def test_once_with_cut_inside(self):
        """Test once/1 with cut inside the goal."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        # once((p(X), !)) - cut inside once
        goal = Struct(",", (Struct("p", (X,)), Atom("!")))
        query = Struct("once", (goal,))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")

    def test_once_with_true(self, multi_solution_engine):
        """Test once/1 with true."""
        query = Struct("once", (Atom("true"),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 1

    def test_once_with_fail(self, multi_solution_engine):
        """Test once/1 with fail."""
        query = Struct("once", (Atom("fail"),))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 0

    def test_once_wrong_arity(self, multi_solution_engine):
        """Test once with wrong number of arguments."""
        query = Struct("once", (Atom("true"), Atom("extra")))
        solutions = multi_solution_engine.run([query])
        assert len(solutions) == 0