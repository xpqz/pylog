"""Tests for once/1 builtin.

Tests deterministic execution for Stage 1.

once/1 succeeds at most once - it commits to the first solution of its goal
and cuts away any remaining choicepoints created by that goal.

Error policy: dev-mode. Ill-typed calls to once/1 fail rather than throwing
ISO errors. ISO error behavior to be added in later stages.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, mk_rule, program


class TestOnceBasicBehavior:
    """Test basic once/1 functionality."""

    def test_once_deterministic_goal(self):
        """Test once/1 with already deterministic goal."""
        p = program(mk_fact("det", Atom("a")))
        engine = Engine(p)

        # Query: once(det(a))
        results = engine.run([Struct("once", (Struct("det", (Atom("a"),)),))])

        assert len(results) == 1

    def test_once_nondeterministic_goal(self):
        """Test once/1 commits to first solution only."""
        p = program(
            mk_fact("nondet", Atom("a")),
            mk_fact("nondet", Atom("b")),
            mk_fact("nondet", Atom("c")),
        )
        engine = Engine(p)

        # Query: once(nondet(X))
        results = engine.run([Struct("once", (Struct("nondet", (Var(0, "X"),)),))])

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")  # Should get first solution only

    def test_once_failing_goal(self):
        """Test once/1 with failing goal fails."""
        p = program(mk_fact("fact", Atom("a")))
        engine = Engine(p)

        # Query: once(fact(b))
        results = engine.run([Struct("once", (Struct("fact", (Atom("b"),)),))])

        assert len(results) == 0

    def test_once_with_true(self):
        """Test once(true) succeeds exactly once."""
        engine = Engine(program())

        # Query: once(true)
        results = engine.run([Struct("once", (Atom("true"),))])

        assert len(results) == 1

    def test_once_with_fail(self):
        """Test once(fail) fails."""
        engine = Engine(program())

        # Query: once(fail)
        results = engine.run([Struct("once", (Atom("fail"),))])

        assert len(results) == 0


class TestOnceWithConjunction:
    """Test once/1 with conjunctive goals."""

    def test_once_conjunction_first_solution(self):
        """Test once((p(X), q(X))) commits to first combined solution.

        Note: Assumes clause order p(a), p(b), q(a), q(b) so first
        combined solution is X=a. Future indexing may change this.
        """
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b")),
        )
        engine = Engine(p)

        # Query: once((p(X), q(X)))
        results = engine.run(
            [
                Struct(
                    "once",
                    (
                        Struct(
                            ",",
                            (Struct("p", (Var(0, "X"),)), Struct("q", (Var(0, "X"),))),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")

    def test_once_conjunction_no_solution(self):
        """Test once/1 with failing conjunction."""
        p = program(mk_fact("p", Atom("a")), mk_fact("q", Atom("b")))
        engine = Engine(p)

        # Query: once((p(X), q(X)))
        results = engine.run(
            [
                Struct(
                    "once",
                    (
                        Struct(
                            ",",
                            (Struct("p", (Var(0, "X"),)), Struct("q", (Var(0, "X"),))),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 0


class TestOnceWithDisjunction:
    """Test once/1 with disjunctive goals."""

    def test_once_disjunction_first_branch(self):
        """Test once((p(X) ; q(X))) takes first successful branch."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("q", Atom("c")),
            mk_fact("q", Atom("d")),
        )
        engine = Engine(p)

        # Query: once((p(X) ; q(X)))
        results = engine.run(
            [
                Struct(
                    "once",
                    (
                        Struct(
                            ";",
                            (Struct("p", (Var(0, "X"),)), Struct("q", (Var(0, "X"),))),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")  # First solution from first branch

    def test_once_disjunction_second_branch(self):
        """Test once/1 uses second branch if first fails."""
        p = program(mk_fact("q", Atom("c")))
        engine = Engine(p)

        # Query: once((p(X) ; q(X)))
        results = engine.run(
            [
                Struct(
                    "once",
                    (
                        Struct(
                            ";",
                            (Struct("p", (Var(0, "X"),)), Struct("q", (Var(0, "X"),))),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("c")


class TestOnceInteractionWithCut:
    """Test once/1 interaction with cut."""

    def test_once_contains_cut(self):
        """Test once/1 with goal containing cut."""
        p = program(
            mk_fact("p", Atom("a")), mk_fact("p", Atom("b")), mk_fact("q", Atom("x"))
        )
        engine = Engine(p)

        # Query: once((p(X), !, q(x)))
        results = engine.run(
            [
                Struct(
                    "once",
                    (
                        Struct(
                            ",",
                            (
                                Struct(
                                    ",", (Struct("p", (Var(0, "X"),)), Struct("!", ()))
                                ),
                                Struct("q", (Atom("x"),)),
                            ),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")

    def test_once_after_cut(self):
        """Test cut before once/1 doesn't affect once's behavior."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("q", Atom("x")),
            mk_fact("q", Atom("y")),
        )
        engine = Engine(p)

        # Query: p(a), !, once(q(X))
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct(",", (Struct("p", (Atom("a"),)), Struct("!", ()))),
                        Struct("once", (Struct("q", (Var(0, "X"),)),)),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("x")  # once still takes first q/1 solution


class TestOnceNesting:
    """Test nested once/1 calls."""

    def test_nested_once(self):
        """Test once(once(Goal))."""
        p = program(mk_fact("p", Atom("a")), mk_fact("p", Atom("b")))
        engine = Engine(p)

        # Query: once(once(p(X)))
        results = engine.run(
            [Struct("once", (Struct("once", (Struct("p", (Var(0, "X"),)),)),))]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")

    def test_once_inside_conjunction(self):
        """Test conjunction with once/1 in middle."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b")),
            mk_fact("r", Atom("a")),
            mk_fact("r", Atom("b")),
        )
        engine = Engine(p)

        # Query: p(X), once(q(X)), r(X)
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct(
                            ",",
                            (
                                Struct("p", (Var(0, "X"),)),
                                Struct("once", (Struct("q", (Var(0, "X"),)),)),
                            ),
                        ),
                        Struct("r", (Var(0, "X"),)),
                    ),
                )
            ]
        )

        # Should try p(a), succeed with once(q(a)), succeed with r(a)
        # Then backtrack to p(b), succeed with once(q(b)), succeed with r(b)
        assert len(results) == 2
        assert results[0]["X"] == Atom("a")
        assert results[1]["X"] == Atom("b")


class TestOnceWithCall:
    """Test once/1 with call/1."""

    def test_once_of_call(self):
        """Test once(call(Goal))."""
        p = program(mk_fact("p", Atom("a")), mk_fact("p", Atom("b")))
        engine = Engine(p)

        # Query: once(call(p(X)))
        results = engine.run(
            [Struct("once", (Struct("call", (Struct("p", (Var(0, "X"),)),)),))]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")

    def test_call_of_once(self):
        """Test call(once(Goal))."""
        p = program(mk_fact("p", Atom("a")), mk_fact("p", Atom("b")))
        engine = Engine(p)

        # Query: call(once(p(X)))
        results = engine.run(
            [Struct("call", (Struct("once", (Struct("p", (Var(0, "X"),)),)),))]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("a")


class TestOnceErrorCases:
    """Test error conditions for once/1."""

    def test_once_with_variable_goal(self):
        """Test once(X) with unbound X fails in dev-mode."""
        engine = Engine(program())

        # Query: once(X)
        results = engine.run([Struct("once", (Var(0, "X"),))])

        assert len(results) == 0  # Dev-mode: fail on insufficient instantiation

    def test_once_with_integer_goal(self):
        """Test once(42) fails in dev-mode."""
        engine = Engine(program())

        # Query: once(42)
        results = engine.run([Struct("once", (Int(42),))])

        assert len(results) == 0  # Dev-mode: fail on type error

    def test_once_wrong_arity(self):
        """Test once/2 is undefined."""
        engine = Engine(program())

        # Query: once(p(X), Y)
        # Note: This relies on dev-mode undefined predicate policy (fail)
        # ISO mode would throw existence_error
        results = engine.run(
            [Struct("once", (Struct("p", (Var(0, "X"),)), Var(1, "Y")))]
        )

        assert len(results) == 0  # once/2 doesn't exist


class TestOnceSemantics:
    """Test that once/1 has correct determinism semantics."""

    def test_once_is_semidet(self):
        """Test once/1 succeeds at most once (semidet)."""
        p = program(
            mk_fact("multi", Atom("a")),
            mk_fact("multi", Atom("b")),
            mk_fact("multi", Atom("c")),
        )
        engine = Engine(p)

        # Query: once(multi(X)), multi(Y)
        # once commits to X=a, then multi(Y) generates all solutions
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("once", (Struct("multi", (Var(0, "X"),)),)),
                        Struct("multi", (Var(1, "Y"),)),
                    ),
                )
            ]
        )

        assert len(results) == 3
        # X should always be 'a' (first solution)
        for result in results:
            assert result["X"] == Atom("a")
        # Y should be all values
        assert [r["Y"] for r in results] == [Atom("a"), Atom("b"), Atom("c")]

    def test_once_removes_choicepoints(self):
        """Test that once/1 removes choicepoints from its goal."""
        p = program(
            mk_rule(
                "test",
                (Var(0, "X"),),
                Struct(
                    "once",
                    (
                        Struct(
                            "member",
                            (
                                Var(0, "X"),
                                Struct(
                                    ".",
                                    (Atom("a"), Struct(".", (Atom("b"), Atom("[]")))),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            # Simple member/2 that would normally be nondeterministic
            mk_fact("member", Var(0, "X"), Struct(".", (Var(0, "X"), Var(1, "_")))),
            mk_rule(
                "member",
                (Var(0, "X"), Struct(".", (Var(1, "_"), Var(2, "T")))),
                Struct("member", (Var(0, "X"), Var(2, "T"))),
            ),
        )
        engine = Engine(p)

        # Query: test(X)
        results = engine.run([Struct("test", (Var(0, "X"),))])

        # Should only get one solution due to once/1
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")


class TestOnceAdvancedSemantics:
    """Test advanced semantic properties of once/1."""

    def test_once_equivalent_to_call_then_cut(self):
        """Test once(G) has same semantics as (call(G), !)."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
        )

        # Test once(p(X))
        e1 = Engine(p)
        r1 = e1.run([Struct("once", (Struct("p", (Var(0, "X"),)),))])

        # Test (call(p(X)), !)
        e2 = Engine(p)
        r2 = e2.run(
            [
                Struct(
                    ",",
                    (Struct("call", (Struct("p", (Var(0, "X"),)),)), Struct("!", ())),
                )
            ]
        )

        assert len(r1) == 1
        assert len(r2) == 1
        assert r1[0]["X"] == r2[0]["X"] == Atom("a")

    def test_once_does_not_prune_surrounding_choicepoints(self):
        """Test once/1 only prunes choicepoints created by its goal."""
        p = program(mk_fact("left"), mk_fact("right"))

        # Query: (left ; right), once(true)
        # Should get two solutions: one for left, one for right
        results = Engine(p).run(
            [
                Struct(
                    ",",
                    (
                        Struct(";", (Atom("left"), Atom("right"))),
                        Struct("once", (Atom("true"),)),
                    ),
                )
            ]
        )

        assert len(results) == 2

    def test_once_prunes_inner_not_outer(self):
        """Test once/1 prunes inner choicepoints but preserves outer ones."""
        p = program(mk_fact("p", Atom("a")), mk_fact("p", Atom("b")), mk_fact("q"))

        # Query: (p(X), once(q) ; p(Y), q)
        # Left branch: p has 2 solutions, once(q) succeeds once => 2 solutions
        # Right branch: p has 2 solutions, q succeeds => 2 solutions
        # But we need distinct variables
        results = Engine(p).run(
            [
                Struct(
                    ";",
                    (
                        Struct(
                            ",",
                            (Struct("p", (Var(0, "X"),)), Struct("once", (Atom("q"),))),
                        ),
                        Struct(",", (Struct("p", (Var(1, "Y"),)), Atom("q"))),
                    ),
                )
            ]
        )

        # Should get 4 solutions total
        assert len(results) == 4

    def test_once_on_disjunction_with_fail(self):
        """Test once((fail ; q)) succeeds once via second branch."""
        p = program(mk_fact("q"))

        # Query: once((fail ; q))
        results = Engine(p).run(
            [Struct("once", (Struct(";", (Atom("fail"), Atom("q"))),))]
        )

        assert len(results) == 1

    def test_once_call_uninstantiated_goal_fails_dev_mode(self):
        """Test once(call(G)) with unbound G fails in dev-mode."""
        results = Engine(program()).run(
            [Struct("once", (Struct("call", (Var(0, "G"),)),))]
        )

        assert len(results) == 0  # Dev-mode: fail on uncallable

    def test_once_of_cut_succeeds_once(self):
        """Test once(!) succeeds exactly once."""
        results = Engine(program()).run([Struct("once", (Struct("!", ()),))])

        assert len(results) == 1

    def test_once_leaves_no_choicepoints_after_success(self):
        """Test once/1 cleans up all its internal choicepoints.

        Note: This test is aspirational - it will work when engine
        exposes choicepoint stack size for debugging.
        """
        e = Engine(program(mk_fact("p"), mk_fact("p")))  # nondet inside once
        results = e.run([Struct("once", (Atom("p"),))])

        assert len(results) == 1
        # When available, assert no residual choicepoints:
        # if hasattr(e, "choices"):
        #     assert len(e.choices) == 0

    def test_once_with_infinite_choices(self):
        """Test once/1 with potentially infinite solutions."""
        p = program(
            # repeat/0 - infinite choicepoint generator
            mk_fact("repeat"),
            mk_rule("repeat", (), Atom("repeat")),
        )

        # Query: once((repeat, true))
        # Should succeed exactly once despite repeat being infinite
        results = Engine(p).run(
            [Struct("once", (Struct(",", (Atom("repeat"), Atom("true"))),))]
        )

        assert len(results) == 1

    def test_once_bindings_persist(self):
        """Test that bindings made inside once/1 persist."""
        p = program(mk_fact("bind", Atom("a"), Atom("bound")))

        # Query: once(bind(a, Y)), Y = bound
        results = Engine(p).run(
            [
                Struct(
                    ",",
                    (
                        Struct("once", (Struct("bind", (Atom("a"), Var(0, "Y"))),)),
                        Struct("=", (Var(0, "Y"), Atom("bound"))),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["Y"] == Atom("bound")


# =============================================================================
# MERGED FROM test_once_builtin.py
# =============================================================================

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


class TestOnceNestedMerged:
    """Tests for nested once/1 calls (merged from test_once_builtin.py)."""

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


class TestOnceWithBacktrackingMerged:
    """Tests for once/1 interaction with backtracking (merged)."""

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


class TestOnceWithBuiltinsMerged:
    """Tests for once/1 with other builtins (merged)."""

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
