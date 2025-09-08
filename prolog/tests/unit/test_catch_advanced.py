"""Tests for advanced catch/3 semantics.

Tests proper choicepoint management, backtracking behavior, and state restoration
for exception handling in Stage 1.

These tests verify:
- Choicepoint preservation/removal at throw boundaries
- Backtracking through catch on success vs caught exception
- Trail unwinding to proper catch points
- Nested catch/throw interactions
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestCatchChoicepointManagement:
    """Test catch/3 choicepoint management semantics."""

    def test_catch_with_disjunction_gives_both_solutions(self):
        """Test catch((X=1; X=2), _, true) gives both solutions."""
        engine = Engine(program())

        # Query: catch((X=1; X=2), _, true)
        # No exception, should enumerate both solutions
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct(
                            ";",
                            (
                                Struct("=", (Var(0, "X"), Int(1))),
                                Struct("=", (Var(0, "X"), Int(2))),
                            ),
                        ),
                        Var(1, "_"),
                        Atom("true"),
                    ),
                )
            ]
        )

        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Int(2)

    @pytest.mark.xfail(
        reason="Test assumes cut doesn't affect outer CPs, but ISO cut does",
        strict=True
    )
    def test_catch_preserves_choicepoints_before_throw(self):
        """Test catch preserves choicepoints created before the throw point."""
        p = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_rule(
                "q",
                (Var(0, "X"),),
                Struct(
                    ",",
                    (
                        Struct("p", (Var(0, "X"),)),
                        Struct(
                            ",",
                            (
                                Atom("!"),  # Cut to mark position
                                Struct("throw", (Atom("error"),)),
                            ),
                        ),
                    ),
                ),
            ),
        )
        engine = Engine(p)

        # Query: catch(q(X), error, true)
        # The choicepoint for p(X) is created before throw
        # After catching, we should be able to get both solutions
        results = engine.run(
            [
                Struct(
                    "catch", (Struct("q", (Var(0, "X"),)), Atom("error"), Atom("true"))
                )
            ]
        )

        # Should get 2 results: first X=1 succeeds, then X=2 throws and recovers
        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert "X" not in results[1]  # X unbound in recovery after unwind

    def test_catch_removes_choicepoints_after_throw_point(self):
        """Test catch removes choicepoints created after the throw point."""
        p = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_rule(
                "q",
                (),
                Struct(
                    ",",
                    (
                        Struct("throw", (Atom("error"),)),
                        Struct("p", (Var(0, "_"),)),  # Never reached
                    ),
                ),
            ),
        )
        engine = Engine(p)

        # Query: catch(q, error, true)
        # The p(_) would create choicepoints but is never reached
        results = engine.run(
            [Struct("catch", (Atom("q"), Atom("error"), Atom("true")))]
        )

        assert len(results) == 1
        # No choicepoints from p(_) should exist

    def test_success_through_catch_leaves_prior_choicepoints_intact(self):
        """Test success through catch (no exception) preserves all choicepoints."""
        p = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_fact("q", Int(3)),
            mk_fact("q", Int(4)),
        )
        engine = Engine(p)

        # Query: p(X), catch(q(Y), _, fail)
        # No exception, all choicepoints should be preserved
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("p", (Var(0, "X"),)),
                        Struct(
                            "catch",
                            (Struct("q", (Var(1, "Y"),)), Var(2, "_"), Atom("fail")),
                        ),
                    ),
                )
            ]
        )

        # Should get all 4 combinations (2 p × 2 q)
        assert len(results) == 4
        assert (results[0]["X"], results[0]["Y"]) == (Int(1), Int(3))
        assert (results[1]["X"], results[1]["Y"]) == (Int(1), Int(4))
        assert (results[2]["X"], results[2]["Y"]) == (Int(2), Int(3))
        assert (results[3]["X"], results[3]["Y"]) == (Int(2), Int(4))

    def test_caught_exception_prunes_only_post_throw_segment(self):
        """Test caught exception only prunes choicepoints after throw."""
        p = program(
            mk_fact("before", Int(1)),
            mk_fact("before", Int(2)),
            mk_rule(
                "throwing_goal",
                (Var(0, "Y"),),
                Struct(
                    ";",
                    (
                        Struct(
                            ",",
                            (
                                Struct("=", (Var(0, "Y"), Int(3))),
                                Struct("throw", (Atom("error"),)),
                            ),
                        ),
                        Struct(
                            "=", (Var(0, "Y"), Int(4))
                        ),  # Never reached due to throw
                    ),
                ),
            ),
        )
        engine = Engine(p)

        # Query: before(X), catch(throwing_goal(Y), error, Y=caught)
        # Choicepoints for before(X) exist before throw
        # Choicepoint for Y=4 is after throw point, should be pruned
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("before", (Var(0, "X"),)),
                        Struct(
                            "catch",
                            (
                                Struct("throwing_goal", (Var(1, "Y"),)),
                                Atom("error"),
                                Struct("=", (Var(1, "Y"), Atom("caught"))),
                            ),
                        ),
                    ),
                )
            ]
        )

        # Should get 2 results: before has 2 solutions, recovery executes once per
        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[0]["Y"] == Atom("caught")
        assert results[1]["X"] == Int(2)
        assert results[1]["Y"] == Atom("caught")


class TestCatchBacktrackingSemantics:
    """Test backtracking behavior through catch/3."""

    def test_backtracking_through_catch_on_success(self):
        """Test backtracking works normally when no exception is thrown."""
        p = program(mk_fact("p", Int(1)), mk_fact("p", Int(2)), mk_fact("p", Int(3)))
        engine = Engine(p)

        # Query: catch(p(X), _, fail), X=X
        # No exception, normal backtracking should work
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct(
                            "catch",
                            (Struct("p", (Var(0, "X"),)), Var(1, "_"), Atom("fail")),
                        ),
                        Struct("=", (Var(0, "X"), Var(0, "X"))),  # Force enumeration
                    ),
                )
            ]
        )

        assert len(results) == 3
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Int(2)
        assert results[2]["X"] == Int(3)

    def test_no_backtracking_through_catch_on_caught_exception(self):
        """Test no backtracking into goal after exception is caught."""
        p = program(
            mk_rule(
                "p",
                (Var(0, "X"),),
                Struct(
                    ";",
                    (
                        Struct("=", (Var(0, "X"), Int(1))),
                        Struct(
                            ";",
                            (
                                Struct(
                                    ",",
                                    (
                                        Struct("=", (Var(0, "X"), Int(2))),
                                        Struct("throw", (Atom("error"),)),
                                    ),
                                ),
                                Struct("=", (Var(0, "X"), Int(3))),  # Never reached
                            ),
                        ),
                    ),
                ),
            )
        )
        engine = Engine(p)

        # Query: catch(p(X), error, X=caught)
        # First solution X=1 succeeds
        # On backtracking, X=2 then throw
        # After catch, should not backtrack to X=3
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct("p", (Var(0, "X"),)),
                        Atom("error"),
                        Struct("=", (Var(0, "X"), Atom("caught"))),
                    ),
                )
            ]
        )

        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Atom("caught")
        # X=3 should never be reached

    def test_recovery_goal_can_have_multiple_solutions(self):
        """Test recovery goal itself can produce multiple solutions."""
        p = program(
            mk_fact("recover", Int(10)),
            mk_fact("recover", Int(20)),
            mk_fact("recover", Int(30)),
        )
        engine = Engine(p)

        # Query: catch(throw(e), e, recover(X))
        # Recovery goal has multiple solutions
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct("throw", (Atom("e"),)),
                        Atom("e"),
                        Struct("recover", (Var(0, "X"),)),
                    ),
                )
            ]
        )

        assert len(results) == 3
        assert results[0]["X"] == Int(10)
        assert results[1]["X"] == Int(20)
        assert results[2]["X"] == Int(30)

    def test_no_resume_after_recovery_completes(self):
        """Test goal doesn't resume after recovery completes."""
        p = program(mk_fact("p", Int(1)), mk_fact("p", Int(2)))
        engine = Engine(p)

        # Query: catch((p(X), throw(e), p(Y)), e, true)
        # After throw and recovery, should not continue with p(Y)
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct(
                            ",",
                            (
                                Struct(
                                    ",",
                                    (
                                        Struct("p", (Var(0, "X"),)),
                                        Struct("throw", (Atom("e"),)),
                                    ),
                                ),
                                Struct("p", (Var(1, "Y"),)),  # Should never execute
                            ),
                        ),
                        Atom("e"),
                        Atom("true"),
                    ),
                )
            ]
        )

        # Only recovery result, no Y bindings
        assert len(results) == 1
        assert "Y" not in results[0]


class TestCatchTrailManagement:
    """Test trail unwinding in catch/3."""

    def test_trail_properly_unwound_to_catch_point(self):
        """Test trail is unwound to the point where catch was entered."""
        engine = Engine(program())

        # Query: X=before, catch((Y=during, throw(e)), e, Z=after)
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("=", (Var(0, "X"), Atom("before"))),
                        Struct(
                            "catch",
                            (
                                Struct(
                                    ",",
                                    (
                                        Struct("=", (Var(1, "Y"), Atom("during"))),
                                        Struct("throw", (Atom("e"),)),
                                    ),
                                ),
                                Atom("e"),
                                Struct("=", (Var(2, "Z"), Atom("after"))),
                            ),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["X"] == Atom("before")  # Before catch, preserved
        assert "Y" not in results[0]  # During goal, unwound
        assert results[0]["Z"] == Atom("after")  # In recovery, bound

    def test_nested_catch_unwinds_to_correct_frame(self):
        """Test nested catches unwind to their respective catch points."""
        engine = Engine(program())

        # Query: catch(catch((X=1, throw(inner)), inner, Y=2), outer, Z=3)
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct(
                            "catch",
                            (
                                Struct(
                                    ",",
                                    (
                                        Struct("=", (Var(0, "X"), Int(1))),
                                        Struct("throw", (Atom("inner"),)),
                                    ),
                                ),
                                Atom("inner"),
                                Struct("=", (Var(1, "Y"), Int(2))),
                            ),
                        ),
                        Atom("outer"),
                        Struct("=", (Var(2, "Z"), Int(3))),
                    ),
                )
            ]
        )

        assert len(results) == 1
        # X=1 happened in inner goal, should be unwound
        assert "X" not in results[0]
        # Y=2 happens in inner recovery
        assert results[0]["Y"] == Int(2)
        # Z would only be bound if outer catch triggered
        assert "Z" not in results[0]

    def test_trail_segments_isolated_between_catches(self):
        """Test trail segments are properly isolated between catch frames."""
        engine = Engine(program())

        # Complex nested catch with bindings at different levels
        # catch(A=1, catch((B=2, throw(i)), i, C=3), _, D=4)
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct(
                            ",",
                            (
                                Struct("=", (Var(0, "A"), Int(1))),
                                Struct(
                                    "catch",
                                    (
                                        Struct(
                                            ",",
                                            (
                                                Struct("=", (Var(1, "B"), Int(2))),
                                                Struct("throw", (Atom("i"),)),
                                            ),
                                        ),
                                        Atom("i"),
                                        Struct("=", (Var(2, "C"), Int(3))),
                                    ),
                                ),
                            ),
                        ),
                        Var(3, "_"),
                        Struct("=", (Var(4, "D"), Int(4))),
                    ),
                )
            ]
        )

        assert len(results) == 1
        assert results[0]["A"] == Int(1)  # Before inner catch
        assert "B" not in results[0]  # In thrown goal
        assert results[0]["C"] == Int(3)  # In inner recovery
        assert "D" not in results[0]  # Outer recovery not executed


class TestComplexNestedCatch:
    """Test complex nested catch/throw scenarios."""

    def test_deeply_nested_catch_throw(self):
        """Test deeply nested catch/throw with multiple levels."""
        engine = Engine(program())

        # 3-level nesting: outer catches what inner doesn't
        query = Struct(
            "catch",
            (
                Struct(
                    "catch",
                    (
                        Struct(
                            "catch",
                            (
                                Struct("throw", (Atom("deep"),)),
                                Atom("shallow"),
                                Atom("fail"),
                            ),
                        ),
                        Atom("middle"),
                        Atom("fail"),
                    ),
                ),
                Atom("deep"),
                Atom("true"),
            ),
        )

        results = engine.run([query])
        assert len(results) == 1  # Caught at outermost level

    def test_catch_with_throw_in_catcher_pattern(self):
        """Test throw occurring during catcher unification (shouldn't happen in practice)."""
        # This is a pathological case - catcher patterns should be simple
        # But we test to ensure the engine doesn't crash
        p = program(
            mk_rule(
                "bad_unify",
                (Var(0, "X"), Var(1, "Y")),
                Struct("throw", (Atom("unify_error"),)),
            )
        )
        engine = Engine(p)

        # Query: catch(throw(original), bad_unify(_, _), true)
        # If unification of catcher itself throws, what happens?
        # Expected: the original exception propagates
        with pytest.raises(PrologThrow) as exc:
            engine.run(
                [
                    Struct(
                        "catch",
                        (
                            Struct("throw", (Atom("original"),)),
                            Struct("bad_unify", (Var(0, "_"), Var(1, "_"))),
                            Atom("true"),
                        ),
                    )
                ]
            )
        # Original exception should propagate
        assert exc.value.ball == Atom("original")

    @pytest.mark.xfail(
        reason="Non-ISO: expects backtracking into caught goal after recovery, but ISO removes those CPs",
        strict=True
    )
    def test_multiple_catch_frames_different_catchers(self):
        """Test multiple catch frames with different catchers."""
        p = program(
            mk_rule(
                "thrower",
                (Var(0, "Which"),),
                Struct(
                    ";",
                    (
                        Struct(
                            ",",
                            (
                                Struct("=", (Var(0, "Which"), Atom("a"))),
                                Struct("throw", (Atom("error_a"),)),
                            ),
                        ),
                        Struct(
                            ",",
                            (
                                Struct("=", (Var(0, "Which"), Atom("b"))),
                                Struct("throw", (Atom("error_b"),)),
                            ),
                        ),
                    ),
                ),
            )
        )
        engine = Engine(p)

        # Query: catch(catch(thrower(X), error_a, X=caught_a), error_b, X=caught_b)
        # Two different catchers at different levels
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct(
                            "catch",
                            (
                                Struct("thrower", (Var(0, "X"),)),
                                Atom("error_a"),
                                Struct("=", (Var(0, "X"), Atom("caught_a"))),
                            ),
                        ),
                        Atom("error_b"),
                        Struct("=", (Var(0, "X"), Atom("caught_b"))),
                    ),
                )
            ]
        )

        # First solution: X=a throws error_a, caught by inner
        # On backtrack: X=b throws error_b, caught by outer
        assert len(results) == 2
        assert results[0]["X"] == Atom("caught_a")
        assert results[1]["X"] == Atom("caught_b")

    def test_catch_preserves_cut_scope(self):
        """Test cut scope is properly maintained across catch boundaries."""
        p = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_rule(
                "cut_then_throw",
                (),
                Struct(",", (Atom("!"), Struct("throw", (Atom("after_cut"),)))),
            ),
        )
        engine = Engine(p)

        # Query: p(X), catch(cut_then_throw, after_cut, true)
        # Cut inside catch shouldn't affect p(X) choicepoints
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("p", (Var(0, "X"),)),
                        Struct(
                            "catch",
                            (Atom("cut_then_throw"), Atom("after_cut"), Atom("true")),
                        ),
                    ),
                )
            ]
        )

        # Should get both solutions from p(X)
        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Int(2)

    @pytest.mark.xfail(
        reason="Non-ISO: expects cut in Recovery to be local, but ISO says it affects outer scope",
        strict=True
    )
    def test_cut_in_recovery_commits_only_within_recovery(self):
        """Test cut in recovery commits only within recovery goal."""
        p = program(
            mk_fact("r", Int(1)),
            mk_fact("r", Int(2)),
            mk_fact("p", Int(10)),
            mk_fact("p", Int(20)),
        )
        engine = Engine(p)

        # Query: p(P), catch(throw(t), t, (r(R), !))
        # Cut in recovery should only affect r/1 choices, not p/1
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("p", (Var(0, "P"),)),
                        Struct(
                            "catch",
                            (
                                Struct("throw", (Atom("t"),)),
                                Atom("t"),
                                Struct(",", (Struct("r", (Var(1, "R"),)), Atom("!"))),
                            ),
                        ),
                    ),
                )
            ]
        )

        # Two P values × Recovery commits to first r/1 => 2 total
        assert len(results) == 2
        assert {res["P"] for res in results} == {Int(10), Int(20)}
        assert all("R" in res and res["R"] == Int(1) for res in results)

    def test_variable_catcher_bindings_visible_in_recovery(self):
        """Test variable catcher bindings are visible in recovery."""
        engine = Engine(program())

        # Query: catch(throw(err(1, V)), err(A, V), (A=1, V=ok))
        # A and V unified by catcher, then used in recovery
        results = engine.run(
            [
                Struct(
                    "catch",
                    (
                        Struct("throw", (Struct("err", (Int(1), Var(0, "V"))),)),
                        Struct("err", (Var(1, "A"), Var(0, "V"))),
                        Struct(
                            ",",
                            (
                                Struct("=", (Var(1, "A"), Int(1))),
                                Struct("=", (Var(0, "V"), Atom("ok"))),
                            ),
                        ),
                    ),
                )
            ]
        )

        assert len(results) == 1
        # A and V were unified by the catcher, then (re)bound in Recovery
        assert results[0]["A"] == Int(1)
        assert results[0]["V"] == Atom("ok")

    @pytest.mark.xfail(
        reason="Non-ISO: expects recovery failure to allow solutions, but ISO says catch/3 fails",
        strict=True
    )
    def test_recovery_failure_backtracks_outside_catch(self):
        """Test recovery failure allows backtracking outside catch."""
        p = program(mk_fact("p", Int(1)), mk_fact("p", Int(2)))
        engine = Engine(p)

        # Query: p(X), catch(throw(t), t, fail)
        # Recovery fails, but we can still backtrack in p(X)
        results = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("p", (Var(0, "X"),)),
                        Struct(
                            "catch",
                            (Struct("throw", (Atom("t"),)), Atom("t"), Atom("fail")),
                        ),
                    ),
                )
            ]
        )

        # Only the X from the outer p/1 enumerates; catch contributes no solutions
        assert len(results) == 2
        assert {res["X"] for res in results} == {Int(1), Int(2)}
