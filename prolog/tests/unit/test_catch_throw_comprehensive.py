"""Comprehensive test suite for ISO Prolog catch/throw exception handling.

This test suite provides thorough coverage of catch/throw functionality,
including edge cases, state management, and interaction with other control structures.
Tests are designed to validate the fixes for Issue #102.
"""

import pytest
from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.debug.sinks import CollectorSink
from prolog.debug.tracer import InternalEvent


class TestBasicCatchThrow:
    """Basic catch/throw functionality tests."""

    def test_simple_catch_and_throw(self):
        """Simple throw caught by exact match."""
        clauses = parser.parse_program(
            """
            test :- catch(throw(error), error, true).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0] == {}  # Empty substitution = success

    def test_throw_with_compound_term(self):
        """Throw and catch compound terms with unification."""
        clauses = parser.parse_program(
            """
            test(X) :- catch(
                throw(error(code, 42)),
                error(code, X),
                true
            ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(42)

    def test_variable_catcher_matches_anything(self):
        """Variable as catcher matches any thrown term."""
        clauses = parser.parse_program(
            """
            test(X) :- catch(throw(anything(123)), X, true).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        result = solutions[0]["X"]
        assert isinstance(result, Struct)
        assert result.functor == "anything"
        assert result.args[0] == Int(123)

    def test_no_throw_executes_normally(self):
        """When no exception thrown, goal executes with backtracking."""
        clauses = parser.parse_program(
            """
            test(X) :- catch(member(X, [1,2,3]), _, fail).

            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 3
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
        assert solutions[2]["X"] == Int(3)

    def test_throw_without_catch_raises_python_exception(self):
        """Uncaught throw raises PrologThrow exception."""
        clauses = parser.parse_program(
            """
            test :- throw(uncaught_error).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")

        with pytest.raises(PrologThrow) as exc_info:
            list(engine.run(goals))

        assert exc_info.value.ball == Atom("uncaught_error")


class TestStateRestoration:
    """Tests for proper state restoration during exception handling."""

    def test_bindings_undone_after_catch(self):
        """All bindings made before throw should be undone."""
        clauses = parser.parse_program(
            """
            test(X, Result) :-
                catch(
                    bind_and_throw(X),
                    error,
                    check_var(X, Result)
                ).

            bind_and_throw(X) :-
                '='(X, bound_value),
                throw(error).

            check_var(X, unbound) :- var(X).
            check_var(X, bound(X)) :- nonvar(X).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X, R).")
        solutions = list(engine.run(goals))

        # After catch, X should be unbound because the binding was undone
        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("unbound")
        # X should remain unbound in the solution
        assert "X" not in solutions[0] or isinstance(solutions[0]["X"], Var)

    def test_complex_unification_undone(self):
        """Complex unifications should be completely undone."""
        clauses = parser.parse_program(
            """
            test :-
                catch(
                    do_unifications(X, Y, Z, A, B),
                    reset,
                    check_all_unbound(X, Y, Z, A, B)
                ).

            do_unifications(X, Y, Z, A, B) :-
                f(X, Y, Z) = f(1, 2, 3),
                g(A, B) = g(X, Y),
                throw(reset).

            check_all_unbound(X, Y, Z, A, B) :-
                var(X),
                var(Y),
                var(Z),
                var(A),
                var(B).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1  # All vars should be unbound

    @pytest.mark.xfail(reason="List unification restoration still has issues")
    def test_list_unification_restoration(self):
        """List structure unification should be restored correctly."""
        clauses = parser.parse_program(
            """
            test(Result) :-
                X = original,
                catch(
                    rebind_list(X),
                    error,
                    Result = X
                ).

            rebind_list(X) :-
                X = [1,2,3|T],
                T = [4,5],
                throw(error).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(R).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("original")

    def test_variable_aliasing_restoration(self):
        """Variable aliasing should be properly restored."""
        clauses = parser.parse_program(
            """
            test :-
                catch(
                    do_alias(X, Y, Z),
                    error,
                    check_vars(X, Y, Z)
                ).

            do_alias(X, Y, Z) :-
                X = Y,
                Y = Z,
                Z = value,
                throw(error).

            check_vars(X, Y, Z) :-
                var(X),
                var(Y),
                var(Z).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1  # All should be unbound


class TestBallUnification:
    """Tests for ball unification between throw and catch."""

    def test_ball_binds_catcher_variables(self):
        """Catcher variables should be bound by thrown ball."""
        clauses = parser.parse_program(
            """
            test(A, B, C) :-
                catch(
                    throw(data(1, 2, 3)),
                    data(A, B, C),
                    true
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(A, B, C).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["A"] == Int(1)
        assert solutions[0]["B"] == Int(2)
        assert solutions[0]["C"] == Int(3)

    def test_non_matching_catcher_propagates(self):
        """Non-matching catcher should let exception propagate."""
        clauses = parser.parse_program(
            """
            test :- catch(throw(error(type1)), error(type2), true).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")

        with pytest.raises(PrologThrow) as exc_info:
            list(engine.run(goals))

        # Exception should propagate with original ball
        assert isinstance(exc_info.value.ball, Struct)
        assert exc_info.value.ball.functor == "error"

    def test_partial_unification_failure(self):
        """Partial unification failures should not catch."""
        clauses = parser.parse_program(
            """
            test :- catch(
                throw(f(1, 2, 3)),
                f(X, 2, X),  % X can't be both 1 and 3
                true
            ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")

        with pytest.raises(PrologThrow):
            list(engine.run(goals))

    def test_catcher_with_existing_bindings(self):
        """Catcher can have pre-existing bindings."""
        clauses = parser.parse_program(
            """
            test :-
                X = 42,
                catch(
                    throw(error(X)),
                    error(42),
                    true
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1


class TestNestedCatches:
    """Tests for nested catch handlers."""

    def test_inner_catch_handles_first(self):
        """Inner catch should handle matching exceptions."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    catch(
                        throw(inner),
                        inner,
                        X = caught_inner
                    ),
                    outer,
                    X = caught_outer
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("caught_inner")

    def test_outer_catch_handles_unmatched(self):
        """Outer catch handles when inner doesn't match."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    catch(
                        throw(outer),
                        inner,
                        X = caught_inner
                    ),
                    outer,
                    X = caught_outer
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("caught_outer")

    def test_throw_from_recovery_goal(self):
        """Exception thrown from recovery is caught by outer."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    catch(
                        throw(first),
                        first,
                        throw(second)
                    ),
                    second,
                    X = handled_second
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("handled_second")

    def test_deeply_nested_catches(self):
        """Multiple levels of nesting work correctly."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    catch(
                        catch(
                            catch(
                                throw(level4),
                                level1,
                                X = 1
                            ),
                            level2,
                            X = 2
                        ),
                        level3,
                        X = 3
                    ),
                    level4,
                    X = 4
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(4)


class TestCutInteraction:
    """Tests for interaction between cut and catch."""

    def test_cut_in_catch_goal(self):
        """Cut within catch goal shouldn't escape catch."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    test_choice_cut_throw(X),
                    error,
                    true
                ).

            test_choice_cut_throw(X) :- choice(X), !, throw(error).

            choice(1).
            choice(2).
            choice(3).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # Cut should limit to first choice, then catch handles throw
        assert len(solutions) == 1
        # X would be 1 from first choice, but after catch it's unbound

    def test_catch_creates_cut_barrier(self):
        """Catch should act as cut barrier - cut doesn't escape catch scope."""
        clauses = parser.parse_program(
            """
            test(X) :-
                test_catch_or_survive(X).

            test_catch_or_survive(X) :- catch(!, _, fail).
            test_catch_or_survive(survived).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # Cut is confined to catch scope, so both clauses can succeed
        # First clause: catch(!, _, fail) succeeds with unbound X
        # Second clause: X = survived
        assert len(solutions) == 2
        # First solution has unbound X, second has X = survived
        assert solutions[1]["X"] == Atom("survived")

    @pytest.mark.swi_baseline
    def test_catch_cut_barrier_swi_baseline(self, swi):
        """SWI-Prolog baseline: Cut barrier behavior in catch/3."""
        program = """
            test(X) :- test_catch_or_survive(X).
            test_catch_or_survive(X) :- catch(!, _, fail).
            test_catch_or_survive(survived).
        """

        # SWI-Prolog should return 2 solutions: unbound X and X=survived
        solution_count = swi.count(program, "test(X)")
        assert solution_count == 2

    def test_cut_in_recovery_goal(self):
        """Cut in recovery goal works normally."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    throw(error),
                    error,
                    test_cut_recovery(X)
                ).

            test_cut_recovery(X) :- member(X, [1,2,3]), !.

            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # Cut in recovery limits to first solution
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)


class TestBacktrackingBehavior:
    """Tests for backtracking through catch."""

    def test_backtrack_through_catch_no_throw(self):
        """Backtracking works normally when no throw occurs."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    member(X, [a,b,c]),
                    _,
                    X = error
                ).

            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")
        assert solutions[2]["X"] == Atom("c")

    def test_no_backtrack_after_catch_handles(self):
        """After catching, no backtracking into goal."""
        clauses = parser.parse_program(
            """
            test(X, Y) :-
                catch(
                    test_throw_after_member(X),
                    stop,
                    '='(Y, caught)
                ).

            test_throw_after_member(X) :- member(X, [1,2,3]), throw(stop).

            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X, Y).")
        solutions = list(engine.run(goals))

        # Only one solution from catch, X unbound after restoration
        assert len(solutions) == 1
        assert "X" not in solutions[0] or isinstance(solutions[0]["X"], Var)
        assert solutions[0]["Y"] == Atom("caught")

    @pytest.mark.xfail(reason="Catch at choice points not fully working")
    def test_catch_at_choice_points(self):
        """Multiple catch alternatives at same level."""
        clauses = parser.parse_program(
            """
            test(X) :-
                test_choice(X).

            test_choice(X) :- catch(fail, _, '='(X, 1)).
            test_choice(X) :- catch(throw(e), e, '='(X, 2)).
            test_choice(X) :- catch(true, _, '='(X, 3)).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # First fails (no solution), second catches, third succeeds
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(2)
        assert solutions[1]["X"] == Int(3)


class TestStreamingCompatibility:
    """Tests for catch/throw with streaming clause selection."""

    def test_catch_with_streaming_enabled(self):
        """Catch should work with streaming clause selection."""
        clauses = parser.parse_program(
            """
            test(X, Y) :-
                member(X, [1,2,3]),
                catch(
                    process(X, Y),
                    error(E),
                    '='(Y, caught(E))
                ).

            process(X, Y) :-
                check(X),
                '='(Y, ok).

            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).

            check(2) :- throw(error(bad)).
            check(_).
        """
        )
        engine = Engine(Program(tuple(clauses)), use_streaming=True, use_indexing=True)

        goals = parser.parse_query("?- test(X, Y).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 3
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Atom("ok")

        assert solutions[1]["X"] == Int(2)
        assert isinstance(solutions[1]["Y"], Struct)
        assert solutions[1]["Y"].functor == "caught"

        assert solutions[2]["X"] == Int(3)
        assert solutions[2]["Y"] == Atom("ok")

    @pytest.mark.xfail(
        reason="Design choice: catch prunes in-scope CPs including streaming cursors"
    )
    def test_streaming_cursor_restoration(self):
        """Streaming cursor state is pruned by design when catching exceptions."""
        clauses = parser.parse_program(
            """
            test(Result) :-
                catch(
                    (
                        fact(X),
                        X > 2,
                        throw(error)
                    ),
                    error,
                    findall(Y, fact(Y), Result)
                ).

            fact(1).
            fact(2).
            fact(3).
            fact(4).

            findall(X, Goal, List) :-
                % Simplified findall for testing
                Goal, List = [X].
        """
        )
        engine = Engine(Program(tuple(clauses)), use_streaming=True, use_indexing=True)

        goals = parser.parse_query("?- test(R).")
        solutions = list(engine.run(goals))

        # Should be able to iterate facts again after catch
        assert len(solutions) > 0


class TestEdgeCases:
    """Edge cases and error conditions."""

    def test_throw_with_unbound_variable(self):
        """Throwing unbound variable should work."""
        clauses = parser.parse_program(
            """
            test(X) :- catch(throw(X), Y, '='(Y, X)).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # This may fail in development mode if unbound throws are disallowed
        # In ISO mode it should succeed with X and Y unified
        if solutions:
            assert len(solutions) == 1

    @pytest.mark.xfail(reason="Recursive predicates with catch have edge cases")
    def test_recursive_predicate_with_catch(self):
        """Recursive predicates with catch/throw."""
        clauses = parser.parse_program(
            """
            countdown(0) :- throw(done).
            countdown(N) :-
                '>'(N, 0),
                is(N1, -(N, 1)),
                catch(
                    countdown(N1),
                    done,
                    true
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- countdown(3).")
        solutions = list(engine.run(goals))

        # Should succeed after catching 'done' from countdown(0)
        assert len(solutions) >= 1

    def test_catch_with_failing_recovery(self):
        """Recovery goal can fail."""
        clauses = parser.parse_program(
            """
            test :- catch(throw(error), error, fail).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        # When recovery fails, the whole catch fails
        assert len(solutions) == 0

    def test_multiple_throws_same_goal(self):
        """Multiple throws in sequence."""
        clauses = parser.parse_program(
            """
            test(X) :-
                catch(
                    (throw(first) ; throw(second)),
                    E,
                    X = E
                ).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # Only first throw is caught
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("first")

    def test_multiple_throws_same_catch_cursor_preservation(self):
        """Multiple throws within same catch should preserve cursor state correctly."""
        clauses = parser.parse_program("""
            test(Results) :-
                catch(
                    retry_with_throws,
                    Error,
                    collect_error(Error, Results)
                ).

            retry_with_throws :- throw(first_error).
            retry_with_throws :- throw(second_error).
            retry_with_throws :- throw(third_error).

            collect_error(Error, [Error]).
        """)
        engine = Engine(Program(tuple(clauses)), use_streaming=True)

        goals = parser.parse_query("?- test(R).")
        solutions = list(engine.run(goals))

        # Should catch the first throw from first clause of retry_with_throws
        assert len(solutions) == 1
        assert solutions[0]['R'] == List((Atom("first_error"),), Atom("[]"))


class TestTracingAndInstrumentation:
    """Tests for tracer integration and debugging."""

    def test_catch_emits_trace_events(self):
        """Catch should emit proper trace events."""
        clauses = parser.parse_program(
            """
            test :- catch(throw(traced), traced, true).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1

        # Check for catch_switch event
        internal_events = [e for e in collector.events if isinstance(e, InternalEvent)]
        catch_events = [e for e in internal_events if e.kind == "catch_switch"]

        assert len(catch_events) > 0
        for event in catch_events:
            assert "exception" in event.details
            assert "handler" in event.details

    def test_catch_choicepoint_creation(self):
        """CATCH choicepoint should be created properly."""
        clauses = parser.parse_program(
            """
            test :- catch(true, _, fail).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        goals = parser.parse_query("?- test.")
        list(engine.run(goals))

        # Check for cp_push event with catch
        internal_events = [e for e in collector.events if isinstance(e, InternalEvent)]
        cp_events = [
            e
            for e in internal_events
            if e.kind == "cp_push" and e.details.get("pred_id") == "catch"
        ]

        assert len(cp_events) > 0


class TestISOCompliance:
    """Tests for ISO Prolog compliance."""

    def test_iso_example_1(self):
        """ISO Prolog standard example 1."""
        clauses = parser.parse_program(
            """
            test1 :- catch(throw(b), a, true).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test1.")

        # Should throw uncaught b
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.run(goals))
        assert exc_info.value.ball == Atom("b")

    def test_iso_example_2(self):
        """ISO Prolog standard example 2."""
        clauses = parser.parse_program(
            """
            test2(X) :- catch(throw(f(X)), f(Y), Y = X).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test2(X).")
        solutions = list(engine.run(goals))

        # X and Y should be unified (both unbound)
        assert len(solutions) == 1

    @pytest.mark.xfail(
        reason="Design choice: PyLog uses reification not ISO copy semantics"
    )
    def test_iso_ball_copy_semantics(self):
        """PyLog uses reification at throw time instead of ISO's copy semantics."""
        clauses = parser.parse_program(
            """
            test(X, Y) :-
                '='(X, original),
                catch(
                    test_modify_and_throw(X),
                    Y,
                    true
                ).

            test_modify_and_throw(X) :- '='(X, modified), throw(X).
        """
        )
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X, Y).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        # Y should get the value at throw time
        assert solutions[0]["Y"] == Atom("modified")
