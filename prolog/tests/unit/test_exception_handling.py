"""
Test exception handling (catch/throw) to cover uncovered paths.

Targets lines 463-522 in engine.py which handle exception catching.
"""

import pytest

from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.ast.terms import Atom, Struct, Int
from prolog.engine.engine import Engine
from prolog.debug.sinks import CollectorSink


class TestExceptionHandling:
    """Test catch/throw exception handling.

    Note: Some tests are marked as expected failures (xfail) because
    catch/throw is not fully implemented yet. These tests document
    expected behavior for future implementation.
    """

    def test_basic_catch_throw(self):
        """Test basic catch/throw mechanism."""
        # Create a program with catch/throw
        clauses = parser.parse_program("""
            test(X) :- catch(may_throw(X), error(E), handle_error(E)).

            may_throw(1) :- throw(error(bad_value)).
            may_throw(2) :- true.

            handle_error(bad_value) :- true.
        """)
        engine = Engine(Program(tuple(clauses)))

        # Query that throws and catches
        goals = parser.parse_query("?- test(1).")
        solutions = list(engine.run(goals))

        # Should succeed by catching the exception
        assert len(solutions) == 1

        # Query that doesn't throw
        goals = parser.parse_query("?- test(2).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1

    @pytest.mark.xfail(reason="Catch with streaming cursors not fully implemented")
    def test_catch_with_streaming(self):
        """Test catch/throw with streaming enabled."""
        # Create a program with catch/throw
        clauses = parser.parse_program("""
            test(X, Y) :-
                catch(
                    (member(X, [1,2,3]), check(X), '='(Y, ok)),
                    error(E),
                    '='(Y, caught(E))
                ).

            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).

            check(2) :- throw(error(bad)).
            check(_).
        """)
        engine = Engine(Program(tuple(clauses)), use_streaming=True, use_indexing=True)

        # Query - should catch on X=2
        goals = parser.parse_query("?- test(X, Y).")
        solutions = list(engine.run(goals))

        # Should get 3 solutions: ok for 1, caught for 2, ok for 3
        assert len(solutions) == 3
        assert solutions[0]["Y"] == Atom("ok")
        assert solutions[0]["X"] == Int(1)

        # Second solution catches the exception
        assert isinstance(solutions[1]["Y"], Struct)
        assert solutions[1]["Y"].functor == "caught"
        assert solutions[1]["X"] == Int(2)

        assert solutions[2]["Y"] == Atom("ok")
        assert solutions[2]["X"] == Int(3)

    @pytest.mark.xfail(reason="Catch unification failure case not fully handled")
    def test_catch_unification_failure(self):
        """Test catch when catcher doesn't unify with thrown ball."""
        clauses = parser.parse_program("""
            test1 :- catch(throw(error(type1)), error(type2), true).
            test2 :- catch(throw(error(type1)), error(type1), true).
        """)
        engine = Engine(Program(tuple(clauses)))

        # Query where catcher doesn't match - should fail
        goals = parser.parse_query("?- test1.")
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # No catch matches

        # Query where catcher matches
        goals = parser.parse_query("?- test2.")
        solutions = list(engine.run(goals))
        assert len(solutions) == 1  # Catch matches

    def test_nested_catch(self):
        """Test nested catch handlers."""
        clauses = parser.parse_program("""
            outer :-
                catch(
                    inner,
                    error(outer_error),
                    true
                ).

            inner :-
                catch(
                    may_throw,
                    error(inner_error),
                    handle_inner
                ).

            may_throw :- throw(error(inner_error)).
            handle_inner :- true.
        """)
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- outer.")
        solutions = list(engine.run(goals))

        # Inner catch should handle it
        assert len(solutions) == 1

    @pytest.mark.xfail(reason="Catch with cut interaction has edge cases")
    def test_catch_with_cut(self):
        """Test catch interaction with cut."""
        clauses = parser.parse_program("""
            test(X) :-
                catch(
                    (choice(X), !, check(X)),
                    error(_),
                    '='(X, caught)
                ).

            choice(1).
            choice(2).
            choice(3).

            check(1) :- throw(error(bad)).
            check(X) :- '>'(X, 1).
        """)
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # Cut prevents backtracking to choice(2) and choice(3)
        # So we only get the caught solution for choice(1)
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("caught")

    @pytest.mark.xfail(reason="State restoration in catch has remaining issues")
    def test_catch_restores_state(self):
        """Test that catch properly restores engine state."""
        clauses = parser.parse_program("""
            test(Result) :-
                '='(X, initial),
                catch(
                    ('='(X, modified), throw(error(test))),
                    error(_),
                    '='(Result, X)
                ).
        """)
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(R).")
        solutions = list(engine.run(goals))

        # After catch, X should still be bound to initial (state restored)
        assert len(solutions) == 1
        # The binding of X=modified should be undone
        assert solutions[0]["R"] == Atom("initial")

    def test_catch_with_tracer(self):
        """Test catch emits internal events when tracing."""
        clauses = parser.parse_program("""
            test :- catch(throw(error(msg)), error(E), handle(E)).
            handle(msg) :- true.
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use collector to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        goals = parser.parse_query("?- test.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1

        # Check for catch_switch internal event
        from prolog.debug.tracer import InternalEvent
        internal_events = [e for e in collector.events if isinstance(e, InternalEvent)]

        catch_events = [e for e in internal_events if e.kind == "catch_switch"]
        assert len(catch_events) > 0, "Should emit catch_switch event"

        # Check event details - these keys should only be present for catch_switch events
        for event in catch_events:
            assert "exception" in event.details, "catch_switch should have exception key"
            assert "handler" in event.details, "catch_switch should have handler key"
            # Ensure no other unexpected keys
            assert set(event.details.keys()) == {"exception", "handler"}

    def test_throw_without_catch(self):
        """Test throw without any catch handler raises Python exception."""
        from prolog.engine.errors import PrologThrow

        clauses = parser.parse_program("""
            test :- throw(error(uncaught)).
        """)
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test.")

        # Should raise PrologThrow when no handler found
        with pytest.raises(PrologThrow) as exc_info:
            solutions = list(engine.run(goals))

        # Check the exception contains the thrown term
        assert "uncaught" in str(exc_info.value)

    def test_catch_scope_boundary(self):
        """Test catch scope boundaries are respected."""
        clauses = parser.parse_program("""
            outer :- catch(middle, error(X), handle_outer(X)).
            middle :- inner, after_inner.
            inner :- throw(error(from_inner)).
            after_inner :- true.
            handle_outer(from_inner) :- true.
        """)
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- outer.")
        solutions = list(engine.run(goals))

        # Should catch and handle
        assert len(solutions) == 1

    def test_catch_with_variable_catcher(self):
        """Test catch with variable as catcher (catches anything)."""
        clauses = parser.parse_program("""
            test(X) :- catch(may_throw, E, '='(X, caught(E))).
            may_throw :- throw(anything(123)).
        """)
        engine = Engine(Program(tuple(clauses)))

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        result = solutions[0]["X"]
        assert isinstance(result, Struct)
        assert result.functor == "caught"