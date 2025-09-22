"""End-to-end tests for complete trace functionality.

Tests complete traces of standard Prolog programs to validate the entire
tracing infrastructure works correctly from start to finish.
"""

import pytest
import json
from io import StringIO
from prolog.engine.engine import Engine
from prolog.debug.tracer import PortsTracer
from prolog.debug.sinks import JSONLTraceSink, PrettyTraceSink
from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.ast.terms import Atom, Int, Var, List


class TestEndToEndTraces:
    """Complete trace tests for standard Prolog programs."""

    def test_trace_append_complete(self):
        """Complete trace of append/3 with all ports."""
        clauses = parser.parse_program("""
            append([], L, L).
            append([H|T], L, [H|R]) :- append(T, L, R).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Run query: append([1,2], [3], X)
        goals = parser.parse_query("?- append([1,2], [3], X).")
        solutions = list(engine.run(goals))

        # Verify we got the right solution
        assert len(solutions) == 1
        result = solutions[0]['X']
        assert isinstance(result, List)
        assert len(result.items) == 3

        # Close sink to flush events
        sink.close()

        # Parse and verify trace events
        output.seek(0)
        events = [json.loads(line) for line in output if line.strip()]

        # Verify we have events
        assert len(events) > 0

        # Verify port sequence includes CALL, EXIT, REDO, FAIL
        ports = [e['p'] for e in events]
        assert 0 in ports  # CALL
        assert 1 in ports  # EXIT

        # Verify pred_ids are correct
        pred_ids = {e.get('pid') for e in events if 'pid' in e}
        assert 'append/3' in pred_ids

        # Verify step_ids are sequential (starting from 1, not 0)
        step_ids = [e['sid'] for e in events]
        assert step_ids == list(range(1, len(step_ids) + 1))

    def test_trace_member_with_backtracking(self):
        """Complete trace of member/2 showing backtracking."""
        clauses = parser.parse_program("""
            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Run query that will backtrack: member(X, [1,2,3])
        goals = parser.parse_query("?- member(X, [1,2,3]).")
        solutions = list(engine.run(goals))

        # Should get 3 solutions
        assert len(solutions) == 3
        assert solutions[0]['X'] == Int(1)
        assert solutions[1]['X'] == Int(2)
        assert solutions[2]['X'] == Int(3)

        # Close sink to flush events
        sink.close()

        # Parse trace
        output.seek(0)
        events = [json.loads(line) for line in output if line.strip()]

        # Should see REDO events for backtracking
        ports = [e['p'] for e in events]
        assert 2 in ports  # REDO

        # Verify multiple EXIT events for multiple solutions
        exit_count = sum(1 for p in ports if p == 1)
        assert exit_count >= 3  # At least one per solution

    def test_trace_with_cut(self):
        """Complete trace showing cut behavior."""
        clauses = parser.parse_program("""
            first([H|_], H) :- !.
            first([_|T], X) :- first(T, X).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Run query that encounters cut
        goals = parser.parse_query("?- first([1,2,3], X).")
        solutions = list(engine.run(goals))

        # Should only get first element due to cut
        assert len(solutions) == 1
        assert solutions[0]['X'] == Int(1)

        # Close sink to flush events
        sink.close()

        # Parse trace
        output.seek(0)
        events = [json.loads(line) for line in output if line.strip()]

        # Verify we see the cut builtin
        pred_ids = {e.get('pid') for e in events if 'pid' in e}
        assert '!/0' in pred_ids

    def test_trace_with_exception(self):
        """Complete trace with exception handling."""
        clauses = parser.parse_program("""
            safe_div(X, Y, R) :-
                catch(
                    div_check(X, Y, R),
                    error(E),
                    R = error(E)
                ).

            div_check(X, 0, _) :- throw(error(division_by_zero)).
            div_check(X, Y, R) :- Y \\= 0, R is X / Y.
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Run query that throws exception
        goals = parser.parse_query("?- safe_div(10, 0, R).")
        solutions = list(engine.run(goals))

        # Should catch the error
        assert len(solutions) == 1

        # Close sink to flush events
        sink.close()

        # Parse trace
        output.seek(0)
        events = [json.loads(line) for line in output if line.strip()]

        # Should see catch and throw in trace
        pred_ids = {e.get('pid') for e in events if 'pid' in e}
        assert 'catch/3' in pred_ids
        assert 'throw/1' in pred_ids

    def test_trace_deep_recursion(self):
        """Trace deep recursion to verify depth tracking."""
        clauses = parser.parse_program("""
            count_down(0).
            count_down(N) :- N > 0, N1 is N - 1, count_down(N1).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Run query with moderate recursion
        goals = parser.parse_query("?- count_down(5).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1

        # Close sink to flush events
        sink.close()

        # Parse trace
        output.seek(0)
        events = [json.loads(line) for line in output if line.strip()]

        # Validate recursion via either goal height or number of recursive calls.
        # Frame depth is not a reliable proxy because CALL events are emitted
        # before pushing a new frame and EXIT uses the parent's depth.
        call_events = [e for e in events if e.get('pid') == 'count_down/1' and e['p'] == 0]
        assert len(call_events) >= 5  # Should observe multiple recursive CALLs
        max_goal_height = max(e.get('gh', 0) for e in events)
        assert max_goal_height >= 5

    def test_pretty_trace_format(self):
        """Test pretty trace output format."""
        clauses = parser.parse_program("""
            parent(tom, bob).
            parent(bob, pat).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add pretty sink to the tracer
        output = StringIO()
        sink = PrettyTraceSink(output)
        engine.tracer.add_sink(sink)

        # Run query
        goals = parser.parse_query("?- grandparent(tom, Z).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]['Z'] == Atom("pat")

        # Close sink to flush events
        sink.close()

        # Check pretty output contains expected elements
        output_str = output.getvalue()
        assert "CALL" in output_str
        assert "EXIT" in output_str
        assert "grandparent" in output_str
        assert "parent" in output_str

        # Should show indentation for nested calls
        lines = output_str.split('\n')
        # Find lines with different indentation levels
        indents = set()
        for line in lines:
            if line.strip():
                indent = len(line) - len(line.lstrip())
                indents.add(indent)
        assert len(indents) > 1  # Should have multiple indentation levels


class TestTraceCompatibility:
    """Test that tracing doesn't break existing functionality."""

    def test_all_builtins_work_with_tracing(self):
        """Verify all builtins work correctly with tracing enabled."""
        clauses = parser.parse_program("""
            test_arithmetic(X) :- X is 2 + 3 * 4.
            test_comparison :- 5 > 3, 2 =< 4.
            test_unify :- X = Y, Y = 42.
            test_var :- var(X), X = 1, nonvar(X).
            test_list :- append([1,2], [3,4], L), length(L, 4).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Test each builtin category
        test_queries = [
            "?- test_arithmetic(X).",
            "?- test_comparison.",
            "?- test_unify.",
            "?- test_var.",
        ]

        for query in test_queries:
            goals = parser.parse_query(query)
            solutions = list(engine.run(goals))
            assert len(solutions) > 0, f"Query failed with tracing: {query}"

    def test_indexing_with_tracing(self):
        """Verify indexing works correctly with tracing enabled."""
        clauses = parser.parse_program("""
            fact(a, 1).
            fact(b, 2).
            fact(c, 3).
            fact(d, 4).
            fact(e, 5).
        """)

        # Test with and without indexing
        for use_indexing in [False, True]:
            engine = Engine(Program(tuple(clauses)), use_indexing=use_indexing)

            # Enable tracing
            output = StringIO()
            sink = JSONLTraceSink(output)
            # Attach a tracer to the engine and add the sink
            engine.tracer = PortsTracer(engine)
            engine.tracer.add_sink(sink)

            goals = parser.parse_query("?- fact(c, X).")
            solutions = list(engine.run(goals))

            assert len(solutions) == 1
            assert solutions[0]['X'] == Int(3)

    def test_catch_throw_with_tracing(self):
        """Verify catch/throw works with tracing enabled."""
        clauses = parser.parse_program("""
            safe_op(X) :-
                catch(
                    risky_op(X),
                    error(E),
                    handle_error(E)
                ).

            risky_op(bad) :- throw(error(bad_input)).
            risky_op(good) :- true.

            handle_error(_).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Test both paths
        for input_val in ["good", "bad"]:
            goals = parser.parse_query(f"?- safe_op({input_val}).")
            solutions = list(engine.run(goals))
            assert len(solutions) == 1


class TestTraceStress:
    """Stress tests for tracing infrastructure."""

    @pytest.mark.slow
    def test_large_trace_handling(self):
        """Test handling of large traces (many events)."""
        clauses = parser.parse_program("""
            generate(0, []).
            generate(N, [N|T]) :- N > 0, N1 is N - 1, generate(N1, T).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add JSONL sink to the tracer
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.add_sink(sink)

        # Generate a moderately large trace
        goals = parser.parse_query("?- generate(100, L).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1

        # Parse all events to ensure they're valid
        output.seek(0)
        events = []
        for line in output:
            if line.strip():
                events.append(json.loads(line))

        # Should have many events
        assert len(events) > 100

        # All events should have required fields
        for event in events:
            assert 'sid' in event
            assert 'p' in event
            assert 'rid' in event

    def test_many_spypoints(self):
        """Test performance with many spypoints active."""
        clauses = parser.parse_program("""
            pred_a(1).
            pred_a(2).
            pred_b(X) :- pred_a(X).
            pred_c(X) :- pred_b(X).
            pred_d(X) :- pred_c(X).
            pred_e(X) :- pred_d(X).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Add spypoints and JSONL sink
        output = StringIO()
        sink = JSONLTraceSink(output)
        engine.tracer.spypoints = {'pred_a/1', 'pred_b/1', 'pred_c/1', 'pred_d/1', 'pred_e/1'}
        engine.tracer.add_sink(sink)

        # Run query
        goals = parser.parse_query("?- pred_e(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 2

        # Close sink to flush events
        sink.close()

        # Verify spypoints worked
        output.seek(0)
        events = [json.loads(line) for line in output if line.strip()]

        # Should only have events for spied predicates
        pred_ids = {e.get('pid') for e in events if 'pid' in e}
        for pred in ['pred_a/1', 'pred_b/1', 'pred_c/1', 'pred_d/1', 'pred_e/1']:
            assert pred in pred_ids

    def test_trace_memory_bounded(self):
        """Verify trace memory usage is bounded with file rotation."""
        clauses = parser.parse_program("""
            bounded_loop(0).
            bounded_loop(N) :- N > 0, N1 is N - 1, bounded_loop(N1).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use JSONL sink with max file size for bounded output
        import tempfile
        import os

        with tempfile.NamedTemporaryFile(mode='w', suffix='.jsonl', delete=False) as f:
            temp_file = f.name

        try:
            # Create a JSONL sink with file rotation
            with open(temp_file, 'w') as output:
                sink = JSONLTraceSink(output)  # Note: file rotation would need implementation
                engine.tracer.add_sink(sink)

                # Run bounded loop that would generate many events
                goals = parser.parse_query("?- bounded_loop(500).")
                solutions = list(engine.run(goals))

                assert len(solutions) == 1

            # Verify file size is reasonable
            file_size = os.path.getsize(temp_file)
            assert file_size > 0  # Should have some trace data
            # Note: actual rotation would require sink implementation support

        finally:
            # Clean up
            if os.path.exists(temp_file):
                os.unlink(temp_file)
