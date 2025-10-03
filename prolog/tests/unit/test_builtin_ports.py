"""
Test 4-port events for builtin predicates.

Tests that builtin predicates emit correct CALL/EXIT/FAIL ports in traces.
"""

from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.ast.terms import Int
from prolog.engine.engine import Engine
from prolog.debug.sinks import CollectorSink
from prolog.debug.tracer import TraceEvent


class TestBuiltinPorts:
    """Test 4-port events for builtin predicates."""

    def test_is_builtin_success_ports(self):
        """Test is/2 builtin emits CALL and EXIT on success."""
        engine = Engine(Program(()), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that succeeds
        goals = parser.parse_query("?- X is 2 + 3.")
        solutions = list(engine.run(goals))

        # Should succeed with X = 5
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(5)

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find is/2 events
        is_events = [(e.port, e.pred_id) for e in trace_events if e.pred_id == "is/2"]

        # Should have CALL followed by EXIT
        assert len(is_events) >= 2
        assert is_events[0] == ("call", "is/2")
        assert is_events[1] == ("exit", "is/2")

    def test_is_builtin_failure_ports(self):
        """Test is/2 builtin emits CALL and FAIL on failure."""
        engine = Engine(Program(()), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that fails (trying to unify 5 with 6)
        goals = parser.parse_query("?- 5 is 2 + 4.")
        solutions = list(engine.run(goals))

        # Should fail
        assert len(solutions) == 0

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find is/2 events
        is_events = [(e.port, e.pred_id) for e in trace_events if e.pred_id == "is/2"]

        # Should have CALL followed by FAIL
        assert len(is_events) >= 2
        assert is_events[0] == ("call", "is/2")
        assert is_events[1] == ("fail", "is/2")

    def test_comparison_builtin_ports(self):
        """Test comparison builtins emit correct ports."""
        engine = Engine(Program(()), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query with =< that succeeds
        goals = parser.parse_query("?- 3 =< 5.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1  # Should succeed

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find =</2 events
        comp_events = [(e.port, e.pred_id) for e in trace_events if e.pred_id == "=</2"]

        # Should have CALL followed by EXIT
        assert len(comp_events) >= 2
        assert comp_events[0] == ("call", "=</2")
        assert comp_events[1] == ("exit", "=</2")

    def test_var_nonvar_builtin_ports(self):
        """Test var/1 and nonvar/1 builtins emit correct ports."""
        engine = Engine(Program(()), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query with var/1 that succeeds
        goals = parser.parse_query("?- var(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1  # Should succeed

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find var/1 events
        var_events = [(e.port, e.pred_id) for e in trace_events if e.pred_id == "var/1"]

        # Should have CALL followed by EXIT
        assert len(var_events) >= 2
        assert var_events[0] == ("call", "var/1")
        assert var_events[1] == ("exit", "var/1")

        # Now test nonvar with failure
        collector.events.clear()
        goals = parser.parse_query("?- nonvar(Y).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 0  # Should fail (Y is unbound)

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find nonvar/1 events
        nonvar_events = [
            (e.port, e.pred_id) for e in trace_events if e.pred_id == "nonvar/1"
        ]

        # Should have CALL followed by FAIL
        assert len(nonvar_events) >= 2
        assert nonvar_events[0] == ("call", "nonvar/1")
        assert nonvar_events[1] == ("fail", "nonvar/1")

    def test_multiple_builtins_in_conjunction(self):
        """Test multiple builtins in a conjunction emit correct ports."""
        engine = Engine(Program(()), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query with multiple builtins
        goals = parser.parse_query("?- X is 2 + 3, Y is X * 2, Y > 5.")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1  # Should succeed
        assert solutions[0]["X"] == Int(5)
        assert solutions[0]["Y"] == Int(10)

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Check sequence of builtin calls
        builtin_events = [
            (e.port, e.pred_id) for e in trace_events if e.pred_id in ["is/2", ">/2"]
        ]

        # Expected sequence: is/2 CALL, EXIT, is/2 CALL, EXIT, >/2 CALL, EXIT
        expected = [
            ("call", "is/2"),
            ("exit", "is/2"),
            ("call", "is/2"),
            ("exit", "is/2"),
            ("call", ">/2"),
            ("exit", ">/2"),
        ]

        assert builtin_events == expected

    def test_builtin_frame_depth(self):
        """Test that builtins have correct frame_depth in traces."""
        # Create a program with a rule that calls builtins
        clauses = parser.parse_program(
            """
            test(X, Y) :- X is 5, Y is X + 1.
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query the rule
        goals = parser.parse_query("?- test(A, B).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["A"] == Int(5)
        assert solutions[0]["B"] == Int(6)

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find test/2 and is/2 events with depths
        events_with_depths = [
            (e.port, e.pred_id, e.frame_depth)
            for e in trace_events
            if e.pred_id in ["test/2", "is/2"]
        ]

        # test/2 should be at depth 0, builtins at depth 1
        test_events = [e for e in events_with_depths if e[1] == "test/2"]
        is_events = [e for e in events_with_depths if e[1] == "is/2"]

        # test/2 should be at depth 0
        assert all(depth == 0 for _, _, depth in test_events)

        # is/2 should be at depth 1 (called from within test/2)
        assert all(depth == 1 for _, _, depth in is_events)

    def test_zero_arity_port_sequence(self):
        """Test zero-arity predicates maintain correct 4-port parity."""
        # Create a program with zero-arity facts
        clauses = parser.parse_program(
            """
            z.
            z.
            other :- z, fail.
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query z/0 which has two facts
        goals = parser.parse_query("?- z.")
        solutions = list(engine.run(goals))

        # Should get two solutions (two facts)
        assert len(solutions) == 2

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find z/0 events
        z_events = [(e.port, e.pred_id) for e in trace_events if e.pred_id == "z/0"]

        # The exact sequence depends on implementation, but we should see:
        # - One CALL at the beginning
        # - Two EXITs (one for each fact)
        # - May or may not have REDO depending on how the engine handles multiple facts

        # Check minimum requirements for 4-port correctness
        assert z_events[0] == ("call", "z/0"), "Should start with CALL"

        # Count exits - should be 2 (one for each fact)
        exit_count = sum(1 for port, _ in z_events if port == "exit")
        assert (
            exit_count == 2
        ), f"Should have 2 EXIT events (one per fact), got {exit_count}"

        # If there's a REDO, it should be between the two EXITs
        if ("redo", "z/0") in z_events:
            redo_idx = z_events.index(("redo", "z/0"))
            first_exit = z_events.index(("exit", "z/0"))
            assert redo_idx > first_exit, "REDO should come after first EXIT"

        # Test z/0 in a failing context
        collector.events.clear()
        goals = parser.parse_query("?- other.")
        solutions = list(engine.run(goals))

        # Should fail (because of fail/0)
        assert len(solutions) == 0

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Find other/0 and z/0 events
        other_events = [
            (e.port, e.pred_id) for e in trace_events if e.pred_id == "other/0"
        ]
        z_events = [(e.port, e.pred_id) for e in trace_events if e.pred_id == "z/0"]

        # other/0 should CALL and FAIL
        assert ("call", "other/0") in other_events
        assert ("fail", "other/0") in other_events

        # z/0 should be called and succeed initially, then redo and succeed again, then fail
        # The exact sequence depends on how fail/0 propagates, but z/0 should show all attempts
        assert ("call", "z/0") in z_events
        assert ("exit", "z/0") in z_events  # At least one success before fail/0
