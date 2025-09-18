"""Tests for internal debug events (beyond standard 4-port tracing)."""

import json
from typing import List, Dict, Any
from dataclasses import dataclass

from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.ast.clauses import Clause
from prolog.engine.engine import Engine, Program
from prolog.debug.tracer import PortsTracer, TraceEvent, InternalEvent
from prolog.debug.sinks import CollectorSink


class TestInternalEventStructure:
    """Test InternalEvent dataclass structure and behavior."""

    def test_internal_event_dataclass(self):
        """InternalEvent should be frozen with slots."""
        event = InternalEvent(
            step_id=1,
            kind="cp_push",
            details={"pred_id": "append/3", "trail_top": 42}
        )

        # Should be frozen (immutable)
        import pytest
        with pytest.raises(AttributeError):
            event.step_id = 2

        # Should have expected fields
        assert event.step_id == 1
        assert event.kind == "cp_push"
        assert event.details["pred_id"] == "append/3"
        assert event.details["trail_top"] == 42

    def test_internal_event_types(self):
        """All internal event types should be recognized."""
        valid_kinds = [
            "cp_push", "cp_pop",
            "frame_push", "frame_pop",
            "cut_commit", "catch_switch"
        ]

        for kind in valid_kinds:
            event = InternalEvent(step_id=1, kind=kind, details={})
            assert event.kind == kind

    def test_internal_event_extends_base(self):
        """InternalEvent should be distinguishable from TraceEvent."""
        trace_event = TraceEvent(
            version=1,
            run_id="test-run",
            step_id=1,
            port="call",
            goal=Atom("test"),
            goal_pretty="test",
            goal_canonical="test",
            frame_depth=0,
            cp_depth=0,
            goal_height=0,
            write_stamp=0,
            pred_id="test/0"
        )

        internal_event = InternalEvent(
            step_id=2,
            kind="cp_push",
            details={}
        )

        # Should be different types
        assert type(trace_event) != type(internal_event)

        # Should both have step_id
        assert hasattr(trace_event, "step_id")
        assert hasattr(internal_event, "step_id")

        # Only TraceEvent has port
        assert hasattr(trace_event, "port")
        assert not hasattr(internal_event, "port")

        # Only InternalEvent has kind
        assert not hasattr(trace_event, "kind")
        assert hasattr(internal_event, "kind")


class TestInternalEventGeneration:
    """Test generation of internal events during execution."""

    def test_internal_events_off_by_default(self):
        """Internal events should be OFF by default."""
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)

        # Run a simple query
        list(engine.query("?- test."))

        # Should only have standard 4-port events
        for event in sink.events:
            assert isinstance(event, TraceEvent)
            assert not isinstance(event, InternalEvent)
            assert event.port in ["call", "exit", "redo", "fail"]

    def test_enable_internal_events(self):
        """When enabled, internal events should be generated."""
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)

        # Enable internal events
        engine.tracer.enable_internal_events = True

        # Run a simple query
        list(engine.query("?- test."))

        # Should have both TraceEvents and InternalEvents
        trace_events = [e for e in sink.events if isinstance(e, TraceEvent)
                       and not isinstance(e, InternalEvent)]
        internal_events = [e for e in sink.events if isinstance(e, InternalEvent)]

        assert len(trace_events) > 0  # Standard events still present
        assert len(internal_events) > 0  # Internal events added

        # Check for frame events (test/0 should push/pop frame)
        frame_pushes = [e for e in internal_events if e.kind == "frame_push"]
        frame_pops = [e for e in internal_events if e.kind == "frame_pop"]
        assert len(frame_pushes) > 0
        assert len(frame_pops) > 0

    def test_four_port_stream_unchanged(self):
        """Enabling internal events should not change 4-port stream."""
        program = Program((
            Clause(Struct("test", (Var(0, "X"),)), (
                Struct("member", (Var(0, "X"),
                       Struct(".", (Int(1), Struct(".", (Int(2), Atom("[]"))))))),
            )),
            Clause(Struct("member", (Var(0, "X"),
                   Struct(".", (Var(0, "X"), Var(1, "_"))))), ()),
            Clause(Struct("member", (Var(0, "X"),
                   Struct(".", (Var(1, "_"), Var(2, "T"))))), (
                Struct("member", (Var(0, "X"), Var(2, "T"))),
            )),
        ))

        engine1 = Engine(program, trace=True)
        sink1 = CollectorSink()
        engine1.tracer.add_sink(sink1)

        # Run without internal events
        list(engine1.query("?- test(X)."))

        engine2 = Engine(program, trace=True)
        sink2 = CollectorSink()
        engine2.tracer.add_sink(sink2)
        engine2.tracer.enable_internal_events = True

        # Run with internal events
        list(engine2.query("?- test(X)."))

        # Extract only 4-port events from both (ignoring step_id)
        ports1 = [(e.port, e.pred_id)
                  for e in sink1.events
                  if isinstance(e, TraceEvent)]
        ports2 = [(e.port, e.pred_id)
                  for e in sink2.events
                  if isinstance(e, TraceEvent) and not isinstance(e, InternalEvent)]

        # 4-port sequence should be identical (ignoring step_ids which will differ)
        assert ports1 == ports2


class TestChoicepointEvents:
    """Test choicepoint push/pop events."""

    def test_cp_push_event(self):
        """cp_push events should be generated when choicepoints are created."""
        program = Program((
            # Multiple clauses create choicepoints
            Clause(Struct("multi", (Int(1),)), ()),
            Clause(Struct("multi", (Int(2),)), ()),
            Clause(Struct("multi", (Int(3),)), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- multi(X)."))

        cp_pushes = [e for e in sink.events
                     if isinstance(e, InternalEvent) and e.kind == "cp_push"]

        # Should have CP pushes for multi/1
        assert len(cp_pushes) > 0

        # Check details
        for event in cp_pushes:
            assert "pred_id" in event.details
            assert "trail_top" in event.details
            assert event.details["pred_id"] == "multi/1"
            assert isinstance(event.details["trail_top"], int)

    def test_cp_pop_event(self):
        """cp_pop events should be generated when choicepoints are removed."""
        program = Program((
            Clause(Struct("single", ()), ()),  # No choicepoint
            Clause(Struct("multi", (Int(1),)), ()),
            Clause(Struct("multi", (Int(2),)), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        # Force backtracking by collecting all solutions
        solutions = list(engine.query("?- multi(X)."))
        assert len(solutions) == 2

        cp_pops = [e for e in sink.events
                   if isinstance(e, InternalEvent) and e.kind == "cp_pop"]

        # Should have CP pops when exhausting alternatives
        assert len(cp_pops) > 0

        for event in cp_pops:
            assert "pred_id" in event.details

    def test_cp_events_balanced(self):
        """CP push/pop events should be balanced."""
        program = Program((
            Clause(Struct("test", (Var(0, "X"),)), (
                Struct("multi", (Var(0, "X"),)),
            )),
            Clause(Struct("multi", (Atom("a"),)), ()),
            Clause(Struct("multi", (Atom("b"),)), ()),
            Clause(Struct("multi", (Atom("c"),)), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        # Get all solutions to ensure all CPs are popped
        list(engine.query("?- test(X)."))

        cp_pushes = [e for e in sink.events
                     if isinstance(e, InternalEvent) and e.kind == "cp_push"]
        cp_pops = [e for e in sink.events
                   if isinstance(e, InternalEvent) and e.kind == "cp_pop"]

        # All pushed CPs should eventually be popped when query completes
        # (CP stack should be empty at end)
        assert len(cp_pops) == len(cp_pushes), \
            f"Unbalanced CP events: {len(cp_pushes)} pushes but {len(cp_pops)} pops"


class TestFrameEvents:
    """Test frame push/pop events."""

    def test_frame_push_event(self):
        """frame_push events should be generated for goal activation."""
        program = Program((
            Clause(Struct("parent", ()), (
                Struct("child", ()),
            )),
            Clause(Struct("child", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- parent."))

        frame_pushes = [e for e in sink.events
                        if isinstance(e, InternalEvent) and e.kind == "frame_push"]

        # Should have frame pushes for both parent and child
        assert len(frame_pushes) >= 2

        pred_ids = [e.details.get("pred_id") for e in frame_pushes]
        assert "parent/0" in pred_ids
        assert "child/0" in pred_ids

        # Check frame_id is present
        for event in frame_pushes:
            assert "frame_id" in event.details
            assert isinstance(event.details["frame_id"], int)

    def test_frame_pop_event(self):
        """frame_pop events should be generated when goals complete."""
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- test."))

        frame_pops = [e for e in sink.events
                      if isinstance(e, InternalEvent) and e.kind == "frame_pop"]

        # Should have frame pop for test/0
        assert len(frame_pops) >= 1

        for event in frame_pops:
            assert "frame_id" in event.details
            assert "pred_id" in event.details

    def test_frame_events_nested(self):
        """Frame events should properly track nested calls."""
        program = Program((
            Clause(Struct("a", ()), (Struct("b", ()),)),
            Clause(Struct("b", ()), (Struct("c", ()),)),
            Clause(Struct("c", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- a."))

        frame_pushes = [e for e in sink.events
                        if isinstance(e, InternalEvent) and e.kind == "frame_push"]
        frame_pops = [e for e in sink.events
                      if isinstance(e, InternalEvent) and e.kind == "frame_pop"]

        # Push order should be: a, b, c
        push_preds = [e.details["pred_id"] for e in frame_pushes]
        assert push_preds == ["a/0", "b/0", "c/0"]

        # Pop order should be: c, b, a (reverse)
        pop_preds = [e.details["pred_id"] for e in frame_pops]
        assert pop_preds == ["c/0", "b/0", "a/0"]


class TestCutEvents:
    """Test cut execution events."""

    def test_cut_commit_event(self):
        """cut_commit events should track alternatives pruned."""
        program = Program((
            Clause(Struct("cuttest", (Var(0, "X"),)), (
                Struct("multi", (Var(0, "X"),)),
                Atom("!"),
            )),
            Clause(Struct("multi", (Int(1),)), ()),
            Clause(Struct("multi", (Int(2),)), ()),
            Clause(Struct("multi", (Int(3),)), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        # Cut should prune alternatives
        solutions = list(engine.query("?- cuttest(X)."))
        assert len(solutions) == 1  # Only first solution

        cut_events = [e for e in sink.events
                      if isinstance(e, InternalEvent) and e.kind == "cut_commit"]

        # Should have a cut event
        assert len(cut_events) >= 1

        for event in cut_events:
            assert "alternatives_pruned" in event.details
            # Should have pruned the other 2 alternatives (multi/1 has 3 clauses)
            assert event.details["alternatives_pruned"] == 2, \
                f"Expected 2 alternatives pruned, got {event.details['alternatives_pruned']}"

    def test_cut_no_alternatives(self):
        """Cut with no alternatives should still generate event."""
        program = Program((
            Clause(Struct("noalt", ()), (
                Atom("!"),
            )),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- noalt."))

        cut_events = [e for e in sink.events
                      if isinstance(e, InternalEvent) and e.kind == "cut_commit"]

        # Should still have cut event
        assert len(cut_events) >= 1

        for event in cut_events:
            # No alternatives to prune
            assert event.details["alternatives_pruned"] == 0


class TestCatchEvents:
    """Test exception catch events."""

    def test_catch_switch_event(self):
        """catch_switch events should be generated when catching exceptions."""
        program = Program((
            Clause(Struct("safe", (Var(0, "Result"),)), (
                Struct("catch", (
                    Struct("risky", ()),
                    Var(1, "Error"),
                    Struct("=", (Var(0, "Result"), Atom("caught")))
                )),
            )),
            Clause(Struct("risky", ()), (
                Struct("throw", (Atom("error"),)),
            )),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        solutions = list(engine.query("?- safe(R)."))
        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("caught")

        catch_events = [e for e in sink.events
                        if isinstance(e, InternalEvent) and e.kind == "catch_switch"]

        # Should have catch event
        assert len(catch_events) >= 1

        for event in catch_events:
            assert "exception" in event.details
            assert "handler" in event.details

    def test_no_catch_event_on_success(self):
        """No catch_switch event when goal succeeds."""
        program = Program((
            Clause(Struct("safe", ()), (
                Struct("catch", (
                    Atom("true"),  # Always succeeds
                    Var(0, "_"),
                    Atom("fail")
                )),
            )),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- safe."))

        catch_events = [e for e in sink.events
                        if isinstance(e, InternalEvent) and e.kind == "catch_switch"]

        # Should NOT have catch event (no exception)
        assert len(catch_events) == 0

    def test_catch_with_backtracking_regression(self):
        """Regression test: CATCH choicepoint goal height tracking with POP_FRAME sentinel."""
        # This tests the specific bug where CATCH CP stored stale goal height
        # when POP_FRAME sentinel was consumed before backtracking
        program = Program((
            Clause(Struct("test_catch_backtrack", ()), (
                Struct("catch", (
                    Struct(";", (Atom("true"), Atom("fail"))),  # Disjunction that backtracks
                    Var(0, "_"),
                    Atom("fail")
                )),
            )),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        results = list(engine.query("?- test_catch_backtrack."))

        # Should succeed once (from the true branch of disjunction)
        assert len(results) == 1

        # Verify CP events are balanced
        cp_push = [e for e in sink.events
                   if isinstance(e, InternalEvent) and e.kind == "cp_push"]
        cp_pop = [e for e in sink.events
                  if isinstance(e, InternalEvent) and e.kind == "cp_pop"]

        # Should have at least one CP (for disjunction)
        # CATCH doesn't always create a CP - only when needed
        assert len(cp_push) >= 1
        # All CPs should be properly cleaned up
        assert len(cp_push) == len(cp_pop)


class TestEventOrdering:
    """Test that internal events appear in correct order."""

    def test_frame_before_port(self):
        """frame_push should come after CALL port."""
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- test."))

        # Find frame_push for test/0
        frame_idx = None
        for i, e in enumerate(sink.events):
            if (isinstance(e, InternalEvent) and
                e.kind == "frame_push" and
                e.details.get("pred_id") == "test/0"):
                frame_idx = i
                break

        # Find CALL port for test/0
        call_idx = None
        for i, e in enumerate(sink.events):
            if (isinstance(e, TraceEvent) and
                e.port == "call" and
                e.pred_id == "test/0"):
                call_idx = i
                break

        assert frame_idx is not None
        assert call_idx is not None
        assert call_idx < frame_idx  # CALL comes before frame_push

    def test_port_before_frame_pop(self):
        """EXIT/FAIL port should come after frame_pop."""
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        list(engine.query("?- test."))

        # Find EXIT port for test/0
        exit_idx = None
        for i, e in enumerate(sink.events):
            if (isinstance(e, TraceEvent) and
                e.port == "exit" and
                e.pred_id == "test/0"):
                exit_idx = i
                break

        # Find frame_pop for test/0
        frame_idx = None
        for i, e in enumerate(sink.events):
            if (isinstance(e, InternalEvent) and
                e.kind == "frame_pop" and
                e.details.get("pred_id") == "test/0"):
                frame_idx = i
                break

        assert exit_idx is not None
        assert frame_idx is not None
        assert frame_idx < exit_idx  # frame_pop comes before EXIT


class TestEventCounting:
    """Test event counting and overhead."""

    def test_internal_events_increase_event_count(self):
        """Enabling internal events should increase total event count."""
        program = Program((
            Clause(Struct("test", ()), (
                Struct("child1", ()),
                Struct("child2", ()),
            )),
            Clause(Struct("child1", ()), ()),
            Clause(Struct("child2", ()), ()),
        ))

        # Run without internal events
        engine1 = Engine(program, trace=True)
        sink1 = CollectorSink()
        engine1.tracer.add_sink(sink1)
        engine1.tracer.enable_internal_events = False
        list(engine1.query("?- test."))

        # Run with internal events
        engine2 = Engine(program, trace=True)
        sink2 = CollectorSink()
        engine2.tracer.add_sink(sink2)
        engine2.tracer.enable_internal_events = True
        list(engine2.query("?- test."))

        # Should have more events with internal events enabled
        assert len(sink2.events) > len(sink1.events)

        # Calculate overhead ratio
        internal_count = len([e for e in sink2.events if isinstance(e, InternalEvent)])
        trace_count = len([e for e in sink2.events
                          if isinstance(e, TraceEvent) and not isinstance(e, InternalEvent)])

        # Should have both types of events
        assert internal_count > 0
        assert trace_count > 0

        # Internal events should not overwhelm trace events (reasonable ratio)
        # Allow up to 3x internal events vs trace events
        assert internal_count <= trace_count * 3

    def test_no_internal_events_when_debug_false(self):
        """No events at all when trace=False."""
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        engine = Engine(program, trace=False)
        # No tracer at all when trace=False
        assert engine.tracer is None

        # Query should still work without tracer
        list(engine.query("?- test."))

class TestBuiltinTracing:
    """Test that builtins emit trace events correctly."""

    def test_builtin_emits_call_exit(self):
        """Test that builtins emit CALL and EXIT events."""
        engine = Engine(Program(()), trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)

        # Execute a successful builtin
        list(engine.query("?- is(1,1)."))

        # Should have CALL and EXIT events
        trace_events = [e for e in sink.events if isinstance(e, TraceEvent)]
        assert len(trace_events) == 2
        assert trace_events[0].port == "call"
        assert trace_events[0].pred_id == "is/2"
        assert trace_events[1].port == "exit"
        assert trace_events[1].pred_id == "is/2"

    def test_builtin_emits_call_fail(self):
        """Test that failing builtins emit CALL and FAIL events."""
        engine = Engine(Program(()), trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)

        # Execute a failing builtin
        list(engine.query("?- is(1,2)."))

        # Should have CALL and FAIL events
        trace_events = [e for e in sink.events if isinstance(e, TraceEvent)]
        assert len(trace_events) == 2
        assert trace_events[0].port == "call"
        assert trace_events[0].pred_id == "is/2"
        assert trace_events[1].port == "fail"
        assert trace_events[1].pred_id == "is/2"

    def test_builtin_var_emits_events(self):
        """Test that type-checking builtins emit events."""
        engine = Engine(Program(()), trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)

        # Execute var/1 builtin (should fail on integer)
        list(engine.query("?- var(5)."))

        # Should have CALL and FAIL events
        trace_events = [e for e in sink.events if isinstance(e, TraceEvent)]
        assert len(trace_events) == 2
        assert trace_events[0].port == "call"
        assert trace_events[0].pred_id == "var/1"
        assert trace_events[1].port == "fail"


class TestSinkCompatibility:
    """Test that all sinks can handle internal events."""

    def test_pretty_sink_with_internal_events(self):
        """PrettyTraceSink should format internal events without crashing."""
        import io
        from prolog.debug.sinks import PrettyTraceSink
        
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        output = io.StringIO()
        engine = Engine(program, trace=True)
        sink = PrettyTraceSink(output=output, batch_size=1)
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        # Should not raise AttributeError
        list(engine.query("?- test."))
        
        result = output.getvalue()
        assert "CALL test/0" in result
        assert "INTERNAL frame_push" in result
        assert "INTERNAL frame_pop" in result
        assert "EXIT test/0" in result

    def test_jsonl_sink_with_internal_events(self):
        """JSONLTraceSink should serialize internal events without crashing."""
        import io
        import json
        from prolog.debug.sinks import JSONLTraceSink
        
        program = Program((
            Clause(Struct("test", ()), ()),
        ))

        output = io.StringIO()
        engine = Engine(program, trace=True)
        sink = JSONLTraceSink(output=output, batch_size=1)
        engine.tracer.add_sink(sink)
        engine.tracer.enable_internal_events = True

        # Should not raise AttributeError
        list(engine.query("?- test."))
        sink.flush()  # Make sure everything is written

        # Parse each line as JSON to verify format
        lines = output.getvalue().strip().split('\n')
        for line in lines:
            obj = json.loads(line)
            assert 'v' in obj
            assert 'type' in obj
            assert obj['type'] in ['trace', 'internal']
            if obj['type'] == 'internal':
                assert 'kind' in obj
                assert 'details' in obj
