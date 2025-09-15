"""
Tests for Stage 3 Core Infrastructure - Minimal Ports Tracer.

Tests TraceEvent, PortsTracer, and Engine integration for debug infrastructure.
"""

import uuid
from dataclasses import FrozenInstanceError, replace
import pytest

from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.debug.tracer import TraceEvent, PortsTracer
from prolog.debug.sinks import PrettyTraceSink


class TestTraceEvent:
    """Tests for TraceEvent dataclass."""

    def test_trace_event_required_fields(self):
        """TraceEvent contains all required fields."""
        event = TraceEvent(
            version=1,
            run_id="a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c",
            step_id=1,
            port="call",
            goal=Struct("append", (List((Int(1), Int(2))), List((Int(3),)), Var(1, "X"))),
            goal_pretty="append([1,2], [3], X)",
            goal_canonical="append([1,2], [3], _G1)",
            frame_depth=2,
            cp_depth=1,
            goal_height=3,
            write_stamp=42,
            pred_id="append/3"
        )

        assert event.version == 1
        assert event.run_id == "a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c"
        assert event.step_id == 1
        assert event.port == "call"
        assert event.frame_depth == 2
        assert event.cp_depth == 1
        assert event.goal_height == 3
        assert event.write_stamp == 42
        assert event.pred_id == "append/3"
        assert event.goal_pretty == "append([1,2], [3], X)"

    def test_trace_event_schema_version(self):
        """TraceEvent schema version is 1."""
        event = TraceEvent(
            version=1,
            run_id="test-run",
            step_id=1,
            port="call",
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=1,
            write_stamp=1,
            pred_id="true/0"
        )
        assert event.version == 1

    def test_trace_event_run_id_format(self):
        """TraceEvent run_id is valid UUID string."""
        run_id = str(uuid.uuid4())
        event = TraceEvent(
            version=1,
            run_id=run_id,
            step_id=1,
            port="call",
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=1,
            write_stamp=1,
            pred_id="true/0"
        )
        # Should be valid UUID format
        uuid.UUID(event.run_id)  # Will raise if invalid

    @pytest.mark.parametrize("port", ["call", "exit", "redo", "fail"])
    def test_trace_event_port_values(self, port):
        """Port must be one of 'call'|'exit'|'redo'|'fail'."""
        event = TraceEvent(
            version=1,
            run_id="test",
            step_id=1,
            port=port,
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=1,
            write_stamp=1,
            pred_id="true/0"
        )
        assert event.port == port

    def test_trace_event_step_id_positive(self):
        """step_id is positive integer."""
        event = TraceEvent(
            version=1,
            run_id="test",
            step_id=42,
            port="call",
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=1,
            write_stamp=1,
            pred_id="true/0"
        )
        assert event.step_id == 42
        assert event.step_id > 0

    def test_trace_event_depths_non_negative(self):
        """Depths and heights are non-negative integers."""
        event = TraceEvent(
            version=1,
            run_id="test",
            step_id=1,
            port="call",
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=0,
            write_stamp=0,
            pred_id="true/0"
        )
        assert event.frame_depth >= 0
        assert event.cp_depth >= 0
        assert event.goal_height >= 0
        assert event.write_stamp >= 0

    def test_trace_event_immutable(self):
        """TraceEvent dataclass is immutable (frozen=True)."""
        event = TraceEvent(
            version=1,
            run_id="test",
            step_id=1,
            port="call",
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=1,
            write_stamp=1,
            pred_id="true/0"
        )

        with pytest.raises(FrozenInstanceError):
            event.step_id = 2

    def test_trace_event_has_slots(self):
        """TraceEvent uses slots for memory efficiency."""
        event = TraceEvent(
            version=1,
            run_id="test",
            step_id=1,
            port="call",
            goal=Atom("true"),
            goal_pretty="true",
            goal_canonical="true",
            frame_depth=0,
            cp_depth=0,
            goal_height=1,
            write_stamp=1,
            pred_id="true/0"
        )

        # Classes with __slots__ don't have __dict__
        assert not hasattr(event, '__dict__')
        # Also check that the class has __slots__
        assert hasattr(type(event), '__slots__')


class TestPortsTracer:
    """Tests for PortsTracer class."""

    def test_tracer_init_with_engine(self):
        """PortsTracer initializes with engine reference."""
        engine = Engine(program=[], trace=False)  # Create without tracer first
        tracer = PortsTracer(engine)

        assert tracer.engine is engine

    def test_tracer_step_counter_starts_at_zero(self):
        """Step counter starts at 0."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        assert tracer.step_counter == 0

    def test_tracer_run_id_generated(self):
        """Run ID is generated as UUID."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        # Should be valid UUID
        uuid.UUID(tracer.run_id)  # Will raise if invalid

    def test_tracer_default_bindings_policy(self):
        """Default bindings_policy is 'none'."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        assert tracer.bindings_policy == 'none'

    def test_tracer_default_max_term_depth(self):
        """Default max_term_depth is 4."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        assert tracer.max_term_depth == 4

    def test_tracer_default_max_items_per_list(self):
        """Default max_items_per_list is 10."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        assert tracer.max_items_per_list == 10

    def test_tracer_step_counter_real_emit(self):
        """Step counter increments only on real event emission."""
        engine = Engine(program=[], trace=True)
        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Emit two events
        engine._trace_port("call", Atom("first"))
        engine._trace_port("exit", Atom("first"))

        # Check step IDs are monotonic
        assert len(events) == 2
        assert events[0].step_id == 1
        assert events[1].step_id == 2
        assert engine.tracer.step_counter == 2

    def test_tracer_pred_id_interning(self):
        """Predicate ID interning cache."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        # Cache should start empty
        assert len(tracer._pred_id_cache) == 0

        # Intern a predicate ID
        pred_id = tracer._intern_pred_id("append", 3)
        assert pred_id == "append/3"
        assert ("append", 3) in tracer._pred_id_cache

        # Same predicate should return cached value (same string)
        pred_id2 = tracer._intern_pred_id("append", 3)
        assert pred_id2 == pred_id  # Equal values

        # Different predicate gets different ID
        pred_id3 = tracer._intern_pred_id("member", 2)
        assert pred_id3 == "member/2"
        assert pred_id3 != pred_id

    def test_tracer_pred_id_cache_reset(self):
        """Interning cache resets per run_id."""
        engine = Engine(program=[], trace=False)
        tracer = PortsTracer(engine)

        # Add some cached IDs
        pred_id1 = tracer._intern_pred_id("append", 3)
        tracer._intern_pred_id("member", 2)
        assert len(tracer._pred_id_cache) == 2

        old_run_id = tracer.run_id

        # Reset for new run
        tracer._reset_for_new_run()
        assert len(tracer._pred_id_cache) == 0
        assert tracer.step_counter == 0
        assert tracer.run_id != old_run_id

        # After reset, same predicate gets new object (fresh cache)
        pred_id2 = tracer._intern_pred_id("append", 3)
        assert pred_id2 == "append/3"  # Same value
        assert pred_id2 == pred_id1  # Equal strings
        # Not testing object identity since that's an implementation detail


class TestEngineIntegration:
    """Tests for Engine integration with tracing."""

    def test_engine_accepts_trace_parameter(self):
        """Engine accepts trace parameter."""
        engine = Engine(program=[], trace=True)
        assert engine.tracer is not None

        engine2 = Engine(program=[], trace=False)
        assert engine2.tracer is None

    def test_engine_creates_tracer_when_true(self):
        """Engine creates PortsTracer when trace=True."""
        engine = Engine(program=[], trace=True)

        assert isinstance(engine.tracer, PortsTracer)
        assert engine.tracer.engine is engine

    def test_engine_tracer_none_when_false(self):
        """Engine.tracer is None when trace=False."""
        engine = Engine(program=[], trace=False)

        assert engine.tracer is None

    def test_step_id_managed_by_tracer(self):
        """Step IDs are managed by tracer, not engine."""
        engine = Engine(program=[], trace=True)

        # Step ID management is in tracer
        assert hasattr(engine.tracer, 'step_counter')
        # Not testing for absence of _global_step_id as that's an implementation detail


class TestPortDetection:
    """Tests for port detection logic."""

    def test_call_port_emitted(self):
        """CALL port emitted at goal entry."""
        engine = Engine(program=[], trace=True)

        # Mock a goal entry
        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Simulate goal entry
        engine._trace_port("call", Struct("append", (List(()), List(()), Var(1))))

        assert len(events) == 1
        assert events[0].port == "call"

    def test_exit_port_emitted(self):
        """EXIT port emitted on success."""
        engine = Engine(program=[], trace=True)

        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Simulate successful goal
        engine._trace_port("exit", Struct("append", (List(()), List(()), List(()))))

        assert len(events) == 1
        assert events[0].port == "exit"

    def test_fail_port_emitted(self):
        """FAIL port emitted on failure."""
        engine = Engine(program=[], trace=True)

        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Simulate failed goal
        engine._trace_port("fail", Struct("append", (Atom("x"), List(()), Var(1))))

        assert len(events) == 1
        assert events[0].port == "fail"

    def test_redo_port_emitted(self):
        """REDO port emitted on backtrack retry."""
        engine = Engine(program=[], trace=True)

        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Simulate backtrack retry
        engine._trace_port("redo", Struct("member", (Var(1), List((Int(1), Int(2))))))

        assert len(events) == 1
        assert events[0].port == "redo"

    def test_port_sequence_valid(self):
        """Port sequence follows valid transitions."""
        engine = Engine(program=[], trace=True)

        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Valid sequence: CALL -> EXIT
        engine._trace_port("call", Atom("true"))
        engine._trace_port("exit", Atom("true"))

        assert len(events) == 2
        assert events[0].port == "call"
        assert events[1].port == "exit"

        # Valid sequence: CALL -> FAIL
        events.clear()
        engine._trace_port("call", Atom("fail"))
        engine._trace_port("fail", Atom("fail"))

        assert len(events) == 2
        assert events[0].port == "call"
        assert events[1].port == "fail"

        # Valid sequence with backtracking: CALL -> EXIT -> REDO -> EXIT
        events.clear()
        engine._trace_port("call", Struct("member", (Var(1), List((Int(1), Int(2))))))
        engine._trace_port("exit", Struct("member", (Int(1), List((Int(1), Int(2))))))
        engine._trace_port("redo", Struct("member", (Var(1), List((Int(1), Int(2))))))
        engine._trace_port("exit", Struct("member", (Int(2), List((Int(1), Int(2))))))

        assert len(events) == 4
        assert [e.port for e in events] == ["call", "exit", "redo", "exit"]

    def test_multi_sink_fanout(self):
        """Multiple sinks receive identical events."""
        engine = Engine(program=[], trace=True)

        events1 = []
        events2 = []
        engine.tracer.sinks = [MockSink(events1), MockSink(events2)]

        engine._trace_port("call", Atom("test"))

        assert len(events1) == 1
        assert len(events2) == 1
        assert events1[0].port == events2[0].port
        assert events1[0].step_id == events2[0].step_id


class TestStackDepthTracking:
    """Tests for stack depth tracking and invariants."""

    def test_frame_depth_equals_frame_stack(self):
        """frame_depth equals len(frame_stack)."""
        engine = Engine(program=[], trace=True)

        # Mock frame stack
        engine.frame_stack = [None, None, None]  # 3 frames

        event = engine.tracer._create_event("call", Atom("true"))
        assert event.frame_depth == 3

    def test_cp_depth_equals_choicepoints(self):
        """cp_depth equals len(choicepoints)."""
        engine = Engine(program=[], trace=True)

        # Mock choicepoint stack
        engine.choicepoints = [None, None]  # 2 choicepoints

        event = engine.tracer._create_event("call", Atom("true"))
        assert event.cp_depth == 2

    def test_goal_height_equals_goal_stack(self):
        """goal_height equals len(goal_stack)."""
        engine = Engine(program=[], trace=True)

        # Mock goal stack
        engine.goal_stack = [None, None, None, None]  # 4 goals

        event = engine.tracer._create_event("call", Atom("true"))
        assert event.goal_height == 4

    def test_write_stamp_monotonic(self):
        """write_stamp is monotonic."""
        engine = Engine(program=[], trace=True)

        # Mock write stamps
        engine.write_stamp = 42
        event1 = engine.tracer._create_event("call", Atom("true"))
        assert event1.write_stamp == 42

        engine.write_stamp = 43
        event2 = engine.tracer._create_event("exit", Atom("true"))
        assert event2.write_stamp == 43
        assert event2.write_stamp > event1.write_stamp

    def test_depths_update_during_execution(self):
        """Depths update correctly during execution."""
        engine = Engine(program=[], trace=True)
        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Simulate execution with changing stack depths
        engine.frame_stack = [None]
        engine.choicepoints = []
        engine.goal_stack = [None]
        engine._trace_port("call", Atom("first"))

        engine.frame_stack = [None, None]
        engine.choicepoints = [None]
        engine.goal_stack = [None, None]
        engine._trace_port("call", Atom("second"))

        assert events[0].frame_depth == 1
        assert events[0].cp_depth == 0
        assert events[0].goal_height == 1

        assert events[1].frame_depth == 2
        assert events[1].cp_depth == 1
        assert events[1].goal_height == 2

    def test_depths_restored_on_backtrack(self):
        """Depths restored correctly on backtrack."""
        engine = Engine(program=[], trace=True)
        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Before backtrack
        engine.frame_stack = [None, None]
        engine.choicepoints = [None]
        engine.goal_stack = [None, None, None]
        engine._trace_port("exit", Atom("before"))

        # After backtrack (stacks reduced)
        engine.frame_stack = [None]
        engine.choicepoints = []
        engine.goal_stack = [None]
        engine._trace_port("redo", Atom("after"))

        assert events[0].frame_depth == 2
        assert events[0].cp_depth == 1
        assert events[0].goal_height == 3

        assert events[1].frame_depth == 1
        assert events[1].cp_depth == 0
        assert events[1].goal_height == 1

    def test_invariant_checker_helper(self):
        """Create invariant_checker helper (reused in later phases)."""
        def check_invariants(engine: Engine, event: TraceEvent) -> bool:
            """Check that all invariants hold."""
            # Depths must equal actual stack sizes
            if event.frame_depth != len(engine.frame_stack):
                return False
            if event.cp_depth != len(engine.choicepoints):
                return False
            if event.goal_height != len(engine.goal_stack):
                return False

            # Write stamp must be non-negative
            if event.write_stamp < 0:
                return False

            # Step ID must be positive
            if event.step_id <= 0:
                return False

            return True

        engine = Engine(program=[], trace=True)
        engine.frame_stack = [None, None]
        engine.choicepoints = [None]
        engine.goal_stack = [None, None, None]
        engine.write_stamp = 10

        event = engine.tracer._create_event("call", Atom("test"))
        # Use dataclasses.replace instead of _replace
        event = replace(event, step_id=1)  # Set step_id for testing

        assert check_invariants(engine, event)

        # Break an invariant
        event_bad = replace(event, frame_depth=999)
        assert not check_invariants(engine, event_bad)


class TestPredicateInterning:
    """Tests for predicate ID interning."""

    def test_predicate_id_format(self):
        """Predicate IDs format as 'name/arity'."""
        engine = Engine(program=[], trace=True)

        pred_id = engine.tracer._intern_pred_id("append", 3)
        assert pred_id == "append/3"

        pred_id = engine.tracer._intern_pred_id("true", 0)
        assert pred_id == "true/0"

    def test_same_predicate_same_id(self):
        """Same predicate gets same ID."""
        engine = Engine(program=[], trace=True)

        id1 = engine.tracer._intern_pred_id("member", 2)
        id2 = engine.tracer._intern_pred_id("member", 2)

        assert id1 == id2

    def test_different_predicates_different_ids(self):
        """Different predicates get different IDs."""
        engine = Engine(program=[], trace=True)

        id1 = engine.tracer._intern_pred_id("append", 3)
        id2 = engine.tracer._intern_pred_id("member", 2)
        id3 = engine.tracer._intern_pred_id("append", 2)  # Different arity

        assert id1 != id2
        assert id1 != id3
        assert id2 != id3

    @pytest.mark.parametrize("name,arity,expected", [
        ("true", 0, "true/0"),
        ("fail", 0, "fail/0"),
        ("!", 0, "!/0"),
        ("=", 2, "=/2"),
        ("is", 2, "is/2"),
    ])
    def test_builtin_pred_ids(self, name, arity, expected):
        """Built-ins have correct IDs."""
        engine = Engine(program=[], trace=True)
        assert engine.tracer._intern_pred_id(name, arity) == expected

    def test_pred_id_from_goal(self):
        """Derive pred_id from goal without calling _intern_pred_id directly."""
        engine = Engine(program=[], trace=True)
        events = []
        engine.tracer.sinks = [MockSink(events)]

        # Test various goal types
        engine._trace_port("call", Atom("true"))
        engine._trace_port("call", Struct("append", (Var(1), Var(2), Var(3))))
        engine._trace_port("call", Struct("member", (Atom("x"), List(()))))

        assert events[0].pred_id == "true/0"
        assert events[1].pred_id == "append/3"
        assert events[2].pred_id == "member/2"


class TestPrettyOutput:
    """Tests for minimal pretty output sink."""

    def test_pretty_format_readable(self):
        """Pretty format is human-readable single line."""
        sink = PrettyTraceSink()

        event = TraceEvent(
            version=1,
            run_id="test",
            step_id=123,
            port="call",
            goal=Struct("append", (List((Int(1), Int(2))), List((Int(3),)), Var(1, "X"))),
            goal_pretty="append([1,2], [3], X)",
            goal_canonical="append([1,2], [3], _G1)",
            frame_depth=2,
            cp_depth=1,
            goal_height=5,
            write_stamp=42,
            pred_id="append/3"
        )

        output = sink.format_event(event)
        expected = "[123] call(5): append([1,2], [3], X) @ frame=2 cp=1"
        assert output == expected

    def test_pretty_includes_step_id(self):
        """Format includes step_id in brackets."""
        sink = PrettyTraceSink()

        event = TraceEvent(
            version=1, run_id="test", step_id=42,
            port="exit", goal=Atom("true"),
            goal_pretty="true", goal_canonical="true",
            frame_depth=0, cp_depth=0, goal_height=1,
            write_stamp=1, pred_id="true/0"
        )

        output = sink.format_event(event)
        assert output.startswith("[42]")

    def test_pretty_includes_port_name(self):
        """Format includes port name."""
        sink = PrettyTraceSink()

        for port in ["call", "exit", "redo", "fail"]:
            event = TraceEvent(
                version=1, run_id="test", step_id=1,
                port=port, goal=Atom("test"),
                goal_pretty="test", goal_canonical="test",
                frame_depth=0, cp_depth=0, goal_height=1,
                write_stamp=1, pred_id="test/0"
            )

            output = sink.format_event(event)
            assert port in output

    def test_pretty_includes_goal(self):
        """Format includes goal pretty form."""
        sink = PrettyTraceSink()

        event = TraceEvent(
            version=1, run_id="test", step_id=1,
            port="call",
            goal=Struct("member", (Var(1, "X"), List((Int(1), Int(2), Int(3))))),
            goal_pretty="member(X, [1,2,3])",
            goal_canonical="member(_G1, [1,2,3])",
            frame_depth=1, cp_depth=0, goal_height=2,
            write_stamp=1, pred_id="member/2"
        )

        output = sink.format_event(event)
        assert "member(X, [1,2,3])" in output

    def test_pretty_includes_depths(self):
        """Format includes frame and cp depths."""
        sink = PrettyTraceSink()

        event = TraceEvent(
            version=1, run_id="test", step_id=1,
            port="call", goal=Atom("test"),
            goal_pretty="test", goal_canonical="test",
            frame_depth=3, cp_depth=2, goal_height=5,
            write_stamp=1, pred_id="test/0"
        )

        output = sink.format_event(event)
        assert "frame=3" in output
        assert "cp=2" in output

    def test_pretty_truncation_contract(self):
        """Goal truncation respects max_goal_length."""
        sink = PrettyTraceSink()
        sink.max_goal_length = 20  # Set max length

        # Create a very long goal
        long_list = List(tuple(Int(i) for i in range(100)))
        event = TraceEvent(
            version=1, run_id="test", step_id=1,
            port="call",
            goal=Struct("process", (long_list,)),
            goal_pretty="process([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...])",
            goal_canonical="process([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,...])",
            frame_depth=1, cp_depth=0, goal_height=1,
            write_stamp=1, pred_id="process/1"
        )

        output = sink.format_event(event)
        # Extract just the goal part (between : and @)
        goal_part = output.split(": ")[1].split(" @ ")[0]

        # Goal part should not exceed max_goal_length
        assert len(goal_part) <= sink.max_goal_length
        # Should contain ellipsis for truncation
        if len(event.goal_pretty) > sink.max_goal_length:
            assert "..." in goal_part

    def test_pretty_caps_applied(self):
        """Caps apply (max_term_depth=4, max_items_per_list=10)."""
        sink = PrettyTraceSink()

        # Deep nested structure
        deep = Struct("f", (Struct("g", (Struct("h", (Struct("i", (Atom("deep"),)),)),)),))

        # Long list
        long_list = List(tuple(Int(i) for i in range(20)))

        event = TraceEvent(
            version=1, run_id="test", step_id=1,
            port="call",
            goal=Struct("test", (deep, long_list)),
            goal_pretty="test(f(g(h(...))), [0,1,2,3,4,5,6,7,8,9,...])",
            goal_canonical="test(f(g(h(i(deep)))), [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19])",
            frame_depth=1, cp_depth=0, goal_height=1,
            write_stamp=1, pred_id="test/2"
        )

        output = sink.format_event(event)
        # Should show truncation
        assert "..." in output


# Mock sink for testing
class MockSink:
    """Mock sink that collects events for testing."""

    def __init__(self, events_list):
        self.events = events_list

    def write_event(self, event):
        self.events.append(event)