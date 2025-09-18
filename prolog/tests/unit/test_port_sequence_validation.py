"""
Tests for validating 4-port trace sequences.

Ensures that:
- CALL must precede EXIT/FAIL
- REDO only occurs after EXIT
- No invalid port transitions
- Sequence checker detects violations
"""

from typing import List, Tuple, Optional, Set
from dataclasses import dataclass

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.debug.tracer import PortsTracer, TraceEvent
from prolog.debug.sinks import CollectorSink


@dataclass(frozen=True)
class PortTransition:
    """Represents a port transition in the trace."""
    from_port: str
    to_port: str
    pred_id: str
    frame_depth: int


class PortSequenceValidator:
    """Validates 4-port trace sequences for correctness."""

    VALID_TRANSITIONS = {
        # From CALL
        ('call', 'exit'),     # Successful completion
        ('call', 'fail'),     # Immediate failure
        ('call', 'call'),     # Nested call

        # From EXIT
        ('exit', 'redo'),     # Backtrack to retry
        ('exit', 'call'),     # Next goal
        ('exit', 'exit'),     # Parent completion
        ('exit', 'fail'),     # Parent fails

        # From REDO
        ('redo', 'exit'),     # Retry succeeds
        ('redo', 'fail'),     # No more alternatives
        ('redo', 'call'),     # Nested call during retry

        # From FAIL
        ('fail', 'redo'),     # Backtrack to earlier choice
        ('fail', 'fail'),     # Propagate failure
        ('fail', 'call'),     # New goal after recovery
        ('fail', 'exit'),     # Parent succeeds despite child failure
    }

    def __init__(self):
        self.errors: List[str] = []
        self.call_stack: List[int] = []  # frame_depths that have been CALLed
        self.exited_frames: Set[int] = set()  # frame_depths that have been EXITed

    def validate_sequence(self, events: List[TraceEvent]) -> bool:
        """Validate a sequence of trace events.

        Returns True if valid, False otherwise.
        Errors are stored in self.errors.
        """
        self.errors = []
        self.call_stack = []
        self.exited_frames = set()

        prev_event: Optional[TraceEvent] = None

        for event in events:
            if event.port not in ['call', 'exit', 'redo', 'fail']:
                continue  # Skip non-4-port events

            # Check transition validity
            if prev_event:
                transition = (prev_event.port, event.port)
                if transition not in self.VALID_TRANSITIONS:
                    self.errors.append(
                        f"Invalid transition: {prev_event.port} -> {event.port} "
                        f"at step {event.step_id} for {event.pred_id}"
                    )

            # Track call stack by frame depth
            if event.port == 'call':
                self.call_stack.append(event.frame_depth)

            elif event.port == 'exit':
                # Must have a corresponding CALL at same depth
                if event.frame_depth not in self.call_stack:
                    self.errors.append(
                        f"EXIT without CALL at step {event.step_id} "
                        f"for {event.pred_id} at depth {event.frame_depth}"
                    )
                self.exited_frames.add(event.frame_depth)

            elif event.port == 'redo':
                # Must have previously exited this frame depth
                if event.frame_depth not in self.exited_frames:
                    self.errors.append(
                        f"REDO without prior EXIT at step {event.step_id} "
                        f"for {event.pred_id} at depth {event.frame_depth}"
                    )

            elif event.port == 'fail':
                # Remove from call stack if present
                if event.frame_depth in self.call_stack:
                    self.call_stack.remove(event.frame_depth)

            prev_event = event

        return len(self.errors) == 0



class TestPortSequenceValidation:
    """Test port sequence validation."""

    def test_valid_simple_sequence(self):
        """Test validation of a simple valid sequence."""
        clauses = [
            Clause(head=Struct("fact", (Int(1),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("fact(1)"))

        validator = PortSequenceValidator()
        is_valid = validator.validate_sequence(collector.events)
        if not is_valid:
            print(f"Validation errors: {validator.errors}")
            for ev in collector.events:
                print(f"  {ev.port} {ev.pred_id} depth={ev.frame_depth}")
        assert is_valid
        assert len(validator.errors) == 0

    def test_valid_backtracking_sequence(self):
        """Test validation with backtracking."""
        clauses = [
            Clause(head=Struct("choice", (Atom("a"),)), body=()),
            Clause(head=Struct("choice", (Atom("b"),)), body=()),
            Clause(head=Struct("choice", (Atom("c"),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("choice(X)"))

        validator = PortSequenceValidator()
        assert validator.validate_sequence(collector.events)

    def test_redo_after_exit_valid(self):
        """Test REDO only occurs after EXIT."""
        clauses = [
            Clause(head=Struct("multi", (Int(1),)), body=()),
            Clause(head=Struct("multi", (Int(2),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("multi(X)"))

        # Find REDO events and verify they follow EXIT
        events = collector.events
        for i, event in enumerate(events):
            if event.port == 'redo':
                # Look back for matching EXIT
                found_exit = False
                for j in range(i-1, -1, -1):
                    if (events[j].port == 'exit' and
                        events[j].pred_id == event.pred_id and
                        events[j].frame_depth == event.frame_depth):
                        found_exit = True
                        break
                assert found_exit, f"REDO without prior EXIT for {event.pred_id}"

    def test_nested_calls_valid(self):
        """Test nested predicate calls have valid sequences."""
        clauses = [
            Clause(head=Struct("a", (Int(1),)), body=()),
            Clause(head=Struct("a", (Int(2),)), body=()),
            Clause(
                head=Struct("b", (Var(0, "X"),)),
                body=(Struct("a", (Var(0, "X"),)),)
            ),
            Clause(
                head=Struct("c", (Var(0, "Y"),)),
                body=(Struct("b", (Var(0, "Y"),)),)
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("c(Y)"))

        validator = PortSequenceValidator()
        assert validator.validate_sequence(collector.events)

    def test_cut_sequence_valid(self):
        """Test sequence with cut operator is valid."""
        clauses = [
            Clause(head=Struct("det", (Int(1),)), body=(Atom("!"),)),
            Clause(head=Struct("det", (Int(2),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("det(X)"))

        validator = PortSequenceValidator()
        assert validator.validate_sequence(collector.events)

    def test_if_then_else_sequence_valid(self):
        """Test if-then-else produces valid sequences."""
        clauses = [
            Clause(head=Struct("cond", (Int(1),)), body=()),
            Clause(
                head=Struct("test", (Var(0, "X"), Var(1, "Y"))),
                body=(
                    Struct(";", (
                        Struct("->", (
                            Struct("cond", (Var(0, "X"),)),
                            Struct("=", (Var(1, "Y"), Int(10))),
                        )),
                        Struct("=", (Var(1, "Y"), Int(20)))
                    )),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Test both branches
        list(engine.query("test(1, Y)"))  # then branch
        list(engine.query("test(2, Y)"))  # else branch

        validator = PortSequenceValidator()
        assert validator.validate_sequence(collector.events)

    def test_failure_propagation_valid(self):
        """Test failure propagation has valid sequence."""
        clauses = [
            Clause(
                head=Struct("will_fail", ()),
                body=(
                    Struct("=", (Int(1), Int(2))),  # Will fail
                )
            ),
            Clause(
                head=Struct("test", ()),
                body=(
                    Struct("will_fail", ()),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("test"))

        validator = PortSequenceValidator()
        assert validator.validate_sequence(collector.events)

    def test_complex_backtracking_valid(self):
        """Test complex backtracking produces valid sequences."""
        clauses = [
            # Multiple choice points
            Clause(head=Struct("a", (Int(1),)), body=()),
            Clause(head=Struct("a", (Int(2),)), body=()),
            Clause(head=Struct("b", (Int(3),)), body=()),
            Clause(head=Struct("b", (Int(4),)), body=()),
            Clause(head=Struct("c", (Int(5),)), body=()),
            Clause(head=Struct("c", (Int(6),)), body=()),

            Clause(
                head=Struct("test", (Var(0, "X"), Var(1, "Y"), Var(2, "Z"))),
                body=(
                    Struct("a", (Var(0, "X"),)),
                    Struct("b", (Var(1, "Y"),)),
                    Struct("c", (Var(2, "Z"),)),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # This will generate 2*2*2 = 8 solutions with lots of backtracking
        list(engine.query("test(X, Y, Z)"))

        validator = PortSequenceValidator()
        is_valid = validator.validate_sequence(collector.events)
        if not is_valid:
            print(f"Validation errors: {validator.errors}")
            for ev in collector.events:
                print(f"  {ev.port} {ev.pred_id} depth={ev.frame_depth}")
        assert is_valid
        assert len(validator.errors) == 0


class TestPortSequenceInvariants:
    """Test invariants that must hold for port sequences."""

    def test_call_depth_increases(self):
        """Test CALL increases frame depth for nested calls."""
        clauses = [
            Clause(head=Struct("a", ()), body=()),
            Clause(
                head=Struct("b", ()),
                body=(Struct("a", ()),)
            ),
            Clause(
                head=Struct("c", ()),
                body=(Struct("b", ()),)
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("c"))

        # Extract CALL events
        calls = [ev for ev in collector.events if ev.port == 'call']

        # c should be at depth 0, b at depth 1, a at depth 2
        assert any(ev.pred_id == 'c/0' and ev.frame_depth == 0 for ev in calls)
        assert any(ev.pred_id == 'b/0' and ev.frame_depth == 1 for ev in calls)
        assert any(ev.pred_id == 'a/0' and ev.frame_depth == 2 for ev in calls)

    def test_exit_matches_call_depth(self):
        """Test EXIT has same frame depth as matching CALL."""
        clauses = [
            Clause(head=Struct("fact", (Int(42),)), body=()),
            Clause(
                head=Struct("rule", (Var(0, "X"),)),
                body=(Struct("fact", (Var(0, "X"),)),)
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("rule(X)"))

        # Map calls to exits by pred_id and depth
        calls = {}
        exits = {}

        for ev in collector.events:
            if ev.port == 'call':
                calls[(ev.pred_id, ev.frame_depth)] = ev
            elif ev.port == 'exit':
                exits[(ev.pred_id, ev.frame_depth)] = ev

        # Every exit should have a matching call at same depth
        for key in exits:
            assert key in calls, f"EXIT without CALL for {key}"

    def test_redo_preserves_depth(self):
        """Test REDO maintains same frame depth as original CALL."""
        clauses = [
            Clause(head=Struct("choice", (Int(1),)), body=()),
            Clause(head=Struct("choice", (Int(2),)), body=()),
            Clause(head=Struct("choice", (Int(3),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("choice(X)"))

        # Track depths for each predicate
        pred_depths = {}

        for ev in collector.events:
            if ev.port in ['call', 'exit', 'redo', 'fail']:
                if ev.pred_id not in pred_depths:
                    pred_depths[ev.pred_id] = set()
                pred_depths[ev.pred_id].add(ev.frame_depth)

        # Each predicate should maintain consistent depth
        for pred_id, depths in pred_depths.items():
            assert len(depths) == 1, f"{pred_id} has inconsistent depths: {depths}"

    def test_fail_does_not_leave_dangling_calls(self):
        """Test FAIL properly unwinds the call stack."""
        clauses = [
            Clause(
                head=Struct("will_fail", ()),
                body=(Struct("fail", ()),)  # Explicit fail
            ),
            Clause(
                head=Struct("wrapper", ()),
                body=(Struct("will_fail", ()),)
            ),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        list(engine.query("wrapper"))

        # Count CALLs and FAILs for each predicate
        calls = {}
        fails = {}

        for ev in collector.events:
            if ev.port == 'call':
                calls[ev.pred_id] = calls.get(ev.pred_id, 0) + 1
            elif ev.port == 'fail':
                fails[ev.pred_id] = fails.get(ev.pred_id, 0) + 1

        # Each called predicate should fail (no dangling calls)
        for pred_id in calls:
            if pred_id not in ['true/0', 'fail/0']:  # Skip builtins
                assert pred_id in fails, f"{pred_id} called but never failed/exited"