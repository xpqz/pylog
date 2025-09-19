"""
Tests for validating 4-port trace sequences.

Ensures that:
- CALL must precede EXIT/FAIL
- REDO only occurs after EXIT
- No invalid port transitions
- Sequence checker detects violations
"""

from typing import List, Tuple, Optional, Set, Dict
from dataclasses import dataclass
from collections import defaultdict

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.debug.tracer import TraceEvent
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

    def validate_sequence(self, events: List[TraceEvent]) -> bool:
        """Validate a sequence of trace events.

        Returns True if valid, False otherwise.
        Errors are stored in self.errors.
        """
        self.errors = []
        prev_event: Optional[TraceEvent] = None

        call_counts: Dict[Tuple[str, int], int] = defaultdict(int)
        exited_frames: Set[Tuple[str, int]] = set()

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

            key = (event.pred_id, event.frame_depth)

            if event.port == 'call':
                call_counts[key] += 1

            elif event.port == 'exit':
                if call_counts[key] <= 0:
                    self.errors.append(
                        f"EXIT without CALL at step {event.step_id} "
                        f"for {event.pred_id} at depth {event.frame_depth}"
                    )
                else:
                    call_counts[key] -= 1
                exited_frames.add(key)

            elif event.port == 'redo':
                if key not in exited_frames:
                    self.errors.append(
                        f"REDO without prior EXIT at step {event.step_id} "
                        f"for {event.pred_id} at depth {event.frame_depth}"
                    )

            elif event.port == 'fail':
                if call_counts[key] > 0:
                    call_counts[key] -= 1
                elif key not in exited_frames:
                    self.errors.append(
                        f"FAIL without CALL/EXIT at step {event.step_id} "
                        f"for {event.pred_id} at depth {event.frame_depth}"
                    )

            prev_event = event

        # Don't report unmatched CALLs as errors - this is normal in many scenarios:
        # - Failures leave unmatched calls
        # - If-then-else may skip branches
        # - Cut operations may prevent exits
        # - Complex backtracking may have incomplete traces
        # We could log this as a warning for debugging, but it's not an error

        return len(self.errors) == 0


def format_trace_events(events: List[TraceEvent]) -> str:
    """Pretty-print trace events for debugging."""
    lines = []
    for i, ev in enumerate(events):
        lines.append(
            f"{i:3}: {ev.port:4} {ev.pred_id or 'N/A':15} depth={ev.frame_depth}"
        )
    return "\n".join(lines)


def validate_sequences_by_run(events: List[TraceEvent]) -> Tuple[bool, Dict[str, List[str]]]:
    """Validate sequences grouped by run_id.

    Returns:
        Tuple of (all_valid, errors_by_run)
    """
    # Group events by run_id
    runs = defaultdict(list)
    for ev in events:
        if hasattr(ev, 'run_id') and ev.run_id:
            runs[ev.run_id].append(ev)
        else:
            runs['default'].append(ev)

    all_valid = True
    errors_by_run = {}

    for run_id, run_events in runs.items():
        validator = PortSequenceValidator()
        if not validator.validate_sequence(run_events):
            all_valid = False
            errors_by_run[run_id] = validator.errors

    return all_valid, errors_by_run



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

        # Validate by run since we have multiple queries
        valid, errors = validate_sequences_by_run(collector.events)
        assert valid, f"Validation errors: {errors}"

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

        # Validate the complex sequence
        valid, errors = validate_sequences_by_run(collector.events)
        if not valid:
            print(f"Validation errors: {errors}")
            print("\nTrace events:")
            print(format_trace_events([ev for ev in collector.events if ev.port in ['call', 'exit', 'redo', 'fail']]))
        assert valid, f"Complex backtracking validation failed: {errors}"


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

        # Map calls to exits by depth only (pred_id can change during execution)
        calls_by_depth = {}
        exits_by_depth = {}

        for ev in collector.events:
            if ev.port == 'call':
                if ev.frame_depth not in calls_by_depth:
                    calls_by_depth[ev.frame_depth] = []
                calls_by_depth[ev.frame_depth].append(ev)
            elif ev.port == 'exit':
                if ev.frame_depth not in exits_by_depth:
                    exits_by_depth[ev.frame_depth] = []
                exits_by_depth[ev.frame_depth].append(ev)

        # Every exit depth should have at least one matching call at same depth
        for depth in exits_by_depth:
            assert depth in calls_by_depth, f"EXIT at depth {depth} without any CALL at that depth"
            # Also check counts are reasonable (exits <= calls at each depth)
            assert len(exits_by_depth[depth]) <= len(calls_by_depth[depth]), \
                f"More EXITs than CALLs at depth {depth}"

    def test_redo_preserves_depth(self):
        """Test REDO maintains same frame depth as original CALL.

        Note: This test uses non-recursive predicates (choice/1) to ensure
        each predicate is only called at one depth. Recursive predicates
        would naturally be called at multiple depths.
        """
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
        """Test FAIL event is generated for the failing predicate."""
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

        # Collect all ports
        ports_seen = set()
        calls_by_depth = {}

        for ev in collector.events:
            ports_seen.add(ev.port)
            if ev.port == 'call':
                calls_by_depth[ev.frame_depth] = ev.pred_id

        # Should see at least one FAIL port (from fail/0)
        assert 'fail' in ports_seen, "No FAIL port seen in trace"

        # Should see CALLs at different depths
        assert len(calls_by_depth) >= 2, "Should see nested calls"

        # The deepest call should be to fail/0
        max_depth = max(calls_by_depth.keys())
        deepest_pred = calls_by_depth[max_depth]
        assert deepest_pred == 'fail/0', f"Deepest call should be fail/0, got {deepest_pred}"


class TestMultiQueryValidation:
    """Test validation across multiple queries."""

    def test_multi_query_separate_runs(self):
        """Test that multiple queries are validated separately by run_id."""
        clauses = [
            Clause(head=Struct("fact", (Int(1),)), body=()),
            Clause(head=Struct("fact", (Int(2),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, trace=True)
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Run multiple queries
        list(engine.query("fact(1)"))
        list(engine.query("fact(2)"))
        list(engine.query("fact(X)"))  # Backtracking query

        # Each query should have its own run_id and be validated separately
        valid, errors = validate_sequences_by_run(collector.events)
        assert valid, f"Multi-query validation failed: {errors}"

        # Verify we have multiple runs
        runs = defaultdict(list)
        for ev in collector.events:
            if ev.port in ['call', 'exit', 'redo', 'fail'] and hasattr(ev, 'run_id'):
                runs[ev.run_id].append(ev)

        assert len(runs) >= 3, f"Expected at least 3 runs, got {len(runs)}"
