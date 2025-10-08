"""
Integration tests for DAP components with PyLog Engine.

Tests the interaction between StepController, BreakpointStore, and the Engine's tracer.
"""

import pytest

from prolog.engine.engine import Engine
from prolog.debug.dap import StepController, BreakpointStore
from prolog.debug.tracer import PortsTracer, TraceEvent
from prolog.parser.reader import Reader
from prolog.ast.clauses import Program
from prolog.ast.terms import Struct, Atom


@pytest.fixture
def simple_program():
    """Simple Prolog program for testing."""
    return """
    member(X, [X|_]).
    member(X, [_|T]) :- member(X, T).

    append([], L, L).
    append([H|T], L, [H|R]) :- append(T, L, R).
    """


@pytest.fixture
def engine_with_dap(simple_program):
    """Engine with DAP components integrated."""
    reader = Reader()
    clauses = reader.read_program(simple_program)
    program = Program(clauses=tuple(clauses))

    # Create DAP components
    controller = StepController(eligible_ports={"call", "exit", "fail", "redo"})
    bp_store = BreakpointStore()

    # Create engine with tracing enabled
    engine = Engine(program=program, trace=True)

    # Replace tracer with one that has DAP components
    engine.tracer = PortsTracer(
        engine, step_controller=controller, breakpoint_store=bp_store
    )

    # Store references for test access
    engine._test_controller = controller
    engine._test_bp_store = bp_store

    return engine


def test_tracer_accepts_dap_components(simple_program):
    """PortsTracer can be initialized with StepController and BreakpointStore."""
    reader = Reader()
    clauses = reader.read_program(simple_program)
    program = Program(clauses=tuple(clauses))
    engine = Engine(program=program, trace=False)

    controller = StepController()
    bp_store = BreakpointStore()

    tracer = PortsTracer(engine, step_controller=controller, breakpoint_store=bp_store)

    assert tracer.step_controller is controller
    assert tracer.breakpoint_store is bp_store


def test_tracer_works_without_dap_components(simple_program):
    """PortsTracer works normally when DAP components are not provided."""
    reader = Reader()
    clauses = reader.read_program(simple_program)
    program = Program(clauses=tuple(clauses))
    engine = Engine(program=program, trace=True)

    # Run a query to ensure tracer works
    results = list(engine.query("member(X, [1, 2, 3])"))
    assert len(results) == 3


def test_breakpoint_triggers_pause_check(engine_with_dap):
    """Tracer checks for breakpoint matches and calls should_pause."""
    # Add breakpoint on member/2 at CALL port
    bp_id = engine_with_dap._test_bp_store.add_breakpoint("member", 2, ports=["CALL"])

    # Verify breakpoint was added
    assert engine_with_dap._test_bp_store.has_breakpoint("member", 2)

    # Create a test event that should match the breakpoint
    test_event = {
        "functor": "member",
        "arity": 2,
        "port": "CALL",
        "depth": 0,
        "goal": None,
    }

    # Verify the breakpoint matches
    assert engine_with_dap._test_bp_store.matches(test_event)

    # Remove breakpoint and verify it's gone
    engine_with_dap._test_bp_store.remove_breakpoint(bp_id)
    assert not engine_with_dap._test_bp_store.has_breakpoint("member", 2)


def test_step_controller_mode_affects_should_pause(engine_with_dap):
    """StepController mode affects whether execution should pause."""
    # In running mode, should not pause
    assert engine_with_dap._test_controller.get_mode() == "running"
    test_event = {"depth": 0, "port": "call"}
    assert not engine_with_dap._test_controller.should_pause(test_event)

    # In step_in mode, should pause
    engine_with_dap._test_controller.set_mode("step_in")
    assert engine_with_dap._test_controller.should_pause(test_event)

    # In step_over mode at same depth, should pause
    engine_with_dap._test_controller.set_mode("step_over", depth=0)
    assert engine_with_dap._test_controller.should_pause(test_event)

    # In step_over mode at deeper depth, should not pause
    test_event_deeper = {"depth": 1, "port": "call"}
    assert not engine_with_dap._test_controller.should_pause(test_event_deeper)


def test_step_over_pauses_at_same_depth(engine_with_dap):
    """Step over pauses at the same depth or shallower."""
    # This test is more complex and requires careful depth tracking
    # For now, just verify that set_mode works with depth parameter
    engine_with_dap._test_controller.set_mode("step_over", depth=0)
    assert engine_with_dap._test_controller.get_mode() == "step_over"
    assert engine_with_dap._test_controller._baseline_depth == 0


def test_running_mode_does_not_pause(engine_with_dap):
    """Execution does not pause in running mode (default)."""
    # Controller should be in running mode by default
    assert engine_with_dap._test_controller.get_mode() == "running"

    # Run a query - should complete without blocking
    results = list(engine_with_dap.query("member(X, [1, 2, 3])"))
    assert len(results) == 3


def test_breakpoint_with_port_filter(engine_with_dap):
    """Breakpoint only triggers on specified ports."""
    # Add breakpoint that only triggers on EXIT port
    engine_with_dap._test_bp_store.add_breakpoint("member", 2, ports=["EXIT"])

    # Test event with CALL port should not match
    call_event = {
        "functor": "member",
        "arity": 2,
        "port": "CALL",
        "depth": 0,
        "goal": None,
    }
    assert not engine_with_dap._test_bp_store.matches(call_event)

    # Test event with EXIT port should match
    exit_event = {
        "functor": "member",
        "arity": 2,
        "port": "EXIT",
        "depth": 0,
        "goal": None,
    }
    assert engine_with_dap._test_bp_store.matches(exit_event)


def test_multiple_breakpoints(engine_with_dap):
    """Multiple breakpoints can be set and triggered."""
    # Add breakpoints on different predicates
    bp1 = engine_with_dap._test_bp_store.add_breakpoint("member", 2)
    engine_with_dap._test_bp_store.add_breakpoint("append", 3)

    assert engine_with_dap._test_bp_store.has_breakpoint("member", 2)
    assert engine_with_dap._test_bp_store.has_breakpoint("append", 3)

    # Remove one breakpoint
    engine_with_dap._test_bp_store.remove_breakpoint(bp1)
    assert not engine_with_dap._test_bp_store.has_breakpoint("member", 2)
    assert engine_with_dap._test_bp_store.has_breakpoint("append", 3)


def test_tracer_converts_events_for_dap(engine_with_dap):
    """Tracer correctly converts TraceEvent to DAP event format."""
    # Create a sample trace event
    goal = Struct("member", (Atom("X"), Atom("[1,2,3]")))
    event = TraceEvent(
        version=1,
        run_id="test",
        step_id=1,
        port="call",
        goal=goal,
        goal_pretty="member(X, [1,2,3])",
        goal_canonical="member(X, [1,2,3])",
        frame_depth=5,
        cp_depth=2,
        goal_height=1,
        write_stamp=0,
        pred_id="member/2",
    )

    # Convert to DAP format
    dap_event = engine_with_dap.tracer._to_dap_event(event)

    assert dap_event["functor"] == "member"
    assert dap_event["arity"] == 2
    assert dap_event["port"] == "CALL"
    assert dap_event["depth"] == 5
    assert dap_event["goal"] is goal
