"""Tests for ISO test runner safety limits.

The runner documents two per-test safety controls in docs/ISO_TESTING_USAGE.md,
`--timeout` and `--max-steps`. Both must actually bound execution: a single
non-terminating test must be recorded and the run must continue, rather than
hanging the whole suite.

Both are enforced inside the engine's goal loop, between steps, and neither
can interrupt a single long-running step. That is deliberate: enforcing the
timeout by preemption instead - a signal handler raising into whatever happens
to be executing - was tried and is unsafe, because the exception can surface
while unrelated code holds a lock and leave it held, wedging every later test.
A runaway loop inside a builtin therefore has to be fixed at source, as the
functor/3 arity guard does, rather than contained here.

They still catch different things and are tested separately: the step budget
bounds work, the timeout bounds elapsed time, and a goal can exhaust either
one without the other.
"""

import time

import pytest

from prolog.ast.terms import Atom, Int, Struct, Var
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_rule, program
from scripts.iso_test_executor import ExecutionStatus, ISOTestExecutor


def looping_program():
    """Clauses defining `loop :- loop.` - non-terminating in the goal loop."""
    return [mk_rule("loop", (), Atom("loop"))]


class TestExecutorTimeout:
    """Per-test wall-clock timeout enforcement."""

    @pytest.mark.timeout(60)
    def test_timeout_aborts_non_terminating_goal(self):
        """A goal that never terminates must be cut off by timeout_ms."""
        executor = ISOTestExecutor(timeout_ms=500, max_steps=None)

        start = time.time()
        result = executor.run_should_fail(Atom("loop"), program=looping_program())
        elapsed_ms = (time.time() - start) * 1000

        assert result.status is not ExecutionStatus.PASS
        assert elapsed_ms < 30000, f"timeout not enforced, took {elapsed_ms:.0f}ms"

    @pytest.mark.timeout(60)
    def test_timeout_result_identifies_itself_as_a_timeout(self):
        """The recorded result must say it timed out, not merely that it errored."""
        executor = ISOTestExecutor(timeout_ms=500, max_steps=None)

        result = executor.run_should_fail(Atom("loop"), program=looping_program())

        assert result.error_message is not None
        assert "timeout" in result.error_message.lower()

    @pytest.mark.timeout(60)
    def test_timeout_leaves_the_process_usable(self):
        """A fired timeout must not disturb unrelated work afterwards.

        Regression test. The first implementation enforced the timeout with a
        SIGALRM handler that raised into whatever was running. Under coverage
        that exception surfaced inside the tracer while it held its data lock,
        so the lock stayed held and every later engine run blocked on it. A
        plain query after a timeout is the cheapest way to catch that class of
        breakage.
        """
        executor = ISOTestExecutor(timeout_ms=500, max_steps=None)

        executor.run_should_fail(Atom("loop"), program=looping_program())

        # Fresh executor, no budget at all: must behave completely normally.
        clean = ISOTestExecutor(timeout_ms=None, max_steps=None)
        assert clean.run_should_fail(Atom("fail")).status is ExecutionStatus.PASS
        assert clean.run_should_fail(Atom("true")).status is ExecutionStatus.FAIL

    @pytest.mark.timeout(60)
    def test_timeout_applies_to_should_throw(self):
        """should_throw tests must be bounded too."""
        executor = ISOTestExecutor(timeout_ms=500, max_steps=None)

        start = time.time()
        result = executor.run_should_throw(
            Atom("loop"), Atom("anything"), program=looping_program()
        )
        elapsed_ms = (time.time() - start) * 1000

        assert result.status is not ExecutionStatus.PASS
        assert elapsed_ms < 30000, f"timeout not enforced, took {elapsed_ms:.0f}ms"

    @pytest.mark.timeout(60)
    def test_runner_continues_after_a_timeout(self):
        """One timed-out test must not abort the remaining tests."""
        executor = ISOTestExecutor(timeout_ms=500, max_steps=None)

        timed_out = executor.run_should_fail(Atom("loop"), program=looping_program())
        assert timed_out.status is not ExecutionStatus.PASS

        # A subsequent, well-behaved test must still execute normally.
        after = executor.run_should_fail(Atom("fail"))
        assert after.status is ExecutionStatus.PASS

    @pytest.mark.timeout(60)
    def test_no_timeout_configured_leaves_terminating_tests_alone(self):
        """timeout_ms=None must not interfere with normal execution."""
        executor = ISOTestExecutor(timeout_ms=None)

        result = executor.run_should_fail(Atom("fail"))

        assert result.status is ExecutionStatus.PASS

    @pytest.mark.timeout(60)
    def test_timeout_does_not_fire_for_fast_tests(self):
        """A test well inside the budget must pass, not report a timeout."""
        executor = ISOTestExecutor(timeout_ms=30000)

        result = executor.run_should_fail(Atom("fail"))

        assert result.status is ExecutionStatus.PASS
        assert result.error_message is None


class TestExecutorMaxSteps:
    """Per-test step budget enforcement."""

    @pytest.mark.timeout(60)
    def test_max_steps_reaches_the_engine(self):
        """A small step budget must bound a non-terminating goal loop."""
        executor = ISOTestExecutor(max_steps=10000, timeout_ms=None)

        start = time.time()
        executor.run_should_fail(Atom("loop"), program=looping_program())
        elapsed_ms = (time.time() - start) * 1000

        assert elapsed_ms < 30000, f"max_steps not enforced, took {elapsed_ms:.0f}ms"

    @pytest.mark.timeout(60)
    def test_step_exhaustion_is_not_reported_as_a_conformance_pass(self):
        """Budget exhaustion must not masquerade as a genuine result.

        The engine stops stepping when the budget runs out, leaving zero
        solutions behind. For a should_fail test that is indistinguishable from
        a true failure, so exhaustion would be recorded as PASS - a false
        conformance pass. The runner must report exhaustion explicitly instead.
        """
        executor = ISOTestExecutor(max_steps=10000, timeout_ms=None)

        result = executor.run_should_fail(Atom("loop"), program=looping_program())

        assert result.status is not ExecutionStatus.PASS
        assert result.error_message is not None
        assert "step" in result.error_message.lower()

    @pytest.mark.timeout(60)
    def test_generous_max_steps_does_not_disturb_normal_tests(self):
        """The default-sized budget must leave ordinary tests passing."""
        executor = ISOTestExecutor(max_steps=1000000, timeout_ms=None)

        result = executor.run_should_fail(Atom("fail"))

        assert result.status is ExecutionStatus.PASS
        assert result.error_message is None

    @pytest.mark.timeout(60)
    def test_terminating_test_within_budget_is_unaffected(self):
        """A test that finishes inside its budget must report its real result."""
        executor = ISOTestExecutor(max_steps=1000000, timeout_ms=None)

        result = executor.run_should_fail(Atom("true"))

        # true/0 succeeds, so a should_fail test on it is a genuine FAIL.
        assert result.status is ExecutionStatus.FAIL
        assert result.error_message is None


class TestEngineBudgetContract:
    """Contract the executor relies on when it hands budgets to the engine."""

    @pytest.mark.timeout(30)
    def test_max_steps_assigned_after_construction_is_honoured(self):
        """Assigning engine.max_steps post-construction must take effect.

        Regression test. An earlier version cached "is any budget set?" in
        __init__ to keep the goal loop cheap, which silently ignored later
        assignment to max_steps. Callers do exactly that: the REPL's timeout
        protection sets it per query, as do several library tests.
        """
        engine = Engine(program(mk_rule("loop", (), Atom("loop"))))
        assert engine.max_steps is None

        engine.max_steps = 100
        solutions = engine.run([Atom("loop")])

        assert solutions == []
        assert engine.steps_exhausted is True

    @pytest.mark.timeout(30)
    def test_max_time_ms_assigned_after_construction_is_honoured(self):
        """The wall-clock budget must be live in the same way."""
        engine = Engine(program(mk_rule("loop", (), Atom("loop"))))

        engine.max_time_ms = 300
        start = time.time()
        solutions = engine.run([Atom("loop")])
        elapsed_ms = (time.time() - start) * 1000

        assert solutions == []
        assert engine.time_exhausted is True
        assert elapsed_ms < 20000, f"deadline not applied, took {elapsed_ms:.0f}ms"

    @pytest.mark.timeout(30)
    def test_reset_clears_the_deadline(self):
        """reset() must not leave a stale deadline behind.

        run() recomputes the deadline, so a stale value cannot currently leak
        into a later run. The invariant is asserted anyway because reset()
        clears every other budget field, and an inconsistent reset is the kind
        of thing a future caller reads as intentional.
        """
        engine = Engine(program(mk_rule("loop", (), Atom("loop"))), max_time_ms=300)
        engine.run([Atom("loop")])
        assert engine.time_exhausted is True

        engine.reset()

        assert engine._deadline is None
        assert engine.time_exhausted is False
        assert engine.steps_exhausted is False

    @pytest.mark.timeout(30)
    def test_exhaustion_flags_are_cleared_between_runs(self):
        """A later, well-behaved run must not inherit an earlier verdict."""
        engine = Engine(program(mk_rule("loop", (), Atom("loop"))), max_steps=100)
        engine.run([Atom("loop")])
        assert engine.steps_exhausted is True

        engine.max_steps = 1000000
        engine.run([Atom("true")])

        assert engine.steps_exhausted is False
        assert engine.time_exhausted is False


class TestFunctorArityGuardUnderRunner:
    """The iso.tst:214 probe must be bounded even with no timeout configured."""

    @pytest.mark.timeout(60)
    def test_huge_arity_functor_does_not_hang_executor(self):
        """functor(_,f,2^63) must resolve promptly, without relying on a timeout.

        This is the test that stalled the full suite. The arity guard in
        functor/3 is what fixes it; the timeout is only a backstop.

        The expected ball is SWI's error(resource_error(stack), _), not the
        representation_error(max_arity) that iso.tst:214 asks for. Running the
        probe through the executor with that SWI expectation proves both that
        the guard fires and that the executor reports it as a normal throw.
        """
        executor = ISOTestExecutor(timeout_ms=None, max_steps=None)

        # The literal 2^63 arity the ISO suite probes with.
        goal = Struct("functor", (Var(0, "X"), Atom("f"), Int(9223372036854775808)))

        start = time.time()
        result = executor.run_should_throw(
            goal,
            Struct(
                "error",
                (Struct("resource_error", (Atom("stack"),)), Var(1, "_")),
            ),
        )
        elapsed_ms = (time.time() - start) * 1000

        assert (
            elapsed_ms < 30000
        ), f"functor/3 arity guard missing, took {elapsed_ms:.0f}ms"
        assert result.status is ExecutionStatus.PASS
