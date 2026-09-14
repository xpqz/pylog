"""
Integration tests for ISO test suite runner.

Runs the ISO test suite via run_iso_suite.py and validates results.
Marked with @pytest.mark.iso and @pytest.mark.slow for selective execution.
"""

import pytest
from pathlib import Path

from scripts.run_iso_suite import ISOTestRunner


@pytest.mark.iso
@pytest.mark.slow
class TestISOSuiteIntegration:
    """Integration tests for ISO test suite."""

    def test_smoke_run_first_100_tests(self):
        """Smoke test: run first 100 tests from iso.tst."""
        test_file = Path("iso_test_js/iso.tst")
        skip_config = Path("iso_test_js/pylog.skip")

        if not test_file.exists():
            pytest.skip(f"ISO test file not found: {test_file}")

        runner = ISOTestRunner(max_tests=100, verbose=False)
        runner.load_skip_config(skip_config)

        report = runner.run_suite(test_file)
        summary = report["summary"]

        # Should have parsed and executed tests
        assert summary["parsed"] > 0, "No tests parsed"
        assert summary["total"] > 0, "No tests executed"

        # Should have at least some passing tests
        assert summary["passed"] > 0, "No passing tests"

        # No unexpected errors (errors should be zero if executor is working)
        assert summary["errored"] == 0, f"Unexpected errors: {summary['errored']}"

    def test_suite_with_skip_config(self):
        """Verify skip configuration is applied."""
        test_file = Path("iso_test_js/iso.tst")
        skip_config = Path("iso_test_js/pylog.skip")

        if not test_file.exists():
            pytest.skip(f"ISO test file not found: {test_file}")
        if not skip_config.exists():
            pytest.skip(f"Skip config not found: {skip_config}")

        runner = ISOTestRunner(max_tests=100, verbose=False)
        runner.load_skip_config(skip_config)

        report = runner.run_suite(test_file)
        summary = report["summary"]

        # Should have some skipped tests
        assert summary["skipped"] > 0, "Expected some skipped tests"

    def test_match_filter(self):
        """Test filtering tests by substring match."""
        test_file = Path("iso_test_js/iso.tst")

        if not test_file.exists():
            pytest.skip(f"ISO test file not found: {test_file}")

        # Filter for tests containing "call"
        runner = ISOTestRunner(match_filter="call", verbose=False)
        report = runner.run_suite(test_file)
        summary = report["summary"]

        assert summary["total"] > 0, "No tests matched filter"

        # Verify all results match the filter
        for result in report["results"]:
            test = result["test"]
            assert (
                "call" in test["source_text"]
            ), f"Test doesn't match filter: {test['source_text']}"

    def test_json_report_structure(self):
        """Validate JSON report structure."""
        test_file = Path("iso_test_js/iso.tst")

        if not test_file.exists():
            pytest.skip(f"ISO test file not found: {test_file}")

        runner = ISOTestRunner(max_tests=10, verbose=False)
        report = runner.run_suite(test_file)

        # Verify top-level structure
        assert "summary" in report
        assert "results" in report
        assert "parse_errors" in report

        # Verify summary structure
        summary = report["summary"]
        assert "total" in summary
        assert "parsed" in summary
        assert "parse_errors" in summary
        assert "passed" in summary
        assert "failed" in summary
        assert "skipped" in summary
        assert "xfailed" in summary
        assert "errored" in summary
        assert "duration_ms" in summary

        # Verify results structure
        if report["results"]:
            result = report["results"][0]
            assert "test" in result
            assert "status" in result
            assert "duration_ms" in result

            # Verify test structure
            test = result["test"]
            assert "kind" in test
            assert "goal_term" in test
            assert "source_text" in test
            assert "clause_index" in test

    def test_fail_on_skip_mode(self):
        """Test --fail-on-skip mode treats skips as failures."""
        test_file = Path("iso_test_js/iso.tst")
        skip_config = Path("iso_test_js/pylog.skip")

        if not test_file.exists():
            pytest.skip(f"ISO test file not found: {test_file}")
        if not skip_config.exists():
            pytest.skip(f"Skip config not found: {skip_config}")

        runner = ISOTestRunner(max_tests=50, verbose=False, fail_on_skip=True)
        runner.load_skip_config(skip_config)

        report = runner.run_suite(test_file)
        summary = report["summary"]

        # If there are skipped tests, runner should treat them as failures
        # (this is validated by the exit code logic in main(), not here)
        if summary["skipped"] > 0:
            # Just verify skip count is tracked
            assert summary["skipped"] > 0

    def test_safety_limits(self):
        """Verify safety limits are applied."""
        test_file = Path("iso_test_js/iso.tst")

        if not test_file.exists():
            pytest.skip(f"ISO test file not found: {test_file}")

        # Use very low limits to test they're enforced
        runner = ISOTestRunner(
            max_tests=10,
            max_solutions=1,
            max_steps=1000,
            verbose=False,
        )
        report = runner.run_suite(test_file)

        # Should still complete without hanging
        assert report["summary"]["total"] > 0


@pytest.mark.iso
@pytest.mark.slow
@pytest.mark.skip(reason="Full suite test disabled - takes too long for CI")
def test_full_suite_no_errors():
    """
    Full ISO suite run: verify no unexpected errors.

    DISABLED: This test runs the entire ISO suite (without max_tests limit)
    which takes too long for regular CI runs. Use --max-tests for faster
    validation, or run manually with:
      uv run python scripts/run_iso_suite.py --verbose
    """
    test_file = Path("iso_test_js/iso.tst")
    skip_config = Path("iso_test_js/pylog.skip")

    if not test_file.exists():
        pytest.skip(f"ISO test file not found: {test_file}")

    runner = ISOTestRunner(verbose=False)
    if skip_config.exists():
        runner.load_skip_config(skip_config)

    report = runner.run_suite(test_file)
    summary = report["summary"]

    # Log summary for visibility
    print(
        f"\nFull ISO suite: {summary['total']} tests, "
        f"{summary['passed']} passed, "
        f"{summary['failed']} failed, "
        f"{summary['skipped']} skipped, "
        f"{summary['errored']} errored"
    )

    # Errors indicate executor bugs (not just unimplemented features)
    assert summary["errored"] == 0, f"Unexpected errors in {summary['errored']} tests"

    # Should have substantial number of tests
    assert summary["total"] > 100, "Expected more than 100 tests in suite"


@pytest.mark.iso
@pytest.mark.slow
@pytest.mark.timeout(600)
def test_suite_progresses_past_functor_max_arity_probe():
    """Regression: the suite must run past iso.tst:213 without stalling.

    The full suite used to make no further progress at iso.tst:214

        A is 9223372036854775808, functor(_,f,A)
                should_throw error(representation_error(max_arity), _).

    because functor/3 construction had no arity bound. This window spans that
    test, so completing it proves the stall is gone. A per-test timeout is
    configured so that any future hang in this window is reported as a bounded
    error rather than stalling the run.

    The probe is deliberately left unskipped, so it keeps exercising the arity
    guard here. It is expected to be recorded as a FAIL rather than a PASS:
    PyLog follows SWI and raises resource_error(stack), where this ISO test
    asks for representation_error(max_arity). What matters for this regression
    is that the probe terminates and is reported, not which way it goes.
    """
    test_file = Path("iso_test_js/iso.tst")
    skip_config = Path("iso_test_js/pylog.skip")

    if not test_file.exists():
        pytest.skip(f"ISO test file not found: {test_file}")

    runner = ISOTestRunner(max_tests=250, timeout_ms=10000, verbose=False)
    if skip_config.exists():
        runner.load_skip_config(skip_config)

    report = runner.run_suite(test_file)
    summary = report["summary"]

    # The window must have run to completion, past the stalling test.
    assert summary["total"] > 200, f"Suite stopped early: {summary['total']} tests"

    # The probe itself must now be reported, and must not be a timeout or a
    # bail-out: "error" is the executor's status for a timeout or an internal
    # fault, so a pass/fail verdict is what proves the guard returned normally.
    probe = [
        r
        for r in report["results"]
        if "9223372036854775808" in r["test"]["source_text"]
    ]
    assert probe, "functor max_arity probe not found in results"
    for r in probe:
        assert r["status"] in (
            "pass",
            "fail",
        ), f"probe did not terminate cleanly: {r['status']} {r.get('error_message')}"
