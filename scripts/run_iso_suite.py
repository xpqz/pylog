#!/usr/bin/env python3
"""
ISO Prolog test suite runner - Phase A.

Executes ISO test patterns from iso_test_js/iso.tst using Python-side
parser and executor. Supports CLI with reporting and skip/XFAIL configuration.
"""

import sys
import json
import argparse
from pathlib import Path
from typing import List, Dict, Any, Optional
import time

from scripts.iso_test_parser import (
    ClauseScanner,
    PatternDetector,
    ISOTestCase,
    ISOTestKind,
)
from scripts.iso_test_executor import ISOTestExecutor, ExecutionStatus
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.parser.parser import parse_query
import io
import contextlib


def _test_to_dict(test: ISOTestCase) -> Dict[str, Any]:
    """Convert ISOTestCase to JSON-serializable dict."""
    return {
        "kind": test.kind.value,
        "goal_term": str(test.goal_term),
        "check_term": str(test.check_term) if test.check_term else None,
        "exception_term": str(test.exception_term) if test.exception_term else None,
        "skipped": test.skipped,
        "clause_index": test.clause_index,
        "line_number": test.line_number,
        "source_text": test.source_text,
    }


def parse_harness_output(output: str) -> Dict[str, Any]:
    """
    Parse output from harness.pl test execution.

    Harness output format:
        Test N/Pos: OK
        Test N/Pos: expected X, got Y
        ----- Finished tests from file ...
        N tests found.
        M tests succeeded.
        K tests failed.
        J tests skipped.

    Args:
        output: Raw output string from harness execution

    Returns:
        Dictionary with parsed results and summary
    """
    lines = output.strip().split("\n")
    results = []
    summary = {"total": 0, "passed": 0, "failed": 0, "skipped": 0}

    for line in lines:
        line = line.strip()

        # Parse test result lines
        if line.startswith("Test ") and ": OK" in line:
            summary["passed"] += 1
            summary["total"] += 1

        elif line.startswith("Test ") and ": expected" in line:
            summary["failed"] += 1
            summary["total"] += 1

        elif line.startswith("Test ") and ": skipped" in line:
            summary["skipped"] += 1
            summary["total"] += 1

        # Parse summary lines
        elif "tests found" in line:
            parts = line.split()
            if parts and parts[0].isdigit():
                summary["total"] = int(parts[0])

        elif "tests succeeded" in line:
            parts = line.split()
            if parts and parts[0].isdigit():
                summary["passed"] = int(parts[0])

        elif "tests failed" in line:
            parts = line.split()
            if parts and parts[0].isdigit():
                summary["failed"] = int(parts[0])

        elif "tests skipped" in line:
            parts = line.split()
            if parts and parts[0].isdigit():
                summary["skipped"] = int(parts[0])

    return {"summary": summary, "results": results}


def compare_modes(
    python_results: Dict[str, Any], harness_results: Dict[str, Any]
) -> Dict[str, Any]:
    """
    Compare results from Python and harness modes.

    Args:
        python_results: Results from Python runner
        harness_results: Results from harness runner

    Returns:
        Dictionary with comparison results and differences
    """
    differences = []

    python_summary = python_results.get("summary", {})
    harness_summary = harness_results.get("summary", {})

    # Compare summaries
    summary_diff = {
        "python_passed": python_summary.get("passed", 0),
        "harness_passed": harness_summary.get("passed", 0),
        "python_failed": python_summary.get("failed", 0),
        "harness_failed": harness_summary.get("failed", 0),
        "python_skipped": python_summary.get("skipped", 0),
        "harness_skipped": harness_summary.get("skipped", 0),
    }

    return {
        "python_summary": python_summary,
        "harness_summary": harness_summary,
        "summary_diff": summary_diff,
        "differences": differences,
    }


class ISOTestRunner:
    """
    Runner for ISO test suite.

    Coordinates parsing of iso.tst file, execution of tests, and reporting.
    """

    def __init__(
        self,
        max_tests: Optional[int] = None,
        max_solutions: int = 10000,
        max_steps: int = 1000000,
        timeout_ms: Optional[int] = None,
        fail_on_skip: bool = False,
        match_filter: Optional[str] = None,
        verbose: bool = False,
        mode: str = "python",
    ):
        """
        Initialize runner with configuration.

        Args:
            max_tests: Maximum number of tests to run (for smoke testing)
            max_solutions: Maximum solutions per query
            max_steps: Maximum steps per query
            timeout_ms: Optional timeout in milliseconds
            fail_on_skip: Treat skipped tests as failures
            match_filter: Run only tests matching this substring
            verbose: Enable verbose output
            mode: Execution mode - "python" (default), "harness", or "compare"
        """
        self.max_tests = max_tests
        self.executor = ISOTestExecutor(
            max_solutions=max_solutions,
            max_steps=max_steps,
            timeout_ms=timeout_ms,
        )
        self.fail_on_skip = fail_on_skip
        self.match_filter = match_filter
        self.verbose = verbose
        self.mode = mode
        self.skip_patterns: Dict[str, str] = {}  # pattern -> reason

    def load_skip_config(self, skip_file: Path) -> None:
        """
        Load skip/XFAIL configuration from file.

        File format: text patterns to match against source_text, one per line.
        Lines starting with # are comments.

        Args:
            skip_file: Path to skip configuration file
        """
        if not skip_file.exists():
            return

        for line in skip_file.read_text().splitlines():
            line = line.strip()
            # Skip empty lines and comments
            if not line or line.startswith("#"):
                continue

            # Format: pattern # reason
            if "#" in line:
                pattern, reason = line.split("#", 1)
                pattern = pattern.strip()
                reason = reason.strip()
            else:
                pattern = line
                reason = "no reason given"

            self.skip_patterns[pattern] = reason

    def should_skip(self, test: ISOTestCase) -> Optional[str]:
        """
        Check if test should be skipped based on configuration.

        Args:
            test: Test case to check

        Returns:
            Skip reason if test should be skipped, None otherwise
        """
        # Check if already marked with fixme
        if test.skipped:
            return "marked fixme in source"

        # Check skip patterns
        for pattern, reason in self.skip_patterns.items():
            if pattern in test.source_text:
                return reason

        return None

    def run_suite(self, test_file: Path) -> Dict[str, Any]:
        """
        Run ISO test suite from file.

        Args:
            test_file: Path to iso.tst file

        Returns:
            Dictionary with test results and summary
        """
        start_time = time.time()

        # Parse test file
        content = test_file.read_text()
        scanner = ClauseScanner(content)
        clauses = scanner.scan_clauses()

        # Parse each clause into test case
        test_cases: List[ISOTestCase] = []
        parse_errors = []

        for clause_text, line_number in clauses:
            test_case = PatternDetector.detect_pattern(
                clause_text, line_number, len(test_cases)
            )
            if test_case:
                test_cases.append(test_case)
            else:
                parse_errors.append(
                    {"line": line_number, "text": clause_text, "reason": "no pattern"}
                )

        # Apply max_tests limit
        if self.max_tests:
            test_cases = test_cases[: self.max_tests]

        # Apply match filter
        if self.match_filter:
            test_cases = [t for t in test_cases if self.match_filter in t.source_text]

        # Execute tests
        results = []
        counts = {
            "pass": 0,
            "fail": 0,
            "skip": 0,
            "xfail": 0,
            "error": 0,
        }

        for test in test_cases:
            # Check if should skip
            skip_reason = self.should_skip(test)
            if skip_reason:
                results.append(
                    {
                        "test": _test_to_dict(test),
                        "status": "skip",
                        "duration_ms": 0.0,
                        "skip_reason": skip_reason,
                    }
                )
                counts["skip"] += 1
                if self.verbose:
                    print(
                        f"SKIP [{test.clause_index}:{test.line_number}] {test.source_text[:60]}... ({skip_reason})"
                    )
                continue

            # Execute test
            exec_result = self._execute_test(test)
            result_dict = {
                "test": _test_to_dict(test),
                "status": exec_result.status.value,
                "duration_ms": exec_result.duration_ms,
                "expected": exec_result.expected,
                "actual": exec_result.actual,
                "error_message": exec_result.error_message,
            }
            results.append(result_dict)
            counts[exec_result.status.value] += 1

            if self.verbose or exec_result.status in (
                ExecutionStatus.FAIL,
                ExecutionStatus.ERROR,
            ):
                status_str = exec_result.status.value.upper()
                print(
                    f"{status_str} [{test.clause_index}:{test.line_number}] {test.source_text[:60]}..."
                )
                if exec_result.status == ExecutionStatus.FAIL:
                    print(f"  Expected: {exec_result.expected}")
                    print(f"  Actual:   {exec_result.actual}")
                if exec_result.error_message:
                    print(f"  Error: {exec_result.error_message}")

        duration_sec = time.time() - start_time

        # Build summary
        total = len(results)
        summary = {
            "total": total,
            "parsed": len(test_cases),
            "parse_errors": len(parse_errors),
            "passed": counts["pass"],
            "failed": counts["fail"],
            "skipped": counts["skip"],
            "xfailed": counts["xfail"],
            "errored": counts["error"],
            "duration_ms": duration_sec * 1000,
        }

        return {
            "summary": summary,
            "results": results,
            "parse_errors": parse_errors,
        }

    def run_harness_mode(self, test_file: Path) -> Dict[str, Any]:
        """
        Run ISO test suite using harness.pl natively in PyLog.

        Args:
            test_file: Path to iso.tst file

        Returns:
            Dictionary with test results and summary
        """
        start_time = time.time()

        # Initialize PyLog engine in ISO mode
        engine = Engine(Program(tuple()), mode="iso")

        # Consult harness.pl
        harness_path = Path("iso_test_js/harness.pl")
        if not harness_path.exists():
            # Gracefully report missing harness
            return {
                "summary": {
                    "total": 0,
                    "passed": 0,
                    "failed": 0,
                    "skipped": 0,
                    "errored": 1,
                    "duration_ms": (time.time() - start_time) * 1000,
                },
                "results": [],
                "error": f"harness.pl not found at {harness_path}",
            }

        harness_text = harness_path.read_text()

        # Directives (:- op(...)) aren’t supported yet; catch parse errors
        try:
            engine.consult_string(harness_text)
        except Exception as e:
            return {
                "summary": {
                    "total": 0,
                    "passed": 0,
                    "failed": 0,
                    "skipped": 0,
                    "errored": 1,
                    "duration_ms": (time.time() - start_time) * 1000,
                },
                "results": [],
                "error": f"Failed to consult harness.pl: {e}",
            }

        # Consult auxiliaries.pl if exists
        aux_path = Path("iso_test_js/auxiliaries.pl")
        if aux_path.exists():
            aux_text = aux_path.read_text()
            try:
                engine.consult_string(aux_text)
            except Exception as e:
                return {
                    "summary": {
                        "total": 0,
                        "passed": 0,
                        "failed": 0,
                        "skipped": 0,
                        "errored": 1,
                        "duration_ms": (time.time() - start_time) * 1000,
                    },
                    "results": [],
                    "error": f"Failed to consult auxiliaries.pl: {e}",
                }

        # Capture output from harness execution
        output_buffer = io.StringIO()

        # Execute test predicate: test('iso.tst')
        # Note: This requires ISO I/O support in the engine
        # For now, we'll use a simplified approach
        test_goal_text = f"test('{test_file}')"
        test_goal = parse_query(test_goal_text)

        # Try to execute the query and capture output
        # This is a simplified implementation - full ISO I/O may be needed
        try:
            with contextlib.redirect_stdout(output_buffer):
                _ = list(engine.query(test_goal, max_solutions=1))

            output = output_buffer.getvalue()

            # Parse harness output
            results = parse_harness_output(output)

            # Add execution time
            duration_sec = time.time() - start_time
            results["summary"]["duration_ms"] = duration_sec * 1000

            return results

        except Exception as e:
            # If harness execution fails, return error results
            return {
                "summary": {
                    "total": 0,
                    "passed": 0,
                    "failed": 0,
                    "skipped": 0,
                    "errored": 1,
                    "duration_ms": (time.time() - start_time) * 1000,
                },
                "results": [],
                "error": str(e),
            }

    def _execute_test(self, test: ISOTestCase):
        """Execute a single test case based on its kind."""
        if test.kind == ISOTestKind.SHOULD_FAIL:
            return self.executor.run_should_fail(test.goal_term)

        elif test.kind == ISOTestKind.SHOULD_GIVE:
            # Extract goal and check text from source
            # We need to split the source at should_give
            parts = test.source_text.split("should_give", 1)
            if len(parts) != 2:
                raise ValueError(f"Cannot split should_give: {test.source_text}")

            goal_text = parts[0].strip()
            check_text = parts[1].strip()

            # Remove trailing dot from check if present
            if check_text.endswith("."):
                check_text = check_text[:-1].strip()

            return self.executor.run_should_give(goal_text, check_text)

        elif test.kind == ISOTestKind.SHOULD_THROW:
            return self.executor.run_should_throw(test.goal_term, test.exception_term)

        else:
            raise ValueError(f"Unknown test kind: {test.kind}")


def print_summary(report: Dict[str, Any]) -> None:
    """Print human-readable summary."""
    summary = report.get("summary", {})

    print()
    print("=" * 70)

    # Check if this is a comparison report
    if report.get("mode") == "compare":
        print("ISO TEST SUITE COMPARISON")
        print("=" * 70)
        print("Python Mode:")
        print(
            f"  {summary.get('python_passed', 0)} passed | "
            f"{summary.get('python_failed', 0)} failed | "
            f"{summary.get('python_skipped', 0)} skipped"
        )
        print("Harness Mode:")
        print(
            f"  {summary.get('harness_passed', 0)} passed | "
            f"{summary.get('harness_failed', 0)} failed | "
            f"{summary.get('harness_skipped', 0)} skipped"
        )
    else:
        print("ISO TEST SUITE SUMMARY")
        print("=" * 70)
        print(
            f"{summary.get('total', 0)} tests | "
            f"{summary.get('passed', 0)} passed | "
            f"{summary.get('failed', 0)} failed | "
            f"{summary.get('skipped', 0)} skipped | "
            f"{summary.get('xfailed', 0)} xfailed | "
            f"{summary.get('errored', 0)} errored | "
            f"{summary.get('duration_ms', 0):.0f} ms"
        )

        if summary.get("parse_errors"):
            print(f"\nParse errors: {summary['parse_errors']}")

        # Show first N failures
        results = report.get("results", [])
        failures = [r for r in results if r.get("status") in ("fail", "error")]
        if failures:
            print(f"\n{len(failures)} FAILURES/ERRORS:")
            for i, result in enumerate(failures[:10], 1):  # Show first 10
                test = result.get("test", {})
                print(
                    f"\n{i}. [{test.get('clause_index', '?')}:{test.get('line_number', '?')}] "
                    f"{test.get('source_text', '')[:70]}"
                )
                if "expected" in result:
                    print(f"   Expected: {result['expected']}")
                if "actual" in result:
                    print(f"   Actual:   {result['actual']}")
                if result.get("error_message"):
                    print(f"   Error: {result['error_message'][:100]}")

            if len(failures) > 10:
                print(f"\n... and {len(failures) - 10} more failures")

    print("=" * 70)


def main():
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Run ISO Prolog test suite (Phase A Python runner)"
    )

    parser.add_argument(
        "test_file",
        nargs="?",
        type=Path,
        default=Path("iso_test_js/iso.tst"),
        help="Path to ISO test file (default: iso_test_js/iso.tst)",
    )

    parser.add_argument(
        "--json",
        action="store_true",
        help="Output results as JSON",
    )

    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Verbose output (show all tests)",
    )

    parser.add_argument(
        "--max-tests",
        type=int,
        help="Maximum number of tests to run (for smoke testing)",
    )

    parser.add_argument(
        "--match",
        type=str,
        help="Run only tests matching this substring",
    )

    parser.add_argument(
        "--fail-on-skip",
        action="store_true",
        help="Treat skipped tests as failures",
    )

    parser.add_argument(
        "--max-solutions",
        type=int,
        default=10000,
        help="Maximum solutions per query (default: 10000)",
    )

    parser.add_argument(
        "--max-steps",
        type=int,
        default=1000000,
        help="Maximum steps per query (default: 1000000)",
    )

    parser.add_argument(
        "--timeout",
        type=int,
        help="Timeout in milliseconds per test",
    )

    parser.add_argument(
        "--skip-config",
        type=Path,
        default=Path("iso_test_js/pylog.skip"),
        help="Path to skip/XFAIL config (default: iso_test_js/pylog.skip)",
    )

    parser.add_argument(
        "--output",
        "-o",
        type=Path,
        help="Write JSON report to file",
    )

    parser.add_argument(
        "--mode",
        type=str,
        choices=["python", "harness", "compare"],
        default="python",
        help="Execution mode: python (default), harness (use harness.pl), or compare (run both)",
    )

    args = parser.parse_args()

    # Check test file exists
    if not args.test_file.exists():
        print(f"Error: Test file not found: {args.test_file}", file=sys.stderr)
        return 1

    # Create runner
    runner = ISOTestRunner(
        max_tests=args.max_tests,
        max_solutions=args.max_solutions,
        max_steps=args.max_steps,
        timeout_ms=args.timeout,
        fail_on_skip=args.fail_on_skip,
        match_filter=args.match,
        verbose=args.verbose,
        mode=args.mode,
    )

    # Load skip configuration
    runner.load_skip_config(args.skip_config)

    # Run suite based on mode
    if args.mode == "harness":
        report = runner.run_harness_mode(args.test_file)
    elif args.mode == "compare":
        # Run both modes and compare
        python_report = runner.run_suite(args.test_file)
        harness_report = runner.run_harness_mode(args.test_file)
        comparison = compare_modes(python_report, harness_report)
        report = {
            "mode": "compare",
            "python": python_report,
            "harness": harness_report,
            "comparison": comparison,
            "summary": comparison["summary_diff"],
        }
    else:
        # Default: python mode
        report = runner.run_suite(args.test_file)

    # Output results
    if args.json:
        print(json.dumps(report, indent=2))
    else:
        print_summary(report)

    # Write JSON report if requested
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2))
        if not args.json:
            print(f"\nJSON report written to: {args.output}")

    # Exit code based on failures
    summary = report.get("summary", {})

    # For comparison mode, check both python and harness results
    if report.get("mode") == "compare":
        python_summary = report.get("python", {}).get("summary", {})
        harness_summary = report.get("harness", {}).get("summary", {})

        if (
            python_summary.get("failed", 0) > 0
            or python_summary.get("errored", 0) > 0
            or harness_summary.get("failed", 0) > 0
            or harness_summary.get("errored", 0) > 0
        ):
            return 1

        if args.fail_on_skip and (
            python_summary.get("skipped", 0) > 0
            or harness_summary.get("skipped", 0) > 0
        ):
            return 1
    else:
        # Single mode
        if summary.get("failed", 0) > 0 or summary.get("errored", 0) > 0:
            return 1
        if args.fail_on_skip and summary.get("skipped", 0) > 0:
            return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
