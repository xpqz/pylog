# ISO Prolog Test Suite - Usage Guide

This guide covers running the ISO Prolog conformance test suite in PyLog.

## Overview

PyLog includes a Python-side runner for the ISO Prolog test suite from [iso_test_js/iso.tst](../iso_test_js/iso.tst). This Phase A implementation parses and executes ISO test patterns without depending on ISO I/O predicates.

## Quick Start

### Smoke Test (Recommended for local development)

```bash
make iso-smoke
# Runs first 200 tests with verbose output
```

### Run Full Pytest Integration

```bash
make test-iso
# Runs pytest integration tests (~1.1s)
```

### Run Custom Test Subsets

```bash
# First 100 tests
uv run python -m scripts.run_iso_suite --max-tests 100 --verbose

# Filter by pattern
uv run python -m scripts.run_iso_suite --match "call" --verbose

# JSON output
uv run python -m scripts.run_iso_suite --max-tests 50 --json

# Save report
uv run python -m scripts.run_iso_suite --max-tests 200 --output results.json
```

## CLI Reference

### scripts/run_iso_suite.py

```
usage: run_iso_suite.py [test_file] [options]

Arguments:
  test_file              Path to ISO test file (default: iso_test_js/iso.tst)

Output Options:
  --json                 Output results as JSON
  --verbose, -v          Show all tests
  --output, -o FILE      Write JSON report to file

Test Selection:
  --max-tests N          Maximum number of tests to run (for smoke testing)
  --match SUBSTRING      Run only tests matching this substring

Safety Controls:
  --max-solutions N      Maximum solutions per query (default: 10000)
  --max-steps N          Maximum steps per query (default: 1000000)
  --timeout MS           Timeout in milliseconds per test

Configuration:
  --skip-config FILE     Path to skip/XFAIL config (default: iso_test_js/pylog.skip)
  --fail-on-skip         Treat skipped tests as failures

Examples:
  # Quick smoke test
  ./scripts/run_iso_suite.py --max-tests 100 --verbose

  # Filter for specific functionality
  ./scripts/run_iso_suite.py --match "unify" --verbose

  # Generate CI report
  ./scripts/run_iso_suite.py --max-tests 500 --json --output ci-report.json

  # Strict mode (no skips allowed)
  ./scripts/run_iso_suite.py --max-tests 100 --fail-on-skip
```

## Test Patterns

The ISO test suite uses special operators to express test semantics:

### should_fail

Expects goal to produce zero solutions:

```prolog
call(fail) should_fail.
```

### should_give

Expects goal to succeed with check satisfied (parse-once approach preserves variable namespace):

```prolog
X=1 should_give X==1.
```

**Implementation note**: Goal and Check are combined into single query `Goal, !, Check` to ensure variables share the same namespace.

### should_throw

Expects goal to throw exception matching pattern (subsumption check):

```prolog
throw(my_error) should_throw my_error.
call(_) should_throw error(instantiation_error, _).
```

### multiple_solutions(K, SolutionCheck, FinalCheck)

Enumerates all solutions, validates each with K=i, then runs FinalCheck with K=N:

```prolog
(X=1;X=2) should_give multiple_solutions(K, K==1, X==K).
```

## Skip Configuration

Tests revealing unimplemented features or semantic differences are documented in [iso_test_js/pylog.skip](../iso_test_js/pylog.skip):

```
# Format: pattern # reason
call(_) should_throw error(instantiation_error,_) # Issue #XXX: call/1 type validation not implemented
```

Patterns match against test source text. Tests are skipped if pattern appears anywhere in source.

## Integration with pytest

ISO tests are marked with `@pytest.mark.iso` and `@pytest.mark.slow`:

```bash
# Run only ISO tests
pytest -m iso

# Exclude ISO tests (faster developer loop)
pytest -m "not iso"

# Fast test suite (excludes slow/stress/iso)
make test-fast
```

## Makefile Targets

| Target       | Description                                      |
|--------------|--------------------------------------------------|
| `test-iso`   | Run pytest integration tests (~1.1s)             |
| `iso-smoke`  | Quick CLI smoke test (first 200 tests, verbose)  |
| `test-fast`  | Fast suite excluding slow/stress/iso tests       |

## CI Integration

For CI pipelines, recommend:

1. **Smoke job** (non-blocking):
   ```bash
   scripts/run_iso_suite.py --max-tests 200 --json --output .iso-reports/smoke.json
   ```

2. **Pytest integration** (blocking):
   ```bash
   pytest -m iso
   ```

3. **Artifact upload**: Upload JSON reports for trend analysis

## Exit Codes

- **0**: All tests passed (or skipped, unless `--fail-on-skip`)
- **1**: One or more tests failed or errored

## Architecture Notes

### Parse-Once Approach (should_give)

**Critical**: Goal and Check must share variable namespace. If parsed separately, Reader assigns different Var IDs for same variable names.

**Solution**: Parse combined query `Goal, !, Check` as single term.

```python
# WRONG: Separate parsing
goal_term = reader.read_term(goal_text)
check_term = reader.read_term(check_text)
# Variables have different IDs!

# CORRECT: Parse once
combined = f"{goal_text}, !, {check_text}"
combined_term = reader.read_term(combined)
# Variables share same IDs
```

### Exception Subsumption

Expected pattern unifies with thrown exception using fresh Store:

```python
store = Store()
trail = []
# Allocate cells for variables in expected pattern
success = unify(expected, thrown, store, trail, occurs_check=False)
```

This avoids mutating engine state and treats variables in expected as wildcards.

### Safety Limits

Default limits prevent runaway queries:

- `max_solutions`: 10000 solutions per query
- `max_steps`: 1000000 steps per query
- `timeout_ms`: Optional timeout (not set by default)

Override via CLI flags or when instantiating `ISOTestExecutor`.

## Future Work

- **Phase B** (Issue #414): Run native harness.pl inside PyLog with ISO I/O predicates
- **Reader enhancements** (Issue #424): Dynamic operator registration, postfix operator support
- **multiple_solutions improvements**: Properly apply solution bindings to checks (currently uses simplified K=i approach)

## References

- [ISO_TESTING.md](plans/ISO_TESTING.md): Full Phase A & B plan
- Issue #413: Phase A implementation
- Issue #414: Phase B (native harness)
- Issue #424: Reader enhancements for dynamic operators
