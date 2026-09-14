# ISO Prolog Test Suite Integration Plan

This document describes how to integrate the ISO Prolog verification suite under `iso_test_js/` into Pylog's automated tests and CI.

The goal is to execute the third‑party ISO conformance patterns regularly, produce a structured report, and gate CI without slowing down the core fast test loop.


## Objectives

- Run the ISO suite (`iso_test_js/iso.tst`) against Pylog.
- Report per‑test pass/fail/skip with locations and brief diagnostics.
- Keep default developer loop fast; expose opt‑in targets and pytest markers.
- Provide a migration path from a Python‑side runner to a pure Prolog harness once we have the required ISO I/O builtins.


## Suite Overview (current folder)

- `iso_test_js/harness.pl` — pure ISO test harness that reads `.tst` files using ISO I/O (open/read/stream_property/write/nl/close).
- `iso_test_js/auxiliaries.pl` — auxiliary predicates expected by some tests (system‑dependent).
- `iso_test_js/iso.tst` — the main test patterns. Syntax is based on operators defined in the harness:
  - `Goal should_fail.`
  - `Goal should_give Check.`
  - `Goal should_throw Exception.`
  - `fixme <test>` skips known failures.
  - Special `Check`: `multiple_solutions(K, FinalCheck, SolutionCheck)`.
- Side files used by tests: `nowrite`, `empty`, `hello`, `scowen`, `nosuch` (must be present with expected permissions/content).

The harness expects: `test('iso.tst').` (or `test(In,Out)`) after loading `harness.pl` and system‑specific `auxiliaries.pl`.


## Constraints and Strategy

Pylog currently lacks several ISO I/O builtins used by `harness.pl` (e.g., `open/3`, `read/1-2`, `stream_property/2`, `current_output/1`, `nl/1`). Implementing those is non‑trivial and orthogonal to the core plan.

Therefore, we will proceed in two phases:

- Phase A (Python runner): Build a Python‑side runner that parses `iso.tst` using Pylog's own parser and executes tests using the Pylog engine directly. This avoids ISO I/O requirements while validating engine semantics.
- Phase B (Harness mode): Once minimal ISO I/O is available, add an option to run the original `harness.pl` inside Pylog (preferred long‑term), keeping the Python runner as a fallback/debug aid.


## Phase A — Python Runner Integration

### A1. Runner script

Add `scripts/run_iso_suite.py` (CLI + importable) with responsibilities:

- Parse `iso_test_js/iso.tst` via `prolog.parser.parser.parse_program`.
- Interpret each top‑level term:
  - `fixme T` — record as skipped.
  - `should_fail(Goal)` — run `Goal`; expect zero solutions.
  - `should_give(Goal, Check)` — commit to first solution of `Goal` and validate `Check`.
  - `should_throw(Goal, Exception)` — run `Goal` and verify thrown exception subsumes `Exception`.
- Implement `multiple_solutions` by enumerating all solutions of `Goal`, invoking per‑solution `SolutionCheck(K, …)` (K = 1..N), then `FinalCheck(K=N, …)`. If any SolutionCheck fails for any K, the test fails.
- Produce a structured result object per test:
  - `{ file, index, lineno?, term_str, status: pass|fail|skip, why?, duration_ms }`.
- Output formats: human‑readable summary and `--json` machine‑readable report for CI.

Notes:

- Parsing: `iso.tst` entries are facts (e.g., `Goal should_give Check.`) and will be seen as structs with functors `should_give/2`, `should_fail/1`, etc. Use AST to extract subterms.
- Variable binding for `Check`:
  - Preferred: Build a combined query `Goal, !, Check` so the same variable namespace is used and `Check` runs in the same logical environment. Evaluate success/failure on that combined query.
  - For `multiple_solutions`, evaluate `Goal` alone to collect solutions. For each binding set `Bi`, substitute `K = i` and apply `Bi` bindings into `SolutionCheck`, then run `SolutionCheck` as a standalone query. After the loop, substitute `K = N` into `FinalCheck` and evaluate.
- Exception checks: Execute `Goal` and intercept Pylog's `PrologThrow`. Implement a subsumption check (term instance) between thrown and expected exception terms analogous to `subsumes_term/2`.

Edge cases to support early:

- `Check = true` — always pass if `Goal` has at least one solution (with cut).
- `var/1`, `nonvar/1`, `number/1`, `atom/1`, etc., inside `Check` — these are normal goals, so combined query path already handles them.
- Equality operators (`==`, `=..`, etc.) inside `Check` — handled by engine.


### A2. Pytest shim

Add `prolog/tests/integration/test_iso_suite.py` to run the runner in CI:

- Mark with `@pytest.mark.iso` and `@pytest.mark.slow`.
- Default assertion: Zero failures in the report. If there are known failures initially, parametrize as XFAIL or maintain a static skiplist (see A4).
- Expose environment variables and options:
  - `PYLOG_ISO_MAX_TESTS` — limit number of parsed tests (useful for local smoke runs).
  - `PYLOG_ISO_FAIL_ON_SKIP=1` — optionally treat `fixme` as failures.


### A3. CLI and Makefile integration

- Add `make test-iso` to run only the ISO shim (respecting markers).
- Ensure `make test-fast` excludes `iso` marker by default.
- Document usage in `docs/testing.md` (or `docs/repl.md` adjunct): prerequisites, invocation, time expectations.


### A4. Skip/XFAIL policy

Initially, some ISO patterns may exercise features not implemented or deliberately non‑ISO (e.g., full stream I/O, some error term shapes). Create a small control file `iso_test_js/pylog.skip` listing patterns to skip or mark XFAIL, with comments and TODO issue links:

- Format: simple textual filters: `functor/3: arity=...`, or raw string match of the printed test term.
- The runner reads this file and applies skip/XFAIL mapping before execution. XFAILs should still be executed but not cause suite failure.


### A5. Reporting and CI

- Human summary:
  - `N tests | P passed | F failed | S skipped | X xfailed | T total ms`
  - List first K failures with location and expected vs actual (e.g., `expected success, got failure`), plus the rendered term.
- JSON report persisted to `./.iso-reports/latest.json` for artifacts and trend analysis.
- Optional: emit GitHub Actions annotations on failures (file:line anchored to `iso_test_js/iso.tst`).


## Phase B — Running the original harness.pl inside Pylog

When the following builtins are available, we can run `harness.pl` directly:

- `open/3`, `close/1-2`, `read/1-2`, `write/1-2`, `nl/0-1`, `stream_property/2`, `current_output/1`.
- The auxiliary predicates required by `iso.tst` from `auxiliaries.pl`:
  - `iso_test_ensure_loaded/1`
  - `iso_test_variant/2`
  - `iso_test_os/1`
  - `iso_test_non_repositionable_stream/1`
  - `iso_test_same_members/2`

Plan:

- B1. Implement a minimal ISO stream module in Pylog with in‑memory/text streams sufficient for harness usage.
- B2. Implement/port `auxiliaries.pl` in a Pylog‑compatible way (possibly embed as part of standard library `prolog/lib/` and auto‑consult for ISO runs).
- B3. Add a `--mode=harness` to `scripts/run_iso_suite.py` to chunk to the Prolog harness path: consult `harness.pl` and `auxiliaries.pl`, then execute `test('iso.tst')`, capturing printed output and parsing the summary or (preferably) augment harness with a thin extra predicate to emit machine‑readable counts.
- B4. Keep the Python runner as fallback and for fine‑grained diagnostics.


## Implementation Details

### Parsing `iso.tst`

Use `parse_program()` against the raw contents of `iso.tst`. Each entry is a fact:

- `Struct('should_fail', (Goal,))`
- `Struct('should_give', (Goal, Check))`
- `Struct('should_throw', (Goal, Exception))`
- `Struct('fixme', (Term,))` where `Term` is one of the above.

Retain source offsets to report file positions:

- If our parser yields positions, map them; else, pre‑scan to compute approximate line numbers (count dots) to annotate results.

### Executing tests (Python runner)

- Construct goal lists as sequences (conjunctions) using Pylog AST `Struct(',', (A,B))` when needed.
- For `should_give`, build `Query = (Goal , ! , Check)` and run once. If any solution exists, the test passes.
- For `multiple_solutions`:
  1. Extract `K`, `FinalCheck`, `SolutionCheck` from the AST.
  2. Run `Goal` to collect all solutions (respect a safety `maxSolutions` limit; default large).
  3. For `i, sol` enumerate, substitute `K = i` and apply `sol` bindings into `SolutionCheck`, then run it; require success for each i.
  4. Substitute `K = N` into `FinalCheck` and run it; require success.

Term substitution: write a small helper that walks an AST term and replaces `Var(hint=Name)` occurrences based on `{Name -> ValueTerm}` from the solution dict.

### Exceptions

- Wrap engine execution and catch `PrologThrow`. Compare the thrown term against expected using a structural “instance” check analogous to `subsumes_term/2` (variables in expected unify with the thrown term).

### Performance

- Provide `--max-tests` and `--max-time` options in runner to cap local runs.
- Cache parse tree for `iso.tst` across runs (optional).


## Developer UX

- Local smoke test: `uv run python scripts/run_iso_suite.py --max-tests 100`.
- Full ISO as part of CI: `make test-iso` (marked `slow`, separate job or step after fast tests).
- Investigate failures with `--verbose --json out.json` and by re‑running a single pattern (support `--match '<substring>'`).


## Makefile and CI

- Makefile:
  - `test-iso`: `uv run pytest -m iso -q` (or direct `uv run python scripts/run_iso_suite.py`).
  - Ensure `test-fast` excludes `iso` marker.
- CI:
  - Add a dedicated job `iso` that runs on PR and nightly.
  - Upload JSON report as artifact and optionally post summarized comment on PR.


## Risks and Mitigations

- Harness dependency on ISO I/O: mitigated by Phase A Python runner.
- Differences in error terms: start with a small XFAIL list and open tracking issues with examples.
- Platform‑specific file permission tests (`nowrite`): guard runner to skip on platforms where permissions cannot be reproduced (Windows), or pre‑check and mark XFAIL with rationale.


## Roadmap

- Milestone 1: Land Python runner + pytest shim + Makefile/CI wiring with initial skiplist. Document usage.
- Milestone 2: Reduce XFAIL/skip by addressing engine gaps (error terms, edge semantics). Track progress with report history.
- Milestone 3: Implement minimal ISO stream I/O; enable harness mode (`harness.pl` inside Pylog) and compare outcomes with Python runner.
- Milestone 4: Optional: Run suite in WebAssembly (Pyodide) by reusing the Python runner in the worker context; publish results on a “Try Pylog” page for reference.


## File/Additions Summary (to be implemented)

- `scripts/run_iso_suite.py` — test runner (Phase A), CLI + library.
- `prolog/tests/integration/test_iso_suite.py` — pytest shim, `@pytest.mark.iso`.
- `iso_test_js/pylog.skip` — optional skip/XFAIL controls (doc‑commented).
- Makefile targets: `test-iso` (and ensure exclusion from `test-fast`).
- Documentation updates: this plan and a short usage section in `docs/testing.md`.


## Acceptance Criteria

- New `make test-iso` runs locally and in CI, generating a summary with non‑zero executed tests.
- Default `make test-fast` remains green and time‑bounded as before.
- ISO failures (if any) are visible in CI but do not block unrelated development once guarded by the `iso` job or an allowed‑to‑fail phase (until we decide to gate on it).

