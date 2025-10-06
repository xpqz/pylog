# Refactor Plan: Engine Decomposition

This plan breaks `prolog/engine/engine.py` into cohesive modules while preserving behavior, public API, and test coverage. Work proceeds incrementally with green tests at every step and no conditional imports (see `coderules.md`).

## Goals
- Reduce `engine.py` complexity by isolating concerns: scheduling/dispatch, builtins, utilities, arithmetic, exceptions, and dynamic DB.
- Preserve Engine API and observable behavior (ports/tracing order, indexing semantics, CLP(FD) behavior).
- Keep imports at module tops; no conditional imports anywhere.
- Maintain test green status at each phase.

## Guiding Principles
- Surgical, incremental extraction; small PRs, full test runs after each.
- Keep Engine methods initially as thin wrappers delegating to new modules to minimize risk.
- Do not move trace/port emission points; preserve ordering and payloads.
- When indexing is enabled, replace programs using `IndexedProgram.from_clauses` exactly as today.

## Target Package Layout

- `prolog/engine/engine.py`
  - Orchestrates: `Engine` class, `run/solve`, stacks, frame/choicepoint lifecycle, dispatch entrypoints, builtin registry wiring, backtracking.
- `prolog/engine/dispatch.py` (optional, later)
  - Bodies of `_dispatch_predicate/_dispatch_builtin/_dispatch_conjunction/_dispatch_disjunction/_dispatch_if_then_else/_dispatch_cut` as free functions taking `engine`.
- `prolog/engine/builtins/`
  - `control.py`: `true/0`, `fail/0`, `!/0`, `call/1`, `call/2-8`, `\+/1`.
  - `types.py`: `var/1`, `nonvar/1`, `atom/1`, `integer/1`, `float/1`, `number/1`, `atomic/1`, `compound/1`, `callable/1`, `ground/1`.
  - `terms.py`: `=/2`, `\=/2`, `copy_term/2`, `unify_with_occurs_check/2`, `=../2`, `functor/3`, structural compares `==/2`, `\\==/2`, `@</2`, `@>/2`, `@=</2`, `@>=/2`.
  - `arithmetic.py`: `is/2`, `=:=/2`, `=\\=/2`, `>/2`, `</2`, `>=/2`, `=</2` (uses arithmetic evaluator util).
  - `solutions.py`: `findall/3`, `bagof/3`, `setof/3` (+ helper collect routine).
  - `exceptions.py`: `throw/1`, `catch/3` (+ core handle_throw routine).
  - `dynamic_db.py`: `dynamic/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`, `abolish/1` (+ helpers).
  - `clpfd.py`: keep current `builtins_clpfd.py` or add a thin wrapper that re-exports registrations; wire in registry.
  - `__init__.py`: `register_all(registry)` that imports siblings and calls each `register(registry)`.
- `prolog/engine/utils/`
  - `terms.py`: `reify_term`, `is_ground`, list helpers (build/convert/flatten), `decompose_to_list`, functor/arity extraction, structural compare, `deref_term`.
  - `copy.py`: `copy_term_recursive`, `copy_term_with_fresh_vars`, clause-term builders used by dynamic DB.
  - `arithmetic.py`: `eval_arithmetic(engine, term)` (iterative evaluator; uses engine deref/unify rules).
  - `selection.py`: clause selection helpers wrapping `IndexedProgram` and streaming cursors.
  - `cleanup.py`: post-run cleanup for query var unbinding and CLP(FD) domain persistence.

All imports must be at tops of files, and registration must be deterministic.

## Registration Pattern (No Conditional Imports)
- Engine keeps a dict `_builtins[(name, arity)] -> fn(engine, args_tuple) -> bool`.
- `prolog/engine/builtins/__init__.py` exposes `register_all(registry)` and imports all sibling modules unconditionally; each sibling exposes `register(registry)`.
- `Engine._register_builtins()` calls `builtins.register_all(self._builtins)` and separately wires CLP(FD) registrations (import at top-level only).

## Extraction Phases

Each phase ends with a full test run. Order chosen to minimize coupling risk.

1) Extract Pure Utilities (lowest risk)
- Move to `prolog/engine/utils/`:
  - `_reify_term` → `utils.terms.reify_term(engine, term)`.
  - `_copy_term_recursive`, `_copy_term_with_fresh_vars` → `utils.copy.*`.
  - `_build_prolog_list`, `_prolog_list_to_python_list`, `_decompose_to_list`, `_deref_term`, `_is_ground`, functor/arity helpers, `_structural_compare` → `utils.terms.*`.
  - `_eval_arithmetic` → `utils.arithmetic.eval_arithmetic(engine, term)`.
  - Post-`run()` cleanup path (query var unbinding + CLP(FD) domain persist) → `utils.cleanup.cleanup_after_query(engine)`.
- Keep existing Engine methods as thin wrappers initially to avoid touching call sites.
- Acceptance: no behavior change; `test_list_edge_cases.py`, `test_univ.py`, `test_functor.py`, and CLP(FD) scenario tests green.

2) Builtins: Types/Predicates Group
- Move `var/nonvar/atom/integer/float/number/atomic/compound/callable/ground` to `builtins/types.py`.
- Add `builtins/__init__.py` and `builtins/types.py::register(reg)`.
- Wire via `Engine._register_builtins()` using the registry function.
- Acceptance: `prolog/tests/unit/test_builtins.py::TestBuiltinInfrastructure` and type predicate tests green.

3) Builtins: Arithmetic
- Move `is/2`, `=:=/2`, `=\\=/2`, `>/</>=/=/` to `builtins/arithmetic.py` using `utils.arithmetic.eval_arithmetic`.
- Acceptance: arithmetic portions of `test_builtins.py` and any numeric comparison tests green.

4) Builtins: Term Construction/Inspection
- Move `=../2`, `functor/3`, structural compares `==/2`, `\\==/2`, `@</2`, `@>/2`, `@=</2`, `@>=/2` to `builtins/terms.py` using `utils.terms`.
- Acceptance: `test_univ.py`, `test_functor.py`, structural comparison tests green.

5) Builtins: All-Solutions
- Move `findall/3`, `bagof/3`, `setof/3` and the `_collect_all_solutions` helper to `builtins/solutions.py`.
- Keep current simplified semantics (no `^/2` yet) for parity with tests.
- Acceptance: `test_builtins.py` solutions tests and any set/bag tests green.

6) Exceptions
- Move `throw/1`, `catch/3`, and `_handle_throw` to `builtins/exceptions.py`.
- Ensure cursor snapshot/restore and frame/goal stack restoration semantics preserved (including recursion depth validation and POP_FRAME handling).
- Acceptance: `test_throw.py`, `test_catch.py`, and any exception-path integration tests green.

7) Dynamic Database
- Move `dynamic/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`, `abolish/1` plus helpers (`_term_to_clause`, `_parse_pred_indicator`, `_flatten_conjunction`, `_extract_predicate_key`, `_build_clause_term_copy`, `_clause_key_from_head`, `_set_program_clauses`) to `builtins/dynamic_db.py` and `utils.copy/terms` as appropriate.
- Preserve indexing rebuild via `IndexedProgram.from_clauses` when `use_indexing=True`.
- Acceptance: dynamic DB unit tests and `test_predicate_isolation.py` green.

8) Clause Selection Wrapper
- Extract clause selection and cursor management to `utils/selection.py` and call from `_dispatch_predicate`.
- Keep streaming/non-streaming decision points and metrics accounting intact.
- Acceptance: general engine tests (`test_performance.py`, `test_builtins.py` dispatch paths) green.

9) Optional: Dispatch Module
- If `_dispatch_*` bodies remain large, move them to `prolog/engine/dispatch.py` as free functions (e.g., `dispatch_predicate(engine, goal, depth, call_emitted)`), keeping Engine methods as thin delegates.
- Acceptance: no behavior change across all tests.

## API Compatibility
- Preserve `Engine` public methods (`solve`, `run`, `unify`) and debug helpers (`debug_ports`, `debug_frame_pops`, `debug_trail_writes`).
- Keep `_is_builtin` behavior and the builtin registry shape.
- Maintain tracer events and port emission ordering; do not move emission sites.
- Preserve CLP(FD) queue semantics and attribute hook dispatch.

## Testing & Validation Strategy
- Per phase, first run the most relevant subset of tests, then the full suite:
  - Utilities: `test_list_edge_cases.py`, `test_univ.py`, `test_functor.py`.
  - Types/Terms/Arithmetic: `test_builtins.py`, `test_univ.py`, `test_functor.py`.
  - Exceptions: `test_throw.py`, `test_catch.py`.
  - Dynamic DB: `test_library_predicates.py`, `test_predicate_isolation.py`.
  - General: `test_repl*.py`, `test_performance.py`, scenarios including CLP(FD).
- Verify ports/tracing integrity by running any tests asserting on ports; maintain exact sequences.
- Measure engine.py LoC reduction and ensure imports remain top-level.

## Risks & Mitigations
- Hidden coupling to Engine internals: start with wrappers; pass `engine` explicitly to utils for required access.
- Trace/port order drift: preserve calls in the same call sites; utilities must not emit ports.
- Catch/disjunction and streaming cursor restoration: keep current semantics and tests; do not move logic across boundaries until exceptions are fully covered.
- Dynamic DB with indexing: always rebuild via `IndexedProgram.from_clauses`.
- CLP(FD) propagation and domain persistence: keep cleanup behavior in one place (`utils.cleanup`).

## Non-Goals (for this refactor)
- Changing semantics or ISO compliance beyond current behavior (e.g., real `^/2` support for bagof/setof).
- Rewriting CLP(FD) builtins; only re-wiring their registration location if desired.

## Rollout & PR Boundaries
- One PR per phase; do not mix concerns.
- Each PR includes:
  - Extraction, Engine delegations, and import/registry wiring.
  - No other drive-by changes.
  - Full test suite run locally; ensure zero regressions.

## Definition of Done
- `engine.py` reduced substantially and focused on orchestration.
- Builtins live under `prolog/engine/builtins/` with deterministic registration.
- Utilities live under `prolog/engine/utils/` and are reused across builtins and Engine.
- All tests pass; no conditional imports anywhere.
- Developer docs updated (this file) and any README notes if needed.

---

Appendix: Initial Extraction Map (from current code)
- Run/scheduling/dispatch entrypoints remain in `engine.py` (e.g., `solve`, `run`, `_dispatch_*`).
- Utilities to extract:
  - Reify/copy/list/functor helpers; arithmetic evaluator; structural compare; cleanup.
- Builtins grouping:
  - Types: `var/nonvar/…`.
  - Arithmetic: `is/2`, comparisons.
  - Terms: `=../2`, `functor/3`, structural compares.
  - Solutions: `findall/bagof/setof` + `collect_all_solutions`.
  - Exceptions: `throw/1`, `catch/3`, `handle_throw`.
  - Dynamic DB: `dynamic/assert*/retract*/abolish` + helpers.
- Clause selection wrapper from `_dispatch_predicate` to `utils/selection`.

