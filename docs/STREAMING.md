# Streaming Clause Selection Plan (Engine TODO)

This document proposes how to implement true streaming clause selection in the Engine when indexing is enabled, eliminating the current list materialization bottleneck while preserving semantics, trace ordering, and existing debug behavior.

## Goals

- Remove `list(...)` materialization at clause selection sites when `use_indexing=True`.
- Preserve Prolog semantics and 4‑port trace ordering (CALL/EXIT/REDO/FAIL).
- Keep solution order identical to the non‑indexed path (Order ∩ Candidates).
- Avoid regressions in existing debug/metrics tests; keep those paths stable.

## Design Overview

Introduce a streaming cursor that presents the same interface as the current `ClauseCursor`, but pulls clause indices lazily from an iterator returned by the indexed program.

- Interface compatibility:
  - Methods: `has_more()`, `peek()`, `take()`, `clone()`
  - Attributes: `functor`, `arity`
- Internals:
  - Wraps an iterator of clause indices with a 1‑item lookahead buffer.
  - Maintains a list of consumed indices to support backtracking resume.
  - `clone()` creates a cursor at the same logical position (document that clones must not be advanced concurrently).

This enables the Engine to retain its control flow (take first clause, optionally create a choicepoint if more remain, resume on backtracking), but without building a full list of candidate indices up front.

## Engine Integration

Selection site: `prolog/engine/engine.py` in `_dispatch_predicate`:

- When `use_indexing=True`:
  - Replace materialization:
    ```python
    # Old
    matches = list(self.program.select(pred_key, goal.term, self.store))
    cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)
    ```
    with streaming use:
    ```python
    it = self.program.select(pred_key, goal.term, self.store)
    cursor = StreamingClauseCursor(functor=functor, arity=arity, it=it)
    ```
  - Keep existing logic:
    - `if not cursor.has_more():` → emit FAIL
    - `clause_idx = cursor.take()` for the first attempt
    - `if cursor.has_more():` → create PREDICATE CP with `cursor` in payload
- When `use_indexing=False`, continue using the existing `ClauseCursor` on a list from `program.clauses_for(...)`.

Backtracking resume (`_backtrack`):
- No changes. The CP payload already stores `cursor` and resumes using `has_more()`/`take()`; streaming cursor will yield the next clause lazily.

## Debug & Metrics Preservation

Current tests expect candidate counts computed up front (e.g., `_candidates_considered == len(matches)`). To avoid regressions:

- Gate streaming behind a condition: enable streaming only when `debug is False` and metrics are disabled. Keep list materialization when debug/metrics are enabled so existing assertions continue to hold.
- Optional future: switch candidate counting to per‑yield increments in the streaming path, and update tests accordingly.

## API & Placement

- Add `StreamingClauseCursor` in a new module `prolog/engine/cursors.py` (or colocate with `ClauseCursor` in `prolog/ast/clauses.py` with clear internal‑use documentation).
- Public interface mirrors `ClauseCursor` to minimize Engine changes.

## Performance Micro‑optimizations

- In the hot loop of `ClauseIndex.select` (already optimized to avoid union sets), hoist dict lookups out of the loop:
  - `struct_set = pred_idx.struct_functor.get(functor_key)`
  - `var_ids = pred_idx.var_ids`
  - Then test membership as `if typed_bucket and cid in typed_bucket` / `if struct_set and cid in struct_set` / `if cid in var_ids`.
- Keep `StreamingClauseCursor` buffer at size 1 to minimize memory and branching.

## Tests

Add targeted tests alongside existing ones.

- Cursor unit tests:
  - `has_more()` before/after `take()`, then after exhaustion.
  - Order preservation over a known iterator of indices.
  - Backtracking simulation: take one, ensure `has_more()` true, consume rest, ensure false.
- Engine integration (with `use_indexing=True` and streaming enabled):
  - Result parity vs. non‑indexed engine on representative programs: facts, mixed first‑arg types, variable heads, lists (sugar and canonical). Order must match.
  - Early termination scenario (first clause succeeds) to implicitly exercise streaming.
  - 4‑port parity: collect events via a Collector sink and compare sequences with/without streaming enabled for a simple query.
- CI stability: keep debug/metrics mode using list materialization so existing candidate‑count assertions remain valid.

## Rollout Plan

1. Implement `StreamingClauseCursor` and basic unit tests.
2. Wire into Engine when `use_indexing=True` and `debug=False` and metrics disabled. Add integration tests validating parity and early termination.
3. (Optional) Enable streaming with debug by switching to per‑yield candidate counting and adjusting tests.
4. (Optional) Wrap the non‑indexed path with a trivial streaming iterator over `clauses_for(...)` if beneficial.

## Risks & Mitigations

- Clone concurrency: document that clones should not be advanced concurrently; Engine doesn’t currently clone predicate cursors, so safe.
- Debug/metrics parity: keep list materialization when debug/metrics enabled until tests are migrated.
- Trace ordering: preserve current Engine control flow so 4‑port sequencing remains unchanged.

## Follow‑ups

- Add an env flag (e.g., `PYLOG_STREAM_SELECTION=0/1`) to force streaming on/off for troubleshooting.
- Expand benchmarks to measure Engine end‑to‑end improvements (time‑to‑first‑clause, peak memory) under realistic workloads.
- Consider a tiny buffering strategy in the choicepoint path if profiling shows `has_more()` overhead is noticeable.

