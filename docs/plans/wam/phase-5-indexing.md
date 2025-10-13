# Phase 5 — Indexing

Objective
- Add first‑argument indexing to reduce dispatch and backtracking work.

Outcomes
- `switch_on_term/constant/structure` tables emitted and used for clause selection.

Scope
- In: index table builder in compiler/linker, runtime jump via tables.
- Out: perfect hash indexing and deep multi‑arg indexing (future).

Dependencies
- Phase 3 compiler end‑to‑end; Phase 2 control.

Design
- Build index during link: map constants/structures of arity 1 to clause entry; default linear chain for others.
- Tables stored per predicate; keep density thresholds to avoid bloat.
- Fallbacks: when density is low or cases too sparse, skip tables in favor of linear scan.

Index Table Encoding
- Per predicate, build:
  - `on_term`: jump table by tag (VAR/CON/STR/LIST) to a subtable or default
  - `on_constant`: dict from atom/int to label index
  - `on_structure`: dict from functor key `"name/arity"` to label index
- Thresholds:
  - Only emit tables if distinct heads ≥ 2 and density ≥ 0.5 (tunable), else fall back to linear scan.

switch_on_* Handlers (pseudo‑code)
```
switch_on_term VAR:Lvar, CON:Lcon, STR:Lstr, LIST:Llist, Ldefault
  tag = tag_of(A1)
  jump to matching label or Ldefault

switch_on_constant Table, Ldefault
  key = value_of(A1)
  if key in Table: P = Table[key] else P = Ldefault

switch_on_structure Table, Ldefault
  key = functor_of(A1)
  if key in Table: P = Table[key] else P = Ldefault
```

Implementation Tasks
- Add `indexer.py` in compiler; extend assembler/loader to encode tables.
- Implement `switch_on_*` handlers; fallback to linear scan when absent.
- Add density thresholds and metrics; allow opt‑out when tables aren’t profitable.

Tests
- Indexing improves clause selection on `color/1`, `member/2` with diverse heads.
- Fallbacks: when table density low, ensure linear scan used.
- Negative cases: conflicts between constant and structure maps handled predictably.

Acceptance Criteria
- Index tables used when available; functional and measurable improvement on microbenchmarks.

Risks
- Table bloat or wrong matches. Mitigate with thresholds and exhaustive tests per head domain.

Rollout
- Behind flag; no user breakage.

Implementation Checklist
- [ ] Index builder + encoding
- [ ] `switch_on_*` handlers
- [ ] Benchmarks captured

