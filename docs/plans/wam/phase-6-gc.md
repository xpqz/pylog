# Phase 6 — Memory management (GC)

Objective
- Introduce a simple, stop‑the‑world GC to reclaim unreachable heap cells and maintain long‑running stability.

Outcomes
- Mark‑and‑sweep across roots (registers, environments, choicepoints, trail); optional compaction later.
 - Supersedes the interim Phase 2 GC stub/guard; root set and invariants fully documented and enforced.

Scope
- In: Mark, sweep; root enumeration; sanity checker; GC triggers (heap high‑water, manual debug call).
- Out: Moving/compacting GC (optional follow‑up once stable).

Dependencies
- Stable heap/trail layout (Phases 1–2).

Design
- Roots: `X`, `E` chain, `B` chain, `TR` entries, any global constants area.
- Mark: color bits or side table; sweep rebuilds free list or bumps new region.
- Triggers: on `H` growth beyond threshold; optional `gc/0` builtin hook for tests.

Mark Representation
- Prefer a side `marked: set[int]` or a parallel boolean list sized to heap length.
- Do not mutate cell tags during mark to avoid interfering with execution.

Root Enumeration
- Registers: all non‑null X entries.
- Environments: follow `E` chain; visit all Y slots.
- Choicepoints: follow `B` chain; include saved `E` frames where applicable.
- Trail: addresses referenced by trail entries may hold REFs—walk from those.

Sweep
- Iterate heap; if `addr` not in `marked`, reclaim cell (set to a tombstone or free list entry).
- Optional: keep a simple free list to reuse addresses; document allocation policy.

Backtracking Interaction
- Ensure GC never runs while the machine is in an inconsistent state (e.g., partial unify).
- Gate GC to safe points: post‑instruction boundary; only when no unify in progress.
- Verify that untrailing afterward does not point to reclaimed cells (roots must include trail targets).

Implementation Tasks
- Implement root walker; mark pass; sweep pass to free unreachable cells.
- Add `heap.verify()` to detect dangling refs/non‑canonical cells.
- Expose debug stats: allocs, reclaimed, max H, GC time.

Tests
- Unit: allocate/release patterns; ensure live terms survive GC; unreachable reclaimed.
- Stress: repeated allocations during backtracking; GC does not break unification/backtracking.

Acceptance Criteria
- Long‑running tests do not grow heap unbounded; verification passes.
 - Root enumeration is exhaustive (registers/E/B/TR) and validated by targeted tests.

Risks
- Missing roots lead to crashes; mitigate with exhaustive root collection tests and invariants.

Rollout
- Behind flag; keep conservative thresholds initially.

Implementation Checklist
- [ ] Root walker
- [ ] Mark pass
- [ ] Sweep pass
- [ ] Stats + verify
- [ ] Tests (unit + stress)
