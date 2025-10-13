# Phase 5.5 — Optimization passes

Objective
- Add basic compile‑time optimizations and selective opcode fusion to reduce dispatch and backtracking work beyond indexing.

Outcomes
- Peephole rules and groundwork for environment trimming and last call optimization (LCO).
- Profiling of instruction dispatch overhead and opcode granularity; initial performance bounds established (WAM not worse than ~2× tree‑walker on representative tests).

Scope
- In: peephole rules; LCO (optional at this stage); environment trimming analysis scaffold; opcode granularity review.
- Out: perfect hash indexing and deep multi‑arg indexing (covered elsewhere or deferred).

Dependencies
- Phase 3 compiler end‑to‑end; Phase 2 control; Phase 5 indexing baseline in place.

Design
- Peephole: fold redundant `put`/`get`, remove dead moves; keep rule list composable and testable.
- Opcode Granularity: review hotspots and consider fusing small instruction sequences where profitable while avoiding code bloat; re‑measure after changes.
- LCO/Environment trimming: analyze safe sites for tail calls and frame trimming; keep implementation conservative.
  - **Environment trimming assignment**: Implement optional `trim N` instruction to reduce frame size when permanent variables dead; requires liveness analysis at call boundaries and safe point identification. Document trade-offs (complexity vs memory savings) and enable selectively based on profiling data.

Peephole Candidates
- `put_value Xi, Aj` followed by `get_value Xi, Aj` in same clause → remove redundant moves.
- Consecutive `set_variable` on fresh X registers → collapse by allocating batch.
- Dead `set_x`/debug ops removed in release mode.

Implementation Tasks
- Implement peephole pass pipeline with unit tests per rule.
- Optional: environment trimming analysis scaffold.
- Profile instruction dispatch and opcode granularity; adjust encoding or handler fusion; document results.

Tests
- Peephole doesn’t change semantics; instruction counts drop.
- Benchmarks track improvements (non‑blocking in CI).

Acceptance Criteria
- Speedups on common patterns vs. Phase 5 baseline; stable memory profile.
- Phase performance target met: within 2× of tree‑walker on representative microbenchmarks.

Risks
- Code bloat from aggressive fusion; mitigate with thresholds and measurement.
- Over‑fitting peephole rules; ensure maintainability and readability of emitted code.

Rollout
- Behind flag; no user breakage.

Implementation Checklist
- [ ] Peephole passes + tests
- [ ] Dispatch/opcode profiling
- [ ] LCO and trimming analysis
- [ ] Benchmarks captured

