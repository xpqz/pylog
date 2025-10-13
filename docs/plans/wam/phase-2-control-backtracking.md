# Phase 2 — Control flow, environments, backtracking

Objective
- Implement call/return, environments, choicepoints, and cut so multi‑clause predicates and recursion execute under WAM.

Outcomes
- `call/execute/proceed`, `allocate/deallocate` (baseline), choicepoint ops, and `cut` semantics.
 - Interim memory safety: minimal GC stub or conservative heap growth guard to allow long‑running tests; full GC deferred to Phase 6.

Non‑Goals (Phase 2)
- No compiler integration beyond hand‑assembled programs for tests.
- No indexing (linear clause selection is fine in this phase).
- No advanced frame trimming/LCO yet (can be stubbed).

Scope
- In: `call`, `execute`, `proceed`, `allocate`, `deallocate`, `try_me_else`, `retry_me_else`, `trust_me`, `get_level`, `cut`, `neck_cut`.
- Out: Compiler (still hand‑assembled for tests), indexing (stub only).

Dependencies
- Phase 1 unification implemented; dispatcher stable.

Design
- Environments: frame at `E`, save `CP`; `Y` registers mapped to frame slots.
- Choicepoints: store `CP, B, E, P, H, TR, HB`, and continuation pointer to next alternative.
- Cut: `get_level(Yk)` + `cut(Yk)`

Concrete Frame Layout (at address `E`)
```
E[0] = PrevE
E[1] = CP
E[2..] = Y0, Y1, ... (permanent variables)
```
`allocate k`: push frame, set `E = H` or a separate frame stack; reserve `k` Y slots; save `CP`.
`deallocate`: restore `CP = E[1]`, `E = E[0]`.

**Note on environment trimming**: Reducing `k` (frame size) when permanent variables become dead is an optimisation deferred to Phase 5.5. Requires safe point identification and liveness analysis at call sites.

Choicepoint Record (at address `B`)
```
B[0] = PrevB
B[1] = CP
B[2] = E
B[3] = P   (retry continuation)
B[4] = H
B[5] = TR
B[6] = HB
```
`try_me_else L2`: create CP with retry to `L2`; jump to first clause; `B = &record`.
`retry_me_else L3`: restore from `B` record; update retry to `L3`.
`trust_me`: restore from `B` record; pop `B = PrevB`.

Call/Execute/Proceed
- `call Pred`: save `CP = P+1`; set `P = entry(Pred)`; may require `allocate` in predicate prologue.
- `execute Pred`: tail call: set `P = entry(Pred)` without saving `CP`.
- `proceed`: set `P = CP`.

Cut Semantics
- `get_level(Yk)`: save current `B` into `Yk`.
- `cut(Yk)`: set `B = Yk` (pop all choicepoints above that level).

Implementation Tasks
- Implement environment frame layout and `allocate/deallocate` ops.
- Implement choicepoint push/pop and retry chain (`try_me_else/retry_me_else/trust_me`).
- Implement `neck_cut`, `get_level`, `cut` with semantics to prune choicepoints above given level.
- Add small runner to step predicates with 1–3 clauses.
 - Add invariant checks: after `retry/trust`, restored `H,TR,HB,E,CP` match CP record.
 - Provide helpers to assemble multi‑clause predicate layouts with labels.
 - Add a minimal GC stub: track root set and expose a debug `gc_safe_point()` hook (no sweeping yet) or enforce conservative heap growth limits configurable for tests.

Tests
- Unit: single‑clause call/return; multi‑clause backtracking yields all solutions; cut prunes correctly; recursion works.
- Edge: nested choicepoints; cut across frames; backtracking restores heap/trail pointers.
 - Bounce tests: tail recursion with `execute` avoids growing environment depth (observe `E`).
 - Clause chains: `try; retry; trust` order and correctness when last alternative fails.

Acceptance Criteria
- Hand‑assembled code for `member/2`, `append/3` runs and produces same solutions as tree‑walker.
 - Invariants checker passes across call/backtrack/cut scenarios.
 - Long‑running scenario tests do not show unbounded heap growth under the interim guard.

Risks
- Incorrect CP restoration or frame layout. Mitigate with snapshot diffs at each retry.

Rollout
- Behind flag; no user‑visible change.

Implementation Checklist
- [ ] Frames + allocate/deallocate
- [ ] Choicepoints + try/retry/trust
- [ ] Cut ops
- [ ] Tests for multi‑clause + recursion
