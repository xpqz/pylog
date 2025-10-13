# Phase 7 — Attributed variables + CLP(FD) interop

Objective
- Support attributed variables at the WAM level and bridge attribute hooks to existing CLP(FD) mechanisms.

Outcomes
- Attributes attached to REF roots; hooks on bind/deref; minimal CLP(FD) propagation wiring.

Scope
- In: attribute store keyed by var root; hook invocations; bridge to current CLP(FD) queue.
- Out: Rewriting CLP(FD) in WAM (future).

Dependencies
- Stable unification with deref (Phase 1) and control (Phase 2).
- GC (Phase 6) strongly recommended but not strictly required; proceed with one of:
  - **Preferred path**: Full Phase 6 GC operational, ensuring attribute payloads are traced and collected properly.
  - **Lightweight path**: Conservative attribute retention with explicit cleanup hooks on backtracking; suitable for short‑lived constraints but risks memory accumulation in long‑running queries.
  - **Decision point**: If Phase 7 proceeds without Phase 6, document memory growth limits and plan Phase 6 integration before production use.

Design
- Store attributes in a side map `{root_addr: {mod: attr}}` on the machine; preserve across trail/untrail.
- On bind: if either side has attributes, invoke module hook (bridge into Python impl initially).
- Ensure trailing includes attribute state restoration on backtracking.
 - Unification hooks: expose the same bind/deref hook points as the current engine to keep CLP(FD) propagation behavior consistent; beware propagation overhead on hot unify paths.

Attribute Invariants
- Only root REF addresses appear as keys in the attribute map.
- On deref/bind, attributes migrate to the new root; old entries removed.
- Trail records old attribute states so `untrail` restores both REF links and attributes.

Hook Protocol (initial)
- `call_attr_hook(module:str, event:str, root:int, term_addr:int) -> bool`
  - `event` could be `bind` or `deref` (phase 1 only needs `bind`).
  - Returns success/failure; on failure, unification must fail.

CLP(FD) Bridge
- For `clpfd` module, bridge into existing Python queue/propagator:
  - On bind, extract/reify domain; enqueue propagation; run to fixpoint (bounded).
  - Export helper to get current domain for testing.

Implementation Tasks
- Add attribute map + trail support for attribute changes.
- Define hook protocol; implement bridge into existing CLP(FD) (Python) for propagation.
- Add reification helpers to expose domains for testing.

Tests
- Unit: attaching/removing attrs; unification triggers hooks; backtracking restores attrs.
- CLP(FD) smoke: simple domains and labeling via bridge.

Acceptance Criteria
- Existing CLP(FD) unit tests can run under WAM mode (allowing slower performance initially).

Risks
- Attribute trail correctness; mitigate with focused tests and consistent invariants.
 - Memory pressure from attribute payloads if GC is deferred; mitigate with conservative retention policies or lightweight ref‑counting.

Rollout
- Behind flag; CLP(FD) still defaults to tree‑walker unless WAM explicitly chosen.

Implementation Checklist
- [ ] Attribute store + trail
- [ ] Hook invocation
- [ ] Bridge to CLP(FD)
- [ ] Tests (attrs + CLP(FD) smoke)
