# Phase 8 — Tracing, debugging, and ISO polish

Objective
- Integrate WAM with the existing tracer (CALL/EXIT/REDO/FAIL), improve error surfaces, and document ISO deltas.

Outcomes
- Trace parity on simple programs; improved error terms and positions; documented behavior differences.
 - Debug Adapter Protocol (DAP) adapter updates for WAM: step by instruction or by goal; map WAM registers and stacks into Variables/Scopes.

Scope
- In: tracer sink for WAM events; error formatting; ISO audit pass over core behaviors.
- Out: Full ISO compliance (long tail) beyond current project scope.

Dependencies
- WAM control + compiler in place.

Design
- Emit tracer events from instruction handlers (`call/execute`, retry/failure, proceed) with depth.
- Collect bindings (from X/Y + heap reification) for user‑friendly prints.
- Error mappers for common exceptions; position info where reader provides it.
 - DAP mapping: extend the existing VS Code debugger support to include WAM state (P, CP, B, E, X/Y). Provide a toggle to choose stepping granularity (goal‑level ports vs instruction‑level).

Event Mapping
- CALL: on `call Pred`/`execute Pred`; include depth and goal reified from X registers.
- EXIT: on `proceed` at end of predicate; include bindings for output variables.
- REDO: on retry via `retry_me_else`/`trust_me`.
- FAIL: on unify failure or when no more alternatives; for final FAIL, include last EXIT info if helpful.

Binding Reifier
- Reuse tree‑walker pretty where possible; otherwise implement `reify(Xi)` following heap REFs.
- Avoid expensive reification unless tracing is enabled (guard calls).

ISO Audit Topics
- Cut semantics and side effects ordering.
- Arithmetic errors (division by zero, type errors) and their error terms.
- Meta‑predicates basic behaviours if bridged.

Implementation Tasks
- Add `trace.py` in `prolog/wam/`; wire tracer sink compatibility with existing `PortsTracer`.
- Implement binding reifier consistent with tree‑walker pretty.
- ISO notes: audit cut, arithmetic exceptions, type/domain/instantiation.
 - Extend DAP adapter to surface WAM state; implement step‑over/into/out based on ports or instruction boundaries; document launch options.

Tests
- Differential trace comparison on `member/2`, `append/3` (allowing minor internal event variance).
- Error formatting snapshot tests.
 - DAP smoke tests: pause/resume, step over/into/out under WAM backend; variables view shows X/Y and selected heap excerpts.

Acceptance Criteria
- Users can enable tracing under WAM and see coherent CALL/EXIT/REDO/FAIL sequences.
 - WAM mode is debuggable via DAP with visible registers/stacks and working step semantics.

Risks
- Excessive overhead when tracing; mitigate by gating behind a flag and avoiding heavy reification by default.

Rollout
- Behind flag; documentation updated internally.

Implementation Checklist
- [ ] Tracer integration
- [ ] Binding reifier
- [ ] Error mappers
- [ ] Trace + error tests
