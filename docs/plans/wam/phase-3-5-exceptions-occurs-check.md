# Phase 3.5 — Exceptions and occurs‑check policy

Objective
- Provide ISO‑style `throw/1` and `catch/3` under the WAM with correct unwind semantics, and define the occurs‑check policy with an optional flag.

Outcomes
- Exception frames integrated with WAM control structures; correct unwinding across environments, choicepoints, and the trail.
- Occurs‑check documented: default off for performance; optional on via flag and supported by unification paths.

Non‑Goals (Phase 3.5)
- Full port of all exception-raising builtins; Phase 4 will bridge many through existing Python builtins.

Scope
- In: exception frame representation; unwind protocol; mapping between Python exceptions and Prolog error terms; occurs‑check toggle in unify.
- Out: Advanced exception features not present in the current engine.

Dependencies
- Phase 2 control flow (environments, choicepoints) and Phase 1 unification.

Design
- Exception Frames: augment control stack with an exception record (or reuse CP layout with an exception flag) capturing `CP, E, B, P, H, TR, HB` at the throw site.
- Unwind Protocol: on `throw(Term)`, find the nearest matching `catch/3`; restore machine state using the recorded frame and set up goal `Handler` with `Ball = Term` semantics.
- Mapping: provide utilities to convert Python errors into standard Prolog error terms (`instantiation_error`, `type_error/2`, `domain_error/2`, etc.), consistent with the tree‑walker.
- Occurs‑check: add a machine or compile flag to enable occurs‑check in unification; when enabled, `unify` fails on cycles (e.g., `X = f(X)`).

Instruction/Runtime Additions
- Opcodes (if needed): `throw` and `catch_setup`/`catch_cleanup` to manage exception regions; or implement as compiler/runtime helpers that manipulate choicepoints and handlers.
- Unification: guard aliasing that would create cycles when occurs‑check is on; keep default off for performance.

Implementation Tasks
- Add exception frame data structure and integration points in call/execute/try/retry/trust.
- Implement `throw/1` and `catch/3` semantics at WAM level; add minimal opcodes or runtime helpers.
- Add error mapping helpers and ensure bridge (Phase 4) preserves exceptions across Python boundary.
- Wire occurs‑check flag through machine/engine; update unify to consult it.

Tests
- Unit: `catch(throw(ball), Ball=ball, fail)` succeeds with `Ball = ball`; non‑matching catch remains unhandled.
- Backtracking: exceptions unwind and restore `H, TR, HB, E, CP` correctly; no leaks.
- Occurs‑check: with flag on, `X = f(X)` fails; with flag off, it succeeds (or creates a cycle) matching current engine behavior.

Acceptance Criteria
- Existing exception unit tests (`test_throw.py`, `test_catch.py`, `test_error_handling.py`) pass under WAM mode.
- Occurs‑check behavior matches tree‑walker based on flag setting.

Risks
- Incorrect unwind ordering leading to corrupted state; mitigate with invariants and targeted snapshot tests.
- Performance overhead of occurs‑check; keep default off and guard with flag.

Rollout
- Behind flag; no change to user entry points; document in roadmap and user docs.

