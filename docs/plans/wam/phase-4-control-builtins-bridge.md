# Phase 4 — Full control features + builtins bridge

Objective
- Add disjunction and if‑then‑else compilation; introduce a bridge so WAM can call existing Python builtins until ported.

Outcomes
- `(;)/2` and `->/2 ;/2` compile patterns; `call/N` bridge into tree‑walker builtins.
 - Exception handling interoperates with disjunction, cut, and the builtins bridge; unwind rules defined across bridge boundaries.

Non‑Goals (Phase 4)
- Porting builtins into WAM—bridge suffices for now.
- Full meta‑call semantics; we support the subset needed for current builtins.

Scope
- In: compile disjunction via choicepoints; if‑then‑else via commit; Python bridge for builtins; error propagation.
- Out: Porting builtins into WAM (future).

Dependencies
- Phase 3 compiler and Phase 2 control working.

Design
- Disjunction: emit `try_me_else` chains per branch; join after `trust_me`.
- ITE: compile `Condition -> Then ; Else` into eval(Condition) with temp barrier and commit to Then.
- Bridge: `call_builtin(name, arity, Xs)` marshal/reify args to Python, run existing builtin, import/unify results back.
- Errors: wrap Python exceptions as Prolog error terms; preserve positions when available.
 - Exceptions: ensure that throws across the bridge are mapped and rethrown on the WAM side; catches in either side correctly intercept and unwind the appropriate frames.

Compilation Patterns
- Disjunction `(A ; B ; C)` in body:
  1) `try_me_else Lb`, emit code for `A`, then `trust_me Lend`
  2) `label Lb`, emit code for `B`, then `trust_me Lend`
  3) `label Lc`, emit code for `C`
  4) `label Lend`
- If‑then‑else `Cond -> Then ; Else`:
  - Evaluate `Cond` under a temp choicepoint; on success, commit and execute `Then`; else, execute `Else`.

Builtin Bridge Details
- Marshal: reify terms in `X` (or via heap deref) into Python AST used by current builtins.
- Invoke: use the existing builtin registry (tree‑walker) based on `name/arity`.
- Import: on success, unify returned bindings/terms back into WAM heap via `unify` primitives.
- Allowlist: start with `var/nonvar/atom/integer/number`, arithmetic `is/2`, and a few list utilities (`length/2`).

Error Mapping
- Map Python exceptions to Prolog error terms consistently: `instantiation_error`, `type_error`, `domain_error`.
- Include location when available (reader position) to improve web REPL messages.

Implementation Tasks
- Extend compiler to emit disjunction/ITE patterns; update assembler and loader.
- Implement `call_builtin` opcode and bridge helper in `bridge.py`.
- Wire selected core builtins through the bridge path; add allowlist.
- Add error mapping utilities (domain/type/instantiation) to WAM layer.
 - Provide `reify_term` and `import_term` helpers for marshalling across bridge.
 - Implement an allowlist registry and metrics to observe bridge frequency.
 - Validate exception crossing: unit tests where `throw/1` originates on either side and `catch/3` resides on the other, ensuring proper unwinding.

Tests
- Disjunction: `p(X) :- (X=a ; X=b).` solutions order.
- ITE: semantics vs. cut-commit equivalence on small examples.
- Bridge: `length/2`, `var/1`, arithmetic via existing tree‑walker.
- Errors: type/domain errors mirrored.

Acceptance Criteria
- Tests pass for the above; WAM can execute common programs via the bridge.

Risks
- Excessive marshalling overhead. Mitigate by limiting builtins used in hot loops initially.

Rollout
- Behind flag; no changes to user entrypoints.

Implementation Checklist
- [ ] Compiler: disjunction + ITE
- [ ] Bridge opcode + helper
- [ ] Error mapping
- [ ] Tests for disjunction/ITE/bridge/errors
