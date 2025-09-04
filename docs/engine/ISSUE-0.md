# Task 0: single-loop, non-recusive VM

## Phase A — Foundations

### T0.1 – VM state & step contract (design doc) — S
- **Goal**: Lock down the single-loop VM invariants.
- **Deliverables**:  docs/vm-loop.md describing:
	- State: (goal_stack, frame_stack, choicepoint_stack, trail, fd_store_ref, solution_count, config)
	- Step rules, backtracking rule, and error handling.
- **Accept**:  Doc must be reviewed.

### T0.2 – Core runtime types — S
- **Goal**: Introduce minimal structs.
- **Deliverables**: 
	- engine/runtime.py: Frame(cut_barrier, pred, env), Choicepoint{kind, resume, trail_top, cut_parent}, GoalStack, Trail with write-stamp + tests for unwind_to.
- **Accept**:  Trail tests pass; restore semantics verified.

## Phase B — Loop & Backtracking

### T0.3 – Engine.run() single iterative loop — M
- **Goal**: Add the non-recursive interpreter loop.
- **Deliverables**:  Engine.run() with dispatch(goal); no Python recursion in control flow.
- **Accept**:  Trivial facts program returns all answers.

### T0.4 – Uniform backtracking via CPs — M
- **Goal**: Replace mutual recursion with backtrack().
- **Deliverables**:  backtrack() pops CPs, unwinds trail, calls resume(); tests for multi-fact predicates.
- **Accept**:  3-fact predicate yields 3 answers, in order.

## Phase C — Calls & Clauses

### T0.5 – Predicate choicepoints (clause cursor) — M
- **Goal**: Clause alternatives are a CP kind.
- **Deliverables**:  On CALL → push Frame(cut_barrier=len(cp_stack)), create predicate CP (pred_ref, next_clause_idx), try head unify, push body goals.
- **Accept**:  p(1). p(2). produces [1,2].

### T0.6 – Iterative head unification — S
- **Goal**: Unifier is stack-based; all binds go through bind() with stamp-guarded trailing.
- **Deliverables**:  Unit tests for atoms, structs, and var sharing.
- **Accept**:  Tests green; no recursion.

### T0.7 – Body scheduling — S
- **Goal**: Push clause body goals (reverse order) onto goal_stack.
- **Deliverables**:  Conjunction respects order.
- **Accept**:  (a, b) executes a then b.

## Phase D — Builtin Dispatch (minimal to keep forward progress)

### T0.8 – Builtin adapter (det/semidet stub) — M
- **Goal**: Single call path for builtins; nondet to be handled in later task.
- **Deliverables**:  execute_builtin(goal) handling true/0, fail/0 and simple arithmetic comparisons.
- **Accept**:  Tests with true/0/fail/0 + a simple builtin pass under the loop.

## Phase E — Control Constructs (baseline)

### T0.9 – Disjunction CPs (A ; B) — M
- **Goal**: Implement disjunction CP kind.
- **Deliverables**:  Push CP for B, then schedule A.
- **Accept**:  Both branches produced in order; backtracking works.

### T0.10 – If–Then–Else skeleton (A -> B ; C) — M
- **Goal**: Choose branch correctly (full cut semantics later).
- **Deliverables**:  Execute A; on success schedule B, else C.
- **Accept**:  Correct branch selection; no recursion paths.

## Phase F — Forward-only Migration (no legacy path)

### T0.11 – Route all entry points to new loop — S
- **Goal**: Make the loop the only engine.
- **Deliverables**:  Replace engine instantiation and runners to point at Engine.run(); update any factory functions.
- **Accept**:  Repo runs exclusively via the loop.

### T0.12 – Remove legacy recursion code — M
- **Goal**: Delete _try_clause, _backtrack, and any helpers only used by legacy path.
- **Deliverables**:  Code removal + imports cleanup.
- **Accept**:  grep shows no references; tests compile and run.

### T0.13 – Test harness & fixtures update — S
- **Goal**: Ensure tests call the new engine directly.
- **Deliverables**:  Update pytest fixtures/utilities; remove any legacy skips.
- **Accept**:  Test suite executes against the new loop end-to-end.

### T0.14 – Migration notes + changelog — S
- **Goal**: Document the one-way switch.
- **Deliverables**:  docs/ENGINE_MIGRATION.md + CHANGELOG.md entry (breaking change: legacy engine removed).
- **Accept**:  Docs present and linked from README.

### T0.15 – Guardrails: recursion sentinels — S
- **Goal**: Ensure we never regress into recursion.
- **Deliverables**:  Lightweight assertion/metric: e.g., a module-level “in_step_call_depth” that must remain 0; CI check for sys.getrecursionlimit() proximity not necessary, but add a smoke test with 5k clauses and 10k-depth list.
- **Accept**:  Guard in place; stress test passes.

### T0.16 – Dead-code & import pruning — S
- **Goal**: Keep the codebase lean.
- **Deliverables**:  Remove unused exceptions, adapters, and legacy helpers; run static analysis (ruff/mypy if configured).
- **Accept**:  Linter passes; no unused symbols.

## Dependencies & order of work
1. T0.1–T0.2
2. T0.3–T0.4
3. T0.5–T0.7
4. T0.8–T0.10
5.T0.11–T0.16

## Acceptance gates for Task 0 completion
- No Python recursion in engine control paths (verified by code inspection + the 5k/10k stress test).
- All existing facts/controls/basic builtins tests green under the loop.
- Legacy engine code fully removed.
- Docs updated.
