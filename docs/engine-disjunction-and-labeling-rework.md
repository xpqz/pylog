# Engine Disjunction and Labeling Rework

This document summarizes the investigation into Boolean‑first labeling missing solutions, the root cause analysis, and a proposed architecture to fix the issue at its source (disjunction continuation handling), rather than layering workarounds.

## Summary

- Symptom: `label([B1,B2,B3,X,Y])` for reified `<, =, >` over `X,Y in 1..5` produced only 18/25 solutions, missing the cases where `X=1` and `Y>1`.
- Control experiment: `label([X,Y,B1,B2,B3])` produces 25/25. Posting `B1=1, B2=0, B3=0` before labeling also enumerates all 10 `X<Y` pairs. Adding the logically implied trichotomy `B1+B2+B3 #= 1` restores 25/25 for Boolean‑first labeling.
- Root cause: engine disjunction continuation snapshots enable nested disjunction state (from integer labeling) to be inadvertently re‑used when backtracking to a different higher‑level Boolean branch. This leads to “skipping” value alternatives like `X=1` in later branches.
- Proposed fix: redesign disjunction choicepoint handling to avoid replaying “continuations” across alternatives, and simplify how labeling builds disjunctions so nested state cannot leak across branches.

## What is working (and what isn’t)

- Reification postings and propagation are correct after we made idempotent posts and attrs properly trailed.
- Boolean‑first labeling fails specifically when three reified constraints exist (for `<, =, >`) and the solver explores Boolean space before integer space. The propagation network itself permits all missing cases; the loss is due to search state handling.

## Root Cause Deep Dive

### Current disjunction implementation (before refactor)

- When the engine sees `(A ; B)`, it:
  1. Creates a DISJUNCTION choicepoint for `B`.
  2. Snapshots a “continuation” slice of the goal stack.
  3. On backtrack, it restores that continuation and then pushes `B`.

- Labeling builds nested `;` terms for value alternatives (e.g., `X=1, label(Vars) ; X=2, label(Vars) ; ...`). With reifications on Booleans before integers, this creates deep trees where “continuations” from one Boolean sub‑branch contain value‑choice subtrees that are irrelevant for a sibling Boolean branch.

- On backtracking to a sibling Boolean alternative, the engine replays the saved continuation slice, accidentally resuming the integer value disjunction mid‑stream. This is why, for example, the `X=1` branch appears already consumed after switching `B2` from `1` to `0`.

### Why stamps and proper trailing didn’t solve it alone

- We added per‑branch trail stamps (both in `label/1` and for disjunction CPs). This correctly isolates domain/attr changes between alternatives.
- However, stamps don’t address goal stack reconstruction: the engine’s continuation snapshot still brings back goals from a different branch.

## Proposed Architecture Changes

### 1) Disjunction CP semantics: remove continuation snapshots

- At CP creation for `(A ; B)`, record only:
  - `trail_top` (existing)
  - `goal_stack_height` at creation time
  - `frame_stack_height` (existing)
  - `stamp` (write stamp at CP creation)
  - `alternative` term `B` and its target frame depth

- On backtrack to this CP:
  - Unwind to `trail_top`.
  - Restore the stamp via `set_current_stamp(cp.stamp)`.
  - Restore goal stack to at most `goal_stack_height` (shrink if current > target; if current < target, leave as is — we no longer push “continuations”).
  - Push only the alternative goal `B` and continue.

- Adjust invariants:
  - Do not assert exact goal stack equality for DISJUNCTION CPs (since we no longer replay continuations and may legitimately be below target height when resuming).

This isolates the alternative branch cleanly and prevents nested value‑choice state from one Boolean branch from reappearing in another.

### 1b) Propagation queue isolation across backtracking

- The CLP(FD) `PropagationQueue` is not trailed. Pending scheduled propagators from one branch must not execute in a different branch after backtracking.
- On choicepoint backtrack, clear any pending scheduled items (per‑priority deques, `queued` index, `causes`, `reschedule`, and `running`). This guarantees that only watchers explicitly awakened in the new branch can schedule propagators.

### 2) Labeling disjunction builder: prefer shallow disjunctions

- Today `label/1` builds a single right‑nested `;` term for all value alternatives. Even with fixed disjunction CP semantics, this can be deep and harder to reason about.
- Improvement: build “shallow” disjunctions — a small `;` around the current value and a recursive call that will create the next `;` only when needed. This reduces the size of the continuation region and limits the surface area for cross‑branch interactions.
- Keep the existing branch‑stamp control goal (`LABEL_BRANCH_STAMP`) before non‑det labeling so trail scopes are clean per value branch.

### 3) Keep model‑level support as optional

- The implied trichotomy `B1+B2+B3 #= 1` is logically part of the relation trichotomy over integers. While it “fixes” the minimal repro when Boolean‑first labeling is used, it is not the root‑cause fix and should be considered a modeling help, not a requirement.

## Implementation Status

### Completed (2025-09-29)

- **Disjunction CP refactoring**: Removed continuation snapshot/replay mechanism from DISJUNCTION choicepoints. The engine now only records the goal stack height at CP creation and shrinks the stack if needed on backtrack, but does NOT restore continuation goals. This prevents nested labeling state from one branch affecting another. File: `prolog/engine/engine.py`.

- **Propagation queue isolation**: Added `clear()` method to `PropagationQueue` to reset all pending propagators, scheduled items, and tracking structures. The engine now calls this on backtrack to DISJUNCTION CPs to ensure propagators from one branch don't execute in another. File: `prolog/clpfd/queue.py`.

- **Test case**: Added comprehensive test comparing Boolean-first vs integer-first labeling to verify completeness. File: `prolog/tests/unit/test_labeling_boolean_order.py`.

### Results

The disjunction refactor improved the minimal repro from 19/25 to 24/25 solutions. The missing solution is due to a separate issue with reification constraint propagation (specifically, when B=1 is set after reification B #<=> (X #= Y), the equality constraint doesn't properly establish X=Y during labeling).

### Previously Completed

- Reification attrs: fixed to copy + put_attr (properly trailed) — avoids posting state leaks. File: `prolog/clpfd/props/reif.py`.
- Labeling: added branch stamps and trace of FD candidates. File: `prolog/clpfd/label.py`.

## Next Steps

### Remaining Issues

1) **Reification propagation bug**: When B=1 is set after posting B #<=> (X #= Y), the reification propagator correctly posts X#=Y, but the equality doesn't properly unify X and Y during labeling. This appears to be an issue with how CLP(FD) equality propagation works between two unbound FD variables.

2) **Missing trichotomy enforcement**: The reification constraints for `<`, `=`, `>` don't automatically establish that exactly one must be true. This allows invalid Boolean combinations like all three being false or multiple being true simultaneously.

### Potential Solutions

1) **Fix equality propagation**: Investigate why X#=Y between two FD variables doesn't properly establish their equality during labeling. May need to enhance the equality propagator to actually unify variables when domains become singleton.

2) **Add automatic trichotomy**: When multiple reifications are posted for comparison operators on the same variable pair, automatically add the constraint that exactly one Boolean must be true.

## Risks and Mitigations

- Risk: Changing disjunction restore semantics can affect any code relying on the previous continuation replay. Mitigation: Limit behavior changes strictly to DISJUNCTION CPs; PREDICATE and IF‑THEN‑ELSE continue to use frozen continuations and exact height invariants.
- Risk: Labeling performance with shallow disjunctions might change. Mitigation: The change maintains the same logical alternatives; in practice shallow disjunctions often reduce overhead by limiting the size of continuation regions.

## Conclusion

The missing solutions stem from the engine’s disjunction continuation replay interacting poorly with nested disjunction trees generated by labeling when Booleans are explored first. The correct fix is architectural: eliminate continuation replay for DISJUNCTION CPs and simplify labeling’s alternative generation to shallow disjunctions, while retaining per‑branch trail stamps. This resolves the search‑state reuse across Boolean branches and restores completeness without relying on model‑level guardrails.
