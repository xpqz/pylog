all_different/1 — Design and Implementation Plan

Overview

- Goal: Provide an efficient, correct all_different/1 CLP(FD) global constraint.
- Scope: Baseline pruning (value elimination) and improved bounds consistency via Hall-interval pruning; full integration with watchers, trail, hooks, and labeling.

Public API and Semantics

- Predicate: all_different(List)
  - List: Prolog list in either [X,Y|T] or '.'/2 form.
  - Elements: FD variables and/or integers.
  - Semantics: All elements must take pairwise distinct integer values.
  - Immediate failure if the list contains two equal integers, or an FD var conflicts with a fixed integer once domain is singleton.
  - Type guard: non-integer ground terms cause failure for FD context (we only support integers in CLP(FD)).

Engine Integration

- Register builtin in prolog/engine/engine.py within _register_builtins:
  - ("all_different", 1) -> _builtin_all_different in prolog/engine/builtins_clpfd.py
- Parsing: no grammar change required; standard predicate call.

Constraint Posting (builtin)

- Implement _builtin_all_different(engine, list_term) in prolog/engine/builtins_clpfd.py:
  - Parse list to collect:
    - fixed_values: integers and bound FD variables; deduplicate and fail on duplicates.
    - var_ids: dereferenced unbound var IDs; include even without current domain.
  - Create a single propagator instance that captures var_ids and fixed_values.
  - Register with propagation queue: pid = queue.register(prop).
  - For each var_id: add_watcher(store, var_id, pid, Priority.MED, trail).
  - Schedule and run to fixpoint: queue.schedule(pid, Priority.MED); queue.run_to_fixpoint(...).

Propagator Design

- Location: prolog/clpfd/props/all_different.py
- Factory: create_all_different_propagator(var_ids, fixed_values=()) -> callable(store, trail, engine, cause)
- On each run:
  1) Gather state
     - For each vid: get_domain; collect singletons and general domains.
     - Combine singletons with fixed_values; fail on duplicates.
  2) Baseline pruning (value-elimination)
     - For each non-singleton domain, remove all fixed/singleton values.
     - set_domain when changed; accumulate changed IDs.
  3) Hall-interval pruning (bounds consistency, improved)
     - Compute candidate intervals from unique bounds across current domains.
     - For each interval [a,b]:
       - tight = count of vars whose domain is a subset of [a,b].
       - If tight > size([a,b]) -> fail (pigeonhole principle).
       - If tight == size([a,b]) -> remove [a,b] from other vars (requires remove_interval on Domain).
  4) Return ('ok', changed or None).
- Priority: Priority.MED by default; can be elevated to Priority.HIGH if we want earlier pruning.

Domain Helpers

- Extend prolog/clpfd/domain.py:
  - remove_interval(low, high) -> Domain: subtract closed interval [low,high] from the domain.
  - Optional future: subtract(other_domain) -> Domain for general difference.
- Preserve immutability and revision logic consistent with existing Domain operations.

Watchers and Propagation Queue

- Register propagator once; add watchers on all var_ids so any domain update wakes all_different.
- Rely on existing queue dedup/escalation to avoid flooding.
- Existing set_domain wake-ups (and in/2, #=, #<, etc.) will schedule the propagator appropriately.

Unification Hooks Interaction

- Hooks already handle merging attributes and watcher sets on var-var aliasing (Stage 5 hooks).
- all_different’s watcher set will be merged automatically during unification; no extra work needed.

Labeling Integration

- No special handling required.
- all_different increases watcher counts, improving most_constrained selection.
- With value elimination + Hall pruning, labeling explores dramatically fewer branches (e.g., Sudoku row).

Edge Cases

- Duplicate integers in the list: fail immediately.
- Mixed ints and variables: remove fixed ints from variable domains; fail if any domain becomes empty.
- Bound variables: treat as fixed ints if bound to Int; otherwise fail (non-integer).
- Variables without domains: still add watchers; pruning begins once domains are posted.

Testing Strategy

- Unit tests (new):
  - Success with domains: X,Y,Z in 1..3, all_different([X,Y,Z]), label([...]) → all permutations.
  - Duplicate integers: all_different([1,2,1]) fails.
  - Pruning chain: X in 1..3, Y in 1..3, Z in 1..3, all_different([X,Y,Z]), X=1, Y=2 narrows Z to 3.
  - Hall-interval case: X∈{1..2}, Y∈{1..2}, Z∈{1..3} narrows Z to 3 without labeling.
  - Idempotence: posting twice yields identical fixpoint and no memory accumulation.
  - Backtracking: domains restored; no orphan watchers.
- Integration:
  - Sudoku row: replace pairwise #\=/2 with all_different/1 and verify solutions + improved runtime.

Performance Expectations

- Baseline value elimination: O(n·D) per fixpoint cycle; very fast for typical n.
- Hall-interval pruning: naive O(n^2) over candidate intervals; acceptable for small/medium lists (e.g., 9 for Sudoku).
- Propagation queue’s dedup + priority staging keep cost under control.

Incremental Implementation Plan

1) Add builtin entry in engine and skeleton _builtin_all_different in builtins_clpfd.py.
2) Implement value-elimination propagator; add watchers; schedule to fixpoint.
3) Add Domain.remove_interval(low, high) and wire Hall-interval pruning; unit tests for both.
4) Add scenario test using all_different/1 (Sudoku row) to confirm improved pruning.
5) Docs and examples; review for consistency with existing CLP(FD) API.

Acceptance Criteria

- Correctness: New unit tests pass; existing CLP(FD) suites remain green; domains only shrink; idempotent posting; trail restores.
- Integration: Hooks preserve watchers across unification; labeling strategies unaffected; queue behavior stable.
- Performance: Sudoku-row style constraints solve measurably faster than pairwise #\=/2.

File Touch List (anticipated)

- prolog/engine/engine.py: register builtin ("all_different", 1)
- prolog/engine/builtins_clpfd.py: _builtin_all_different implementation
- prolog/clpfd/props/all_different.py: propagator factory and logic
- prolog/clpfd/domain.py: remove_interval(low, high) helper
- prolog/tests/unit/test_clpfd_all_different.py: unit tests
- prolog/tests/scenarios/...: optional Sudoku row with all_different/1

Future Extensions (not in initial scope)

- Regin’s filtering (matching-based) for stronger pruning (AC on alldiff)
- Value-precedence constraints for symmetry breaking
- Generalization toward global_cardinality/2

