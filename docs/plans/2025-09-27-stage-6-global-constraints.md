# Stage 6: CLP(FD) Global Constraints Implementation Plan

## Overview

Introduce a core set of global constraints to PyLog’s CLP(FD) layer to enable
expressive modeling and stronger propagation than pairwise decompositions. These
constraints are standard across CP systems and provide substantial pruning on
real-world problems (scheduling, routing, allocation).

Initial target set (excluding `all_different/1`, which is already implemented):
- element/3 (Z is the Ith element of a list)
- global_cardinality/2 (GCC)
- nvalue/2 (number of distinct values)
- cumulative/1 (unary resource scheduling)
- lex_chain/1 (lexicographic ordering over a sequence of vectors)
- table/2 (positive table, compact) — optional stretch goal

We aim for sound, efficient propagators with at least bounds consistency (BC),
upgrading to range or generalized arc consistency (GAC) when tractable.

## Current State Analysis

What we have:
- CLP(FD) core with domains, propagation queue, and attribute hooks
  - Domains as disjoint intervals with normalization and revision tracking
  - PropagationQueue with priorities (HIGH/MED/LOW)
  - Propagators for equality, comparisons, linear sums
  - all_different/1 implemented with Hall-interval pruning and performance tests
- Reification (Phase 5) complete; entailment detection framework in place
- Labeling strategies and integration tests (scenarios) available

Gaps for global constraints:
- No infrastructure for “vector constraints” ergonomics (helpers for lists of vars)
- No posting helpers to attach watchers across variable arrays efficiently
- No specialized filtering algorithms for element/GCC/cumulative/etc.

Key observations from codebase:
- Builtins are registered in `engine._register_builtins()` and implemented in
  `prolog/engine/builtins_clpfd.py`; propagators live under `prolog/clpfd/props/`
- The queue and watcher sets are ready to support multi-variable constraints
  (already used for all_different)

## Desired End State

Add well-documented, unit-tested global constraints with clear semantics,
propagation strength statements, and performance baselines:
- element/3: BC (with index-domain pruning) and membership pruning on Z
- global_cardinality/2: at least BC on counts and variable domains
- nvalue/2: BC via GCC relaxation; tight bounds on N with pruning
- cumulative/1: BC with time-table reasoning for unary resources (no preemption)
- lex_chain/1: pairwise lex ordering with filtering (bounds-based)
- table/2: positive table with compact scheme; GAC via simple support lists (STR-lite)

Integration goals:
- Clean posting APIs/builtins, coherent error handling, domain checks
- Work with reification (B #<=> global(...)) via entailment stubs where feasible
- Maintain propagation priorities consistent with existing constraints

## What We’re NOT Doing (Stage 6)

- No advanced cumulative filtering (edge-finding, energetic reasoning)
- No disjoint2/rectangle packing or circuit/TSP constraints
- No negative/short tables or compressed automaton constraints (REGULAR)
- No symmetry-breaking strategies beyond lex_chain
- No global constraint learning/cut generation

These can be tackled in later stages once the foundation is stable.

## Implementation Approach

General pattern (mirrors existing props):
- Add builtin wrapper(s) in `prolog/engine/builtins_clpfd.py`
  - Parse and validate terms (lists, vectors, integers)
  - Deref variables, collect var IDs, initial fixed values
  - Register a propagator with the queue
  - Attach watchers to all involved variables at suitable priority
- Implement propagator factories in `prolog/clpfd/props/*.py`
  - Keep state minimal and immutable where practical (captured closures)
  - Use Domain operations: intersect, remove_value, remove_(lt/le/gt/ge), etc.
  - Return ("ok", changed_vars) or ("fail", None); schedule follow-ups via queue
- Add targeted unit tests and scenario-style tests with performance expectations

### Constraint-specific designs

1) element/3 — Z is the Ith element of List
   - Form: element(I, List, Z) or Z #= element(I, List)
   - Consistency: Bounds + membership (BC) initially
   - Filtering:
     - Prune Z’s domain to union of values at feasible indices I ∈ D(I)
     - Prune I’s domain to indices whose list-value intersects D(Z)
     - If I is singleton k, then Z’s domain := Z ∩ D(List[k])
     - If Z is singleton v, remove indices k where D(List[k]) ∩ {v} = ∅
   - Watchers: I and all list variables (scalable for moderate list sizes)

2) global_cardinality/2 — GCC
   - Form: global_cardinality(Vars, Pairs) with Pairs like [V1-C1, V2-C2, ...]
   - Consistency: BC (count lower/upper bounds, prune impossible values)
   - Filtering:
     - Maintain min/max feasible occurrences for each value
     - If value v’s min occurrences exceed count, fail; if max < required, fail
     - For each variable Xi, if value v cannot achieve remaining capacity, prune v from D(Xi)
   - Future: flow-based (Régin) for stronger GAC; out-of-scope in Stage 6

3) nvalue/2 — number of distinct values
   - Form: nvalue(Vars, N)
   - Consistency: BC via GCC relaxation
   - Filtering:
     - Lower bound on N from pigeonhole-like reasoning over disjoint domains
     - Upper bound on N by union-of-domains cardinality
     - If N is fixed, reduce to GCC with [value: 0..1] style and propagate

4) cumulative/1 — unary resource scheduling
   - Form: cumulative(Tasks) with tasks of shape task(S, D, E, R, C)
           Start S, Duration D, End E, Resource usage R, Capacity C
   - Assumptions: non-preemptive tasks, fixed capacity C (can be shared)
   - Consistency: BC using time-table envelope (no edge-finding yet)
   - Filtering:
     - Build mandatory parts from (S,D) bounds and aggregate resource profiles
     - For each time window, ensure sum of minimal usages ≤ capacity
     - Prune variable bounds to avoid profile overloads (bounds tightening)
   - Priorities: MED or HIGH depending on bound tightening frequency

5) lex_chain/1 — lex ordering over sequences
   - Form: lex_chain([X1, X2, ..., Xk]) where Xi are equal-length vectors
   - Consistency: BC; enforce Xi ≤lex Xi+1 with element-wise bounds filtering
   - Filtering:
     - Propagate prefix equalities and first strict position using bounds
     - Use pairwise chain; complexity linear in number of vectors

6) table/2 (optional) — positive table of allowed tuples
   - Form: table(Vars, Tuples)
   - Consistency: GAC-lite via supports (simple STR approach)
   - Filtering:
     - Precompute supports per (var, value); remove values without any support
     - Rescan supports on domain changes (optimize with revision counters)

## Phased Delivery

We’ll deliver in small, verifiable increments. Each phase includes tests and docs.

Phase 6.0 – Foundations and Helpers
- Add vector helpers in `builtins_clpfd.py` (parse lists of vars/ints)
- Common utilities for posting array constraints + attaching watchers
- Test harness helpers for building variable arrays and domains

TODO (6.0)
- [ ] Helpers: list/vector parsing validates shapes and types
- [ ] Watcher attach/detach helpers with priorities
- [ ] Unit tests for helpers (happy path + errors)
- [ ] Docs: developer notes on posting multi-var constraints

Phase 6.1 – element/3
- Implement `prolog/clpfd/props/element.py`
- Builtin: `element(Index, List, Value)` and `Z #= element(I, List)`
- Unit tests: membership pruning, index pruning, singleton cases, failures
- Scenario tests: basic modeling examples (lookup tables)

TODO (6.1)
- [ ] Builtin validates (Index, List, Value) and deref rules
- [ ] Propagator prunes Z to union(List[I]) and I to feasible indices
- [ ] Handles singleton I and/or Z efficiently
- [ ] Unit + scenario tests; docs + examples

Phase 6.2 – GCC and nvalue
- Implement `prolog/clpfd/props/gcc.py` and `prolog/clpfd/props/nvalue.py`
- Builtins: `global_cardinality/2`, `nvalue/2`
- Unit tests: count feasibility, pruning on counts, N bounds tightening
- Scenario tests: bin-packing-lite counting, grouping constraints

TODO (6.2)
- [ ] GCC: counts parsed (value–count pairs) and validated
- [ ] GCC: BC on counts and values; early detection of infeasible totals
- [ ] nvalue: N lower/upper bounds; reduces to GCC when N is fixed
- [ ] Unit + scenario tests; docs + examples

Phase 6.3 – cumulative (time-table BC)
- Implement `prolog/clpfd/props/cumulative.py` (unary resource)
- Builtin: `cumulative/1`
- Unit tests: overload detection, mandatory parts, bound tightening
- Scenario tests: small job-shop rows, simple RCPSP-like fragments

TODO (6.3)
- [ ] Task parsing: task(S,D,E,R,C) with consistent domains (E=S+D)
- [ ] Time-table envelope from mandatory parts; no overloads
- [ ] Bounds tightening where envelope conflicts
- [ ] Unit + scenario tests; docs + examples

Phase 6.4 – lex_chain
- Implement `prolog/clpfd/props/lex.py`
- Builtin: `lex_chain/1`
- Unit tests: pairwise ordering, equal vectors, strictness at first diff
- Scenario tests: symmetry-breaking in small models

TODO (6.4)
- [x] Parse list of equal-length vectors (lists of vars/ints)
- [x] Enforce pairwise lex ordering with bounds-based filtering
- [x] Unit + scenario tests; docs + examples

Phase 6.5 – table (optional stretch)
- Implement `prolog/clpfd/props/table.py` (positive tables only)
- Builtin: `table/2`
- Unit tests: support-based pruning, failure on empty support
- Scenario tests: CSP instances expressible via extensional constraints

TODO (6.5)
- [ ] Parse static tuple list; precompute supports
- [ ] Remove values without any supporting tuple (GAC-lite)
- [ ] Unit + scenario tests; docs + examples

Documentation & Examples (rolling)
- MkDocs pages under `mkdocs/docs/clpfd/` for each global
- Reference updates: operators/grammar where relevant

## Verification & Benchmarks

Unit Tests
- Soundness: no missed failures; pruning is monotonic and reversible via trail
- Correctness on edge cases: empty lists, invalid specs, degenerate bounds

Performance
- Compare against decomposed models (e.g., pairwise disequalities vs GCC)
- Scenario tests (already present: SEND+MORE, Sudoku row) extended for new globals
- Targets: pruning quality demonstrably better than naive decompositions

Reification Hooks (basic)
- Provide entailment stubs where cheap (e.g., element with fixed I)
- Allow B #<=> global(...) round-trip posting without loops (reuse Phase 5 infra)

## Risks and Mitigations

- Overly aggressive pruning (incorrect):
  - Mitigation: begin with BC; add property-based tests; guard with domain checks
- Performance regressions due to large watcher sets:
  - Mitigation: only watch variables that affect current filtering (e.g., for element, skip unrelated list entries whose indices are pruned)
- API ergonomics for list-based constraints:
  - Mitigation: add robust parsing/validation helpers and clear error messages

## Out-of-Scope Follow-ups (Stage 6.x+)

- cumulative: edge-finding, not-first/not-last, energetic reasoning
- GCC: flow-based GAC (Régin) with residual graphs
- table: STR2/STR3 improvements, negative/short tables
- disjoint2 (rectangle packing), circuit, path constraints, regular/automaton

## Success Criteria

- All new unit tests pass; existing suites remain green
- Documented semantics + examples for each added global
- Measurable pruning improvements vs decompositions on scenario tests
- No observed regressions in propagation performance on existing models

## Implementation Notes (Files & Priorities)

- Builtins: `prolog/engine/builtins_clpfd.py`
  - Add posting functions for element, gcc, nvalue, cumulative, lex_chain, table
  - Choose priorities: HIGH for constraints that immediately tighten bounds broadly (e.g., element when I is singleton); MED for others

- Props: `prolog/clpfd/props/`
  - `element.py`, `gcc.py`, `nvalue.py`, `cumulative.py`, `lex.py`, `table.py`
  - Follow existing style (factories returning closures), trail-safe domain updates

- Docs: `mkdocs/docs/clpfd/*.md`, reference pages

---

References (state of the art)
- P. Van Hentenryck, The OPL Optimization Programming Language — global constraints catalog
- C. Bessiere, The Constraint Satisfaction Problem: Formalization and Techniques (surveys GAC/BC)
- N. Beldiceanu & M. Carlsson, Global Constraint Catalog (element, gcc, cumulative, etc.)
- J.-C. Régin, “A filtering algorithm for global sequencing constraints” (GCC)
- C. Schulte & G. Tack, “Views and iterators for generic constraint propagation” (design insights)
