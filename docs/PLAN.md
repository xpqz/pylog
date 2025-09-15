# PLAN.md

*A staged roadmap with aims, deliverables, and acceptance criteria. Each stage is meant to be shippable and to survive subsequent stages unchanged at its interfaces.*

---

## Stage -1 — **Unifier Workbench**

**Aim:** Prove out `Store`, `deref`, `bind`, and `trail` in isolation.

**Deliverables**
- `Store`, `Cell`, `deref()`, `bind()` (with optional occurs-check).
- Trail ops: `('parent', ...)`, `('bind', ...)` and `undo_to(top)`.
- REPL: `?- unify(T1, T2).` shows bindings; `undo()` restores.

**Acceptance criteria**
- Idempotence & symmetry of unification pass.
- Cycles: `X = f(X)` accepted when occurs-check off; rejected when on.
- Aliasing chains restored correctly after `undo_to`.
- Memory stress: 1e6 binds + undos in batches without leaks.

---

## Stage 0 — **Core Shapes & Explicit Stacks**

**Aim:** Eliminate Python recursion from execution; lock down the run loop and choicepoints.

**Deliverables**
- `engine.run(goals)` with explicit goal stack and `ClauseCursor`.
- Choicepoints with `goals` snapshot, clause cursor, `trail_top`, `cut_barrier`.
- `throw/1`/`catch/3` skeleton integrated into loop.

**Acceptance criteria**
- `deep(5000)` variant (iterative) succeeds (no recursion limit).
- Backtracking yields multiple solutions; state is identical after failure.
- `!` removes exactly the right alternatives (green and red cut tests).

---

## Stage 1 — **Minimal ISO (operator-free)**

**Aim:** Make a usable Prolog core: facts, rules, conjunction, disjunction, simple arithmetic via `is/2`.

**Deliverables**
- Built-ins: `true/0`, `fail/0`, `!/0`, `=/2`, `\=/2`, `var/1`, `nonvar/1`, `atom/1`, `integer/1`, `=../2`, `call/1`, `throw/1`, `catch/3`, `is/2`.
- Parser for atoms/ints/vars/lists/structs/clauses (no operators).
- Pretty-printer for solutions (stable var names by creation order).

**Acceptance criteria**
- `append/3`, `member/2`, `reverse/2`, `between/3` pass.
- `catch/3` captures thrown terms; backtracking resumes correctly.
- `once/1` determinism behavior is correct.

---

## Stage 1.5 — **Operators via Reader**

**Aim:** Comfortable syntax without touching the grammar or engine.

**Deliverables**
- Reader that folds operators (`;`, `,`, `->`, arithmetic comparators) from a static `op/3` table into AST.
- Tests for precedence and associativity.

**Acceptance criteria**
- `if -> then ; else` equivalent to desugared clauses.
- Parentheses vs precedence produce identical AST/answers.

---

## Stage 2 — **Indexing**

**Aim:** Speed without changing semantics.

**Deliverables**
- First-argument indexing (principal functor → clause bucket).
- Switch on type (var/atom/int) prefilter.

**Acceptance criteria**
- Scenario tests run faster (measurable), same solutions/ordering (except for performance-dependent traces).

---

## Stage 3 — **Debug & Observability**

Aim: See the machine at work and make failures reproducible.

**Sub-stages**
	- 3.0 Ports Tracer (human & machine formats)
	- 3.1 Snapshots & diffs
	- 3.2 Counters & metrics
	- 3.3 Exporters (constraint graph / call graph)
	- 3.4 Sinks & tooling
	- 3.5 Trace-based tests & replay

**Deliverables**
	- Ports tracer: Emits call/exit/redo/fail events with:
	- step_id (monotonic), depth, frame_id, cp_depth, goal_height, write_stamp.
	- pred_id (interned name/arity) and short pretty form of the current goal.
	- Optional projected bindings (configurable: none | names | names+values, with depth/length caps).
	- Filters: by predicate, depth, ports, step range; and optional sampling (e.g., 1/N).
	- Two encodings:
	  - Pretty stream (human): single-line compact text.
	  - JSONL (machine): one event per line; schema documented in docs/TRACE_FORMAT.md.
	- Engine hooks & APIs:
	  - Global monotonically increasing step_id.
    - Engine(trace=..., debug=...) flags; programmatic trace_on()/trace_off().
	  - spy/1 and unspy/1 (non-interactive analogues) to include/exclude predicates.
	- Snapshots:
	  - debug.snapshot() returns structured data: store_size, trail_len, goal_height, frame_height, cp_depth, write_stamp, choices, frames, candidates_considered, and optional memory hints.
	  - debug.diff(a, b) produces human+JSON deltas for quick regression triage.
	- Metrics & counters (reset per run/solve):
	  - Unifications attempted/succeeded, backtracks taken, cuts executed and alternatives pruned, exceptions thrown/caught.
	- Indexing: candidates considered vs yielded (from Stage 2).
	- Exporters:
	  - export_constraint_graph(program_or_engine) -> .dot (placeholder nodes until Stage 5; respects variable identity).
	  - export_call_graph(program) -> .dot by static analysis of clause heads/bodies.
	- Sinks & tooling:
	  File sinks with rotation: --trace-json out.jsonl, --trace-pretty out.log.
	  - REPL commands: trace(on|off), spy Name/Arity, untrace, unspy.
	  - CI helper: attach last N MB of traces and latest snapshot on failure.
	- Safety & ergonomics:
	  - Pretty-printer depth/length caps and redaction for large terms; no Python object ids leak.
	  - Deterministic variable naming; timestamps optional and monotonic per run.
	  - Tracer is non-recursive and uses bounded buffers.

**Acceptance criteria**
	- With tracing off: ≤ 5% runtime overhead on Stage-1 scenario suite.
	- With pretty trace on: ≤ 20% overhead; with JSONL: ≤ 30% overhead.
	- Traces are deterministic for the same program/query and seed; replay scripts in tools/replay_trace.py reconstruct the last 200 steps.
	- Snapshot/diff across pure backtracking cycles shows no monotonic growth (store/trail/cp/frames/goal heights stable).
	- export_call_graph/constraint_graph produce valid DOT viewable by Graphviz.

---

## Stage 4 — **Attributed Variables (mechanism only)**

**Aim:** Provide the extensibility hook for solvers.

**Deliverables**
- API: `put_attr/3`, `get_attr/3`, `del_attr/2`.
- Registration of `attr_unify_hook(Module, VarOrTerm, Other)`.
- Trail `('attr', ...)` and correct backtracking.

**Acceptance criteria**
- Toy attribute (“must_be_even”): unification fails for odd `is/2` bindings; succeeds for even.
- Aliasing: `X=Y`, setting attr on one is visible on the other; backtracking restores.

---

## Stage 5 — **CLP(FD) Core (no reification yet)**

**Aim:** Working finite domain solver with propagation to fixpoint and labeling.

**Deliverables**
- `Domain` (interval/holes/bitset) with `rev`.
- `PropagationQueue` with high/med/low priorities and self-requeue guard.
- Propagators: comparisons (`#=/2`, `#</2`, `#=</2`, `#>/2`, `#>=/2`) and small linear sums.
- Posting API: attach to vars’ watchers; schedule once.
- `label/1` or `labeling/2` (var selection: `first`/`ff`; value choice: `indomain_min`/`bisect`).

**Acceptance criteria**
- Interval tightening: `X in 1..10, X #> 5` ⇒ `X in 6..10`.
- Chains: `X #< Y, Y #< Z, Z in 1..5` prunes X/Y correctly.
- Confluence: shuffled posting order yields same domains (property test).
- Labeling enumerates all solutions on small problems.

---

## Stage 5.5 — **Reification**

**Aim:** Correct T/F/U semantics linking Booleans to constraints.

**Deliverables**
- `B #<==> C`, `#==>`, `#<==` (Boolean ↔ constraint) with:
  - Directional posting (`B=1` ⇒ post `C`; `B=0` ⇒ post `¬C`).
  - Entailment tests per constraint; dis-entailment detection.
  - Guard against self-notify loops.

**Acceptance criteria**
- `B #<==> (X #= 3), X in 1..5, B=0` ⇒ `X ≠ 3`.
- Bounds entailment: `X in 5..9, Y in 1..4, B #<==> (X #> Y)` ⇒ `B=1`.
- Unknown remains unknown until labeling/propagation decides.

---

## Stage 6 — **Global constraints**

**Aim:** Practical scalability for common patterns.

**Deliverables**
- `all_different/1`:
  - Stage 6.0: pairwise `#\=/2` for N≤6.
  - Stage 6.5: bounds-consistent Hall intervals (O(n log n)).
- `global_cardinality/2` (relaxed).

**Acceptance criteria**
- Pigeonhole failures detected early.
- Hall interval tests prune mins/maxes as expected.

---

## Ongoing non-functional criteria (all stages)

- **Centralized mutation only via `bind()`** (and domain updates inside CLP(FD) posting/props).  
- **Every backtrackable change** pushes a trail entry; `undo_to(top)` restores exactly.  
- **No Python recursion** in the engine.  
- **Tests**: Each PR adds/updates unit tests; scenarios for regressions; randomized confluence checks for Stage 5+.  
- **Docs**: Keep `ARCH.md` and `PLAN.md` updated if any public shape changes.

---

## Practical prompts for LLM-assisted implementation

- *Implement a stub:*  
  “Fill in `unify.bind()` to unify two deref’d terms. You may only modify this function. Use `trail.append(('bind', vid, old_cell))` when binding a var. Handle occurs-check if `config.occurs_check`.”

- *Add a propagator:*  
  “Implement `CompareGE.run()` which enforces `X #>= Y + K`. Use bounds reasoning. On domain change, push `('domain', vid, olddom)` and bump `dom.rev`. Schedule watchers via `queue.schedule(pid, prio='high')`.”

- *Write a property test:*  
  “Create a randomized test ensuring posting order independence: generate 5 vars in 1..9 and constraints `[X0<X1, X1<X2, all_different(V), sum(V)#=15]`, shuffle postings, and assert final domains equal a canonical snapshot.”
