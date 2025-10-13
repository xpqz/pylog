# WAM Implementation Roadmap

This document outlines a pragmatic, incremental path to evolve PyLog from the current tree‑walking engine to a Warren Abstract Machine (WAM)–based runtime. The plan emphasizes correctness, observability, and gradual integration, so we can ship value continuously without destabilizing existing users.

## Goals
- Preserve current user‑visible semantics (ISO‑ish where we already are) while improving performance and scalability.
- Keep the pure‑Python story viable (local dev, PyPI, Pyodide/web) and avoid native deps.
- Maintain feature parity during the transition (builtins, CLP(FD) interop, tracing, error reporting).
- Build strong test coverage and microbenchmarks to catch regressions early.

## Guiding Principles
- Increment first; optimize later. Prioritize a clear, correct emulator and compiler before micro‑tuning.
- Dual‑engine strategy. Allow switching per‑query (or per module) between tree‑walking and WAM while we ramp up.
- Make state explicit. Keep heap/stack/trail/registers observable (debug dumps, tracer hooks) to simplify debugging.
- Ship in thin, verifiable slices. Add small instruction clusters with targeted tests and golden examples.

---

## Phase 0 — Foundations (design + scaffolding)
Deliverables:
- Design notes: instruction set baseline, memory layout, tagging scheme (bit layout), and registers.
- Documentation artifacts: memory layout diagram (heap/stack/trail/registers), instruction reference ([wam-instruction-set.md](wam-instruction-set.md)), and state transition examples for key ops.
- Emulator skeleton with a fetch‑decode‑dispatch loop and instrumentation points.
- Debug/trace hooks + minimal pretty printers for WAM state.

Tasks:
- Choose variant (Aït‑Kaci WAM baseline with minor modernizations).
- Tagging + memory layout:
  - Heap cells for REF, STR, CON (ATOM/INT/FLOAT), LIST.
  - Choicepoint stack (B), environment stack (E/Y), trail (TR).
  - Registers: `X[i]` (argument/temporary), `Y[i]` (environment), `H`, `HB`, `B`, `E`, `CP`, `P`, `TR`.
- Define Python data structures with small, typed tuples/classes; document tag bits layout explicitly (even if represented as Python objects) and note trade‑offs (boxed Python objects vs tagged integers for small ints).
- Build a minimal WAM module boundary: `wam/heap.py`, `wam/instructions.py`, `wam/machine.py`.
- Prototype instruction dispatch approaches and benchmark on micro loops:
  - Option A: dict dispatch (`opcode -> handler`), highly debuggable.
  - Option B: enum + list/array dispatch, faster but less flexible.
  - Option C: structural `match/case` (Python 3.11) for readability; compare.

Success criteria:
- Machine boots, can step N instructions, prints consistent snapshots.
 - Baseline dispatch benchmark recorded with methodology and chosen default (dict dispatch acceptable initially).

---

## Phase 1 — Core unification + term access
Deliverables:
- Deterministic unification via classic WAM sequences.
- Unit tests for atoms, integers, small structs/lists, and variable binding/trailing.

Instructions (initial set):
- Get/Put for arguments: `get_variable`, `get_value`, `put_variable`, `put_value`, `get_constant`, `put_constant`.
- Structures/lists: `get_structure`, `unify_variable`, `unify_value`, `put_structure`, `set_variable`, `set_value`.
- Helpers: deref, trail, occurs‑check (optional flag), list cell sugar.

Success criteria:
- Hand‑assembled code loads and proves simple queries (e.g., `p(a).`, `p(f(X)).`).
- 100+ targeted unit tests pass for unification paths (ground/var/compound).

---

## Phase 2 — Control flow, environments, backtracking
Deliverables:
- Proper call/return, environment frames, choicepoints, and cut support.

Instructions:
- Control: `call`, `execute`, `proceed`, `allocate`, `deallocate` (with environment trimming later).
- Nondet: `try_me_else`, `retry_me_else`, `trust_me`, plus `neck_cut`, `get_level`, `cut`.
- Indexing primitives (stubs): `switch_on_term`, `switch_on_constant`, `switch_on_structure` (table building later).

Success criteria:
- Clauses with multiple alternatives backtrack correctly.
- `cut/0`, `true/0`, `fail/0` and simple recursion work under WAM.
 - Interim memory safety: introduce a minimal GC stub (root marking only) or conservative heap growth guard to enable long‑running tests; full GC deferred to Phase 6.

---

## Phase 3 — Minimal compiler (Prolog → WAM)
Deliverables:
- A first compiler pass that translates a subset of clauses into WAM, emitting a textual assembler and bytecode loader.

Scope:
- Facts and simple rules (conjunction), no disjunction yet.
- Head normalization, permanent/temporary variable assignment (X/Y allocation).
- Clause labels, predicate tables, module linkage; loader that resolves labels.
- Golden tests comparing compiled vs. hand‑assembled instruction sequences.
 - Architecture decision recorded: assembler → bytecode → loader retained for debuggability; in‑memory only loading supported too. Persisted bytecode optional.

Success criteria:
- `member/2`, `append/3`, small examples compile and run under WAM.

---

## Phase 3.5 — Exceptions and occurs‑check policy
Deliverables:
- ISO‑style `throw/1` and `catch/3` under WAM with proper exception frames and unwind protocol across environments, choicepoints, and the trail.
- Document and implement occurs‑check policy: default off for performance, optional occurs‑check in unification paths via a flag (matching current engine capability).

Tasks:
- Add exception frames on the control stack; define unwind ordering and restoration of registers, environments, choicepoints, and trail.
- Map Python exceptions and error atoms to Prolog error terms consistently with the current engine.
- Wire optional occurs‑check into unify instructions or document deviation where omitted.

Success criteria:
- Existing exception unit tests (`throw/1`, `catch/3`, error terms) pass under WAM mode.
- Occurs‑check tests run with and without the flag to verify behavior parity.

## Phase 4 — Full control features + builtins bridge
Deliverables:
- Compiler handles disjunction `(;)/2`, if‑then‑else, and cut placement.
- WAM can call back into existing Python builtins (bridge) until we port them.

Tasks:
- Compile disjunction via choicepoint blocks; `if-then-else` via commit pattern.
- Implement `call/N` bridging: unknown predicates go through existing tree‑walker builtins.
- Error propagation consistent with current engine (wrap in Prolog error terms).
 - Ensure exception handling interacts correctly with disjunction, cut, and the builtins bridge; define unwind rules across bridge boundaries.

Success criteria:
- Existing unit tests for control and builtins (subset) pass under WAM mode.

---

## Phase 5 — Indexing
Deliverables:
- First‑argument indexing across constants and structures.

Tasks:
- Build index tables during compile/link (`switch_on_*` payloads) with fallbacks.
- Add index density thresholds and safe fallbacks to linear scan.
- Microbenchmarks before/after (append/member/naive_reverse) to validate impact.

Success criteria:
- Measurable clause selection speedups on common patterns vs. tree‑walker.
- Stable memory usage; correctness preserved with and without tables.

---

## Phase 5.5 — Optimization passes
Deliverables:
- Optional optimizations: environment trimming, last call optimization (LCO), peephole rules, and selective opcode fusion.

Tasks:
- Add compile‑time rewrite passes (peephole: redundant moves, put/get folding) with unit tests per rule.
- Profile instruction dispatch overhead and opcode granularity; adjust grouping or handler fusion where profitable (avoid code bloat).
- Explore tail‑call (execute) patterns and simple environment trimming where safe.
- Track microbenchmarks (append/member/naive_reverse, n‑queens setup) before/after.

Success criteria:
- Speedups on common patterns vs. Phase 5 baseline; stable memory profile.
- Performance bounds tracked: WAM not worse than 2× tree‑walker on representative tests at this stage.

---

## Phase 6 — Memory management (GC)
Deliverables:
- A simple, stop‑the‑world heap GC suitable for Python.

Options:
- Mark & sweep over heap roots (registers, environments, choicepoints, trail).
- Heap growth + opportunistic compaction when safe (low‑risk first).

Success criteria:
- Long‑running queries don’t leak; GC verified by stress tests.
 - Supersedes interim P2 GC stub; document root set and invariants.

---

## Phase 7 — Attributed variables + CLP(FD) interop
Deliverables:
- Attributed variables on WAM refs with hook points for CLP(FD) propagation.

Tasks:
- Mirror current attribute store on heap refs; trigger hooks on bind/deref.
- Bridge `labeling/2` and domain operations: either call into existing CLP(FD) or port critical parts.
 - Note dependency on GC readiness; consider lightweight ref‑counting for attribute payloads if full GC is not yet available.
 - Ensure unification instructions expose the same hook points used by the current engine so propagation cost is contained.

Success criteria:
- Existing CLP(FD) tests can run under WAM mode (even if slower initially).

---

## Phase 8 — Tracing, debugging, and ISO polish
Deliverables:
- WAM tracer ports (`CALL/EXIT/REDO/FAIL`) integrated with current tracer UI.
- Improved error terms/locations; ISO behavioral deltas documented/addressed.
 - Debug Adapter Protocol (DAP) adapter updates: ability to step by instruction or by goal; map WAM state (P, CP, B, E, X/Y) to variables view.

Tasks:
- Map emulator events to tracer sink; include register/heap snapshots behind a flag.
- Align with ISO corner cases where feasible; document deviations.
 - Extend existing VS Code debug support to surface WAM state; add toggles to switch stepping granularity.

Success criteria:
- Trace parity on simple programs between tree‑walker and WAM.
 - Usable debugging experience for WAM with instruction‑level stepping.

---

## Phase 9 — Web (Pyodide) readiness + packaging
Deliverables:
- WAM runs in Pyodide with acceptable performance and memory.

Tasks:
- Measure instruction dispatch overhead in WASM Python; consider table‑driven dispatch.
- Bundle assembler/bytecode loader; ensure no native deps.
- Add large‑solution tests and timeouts; measure vs. current web REPL.
 - Validate high‑level API compatibility with the web REPL plan: `Engine(program, backend="wam"|"tree").run(goals)`.
 - Evaluate WASM performance trade‑offs (dict vs array dispatch) and heap allocation patterns to reduce GC pressure.

Success criteria:
- Demo programs (lists, arithmetic, simple CLP(FD)) run smoothly in browser.
 - Within 2× of native Python WAM performance on microbenchmarks under Pyodide.

---

## Testing Strategy
- Start with very small unit tests per instruction and per compile pattern.
- Golden instruction sequences for compiler output (text assembler diff‑tests).
- Property tests for unification (grounding, variable aliasing, occurs‑check off/on).
- Backtracking and cut scenarios (ensure trail/env/choicepoint invariants).
- Cross‑engine differential tests (tree‑walker vs. WAM) on a representative suite.
 - Order‑sensitive tests where applicable; verify both bindings and solution order when defined.
 - Trace comparisons on selected programs (not just final answers) to catch control‑flow regressions.
 - Performance bounds tracked per phase; alert if WAM exceeds phase targets.
 - Loader validation cases: malformed bytecode/opcode/label/register are rejected with structured errors; ensure graceful failure without engine crashes.

Example differential test pattern:
```python
@pytest.mark.parametrize("engine", ["tree", "wam"])
def test_append_deterministic(engine):
    result = run_query("append([1,2],[3,4],X)", engine=engine)
    assert result == [{"X": [1,2,3,4]}]
```

## Benchmarks & KPIs
- Microbenchmarks: `append/3`, `member/2`, naive reverse, permutation generation.
- Macro: subset of Prolog examples and scenario tests; selected CLP(FD) problems.
- Metrics: throughput (solutions/sec), instruction counts, memory footprint, GC frequency.
- **Methodology**: See [wam-benchmarks.md](wam-benchmarks.md) for detailed benchmark definitions, measurement protocol, and reporting format.

### Performance targets (guidance)
- P1: Correctness baseline recorded; no target speedups required.
- P5.5: 2–5× faster than tree‑walker on common microbenchmarks (append/member).
- P9: Pyodide/WASM runs within ~2× of native Python WAM on microbenchmarks.

## Integration & Rollout
- Feature flag: `PYLOG_ENGINE=wam|tree` (env var + programmatic option).
- Per‑query engine selection; fall back to tree‑walker on unimplemented features.
- CI lanes: WAM‑only tests, dual‑engine diff tests, web (Pyodide) smoke tests.

### High‑level API compatibility
- Maintain the existing entry points used by the REPL and web plans:
  - `engine = Engine(program, backend="wam")  # or "tree"`
  - `solutions = engine.run(goals)`
  - Ensure backend selection is available via env var and API.

### Module Resolution (user‑facing)
- Default module is `user`. Unqualified calls and clauses compile to and resolve within the current module.
- Qualified calls `M:Goal` resolve using module `M` at compile and runtime; symbol keys use `module:name/arity`.
- Imports/exports are respected during linking; qualified calls bypass imports by using explicit `M:`.
- Built‑ins live under the `system` module and can be qualified explicitly if needed (internal bridge may route them).
- Error messages include module context when resolving undefined predicates: `undefined predicate m:p/2`.

## Git Strategy (keeping tree‑walker live while evolving WAM)

### Branching model
- Trunk‑based with short‑lived feature branches.
  - Feature branches: `feature/wam-phaseN-topic`
  - Docs/infra: `chore/docs-*`, `chore/ci-*`
- Avoid a long‑lived `wam` fork; favor small, frequent merges behind a flag.

### Isolation via engine flag
- Default engine remains tree‑walker; WAM is opt‑in (env var and programmatic switch).
- Keep WAM under `prolog/wam/…` initially to minimize coupling; migrate shared utils deliberately.

### CI matrix
- Required: unit + scenarios with tree‑walker.
- Non‑blocking (early): a WAM core subset on key tests; promote to required as parity grows.
- Add a differential lane that runs selected tests under both engines and compares solutions.
- Web smoke tests stay tree‑walker until a WAM web path exists.

### Test strategy
- Differential tests per module where feasible (same query under both engines, compare bindings/order).
- Golden tests for the compiler output (assembler/bytecode) independent of engine.
- Microbenchmarks trend reporting (no PR failures; use to spot regressions).

### Merge discipline
- Keep WAM changes small and behind the flag; avoid touching tree‑walker hot paths without reason.
- Land shared‑utility refactors first (no behavior change), then use them from WAM.
- Require green tree‑walker CI; WAM core lane green where applicable.

### Releases
- Tree‑walker releases continue normally.
- Document WAM status as “experimental”; expose a hidden CLI/env toggle for early users.
- When near parity: promote WAM tests to required; consider an RC with WAM as default before flipping.

### Hotfixes
- Maintain `release/x.y` branches for hotfixes; cherry‑pick from `main`.
- Keep hotfixes isolated from WAM internals.

### Developer ergonomics
- Test fixtures/markers to select engine easily (e.g., `@pytest.mark.engine('wam')`).
- Pre‑commit hooks for formatting/lint; keep diffs focused and readable.
- Optional codeowners for `prolog/wam/*` to ensure focused review.

## Risks & Mitigations
- Instruction dispatch overhead in Python: prefer small, predictable dispatch; benchmark dict vs enum/list; consider grouping opcodes.
- Python recursion limits: keep emulator loop iterative; avoid deep Python call stacks inside handlers.
- Opcode granularity trade‑offs: too fine increases dispatch cost; too coarse risks code bloat—profile and tune in P5.5.
- GC complexity: start simple (mark & sweep), add compaction when safe; provide an interim guard in P2.
- Unification performance: Python objects are heavy—measure early; consider packed representations or array‑backed heaps in future work if justified.
- CLP(FD) propagation hooks: ensure WAM unification exposes the same hook points as the current engine; watch propagation overhead.
- Scope creep: keep compiler subset tight early; defer meta‑complexities (modules, tabling) to post‑MVP.
 - Malformed bytecode: add strict loader validation (opcode ranges, arities, label resolution, register bounds) and fail with structured diagnostics; keep dual‑engine fallback.

---

## Future Work (post‑MVP)

### Phase 10 — Tabling (SLG) [Exploratory]
Deliverables:
- Variant‑based tabling with a table space for subgoal calls and answer tries; suspension/resumption of consumers.

Notes:
- Integrates with choicepoint model by introducing suspension frames; requires changes to retry logic and cut interactions.
- Design decisions: variant vs subsumption tabling; answer reuse policy; interaction with CLP(FD) (likely out of initial scope).
- WAM impact: new ops or runtime helpers to consult/insert answers and manage suspension; careful tracer/DAP mapping.

Success criteria:
- Micro examples (`path/2` with cycles, `reachability`) terminate with memoization; performance acceptable vs tree‑walker reference.

## Supporting Documentation

### Implementation References
- [wam-instruction-set.md](wam-instruction-set.md) — Complete instruction set reference with semantics and validation rules
- [wam-benchmarks.md](wam-benchmarks.md) — Benchmark methodology, measurement protocol, and performance targets
- [phase-0-foundations.md](phase-0-foundations.md) through [phase-9-web-packaging.md](phase-9-web-packaging.md) — Detailed phase plans

### Academic References
- Warren, D. H. D. "An Abstract PROLOG Instruction Set." 1983.
- Aït‑Kaci, H. "Warren's Abstract Machine: A Tutorial Reconstruction." 1991.
- Demoen, B. "Indexing of Logic Programs."
- Tarau, P. "A Portable and Efficient Prolog Compiler."
- SWI‑Prolog internals and WAM disassemblies (for inspiration, not strict adherence).

---

## Milestone Summary (suggested)
1. Foundations + unification core (P0–P1)
2. Control & backtracking (P2)
3. Minimal compiler + loader (P3)
4. Disjunction, cut, builtins bridge (P4)
5. Indexing (P5)
6. Optimization passes (P5.5)
7. GC (P6)
8. Attributed vars + CLP(FD) (P7)
9. Tracing + ISO polish (P8)
10. Web packaging + perf (P9)

Each milestone should end with: a demo predicate running under WAM, a reliable benchmark delta, and clear docs of what’s supported.
