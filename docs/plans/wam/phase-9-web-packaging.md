# Phase 9 — Web (Pyodide) readiness + packaging

Objective
- Ensure the WAM runs acceptably in browser (Pyodide) and finalize packaging without native deps.

Outcomes
- WAM demo programs run in the web REPL; artifacts packaged; performance and memory measured.
 - High‑level API compatibility maintained with existing web REPL plan (`Engine(program, backend="wam").run(goals)`).

Scope
- In: performance profiling under Pyodide, message protocol for loading bytecode, safety limits.
- Out: Native extensions; keep pure‑Python for Pyodide compatibility.

Dependencies
- Compiler + runtime stable.

Design
- Send compiled bytecode from main thread to worker; minimize crossings.
- Keep instruction loop efficient in Python (table dispatch; avoid attribute lookups in hot paths).
- Safety: step and solution limits wired; wall‑clock timeout retained.
 - Dispatch trade‑offs in WASM: benchmark dict vs enum/list table dispatch under Pyodide; pick the fastest acceptable option.

Worker Message Protocol (WAM path)
- `init`: as today, initializes Pyodide and loads libraries.
- `load_bytecode`: payload `{ predicates: {...}, symbols: {...} }` — installs code area in worker VM.
- `query`: `{ q: "member(X,[1,2,3])", engine: "wam", options: {...} }` — optionally compile on the fly in worker.
- Responses mirror existing: `solutions` (batch) or `solution` stream + `done`, with stepCount.

Bytecode Security (Network Boundary)
- **Validation**: Apply Phase 3 bytecode validation to all loaded bytecode, including network‑sourced payloads.
- **Threat model**: Malformed bytecode could crash worker or cause incorrect execution; reject invalid bytecode before loading.
- **Checks**: Opcode range, arity, register bounds, label resolution, module existence (see Phase 3 `BytecodeLoadError` schema).
- **Sandboxing**: Worker runs in isolated context; malformed bytecode cannot escape browser sandbox, but validation prevents worker crashes and maintains user experience.
- **Error handling**: On validation failure, return structured error to main thread without loading; allow fallback to tree‑walker or user notification.

Performance Considerations
- Avoid sending large structures repeatedly; cache code areas in worker and only send queries.
- Keep crossings coarse: one `query` → many `solution` posts.
- Measure steps/sec in worker; expose counters in `done`.

Implementation Tasks
- Add bytecode load path in `worker.js` and hook into existing query runner when WAM enabled.
- Add perf counters (steps/sec) and memory snapshots in worker for diagnostics.
- Build & publish wheel; ensure it loads in Pyodide (smoke tests).
 - Ensure `Engine(program, backend="wam")` API works in web worker path; maintain `engine: "wam"|"tree"` message option.

Tests
- Web smoke: `X=1`, `append/3`, `member/2`, small CLP(FD) via bridge.
- Load tests: 100+ solutions; ensure UI streams and limits kick in.
 - Perf target: under Pyodide, WAM within ~2× of native Python WAM on microbenchmarks.

Acceptance Criteria
- Demo set runs smoothly; regressions tracked; no native deps introduced.
 - Within 2× of native Python WAM performance on microbenchmarks; API remains consistent with tree‑walker path.

Risks
- Pyodide overhead masking gains; mitigate by minimizing JS↔Python crossings and batching.

Rollout
- Keep WAM toggle hidden in UI until parity; document internally.

Implementation Checklist
- [ ] Worker bytecode path
- [ ] Perf counters in worker
- [ ] Pyodide smoke tests
- [ ] Packaging verification
