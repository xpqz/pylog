# Phase 0 — Foundations (design + scaffolding)

Objective
- Establish the minimal WAM scaffolding in pure Python: state layout, instruction loop, and observability. No user‑visible change.

Outcomes
- Runnable machine skeleton with a fetch–decode–dispatch loop.
- Canonical data layouts (heap/stack/trail/registers) and tagging scheme.
- Debug/trace hooks and compact state pretty‑printers for snapshots.
- CI job stubs and fixtures to exercise stepping and snapshots.
 - Documentation artifacts: memory layout diagram (heap, stacks, trail, registers), minimal instruction reference, and example state transitions for key operations.

Non‑Goals (Phase 0)
- No user‑visible engine switch or feature flag wiring in the CLI/REPL.
- No unification, control flow, compiler, or indexing yet.
- No performance tuning beyond obvious constant‑factor choices.

Scope
- In: WAM module boundary (`prolog/wam/…`), machine state, instruction enum, dispatcher, debug utilities.
- Out: Unification, control flow, compiler; any change to the tree‑walker.

Dependencies
- None (phase 0 bootstraps the WAM package in isolation).

Design
- Packages: `prolog/wam/machine.py`, `prolog/wam/heap.py`, `prolog/wam/instructions.py`, `prolog/wam/debug.py`.
- Tagging: small ints for tags (REF=0, STR=1, CON=2, LIST=3) with tuples for cells; avoid class overhead on hot paths. Document the tag bit layout explicitly even if represented by Python tuples (note trade‑offs vs. tagged integers for small ints).
- Registers: `X` (list), `Y` (frame cells), scalars `H, HB, B, E, CP, P, TR` on `Machine`.
- Dispatch: start with dict `{opcode: fn(machine, arg)}` with local bindings in the loop for speed. Prototype and benchmark alternatives (enum + list/array table, and structural `match/case`) with a tiny microbenchmark harness; record baseline choice.
- **Decision point**: Select default dispatch by end of Phase 0; document methodology and results. Revisit in Phase 5.5 if profiling shows dispatch dominates execution time (>20% of instruction loop time).
- Tracing: `machine.snapshot()` returns dict of minimal state for printing/comparison.

Data Structures (concrete)
- Heap: Python list of cells. Cell encodings:
  - `REF`: `(0, addr)` where `addr` is an int pointing to a heap location (self‑ref for unbound).
  - `STR`: `(1, functor_addr)` where `heap[functor_addr] == (2, (name:str, arity:int))`.
  - `CON`: `(2, atom_or_number)` atoms as strings; ints/floats as Python numbers.
  - `LIST`: `(3, head_addr, tail_addr)` (or use `./2` via `STR`—keep both options open, choose one and document).
- Stacks:
  - Environment frames (at `E`) store saved `CP`, previous `E`, and Y slots; initial layout documented.
  - Choicepoints (at `B`) store: saved `CP, E, B, P, H, TR, HB` and alternative pointer.
- Registers (on Machine):
  - `X: list[Any]` dynamic length; `Y` materialized via frame slots.
  - Scalars: `H, HB, B, E, CP, P, TR` as Python ints (indices into buffers) or `None` when invalid.

Instruction Encoding Options
- Start simple: instruction as a tuple `(op:int, *args)`; `code: list[tuple]` per predicate.
- `P` is an index into a flat `code` list (code area). Each step fetches `code[P]`, decodes, executes, then updates `P`.
- Keep opcodes as small ints with a constant table for names (e.g., `OP_NOOP = 0`).
- Provide `instructions.name(op:int) -> str` and `instructions.opcode("noop") -> int` helpers for readability.

Loader & Code Areas
- A minimal loader `load_program(program_ir) -> CodeArea` that accepts a prebuilt Python structure:
  - `{pred_id: [(op, args...), ...]}` where `pred_id` is a normalized `"name/arity"` string.
- The loader validates instruction arities and records entry points; no relocation/linking in Phase 0.

State Invariants (enforced by checks)
- `0 <= H <= len(heap)`; all addresses in cells are within `range(len(heap))`.
- REF canonical form: unbound refs point to themselves; deref stops in finite steps.
- If `B` is set, its record fields form a non‑increasing chain (no forward pointers off‑heap).
- Snapshot includes only JSON‑serializable primitives (ints/str/tuples/lists of those) for easy diffing.

Snapshot Schema (example)
```
{
  "regs": {"P": 0, "H": 0, "B": null, "E": null, "CP": null, "TR": 0},
  "X": [null, null, null],
  "heap": [],
  "frames": [],
  "choicepoints": []
}
```
Pretty printer displays compact rows for heap cells and register values; guard against large dumps by optional limits.

Implementation Tasks
- Create `prolog/wam/` package and modules listed above.
- Define `Opcode` enum (or small ints) and a no‑op instruction set for boot.
- Implement `Machine` with constructor initializing empty heap, stacks, registers.
- Implement dispatcher and `run(max_steps)` with step counter + safe stop.
- Add a microbenchmark script to compare dispatch variants (dict vs enum/list vs match/case) on simple unify loops; store results under `docs/` for reference and choose a default.
- Implement `debug.snapshot()`, `debug.pretty_snapshot()`; add `TraceSink` interface.
- Add unit tests: machine boots; steps 0/1/10; snapshot has expected shape.
- Add CI job (non‑blocking) that imports `prolog.wam` and runs unit tests.

Recommended No‑op/Utility Instructions (Phase 0 only)
- `noop`: does nothing other than `P += 1`.
- `halt`: stop the machine (sets an internal flag; `run` returns).
- `set_x i, val`: writes a literal to `X[i]` (test convenience only); remove later.
- `dbg_snap`: pushes current snapshot into an internal ring buffer for tests (guard with debug flag).

Stepping Harness
- A tiny driver function `step_until(machine, pred_id, max_steps=1000)` to enter a predicate and step a bounded number of instructions.
- Expose hooks to break on `halt`, or when `P` leaves the current code area.

Testing
- Unit: `test_wam_boot.py` verifying initial register values, empty stacks, snapshot format.
- Property: construction always yields valid invariants (heap size == H, etc.).
 - Fuzz: randomly generate small `code` sequences of `noop/halt` and assert `P` monotonicity and clean halting.
 - Snapshot stability: multiple successive snapshots without execution are identical.

Acceptance Criteria
- `prolog/wam` importable; `Machine().run(0)` and `.run(10)` execute and return.
- Snapshots pretty‑print and are stable (no nondeterminism in ordering/keys).
 - Invariants checker passes on boot and after executing `noop/halt` programs.

Risks
- Over‑engineering early structures. Mitigate by keeping data flat and documented.
 - Confusion between value vs. address types; enforce helper functions that clearly return addresses.

Rollout
- Merge behind feature flag (no engine switch used yet). CI lane green.

---

Implementation Checklist
- [ ] Package + modules scaffolded
- [ ] Opcode set bootstrapped
- [ ] Machine state + run loop implemented
- [ ] Snapshots + pretty printer
- [ ] Unit tests
- [ ] CI job (non‑blocking)
