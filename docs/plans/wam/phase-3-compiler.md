# Phase 3 — Minimal compiler (Prolog → WAM)

Objective
- Compile a subset of clauses to WAM, with a loader and tiny assembler for readability.

Outcomes
- Compiler pass for facts and simple rules (conjunction only); bytecode/asm output; loader resolves labels.
 - Architecture decision recorded: assembler → bytecode → loader retained for debuggability; in‑memory loading by default with optional persisted bytecode format evaluated later.

Non‑Goals (Phase 3)
- No disjunction/ITE/cut compilation yet (Phase 4).
- No indexing table emission (Phase 5).
- No global optimization beyond correctness (peephole defers to Phase 5).

Scope
- In: Head/body normalization, X/Y allocation, labels, module‑aware predicate table, bytecode emitter, module resolution and qualified calls.
- Out: Disjunction, ITE, indexing tables (later phases).

Dependencies
- Phases 1–2 instruction support and runner.

Design
- IR: normalized clause with variable liveness (allocate Y for permanents), linearized goals.
- Emission: head get‑sequence, body call‑sequence, `proceed` at clause end.
- Module linkage: represent predicate symbols as `module:name/arity`. Maintain a current module during compilation; respect explicit qualification `M:Goal` by using `M` for resolution. Support import/export tables when linking.
- Assembler: text form to aid tests; bytecode structure for runtime. Include golden tests comparing compiled output to hand‑assembled sequences for core patterns.
- **Decision point on persisted bytecode**: Phase 3 supports in‑memory bytecode loading. Persisted bytecode format (save to disk, load later) is optional and deferred pending:
  - Use case validation (deployment scenarios, startup time requirements)
  - Versioning strategy (bytecode compatibility across PyLog versions)
  - Security model (signature/verification for distributed bytecode)
  - Evaluate by Phase 9 whether web/packaging scenarios require persistence; document decision.

IR Details
- Input: existing AST for clauses (`Clause(head, body)`).
- Transformations:
  - Head normalization: structures in head become `get_*` sequences; variable roles determined.
  - Variable classification:
    - Permanent (appears across calls / live after a call) → allocate in Y frame.
    - Temporary (dies before next call) → keep in X.
  - Goal linearization: emit `call/execute` in sequence with necessary moves.

Register Allocation Strategy
- X registers allocated from left to right for goal arguments.
- Y registers assigned via liveness analysis across the body; simple data‑flow sufficient at this stage.

Register Operand Format & Runtime Compatibility
- **Codegen output format**: Code generation emits banked register operands as `(bank, index)` tuples:
  - X registers: `("X", i)` where `i` is the X register index
  - Y registers: `("Y", k)` where `k` is the Y register index (frame-relative)
  - Example: `(OP_GET_VARIABLE, ("X", 0), 1)` means "get Argument 1 into X0"
  - Example: `(OP_GET_VARIABLE, ("Y", 2), 0)` means "get Argument 0 into Y2"
- **Machine operand expectations**: Current Machine implementation expects integer operands for register indices (Phase 1-2 legacy).
- **Format mismatch**: Direct execution of codegen output `(OP_GET_VARIABLE, ("X", 0), 1)` raises `TypeError` in Machine because it attempts integer comparisons on tuple operands.
- **Resolution strategy**: The assembler/loader stage (Issues #364-#367) will bridge this gap by:
  1. Accepting banked operands from codegen (preserves test readability and correctness assertions)
  2. Lowering banked operands to Machine-compatible format during load:
     - For `("X", i)`: pass through as integer `i` (Machine already handles X registers)
     - For `("Y", k)`: either:
       - Expand to Y-specific opcodes (e.g., `OP_GET_Y_VARIABLE`) if Machine adds them, or
       - Encode bank information in operand and teach Machine to decode at runtime
  3. Validating register bounds per predicate during load (max X/Y indices used)
- **Current state**: Codegen tests verify banked operand emission. Machine execution tests defer to assembler/loader implementation.
- **Migration path**: Once assembler/loader is complete, end-to-end tests will compile → load → execute under Machine.

Module Resolution & Qualified Calls
- **Current module context**: Compiler maintains a current module (default `user`); unqualified predicate references resolve within this module.
- **Qualified calls**: `M:Goal` syntax compiles to lookups using explicit module `M`; symbol keys use format `module:name/arity`.
- **Import/export tables**: During linking, respect module boundaries; qualified calls `M:Goal` bypass import restrictions by using explicit module name.
- **Built‑in predicates**: Live under `system` module; can be qualified explicitly if needed (e.g., `system:true/0`). Internal bridge may route common built‑ins automatically.
- **Symbol table**: Predicate registry keyed by `"module:name/arity"`; loader resolves these keys to code addresses.
- **Undefined predicates**: Error messages include module context: `undefined predicate m:p/2`.
- **Default behaviour**: Unqualified calls and clause heads compile to and resolve within the current module; explicit `M:` always uses module `M`.

Assembler/Bytecode
- Text format sample:
```
label p/2:
  get_structure f/2, A1
  unify_variable X1
  unify_constant a
  call q/1
  proceed
```
- Module‑aware form example:
```
module m.
label m:p/2:
  get_structure f/2, A1
  call m:q/1
  proceed
```
- Bytecode: list of `(op, args...)` tuples with a symbol table mapping `module:name/arity` to code indices.

Bytecode Validation & Error Recovery
- Validate at load time:
  - Opcode range and arity per opcode
  - Operand types/shapes (e.g., register indices, functor keys)
  - Label/symbol resolution and module existence
  - Register bounds per predicate (max X/Y used)
- Versioning: include a simple header with magic + version; reject mismatches explicitly.
- Failure mode: raise structured `BytecodeLoadError` with location and reason; in dual‑engine contexts, fall back to tree‑walker when available.

BytecodeLoadError (schema & examples)
```
{
  "type": "BytecodeLoadError",
  "code": "OP_ARITY_MISMATCH" | "UNKNOWN_OPCODE" | "BAD_OPERAND" | "UNRESOLVED_LABEL" | "MODULE_NOT_FOUND" | "REGISTER_OOB" | "VERSION_MISMATCH",
  "message": "human‑readable summary",
  "opcode": 37,                   // optional: offending opcode numeric id
  "expectedArity": 2,             // optional
  "foundArity": 3,                // optional
  "label": "L42",                // optional: label at fault
  "predicate": "m:p/2",          // optional: context
  "pc": 128,                      // optional: program counter
  "module": "m",                 // optional
  "hint": "check assembler emission for get_structure"
}
```

Examples
```
// Arity mismatch
{ "type": "BytecodeLoadError", "code": "OP_ARITY_MISMATCH", "message": "opcode 37 expects 2 args, found 3", "opcode": 37, "expectedArity": 2, "foundArity": 3, "label": "L42", "predicate": "m:p/2", "pc": 57 }

// Unknown opcode
{ "type": "BytecodeLoadError", "code": "UNKNOWN_OPCODE", "message": "unknown opcode 255", "opcode": 255, "pc": 3 }

// Unresolved label
{ "type": "BytecodeLoadError", "code": "UNRESOLVED_LABEL", "message": "label L_foo not defined", "label": "L_foo", "predicate": "user:q/1" }
```

Implementation Tasks
- Add `prolog/wam/compile.py`: transform AST clauses to instruction lists.
- Add `prolog/wam/asm.py`: text assembler + bytecode packer; `load_program()` linking labels.
- Add module and predicate registries and code areas per predicate (keyed by `module:name/arity`).
- Implement errors for unsupported constructs; provide helpful messages.
 - Implement simple liveness analysis to classify Y vs X; include debug dumps for review.
 - Expose a `compile_program(clauses) -> CodeArea` used by tests to run end‑to‑end.
 - Implement bytecode validator and `BytecodeLoadError` with structured diagnostics.
 - Document assembler grammar and loader responsibilities; keep text assembler tests separate from bytecode runtime tests for clarity.

Tests
- Golden: assembler for `member/2`, `append/3`, `p(a).`—compare to expected.
- Runtime: compile then execute under WAM; compare solutions to tree‑walker.
 - Liveness unit tests: crafted bodies where permanents are required; assert Y slots assigned correctly.
 - Failure paths: unsupported constructs raise clear compile errors.
 - Module tests: resolution of unqualified vs qualified calls; import/export; fallback to `user` module when appropriate.
 - Loader validation: malformed opcode/arity/label/register produce precise `BytecodeLoadError` cases.

Acceptance Criteria
- The above examples compile and run; assembler output stable and readable.
 - Liveness debug output matches expectations on unit samples.

Risks
- Incorrect permanent var allocation yields bad `Y` usage. Mitigate with liveness tests and small visualizers.

Rollout
- Behind flag; keep user API unchanged.

Implementation Checklist
- [ ] Compiler pass (facts + conjunction)
- [ ] Assembler + loader
- [ ] Predicate registry
- [ ] Golden tests
- [ ] Runtime diff tests
