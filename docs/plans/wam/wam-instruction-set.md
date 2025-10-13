# WAM Instruction Set Reference

This document defines the complete instruction set for the PyLog WAM implementation. Instructions are grouped by category and phase of implementation.

## Status Legend
- **Phase N**: Instruction implemented in Phase N
- **Syntax**: Instruction mnemonic and operands
- **Semantics**: Operational behaviour
- **Registers affected**: Which machine registers change

---

## Control Flow (Phase 2)

### `call Pred`
- **Syntax**: `call module:name/arity`
- **Semantics**: Save return address `CP = P+1`; set `P = entry(Pred)`; execution continues at predicate entry
- **Registers**: `CP`, `P`
- **Notes**: May require `allocate` in predicate prologue if body contains multiple goals

### `execute Pred`
- **Syntax**: `execute module:name/arity`
- **Semantics**: Tail call; set `P = entry(Pred)` without saving `CP`
- **Registers**: `P`
- **Notes**: Last call optimisation; does not grow environment stack

### `proceed`
- **Syntax**: `proceed`
- **Semantics**: Return from predicate; set `P = CP`
- **Registers**: `P`
- **Notes**: Equivalent to `ret` in assembly; ends clause execution

### `allocate N`
- **Syntax**: `allocate N`
- **Semantics**: Create environment frame for `N` permanent variables; save `CP` and previous `E`; set `E` to new frame address
- **Registers**: `E`, frame stack
- **Frame layout**: `[PrevE, CP, Y0, Y1, ..., Y(N-1)]`

### `deallocate`
- **Syntax**: `deallocate`
- **Semantics**: Restore `CP = E[1]`, `E = E[0]`; pop environment frame
- **Registers**: `E`, `CP`
- **Notes**: Paired with `allocate`; called before `proceed`

---

## Choicepoints & Backtracking (Phase 2)

### `try_me_else Label`
- **Syntax**: `try_me_else Label`
- **Semantics**: Create choicepoint record; save `CP, E, P, H, TR, HB`; set retry continuation to `Label`; set `B` to new choicepoint address
- **Registers**: `B`, choicepoint stack
- **Notes**: First alternative in multi-clause predicate

### `retry_me_else Label`
- **Syntax**: `retry_me_else Label`
- **Semantics**: Restore machine state from choicepoint `B`; update retry continuation to `Label`; keep `B` unchanged
- **Registers**: `CP, E, P, H, TR, HB` (restored)
- **Notes**: Middle alternative in multi-clause predicate

### `trust_me`
- **Syntax**: `trust_me`
- **Semantics**: Restore machine state from choicepoint `B`; pop choicepoint (`B = PrevB`)
- **Registers**: `CP, E, P, H, TR, HB, B` (restored and popped)
- **Notes**: Last alternative in multi-clause predicate

---

## Cut (Phase 2)

### `get_level Yn`
- **Syntax**: `get_level Yn`
- **Semantics**: Save current choicepoint pointer `B` into permanent variable `Yn`
- **Registers**: `Y[n]` (via frame at `E`)
- **Notes**: Paired with `cut`; captures backtrack level

### `cut Yn`
- **Syntax**: `cut Yn`
- **Semantics**: Restore `B = Yn`; discard all choicepoints above saved level
- **Registers**: `B`
- **Notes**: Commits to current clause; prunes alternatives

### `neck_cut`
- **Syntax**: `neck_cut`
- **Semantics**: Equivalent to `cut(B_at_entry)`; shorthand for cut without `get_level`
- **Registers**: `B`
- **Notes**: Compiled from `!` at clause neck position

---

## Unification: Put Instructions (Phase 1)

### `put_variable Xi, Aj`
- **Syntax**: `put_variable Xi, Aj`
- **Semantics**: Allocate new unbound REF on heap; set `Xi = new_ref`; copy to argument register `Aj`
- **Registers**: `Xi`, `Aj`, `H`
- **Notes**: Write mode; creates fresh variable

### `put_value Xi, Aj`
- **Syntax**: `put_value Xi, Aj`
- **Semantics**: Copy pointer/value from `Xi` to `Aj`
- **Registers**: `Aj`
- **Notes**: Read mode; shares variable or passes value

### `put_constant C, Aj`
- **Syntax**: `put_constant atom|int, Aj`
- **Semantics**: Allocate constant cell `CON(C)` if needed; set `Aj` to constant address
- **Registers**: `Aj`, possibly `H`
- **Notes**: Constants may be pooled to avoid duplication

### `put_structure F/N, Aj`
- **Syntax**: `put_structure name/arity, Aj`
- **Semantics**: Allocate functor cell `CON((F,N))` and structure cell `STR(functor_addr)` on heap; set `Aj` to structure address; enter write mode
- **Registers**: `Aj`, `H`, mode flag
- **Notes**: Followed by `N` `set_variable/set_value` instructions for arguments

---

## Unification: Get Instructions (Phase 1)

### `get_variable Xi, Aj`
- **Syntax**: `get_variable Xi, Aj`
- **Semantics**: Copy argument register `Aj` to `Xi` (pointer copy)
- **Registers**: `Xi`
- **Notes**: Read mode; variable binding

### `get_value Xi, Aj`
- **Syntax**: `get_value Xi, Aj`
- **Semantics**: Unify contents of `Xi` and `Aj`; fail if unification fails
- **Registers**: heap, trail (via unify)
- **Notes**: Read mode; may bind variables and trail

### `get_constant C, Aj`
- **Syntax**: `get_constant atom|int, Aj`
- **Semantics**: Unify `Aj` with constant `C`; fail if mismatch
- **Registers**: heap, trail (via unify)
- **Notes**: Read mode; dereferences `Aj` first

### `get_structure F/N, Aj`
- **Syntax**: `get_structure name/arity, Aj`
- **Semantics**: Deref `Aj`; if REF, bind to new structure and enter write mode; if STR with matching functor, enter read mode; else fail
- **Registers**: `Aj`, heap, trail, mode flag
- **Notes**: Followed by `N` `unify_variable/unify_value` instructions; mode depends on outcome

---

## Unification: Set Instructions (Phase 1, write mode)

### `set_variable Xi`
- **Syntax**: `set_variable Xi`
- **Semantics**: Allocate new unbound REF on heap; set `Xi` to that REF; advance heap pointer
- **Registers**: `Xi`, `H`
- **Notes**: Write mode only; builds structure argument

### `set_value Xi`
- **Syntax**: `set_value Xi`
- **Semantics**: Write value (possibly dereferenced) of `Xi` to heap; advance heap pointer
- **Registers**: `H`
- **Notes**: Write mode only; shares variable or constant

---

## Unification: Unify Instructions (Phase 1, read mode)

### `unify_variable Xi`
- **Syntax**: `unify_variable Xi`
- **Semantics**: Deref next heap argument cell; set `Xi` to that address; advance argument pointer
- **Registers**: `Xi`, argument pointer
- **Notes**: Read mode only; extracts structure argument

### `unify_value Xi`
- **Syntax**: `unify_value Xi`
- **Semantics**: Deref and unify `Xi` with next heap argument cell; advance argument pointer; fail if unification fails
- **Registers**: heap, trail (via unify)
- **Notes**: Read mode only; checks structure argument matches

---

## Indexing (Phase 5)

### `switch_on_term LVar, LCon, LStr, LList, LDefault`
- **Syntax**: `switch_on_term LVar, LCon, LStr, LList, LDefault`
- **Semantics**: Inspect tag of dereferenced `A1`; jump to corresponding label based on tag (VAR/CON/STR/LIST); default if no match
- **Registers**: `P`
- **Notes**: First-level dispatch; tables follow for CON/STR cases

### `switch_on_constant Table, LDefault`
- **Syntax**: `switch_on_constant {atom|int: Label, ...}, LDefault`
- **Semantics**: Extract constant value from `A1`; lookup in table; jump to label if found, else `LDefault`
- **Registers**: `P`
- **Notes**: Dict-based dispatch for atoms/integers

### `switch_on_structure Table, LDefault`
- **Syntax**: `switch_on_structure {name/arity: Label, ...}, LDefault`
- **Semantics**: Extract functor from `A1`; lookup `"name/arity"` in table; jump to label if found, else `LDefault`
- **Registers**: `P`
- **Notes**: Dict-based dispatch for compound terms

---

## Exceptions (Phase 3.5)

### `throw`
- **Syntax**: `throw` (ball in `A1` or designated register)
- **Semantics**: Unwind control stack to nearest matching `catch/3` frame; restore machine state; fail if no matching catcher
- **Registers**: all (restored from exception frame)
- **Notes**: Unwinds environments, choicepoints, trail; ISO-compliant error term format

### `catch_setup Handler`
- **Syntax**: `catch_setup Label` (where `Label` points to handler code)
- **Semantics**: Push exception frame recording current `CP, E, B, P, H, TR, HB` and handler entry
- **Registers**: exception stack
- **Notes**: Paired with `catch_cleanup`; protects a goal region

### `catch_cleanup`
- **Syntax**: `catch_cleanup`
- **Semantics**: Pop exception frame on normal exit (goal succeeded without throwing)
- **Registers**: exception stack
- **Notes**: Removes exception handler after protected region completes

---

## Builtins Bridge (Phase 4)

### `call_builtin Name/Arity`
- **Syntax**: `call_builtin system:name/arity`
- **Semantics**: Marshal arguments from `X` registers or heap; invoke Python builtin from tree-walker registry; import results back into heap; fail if builtin fails
- **Registers**: `X`, heap (via marshal/import)
- **Notes**: Temporary bridge until builtins ported to WAM; error mapping to Prolog terms

---

## Utility & Debug (Phase 0)

### `noop`
- **Syntax**: `noop`
- **Semantics**: No operation; advance `P` by 1
- **Registers**: `P`
- **Notes**: Test/debug only; may be removed in production

### `halt`
- **Syntax**: `halt`
- **Semantics**: Stop machine execution; set halt flag
- **Registers**: internal halt flag
- **Notes**: Exits `run()` loop cleanly

### `dbg_snap`
- **Syntax**: `dbg_snap`
- **Semantics**: Push current machine snapshot into debug ring buffer (if debug mode enabled)
- **Registers**: none (debug side-effect only)
- **Notes**: Guard with debug flag; test instrumentation

---

## Future / Deferred Instructions

### Environment Trimming (Phase 5.5, optional)
- `trim N`: Reduce environment size by `N` slots when variables no longer needed
- **Status**: Deferred; requires liveness analysis and safe point identification

### Tabling (Phase 10, exploratory)
- `table_lookup`: Check answer table for subgoal variant
- `table_insert`: Insert answer into table
- `suspend`/`resume`: Manage consumer suspension
- **Status**: Exploratory; requires significant control flow changes

---

## Register Conventions

### Argument Registers
- `A1, A2, ..., AN`: Argument positions; aliases to `X[0], X[1], ..., X[N-1]` in many implementations

### Temporary Registers
- `X[i]`: General-purpose temporary registers; allocated left-to-right for goal arguments; not preserved across calls

### Permanent Registers
- `Y[i]`: Permanent variables; stored in environment frames at `E+2+i`; preserved across calls within a clause

### Machine Registers
- `P`: Program counter (instruction pointer)
- `CP`: Continuation pointer (return address)
- `E`: Environment pointer (current frame)
- `B`: Choicepoint pointer (backtrack stack)
- `H`: Heap top pointer
- `HB`: Heap backtrack boundary (saved at choicepoint creation)
- `TR`: Trail pointer

---

## Opcode Encoding

### Phase 0 Baseline
- Opcodes represented as small integers (0–255 initially)
- Instructions encoded as tuples: `(opcode:int, arg1, arg2, ...)`
- Code area: flat list of instruction tuples per predicate
- Symbol table: `{"module:name/arity": code_offset}`

### Dispatch Strategy (Phase 0 decision)
- **Default**: Dict dispatch `{opcode: handler_fn}`
- **Alternatives benchmarked**: enum + list table, structural `match/case`
- **Decision point**: Revisit in Phase 5.5 if dispatch overhead dominates profiling

---

## Instruction Arity Reference

Quick lookup for instruction validation (Phase 3 loader):

| Instruction        | Arity | Operands                          |
|--------------------|-------|-----------------------------------|
| `noop`             | 0     | none                              |
| `halt`             | 0     | none                              |
| `proceed`          | 0     | none                              |
| `allocate`         | 1     | N:int                             |
| `deallocate`       | 0     | none                              |
| `call`             | 1     | pred_id:str                       |
| `execute`          | 1     | pred_id:str                       |
| `try_me_else`      | 1     | label:int/str                     |
| `retry_me_else`    | 1     | label:int/str                     |
| `trust_me`         | 0     | none                              |
| `get_level`        | 1     | Yn:int                            |
| `cut`              | 1     | Yn:int                            |
| `neck_cut`         | 0     | none                              |
| `put_variable`     | 2     | Xi:int, Aj:int                    |
| `put_value`        | 2     | Xi:int, Aj:int                    |
| `put_constant`     | 2     | const:atom/int, Aj:int            |
| `put_structure`    | 2     | functor:str, Aj:int               |
| `get_variable`     | 2     | Xi:int, Aj:int                    |
| `get_value`        | 2     | Xi:int, Aj:int                    |
| `get_constant`     | 2     | const:atom/int, Aj:int            |
| `get_structure`    | 2     | functor:str, Aj:int               |
| `set_variable`     | 1     | Xi:int                            |
| `set_value`        | 1     | Xi:int                            |
| `unify_variable`   | 1     | Xi:int                            |
| `unify_value`      | 1     | Xi:int                            |
| `switch_on_term`      | 5     | 4 labels + default                |
| `switch_on_constant`  | 2     | table:dict, default:label         |
| `switch_on_structure` | 2     | table:dict, default:label         |
| `throw`            | 0     | (ball in A1)                      |
| `catch_setup`      | 1     | handler_label:int/str             |
| `catch_cleanup`    | 0     | none                              |
| `call_builtin`     | 1     | builtin_id:str                    |
| `dbg_snap`         | 0     | none                              |

---

## Notes on Validation

### Loader Checks (Phase 3)
For each instruction loaded:
1. Opcode in valid range (0–max defined)
2. Arity matches table above
3. Operand types correct (int for registers, str for labels/predicates, dict for tables)
4. Register indices within declared bounds for predicate
5. Labels resolve to valid code offsets
6. Predicate IDs exist in symbol table or are forward-declared

### Error on Validation Failure
Raise `BytecodeLoadError` with structured diagnostics (see Phase 3 documentation).

---

## Evolution & Versioning

- **Phase 0–1**: Core instruction set bootstrapped
- **Phase 2**: Control flow complete
- **Phase 3**: Compiler emits full baseline set
- **Phase 3.5**: Exception instructions added
- **Phase 4**: Builtin bridge added
- **Phase 5**: Indexing instructions operational
- **Phase 5.5**: Optional trim/fusion instructions may be added
- **Future**: Tabling, advanced indexing, optimisations

This document is a living reference; update as instructions are added, deprecated, or semantics refined.
