# WAM Architecture for PyLog

**Definitive architectural reference for the Warren Abstract Machine implementation**

Version: 2025-10-13
Status: Design phase
Audience: Implementers, reviewers, maintainers

---

## Table of Contents

1. [Introduction & Context](#1-introduction--context)
2. [Architectural Overview](#2-architectural-overview)
3. [Memory Architecture](#3-memory-architecture)
4. [Instruction Set Architecture](#4-instruction-set-architecture)
5. [Unification Model](#5-unification-model)
6. [Control Flow Model](#6-control-flow-model)
7. [Compilation Architecture](#7-compilation-architecture)
8. [Module System](#8-module-system)
9. [Indexing Architecture](#9-indexing-architecture)
10. [Exception Handling](#10-exception-handling)
11. [Memory Management (GC)](#11-memory-management-gc)
12. [Integration Architecture](#12-integration-architecture)
13. [Execution Model](#13-execution-model)
14. [Invariants & Safety Properties](#14-invariants--safety-properties)
15. [Design Rationale](#15-design-rationale)
16. [Performance Characteristics](#16-performance-characteristics)
17. [Implementation Phases](#17-implementation-phases)
18. [Future Extensions](#18-future-extensions)
19. [Comparison with Classic WAM](#19-comparison-with-classic-wam)
20. [References](#20-references)

---

## 1. Introduction & Context

### 1.1 Purpose of This Document

This document provides the complete architectural specification for PyLog's Warren Abstract Machine (WAM) implementation. Unlike the roadmap (which answers "what and when") or phase plans (which answer "how to implement"), this document answers "how does it all work and why is it designed this way."

This is a **reference** document, not an implementation guide. Implementers should:
1. Read this document to understand the complete architecture
2. Consult phase plans for implementation steps
3. Refer to the instruction set reference for opcode details
4. Use the benchmark methodology for measurement

### 1.2 Relationship to PyLog Ecosystem

PyLog is a tree-walking Prolog interpreter in Python designed for learning and debuggability. The WAM adds a compiled execution layer while preserving all existing functionality:

```
┌─────────────────────────────────────────────┐
│              PyLog System                    │
│                                              │
│  ┌────────────┐         ┌────────────┐     │
│  │ Tree-Walker│◄───────►│    WAM     │     │
│  │   Engine   │         │   Engine   │     │
│  └────────────┘         └────────────┘     │
│         │                      │            │
│         └──────────┬───────────┘            │
│                    │                        │
│         ┌──────────▼──────────┐            │
│         │  Common Services     │            │
│         │  - Parser/AST        │            │
│         │  - Unify/Store       │            │
│         │  - CLP(FD)           │            │
│         │  - Tracer            │            │
│         │  - DAP               │            │
│         └─────────────────────┘            │
└─────────────────────────────────────────────┘
```

**Dual-Engine Strategy**: Both engines coexist during development. Users can select per-query via `PYLOG_ENGINE=wam|tree` or programmatically via `Engine(program, backend="wam")`. This allows:
- Continuous value delivery (tree-walker remains functional)
- Incremental WAM development and testing
- Differential testing (same query, both engines, compare results)
- Graceful migration path

### 1.3 Design Principles

1. **Correctness First**: Performance improvements come after correctness is validated
2. **Pure Python**: No native dependencies; must run in Pyodide/WASM for web deployment
3. **Observable State**: All machine state (heap, stacks, registers) can be snapshotted for debugging
4. **Incremental Development**: Ship small, testable slices with strong phase boundaries
5. **Stable Interfaces**: Memory layout, instruction semantics, and integration points remain stable across phases
6. **Test-Driven**: Each feature has unit tests, differential tests, and property tests

### 1.4 Target Audience

- **Implementers**: Engineers building the WAM
- **Reviewers**: Code reviewers validating correctness
- **Maintainers**: Future developers evolving the system
- **Educators**: Using PyLog to teach Prolog implementation

**Not for**: End users (they interact via high-level API) or casual readers (start with roadmap instead).

---

## 2. Architectural Overview

### 2.1 Component Diagram

```
┌───────────────────────── WAM Machine ─────────────────────────┐
│                                                                 │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────────────┐  │
│  │   Compiler   │  │   Loader/    │  │   Instruction      │  │
│  │   (AST→WAM)  ├─►│   Linker     ├─►│   Dispatcher       │  │
│  └──────────────┘  └──────────────┘  └─────────┬──────────┘  │
│                                                  │              │
│                    ┌─────────────────────────────┘              │
│                    │                                            │
│  ┌─────────────────▼──────────────────────────────────────┐   │
│  │              Machine State                               │   │
│  │                                                           │   │
│  │  ┌─────────┐  ┌─────────┐  ┌──────────┐  ┌──────────┐ │   │
│  │  │  Heap   │  │  Stacks │  │  Trail   │  │ Registers│ │   │
│  │  │  (H,HB) │  │  (E,B)  │  │   (TR)   │  │ (X,Y,P,..)│ │   │
│  │  └─────────┘  └─────────┘  └──────────┘  └──────────┘ │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │              Integrations                                 │  │
│  │                                                            │  │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌──────────┐   │  │
│  │  │Builtins │  │ Attrs & │  │ Tracer  │  │   DAP    │   │  │
│  │  │ Bridge  │  │ CLP(FD) │  │  Ports  │  │ Adapter  │   │  │
│  │  └─────────┘  └─────────┘  └─────────┘  └──────────┘   │  │
│  └──────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 Execution Model Overview

The WAM execution model is a **fetch-decode-execute loop** operating on bytecode:

1. **Load**: Compiler translates Prolog clauses to WAM bytecode; loader installs in code area
2. **Query**: User submits a query; engine initializes machine state
3. **Execute**: Dispatch loop fetches instruction at `P`, executes, advances `P`
4. **Unify**: Instructions manipulate heap and trail, performing unification
5. **Backtrack**: On failure, restore state from choicepoint, try next alternative
6. **Solution**: On success (empty goal stack), yield bindings; backtrack for more
7. **Terminate**: No more choicepoints; query complete

### 2.3 Data Flow Overview

```
Query String
    │
    ▼
Parser/Reader (shared with tree-walker)
    │
    ▼
AST (Prolog terms)
    │
    ▼
Compiler (AST → WAM bytecode)
    │
    ▼
Loader (bytecode → code area with symbol resolution)
    │
    ▼
Machine Initialization (empty heap/stacks, P=entry)
    │
    ▼
Dispatch Loop ◄──────────┐
    │                     │
    ├─► Instruction       │
    │    Execute          │
    │      │              │
    │      ├─► Success    │
    │      │     │        │
    │      │     └─► Continue (P++)
    │      │              │
    │      └─► Failure ───┘ (backtrack)
    │
    ▼
Solution (bindings snapshot)
    │
    ▼
Backtrack for more? ──► Yes ─┐
    │                         │
    No                        │
    │                         │
    ▼                         │
Done                          │
                              │
                              └──► Dispatch Loop
```

### 2.4 Component Responsibilities

| Component          | Responsibility                                                     |
|--------------------|-------------------------------------------------------------------|
| **Compiler**       | Translate AST to WAM bytecode; register allocation; code generation |
| **Loader/Linker**  | Resolve symbols; validate bytecode; install in code area          |
| **Dispatcher**     | Fetch-decode-execute loop; instruction routing                    |
| **Heap**           | Store terms (REF, STR, CON, LIST); allocate cells                |
| **Stacks**         | Manage environment frames (E) and choicepoints (B)               |
| **Trail**          | Record mutations for backtracking; undo on failure               |
| **Registers**      | X (temp), Y (permanent), machine regs (P, CP, E, B, H, HB, TR)   |
| **Unifier**        | Deref, bind, trail; mode-switched unification (get/put/unify/set)|
| **Control**        | Call/return, choicepoint creation/restoration, cut               |
| **Indexing**       | First-argument dispatch via switch tables                         |
| **GC**             | Mark-and-sweep to reclaim unreachable heap cells                 |
| **Integrations**   | Bridge to builtins, attrs, CLP(FD), tracer, DAP                  |

---

## 3. Memory Architecture

The WAM memory model comprises five major areas: heap, environment stack, choicepoint stack, trail, and registers. Each has specific layout, invariants, and lifecycle protocols.

### 3.1 Heap

**Purpose**: Store all Prolog terms (atoms, integers, structures, lists) as cells.

**Representation**: Python list of tuples: `heap: list[tuple]`

**Cell Tagging Scheme**:

| Tag | Name | Encoding                  | Meaning                                |
|-----|------|---------------------------|----------------------------------------|
| 0   | REF  | `(0, addr)`               | Reference to heap address (unbound var or alias) |
| 1   | STR  | `(1, functor_addr)`       | Structure; functor at `heap[functor_addr]` |
| 2   | CON  | `(2, value)`              | Constant (atom string or int/float)   |
| 3   | LIST | `(3, head_addr, tail_addr)` | List cell (or use STR with `./2`)    |

**Cell Examples**:

```python
# Unbound variable X (self-reference)
heap[5] = (0, 5)  # REF pointing to itself

# Variable X bound to variable Y at address 7
heap[5] = (0, 7)  # REF aliasing Y

# Atom 'foo'
heap[10] = (2, 'foo')  # CON

# Integer 42
heap[11] = (2, 42)  # CON

# Structure f(a,b)
heap[20] = (1, 21)           # STR pointing to functor at 21
heap[21] = (2, ('f', 2))     # Functor cell CON('f'/2)
heap[22] = (2, 'a')          # Arg 1: atom 'a'
heap[23] = (2, 'b')          # Arg 2: atom 'b'

# List [1,2|X]
heap[30] = (3, 31, 32)       # LIST(head=31, tail=32)
heap[31] = (2, 1)            # Head: 1
heap[32] = (3, 33, 34)       # Tail: another LIST
heap[33] = (2, 2)            # Head: 2
heap[34] = (0, 5)            # Tail: variable X at addr 5
```

**Registers**:
- **H** (heap top): Next free address; increments on allocation
- **HB** (heap backtrack): Saved `H` at choicepoint creation; defines trailing boundary

**Allocation Protocol**:

**Heap Growth**: The heap uses **append-based allocation**. Each allocation appends cells to the heap list and increments `H`.

```python
# Heap is a Python list: heap = []
# H tracks the next free index (always == len(heap))

def allocate_ref() -> int:
    """Allocate unbound REF (self-reference)."""
    addr = H
    heap.append((0, addr))  # Self-ref
    H += 1
    return addr

def allocate_con(value) -> int:
    """Allocate constant."""
    addr = H
    heap.append((2, value))
    H += 1
    return addr

def allocate_str(name: str, arity: int) -> int:
    """Allocate structure: STR cell first, then functor, then args follow."""
    str_addr = H
    heap.append((1, H + 1))  # STR points to next cell (functor)
    H += 1
    functor_addr = H
    heap.append((2, (name, arity)))
    H += 1
    # Args follow at functor_addr + 1 through functor_addr + arity
    # Caller must allocate and append arg cells
    return str_addr
```

**Note**: Append-based allocation means `heap[H]` is always invalid (out of bounds); use `heap.append()` to grow the heap. This leverages Python's efficient list growth (amortized O(1)). The invariant `H == len(heap)` holds at all times.

**Invariants**:
1. `H == len(heap)` (append-based growth; H always equals heap length)
2. All addresses in cells are valid indices: `0 <= addr < H`
3. Unbound REFs point to themselves (canonical form for roots)
4. Dereferencing terminates (no cycles in REF chains)

**LIST Representation Decision**

**Choice**: Dedicated LIST tag `(3, head_addr, tail_addr)`

**Rationale**:
- **Explicitness**: Lists are first-class in Prolog; dedicated tag makes this clear in snapshots and debugging
- **Performance**: Direct access to head/tail without functor dereference (one fewer indirection)
- **Simplicity**: Special-case handling for lists is needed anyway (for `./2` syntax sugar); dedicated tag is cleaner
- **Uniformity trade-off**: While STR with `./2` is more uniform, the performance and debuggability benefits outweigh uniformity

**Implementation**: Lists use tag `3` with two address fields:
```python
heap[addr] = (3, head_addr, tail_addr)
```

Empty list `[]` is represented as atom: `(2, '[]')`

**Alternative**: If implementation complexity becomes an issue, Phase 1 can reconsider and unify with STR `./2`.

### 3.2 Environment Stack

**Purpose**: Store environment frames for permanent variables (Y registers) and continuation pointers.

**Frame Structure** (at address `E`):

```
Offset | Content      | Description
-------|--------------|----------------------------------
0      | PrevE        | Previous environment pointer
1      | CP           | Continuation pointer (return address)
2      | Y[0]         | Permanent variable 0
3      | Y[1]         | Permanent variable 1
...    | ...          | ...
2+N-1  | Y[N-1]       | Permanent variable N-1
```

**Example**:

```python
# Frame for predicate with 3 permanent vars
E = 100  # Frame starts at heap[100] or separate frame stack[100]
frames[E+0] = 80       # PrevE (previous frame)
frames[E+1] = 245      # CP (return to code[245])
frames[E+2] = (0, 50)  # Y[0]: REF to heap[50]
frames[E+3] = (2, 'a') # Y[1]: CON 'a'
frames[E+4] = (0, 51)  # Y[2]: REF to heap[51]
```

**Register**:
- **E**: Environment pointer (current frame address)

**Lifecycle**:

1. **allocate N**:
   - Push new frame: `new_E = next_frame_address()`
   - `frames[new_E+0] = E` (save previous)
   - `frames[new_E+1] = CP` (save continuation)
   - Reserve N slots for Y vars
   - `E = new_E`

2. **Use Y[i]**:
   - Access via `frames[E+2+i]`

3. **deallocate**:
   - `CP = frames[E+1]` (restore continuation)
   - `E = frames[E+0]` (restore previous)

**Chaining**: Frames form a linked list via PrevE, enabling traversal for GC root enumeration.

**Note**: Environment trimming (reducing N mid-clause) is an optimization deferred to Phase 5.5.

### 3.3 Choicepoint Stack

**Purpose**: Save machine state for backtracking; enable non-deterministic execution.

**Choicepoint Record** (at address `B`):

```
Offset | Content   | Description
-------|-----------|---------------------------------------
0      | PrevB     | Previous choicepoint pointer
1      | CP        | Continuation pointer (saved)
2      | E         | Environment pointer (saved)
3      | P         | Program counter for next alternative
4      | H         | Heap top (saved)
5      | TR        | Trail top (saved)
6      | HB        | Heap backtrack boundary (saved)
```

Some implementations also save argument registers X[0..N-1]; we document the minimal set above.

**Register**:
- **B**: Choicepoint pointer (current choicepoint address)

**Lifecycle**:

1. **try_me_else Alt**:
   - Allocate choicepoint at `new_B`
   - `cp_stack[new_B+0] = B` (PrevB)
   - `cp_stack[new_B+1] = CP`
   - `cp_stack[new_B+2] = E`
   - `cp_stack[new_B+3] = Alt` (continuation for next clause)
   - `cp_stack[new_B+4] = H`
   - `cp_stack[new_B+5] = TR`
   - `cp_stack[new_B+6] = H` (HB = H at creation)
   - `B = new_B`
   - `HB = H`

2. **retry_me_else Alt**:
   - Restore state from `B`:
     - `CP = cp_stack[B+1]`
     - `E = cp_stack[B+2]`
     - `H = cp_stack[B+4]`
     - `TR_saved = cp_stack[B+5]`
     - `HB = cp_stack[B+6]`
   - Untrail from `TR` down to `TR_saved`
   - Update continuation: `cp_stack[B+3] = Alt`
   - Keep `B` unchanged (still current CP)

3. **trust_me**:
   - Restore state (same as retry)
   - Pop choicepoint: `B = cp_stack[B+0]` (PrevB)
   - `HB = (B != null) ? cp_stack[B+6] : 0`

**Invariants**:
1. Choicepoints form a linked list via PrevB
2. `HB` always equals the `H` value saved in the current choicepoint `B`
3. Saved `H, TR, E, CP` are consistent snapshots
4. `P` (Alt) points to valid code address (next clause)

### 3.4 Trail

**Purpose**: Record all heap mutations for undoing on backtracking.

**Trail Entries**: Append-only list during forward execution; popped during backtracking.

**Entry Types**:

| Type       | Format                     | Meaning                                    |
|------------|----------------------------|--------------------------------------------|
| REF_BIND   | `('bind', addr, old_cell)` | Cell at `addr` was `old_cell`; restore it |
| ATTR       | `('attr', addr, mod, old)` | Attribute `mod` at `addr` was `old`       |
| DOMAIN     | `('domain', addr, old_dom)`| Domain at `addr` was `old_dom` (CLP(FD)) |

(In some implementations, REF_BIND just stores `addr` and restores to self-ref; we store full old_cell for generality.)

**Register**:
- **TR**: Trail top (next free entry)

**Trailing Criteria**:

Trail a binding if the modified address is **older than HB**:

```python
def bind_ref(addr, value):
    """Bind REF at addr to value; trail if necessary."""
    if addr < HB:
        trail.append(('bind', addr, heap[addr]))
        TR += 1
    heap[addr] = value
```

**Rationale**: Addresses >= HB were allocated after the current choicepoint; backtracking won't need them (they'll be discarded when H rewinds to saved value). Addresses < HB existed before the CP and must be trailed.

**Untrailing Protocol**:

```python
def untrail(target_TR):
    """Undo trail entries from TR down to target_TR."""
    while TR > target_TR:
        TR -= 1
        entry = trail[TR]
        if entry[0] == 'bind':
            _, addr, old_cell = entry
            heap[addr] = old_cell  # Restore old cell
        elif entry[0] == 'attr':
            _, addr, mod, old_val = entry
            # Restore attribute (see Phase 7)
        elif entry[0] == 'domain':
            _, addr, old_dom = entry
            # Restore domain (CLP(FD))
```

**Invariants**:
1. Every mutation to `heap[addr]` where `addr < HB` is trailed
2. Trail entries are processed in reverse order (LIFO)
3. After untrailing, heap state is consistent with the saved `H` value

### 3.5 Register Files

**X Registers (Temporary/Argument)**:

- **Purpose**: Hold temporary values and arguments to goals
- **Representation**: Python list `X: list[Any]`
- **Scope**: Not preserved across calls (unless explicitly saved)
- **Usage**:
  - Argument passing: `X[0..N-1]` (also called `A1..AN`)
  - Temporary storage within a clause

**Y Registers (Permanent)**:

- **Purpose**: Hold values that must survive across calls
- **Representation**: Frame slots at `E+2+i` for `Y[i]`
- **Scope**: Valid within the lifetime of the environment frame
- **Usage**:
  - Variables live across multiple goals in a clause body
  - Determined by liveness analysis during compilation

**Machine Registers**:

| Register | Type  | Purpose                                        |
|----------|-------|------------------------------------------------|
| P        | int   | Program counter (current instruction address) |
| CP       | int   | Continuation pointer (return address)         |
| E        | int   | Environment pointer (current frame)           |
| B        | int   | Choicepoint pointer (current backtrack point) |
| H        | int   | Heap top (next free cell)                     |
| HB       | int   | Heap backtrack (saved H at CP creation)       |
| TR       | int   | Trail top (next free trail entry)             |
| EF       | int   | Exception frame top (current catcher, or -1)  |
| S        | int   | Structure argument pointer (for unify/set sequences) |
| mode     | str   | Unification mode: "read", "write", or None    |

**Calling Convention**:

1. **Caller**:
   - Place arguments in `X[0..N-1]`
   - Execute `call pred` or `execute pred`

2. **Callee**:
   - If needs permanent vars: `allocate K` (K = number of Y vars)
   - Extract args from `X[0..N-1]` via `get_*` instructions
   - Execute body goals
   - If allocated: `deallocate`
   - Execute `proceed` (or tail-call via `execute`)

**Register Conventions**:

- X registers are **volatile** (not preserved across calls)
- Y registers are **preserved** (stored in environment frame)
- Machine registers are implicitly saved/restored by control instructions

### 3.6 Exception Stack

**Purpose**: Maintain exception frames for `catch/3` unwinding. Each frame captures machine state at the point where a `catch/3` was established.

**Representation**: Python list `exception_frames: list[ExceptionFrame]`

**Frame Structure**:

```python
@dataclass
class ExceptionFrame:
    """Exception frame for catch/3."""
    prev_frame: Optional[int]     # Index of previous exception frame (or None)
    ball_pattern: int              # Heap address of catcher pattern term
    handler_label: int             # Code address of handler clause
    CP: int                        # Saved continuation pointer
    E: int                         # Saved environment pointer
    B: int                         # Saved choicepoint pointer
    H: int                         # Saved heap top
    TR: int                        # Saved trail top
    HB: int                        # Saved heap backtrack point
```

**Register**:
- **EF** (exception frame top): Index of current exception frame (or -1 if none)

**Operations**:

```python
def push_exception_frame(ball_pattern: int, handler_label: int):
    """Push exception frame on catch/3 entry."""
    frame = ExceptionFrame(
        prev_frame=EF if EF >= 0 else None,
        ball_pattern=ball_pattern,
        handler_label=handler_label,
        CP=CP, E=E, B=B, H=H, TR=TR, HB=HB
    )
    exception_frames.append(frame)
    EF = len(exception_frames) - 1

def pop_exception_frame():
    """Pop exception frame on catch/3 exit (normal success)."""
    if EF >= 0:
        frame = exception_frames[EF]
        EF = frame.prev_frame if frame.prev_frame is not None else -1
        exception_frames.pop()

def unwind_to_catcher(ball: int) -> bool:
    """Unwind exception frames to find matching catcher.

    Returns True if catcher found, False if no match.
    """
    while EF >= 0:
        frame = exception_frames[EF]

        # Try to unify ball with pattern
        saved_trail = TR
        if unify(ball, frame.ball_pattern):
            # Match found: restore state
            CP = frame.CP
            E = frame.E
            B = frame.B
            H = frame.H
            TR = frame.TR
            HB = frame.HB
            untrail_to(TR)
            P = frame.handler_label

            # Pop this frame (handler now active)
            pop_exception_frame()
            return True
        else:
            # No match: undo trial unification
            untrail_to(saved_trail)
            TR = saved_trail

            # Pop and try outer frame
            pop_exception_frame()

    return False  # No catcher found
```

**Invariants**:
1. `EF == -1` when no exception frames exist
2. `EF == len(exception_frames) - 1` when frames exist
3. Exception frames form a stack linked by `prev_frame`
4. Each frame saves sufficient state for complete restoration

**Memory Layout**:

Exception frames are separate from choicepoints but share conceptual similarity (both save machine state). They differ in purpose:
- **Choicepoints**: For backtracking (normal control flow)
- **Exception frames**: For exception unwinding (abnormal control flow)

**Integration with Choicepoints**:

When unwinding during `throw/1`:
1. Search exception frames for matching catcher
2. If found, restore state (including removing later choicepoints if needed)
3. Execute handler at restored state

**Lifecycle**:
- **Push**: On `catch_setup` instruction
- **Pop**: On `catch_cleanup` instruction (normal exit) or during unwind (throw)
- **Inspect**: During `throw/1` to find matching catcher

---

## 4. Instruction Set Architecture

### 4.1 Instruction Encoding

**Format**: Instructions are tuples `(opcode: int, arg1, arg2, ...)`

**Register Operand Convention**: All register operands (Xi, Aj) are **0-based indices** into their respective arrays.
- `Xi` operands index into `X[]` array: X0 = `X[0]`, X1 = `X[1]`, etc.
- `Aj` operands index into `X[]` array (A1 is X[0], A2 is X[1], etc.): A1 = `X[0]`, A2 = `X[1]`
- `Yn` operands index into frame slots: Y0 = `frames[E+2]`, Y1 = `frames[E+3]`, etc.

**Example**:
```python
(OP_CALL, "user:member/2")       # call user:member/2
(OP_GET_VARIABLE, 1, 0)          # get_variable X1, A1 (X[1] = X[0])
(OP_PUT_CONSTANT, 'a', 0)        # put_constant 'a', A1 (X[0] = 'a')
(OP_TRY_ME_ELSE, 245)            # try_me_else label_245
```

**Code Area**: Flat list of instructions per predicate; indexed by `P` (program counter).

**Symbol Table**: Maps `"module:name/arity"` to code entry address.

### 4.2 Instruction Categories

| Category        | Phase | Examples                                       |
|-----------------|-------|------------------------------------------------|
| Control         | 2     | call, execute, proceed, allocate, deallocate  |
| Choicepoints    | 2     | try_me_else, retry_me_else, trust_me          |
| Cut             | 2     | get_level, cut, neck_cut                      |
| Unification (get)| 1    | get_variable, get_value, get_constant, get_structure |
| Unification (put)| 1    | put_variable, put_value, put_constant, put_structure |
| Unification (unify)| 1  | unify_variable, unify_value                    |
| Unification (set)| 1    | set_variable, set_value                        |
| Indexing        | 5     | switch_on_term, switch_on_constant, switch_on_structure |
| Exceptions      | 3.5   | throw, catch_setup, catch_cleanup             |
| Builtins Bridge | 4     | call_builtin                                   |
| Debug           | 0     | noop, halt, dbg_snap                          |

Full instruction semantics in [wam-instruction-set.md](wam-instruction-set.md).

### 4.3 Dispatch Mechanisms

**Option A: Dict Dispatch** (Phase 0 default)

```python
handlers = {
    OP_CALL: handle_call,
    OP_GET_VARIABLE: handle_get_variable,
    # ...
}

def dispatch(machine):
    while machine.running:
        instr = code[machine.P]
        opcode, *args = instr
        handler = handlers[opcode]
        handler(machine, *args)
```

**Pros**: Simple, debuggable, flexible
**Cons**: Python dict lookup overhead (~100ns)

**Option B: Array Dispatch** (Phase 5.5 optimization)

```python
handlers = [handle_noop, handle_call, handle_get_variable, ...]  # Indexed by opcode int

def dispatch(machine):
    while machine.running:
        instr = code[machine.P]
        opcode, *args = instr
        handlers[opcode](machine, *args)
```

**Pros**: Faster (~50ns)
**Cons**: Less flexible (opcodes must be dense integers)

**Option C: Match/Case** (Python 3.11+, exploratory)

```python
match opcode:
    case OP_CALL: handle_call(machine, *args)
    case OP_GET_VARIABLE: handle_get_variable(machine, *args)
    # ...
```

**Pros**: Readable, potentially optimized by interpreter
**Cons**: Unclear performance in CPython 3.11

**Decision Point**: Phase 0 benchmarks these options; revisit in Phase 5.5 if dispatch >20% of execution time.

### 4.4 Mode Flags

Unification instructions operate in two modes:

- **Read Mode**: Match existing heap structures
  - `unify_variable Xi`: Read arg from heap
  - `unify_value Xi`: Unify Xi with arg from heap

- **Write Mode**: Build new heap structures
  - `set_variable Xi`: Allocate new REF, write to heap
  - `set_value Xi`: Write Xi's value to heap

**Transition**: `get_structure F/N, Aj` sets mode based on deref(Aj):
- If REF: **write mode** (building new structure)
- If STR with matching F/N: **read mode** (matching existing)
- Else: fail

---

## 5. Unification Model

### 5.1 Deref Algorithm

**Purpose**: Follow REF chains to find the root cell.

**Algorithm** (iterative, no recursion):

```python
def deref(addr: int) -> int:
    """Follow REF chain to root; returns root address."""
    while True:
        cell = heap[addr]
        if cell[0] != TAG_REF:
            return addr  # Non-REF: this is the root
        if cell[1] == addr:
            return addr  # Self-ref: unbound variable
        addr = cell[1]  # Follow chain
```

**Path Compression** (optional, Phase 1 decision):

Compress paths during mutation (inside `bind()`), trailed:

```python
def deref_compress(addr: int) -> int:
    """Deref with path compression; trails updates."""
    path = []
    while True:
        cell = heap[addr]
        if cell[0] != TAG_REF or cell[1] == addr:
            # Compress path: point all intermediate REFs to root
            for mid in path:
                if mid < HB:
                    trail.append(('bind', mid, heap[mid]))
                    TR += 1
                heap[mid] = (TAG_REF, addr)
            return addr
        path.append(addr)
        addr = cell[1]
```

**Trade-off**: Compression speeds up future derefs but adds trailing overhead. Defer decision to Phase 1 profiling.

### 5.2 Binding Protocol

**Core Bind Function**:

```python
def bind(addr_a: int, addr_b: int) -> bool:
    """Bind two dereferenced addresses; returns success."""
    a = deref(addr_a)
    b = deref(addr_b)

    if a == b:
        return True  # Already unified

    cell_a = heap[a]
    cell_b = heap[b]

    # Occurs check (optional)
    if occurs_check_enabled:
        if occurs(a, b) or occurs(b, a):
            return False  # Cycle detected

    # At least one must be REF for binding
    if cell_a[0] == TAG_REF:
        # Bind a to b
        if a < HB:
            trail.append(('bind', a, cell_a))
            TR += 1
        heap[a] = (TAG_REF, b)

        # Attribute hooks (Phase 7)
        if has_attrs(a) or has_attrs(b):
            invoke_attr_hooks(a, b)

        return True

    elif cell_b[0] == TAG_REF:
        # Bind b to a (symmetric)
        if b < HB:
            trail.append(('bind', b, cell_b))
            TR += 1
        heap[b] = (TAG_REF, a)

        if has_attrs(a) or has_attrs(b):
            invoke_attr_hooks(b, a)

        return True

    else:
        # Both non-REF: structural unification
        return unify_terms(a, b)
```

**Trailing Criterion**: Trail if `addr < HB` (older than current choicepoint).

### 5.3 Occurs Check

**Purpose**: Prevent infinite structures like `X = f(X)`.

**Default**: **Off** (for performance and rational trees support).

**When Enabled**: Check before binding if `a` occurs in the term at `b`:

```python
def occurs(var_addr: int, term_addr: int) -> bool:
    """Check if var occurs in term (cycle detection)."""
    visited = set()
    stack = [deref(term_addr)]

    while stack:
        addr = stack.pop()
        if addr in visited:
            continue
        visited.add(addr)

        if addr == var_addr:
            return True  # Cycle found

        cell = heap[addr]
        if cell[0] == TAG_STR:
            functor_addr = cell[1]
            _, (name, arity) = heap[functor_addr]
            # Add structure args to stack
            for i in range(arity):
                stack.append(deref(functor_addr + 1 + i))
        elif cell[0] == TAG_LIST:
            stack.append(deref(cell[1]))  # head
            stack.append(deref(cell[2]))  # tail

    return False
```

**Configuration**: Occurs check controlled by machine flag or compile-time option.

### 5.4 Structural Unification

**Algorithm** (non-recursive, explicit stack):

```python
def unify_terms(addr_a: int, addr_b: int) -> bool:
    """Unify two non-REF terms."""
    stack = [(deref(addr_a), deref(addr_b))]

    while stack:
        a, b = stack.pop()

        if a == b:
            continue  # Same cell

        cell_a = heap[a]
        cell_b = heap[b]

        # Handle REFs via bind
        if cell_a[0] == TAG_REF or cell_b[0] == TAG_REF:
            if not bind(a, b):
                return False
            continue

        # Both non-REF: must match structurally
        if cell_a[0] != cell_b[0]:
            return False  # Tag mismatch

        if cell_a[0] == TAG_CON:
            if cell_a[1] != cell_b[1]:
                return False  # Constant mismatch

        elif cell_a[0] == TAG_STR:
            functor_a = heap[cell_a[1]]
            functor_b = heap[cell_b[1]]
            if functor_a != functor_b:
                return False  # Functor mismatch
            _, (name, arity) = functor_a
            # Push arg pairs
            for i in range(arity):
                stack.append((cell_a[1] + 1 + i, cell_b[1] + 1 + i))

        elif cell_a[0] == TAG_LIST:
            # Push head and tail pairs
            stack.append((cell_a[1], cell_b[1]))
            stack.append((cell_a[2], cell_b[2]))

    return True
```

### 5.5 Instruction Families

**Get Family** (read from argument registers):

- `get_variable Xi, Aj`: `Xi = Aj` (pointer copy)
- `get_value Xi, Aj`: `unify(Xi, Aj)`
- `get_constant C, Aj`: `unify(Aj, allocate_con(C))`
- `get_structure F/N, Aj`: Complex (sets mode, starts unify sequence)

**Put Family** (write to argument registers):

- `put_variable Xi, Aj`: `Xi = Aj = allocate_ref()`
- `put_value Xi, Aj`: `Aj = Xi`
- `put_constant C, Aj`: `Aj = allocate_con(C)`
- `put_structure F/N, Aj`: Build structure on heap, set `Aj`, enter write mode

**Unify Family** (read mode, matching existing heap):

- `unify_variable Xi`: `Xi = deref(heap[arg_ptr++])`
- `unify_value Xi`: `unify(Xi, deref(heap[arg_ptr++]))`

**Set Family** (write mode, building new heap structure):

- `set_variable Xi`: `Xi = allocate_ref(); heap[H++] = Xi`
- `set_value Xi`: `heap[H++] = deref(Xi)`

---

## 6. Control Flow Model

### 6.1 Call/Execute/Proceed

**call Pred**:
- Save return address: `CP = P + 1`
- Jump to predicate: `P = entry_point(Pred)`
- Predicate may execute `allocate` if it has permanent vars

**execute Pred** (tail call):
- Jump to predicate: `P = entry_point(Pred)`
- No CP update (reuse caller's CP)
- Optimization: avoids growing environment stack

**proceed**:
- Return: `P = CP`
- If environment was allocated: must `deallocate` first

### 6.2 Environment Allocation

**allocate N**:

```python
def handle_allocate(machine, N):
    """Allocate environment frame for N permanent vars."""
    new_E = allocate_frame(N + 2)  # +2 for PrevE and CP
    frames[new_E + 0] = machine.E  # Save previous E
    frames[new_E + 1] = machine.CP # Save CP
    # Y slots [new_E+2 .. new_E+2+N-1] uninitialized
    machine.E = new_E
    machine.P += 1
```

**deallocate**:

```python
def handle_deallocate(machine):
    """Restore previous environment."""
    machine.CP = frames[machine.E + 1]
    machine.E = frames[machine.E + 0]
    machine.P += 1
```

### 6.3 Choicepoint Creation

**try_me_else Alt**:

```python
def handle_try_me_else(machine, alt_label):
    """Create choicepoint; first alternative."""
    new_B = allocate_choicepoint()
    cp_stack[new_B + 0] = machine.B   # PrevB
    cp_stack[new_B + 1] = machine.CP
    cp_stack[new_B + 2] = machine.E
    cp_stack[new_B + 3] = alt_label   # Next alternative
    cp_stack[new_B + 4] = machine.H
    cp_stack[new_B + 5] = machine.TR
    cp_stack[new_B + 6] = machine.H   # HB = H

    machine.B = new_B
    machine.HB = machine.H
    machine.P += 1  # Continue to first clause body
```

**retry_me_else Alt**:

```python
def handle_retry_me_else(machine, alt_label):
    """Restore from choicepoint; middle alternative."""
    B = machine.B
    machine.CP = cp_stack[B + 1]
    machine.E = cp_stack[B + 2]
    saved_H = cp_stack[B + 4]
    saved_TR = cp_stack[B + 5]
    machine.HB = cp_stack[B + 6]

    # Untrail
    untrail(machine, saved_TR)
    machine.H = saved_H

    # Update continuation
    cp_stack[B + 3] = alt_label

    machine.P += 1  # Continue to clause body
```

**trust_me**:

```python
def handle_trust_me(machine):
    """Restore from choicepoint; last alternative (pop CP)."""
    B = machine.B
    machine.CP = cp_stack[B + 1]
    machine.E = cp_stack[B + 2]
    saved_H = cp_stack[B + 4]
    saved_TR = cp_stack[B + 5]

    untrail(machine, saved_TR)
    machine.H = saved_H

    # Pop choicepoint
    machine.B = cp_stack[B + 0]  # PrevB
    machine.HB = (machine.B != None) ? cp_stack[machine.B + 6] : 0

    machine.P += 1
```

### 6.4 Backtracking Protocol

When a goal fails (unification fails, no more clauses):

1. If `B == None`: No choicepoints; query fails completely
2. Else:
   - `P = cp_stack[B + 3]` (jump to retry continuation)
   - Instruction at `P` is `retry_me_else` or `trust_me`
   - That instruction restores state and continues

### 6.5 Cut Semantics

**get_level Yn**:

```python
def handle_get_level(machine, yn):
    """Save current B into Yn."""
    frames[machine.E + 2 + yn] = machine.B
    machine.P += 1
```

**cut Yn**:

```python
def handle_cut(machine, yn):
    """Discard choicepoints above saved level."""
    saved_B = frames[machine.E + 2 + yn]
    machine.B = saved_B
    machine.HB = (machine.B != None) ? cp_stack[machine.B + 6] : 0
    machine.P += 1
```

**neck_cut**:

Shorthand for cut at clause entry (no Y var needed):

```python
def handle_neck_cut(machine):
    """Cut to B level at clause entry (saved during try_me_else)."""
    # Assumes B at entry was saved somewhere accessible
    # (Implementation detail: may reuse a scratch register)
    machine.B = entry_B
    machine.HB = (machine.B != None) ? cp_stack[machine.B + 6] : 0
    machine.P += 1
```

**Effect**: Discards all choicepoints above the saved level, making the clause deterministic (no backtracking past this point).

---

## 7. Compilation Architecture

### 7.1 AST to WAM Pipeline

```
Prolog AST
    │
    ▼
┌─────────────────────┐
│ Head Normalization  │ ← Extract args, flatten structures
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Liveness Analysis   │ ← Classify vars (X/Y), determine allocate N
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Register Allocation │ ← Assign X[i], Y[i] indices
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Code Generation     │ ← Emit get/put/call/execute sequences
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Assembler           │ ← Text format for debugging/golden tests
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Bytecode Encoding   │ ← Tuple format: (opcode, args...)
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Loader/Linker       │ ← Resolve symbols, validate, install
└─────────────────────┘
           │
           ▼
       Code Area
```

### 7.2 Register Allocation Strategy

**Variable Classification**:

- **Temporary (X)**: Variable dies before next call or occurs only once
- **Permanent (Y)**: Variable lives across calls or occurs in multiple goals

**Liveness Analysis** (simplified):

```python
def classify_vars(clause):
    """Classify vars as temporary (X) or permanent (Y)."""
    temp_vars = set()
    perm_vars = set()

    # Scan head: all vars initially temporary
    for var in clause.head.vars():
        temp_vars.add(var)

    # Scan body goals left-to-right
    for i, goal in enumerate(clause.body):
        goal_vars = goal.vars()

        # Any var appearing after a call becomes permanent
        if i > 0:
            for var in goal_vars:
                if var in temp_vars:
                    perm_vars.add(var)
                    temp_vars.remove(var)

        # First occurrence: temporary
        for var in goal_vars:
            if var not in temp_vars and var not in perm_vars:
                temp_vars.add(var)

    return temp_vars, perm_vars
```

**Register Assignment**:

- X registers: allocated left-to-right per goal (X[0], X[1], ...)
- Y registers: allocated once per clause (Y[0], Y[1], ...)

### 7.3 Code Generation

**Head Compilation** (get sequence):

For head `p(f(X), a, Y)`:

```
label user:p/3:
  allocate 1             # If Y is permanent
  get_structure f/1, A1
    unify_variable Y0    # Temporary extraction to Y0 (permanent)
  get_constant a, A2
  get_variable Y1, A3    # Y permanent
  ...
```

**Body Compilation** (call sequence):

For body `q(X, a), r(X)`:

```
  put_value Y0, A1       # X is Y0
  put_constant a, A2
  call user:q/2
  put_value Y0, A1       # X reused
  call user:r/1
  deallocate
  proceed
```

**Last Call Optimization**:

Final call becomes `execute`:

```
  execute user:r/1       # Tail call, no proceed needed
```

### 7.4 Assembler Format

Text format for debugging and golden tests:

```
module user.

label user:append/3:
  try_me_else L1
  get_constant [], A1
  get_variable X1, A2
  get_value X1, A3
  proceed

label L1:
  retry_me_else L2
  allocate 2
  get_structure ./2, A1
    unify_variable Y0
    unify_variable Y1
  get_variable Y2, A2
  get_structure ./2, A3
    unify_value Y0
    unify_variable X3
  put_value Y1, A1
  put_value Y2, A2
  put_value X3, A3
  call user:append/3
  deallocate
  proceed

label L2:
  trust_me
  ...
```

### 7.5 Bytecode Encoding

Tuple format:

```python
bytecode = [
    (OP_TRY_ME_ELSE, 100),      # try_me_else offset_100
    (OP_GET_CONSTANT, [], 1),    # get_constant [], A1
    (OP_GET_VARIABLE, 1, 2),     # get_variable X1, A2
    (OP_GET_VALUE, 1, 3),        # get_value X1, A3
    (OP_PROCEED,),               # proceed
    # ...
]
```

### 7.6 Loader and Validation

**Load-Time Checks**:

1. Opcode in valid range
2. Arity matches opcode definition
3. Operand types correct (int for registers, str for labels)
4. Labels resolve to valid code offsets
5. Register indices within bounds
6. Module names exist

**Validation Errors**:

Raise `BytecodeLoadError` with structured diagnostics (see Phase 3).

**Symbol Resolution**:

```python
symbol_table = {
    "user:append/3": 0,      # Entry point at code[0]
    "user:member/2": 500,    # Entry point at code[500]
    # ...
}
```

### 7.7 Module System Integration

**Qualified Symbols**: `module:name/arity`

**Resolution Rules**:
- Unqualified `p(X)` → compile as `current_module:p/1`
- Qualified `m:p(X)` → compile as `m:p/1`
- Built-ins: `system:true/0`, `system:fail/0`, etc.

---

## 8. Module System

### 8.1 Module-Qualified Symbols

All predicates internally represented as `"module:name/arity"`.

**Example**:
- User predicate `append/3` in module `user` → `"user:append/3"`
- Built-in `true/0` → `"system:true/0"`

### 8.2 Current Module Context

Compiler maintains a `current_module` (default: `"user"`).

**Unqualified references** resolve within current module:

```prolog
% In module user:
foo(X) :- bar(X).   % Compiles as call to user:bar/1
```

### 8.3 Qualified Calls

**Syntax**: `Module:Goal`

**Compilation**:

```prolog
foo(X) :- m:bar(X).  % Compiles as call to m:bar/1
```

**Instruction**: `call "m:bar/1"`

### 8.4 Import/Export Tables

**Export**: Predicates visible to other modules
**Import**: Predicates imported from other modules

**Qualified calls bypass imports**: `M:Goal` always resolves in module `M`.

### 8.5 System Module

Built-in predicates live under `"system"` module:

- `system:true/0`
- `system:fail/0`
- `system:=/2`
- `system:is/2`
- etc.

**Automatic import**: Built-ins may be auto-imported into all modules for convenience.

### 8.6 Undefined Predicates

**Error message format**:

```
undefined predicate m:p/2
```

Includes module context for clarity.

---

## 9. Indexing Architecture

### 9.1 First-Argument Indexing

**Goal**: Avoid linear clause scan by dispatching on first argument.

**Mechanism**: `switch_on_*` instructions at predicate entry.

### 9.2 Switch Instructions

**switch_on_term VAR, CON, STR, LIST, DEFAULT**:

Inspect dereferenced `A1`:
- If VAR → jump to `VAR` label
- If CON → jump to `CON` label (then `switch_on_constant`)
- If STR → jump to `STR` label (then `switch_on_structure`)
- If LIST → jump to `LIST` label
- Else → jump to `DEFAULT`

**switch_on_constant Table, DEFAULT**:

```python
table = {
    'a': offset_100,
    'b': offset_200,
    42: offset_300,
}
value = deref(A1)[1]  # Extract constant value
P = table.get(value, DEFAULT)
```

**switch_on_structure Table, DEFAULT**:

```python
table = {
    'f/2': offset_400,
    'g/1': offset_500,
}
functor_addr = deref(A1)[1]
name, arity = heap[functor_addr][1]
key = f"{name}/{arity}"
P = table.get(key, DEFAULT)
```

### 9.3 Index Building (Compile/Link Time)

For each predicate with N clauses:

1. Inspect first argument of each clause head
2. Build tables mapping constants/functors to clause offsets
3. Compute table density: `distinct_keys / total_clauses`
4. If density >= threshold (e.g., 0.5): emit switch tables
5. Else: emit linear scan (try_me_else chain)

### 9.4 Fallback to Linear Scan

If tables are sparse or first argument is variable in most clauses, use traditional:

```
try_me_else L1
  <clause 1>
label L1:
  retry_me_else L2
  <clause 2>
label L2:
  trust_me
  <clause 3>
```

### 9.5 Performance Characteristics

**Best case**: O(1) dispatch to correct clause (hash table lookup)
**Worst case**: O(N) linear scan if no indexing applicable
**Typical**: 2–10× speedup on predicates with diverse first arguments

---

## 10. Exception Handling

### 10.1 Exception Frames

**Purpose**: Save machine state for `catch/3` unwinding.

**Frame Structure** (conceptual; may piggyback on choicepoints):

```
{
  "prev_exception_frame": ...,
  "catcher": catcher_goal,
  "ball_pattern": pattern,
  "CP": ...,
  "E": ...,
  "B": ...,
  "H": ...,
  "TR": ...,
  "HB": ...
}
```

### 10.2 Throw/Catch Semantics

**catch(Goal, Ball, Handler)**:

1. Execute `catch_setup Handler_Label`:
   - Push exception frame with current state
   - Set catcher to `Ball = ...` unification
2. Execute `Goal`
3. If `Goal` succeeds: execute `catch_cleanup` (pop frame), continue
4. If `Goal` fails: backtrack normally (no exception)
5. If `throw(Term)` occurs during `Goal`:
   - Unwind to nearest matching catcher
   - Unify `Ball` with `Term`
   - If match: restore state, execute `Handler`
   - If no match: propagate to outer catcher

**throw(Term)**:

```python
def handle_throw(machine):
    """Throw exception; unwind to nearest catcher."""
    ball = deref(machine.X[0])  # Ball in A1

    while machine.exception_frames:
        frame = machine.exception_frames.pop()

        # Try to unify ball with pattern
        if unify(ball, frame.ball_pattern):
            # Match: restore state
            machine.CP = frame.CP
            machine.E = frame.E
            machine.B = frame.B
            machine.H = frame.H
            machine.TR = frame.TR
            machine.HB = frame.HB
            untrail(machine, frame.TR)

            # Jump to handler
            machine.P = frame.handler_label
            return

    # No catcher: propagate to top level
    raise UnhandledException(ball)
```

### 10.3 Unwind Protocol

Unwinding restores:
1. Heap (`H`, `HB`)
2. Trail (`TR`) and untrail to saved point
3. Environment (`E`)
4. Choicepoints (`B`)
5. Program counter (`P` = handler entry)

### 10.4 Interaction with Control Structures

**Throw during backtracking**: Exception frames checked during unwind; backtracking stops when catcher found.

**Cut and exceptions**: Cut does not affect exception frames (they are orthogonal).

### 10.5 Error Term Mapping

Python exceptions → Prolog error terms:

| Python Exception       | Prolog Error Term              |
|------------------------|--------------------------------|
| `TypeError`            | `type_error(Type, Culprit)`   |
| `ValueError`           | `domain_error(Domain, Culprit)`|
| `AttributeError`       | `existence_error(Obj, Type)`  |
| Unification failure    | `fail` (not an exception)     |

Errors generated by built-ins or system code map to ISO-standard error terms where applicable.

---

## 11. Memory Management (GC)

### 11.1 Mark-and-Sweep Design

**Algorithm**:

1. **Mark Phase**: Traverse from roots; mark all reachable cells
2. **Sweep Phase**: Iterate heap; reclaim unmarked cells

**Trigger**: When `H` exceeds threshold (e.g., `max_heap_size * 0.8`).

### 11.2 Root Set Enumeration

**Roots** (starting points for marking):

1. **X Registers**: All non-null `X[i]`
2. **Y Registers**: Follow E-chain; mark all `Y[i]` in each frame
3. **Choicepoints**: Follow B-chain; mark saved `E` frames, saved heap slots
4. **Trail**: Addresses in trail entries may reference heap

**Pseudo-code**:

```python
def enumerate_roots(machine):
    """Collect all root addresses."""
    roots = set()

    # X registers
    for addr in machine.X:
        if addr is not None:
            roots.add(addr)

    # E-chain (environment frames)
    E = machine.E
    while E is not None:
        frame = frames[E]
        for i in range(frame.num_y_slots):
            addr = frame.Y[i]
            if addr is not None:
                roots.add(addr)
        E = frame.PrevE

    # B-chain (choicepoints)
    B = machine.B
    while B is not None:
        cp = cp_stack[B]
        # Saved E frames
        roots.add(cp.E)
        # May also save X registers (implementation detail)
        B = cp.PrevB

    # Trail (addresses)
    for entry in machine.trail[:machine.TR]:
        if entry[0] == 'bind':
            roots.add(entry[1])

    return roots
```

### 11.3 Mark Phase

```python
def mark(roots, heap):
    """Mark all cells reachable from roots."""
    marked = set()
    stack = list(roots)

    while stack:
        addr = stack.pop()
        if addr in marked or addr >= len(heap):
            continue
        marked.add(addr)

        cell = heap[addr]
        if cell[0] == TAG_REF:
            stack.append(cell[1])  # Follow REF
        elif cell[0] == TAG_STR:
            functor_addr = cell[1]
            marked.add(functor_addr)
            _, (name, arity) = heap[functor_addr]
            for i in range(arity):
                stack.append(functor_addr + 1 + i)
        elif cell[0] == TAG_LIST:
            stack.append(cell[1])  # head
            stack.append(cell[2])  # tail

    return marked
```

### 11.4 Sweep Phase

```python
def sweep(heap, marked, H):
    """Reclaim unmarked cells."""
    new_heap = []
    addr_map = {}  # Old addr -> new addr

    for old_addr in range(H):
        if old_addr in marked:
            new_addr = len(new_heap)
            addr_map[old_addr] = new_addr
            new_heap.append(heap[old_addr])

    # Update all addresses (REFs, STRs, LISTs)
    for i, cell in enumerate(new_heap):
        if cell[0] == TAG_REF:
            new_heap[i] = (TAG_REF, addr_map[cell[1]])
        elif cell[0] == TAG_STR:
            new_heap[i] = (TAG_STR, addr_map[cell[1]])
        elif cell[0] == TAG_LIST:
            new_heap[i] = (TAG_LIST, addr_map[cell[1]], addr_map[cell[2]])

    # Update roots (X, Y, E, B, trail)
    update_roots(addr_map)

    return new_heap
```

**Note**: Sweep with compaction (above) is optional. Simpler approach: mark dead cells as tombstones, reuse addresses.

### 11.5 Safe Points

**GC must run only at safe points**:
- Between instructions (not mid-unification)
- When machine state is consistent (no dangling pointers)

**Typical trigger**: Check after each instruction or every N instructions.

### 11.6 Interaction with Backtracking

**Trail and GC**:
- Trail contains addresses that may become dead after GC
- Untrailing after GC must use updated addresses (if compacting)
- Or: defer GC until after untrailing completes

**Choicepoints and GC**:
- Choicepoints hold saved `H` values
- If GC compacts heap, update saved `H` in all choicepoints

### 11.7 Interim Guard (Phase 2)

Before full GC (Phase 6), use a **conservative heap growth limit**:

```python
if machine.H > MAX_HEAP_SIZE:
    raise HeapExhausted("GC not yet implemented; heap limit reached")
```

Or a minimal stub that tracks roots but doesn't sweep (for testing root enumeration).

---

## 12. Integration Architecture

### 12.1 Builtins Bridge

**Purpose**: Call tree-walker built-ins from WAM until WAM-native versions are available.

**Instruction**: `call_builtin "system:is/2"`

**Marshalling Protocol** (WAM → Python):

```python
def marshall_term(addr, heap):
    """Convert WAM heap cell to Python AST."""
    addr = deref(addr)
    cell = heap[addr]

    if cell[0] == TAG_CON:
        value = cell[1]
        if isinstance(value, str):
            return Atom(value)
        else:
            return Int(value)

    elif cell[0] == TAG_REF:
        return Var(addr, hint=f"_{addr}")

    elif cell[0] == TAG_STR:
        functor_addr = cell[1]
        _, (name, arity) = heap[functor_addr]
        args = [marshall_term(functor_addr + 1 + i, heap) for i in range(arity)]
        return Struct(name, tuple(args))

    elif cell[0] == TAG_LIST:
        items = []
        tail_addr = addr
        while heap[tail_addr][0] == TAG_LIST:
            head_addr = heap[tail_addr][1]
            items.append(marshall_term(head_addr, heap))
            tail_addr = deref(heap[tail_addr][2])
        tail = marshall_term(tail_addr, heap)
        return List(tuple(items), tail)
```

**Import Protocol** (Python → WAM):

```python
def import_term(term, machine):
    """Convert Python AST to WAM heap cell; returns addr."""
    if isinstance(term, Atom):
        return machine.allocate_con(term.name)
    elif isinstance(term, Int):
        return machine.allocate_con(term.value)
    elif isinstance(term, Var):
        # Map Var.id to existing WAM addr or allocate new
        return machine.get_or_allocate_var(term.id)
    elif isinstance(term, Struct):
        functor_addr = machine.allocate_con((term.functor, len(term.args)))
        str_addr = machine.allocate_str_cell(functor_addr)
        for arg in term.args:
            arg_addr = import_term(arg, machine)
            machine.heap[machine.H] = arg_addr
            machine.H += 1
        return str_addr
    elif isinstance(term, List):
        # Recursive list building...
        return import_list(term, machine)
```

**call_builtin Execution**:

```python
def handle_call_builtin(machine, builtin_name):
    """Call tree-walker builtin; bridge."""
    # Marshall args from X registers
    args = [marshall_term(machine.X[i], machine.heap) for i in range(arity)]

    # Invoke builtin (returns iterator of solutions)
    builtin_fn = builtin_registry[builtin_name]
    solutions = builtin_fn(*args, store=bridge_store)

    try:
        solution = next(solutions)  # Get first solution

        # Import bindings back
        for var_id, value in solution.items():
            wam_addr = machine.var_map[var_id]
            imported_addr = import_term(value, machine)
            unify(wam_addr, imported_addr)

        machine.P += 1  # Success
    except StopIteration:
        backtrack(machine)  # Failure
```

**Allowlist**: Limit which built-ins are accessible via bridge (security, correctness).

### 12.2 Attributed Variables

**Attribute Store**: Side map `{root_addr: {module: attr_value}}`

**Storage**:

```python
machine.attrs = {}  # {addr: {module: value}}
```

**Hook Invocation** (in `bind()`):

```python
def bind_with_attrs(addr_a, addr_b, machine):
    """Bind with attribute hooks."""
    # Unify normally
    if not bind(addr_a, addr_b):
        return False

    # Check for attributes
    attrs_a = machine.attrs.get(addr_a, {})
    attrs_b = machine.attrs.get(addr_b, {})

    if attrs_a or attrs_b:
        # Invoke hooks for each module
        for module in set(attrs_a.keys()) | set(attrs_b.keys()):
            hook = attr_hooks.get(module)
            if hook:
                if not hook(module, 'bind', addr_a, addr_b):
                    return False  # Hook rejected binding

    # Migrate attributes to new root
    new_root = deref(addr_a)
    if new_root != addr_a:
        machine.attrs[new_root] = {**attrs_a, **attrs_b}
        if addr_a in machine.attrs:
            del machine.attrs[addr_a]
        if addr_b in machine.attrs:
            del machine.attrs[addr_b]

    return True
```

**Trail Integration**:

```python
# Trail old attribute state
if addr_a in machine.attrs:
    old_attrs = machine.attrs[addr_a]
    machine.trail.append(('attr', addr_a, module, old_attrs))
    machine.TR += 1
```

### 12.3 CLP(FD) Integration

**Propagation Queue Bridge**:

```python
# On domain change (via attribute hook):
def clpfd_attr_hook(module, event, var_addr, term_addr):
    """CLP(FD) attribute hook."""
    if event == 'bind':
        # Extract domain from attribute
        domain = machine.attrs[var_addr].get('clpfd')
        if domain:
            # Enqueue all propagators watching this var
            for prop_id in domain.watchers:
                propagation_queue.schedule(prop_id, priority='high')

            # Run propagation to fixpoint
            run_propagation_fixpoint()

    return True  # Allow binding
```

**Domain Operations**: Bridge to existing `prolog/clpfd/` Python implementation initially; port to WAM later if justified.

**Labeling**: Create choicepoints via WAM control instructions (`try_me_else` for each value in domain).

### 12.4 Debugging and Tracing

**Tracer Port Events**:

At key instructions, emit port events:

```python
# At call instruction:
emit_port_event('CALL', predicate, depth, bindings)

# At proceed instruction (success):
emit_port_event('EXIT', predicate, depth, bindings)

# At retry_me_else (redo):
emit_port_event('REDO', predicate, depth)

# At failure (no more clauses):
emit_port_event('FAIL', predicate, depth)
```

**Snapshot Mechanism**:

```python
def snapshot(machine):
    """Capture machine state as JSON-serializable dict."""
    return {
        "regs": {"P": machine.P, "CP": machine.CP, "E": machine.E,
                 "B": machine.B, "H": machine.H, "HB": machine.HB, "TR": machine.TR},
        "X": list(machine.X),
        "heap": machine.heap[:machine.H],
        "frames": serialize_frames(machine.E),
        "choicepoints": serialize_choicepoints(machine.B),
        "trail": machine.trail[:machine.TR],
    }
```

**DAP Adapter**:

Map WAM state to DAP protocol:

- **Scopes**: X registers, Y registers (per frame), heap (selected cells)
- **Stepping**: Step over (next instruction), step into (enter call), step out (return)
- **Breakpoints**: On predicates or instruction addresses

---

## 13. Execution Model

### 13.1 Query Lifecycle

```
1. User submits query: "member(X, [1,2,3])"
2. Parser converts to AST: Struct('member', (Var(0), List([Int(1), Int(2), Int(3)])))
3. Compiler emits bytecode (if not already compiled)
4. Loader installs bytecode in code area
5. Machine initializes:
   - H = 0, B = None, E = None, TR = 0
   - X[0] = query args marshalled to heap
   - P = entry_point("user:member/2")
6. Dispatch loop runs until:
   - Success (empty goal stack) → yield solution
   - Failure (backtrack exhausts all choices) → done
7. User requests more solutions → backtrack → repeat from step 6
```

### 13.2 Instruction Dispatch Loop

```python
def run(machine, max_steps=None):
    """Main execution loop."""
    step_count = 0

    while machine.running:
        if max_steps and step_count >= max_steps:
            break

        # Fetch
        instr = machine.code[machine.P]
        opcode, *args = instr

        # Decode & Execute
        handler = dispatch_table[opcode]
        handler(machine, *args)

        step_count += 1

        # Check for solution
        if machine.goal_stack_empty():
            yield snapshot_bindings(machine)
            # Backtrack for more
            if not backtrack(machine):
                break
```

### 13.3 Solution Generation

**Success Path**:

1. Goal stack empties (all goals satisfied)
2. Snapshot current bindings (X registers, heap reified to Python AST)
3. Yield solution to caller
4. Backtrack to find next solution

**Example**:

```python
# Query: member(X, [1,2,3])
# Solutions:
# 1st: X = 1  (try first clause, succeed)
# 2nd: X = 2  (backtrack, retry, succeed)
# 3rd: X = 3  (backtrack, retry, succeed)
# No more: (backtrack, no more clauses, done)
```

### 13.4 Backtracking Flow

```
Failure detected (unification fails or no more clauses)
    │
    ▼
Check B (current choicepoint)
    │
    ├─► B == None? ──► Yes ──► Query fails completely
    │
    ▼ No
Jump to retry continuation: P = cp_stack[B][3]
    │
    ▼
Instruction at P is retry_me_else or trust_me
    │
    ├─► retry_me_else: Restore state, update continuation
    └─► trust_me: Restore state, pop choicepoint
    │
    ▼
Continue execution with restored state
```

### 13.5 Interaction with Tracer

At each port event:

1. Check if tracing enabled
2. If yes: serialize goal, bindings, depth
3. Emit to tracer sink (console, DAP, log file)
4. Continue execution

**Minimal overhead when tracing disabled**: Single flag check per port.

---

## 14. Invariants & Safety Properties

### 14.1 Heap Invariants

1. **Address bounds**: All addresses in cells satisfy `0 <= addr < H`
2. **Canonical REFs**: Unbound variables point to themselves (self-refs)
3. **Deref termination**: No cycles in REF chains (guaranteed by binding protocol)
4. **Structure validity**: STR points to valid functor; functor has correct arity
5. **Tag consistency**: Cell tag matches content type

### 14.2 Trail Invariants

1. **Every mutation trailed**: Binding `heap[addr]` where `addr < HB` adds trail entry
2. **Trail ordering**: Entries are LIFO (last-in, first-out)
3. **Untrail restores**: After untrailing to `TR_saved`, heap state matches saved choicepoint

### 14.3 Stack Invariants

**Environment Stack**:
1. **Frame chaining**: `frames[E][0]` points to valid previous frame or `None`
2. **CP validity**: `frames[E][1]` is valid code address
3. **Y slot bounds**: Accessing `Y[i]` for `i < num_y_slots`

**Choicepoint Stack**:
1. **CP chaining**: `cp_stack[B][0]` points to valid previous CP or `None`
2. **Saved state consistency**: `H, TR, E, CP` in CP record are valid
3. **HB relationship**: `HB == cp_stack[B][4]` (saved H)

### 14.4 Register Conventions

1. **X registers**: Volatile (not preserved across calls)
2. **Y registers**: Preserved (stored in environment)
3. **P always valid**: Points to executable instruction or termination marker
4. **CP valid when set**: If `CP != None`, it's a valid return address

### 14.5 Validation Rules

**Bytecode Loader**:
- Opcode in valid range: `0 <= opcode < NUM_OPCODES`
- Arity matches: `len(args) == expected_arity[opcode]`
- Operands well-typed: Registers are ints, labels resolve, functors are strings

**Runtime Assertions** (debug mode):
- `assert 0 <= H <= len(heap)`
- `assert all(0 <= addr < H for addr in heap cells)`
- `assert deref_terminates(addr)` for all `addr`

### 14.6 Snapshot JSON-Serializability

All state in snapshots uses only:
- Primitives: `int`, `str`, `float`, `bool`, `None`
- Containers: `list`, `tuple`, `dict` of the above

**No Python objects** (classes, lambdas) in snapshots → easy diffing, storage, transfer.

---

## 15. Design Rationale

### 15.1 Python-Specific Considerations

**No Python Recursion**:
- Python has a default recursion limit (~1000)
- WAM uses explicit stacks (heap, environment, choicepoint)
- All loops are iterative (while/for), no recursive function calls in dispatch

**Pure Python Requirement**:
- Must run in Pyodide (WASM) for web deployment
- No native extensions (C, Rust)
- Trade-off: Slower than native WAM implementations (SWI-Prolog, Yap)
- Benefit: Portable, accessible, educational

**Boxed Objects vs Tagged Integers**:
- Classic WAM uses tagged integers (low bits = tag, high bits = value/address)
- Python integers are objects (slower)
- We use tuples: `(tag: int, ...)`
- Trade-off: Larger memory footprint, but clearer and debuggable

### 15.2 Deviations from Classic WAM

**Tuple-Based Cells**:
- Classic: Packed 32-bit or 64-bit words
- PyLog: Python tuples `(tag, ...)`
- Reason: Python doesn't expose low-level memory; tuples are fast and readable

**Dict Dispatch Initially**:
- Classic: Jump tables or switch statements
- PyLog: `dispatch_table = {opcode: handler}`
- Reason: Simplicity; Python dict lookup is fast enough (~100ns)
- Phase 5.5 may switch to array dispatch if profiling shows bottleneck

**Explicit Trail Entries**:
- Classic: Trail stores addresses; untrailing resets to self-ref or uses flag bits
- PyLog: Trail stores `('bind', addr, old_cell)` with full old cell
- Reason: Generality; supports restoring arbitrary old values (attributes, domains)

**No Register Windows**:
- Classic WAM may use register windows (slide X regs on call)
- PyLog: Caller places args in `X[0..N-1]`; callee reads them
- Reason: Simpler; Python list slicing is cheap

### 15.3 Performance vs Debuggability Trade-offs

**Design prioritizes debuggability**:
- Snapshots are comprehensive and readable
- Instructions are tuples (not packed bytes)
- Heap cells are tuples (not bit-packed integers)
- Trade-off: 10-100× slower than native WAM initially
- Acceptable: Goal is education and correctness; optimization in Phase 5.5

**Observability**:
- All state is inspectable (no hidden registers)
- Tracer hooks at every control point
- Snapshots are JSON-serializable (easy testing, debugging)

### 15.4 Why Incremental/Phased Approach

**Continuous value delivery**:
- Tree-walker works today; WAM is additive
- Each phase delivers testable, useful functionality
- No "big bang" integration

**Risk mitigation**:
- Small, verifiable slices reduce bug surface
- Differential testing catches regressions early
- Stable interfaces allow refactoring internals without breaking phases

**Educational value**:
- Each phase can be taught as a standalone topic
- Incremental complexity matches learning curve

### 15.5 ISO Compliance

**Goal**: ISO-ish semantics where the tree-walker already is

**Non-goals**:
- Full ISO compliance (out of scope for MVP)
- Obscure ISO predicates (focus on core + CLP(FD))

**Documented deviations**:
- Some built-ins may have tree-walker semantics (documented in phase plans)
- Module system simplified (no full ISO modules initially)

---

## 16. Performance Characteristics

### 16.1 Hot Paths

**Identified hot paths** (from profiling similar systems):

1. **Deref**: Called on every heap access; must be fast
   - Optimization: Path compression (trailed)
   - Optimization: Cache deref results within instruction

2. **Unify**: Core operation; called by all `get/put/unify` instructions
   - Optimization: Inline common cases (var-var, var-con)
   - Optimization: Avoid Python call overhead (use local functions)

3. **Dispatch**: Fetch-decode-execute loop overhead
   - Phase 0: Dict dispatch (~100ns per instruction)
   - Phase 5.5: Array dispatch (~50ns per instruction)
   - Future: JIT compilation (speculative)

4. **Trailing**: Every binding checks `addr < HB` and appends to trail
   - Optimization: Pre-allocated trail capacity
   - Optimization: Batch untrailing (undo multiple in tight loop)

### 16.2 Dispatch Overhead

**Measurement** (Phase 0 baseline):
- Dict dispatch: ~100ns per instruction (Python 3.11, M1 Mac)
- Array dispatch: ~50ns per instruction
- Match/case: ~80ns per instruction

**Target** (Phase 5.5):
- Reduce dispatch to <5% of total execution time
- If dispatch dominates, switch to array or match/case

### 16.3 Memory Allocation Patterns

**Heap Growth**:
- Allocate in chunks (e.g., 1000 cells at a time)
- Python lists reuse capacity (amortized O(1) append)
- GC compacts and reuses space (Phase 6)

**Trail Growth**:
- Similar chunking strategy
- Peak trail size proportional to `H * backtrack_depth`

**Stack Growth**:
- Environment and choicepoint stacks grow dynamically
- Python lists handle growth efficiently

### 16.4 GC Triggers and Frequency

**Trigger Conditions**:
- Heap exceeds threshold: `H > max_heap * 0.8`
- Manual: `gc/0` builtin for testing
- Automatic: After N allocations (tunable)

**Frequency**:
- Long-running queries: GC every 10K-100K allocations
- Short queries: May never trigger GC (heap resets on query completion)

**Impact**:
- Stop-the-world: Pause execution during GC
- Duration: O(live_heap_size) for mark, O(total_heap_size) for sweep
- Typical: <10ms for heaps <10MB

### 16.5 Optimization Opportunities (Phase 5.5)

**Peephole Optimizations**:
- Fold `put_value Xi, Aj` followed by `get_value Xi, Aj` → noop
- Combine `put_constant` + `call` → specialized `call_with_const`

**Instruction Fusion**:
- Multi-instruction sequences → single fused op
- Example: `get_structure f/2, A1; unify_variable X1; unify_constant a` → `get_f_2_X1_a A1`

**Environment Trimming**:
- Reduce environment size when Y vars dead
- Requires liveness analysis at call boundaries

**Last Call Optimization (LCO)**:
- Reuse environment frame for tail calls
- Already present: `execute` vs `call` distinction

---

## 17. Implementation Phases

### 17.1 Phase Dependencies

```
Phase 0 (Foundations)
    │
    ▼
Phase 1 (Unification)
    │
    ▼
Phase 2 (Control) ────┐
    │                  │
    ▼                  ▼
Phase 3 (Compiler)  Phase 6 (GC)
    │                  │
    ▼                  │
Phase 3.5 (Exceptions) │
    │                  │
    ▼                  │
Phase 4 (Full control) │
    │                  │
    ▼                  │
Phase 5 (Indexing)    │
    │                  │
    ▼                  │
Phase 5.5 (Optimization)
    │                  │
    └──────┬───────────┘
           │
           ▼
    Phase 7 (Attrs + CLP(FD))
           │
           ▼
    Phase 8 (Tracing + ISO)
           │
           ▼
    Phase 9 (Web + Packaging)
           │
           ▼
    Phase 10 (Tabling - Future)
```

### 17.2 Feature Increments Per Phase

| Phase | Feature                          | Testability                      |
|-------|----------------------------------|----------------------------------|
| 0     | Machine skeleton, dispatcher     | Boot, step, snapshot             |
| 1     | Unification (get/put/unify/set) | Hand-assembled unify tests       |
| 2     | Control (call/return/backtrack) | Hand-assembled member/append     |
| 3     | Compiler (AST → bytecode)       | Compile member/append            |
| 3.5   | Exceptions (throw/catch)        | Exception tests from tree-walker |
| 4     | Disjunction, ITE, bridge        | Control tests + builtin calls    |
| 5     | Indexing (switch tables)        | Indexed vs linear benchmarks     |
| 5.5   | Optimizations (peephole, LCO)   | Performance benchmarks           |
| 6     | GC (mark-and-sweep)             | Stress tests, long queries       |
| 7     | Attrs + CLP(FD) bridge          | CLP(FD) unit tests               |
| 8     | Tracing, DAP, ISO polish        | Trace parity, DAP smoke tests    |
| 9     | Pyodide, packaging              | Web demo, performance targets    |

### 17.3 Milestone Markers

**End of Phase 2**: Can execute simple recursive programs (member, append) by hand-assembling bytecode.

**End of Phase 3**: Compiler translates Prolog to WAM; user can write Prolog programs and run under WAM.

**End of Phase 5**: Performance improvements visible (indexing reduces clause scans).

**End of Phase 7**: CLP(FD) works under WAM (bridge to existing Python implementation).

**End of Phase 9**: WAM runs in browser via Pyodide; packaging complete.

### 17.4 Testing Strategy Evolution

- **Phase 0-1**: Unit tests with hand-crafted bytecode
- **Phase 2**: Hand-assembled predicates; solution comparison with tree-walker
- **Phase 3**: Compiled predicates; golden tests (compare bytecode output)
- **Phase 4+**: Differential tests (same query, both engines, compare solutions)
- **Phase 6**: Stress tests (millions of choicepoints, GC validation)
- **Phase 8**: Trace comparison (port events match tree-walker on same program)
- **Phase 9**: Web integration tests (Pyodide smoke tests)

---

## 18. Future Extensions

### 18.1 Tabling (SLG) [Phase 10 - Exploratory]

**Goal**: Memoize subgoal results to avoid recomputation and infinite loops.

**Mechanism**:
- **Table space**: Hash table keyed by subgoal variant
- **Answer trie**: Store solutions for each subgoal
- **Suspension frames**: Pause consumers when producer incomplete
- **Resumption**: Resume consumers when new answers available

**Integration with WAM**:
- New instructions: `table_lookup`, `table_insert`, `suspend`, `resume`
- Choicepoint extensions: Suspension frames save consumer state
- Interaction with cut: Cut does not discard tabled answers

**Challenges**:
- Interaction with CLP(FD): Tabling + constraint propagation is complex
- Tracer integration: How to visualize tabled calls?
- Performance: Table lookups must be fast (hash-based)

### 18.2 JIT Compilation Potential

**Speculative**: Compile hot bytecode sequences to native code.

**Approach**:
- Profile execution; identify hot loops
- Generate Python bytecode or LLVM IR for hot paths
- Inline common patterns (deref, unify, dispatch)

**Challenges**:
- Python JIT support limited (PyPy, Cython)
- Pure-Python constraint (Pyodide compatibility)
- Complexity vs benefit trade-off

### 18.3 Advanced Indexing

**Multi-Argument Indexing**:
- Index on first N arguments (not just first)
- Build multi-dimensional tables

**Deep Indexing**:
- Index on subterms (e.g., `p(f(X))` indexed by `f`)

**Dynamic Indexing**:
- Rebuild index tables at runtime based on usage patterns

### 18.4 Optimization Passes

**Global Optimizations**:
- Inline small predicates
- Dead code elimination (unreachable clauses)
- Constant folding (evaluate arithmetic at compile time)

**Specialization**:
- Generate specialized bytecode for common call patterns
- Example: `member(X, [a,b,c])` → specialized unrolled loop

---

## 19. Comparison with Classic WAM

### 19.1 Where We Follow the Standard

**Core Algorithms**:
- Unification: Classic deref + bind + trail
- Control: Call/return, choicepoints, cut semantics match Warren 1983
- Instruction categories: get/put/unify/set families are standard
- Environment frames: PrevE, CP, Y slots layout is standard

**Semantics**:
- Backtracking: Restore heap/trail/registers from choicepoint
- Cut: Prune choicepoints above saved level
- Unification properties: Idempotent, symmetric (modulo address choices)

### 19.2 Where We Diverge

**Cell Representation**:
- Classic: Tagged 32/64-bit words
- PyLog: Python tuples `(tag, ...)`
- Reason: Python doesn't expose low-level memory

**Dispatch**:
- Classic: Jump tables, threaded code
- PyLog: Dict/array dispatch (Phase 0-5)
- Reason: Python function call overhead; dict fast enough initially

**Trail**:
- Classic: Store addresses only; untrail resets to self-ref
- PyLog: Store `(type, addr, old_value)` tuples
- Reason: Support attributes, domains (generality)

**Registers**:
- Classic: Fixed-size register files, register windows
- PyLog: Python lists (dynamic size)
- Reason: Simpler; Python lists are efficient

**Heap Allocation**:
- Classic: Bump pointer in contiguous memory
- PyLog: Python list appends
- Reason: Python manages memory; we layer on top

### 19.3 Python Adaptations

**No Low-Level Memory Access**:
- Can't manipulate bits directly
- Can't use pointer arithmetic
- Solution: Use Python objects (tuples, lists) as abstractions

**GC Integration**:
- Classic WAM often has manual memory management
- PyLog: Stop-the-world GC layered on Python's GC
- Python's GC still runs (for non-heap objects)

**Performance Expectations**:
- Classic WAM (SWI-Prolog): ~1-10M inferences/sec
- PyLog WAM (Phase 5): ~10K-100K inferences/sec (100× slower)
- PyLog WAM (future JIT): ~100K-1M inferences/sec (10× slower)

**Acceptable Trade-off**: Educational value, portability, debuggability outweigh raw speed.

### 19.4 ISO Compliance Notes

**Goal**: Match ISO semantics where tree-walker already does.

**Scope**:
- Core logic (unification, control): ISO-compliant
- Built-ins: Subset of ISO (focus on common predicates)
- Modules: Simplified (no full ISO module system in MVP)

**Documented Deviations**:
- Some built-ins may have simplified semantics (documented)
- Error terms: Aim for ISO format but may differ in edge cases
- Operators: Handled by reader (tree-walker shared code)

---

## 20. References

### 20.1 Classic WAM Papers

1. **Warren, D. H. D.** (1983). *An Abstract PROLOG Instruction Set.* Technical Note 309, SRI International.
   - **The original WAM paper**; defines instruction set and execution model.

2. **Aït-Kaci, H.** (1991). *Warren's Abstract Machine: A Tutorial Reconstruction.* MIT Press.
   - **Tutorial reconstruction**; excellent for understanding WAM step-by-step.

3. **Demoen, B.** (Various). *Indexing of Logic Programs.*
   - **Indexing techniques**; first-argument and multi-argument indexing.

4. **Tarau, P.** (Various). *A Portable and Efficient Prolog Compiler.*
   - **Compiler design**; register allocation and code generation.

### 20.2 Implementation References

1. **SWI-Prolog Internals**: [https://www.swi-prolog.org/](https://www.swi-prolog.org/)
   - Production WAM implementation; study for inspiration (not strict adherence).

2. **Yap Prolog**: [https://www.dcc.fc.up.pt/~vsc/Yap/](https://www.dcc.fc.up.pt/~vsc/Yap/)
   - Another mature WAM implementation; optimized for performance.

### 20.3 PyLog-Specific Documentation

1. **Tree-Walker Architecture**: [../../ARCH.md](../../ARCH.md)
   - Current PyLog architecture; understand integration points.

2. **WAM Roadmap**: [wam-roadmap.md](wam-roadmap.md)
   - High-level plan; what and when.

3. **Phase Plans**: [phase-0-foundations.md](phase-0-foundations.md) through [phase-9-web-packaging.md](phase-9-web-packaging.md)
   - Detailed implementation steps per phase.

4. **Instruction Set Reference**: [wam-instruction-set.md](wam-instruction-set.md)
   - Complete instruction semantics and validation rules.

5. **Benchmark Methodology**: [wam-benchmarks.md](wam-benchmarks.md)
   - How to measure performance; benchmarks and targets.

### 20.4 Python-Specific References

1. **Python Data Model**: [https://docs.python.org/3/reference/datamodel.html](https://docs.python.org/3/reference/datamodel.html)
   - Understanding Python objects, tuples, lists.

2. **Pyodide Documentation**: [https://pyodide.org/](https://pyodide.org/)
   - Running Python in WASM; constraints and performance.

---

## Appendices

### Appendix A: Glossary

| Term           | Definition                                                     |
|----------------|----------------------------------------------------------------|
| **WAM**        | Warren Abstract Machine; register-based execution model for Prolog |
| **REF**        | Reference cell (unbound variable or alias)                    |
| **STR**        | Structure cell (compound term)                                |
| **CON**        | Constant cell (atom, integer, float)                          |
| **LIST**       | List cell (head + tail)                                       |
| **Deref**      | Dereference; follow REF chains to find root cell             |
| **Bind**       | Unify two cells; update heap and trail                       |
| **Trail**      | Log of mutations for undoing on backtracking                  |
| **Choicepoint**| Saved machine state for backtracking                          |
| **Environment**| Stack frame for permanent variables (Y registers)             |
| **X register** | Temporary/argument register                                   |
| **Y register** | Permanent register (stored in environment frame)             |
| **H**          | Heap top pointer                                              |
| **HB**         | Heap backtrack boundary (for trailing)                        |
| **P**          | Program counter (current instruction)                         |
| **CP**         | Continuation pointer (return address)                         |
| **E**          | Environment pointer (current frame)                           |
| **B**          | Choicepoint pointer (current backtrack point)                |
| **TR**         | Trail pointer (next free trail entry)                         |

### Appendix B: Instruction Quick Reference

See [wam-instruction-set.md](wam-instruction-set.md) for complete details.

**Control**: call, execute, proceed, allocate, deallocate
**Choicepoints**: try_me_else, retry_me_else, trust_me
**Cut**: get_level, cut, neck_cut
**Unify (get)**: get_variable, get_value, get_constant, get_structure
**Unify (put)**: put_variable, put_value, put_constant, put_structure
**Unify (unify)**: unify_variable, unify_value
**Unify (set)**: set_variable, set_value
**Indexing**: switch_on_term, switch_on_constant, switch_on_structure
**Exceptions**: throw, catch_setup, catch_cleanup
**Bridge**: call_builtin
**Debug**: noop, halt, dbg_snap

### Appendix C: State Snapshot Example

```json
{
  "regs": {
    "P": 142,
    "CP": 85,
    "E": 100,
    "B": 200,
    "H": 50,
    "HB": 40,
    "TR": 5
  },
  "X": [15, 20, null],
  "heap": [
    [0, 0],           // REF self-ref
    [2, "a"],         // CON 'a'
    [2, 42],          // CON 42
    [1, 10],          // STR pointing to functor at 10
    [2, ["f", 2]],    // Functor f/2
    // ...
  ],
  "frames": [
    {"addr": 100, "PrevE": 80, "CP": 85, "Y": [[0, 5], [2, "b"]]}
  ],
  "choicepoints": [
    {"addr": 200, "PrevB": null, "CP": 70, "E": 80, "P": 150, "H": 40, "TR": 3, "HB": 40}
  ],
  "trail": [
    ["bind", 5, [0, 5]],
    ["bind", 12, [0, 12]],
    ["bind", 18, [0, 15]]
  ]
}
```

---

**END OF DOCUMENT**

This architecture document provides a comprehensive, definitive reference for the PyLog WAM implementation. It synthesizes information from the roadmap, phase plans, and instruction set reference into a cohesive architectural view, suitable for implementers, reviewers, and maintainers.

For implementation details, consult the phase plans. For instruction semantics, see the instruction set reference. For benchmarking, use the benchmark methodology document.
