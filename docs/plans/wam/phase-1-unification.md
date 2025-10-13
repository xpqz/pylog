# Phase 1 — Core unification + term access

Objective
- Implement classic WAM unification and term access instructions in Python, sufficient to prove facts and small structured terms.

Outcomes
- Deterministic unification via `get/put` + `unify/set` sequences.
- Heap/trail invariants validated by tests; occurs‑check (optional flag) stubbed.

Non‑Goals (Phase 1)
- No predicate call/return or backtracking yet (that’s Phase 2).
- No compiler drive‑by beyond tiny helpers for hand‑assembling tests.
- No indexing or optimizations.

Scope
- In: `get_variable/value/constant/structure`, `put_*`, `unify_variable/value`, `set_variable/value`.
- Out: Control flow (`call/execute/proceed`), choicepoints, compiler.

Dependencies
- Phase 0 foundation modules and dispatcher.

Design
- Cell shapes:
  - REF: `(REF, addr)`
  - STR: `(STR, functor_addr)` where `heap[functor_addr] = (CON, (name, arity))`
  - CON: `(CON, Atom|Int|Float)`
  - LIST: `(LIST, head_addr, tail_addr)` (or STR with functor `./2` if preferred)
- **Decision point on LIST representation**: Choose by end of Phase 1 based on:
  - Implementation simplicity (dedicated LIST cell vs reusing STR)
  - Performance implications (cell size, dispatch overhead)
  - Debuggability (explicit LIST in snapshots vs generic `./2`)
  - Document choice and rationale in Phase 1 completion notes.
- Deref: loop following REF chains; returns `(tag, addr)` and the address of the root.
- Trailing: on binding a REF to non‑REF under a trail segment.

Detailed Instruction Semantics

Put (write to argument registers / heap build)
- `put_variable Xi, Aj`: set `Xi = new_ref(self_ref)`; write `Xi` into argument register `Aj`.
- `put_value Xi, Aj`: copy pointer/value of `Xi` into `Aj`.
- `put_constant C, Aj`: allocate constant if needed; set `Aj = CON(C)`.
- `put_structure F/N, Aj` (write mode):
  1) allocate functor cell: `f = new_functor(F,N)` (as `CON` with pair `(F,N)`)
  2) allocate structure cell: `s = (STR, addr(f))`; set `Aj = s` and push `s` to heap
  3) set unify mode = write; the following `set_variable/set_value` fill arguments.

Get (read from argument registers / unify)
- `get_variable Xi, Aj`: set `Xi = Aj` (pointer copy)
- `get_value Xi, Aj`: unify contents of `Xi` and `Aj` (read mode)
- `get_constant C, Aj`: unify `Aj` with `CON(C)` (read mode)
- `get_structure F/N, Aj` (read mode):
  1) deref `Aj`; if REF: bind to new structure with functor `F/N`, set mode=write
  2) if `Aj` is `STR` with functor `F/N`: set mode=read to check arguments
  3) else fail

Unify/Set (argument sequence depends on mode)
- Write mode:
  - `set_variable Xi`: allocate new REF; write it to heap; set `Xi` to that REF
  - `set_value Xi`: write the (possibly deref’d) value of `Xi` to heap
- Read mode:
  - `unify_variable Xi`: deref next heap argument; set `Xi` to that address
  - `unify_value Xi`: deref and unify `Xi` with next heap argument

Unification Algorithm (pseudo‑code)
```
def deref(addr):
    while is_ref(heap[addr]) and heap[addr].addr != addr:
        addr = heap[addr].addr
    return addr

def unify(a, b):
    stack = [(deref(a), deref(b))]
    while stack:
        u, v = stack.pop()
        if u == v:  # same cell
            continue
        tag_u, tag_v = tag(heap[u]), tag(heap[v])
        if tag_u == REF or tag_v == REF:
            bind_ref(u, v)  # trailing as needed
        elif both CON:
            if heap[u] != heap[v]: return FAIL
        elif both STR with same functor:
            # push their arguments pairwise
            for i in range(arity):
                stack.append((arg_addr(u,i), arg_addr(v,i)))
        elif both LIST:
            stack.append((head(u), head(v)))
            stack.append((tail(u), tail(v)))
        else:
            return FAIL
    return OK

def bind_ref(u, v):
    # u and v are deref’d addresses; at least one is REF
    if tag(heap[u]) != REF: u, v = v, u
    # trailing criterion (classic WAM): trail if u < HB
    if u < HB:
        trail.append(u)
    heap[u] = make_ref(v)
```

Trailing Rules
- Bind a REF to any non‑REF (or another REF) and the REF address is older than the current heap backtrack boundary `HB` ⇒ trail it.
- On backtracking, untrail by restoring REF cells to self‑refs.

Mode Transitions
- `get_structure` can switch the unify sequence to write mode (when encountering a free variable at the argument) or read mode (when matching an existing structure).
- The mode persists only for the subsequent `unify/set` sequence of that structure.

Worked Example (head matching)
Predicate head: `p(f(g(X), a))`

```
get_structure f/2, A1
  unify_structure g/1
    unify_variable X1
  unify_constant a
```

Heap outcome on matching `A1 = f(g(Y), a)` binds `X1` to `Y` and succeeds; mismatched functors fail.

Implementation Tasks
- Add `heap.py`: helpers `new_ref()`, `new_con(value)`, `new_str(name, arity)`, `set_ref(addr, target)`.
- Add `unify.py` under `prolog/wam/` implementing read/write modes for `unify_*` sequences.
- Implement instruction handlers for all `get/put/unify/set` variants; wire into dispatcher.
- Add small assembler helpers for tests (emit instruction tuples and data area).
 - Implement invariants checker (heap address ranges, canonical REFs) and integrate into tests.
 - Add a tiny `run_sequence(code, X_in)` helper to drive instruction lists in tests.

Tests
- Unit: deref/trail basics; unify var–var, var–const, const–const (fail), structure equality, list equality.
- Edge cases: shared variables unify consistently; trailing only when needed; occurs‑check off; on (xfail initially).
- Golden: print heap after unification; compare compact rendering to expected.
 - Symmetry: `unify(X,Y)` behaves same as `unify(Y,X)` (modulo address choices); heap invariants preserved.
 - LIST vs `./2`: whichever representation chosen, ensure equivalence tests exist.
 - Occurs‑check (optional): property test that `X = f(X)` fails only when flag is enabled.

Acceptance Criteria
- Hand‑assembled sequences can prove `p(a).`, `p(f(X)).`, `p([a|X]).`-style structures.
- All unit tests under `test_wam_unify.py` green; heap/trail invariants hold.
 - Invariants checker passes before/after unifications across the suite.

Risks
- Incorrect trailing causing leaks/corruption. Mitigate with focused tests and heap check utility.
 - Non‑canonical list representation mixing; settle on LIST or `./2` and enforce via helpers.
 - Hidden Python object allocations in hot paths; keep data flat and avoid per‑step conversions.

Rollout
- Merge behind flag; no user‑visible changes.

Implementation Checklist
- [ ] Heap helpers
- [ ] Deref + trail
- [ ] `get/put` for const/var/structure
- [ ] `unify/set` pairs
- [ ] Tests (unit + golden)
 - [ ] Invariants checker + symmetry tests
