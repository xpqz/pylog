# ARCH.md

*A concise, implementation-ready architecture for a tree-walking Prolog interpreter in Python with an eventual CLP(FD) layer. Designed for learning and debuggability first, with stable interfaces that survive later stages (operators, attributes, CLP(FD), modules, DCGs).*

---

## 0. Design goals & guardrails

- **Learn-first correctness** over raw performance.
- **Explicit stacks, no Python recursion** (no recursion limit issues).
- **Centralized mutation** via `bind()`; every backtrackable change is trailed.
- **Stable public shapes** (terms, store, trail, engine hooks) from day one.
- **Engine purity**: CLP(FD) integrates via attributed variables, not engine hacks.
- **Observability**: tracer, snapshots, residual constraints, and graph export.

---

## 1. Project layout

```
prolog/
  parser/                # Lark grammar (operator-free) + reader (adds operators later)
    grammar.lark
    reader.py
  ast/
    terms.py             # Atom, Int, Var, Struct, List
    pretty.py
  unify/
    store.py             # Cells, Store, deref(), bind(), trail ops (Stage -1/0)
    unify.py
    occurs.py
  engine/
    engine.py            # run() loop, goal stack, choicepoints, cut, throw/catch
    indexing.py          # first-arg + type switching (Stage 2)
    builtins_iso.py      # true/fail/!/=/\=, var/nonvar, =../2, call/1, is/2, etc.
    tracer.py            # call/exit/redo/fail ports
  attrs/
    api.py               # put_attr/get_attr/del_attr
    hooks.py             # registration & attr_unify dispatch (Stage 4)
  clpfd/
    domain.py            # Domain (interval/holes/bitset), rev counters
    queue.py             # PropagationQueue with priorities
    props/               # linear, comparisons, sum, alldifferent, reif
      linear.py
      compare.py
      sum.py
      alldiff.py
      reif.py            # Stage 5.5
    label.py             # labeling strategies
    explain.py           # who pruned what (optional)
  debug/
    snapshot.py          # store/trail/choicepoints dump
    graphviz.py          # export_constraint_graph()
  tests/
    unit/                # small orthogonal tests
    scenarios/           # multi-file programs
```

---

## 2. Core data structures (stable)

### 2.1 Terms (AST)

```python
# ast/terms.py
from dataclasses import dataclass
from typing import Tuple, Optional

@dataclass(frozen=True)
class Atom:  name: str

@dataclass(frozen=True)
class Int:   value: int

@dataclass(frozen=True)
class Struct:
    functor: str
    args: Tuple['Term', ...]   # Term = Atom|Int|Struct|Var|List

@dataclass(frozen=True)
class List:
    items: Tuple['Term', ...]
    tail: 'Term' = Atom('[]')

@dataclass
class Var:
    id: int
    hint: Optional[str] = None   # pretty-print only
```

**Identity rule:** variable identity is the integer `Var.id`. Names/hints are I/O sugar only.

---

### 2.2 Store, Cells, Trail, Choicepoints

```python
# unify/store.py
from dataclasses import dataclass
from typing import Literal, Optional, Any, List

@dataclass
class Cell:
    tag: Literal["unbound", "bound"]
    ref: int                 # parent varid for unbound (union-find); self if root
    term: Optional[Any]      # bound Term

class Store:
    def __init__(self):
        self.cells: List[Cell] = []

    def new_var(self, hint=None) -> int:
        vid = len(self.cells)
        self.cells.append(Cell("unbound", vid, None))
        return vid
```

- **Dereferencing:** iterative union-find walk.  
- **Path compression:** only on mutating paths (inside `bind()`), trailed.

**Trail entry tags (append-only list):**
- `('parent', varid, old_parent)`
- `('bind', varid, old_cell)`  *(old_cell is a shallow copy)*
- Stage 4+: `('attr', varid, module, old_value_or_missing)`
- Stage 5+: `('domain', varid, old_domain)`

**Choicepoint:** 
```
{
  "goals": list snapshot,
  "cursor": ClauseCursor snapshot,
  "trail_top": int,
  "cut_barrier": int
}
```

Backtracking = pop to `trail_top`, restore goals/cursor.

---

### 2.3 Engine (explicit stacks)

```python
# engine/engine.py (sketch)
def run(goals):
    while True:
        if not goals:
            yield snapshot_bindings()
            if not backtrack(): return
            continue

        g = goals.pop()
        if is_builtin(g):
            if not exec_builtin(g):
                if not backtrack(): return
            continue

        key = (g.functor, len(g.args))
        clauses = predicate_table.get(key, [])
        cursor = ClauseCursor(clauses)
        push_choicepoint(goals, cursor, trail_top())
        while cursor.has_next():
            head, body = cursor.next_renamed()
            if unify(g, head):              # uses store.bind()
                goals.extend(reversed(body))
                break
            undo_to(cp_trail_top())
        else:
            if not backtrack(): return
```

- **Cut `!`** pops choicepoints to `cut_barrier`.  
- **Throw/catch** handled locally in the loop (structured exceptions).  
- **Indexing** (Stage 2) swaps `clauses` for indexed subsets.

---

## 3. Unification (central bind path)

```python
# unify/unify.py (sketch)
def deref(varid, store, compress=False, trail=None):
    # iterative; compress if mutating (trail-aware), as discussed
    ...

def bind(var_or_varid, term, store, trail) -> bool:
    # 1) deref both sides
    # 2) occurs-check (configurable)
    # 3) attribute hooks (Stage 4)
    # 4) union or bind with trail entries
    ...
```

**Modes:** `occurs_check = off` by default; `on` for tests. *Rational trees supported when off.*

**Invariants:** 
- Only `bind()` mutates store/attributes/domains.  
- Every mutation pushes exactly one inverse op on the trail.

---

## 4. Parser & Reader

- **Stage 0/1:** Lark grammar for atoms/ints/vars/lists/terms/clauses.  
- **Stage 1.5:** Add a *reader* (Pratt or shunting-yard) that folds tokens into operator trees using a static `op/3` table (no modules yet).  
- Operators added later do not disturb the grammar or engine.

---

## 5. Built-ins (initial)

- `true/0`, `fail/0`, `!/0`, `=/2`, `\=/2`, `var/1`, `nonvar/1`, `atom/1`, `integer/1`, `=../2`, `call/1`, `throw/1`, `catch/3`.  
- Arithmetic via `is/2` with integers: `+ - * // mod`.  
- Later: comparators as operators (`=:=`, `=\=`, `<`, `=<`, `>`, `>=`).

---

## 6. Indexing (Stage 2)

- **First-argument indexing**: map principal functor/tag to clause buckets.  
- **Switch on type** (var/atom/int) prefilter.  
- Optional tail-call micro-opt (frame reuse) while staying tree-walk.

---

## 7. Debug & Observability (Stage 3)

- **Ports tracer**: `call/exit/redo/fail` with depth, pretty bindings.  
- **Pretty printer**: stable var names by creation order (`A,B,C…`).  
- **Introspection**: store size, trail length, choicepoint count, pending propagators.  
- **Graph export**: `export_constraint_graph(vars, constraints) -> DOT`.

---

## 8. Attributed Variables (Stage 4)

**Public API:** `put_attr/3`, `get_attr/3`, `del_attr/2`.  
**Internal:** `attrs: dict[varid, dict[module, value]]`.

**Hook path:** in `bind()`, when either side has attributes, dispatch `attr_unify_hook(Module, VarOrTerm, Other)`; trail attribute updates via `('attr', ...)`.

*No solver yet—this is the substrate CLP(FD) plugs into.*

---

## 9. CLP(FD) architecture (Stage 5+)

### 9.1 Domain representation (interval ↔ holes ↔ bitset)

```python
# clpfd/domain.py (essentials)
BITSET_CUTOFF = 64

@dataclass
class Domain:
    lo: int; hi: int
    holes: Optional[set[int]] = None   # forbidden values
    mask: Optional[int] = None         # allowed bitset when width <= cutoff
    rev: int = 0                       # revision counter (per-var)

    # contains(), size(), is_singleton(), choose_value_min(), values_iter()
    # tighten_lo(), tighten_hi(), remove_value(), intersect_with()
    # _maybe_rebuild_repr(): switch to bitset when width <= cutoff
```

- **Bitset for small ranges** (≤64) → fast membership/removal.  
- **Large ranges** → interval (+ sparse holes when needed).  
- **`rev`** increments on any change. (Python ints are unbounded; an optional epoch exists if you want hard wrap.)

### 9.2 Propagation queue (priorities + fairness)

```python
# clpfd/queue.py
class PropagationQueue:
    def __init__(self):
        self.high, self.med, self.low = set(), set(), set()
        self.running = None

    def schedule(self, pid: int, prio="med"):
        if pid != self.running:
            (self.high if prio=="high" else self.med if prio=="med" else self.low).add(pid)

    def pop(self):
        if self.high: return self.high.pop()
        if self.med:  return self.med.pop()
        if self.low:  return self.low.pop()
        return None
```

**Fixpoint:** run until `pop()` returns `None`.  
**Loop control:** monotone domains, self-requeue guard (`running`), and no-op detection via `rev` caches per propagator.

### 9.3 Propagators

- **Scope** (vars), **cached revs**, `run()` that:
  - reads domains, prunes monotone, posts `('domain', vid, old_dom)` trail entries,
  - increments domain `rev` on change,
  - notifies watchers via `queue.schedule(pid, prio)`.

Initial set:
- `#=/2`, `#</2`, `#=</2`, `#>/2`, `#>=/2` (bounds-consistent).  
- Linear sums (small coefficients).  
- `sum(List) #= Const`, `sum(List) #>= Const`, etc.

### 9.4 Posting & watchers

- Post ⇒ register propagator with each var’s watch list; schedule once.  
- Notify watchers when a var’s domain `rev` bumps.

### 9.5 Labeling (separate from propagation)

- Var selection: `first`, `ff` (first-fail).  
- Value choice: `indomain_min` (default), `bisect`.  
- Creates choicepoints; after each decision, run propagation to fixpoint.

### 9.6 Reification (Stage 5.5)

- Boolean var `B ∈ {0,1}` and constraint `C`.  
- **Directions:**
  - `B=1` ⇒ post `C`.
  - `B=0` ⇒ post `¬C` (or dis-entail propagator).
  - `C` entailed ⇒ `B=1`; disentailed ⇒ `B=0`.  
- Entailment checks are *per constraint* (bounds proofs for comparisons; quick disproofs for `all_different`, etc.).  
- Re-entrancy guard: `queue.running` prevents immediate self-reschedule loops.

### 9.7 Global constraints (Stage 6+)

- `all_different/1`:
  - Stage 6.0: pairwise `#\=/2` for N≤6.
  - Stage 6.5: bounds-consistent **Hall intervals** (O(n log n)).  
  - (Optional later) matching-based stronger consistency.

- `global_cardinality/2`: value counters with min/max, relaxed flow feasibility.

---

## 10. Memory & performance notes

- **Trail segmentation** per choicepoint (`trail_top`): fast O(k) undo.  
- **Compression** only during mutation and for long paths, trailed.  
- **Domain trailing** stores the old domain snapshot (cheap; domains are small objects).  
- **Deterministic sections** may trim trail capacity (optional; Python lists reuse capacity anyway).

---

## 11. Testing strategy (continuous)

- **Property tests** for unification (idempotence, symmetry), trail inversion, alias chains.  
- **Occurs-check** modes; cyclic terms behavior.  
- **Stress**: millions of choicepoints with quick undo.  
- **Propagation confluence**: shuffled posting reaches same fixpoint.  
- **Reification correctness** on entail/disentail/unknown cases.  
- **Global constraints**: early failure (pigeonhole), Hall pruning.

---

## 12. LLM-assisted implementation guidance

- Keep files small & single-purpose; expose *stable* function signatures in skeletons.  
- Ask the LLM to implement inside given function stubs only; forbid global side effects.  
- Always include/update unit tests alongside code changes.  
- Use tracer/snapshots to produce failing, minimal repros when debugging.
