# Stage -1: Unifier Workbench - Detailed Implementation Plan

## Overview
Build and prove out the core unification engine with `Store`, `deref`, `bind`, and `trail` operations in isolation. This forms the foundation for all subsequent stages.

## Core Components

### 1. Store Implementation (`prolog/unify/store.py`)

#### Data Structures
```python
@dataclass
class Cell:
    tag: Literal["unbound", "bound"]
    ref: int                 # parent varid for unbound (union-find); self if root
    term: Optional[Term]     # bound Term when tag="bound"
    rank: int = 0           # union-by-rank optimization (only meaningful for unbound roots)

class Store:
    cells: List[Cell]        # indexed by varid
    # No next_id needed - use len(cells)
```

#### Key Methods
- `new_var(hint=None) -> int`: Create new unbound variable, return its ID
- `deref(varid) -> tuple`: Follow union-find chains, returns tagged result
- `union_vars(a_root, b_root, store, trail) -> bool`: Union with rank heuristic
- `bind_root_to_term(root_vid, term, store, trail) -> bool`: Bind with trailing
- `get_cell(varid) -> Cell`: Direct cell access for debugging only

#### Deref API
```python
def deref(varid: int, store, compress: bool=False, trail=None):
    # Returns one of:
    # ("BOUND", root_vid, bound_term)
    # ("UNBOUND", root_vid)
    # Path compression only when compress=True AND trail provided
    # Compression threshold: paths >= 4 nodes
```

### 2. Trail Implementation (`prolog/unify/trail.py`)

#### Trail Entry Types
```python
TrailEntry = Union[
    tuple[Literal["parent"], int, int],      # ('parent', varid, old_parent)
    tuple[Literal["bind"], int, Cell],       # ('bind', varid, old_cell_copy)
    tuple[Literal["rank"], int, int]         # ('rank', varid, old_rank)
]
```

#### Trail Operations
- `push(entry)`: Add entry to trail
- `mark() -> int`: Return current trail position
- `undo_to(mark, trail, store)`: Restore store state to marked position
- `clear()`: Empty trail (for testing)

#### Context Manager
```python
@contextmanager
def trail_guard(trail, store):
    mark = len(trail)
    try:
        yield mark
    except Exception:
        undo_to(mark, trail, store)
        raise
```

### 3. Term Representation (`prolog/ast/terms.py`)

#### Basic Terms
```python
@dataclass(frozen=True)
class Atom:
    name: str

@dataclass(frozen=True)
class Int:
    value: int

@dataclass(frozen=True)
class Struct:
    functor: str
    args: Tuple['Term', ...]

@dataclass(frozen=True)
class List:
    items: Tuple['Term', ...]
    tail: 'Term' = Atom('[]')

@dataclass
class Var:
    id: int
    hint: Optional[str] = None
```

### 4. Unification Algorithm (`prolog/unify/unify.py`)

#### Explicit Iterative Implementation
```python
def unify(t1, t2, store, trail, occurs_check=False) -> bool:
    stack = [(t1, t2)]
    while stack:
        a, b = stack.pop()
        
        # Fast path: identical immutable objects
        if a is b:
            continue
            
        # Deref to (VAR, root_id) or (NONVAR, term)
        tag_a, val_a = deref_term(a, store)
        tag_b, val_b = deref_term(b, store)
        
        # Handle all combinations...
        # Push structural pairs onto stack for iteration
```

#### Helper Functions
- `deref_term(term, store) -> tuple[str, Any]`
  - Returns ('VAR', root_vid) or ('NONVAR', dereferenced_term)
  - No path compression during read

- `bind_root_to_term(root_vid, term, store, trail) -> bool`
  - Trails old cell state
  - Updates cell to bound

- `union_vars(a_root, b_root, store, trail) -> bool`
  - Union by rank to keep trees shallow
  - Trails parent and rank changes

### 5. Occurs Check (`prolog/unify/occurs.py`)

#### Iterative Implementation
```python
def occurs(varid: int, term, store) -> bool:
    stack = [term]
    seen = set()  # Use id() for immutable terms
    while stack:
        # Process iteratively, union-find aware
        # Early exit if variable becomes bound
```

### 6. Interactive REPL (`prolog/unify/repl.py`)

#### Commands
- `unify <term1> <term2>`: Attempt unification, show bindings
- `show`: Display current store state with statistics
- `mark`: Create trail mark, return mark ID
- `undo [mark]`: Revert to mark or last unification
- `reset`: Clear store and trail
- `occurs on/off`: Toggle occurs check mode
- `stats`: Show cells count, trail length, max rank
- `seed <n>`: Generate random test case with seed
- `exit`: Quit REPL

#### Cycle-Aware Pretty Printer
- Detect and handle cyclic structures when occurs check is off
- Print `...` when revisiting a term
- Track visited terms by identity

## Testing Strategy

### Unit Tests (`prolog/tests/unit/test_unify.py`)

1. **Basic Unification**
   - Atom-atom: equal/different
   - Int-int: equal/different
   - Var-term: binding creation
   - Var-var: union creation with rank

2. **Structural Unification**
   - Struct-struct: same/different functors, arity
   - List-list: various lengths, with tails
   - Nested structures up to depth 100

3. **Trail and Undo**
   - Single undo
   - Multiple undos to different marks
   - Undo after failed unification (no changes)
   - Trail guard context manager

4. **Union-Find with Rank**
   - Long chains: X=Y, Y=Z, Z=W, W=a
   - Path compression verification (only during mutation)
   - Rank updates and trailing
   - Correctness after undo

5. **Occurs Check**
   - Accept when off: X = f(X) (rational trees)
   - Reject when on: X = f(X)
   - Deep nesting: X = f(g(h(X)))
   - Early exit when variable bound during traversal

### Property Tests (`prolog/tests/unit/test_unify_properties.py`)

1. **Symmetry**: `unify(A, B) == unify(B, A)` on fresh stores
2. **Idempotence**: Second unify produces no new trail entries
3. **Trail Invertibility**: `undo_to(mark)` exactly reverses changes
4. **No Leaks**: Store size matches variable count
5. **Confluence**: Different unification orders reach same final state

### Stress Tests (`prolog/tests/unit/test_unify_stress.py`)

1. **Large Structures**: 10000+ element lists
2. **Deep Nesting**: 1000+ levels (iterative handles without stack overflow)
3. **Many Variables**: 100000+ variables
4. **Batch Undo**: 10000 operations, undo in groups
5. **Path Compression**: Verify compression only on mutation

## Implementation Order

1. **Phase 1: Core Data Structures**
   - `terms.py`: Term classes (frozen dataclasses)
   - `store.py`: Cell with rank, Store basics
   - Basic unit tests

2. **Phase 2: Unification Without Occurs Check**
   - `trail.py`: Trail operations with context manager
   - `unify.py`: Iterative algorithm with pair stack
   - Union by rank implementation
   - Symmetry and idempotence tests

3. **Phase 3: Occurs Check & Rational Trees**
   - `occurs.py`: Iterative occurs check
   - Update `unify.py` to use occurs check
   - Rational tree tests (occurs_check=False)
   - Cyclic structure tests

4. **Phase 4: REPL and Debugging**
   - `repl.py`: Interactive testing tool
   - Cycle-aware pretty printer
   - Statistics and introspection
   - Random test generation

5. **Phase 5: Optimization and Stress Testing**
   - Path compression with trailing (mutation only)
   - Stress tests
   - Performance measurement and trending

## Success Metrics

1. **Correctness**
   - All unit tests pass
   - Property tests pass with 1000+ random cases each
   - No memory leaks in stress tests
   - Rational trees work correctly when occurs_check=False

2. **Performance (Tracking Metrics, not hard targets)**
   - Track ops/second trend in CI (non-gating)
   - Log counters: n_bind, n_parent, n_rank_bumps, n_deref_reads
   - Record in CI artifacts, fail only on >50% regression

3. **Robustness**
   - Handle 1M variables without issues
   - No Python recursion (fully iterative)
   - Consistent behavior with occurs check on/off
   - Clean failure without store corruption

## Design Decisions & Rationale

1. **Explicit Iterative with Pair Stack**
   - No hidden recursion
   - Easy to add features later (attrs, CLP(FD))
   - Clear control flow

2. **Union by Rank**
   - Keeps trees shallow before compression
   - Simple heuristic with proven benefits
   - Properly trailed for undo

3. **Tagged Deref Results**
   - Unambiguous API
   - Avoids repeated lookups
   - Clear separation of VAR/NONVAR cases

4. **Path Compression Only During Mutation**
   - Keeps reads side-effect free
   - Simpler reasoning about correctness
   - Compression threshold prevents over-trailing

5. **Trail Context Manager**
   - Guarantees cleanup even on exceptions
   - Simplifies test writing
   - Clear scope for temporary changes

6. **Cycle-Aware Pretty Printing**
   - Essential for debugging rational trees
   - Prevents runaway output
   - Makes REPL usable with cyclic structures

## Key Invariants

1. **All mutations go through bind() or union_vars()**
2. **Every mutation is trailed before the change**
3. **Deref without compress is side-effect free**
4. **Terms (except Var) are immutable**
5. **Trail can always restore to any previous mark**

## Open Questions Resolved

1. **Path compression on reads?** No - keep reads pure, compress only during mutation
2. **REPL file loading?** Start with seed command for random generation
3. **Benchmarks in CI?** Yes, but non-gating trend tracking
4. **Rational trees?** Yes - standard Prolog behavior when occurs_check=False
5. **Cycle-aware printer?** Yes - essential for rational tree debugging

## Dependencies

- Python 3.10+ (for match statements and type hints)
- No external dependencies for core (pure Python)
- pytest for testing
- Optional: hypothesis for property testing