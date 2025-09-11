# Stage 2: Indexing Implementation Plan

*First-argument indexing and type switching for performance optimization without semantic changes.*

---

## Overview

Stage 2 introduces indexing optimizations to improve clause selection performance. The implementation uses first-argument indexing (principal functor → clause bucket) and type switching to reduce unnecessary unification attempts.

## Goals

1. **Significant performance improvement** for predicate lookups
2. **No semantic changes** - identical solutions and ordering
3. **Transparent integration** - no changes to existing tests
4. **Measurable speedup** on standard benchmarks

## Architecture

### Core Components

#### 1. IndexedProgram
- Replaces flat clause list with indexed structure
- Maps `(predicate_name, arity)` → PredIndex
- Each predicate has its own index structure

#### 2. Per-Predicate Index Structure
```python
class PredIndex:
    """Index for a single predicate's clauses."""
    def __init__(self):
        # Source order of clause IDs for this predicate
        self.order: List[int] = []
        # Buckets for first-argument types
        self.var_ids: Set[int] = set()              # Heads with variable first arg
        self.empty_list_ids: Set[int] = set()       # Heads with []
        self.int_ids: Set[int] = set()              # Heads with integer first arg
        self.list_nonempty_ids: Set[int] = set()    # Heads with [H|T] i.e. '.'/2
        self.struct_functor: Dict[Tuple[str,int], Set[int]] = {}  # Heads with f/n

class ClauseIndex:
    """Global index mapping predicates to their clause indices."""
    def __init__(self):
        # Map from (pred_name, pred_arity) to PredIndex
        self.preds: Dict[Tuple[str,int], PredIndex] = {}
        # Map from (pred_key, clause_id) to actual Clause
        self.clauses: Dict[Tuple[Tuple[str,int], int], Clause] = {}
```

#### 3. First-Argument Analysis
- Extract principal functor from clause heads
- Handle special cases **per predicate**:
  - Variables → var_ids bucket (match everything)
  - Atoms (except `[]`) → struct_functor `(name, 0)` bucket
  - Empty list `[]` → empty_list_ids bucket
  - Non-empty lists `[H|T]` → list_nonempty_ids bucket  
  - Integers → int_ids bucket
  - Structures → struct_functor `(functor, arity)` bucket

### Integration Points

#### Engine Modifications
```python
# In Engine.__init__
self.program = IndexedProgram(clauses) if use_indexing else Program(clauses)

# In Engine.get_matching_clauses(goal)
pred_key = (goal.functor, len(goal.args)) if is_struct(goal) else (goal, 0)
if isinstance(self.program, IndexedProgram):
    return self.program.select(pred_key, goal)
else:
    # Fallback: filter by predicate from flat list
    return [c for c in self.program.clauses if matches_predicate(c, pred_key)]
```

#### Clause Selection Algorithm
```python
def select(self, pred_key: Tuple[str, int], goal) -> Iterable[Clause]:
    """Select clauses preserving source order."""
    if pred_key not in self.preds:
        return []
    
    p = self.preds[pred_key]
    
    # 1. Dereference goal's first argument
    first_arg = deref(goal.args[0]) if goal.args else None
    
    # 2. Build candidate set based on first arg type
    candidates: Set[int] = set()
    
    if is_var(first_arg):
        candidates = set(p.order)  # All clauses
    elif is_atom(first_arg) and first_arg.name != '[]':
        candidates |= p.var_ids
        candidates |= p.struct_functor.get((first_arg.name, 0), set())
    elif is_atom(first_arg) and first_arg.name == '[]':
        candidates |= p.var_ids
        candidates |= p.empty_list_ids
    elif is_int(first_arg):
        candidates |= p.var_ids
        candidates |= p.int_ids
    elif is_list_nonempty(first_arg):  # [H|T] structure
        candidates |= p.var_ids
        candidates |= p.list_nonempty_ids
    elif is_struct(first_arg):
        candidates |= p.var_ids
        key = (first_arg.functor, len(first_arg.args))
        candidates |= p.struct_functor.get(key, set())
    
    # 3. Yield clauses in original source order
    for clause_id in p.order:
        if clause_id in candidates:
            yield self.clauses[(pred_key, clause_id)]
```

## Implementation Phases

### Phase 1: Basic Infrastructure
**Goal:** Set up indexing data structures

**Tasks:**
- [ ] Create `prolog/engine/indexing.py`
- [ ] Implement `ClauseIndex` class
- [ ] Add `IndexedProgram` wrapper
- [ ] Write unit tests for index building

**Tests:**
```python
def test_per_predicate_index_building():
    """Ensure clauses are indexed per predicate, not globally."""
    clauses = parse_program("""
        foo(a).
        foo(b).
        foo(X) :- bar(X).
        baz(1).
        baz(X).
    """)
    index = ClauseIndex(clauses)
    
    # foo/1 should have its own index
    foo_idx = index.preds[('foo', 1)]
    assert len(foo_idx.order) == 3  # Three foo clauses
    assert len(foo_idx.var_ids) == 1  # One with var head
    
    # baz/1 should be separate
    baz_idx = index.preds[('baz', 1)]
    assert len(baz_idx.order) == 2
    assert len(baz_idx.int_ids) == 1
    assert len(baz_idx.var_ids) == 1
```

### Phase 2: First-Argument Indexing
**Goal:** Implement functor-based indexing

**Tasks:**
- [ ] Implement first-argument extraction
- [ ] Build functor → clause mapping
- [ ] Handle special cases (vars, lists)
- [ ] Integrate with engine's clause selection

**Tests:**
```python
def test_functor_indexing():
    """Verify correct clause selection by functor."""
    program = """
        p(a, 1).
        p(b, 2).
        q(c, 3).
    """
    # Query p(a, X) should only try first two clauses
```

### Phase 3: Type Switching
**Goal:** Add type-based pre-filtering

**Tasks:**
- [ ] Implement type detection for first argument
- [ ] Build type → clause mapping
- [ ] Combine with functor indexing
- [ ] Optimize var bucket handling

**Tests:**
```python
def test_type_switching():
    """Verify type-based clause filtering."""
    program = """
        process(1) :- !.
        process([H|T]) :- process(T).
        process(foo(X)) :- process(X).
    """
    # Query with integer should only try first clause
```

### Phase 4: Performance Validation
**Goal:** Verify measurable speedup

**Tasks:**
- [ ] Create benchmark suite
- [ ] Measure clause selection overhead
- [ ] Compare indexed vs linear performance
- [ ] Document performance gains

**Benchmarks:**
```python
# Benchmark 1: Large fact base
# 1000 facts of form fact(atom_i, data)
# Query: fact(atom_500, X)
# Expected: ~500x speedup

# Benchmark 2: Type dispatch
# Multiple clauses with different first-arg types
# Query: Mixed type queries
# Expected: 3-5x speedup

# Benchmark 3: Recursive predicates
# append/3, member/2 with large lists
# Expected: 2-3x speedup
```

## Testing Strategy

### Critical Correctness Tests

#### 1. Order Preservation with Interleaved Var Heads
```python
def test_order_preservation_with_var_heads():
    """Variable heads must not disrupt source order."""
    program = """
        p(a).           % 1
        p(X) :- b(X).   % 2 (var head)
        p(b).           % 3
    """
    # Query p(a) must try #1 then #2 (not skip to #3)
    # Query p(b) must try #2 then #3 (not #1)
```

#### 2. Empty List vs Non-Empty List Separation  
```python
def test_list_type_separation():
    """[] and [H|T] require separate buckets."""
    program = """
        q([]).          % Only for empty list
        q([_|_]).       % Only for non-empty list
        q(X).           % For any list
    """
    # q([]) tries #1 and #3, never #2
    # q([1]) tries #2 and #3, never #1
```

#### 3. Struct Functor Discrimination
```python
def test_struct_functor_filtering():
    """Different functors must use different buckets."""
    program = """
        r(f(_)).
        r(g(_)).
        r(_).
    """
    # r(f(1)) tries r(f(_)) then r(_), never g(_)
```

#### 4. Dereferencing Critical
```python
def test_deref_before_selection():
    """Selection must use dereferenced first argument."""
    program = """
        s(a) :- !.
        s(b).
        test(Y) :- Y = a, s(Y).
    """
    # When s(Y) is called with Y bound to 'a',
    # must select s(a) clause, not all s/1 clauses
```

#### 5. Predicate Isolation
```python
def test_predicate_isolation():
    """Different predicates never share buckets."""
    program = """
        p(a).
        q(a).
    """
    # Selecting for p/1 must never return q/1 clauses
```

### Unit Tests
- Index building correctness
- Per-predicate bucket organization
- Type detection accuracy
- Edge cases (empty programs, single clause)

### Integration Tests
- All Stage 1 tests must pass unchanged
- Solution order preservation
- Backtracking behavior identical
- Cut semantics preserved

### Performance Tests
```python
@pytest.mark.benchmark
def test_indexing_speedup():
    """Verify indexing provides measurable speedup."""
    # Generate 1000 facts
    facts = [f"fact(a{i}, {i})." for i in range(1000)]
    program = parse_program("\n".join(facts))
    
    # Time with indexing
    indexed_engine = Engine(program, use_indexing=True)
    indexed_time = time_query(indexed_engine, "fact(a500, X)")
    
    # Time without indexing
    linear_engine = Engine(program, use_indexing=False)
    linear_time = time_query(linear_engine, "fact(a500, X)")
    
    # Expect significant speedup (30x-300x range)
    speedup_ratio = linear_time / indexed_time
    assert speedup_ratio >= 30, f"Insufficient speedup: {speedup_ratio:.1f}x"
    
def test_small_predicate_no_regression():
    """Small predicates should not regress."""
    program = """
        tiny(a).
        tiny(b).
        tiny(c).
    """
    # Performance should be within ±20% of linear scan
```

### Property Tests
```python
@given(program=prolog_programs(), query=prolog_queries())
def test_indexing_preserves_semantics(program, query):
    """Indexing must not change solutions or ordering."""
    indexed = Engine(program, use_indexing=True)
    linear = Engine(program, use_indexing=False)
    
    indexed_solutions = list(indexed.run(query))
    linear_solutions = list(linear.run(query))
    
    assert indexed_solutions == linear_solutions
```

## Success Criteria

### Functional
- ✅ All Stage 1 and 1.5 tests pass unchanged
- ✅ Solution order identical to linear scan
- ✅ Backtracking behavior preserved
- ✅ No semantic changes

### Performance
- ✅ 30x-300x speedup for large fact bases (mid-table queries)
- ✅ 3-5x speedup for type dispatch scenarios
- ✅ 2-3x speedup for typical recursive predicates
- ✅ No regression (within ±20%) for small predicates (≤3 clauses)

### Code Quality
- ✅ Clean separation of concerns (indexing module)
- ✅ Backward compatible (indexing optional)
- ✅ Well-documented index structure
- ✅ Comprehensive test coverage

## Implementation Notes

### Key Implementation Principle
**Order ∩ Candidates Pattern**: The core correctness invariant is to filter candidates through source order, never concatenate buckets. This ensures identical solution ordering:
```python
# CORRECT: Filter through order
for clause_id in p.order:
    if clause_id in candidates:
        yield clause

# WRONG: Concatenate buckets (breaks ordering)
yield from var_clauses + type_clauses + functor_clauses
```

### Design Decisions
1. **Per-predicate indexing** - Each predicate has its own PredIndex to avoid cross-contamination
2. **Source order preservation** - Critical for Prolog semantics
3. **Variable clauses always checked** - Cannot be filtered out
4. **Index built once** - At program load time
5. **Dereference before selection** - Must use bound values for correct bucket selection
6. **Separate [] and [H|T]** - Empty list is an atom, non-empty is '.'/2 structure

### Optimization Opportunities
- **Multi-argument indexing** (future stage)
- **JIT index building** (build on first query)
- **Argument type vectors** (beyond first arg)
- **Trie-based indexing** (for deep structures)

### Edge Cases
- Programs with all variable heads
- Single clause predicates (no benefit)
- Deeply nested first arguments
- Mixed indexable/non-indexable predicates

## Rollout Plan

### Week 1: Infrastructure
- Set up indexing module
- Implement basic data structures
- Unit tests for index building

### Week 2: Integration
- Wire into engine
- Implement clause selection
- Ensure backward compatibility

### Week 3: Optimization
- Type switching
- Performance tuning
- Benchmark suite

### Week 4: Validation
- Full regression testing
- Performance documentation
- Code review and cleanup

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Ordering changes | High | Extensive testing, property tests |
| Memory overhead | Medium | Measure and document, make optional |
| Complex edge cases | Medium | Comprehensive test suite |
| Performance regression for small programs | Low | Threshold for enabling indexing |

## Future Extensions (Not in Stage 2)

- **Multi-argument indexing** - Index on multiple arguments
- **Argument indexing** - Index on specific argument positions
- **Deep indexing** - Index nested structures
- **Dynamic indexing** - Update index for assert/retract
- **Hash consing** - Share common subterms
- **Mode declarations** - Optimize based on usage patterns

## Acceptance Checklist

- [ ] All Stage 1 tests pass
- [ ] All Stage 1.5 tests pass
- [ ] Performance benchmarks show improvement
- [ ] Solution ordering unchanged
- [ ] Backtracking behavior identical
- [ ] Memory usage documented
- [ ] Code review completed
- [ ] Documentation updated
