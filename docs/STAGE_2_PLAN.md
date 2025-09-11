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
- Maps `(functor, arity)` → clause buckets
- Maintains special buckets for variables and type dispatch

#### 2. ClauseIndex
```python
class ClauseIndex:
    """Index for efficient clause lookup."""
    def __init__(self, clauses: List[Clause]):
        self.by_functor: Dict[Tuple[str, int], List[Clause]] = {}
        self.var_clauses: List[Clause] = []  # Clauses with var in first arg
        self.by_type: Dict[str, List[Clause]] = {
            'atom': [],
            'int': [],
            'list': [],
            'struct': []
        }
```

#### 3. First-Argument Analysis
- Extract principal functor from clause heads
- Handle special cases:
  - Variables → var bucket (match everything)
  - Atoms → `(name, 0)` bucket
  - Integers → int bucket
  - Lists → `('.', 2)` bucket
  - Structures → `(functor, arity)` bucket

### Integration Points

#### Engine Modifications
```python
# In Engine.__init__
self.program = IndexedProgram(clauses) if use_indexing else Program(clauses)

# In Engine.select_clauses(goal)
if isinstance(self.program, IndexedProgram):
    return self.program.get_matching_clauses(goal)
else:
    return self.program.clauses  # Fallback to linear scan
```

#### Clause Selection Algorithm
1. Dereference goal's first argument
2. Determine type/functor:
   - Unbound var → return all clauses
   - Atom → lookup `(atom_name, 0)`
   - Int → lookup int bucket
   - List → lookup `('.', 2)`
   - Struct → lookup `(functor, arity)`
3. Combine matching buckets:
   - Type-specific clauses
   - Variable clauses (always match)
4. Return in original source order

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
def test_index_building():
    clauses = parse_program("""
        foo(a).
        foo(b).
        foo(X) :- bar(X).
        baz(1).
    """)
    index = ClauseIndex(clauses)
    assert len(index.by_functor[('foo', 1)]) == 2
    assert len(index.var_clauses) == 1
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

### Unit Tests
- Index building correctness
- Clause bucket organization
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
    
    assert indexed_time < linear_time / 100  # Expect 100x+ speedup
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
- ✅ 100x+ speedup for large fact bases (best case)
- ✅ 3-5x speedup for type dispatch scenarios
- ✅ 2-3x speedup for typical recursive predicates
- ✅ No performance regression for small programs

### Code Quality
- ✅ Clean separation of concerns (indexing module)
- ✅ Backward compatible (indexing optional)
- ✅ Well-documented index structure
- ✅ Comprehensive test coverage

## Implementation Notes

### Design Decisions
1. **Copy-on-write not needed** - Clauses are immutable after parsing
2. **Source order preservation** - Critical for Prolog semantics
3. **Variable clauses always checked** - Cannot be filtered out
4. **Index built once** - At program load time

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