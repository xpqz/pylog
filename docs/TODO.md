# TODO: Stage -1 (Unifier Workbench)

## Phase 1: Core Data Structures

### 1. Store Implementation

#### Cell Dataclass
- [ ] Write test for Cell creation with unbound state
- [ ] Write test for Cell creation with bound state  
- [ ] Write test for Cell rank field default
- [ ] Implement Cell dataclass
- [ ] Verify tests pass

#### Store.new_var()
- [ ] Write test: new_var returns sequential IDs starting from 0
- [ ] Write test: new_var creates unbound cell with self-reference
- [ ] Write test: new_var sets rank to 0
- [ ] Write test: hint parameter is optional and not stored
- [ ] Implement Store.__init__ 
- [ ] Implement Store.new_var()
- [ ] Verify tests pass

#### Store.deref() - Basic
- [ ] Write test: deref of unbound root returns ("UNBOUND", varid)
- [ ] Write test: deref of bound var returns ("BOUND", varid, term)
- [ ] Write test: deref follows single parent link
- [ ] Write test: deref follows chain of parent links
- [ ] Write test: deref without compress has no side effects
- [ ] Implement Store.deref() without compression
- [ ] Verify tests pass

#### Store.deref() - Path Compression
- [ ] Write test: compression only when compress=True AND trail provided
- [ ] Write test: no compression for paths < 4 nodes
- [ ] Write test: compression for paths >= 4 nodes updates parents
- [ ] Write test: compression adds trail entries for each compressed link
- [ ] Implement path compression in Store.deref()
- [ ] Verify tests pass

### 2. Term Representation

#### Basic Terms
- [ ] Write test for Atom creation and immutability
- [ ] Write test for Int creation and immutability
- [ ] Write test for Var creation with id and optional hint
- [ ] Implement Atom, Int, Var classes
- [ ] Verify tests pass

#### Compound Terms
- [ ] Write test for Struct creation and immutability
- [ ] Write test for List creation with default empty tail
- [ ] Write test for List creation with custom tail
- [ ] Implement Struct, List classes
- [ ] Verify tests pass

### 3. Trail Implementation

#### Basic Trail Operations
- [ ] Write test: push adds entry to trail
- [ ] Write test: mark returns current position
- [ ] Write test: clear empties trail
- [ ] Implement Trail class with push, mark, clear
- [ ] Verify tests pass

#### Trail Undo
- [ ] Write test: undo_to with 'parent' entry restores ref
- [ ] Write test: undo_to with 'bind' entry restores cell
- [ ] Write test: undo_to with 'rank' entry restores rank
- [ ] Write test: undo_to removes entries back to mark
- [ ] Implement undo_to function
- [ ] Verify tests pass

#### Trail Context Manager
- [ ] Write test: trail_guard yields mark
- [ ] Write test: trail_guard calls undo_to on exception
- [ ] Write test: trail_guard doesn't undo on success
- [ ] Implement trail_guard context manager
- [ ] Verify tests pass

### 4. Unification Helpers

#### union_vars()
- [ ] Write test: union_vars with equal roots returns True
- [ ] Write test: union by rank (smaller joins larger)
- [ ] Write test: equal ranks increment winner's rank
- [ ] Write test: trail entries created for parent and rank changes
- [ ] Implement union_vars()
- [ ] Verify tests pass

#### bind_root_to_term()
- [ ] Write test: bind unbound root to atom
- [ ] Write test: bind unbound root to struct
- [ ] Write test: trail entry created with old cell copy
- [ ] Write test: binding already bound root fails
- [ ] Implement bind_root_to_term()
- [ ] Verify tests pass

#### deref_term()
- [ ] Write test: non-var returns ('NONVAR', term)
- [ ] Write test: unbound var returns ('VAR', root_vid)
- [ ] Write test: bound var returns ('NONVAR', dereferenced_term)
- [ ] Write test: no side effects (no compression)
- [ ] Implement deref_term()
- [ ] Verify tests pass

### 5. Basic Unification (without occurs check)

#### Atomic Unification
- [ ] Write test: unify equal atoms succeeds
- [ ] Write test: unify different atoms fails
- [ ] Write test: unify equal ints succeeds
- [ ] Write test: unify different ints fails
- [ ] Implement atom/int cases in unify()
- [ ] Verify tests pass

#### Variable Unification
- [ ] Write test: unify var with atom binds var
- [ ] Write test: unify var with struct binds var
- [ ] Write test: unify two vars creates union
- [ ] Write test: unify var with itself succeeds (no-op)
- [ ] Implement variable cases in unify()
- [ ] Verify tests pass

#### Structural Unification
- [ ] Write test: structs with different functors fail
- [ ] Write test: structs with different arities fail
- [ ] Write test: structs with same shape unify args
- [ ] Write test: nested structs unify recursively
- [ ] Write test: lists unify items and tails
- [ ] Implement structural cases in unify()
- [ ] Verify tests pass

### 6. Occurs Check

#### occurs() Function
- [ ] Write test: var doesn't occur in atom
- [ ] Write test: var doesn't occur in different var
- [ ] Write test: var occurs in itself
- [ ] Write test: var occurs in struct containing it
- [ ] Write test: var occurs in deeply nested structure
- [ ] Write test: handles cyclic structures safely
- [ ] Implement occurs() function
- [ ] Verify tests pass

#### Unification with Occurs Check
- [ ] Write test: X = f(X) fails when occurs_check=True
- [ ] Write test: X = f(X) succeeds when occurs_check=False
- [ ] Write test: X = Y, Y = f(X) fails when occurs_check=True
- [ ] Integrate occurs check into unify()
- [ ] Verify tests pass

### 7. Property Tests

#### Symmetry
- [ ] Write property test: unify(A, B) == unify(B, A)
- [ ] Generate random terms of various types
- [ ] Run 1000+ cases
- [ ] Verify property holds

#### Idempotence  
- [ ] Write property test: second unify produces no trail entries
- [ ] Test on successful unifications
- [ ] Verify property holds

#### Trail Invertibility
- [ ] Write property test: undo_to(mark) restores exact state
- [ ] Test with random unification sequences
- [ ] Verify property holds

### 8. Stress Tests

#### Large Structures
- [ ] Write test: 10000-element list unification
- [ ] Write test: 1000-level nested structures
- [ ] Verify no stack overflow (iterative)

#### Many Variables
- [ ] Write test: create 100000 variables
- [ ] Write test: long union chains
- [ ] Verify performance acceptable

## Completion Criteria

- [ ] All unit tests pass
- [ ] All property tests pass with 1000+ cases
- [ ] No Python recursion in implementation
- [ ] Stress tests complete without errors
- [ ] Code committed with tests