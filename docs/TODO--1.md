# TODO: Stage -1 (Unifier Workbench)

## Phase 1: Core Data Structures

### 1. Store Implementation

#### Cell Dataclass
- [x] Write test for Cell creation with unbound state
- [x] Write test for Cell creation with bound state  
- [x] Write test for Cell rank field default
- [x] Implement Cell dataclass
- [x] Verify tests pass

#### Store.new_var()
- [x] Write test: new_var returns sequential IDs starting from 0
- [x] Write test: new_var creates unbound cell with self-reference
- [x] Write test: new_var sets rank to 0
- [x] Write test: hint parameter is optional and not stored
- [x] Implement Store.__init__ 
- [x] Implement Store.new_var()
- [x] Verify tests pass

#### Store.deref() - Basic
- [x] Write test: deref of unbound root returns ("UNBOUND", varid)
- [x] Write test: deref of bound var returns ("BOUND", varid, term)
- [x] Write test: deref follows single parent link
- [x] Write test: deref follows chain of parent links
- [x] Write test: deref without compress has no side effects
- [x] Implement Store.deref() without compression
- [x] Verify tests pass

#### Store.deref() - Path Compression
- [x] Write test: compression only when compress=True AND trail provided
- [x] Write test: no compression for paths < 4 nodes
- [x] Write test: compression for paths >= 4 nodes updates parents
- [x] Write test: compression adds trail entries for each compressed link
- [x] Implement path compression in Store.deref()
- [x] Verify tests pass

#### Store.deref() - Additional Tests
- [x] Write test: order-agnostic trail assertions
- [x] Write test: undoability of compression
- [x] Write test: no-op compression (re-compress already compressed)
- [x] Write test: compression to bound root
- [x] Write test: invalid varid handling
- [x] Write test: rank invariants during compression

### 2. Term Representation

#### Basic Terms
- [x] Write test for Atom creation and immutability
- [x] Write test for Int creation and immutability
- [x] Write test for Var creation with id and optional hint
- [x] Implement Atom, Int, Var classes
- [x] Verify tests pass

#### Compound Terms
- [x] Write test for Struct creation and immutability
- [x] Write test for List creation with default empty tail
- [x] Write test for List creation with custom tail
- [x] Implement Struct, List classes
- [x] Verify tests pass

### 3. Trail Implementation

#### Basic Trail Operations
- [x] Write test: push adds entry to trail
- [x] Write test: mark returns current position
- [x] Write test: clear empties trail
- [x] Implement Trail class with push, mark, clear
- [x] Verify tests pass

#### Trail Undo
- [x] Write test: undo_to with 'parent' entry restores ref
- [x] Write test: undo_to with 'bind' entry restores cell
- [x] Write test: undo_to with 'rank' entry restores rank
- [x] Write test: undo_to removes entries back to mark
- [x] Implement undo_to function
- [x] Verify tests pass

#### Trail Context Manager
- [x] Write test: trail_guard yields mark
- [x] Write test: trail_guard calls undo_to on exception
- [x] Write test: trail_guard doesn't undo on success
- [x] Implement trail_guard context manager
- [x] Verify tests pass

### 4. Unification Helpers

#### union_vars()
- [x] Write test: union_vars with equal roots returns True
- [x] Write test: union by rank (smaller joins larger)
- [x] Write test: equal ranks increment winner's rank
- [x] Write test: trail entries created for parent and rank changes
- [x] Implement union_vars()
- [x] Verify tests pass

#### bind_root_to_term()
- [x] Write test: bind unbound root to atom
- [x] Write test: bind unbound root to struct
- [x] Write test: trail entry created with old cell copy
- [x] Write test: binding already bound root fails
- [x] Implement bind_root_to_term()
- [x] Verify tests pass

#### deref_term()
- [x] Write test: non-var returns ('NONVAR', term)
- [x] Write test: unbound var returns ('VAR', root_vid)
- [x] Write test: bound var returns ('NONVAR', dereferenced_term)
- [x] Write test: no side effects (no compression)
- [x] Implement deref_term()
- [x] Verify tests pass

### 5. Basic Unification (without occurs check)

#### Atomic Unification
- [x] Write test: unify equal atoms succeeds
- [x] Write test: unify different atoms fails
- [x] Write test: unify equal ints succeeds
- [x] Write test: unify different ints fails
- [x] Implement atom/int cases in unify()
- [x] Verify tests pass

#### Variable Unification
- [x] Write test: unify var with atom binds var
- [x] Write test: unify var with struct binds var
- [x] Write test: unify two vars creates union
- [x] Write test: unify var with itself succeeds (no-op)
- [x] Implement variable cases in unify()
- [x] Verify tests pass

#### Structural Unification
- [x] Write test: structs with different functors fail
- [x] Write test: structs with different arities fail
- [x] Write test: structs with same shape unify args
- [x] Write test: nested structs unify recursively
- [x] Write test: lists unify items and tails
- [x] Implement structural cases in unify()
- [x] Verify tests pass

### 6. Occurs Check

#### occurs() Function
- [x] Write test: var doesn't occur in atom
- [x] Write test: var doesn't occur in different var
- [x] Write test: var occurs in itself
- [x] Write test: var occurs in struct containing it
- [x] Write test: var occurs in deeply nested structure
- [x] Write test: handles cyclic structures safely
- [x] Implement occurs() function
- [x] Verify tests pass

#### Unification with Occurs Check
- [x] Write test: X = f(X) fails when occurs_check=True
- [x] Write test: X = f(X) succeeds when occurs_check=False
- [x] Write test: X = Y, Y = f(X) fails when occurs_check=True
- [x] Integrate occurs check into unify()
- [x] Verify tests pass

### 7. Property Tests

#### Symmetry
- [x] Write property test: unify(A, B) == unify(B, A)
- [x] Generate random terms of various types
- [x] Run 1000+ cases
- [x] Verify property holds

#### Idempotence  
- [x] Write property test: second unify produces no trail entries
- [x] Test on successful unifications
- [x] Verify property holds

#### Trail Invertibility
- [x] Write property test: undo_to(mark) restores exact state
- [x] Test with random unification sequences
- [x] Verify property holds

### 8. Stress Tests

#### Large Structures
- [x] Write test: 10000-element list unification
- [x] Write test: 1000-level nested structures
- [x] Verify no stack overflow (iterative)

#### Many Variables
- [x] Write test: create 100000 variables
- [x] Write test: long union chains
- [x] Verify performance acceptable

## Completion Criteria

- [x] All unit tests pass
- [x] All property tests pass with 1000+ cases
- [x] No Python recursion in implementation
- [x] Stress tests complete without errors
- [x] Code committed with tests