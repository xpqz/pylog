# TODO: Stage 0 (Core Shapes & Explicit Stacks)

## Phase 1: Basic Structures

### 1. Clause and Program Representation

#### Clause Dataclass
- [ ] Write test: Clause with head and empty body (fact)
- [ ] Write test: Clause with head and body (rule)
- [ ] Write test: Clause immutability
- [ ] Implement Clause dataclass
- [ ] Verify tests pass

#### Program Class
- [ ] Write test: Program stores clauses in order
- [ ] Write test: clauses_for returns matching functor/arity
- [ ] Write test: clauses_for returns empty list for no matches
- [ ] Write test: clauses_for preserves order
- [ ] Implement Program class with clauses_for method
- [ ] Verify tests pass

#### ClauseCursor
- [ ] Write test: cursor initialization with matches
- [ ] Write test: has_more returns true when clauses available
- [ ] Write test: peek returns next without advancing
- [ ] Write test: take returns next and advances
- [ ] Write test: clone preserves position
- [ ] Implement ClauseCursor with has_more/peek/take/clone
- [ ] Verify tests pass

### 2. Goal Stack Implementation

#### Goal Dataclass
- [ ] Write test: Goal creation with term
- [ ] Write test: Goal immutability (frozen)
- [ ] Implement Goal dataclass
- [ ] Verify tests pass

#### GoalStack Class
- [ ] Write test: push adds goal to stack
- [ ] Write test: pop returns top goal
- [ ] Write test: pop returns None when empty
- [ ] Write test: push_body adds goals in reverse order
- [ ] Write test: snapshot creates immutable copy
- [ ] Write test: restore from snapshot
- [ ] Implement GoalStack class
- [ ] Verify tests pass

### 3. Choicepoint System

#### Choicepoint Dataclass
- [ ] Write test: Choicepoint creation with all fields
- [ ] Write test: Choicepoint stores immutable goal snapshot
- [ ] Implement Choicepoint dataclass
- [ ] Verify tests pass

#### ChoiceStack Class
- [ ] Write test: push assigns unique ID
- [ ] Write test: pop returns most recent choicepoint
- [ ] Write test: pop returns None when empty
- [ ] Write test: cut_to removes newer choicepoints
- [ ] Write test: cut_to preserves older choicepoints
- [ ] Write test: find locates choicepoint by ID
- [ ] Write test: top_id returns current top ID or None
- [ ] Implement ChoiceStack class
- [ ] Verify tests pass

## Phase 2: Core Engine Loop

### 4. Engine State Management

#### Engine Initialization
- [ ] Write test: Engine initializes with program
- [ ] Write test: Engine tracks query variables
- [ ] Write test: Engine sets initial_var_cutoff
- [ ] Write test: Engine initializes empty solutions list
- [ ] Implement Engine.__init__
- [ ] Verify tests pass

#### Solution Recording
- [ ] Write test: _record_solution captures query variables only
- [ ] Write test: _record_solution excludes renamed clause variables
- [ ] Write test: _record_solution handles unbound variables
- [ ] Write test: _reify_var follows bindings to terms
- [ ] Implement _record_solution and _reify_var
- [ ] Verify tests pass

### 5. Variable Renaming

#### VarRenamer Class
- [ ] Write test: rename_term creates fresh variable for Var
- [ ] Write test: rename_term preserves atoms and ints
- [ ] Write test: rename_term handles nested structures
- [ ] Write test: rename_term handles lists with tails
- [ ] Write test: rename_clause renames head and body
- [ ] Write test: consistent mapping within clause
- [ ] Write test: fresh renamer for each clause use
- [ ] Implement VarRenamer class
- [ ] Verify tests pass

### 6. Main Run Loop (without builtins)

#### Basic Fact Queries
- [ ] Write test: query matches single fact
- [ ] Write test: query fails with no matching facts
- [ ] Write test: query with multiple matching facts
- [ ] Implement basic run loop (facts only)
- [ ] Verify tests pass

#### Rule Expansion
- [ ] Write test: rule body goals pushed in correct order
- [ ] Write test: nested rule expansion
- [ ] Write test: recursive rules don't stack overflow
- [ ] Extend run loop for rules
- [ ] Verify tests pass

## Phase 3: Backtracking

### 7. Choicepoint Creation

#### Multiple Clauses
- [ ] Write test: choicepoint created when multiple clauses match
- [ ] Write test: no choicepoint when single clause matches
- [ ] Write test: choicepoint preserves goal snapshot
- [ ] Write test: choicepoint cursor positioned correctly
- [ ] Implement choicepoint creation in run loop
- [ ] Verify tests pass

### 8. Backtracking Implementation

#### _backtrack Method
- [ ] Write test: backtrack restores store state
- [ ] Write test: backtrack restores goal stack
- [ ] Write test: backtrack tries next clause
- [ ] Write test: backtrack chains to earlier choicepoint
- [ ] Write test: backtrack returns false when no choicepoints
- [ ] Implement _backtrack method
- [ ] Verify tests pass

#### Multiple Solutions
- [ ] Write test: collect all solutions for multi-clause predicate
- [ ] Write test: solutions in source clause order
- [ ] Write test: max_solutions limits results
- [ ] Write test: backtracking through nested rules
- [ ] Verify backtracking produces all solutions
- [ ] Verify tests pass

## Phase 4: Cut Implementation

### 9. Cut Barrier Tracking

#### Cut Barrier Installation
- [ ] Write test: cut barrier set on clause entry
- [ ] Write test: cut barrier restored on backtrack
- [ ] Write test: nested cut barriers
- [ ] Implement cut barrier tracking
- [ ] Verify tests pass

### 10. Cut Builtin

#### builtin_cut Implementation
- [ ] Write test: cut removes choicepoints up to barrier
- [ ] Write test: cut preserves older choicepoints
- [ ] Write test: cut with no barrier (no-op)
- [ ] Write test: green cut (doesn't change semantics)
- [ ] Write test: red cut (changes semantics)
- [ ] Implement builtin_cut
- [ ] Integrate cut into builtin system
- [ ] Verify tests pass

## Phase 5: Core Builtins

### 11. Builtin Infrastructure

#### Builtin Registration
- [ ] Write test: _is_builtin recognizes builtins
- [ ] Write test: _execute_builtin dispatches correctly
- [ ] Write test: unknown builtin returns false
- [ ] Implement builtin registration system
- [ ] Verify tests pass

### 12. Basic Builtins

#### true/0 and fail/0
- [ ] Write test: true always succeeds
- [ ] Write test: fail always fails
- [ ] Write test: true in conjunction
- [ ] Write test: fail causes backtracking
- [ ] Implement builtin_true and builtin_fail
- [ ] Verify tests pass

#### call/1
- [ ] Write test: call with atom goal
- [ ] Write test: call with struct goal
- [ ] Write test: call with variable bound to goal
- [ ] Write test: call with unbound variable fails
- [ ] Write test: call with non-callable fails
- [ ] Implement builtin_call with dereferencing
- [ ] Verify tests pass

## Phase 6: Integration and Stress Tests

### 13. Integration Tests

#### Deep Recursion
- [ ] Write test: deep(s(...s(zero)...)) with 1000 levels
- [ ] Write test: deep(s(...s(zero)...)) with 5000 levels
- [ ] Verify no stack overflow
- [ ] Verify correct success/failure

#### Complex Backtracking
- [ ] Write test: multiple choice points
- [ ] Write test: solutions in correct order
- [ ] Write test: cut in various positions
- [ ] Verify all integration tests pass

### 14. Property Tests

#### Solution Completeness
- [ ] Write property: all valid solutions found
- [ ] Write property: no duplicate solutions
- [ ] Write property: correct left-to-right, depth-first order
- [ ] Run property tests with multiple seeds
- [ ] Verify properties hold

#### State Isolation
- [ ] Write property: failed goals don't leak state
- [ ] Write property: backtracking fully restores
- [ ] Write property: renamed variables don't appear in solutions
- [ ] Verify state isolation properties

### 15. Stress Tests

#### Large Clause Databases
- [ ] Write test: 1000 clauses for single predicate
- [ ] Write test: 10000 total clauses
- [ ] Write test: deep clause hierarchies
- [ ] Verify acceptable performance

#### Deep Goal Stacks
- [ ] Write test: 1000+ pending goals
- [ ] Write test: deeply nested conjunctions
- [ ] Write test: many choicepoints
- [ ] Verify memory stability

## Completion Criteria

- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] No Python recursion in implementation
- [ ] Property tests pass with 100+ random cases
- [ ] Stress tests complete without errors
- [ ] Code reviewed and cleaned up
- [ ] Documentation updated if needed
- [ ] Committed to stage-0-explicit-stacks branch