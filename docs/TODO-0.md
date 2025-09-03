# TODO: Stage 0 (Core Shapes & Explicit Stacks)

## Phase 1: Basic Structures

### 1. Test Helpers and Fixtures
- [ ] Write helper: mk_fact(functor, *args) for creating facts
- [ ] Write helper: mk_rule(functor, head_args, *body_terms) for rules
- [ ] Write helper: program(*clauses) for creating Programs
- [ ] Write helper: run_query(engine, *goals) for running queries
- [ ] Write helper: assert_no_recursion() to verify no Python recursion
- [ ] Verify helpers work correctly

### 2. Clause and Program Representation

#### Clause Dataclass
- [ ] Write test: Clause with head and empty body (fact)
- [ ] Write test: Clause with head and body (rule)
- [ ] Write test: Clause immutability (frozen, tuples)
- [ ] Write test: Atom heads supported as 0-arity functors
- [ ] Implement Clause dataclass (frozen=True)
- [ ] Verify tests pass

#### Program Class
- [ ] Write test: Program stores clauses in order
- [ ] Write test: clauses_for returns matching functor/arity
- [ ] Write test: clauses_for discriminates by arity (f/1 vs f/2)
- [ ] Write test: clauses_for returns empty list for no matches
- [ ] Write test: clauses_for preserves order
- [ ] Write test: Repeated calls return same order & indices
- [ ] Write test: Program clauses immutable (tuple)
- [ ] Implement Program class with clauses_for method
- [ ] Consider caching _index[(functor, arity)] = [clause_idx,...]
- [ ] Verify tests pass

#### ClauseCursor
- [ ] Write test: cursor initialization with matches list
- [ ] Write test: has_more returns true when clauses available
- [ ] Write test: has_more returns false when exhausted
- [ ] Write test: peek returns next without advancing
- [ ] Write test: peek after take reflects next element
- [ ] Write test: peek/take when empty returns None
- [ ] Write test: take returns next and advances exactly once
- [ ] Write test: clone preserves position
- [ ] Write test: clone isolation (mutations don't affect original)
- [ ] Write test: exhausts exactly matches list (no duplicates/skips)
- [ ] Implement ClauseCursor with {matches: list[int], pos: int}
- [ ] Implement has_more/peek/take/clone methods
- [ ] Verify tests pass

### 3. Goal Stack Implementation

#### Goal Dataclass
- [ ] Write test: Goal creation with term
- [ ] Write test: Goal immutability (frozen=True)
- [ ] Implement Goal dataclass (frozen=True)
- [ ] Verify tests pass

#### GoalStack Class
- [ ] Write test: push adds goal to stack
- [ ] Write test: pop returns top goal (LIFO)
- [ ] Write test: pop returns None when empty
- [ ] Write test: push_body adds goals in reverse order (left-to-right execution)
- [ ] Write test: snapshot creates immutable copy (tuple)
- [ ] Write test: snapshot unaffected by future push/pop
- [ ] Write test: restore from snapshot yields identical pop order
- [ ] Implement GoalStack class with _stack: List[Goal]
- [ ] Consider GoalStack(snapshot=None) constructor for clean restore
- [ ] Verify tests pass

### 4. Choicepoint System

#### Choicepoint Dataclass
- [ ] Write test: Choicepoint creation with all fields
- [ ] Write test: Choicepoint stores immutable goal snapshot (tuple)
- [ ] Write test: Pre-goal snapshot semantics (goal still on top)
- [ ] Implement Choicepoint dataclass (frozen=True)
- [ ] Verify tests pass

#### ChoiceStack Class
- [ ] Write test: push assigns unique sequential ID
- [ ] Write test: pop returns most recent choicepoint (LIFO)
- [ ] Write test: pop returns None when empty
- [ ] Write test: cut_to removes exactly choicepoints newer than barrier
- [ ] Write test: cut_to preserves choicepoints at/before barrier
- [ ] Write test: cut_to with None barrier is no-op
- [ ] Write test: find locates choicepoint by ID
- [ ] Write test: top_id returns current top ID or None
- [ ] Implement ChoiceStack with _stack and _next_id
- [ ] Verify tests pass

## Phase 2: Core Engine Loop

### 5. Engine State Management

#### Engine Initialization
- [ ] Write test: Engine initializes with program
- [ ] Write test: Engine tracks query variables (name→varid mapping)
- [ ] Write test: Engine sets initial_var_cutoff after query vars
- [ ] Write test: Engine initializes empty solutions list
- [ ] Write test: Re-run safety (run() twice yields independent results)
- [ ] Write test: Engine.reset() clears state for reuse
- [ ] Implement Engine.__init__ with all state fields
- [ ] Implement Engine.reset() method
- [ ] Verify tests pass

#### Solution Recording
- [ ] Write test: _record_solution captures query variables only
- [ ] Write test: _record_solution excludes renamed clause variables (vid >= cutoff)
- [ ] Write test: _record_solution handles unbound variables (decide format)
- [ ] Write test: _reify_var follows bindings to terms (no path compression)
- [ ] Write test: Solution format stable (e.g., Var(vid, hint) or _G123)
- [ ] Implement _record_solution using query_vars mapping
- [ ] Implement read-only _reify_var (no side effects)
- [ ] Verify tests pass

### 6. Variable Renaming

#### VarRenamer Class
- [ ] Write test: rename_term creates fresh variable for Var
- [ ] Write test: rename_term preserves atoms and ints unchanged
- [ ] Write test: rename_term handles nested structures recursively
- [ ] Write test: rename_term handles lists with tails (default Atom('[]'))
- [ ] Write test: rename_clause renames both head and body
- [ ] Write test: consistent mapping within single clause
- [ ] Write test: fresh renamer for each clause use (no var sharing)
- [ ] Write test: deterministic var IDs (same input → same output)
- [ ] Implement VarRenamer class with store reference
- [ ] Implement single-use mapping (fresh for each clause)
- [ ] Verify tests pass

### 7. Main Run Loop (without builtins)

#### Basic Fact Queries
- [ ] Write test: query matches single fact
- [ ] Write test: query fails with no matching facts
- [ ] Write test: query with multiple matching facts
- [ ] Write test: solutions in clause source order
- [ ] Write test: no side effects on failed head unification
- [ ] Implement basic run loop (facts only)
- [ ] Implement _try_clause helper (rename→unify→push body)
- [ ] Verify tests pass

#### Rule Expansion
- [ ] Write test: rule body goals pushed in correct order
- [ ] Write test: nested rule expansion
- [ ] Write test: recursive rules don't stack overflow (deep/1 test)
- [ ] Write test: deep(s(...s(zero)...)) at 1000 levels
- [ ] Write test: assert no Python recursion approaching limit
- [ ] Extend run loop for rules
- [ ] Verify tests pass

## Phase 3: Backtracking

### 8. Choicepoint Creation

#### Multiple Clauses
- [ ] Write test: choicepoint created when multiple clauses match
- [ ] Write test: no choicepoint when single clause matches
- [ ] Write test: choicepoint preserves pre-goal snapshot (goal on top)
- [ ] Write test: choicepoint cursor positioned at next clause
- [ ] Write test: goal retry correctness (3 clauses → 3 solutions in order)
- [ ] Implement choicepoint creation with pre-goal snapshot
- [ ] Verify tests pass

### 9. Backtracking Implementation

#### _backtrack Method
- [ ] Write test: backtrack restores store state exactly (undo_to + shrink)
- [ ] Write test: backtrack restores goal stack from snapshot
- [ ] Write test: backtrack restores cut_barrier
- [ ] Write test: backtrack re-selects same goal and tries next clause
- [ ] Write test: backtrack chains to earlier choicepoint when exhausted
- [ ] Write test: backtrack returns false when no choicepoints
- [ ] Write test: state fully restored between solution attempts
- [ ] Implement _backtrack method using _try_clause
- [ ] Verify tests pass

#### Multiple Solutions
- [ ] Write test: collect all solutions for multi-clause predicate
- [ ] Write test: solutions in source clause order (left-to-right, depth-first)
- [ ] Write test: max_solutions limits results and stops searching
- [ ] Write test: backtracking through nested rules preserves order
- [ ] Write test: state isolation (failing branch doesn't affect later success)
- [ ] Verify backtracking produces all solutions
- [ ] Verify tests pass

## Phase 4: Cut Implementation

### 10. Cut Barrier Tracking

#### Cut Barrier Installation
- [ ] Write test: cut barrier set on clause entry (after head unify)
- [ ] Write test: cut barrier = choices.top_id() or None
- [ ] Write test: cut barrier restored on backtrack from choicepoint
- [ ] Write test: nested cut barriers (inner cut doesn't affect outer)
- [ ] Write test: cut barrier preserved in choicepoint
- [ ] Implement cut barrier tracking in _try_clause
- [ ] Verify tests pass

### 11. Cut Builtin

#### builtin_cut Implementation
- [ ] Write test: cut removes exactly choicepoints newer than barrier
- [ ] Write test: cut preserves choicepoints at/before barrier
- [ ] Write test: cut with None barrier is no-op
- [ ] Write test: green cut (doesn't change semantics)
- [ ] Write test: red cut (changes semantics)  
- [ ] Write test: cut in Kth clause removes N-K alternatives
- [ ] Write test: (!, fail ; true) only prunes pre-cut alternatives
- [ ] Implement builtin_cut using choices.cut_to(_cut_barrier)
- [ ] Integrate cut into builtin system
- [ ] Verify tests pass

## Phase 5: Core Builtins

### 12. Builtin Infrastructure

#### Builtin Registration
- [ ] Write test: _is_builtin recognizes builtins by name+arity
- [ ] Write test: _execute_builtin dispatches correctly
- [ ] Write test: unknown builtin returns false
- [ ] Write test: user predicate vs builtin precedence (decide & test)
- [ ] Write test: builtins win over user predicates (or vice versa)
- [ ] Implement builtin registration system (dict lookup)
- [ ] Verify tests pass

### 13. Basic Builtins

#### true/0 and fail/0
- [ ] Write test: true always succeeds
- [ ] Write test: fail always fails
- [ ] Write test: true in conjunction continues
- [ ] Write test: fail causes backtracking
- [ ] Write test: no trail entries from true/fail (pure)
- [ ] Implement builtin_true and builtin_fail
- [ ] Verify tests pass

#### call/1
- [ ] Write test: call with atom goal (0-arity)
- [ ] Write test: call with struct goal
- [ ] Write test: call with variable bound to goal (deref)
- [ ] Write test: call with unbound variable fails
- [ ] Write test: call with non-callable (int, list) fails
- [ ] Write test: call(!) executes cut
- [ ] Write test: call pushes goal (doesn't execute immediately)
- [ ] Implement builtin_call with full dereferencing
- [ ] Verify tests pass

## Phase 6: Integration and Stress Tests

### 14. Integration Tests

#### Deep Recursion
- [ ] Write test: deep(zero) and deep(s(N)) :- deep(N)
- [ ] Write test: deep(s(...s(zero)...)) with 1000 levels
- [ ] Write test: deep(s(...s(zero)...)) with 5000 levels
- [ ] Write test: assert_no_recursion() confirms no Python recursion
- [ ] Verify no stack overflow
- [ ] Verify correct success/failure

#### Complex Backtracking
- [ ] Write test: multiple choice points
- [ ] Write test: solutions in correct order
- [ ] Write test: cut in various positions
- [ ] Verify all integration tests pass

### 15. Property Tests

#### Solution Completeness
- [ ] Write property: all valid solutions found
- [ ] Write property: no duplicate solutions
- [ ] Write property: correct left-to-right, depth-first order
- [ ] Write property: solutions follow source clause order
- [ ] Write property: deterministic with tie-break (lower varid wins)
- [ ] Run property tests with multiple seeds
- [ ] Verify properties hold

#### State Isolation
- [ ] Write property: failed goals don't leak state
- [ ] Write property: backtracking fully restores (snapshot comparison)
- [ ] Write property: renamed variables don't appear in solutions
- [ ] Write property: Store/Trail/GoalStack match snapshot after backtrack
- [ ] Verify state isolation properties

### 16. Stress Tests

#### Large Clause Databases
- [ ] Write test: 1000 clauses for single predicate
- [ ] Write test: 10000 total clauses
- [ ] Write test: deep clause hierarchies
- [ ] Add @pytest.mark.stress decorator
- [ ] Use PROLOG_STRESS_SCALE env for CI scaling
- [ ] Verify acceptable performance

#### Deep Goal Stacks
- [ ] Write test: 1000+ pending goals
- [ ] Write test: deeply nested conjunctions
- [ ] Write test: many choicepoints
- [ ] Add @pytest.mark.slow decorator
- [ ] Use time_limit() helper for timing assertions
- [ ] Add pytest-timeout to prevent hangs
- [ ] Verify memory stability

## Completion Criteria

- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] No Python recursion in implementation (verified by grep and tests)
- [ ] Property tests pass with 100+ random cases
- [ ] Stress tests complete without errors
- [ ] Deterministic behavior (consistent var IDs, union tie-break)
- [ ] Terms use frozen dataclasses with tuples
- [ ] List.tail defaults to Atom('[]')
- [ ] occurs_check=False by default in engine
- [ ] Code reviewed and cleaned up
- [ ] Trace flag available for debugging
- [ ] Documentation updated if needed
- [ ] Committed to stage-0-explicit-stacks branch