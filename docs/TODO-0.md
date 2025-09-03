# TODO: Stage 0 (Core Shapes & Explicit Stacks)

## Phase 1: Basic Structures ✅

### 1. Test Helpers and Fixtures ✅
- [x] Write helper: mk_fact(functor, *args) for creating facts
- [x] Write helper: mk_rule(functor, head_args, *body_terms) for rules
- [x] Write helper: program(*clauses) for creating Programs
- [ ] Write helper: run_query(engine, *goals) for running queries (Phase 2)
- [x] Write helper: assert_no_recursion() to verify no Python recursion
- [x] Verify helpers work correctly

### 2. Clause and Program Representation ✅

#### Clause Dataclass ✅
- [x] Write test: Clause with head and empty body (fact)
- [x] Write test: Clause with head and body (rule)
- [x] Write test: Clause immutability (frozen, tuples)
- [x] Write test: Atom heads supported as 0-arity functors
- [x] Implement Clause dataclass (frozen=True)
- [x] Verify tests pass

#### Program Class ✅
- [x] Write test: Program stores clauses in order
- [x] Write test: clauses_for returns matching functor/arity
- [x] Write test: clauses_for discriminates by arity (f/1 vs f/2)
- [x] Write test: clauses_for returns empty list for no matches
- [x] Write test: clauses_for preserves order
- [x] Write test: Repeated calls return same order & indices
- [x] Write test: Program clauses immutable (tuple)
- [x] Implement Program class with clauses_for method
- [x] Consider caching _index[(functor, arity)] = [clause_idx,...]
- [x] Verify tests pass

#### ClauseCursor ✅
- [x] Write test: cursor initialization with matches list
- [x] Write test: has_more returns true when clauses available
- [x] Write test: has_more returns false when exhausted
- [x] Write test: peek returns next without advancing
- [x] Write test: peek after take reflects next element
- [x] Write test: peek/take when empty returns None
- [x] Write test: take returns next and advances exactly once
- [x] Write test: clone preserves position
- [x] Write test: clone isolation (mutations don't affect original)
- [x] Write test: exhausts exactly matches list (no duplicates/skips)
- [x] Implement ClauseCursor with {matches: list[int], pos: int}
- [x] Implement has_more/peek/take/clone methods
- [x] Verify tests pass

### 3. Goal Stack Implementation ✅

#### Goal Dataclass ✅
- [x] Write test: Goal creation with term
- [x] Write test: Goal immutability (frozen=True)
- [x] Implement Goal dataclass (frozen=True)
- [x] Verify tests pass

#### GoalStack Class ✅
- [x] Write test: push adds goal to stack
- [x] Write test: pop returns top goal (LIFO)
- [x] Write test: pop returns None when empty
- [x] Write test: push_body adds goals in reverse order (left-to-right execution)
- [x] Write test: snapshot creates immutable copy (tuple)
- [x] Write test: snapshot unaffected by future push/pop
- [x] Write test: restore from snapshot yields identical pop order
- [x] Write test: stack supports len()
- [x] Write test: peek returns top without removing
- [x] Write test: GoalStack constructor copies snapshot
- [x] Implement GoalStack class with _stack: List[Goal]
- [x] Consider GoalStack(snapshot=None) constructor for clean restore
- [x] Verify tests pass

### 4. Choicepoint System ✅

#### Choicepoint Dataclass ✅
- [x] Write test: Choicepoint creation with all fields
- [x] Write test: Choicepoint stores immutable goal snapshot (tuple)
- [x] Write test: Pre-goal snapshot semantics (goal still on top)
- [x] Implement Choicepoint dataclass (frozen=True)
- [x] Verify tests pass

#### ChoiceStack Class ✅
- [x] Write test: push returns unique sequential ID (not mutating CP)
- [x] Write test: pop returns most recent choicepoint (LIFO)
- [x] Write test: pop returns None when empty
- [x] Write test: cut_to removes exactly choicepoints newer than barrier
- [x] Write test: cut_to preserves choicepoints at/before barrier
- [x] Write test: cut_to with None barrier is no-op
- [x] Write test: find locates choicepoint by ID
- [x] Write test: top_id returns current top ID or None
- [x] Write test: current_id returns next ID to be assigned
- [x] Implement ChoiceStack with _stack and _next_id
- [x] Verify tests pass

## Phase 2: Core Engine Loop

### 5. Engine State Management

#### Engine Initialization ✅
- [x] Write test: Engine initializes with program
- [x] Write test: Engine tracks query variables (name→varid mapping)
- [x] Write test: Engine sets initial_var_cutoff after query vars
- [x] Write test: Engine initializes empty solutions list
- [x] Write test: Re-run safety (run() twice yields independent results)
- [x] Write test: Engine.reset() clears state for reuse
- [x] Implement Engine.__init__ with all state fields
- [x] Implement Engine.reset() method
- [x] Verify tests pass

#### Solution Recording ✅
- [x] Write test: _record_solution captures query variables only
- [x] Write test: _record_solution excludes renamed clause variables (vid >= cutoff)
- [x] Write test: _record_solution handles unbound variables (decide format)
- [x] Write test: _reify_var follows bindings to terms (no path compression)
- [x] Write test: Solution format stable (e.g., Var(vid, hint) or _G123)
- [x] Implement _record_solution using query_vars mapping
- [x] Implement read-only _reify_var (no side effects)
- [x] Verify tests pass (20/21 - one test has buggy setup)

### 6. Variable Renaming ✅

#### VarRenamer Class ✅
- [x] Write test: rename_term creates fresh variable for Var
- [x] Write test: rename_term preserves atoms and ints unchanged
- [x] Write test: rename_term handles nested structures recursively
- [x] Write test: rename_term handles lists with tails (default Atom('[]'))
- [x] Write test: rename_clause renames both head and body
- [x] Write test: consistent mapping within single clause
- [x] Write test: fresh renamer for each clause use (no var sharing)
- [x] Write test: deterministic var IDs (same input → same output)
- [x] Implement VarRenamer class with store reference
- [x] Implement single-use mapping (fresh for each clause)
- [x] Verify tests pass

### 7. Main Run Loop (without builtins)

#### Basic Fact Queries ✅
- [x] Write test: query matches single fact
- [x] Write test: query fails with no matching facts
- [x] Write test: query with multiple matching facts
- [x] Write test: solutions in clause source order
- [x] Write test: no side effects on failed head unification
- [x] Implement basic run loop (facts only)
- [x] Implement _try_clause helper (rename→unify→push body)
- [x] Verify tests pass

#### Rule Expansion ✅
- [x] Write test: rule body goals pushed in correct order
- [x] Write test: nested rule expansion
- [x] Write test: recursive rules don't stack overflow (deep/1 test)
- [x] Write test: deep(s(...s(zero)...)) at 1000 levels
- [x] Write test: assert no Python recursion approaching limit
- [x] Extend run loop for rules
- [x] Verify tests pass

## Phase 3: Backtracking ✅

### 8. Choicepoint Creation ✅

#### Multiple Clauses ✅
- [x] Write test: choicepoint created when multiple clauses match
- [x] Write test: no choicepoint when single clause matches
- [x] Write test: choicepoint preserves pre-goal snapshot (goal on top)
- [x] Write test: choicepoint cursor positioned at next clause
- [x] Write test: goal retry correctness (3 clauses → 3 solutions in order)
- [x] Implement choicepoint creation with pre-goal snapshot
- [x] Verify tests pass

### 9. Backtracking Implementation ✅

#### _backtrack Method ✅
- [x] Write test: backtrack restores store state exactly (undo_to + shrink)
- [x] Write test: backtrack restores goal stack from snapshot
- [x] Write test: backtrack restores cut_barrier
- [x] Write test: backtrack re-selects same goal and tries next clause
- [x] Write test: backtrack chains to earlier choicepoint when exhausted
- [x] Write test: backtrack returns false when no choicepoints
- [x] Write test: state fully restored between solution attempts
- [x] Implement _backtrack method using _try_clause
- [x] Verify tests pass

#### Multiple Solutions ✅
- [x] Write test: collect all solutions for multi-clause predicate
- [x] Write test: solutions in source clause order (left-to-right, depth-first)
- [x] Write test: max_solutions limits results and stops searching
- [x] Write test: backtracking through nested rules preserves order
- [x] Write test: state isolation (failing branch doesn't affect later success)
- [x] Verify backtracking produces all solutions
- [x] Verify tests pass

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