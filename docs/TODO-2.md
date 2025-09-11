# TODO: Stage 2 (Indexing for Performance)

## Phase 1: Basic Infrastructure

### 1. Create Indexing Module
- [ ] Create prolog/engine/indexing.py
- [ ] Add module docstring explaining first-argument indexing
- [ ] Import necessary dependencies (Terms, deref)
- [ ] Set up module structure

### 2. Implement Core Data Structures
- [ ] Write test: ClauseIndex initializes empty
- [ ] Write test: PredIndex stores clauses per predicate
- [ ] Write test: Clause IDs maintain source order
- [ ] Implement PredIndex class with buckets (var_ids, empty_list_ids, int_ids, list_nonempty_ids, struct_functor)
- [ ] Implement ClauseIndex class with preds dict and clauses dict
- [ ] Verify data structure initialization

### 3. Index Building from Clauses
- [ ] Write test: Index building preserves source order
- [ ] Write test: Clauses indexed per predicate, not globally
- [ ] Write test: Each predicate has separate PredIndex
- [ ] Write test: Clause IDs are unique per predicate
- [ ] Implement add_clause method
- [ ] Implement build_from_clauses function
- [ ] Verify index building correctness

### 4. First-Argument Analysis
- [ ] Write test: Extract principal functor from Atom
- [ ] Write test: Extract principal functor from Struct
- [ ] Write test: Detect variable first argument
- [ ] Write test: Detect empty list [] as special atom
- [ ] Write test: Detect non-empty list [H|T] as '.'/2 structure
- [ ] Write test: Detect integer first argument (including negatives)
- [ ] Write test: Clause IDs are monotonic per predicate in source order
- [ ] Write test: build_from_clauses is deterministic given same input
- [ ] Implement analyze_first_arg function
- [ ] Map clause heads to appropriate buckets
- [ ] Verify bucket assignment

### 5. Static Program Assumption
- [ ] Write test: Index assumes static program (no dynamic predicates)
- [ ] Write test: Error/assertion if clauses added after index built
- [ ] Document Stage 2 static program scope
- [ ] Implement guard against post-build modifications
- [ ] Add policy for rebuild vs assertion

## Phase 2: First-Argument Indexing

### 1. Bucket Organization
- [ ] Write test: Variable heads go to var_ids bucket
- [ ] Write test: Atom heads go to struct_functor[(name, 0)]
- [ ] Write test: Atom('[]') goes to empty_list_ids, NOT struct_functor
- [ ] Write test: Empty list [] goes to empty_list_ids
- [ ] Write test: Non-empty list [H|T] goes to list_nonempty_ids
- [ ] Write test: Integer heads go to int_ids (positive and negative)
- [ ] Write test: p(-3) and p(3) both use int_ids bucket
- [ ] Write test: Struct heads go to struct_functor[(functor, arity)]
- [ ] Write test: Float bucket placeholder (mark xfail for future)
- [ ] Add __slots__ to PredIndex for memory efficiency
- [ ] Implement bucket assignment logic
- [ ] Verify all clause types categorized correctly

### 2. Clause Selection Algorithm
- [ ] Write test: Select clauses for atom goal
- [ ] Write test: Select clauses for struct goal
- [ ] Write test: Select clauses for variable goal (all clauses)
- [ ] Write test: Select clauses for integer goal
- [ ] Write test: Select clauses for empty list goal
- [ ] Write test: Select clauses for non-empty list goal
- [ ] Implement select method in ClauseIndex
- [ ] Build candidate set based on dereferenced first arg
- [ ] Verify selection correctness

### 3. Order Preservation (Critical)
- [ ] Write test: Order preservation with interleaved var heads
- [ ] Write test: Source order maintained within candidates
- [ ] Write test: Never concatenate buckets (filter through order)
- [ ] Write test: Variable clauses always included when matching
- [ ] Implement order ∩ candidates pattern
- [ ] Verify identical solution ordering

### 4. Dereferencing Before Selection
- [ ] Write test: Selection uses dereferenced first argument
- [ ] Write test: Bound variables select correct bucket
- [ ] Write test: Unbound variables select all clauses
- [ ] Write test: Deref of attributed variable treated as variable
- [ ] Write test: Attributed vars select all clauses (not struct bucket)
- [ ] Integrate deref into selection logic
- [ ] Verify runtime binding respected

## Phase 2.5: Predicate Isolation & Small-Predicate Heuristic

### 1. Predicate Isolation Guarantees
- [ ] Write test: ClauseIndex never mixes predicates in single bucket
- [ ] Write test: PredIndex objects only reachable via (name, arity) keys
- [ ] Write property test: Two predicates with identical first-arg shapes don't share buckets
- [ ] Write test: Selection for p/1 never returns q/1 clauses
- [ ] Write test: Predicate key includes arity in both build and select
- [ ] Implement strict predicate isolation
- [ ] Verify no cross-contamination possible

### 2. Small Predicate Optimization
- [ ] Write test: Predicates with <= 3 clauses can bypass indexing
- [ ] Write test: Bypass threshold is configurable
- [ ] Write test: No performance regression for tiny predicates
- [ ] Write test: Semantics identical with bypass active
- [ ] Implement bypass heuristic
- [ ] Document rationale and threshold

## Phase 3: Type Switching

### 1. Type Detection
- [ ] Write test: Detect atom type (non-empty-list)
- [ ] Write test: Detect empty list as special atom
- [ ] Write test: Detect non-empty list type
- [ ] Write test: Detect integer type
- [ ] Write test: Detect struct type
- [ ] Write test: Detect variable type
- [ ] Implement type detection functions
- [ ] Verify type discrimination

### 2. List Type Separation
- [ ] Write test: [] and [H|T] require separate buckets
- [ ] Write test: [] never matches [H|T] clauses
- [ ] Write test: [H|T] never matches [] clauses
- [ ] Write test: Variable clauses match both list types
- [ ] Verify list type separation

### 3. Struct Functor Discrimination
- [ ] Write test: Different functors use different buckets
- [ ] Write test: Same functor different arity separate buckets
- [ ] Write test: f/1 doesn't match g/1
- [ ] Write test: f/1 doesn't match f/2
- [ ] Verify functor/arity discrimination

### 4. Predicate Isolation
- [ ] Write test: Different predicates never share buckets
- [ ] Write test: p/1 clauses never returned for q/1 query
- [ ] Write test: Predicate key includes arity
- [ ] Verify complete predicate isolation

## Phase 4: Engine Integration

### 1. IndexedProgram Wrapper
- [ ] Write test: IndexedProgram wraps ClauseIndex
- [ ] Write test: Provides select method interface
- [ ] Write test: Falls back gracefully if not available
- [ ] Implement IndexedProgram class
- [ ] Add use_indexing parameter support
- [ ] Verify wrapper functionality

### 2. Engine Modifications
- [ ] Write test: Engine accepts use_indexing parameter
- [ ] Write test: Engine creates IndexedProgram when enabled
- [ ] Write test: Engine creates Program when disabled
- [ ] Write test: get_matching_clauses uses indexing when available
- [ ] Modify Engine.__init__ for indexing support
- [ ] Modify get_matching_clauses to use program.select
- [ ] Verify integration works

### 3. Backward Compatibility
- [ ] Write test: Engine works without indexing (default)
- [ ] Write test: All Stage 1 tests pass with indexing disabled
- [ ] Write test: All Stage 1.5 tests pass with indexing disabled
- [ ] Write test: use_indexing=False behaves identically to Stage 1.5
- [ ] Ensure no regressions without indexing
- [ ] Verify complete backward compatibility

### 4. Semantic Preservation
- [ ] Write test: Solutions identical with/without indexing
- [ ] Write test: Solution ordering unchanged
- [ ] Write test: Backtracking behavior identical
- [ ] Write test: Cut semantics preserved
- [ ] Write property test: Random programs produce same results
- [ ] Verify no semantic changes

## Phase 4.5: Streaming & Debug Instrumentation

### 1. Streaming Selection
- [ ] Write test: select() returns iterator/generator, not list
- [ ] Write test: Streaming behavior with cut - only first candidate visited
- [ ] Write test: Memory profile stays constant with large candidate sets
- [ ] Implement select() as generator
- [ ] Verify lazy evaluation

### 2. Debug Instrumentation
- [ ] Write test: Debug counter tracks candidates considered
- [ ] Write test: Counter resets between calls
- [ ] Write test: Counter only increments for visited candidates
- [ ] Implement debug counters (behind flag)
- [ ] Add trace output: "pred p/1: considered K of N clauses"
- [ ] Document instrumentation usage

## Phase 5: Correctness Testing

### 1. Critical Edge Cases
- [ ] Write test: Programs with all variable heads
- [ ] Write test: Single clause predicates
- [ ] Write test: Predicates with no clauses
- [ ] Write test: Deeply nested first arguments
- [ ] Write test: Mixed indexable/non-indexable predicates
- [ ] Verify edge case handling

### 2. Order Preservation Tests
- [ ] Write test: Variable heads don't disrupt order
- [ ] Write test: Interleaved types maintain order
- [ ] Write test: Complex ordering scenarios
- [ ] Write test: Backtracking order unchanged
- [ ] Verify strict order preservation

### 3. Type Switching Tests
- [ ] Write test: Integer vs atom discrimination
- [ ] Write test: List vs struct discrimination
- [ ] Write test: Empty vs non-empty list discrimination
- [ ] Write test: All type combinations
- [ ] Verify type switching correctness

### 4. Integration Tests
- [ ] Run all Stage 1 tests with indexing enabled
- [ ] Run all Stage 1.5 tests with indexing enabled
- [ ] Test complex programs (append, member, reverse)
- [ ] Test recursive predicates
- [ ] Verify no test regressions

## Phase 5.5: Cut & Backtracking Regression Guard

### 1. Cut Interaction Tests
- [ ] Write test: Interleaved var/ground heads + cut preserves pruning
- [ ] Write test: Cut in first matching clause behaves identically
- [ ] Write test: Clause with cut followed by non-matching clauses
- [ ] Write test: Cut doesn't affect indexing correctness
- [ ] Verify cut semantics preservation

### 2. Backtracking Edge Cases
- [ ] Write test: Choicepoint creation identical with/without indexing
- [ ] Write test: Trail operations identical
- [ ] Write test: Failure-driven loops work correctly
- [ ] Verify backtracking invariants

## Phase 6: Performance Validation

### 1. Benchmark Suite Creation
- [ ] Create benchmark module in tests/benchmarks/
- [ ] Write large fact base benchmark (1000+ facts)
- [ ] Write type dispatch benchmark
- [ ] Write recursive predicate benchmark
- [ ] Write mixed workload benchmark
- [ ] Set up timing infrastructure

### 2. Performance Measurements
- [ ] Write test: 30x-300x speedup for large fact bases
- [ ] Write test: 3-5x speedup for type dispatch
- [ ] Write test: 2-3x speedup for recursive predicates
- [ ] Write test: No regression (±20%) for small predicates
- [ ] Implement performance comparison
- [ ] Document speedup ratios

### 3. Memory Overhead Analysis
- [ ] Write test: Compare sys.getsizeof with/without indexing
- [ ] Write test: Memory overhead on 10k clause synthetic program
- [ ] Measure index memory usage
- [ ] Compare with flat clause list
- [ ] Document memory overhead
- [ ] Verify acceptable memory usage
- [ ] Add memory usage to documentation

### 4. Performance Documentation
- [ ] Document benchmark results
- [ ] Create performance comparison table
- [ ] Add graphs showing speedup
- [ ] Document when indexing helps most
- [ ] Update README with performance notes

## Phase 6.5: CI & Benchmark Hygiene

### 1. Test Markers
- [ ] Mark all performance tests with @pytest.mark.perf
- [ ] Mark all benchmarks with @pytest.mark.benchmark
- [ ] Configure pytest to exclude perf tests by default
- [ ] Add separate CI job for performance tests
- [ ] Document how to run performance tests

### 2. Benchmark Stability
- [ ] Implement warm-up runs before timing
- [ ] Use median-of-3 or median-of-5 timing
- [ ] Add variance/stability checks
- [ ] Handle CI timing variability
- [ ] Document benchmark methodology

## Phase 7: Property Testing

### 1. Semantic Equivalence Properties
- [ ] Write property: Solutions identical with/without indexing
- [ ] Write property: Solution ordering preserved
- [ ] Write property: Backtracking points identical
- [ ] Write property: Variable bindings identical
- [ ] Implement property test generators
- [ ] Run extensive property tests

### 2. Index Correctness Properties
- [ ] Write property: All matching clauses selected
- [ ] Write property: No non-matching clauses selected
- [ ] Write property: Order preserved within selection
- [ ] Write property: Dereferencing handled correctly
- [ ] Verify index correctness properties

### 3. Performance Properties
- [ ] Write property: Indexing never slower than 2x overhead
- [ ] Write property: Large predicates show speedup
- [ ] Write property: Memory overhead bounded
- [ ] Verify performance properties

## Phase 8: Documentation and Cleanup

### 1. Code Documentation
- [ ] Document ClauseIndex class and methods
- [ ] Document PredIndex structure
- [ ] Document IndexedProgram interface
- [ ] Document engine integration points
- [ ] Add inline comments for complex logic

### 2. User Documentation
- [ ] Update README with Stage 2 features
- [ ] Document how to enable indexing
- [ ] Document performance benefits
- [ ] Add indexing examples
- [ ] Create indexing guide

### 3. Test Documentation
- [ ] Document test strategy
- [ ] Document critical test cases
- [ ] Document performance benchmarks
- [ ] Add test coverage report

### 4. Code Cleanup
- [ ] Remove debug prints
- [ ] Optimize hot paths
- [ ] Refactor for clarity
- [ ] Run black formatter
- [ ] Final code review

## Phase 9: Final Validation

### 1. Full Test Suite
- [ ] Run all unit tests with indexing
- [ ] Run all integration tests with indexing
- [ ] Run all property tests
- [ ] Run all benchmarks
- [ ] Verify 100% pass rate

### 2. Acceptance Criteria
- [ ] ✓ All Stage 1 and 1.5 tests pass unchanged
- [ ] ✓ Solution order identical to linear scan
- [ ] ✓ Backtracking behavior preserved
- [ ] ✓ No semantic changes
- [ ] ✓ 30x-300x speedup for large fact bases
- [ ] ✓ 3-5x speedup for type dispatch
- [ ] ✓ 2-3x speedup for recursive predicates
- [ ] ✓ No regression for small predicates
- [ ] ✓ Clean separation of concerns
- [ ] ✓ Backward compatible
- [ ] ✓ Well-documented
- [ ] ✓ Comprehensive test coverage

### 3. Performance Report
- [ ] Generate final benchmark results
- [ ] Create performance comparison charts
- [ ] Document speedup achieved
- [ ] Identify future optimization opportunities
- [ ] Publish performance report

### 4. Stage 2 Completion
- [ ] Create PR for Stage 2
- [ ] Ensure CI passes
- [ ] Code review completed
- [ ] Merge to main
- [ ] Update project status
- [ ] Stage 2 complete

## Notes
- Order ∩ Candidates is the core correctness pattern - NEVER concatenate buckets
- Always dereference before selection - bound values determine bucket
- Per-predicate indexing prevents cross-contamination - enforced by design
- Variable clauses must always be candidates - cannot be filtered out
- Empty list is Atom('[]'), not a list structure - special bucket required
- Non-empty list [H|T] is '.'/2 structure - separate from empty list
- Source order must be preserved exactly - filter through order, not concatenate
- Stage 2 assumes static program - no assert/retract support
- Integers include negative values - p(-3) and p(3) both use int_ids
- Attributed variables treated as unbound variables for selection
- select() must return iterator/generator for streaming semantics
- Small predicates (≤3 clauses) may bypass indexing for efficiency
- Performance gain is the primary goal - 30x-300x for large fact bases
- Indexing is optional (use_indexing parameter) - backward compatible
- Debug instrumentation helps verify performance claims
- Cut semantics must be preserved exactly
- Memory overhead should be bounded and documented