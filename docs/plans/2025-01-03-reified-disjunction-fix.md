# Implementation Plan: Reified Disjunction Fix

**Date**: January 3, 2025
**Status**: Implementation Plan
**Target**: Fix 2 remaining XFailed tests by implementing reified disjunction support

## Executive Summary

PyLog's CLP(FD) reification system currently lacks support for disjunction constraints (#\/) in reification contexts (#==> and #<=>). This causes 2 XFailed tests in `test_clpfd_reification_scenarios.py` to fail with 0 solutions instead of expected results. The root cause is that the reification builtin only supports basic arithmetic constraints and doesn't handle disjunction constraint types.

## Problem Analysis

### Failing Tests
1. **test_optional_task_scheduling** - Complex scheduling with optional tasks
2. **test_path_with_optional_edges** - Path finding with conditional constraints

### Root Cause
The reification builtin `_builtin_fd_reif_implies` (line 1607 in `builtins_clpfd.py`) fails when encountering disjunction:

```python
# Current constraint type switch (lines 1496-1551) only supports:
if constraint.functor == "#=":
    # arithmetic equality
elif constraint.functor == "#<":
    # arithmetic comparison
elif constraint.functor == "#>":
    # arithmetic comparison
# ... other arithmetic constraints
else:
    return False  # FAILS HERE for #\/ constraints
```

### Missing Components
1. **Constraint type recognition**: #\/ not in reification constraint switch
2. **Entailment checking**: No `check_disjunction_entailment()` function
3. **Negation support**: No disjunction negation for #<=>

## Implementation Strategy

### Phase 1: Add Disjunction Support to Reification Builtin

**Target**: Extend `_builtin_fd_reif_implies` to handle #\/ constraints

**Changes Required**:

1. **Add disjunction case to constraint switch**:
```python
# In _builtin_fd_reif_implies, lines 1496-1551
elif constraint.functor == "#\\/":
    # Handle disjunction constraint
    return self._handle_reified_disjunction(bool_var, constraint, store, trail)
```

2. **Implement disjunction handler**:
```python
def _handle_reified_disjunction(self, bool_var, constraint, store, trail):
    """Handle reified disjunction: B #==> (C1 #\\/ C2)"""
    if len(constraint.args) != 2:
        return False

    left_constraint, right_constraint = constraint.args

    # Check entailment of disjunction
    entailment = self._check_disjunction_entailment(left_constraint, right_constraint)

    if entailment == "ENTAILED":
        # Disjunction is true, so bool_var must be 1
        return self._unify_with_int(bool_var, 1, store, trail)
    elif entailment == "DISENTAILED":
        # Disjunction is false, so bool_var must be 0
        return self._unify_with_int(bool_var, 0, store, trail)
    else:
        # Unknown - create implication propagator
        return self._create_disjunction_implication(bool_var, constraint)
```

### Phase 2: Implement Disjunction Entailment Checking

**Target**: Create entailment logic for disjunction constraints

**Implementation**:
```python
def _check_disjunction_entailment(self, left_constraint, right_constraint):
    """Check if disjunction (C1 #\\/ C2) is entailed/disentailed/unknown"""

    # Check individual constraint entailments
    left_entailment = self._check_constraint_entailment(left_constraint)
    right_entailment = self._check_constraint_entailment(right_constraint)

    # Disjunction logic:
    # C1 ∨ C2 is ENTAILED if either C1 or C2 is ENTAILED
    # C1 ∨ C2 is DISENTAILED if both C1 and C2 are DISENTAILED
    # Otherwise UNKNOWN

    if left_entailment == "ENTAILED" or right_entailment == "ENTAILED":
        return "ENTAILED"
    elif left_entailment == "DISENTAILED" and right_entailment == "DISENTAILED":
        return "DISENTAILED"
    else:
        return "UNKNOWN"
```

### Phase 3: Implement Disjunction Implication Propagators

**Target**: Create propagators for reified disjunction when entailment is unknown

**Design**:
```python
def _create_disjunction_implication(self, bool_var, disjunction):
    """Create implication propagator for B #==> (C1 #\\/ C2)"""

    left_constraint, right_constraint = disjunction.args

    # Create propagator that:
    # 1. If B = 1, then post (C1 #\\/ C2)
    # 2. If (C1 #\\/ C2) entailed, then B = 1
    # 3. If (C1 #\\/ C2) disentailed, then B = 0

    propagator = DisjunctionImplicationPropagator(
        bool_var=bool_var,
        left_constraint=left_constraint,
        right_constraint=right_constraint,
        engine=self
    )

    return self._post_propagator(propagator)
```

### Phase 4: Handle Disjunction Negation (for #<=>)

**Target**: Support bidirectional reification with disjunction

**Implementation using De Morgan's law**:
```python
def _handle_reified_disjunction_bidir(self, bool_var, constraint, store, trail):
    """Handle B #<=> (C1 #\\/ C2)"""

    # Forward: B = 1 ==> (C1 #\\/ C2)
    forward_success = self._handle_reified_disjunction(bool_var, constraint, store, trail)

    if not forward_success:
        return False

    # Backward: B = 0 ==> ¬(C1 #\\/ C2)
    # Using De Morgan's law: ¬(C1 ∨ C2) ≡ (¬C1 ∧ ¬C2)
    left_constraint, right_constraint = constraint.args
    negated_conjunction = self._create_negated_conjunction(left_constraint, right_constraint)

    # Create reverse implication: (¬B) #==> (¬C1 #/\\ ¬C2)
    return self._create_reverse_disjunction_propagator(bool_var, negated_conjunction)
```

## Implementation Details

### File Locations
- **Primary**: `prolog/engine/builtins_clpfd.py:1496-1551` (constraint switch)
- **New methods**: Add to same file after existing reification helpers
- **Tests**: Existing XFailed tests in `prolog/tests/scenarios/test_clpfd_reification_scenarios.py`

### Integration Points
1. **Constraint entailment**: Leverage existing `_check_constraint_entailment()`
2. **Propagator framework**: Use existing propagator posting mechanism
3. **Unification**: Use existing `_unify_with_int()` helper

### Error Handling
- Invalid constraint structures (wrong arity)
- Unsupported nested disjunctions
- Constraint type validation

## Testing Strategy

### Phase 1: Unit Tests
Create targeted unit tests for disjunction entailment:
```python
def test_disjunction_entailment_basic():
    """Test basic disjunction entailment checking"""
    # X in 1..5, Y in 6..10
    # (X #< 3) #\/ (Y #> 8) should be UNKNOWN
    # (X #< 10) #\/ (Y #> 0) should be ENTAILED
    # (X #> 10) #\/ (Y #< 0) should be DISENTAILED
```

### Phase 2: Integration Tests
```python
def test_reified_disjunction_simple():
    """Test B #==> ((X #< 3) #\/ (Y #> 8))"""
    # Should produce solutions where B=0 or disjunction holds
```

### Phase 3: Scenario Tests
Enable the 2 XFailed tests:
- Remove `@pytest.mark.xfail` annotations
- Verify expected solution counts match
- Add regression tests for edge cases

## Risk Assessment

### Low Risk
- **Existing code stability**: Changes are additive to reification system
- **Constraint framework**: Leverages proven propagator architecture
- **Test coverage**: Comprehensive existing tests for reification

### Medium Risk
- **Complex constraint interactions**: Nested reification with disjunction
- **Performance impact**: Additional propagators may affect solving time
- **Entailment correctness**: Logic for disjunction entailment must be precise

### Mitigation Strategies
- **Incremental implementation**: Add basic support first, then optimize
- **Comprehensive testing**: Unit tests for each component
- **Performance monitoring**: Benchmark before/after changes

## Success Criteria

1. **⚠️ 2 XFailed tests pass**: `test_optional_task_scheduling` and `test_path_with_optional_edges` - PARTIALLY IMPLEMENTED
2. **✅ No regressions**: Existing CLP(FD) and reification tests continue passing
3. **⚠️ Correct semantics**: Reified disjunction behaves consistently with SWI-Prolog - BASIC CASES ONLY
4. **✅ Performance**: No significant degradation in constraint solving speed

## IMPLEMENTATION STATUS: PARTIALLY COMPLETE

### ✅ COMPLETED COMPONENTS:
- **Disjunction entailment checking**: Full implementation with comprehensive test coverage
- **Reification constraint switch integration**: Disjunction case added to both unidirectional and bidirectional reification
- **Handler methods**: Basic reified disjunction handlers for entailed/disentailed cases
- **No regressions**: All existing tests pass (3880 passed, 0 new failures)

### ⚠️ PARTIALLY IMPLEMENTED:
- **Disjunction implication propagators**: Basic structure exists but incomplete for unknown entailment cases
- **Bidirectional reification**: Framework in place but simplified implementation

### ❌ NOT YET IMPLEMENTED:
- **Complex disjunction propagation**: Full propagator infrastructure for unknown entailment cases
- **Negation handling**: Complete De Morgan's law implementation for bidirectional reification
- **XFailed test scenarios**: Complex scheduling and graph problems still fail due to incomplete propagation

## Implementation Timeline

### Week 1: Core Infrastructure
- Day 1-2: Add disjunction case to constraint switch
- Day 3-4: Implement basic entailment checking
- Day 5: Unit tests for entailment logic

### Week 2: Propagator Integration
- Day 1-3: Implement disjunction implication propagators
- Day 4: Add negation support for bidirectional reification
- Day 5: Integration testing

### Week 3: Testing & Polish
- Day 1-2: Enable XFailed tests and fix any issues
- Day 3-4: Performance testing and optimization
- Day 5: Documentation and cleanup

## Alternative Approaches Considered

### 1. Expand Disjunction (#\/) Builtin
**Rejected**: Would require major changes to disjunction implementation and doesn't address reification integration.

### 2. Transform to CNF Before Reification
**Rejected**: Complex transformations could affect performance and semantics.

### 3. Separate Disjunction Reification Builtin
**Rejected**: Would duplicate much of the existing reification infrastructure.

## Conclusion

This plan provides a systematic approach to adding reified disjunction support to PyLog's CLP(FD) system. The implementation leverages existing reification infrastructure while adding the minimal necessary components for disjunction handling. The incremental approach allows for validation at each step and maintains system stability.

The 2 remaining XFailed tests represent sophisticated constraint scenarios that will significantly enhance PyLog's capability for real-world constraint modeling applications.