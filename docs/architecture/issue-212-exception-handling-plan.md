# Issue #212: Exception Handling Refinement - Architectural Plan

## Executive Summary

This document provides a comprehensive architectural plan for addressing 7 XFailed tests in PyLog's exception handling system. The plan focuses on surgical fixes within the existing tree-walking engine framework while maintaining the explicit stack design.

## 1. Architectural Analysis

### 1.1 Current Architecture Overview

PyLog's exception handling is built on:
- **Explicit stacks**: Goal stack, frame stack, choicepoint stack (no Python recursion)
- **Trail-based restoration**: All mutations are trailed for backtracking
- **CATCH choicepoints**: Special choicepoints (kind=CATCH) that act as exception handlers
- **PrologThrow exceptions**: Python exceptions that propagate until caught
- **Reification at throw time**: Variables are reified when thrown to capture bindings

### 1.2 Root Cause Analysis of Failing Tests

#### Test 1: `test_list_unification_restoration`
**Problem**: Complex list structure unifications are not fully restored after catch
**Root Cause**: Partial unification trails are incomplete when list tails are unified
**Impact**: Variable bindings persist incorrectly after exception handling

#### Test 2: `test_catch_creates_cut_barrier`
**Problem**: Cut (!) escapes the catch scope instead of being contained
**Root Cause**: CATCH choicepoints don't properly act as cut barriers
**Impact**: Cut prunes choicepoints beyond the catch scope

#### Test 3: `test_catch_at_choice_points`
**Problem**: Multiple catch alternatives at the same level don't work correctly
**Root Cause**: Catch frame management conflicts with regular choicepoint backtracking
**Impact**: Some valid catch alternatives are skipped

#### Test 4 & 7: `test_streaming_cursor_restoration` & `test_catch_with_streaming`
**Problem**: Streaming cursors lose position when exceptions are caught
**Root Cause**: No mechanism to preserve/restore streaming cursor state
**Impact**: Cannot retry predicates with streaming after catch

#### Test 5: `test_recursive_predicate_with_catch`
**Problem**: Deep recursion with catch/throw has edge cases
**Root Cause**: Frame stack restoration doesn't handle recursive predicates correctly
**Impact**: Stack corruption in deeply recursive catch scenarios

#### Test 6: `test_iso_ball_copy_semantics`
**Problem**: PyLog uses reification instead of ISO's copy semantics
**Root Cause**: Design choice - reification at throw time vs. copy semantics
**Impact**: Different behavior from ISO standard (documented design choice)

## 2. Implementation Strategy

### 2.1 Phase 1: Cut Barrier Enforcement (Test 2)

**Goal**: Ensure CATCH choicepoints act as proper cut barriers

**Changes Required**:
```python
# In engine.py, _builtin_cut method
def _builtin_cut(self):
    """Modified cut to respect CATCH barriers"""
    if not self.frame_stack:
        return True

    current_frame = self.frame_stack[-1]
    target_height = current_frame.cut_barrier

    # Find the highest CATCH CP below our target
    catch_barrier = None
    for i in range(len(self.cp_stack) - 1, target_height - 1, -1):
        if self.cp_stack[i].kind == ChoicepointKind.CATCH:
            catch_barrier = i + 1  # Don't prune the CATCH itself
            break

    # Apply the more restrictive barrier
    if catch_barrier is not None:
        target_height = max(target_height, catch_barrier)

    # Prune choicepoints above target
    while len(self.cp_stack) > target_height:
        self.cp_stack.pop()

    return True
```

**Testing**: Verify cuts don't escape catch scopes

### 2.2 Phase 2: Enhanced Trail Management (Test 1)

**Goal**: Complete trailing for complex list unifications

**Changes Required**:
```python
# In unify.py, enhance unification trailing
def unify_with_trail(term1, term2, store, trail):
    """Enhanced unification with complete trailing"""
    # Add pre-unification snapshot for complex structures
    if isinstance(term1, (PrologList, Struct)) or isinstance(term2, (PrologList, Struct)):
        # Trail intermediate unification states
        affected_vars = collect_variables(term1, term2)
        for var_id in affected_vars:
            cell = store.get_cell(var_id)
            trail.push_bind(var_id, cell)

    # Perform standard unification
    return unify(term1, term2, store)
```

**Testing**: Complex list/structure restoration after catch

### 2.3 Phase 3: Streaming Cursor State Management (Tests 4 & 7)

**Goal**: Preserve streaming cursor positions across exception handling

**Changes Required**:

1. **Add cursor snapshot to CATCH choicepoints**:
```python
# In engine.py, _builtin_catch
def _builtin_catch(self, args):
    # ... existing code ...

    # Snapshot active streaming cursors
    cursor_snapshots = {}
    for cp in self.cp_stack:
        if cp.kind == ChoicepointKind.PREDICATE:
            cursor = cp.payload.get("cursor")
            if isinstance(cursor, StreamingClauseCursor):
                # Clone the cursor for restoration
                cursor_snapshots[id(cursor)] = cursor.clone()

    catch_cp = Choicepoint(
        kind=ChoicepointKind.CATCH,
        # ... other fields ...
        payload={
            "catcher": catcher_arg,
            "recovery": recovery_arg,
            "cursor_snapshots": cursor_snapshots,  # NEW
            # ... other payload fields ...
        }
    )
```

2. **Restore cursors when catching**:
```python
# In engine.py, _handle_throw
def _handle_throw(self, ball):
    # ... find matching catch ...

    # Restore streaming cursors (conservative approach)
    cursor_snapshots = cp.payload.get("cursor_snapshots", {})
    for other_cp in self.cp_stack[:cp_height]:
        if other_cp.kind == ChoicepointKind.PREDICATE:
            cursor = other_cp.payload.get("cursor")
            if isinstance(cursor, StreamingClauseCursor):
                snapshot = cursor_snapshots.get(id(cursor))
                if snapshot:
                    # Replace with cloned cursor
                    other_cp.payload["cursor"] = snapshot.clone()
```

**Design Note**: Conservative approach - clone all streaming cursors at catch point, restore on exception. This may over-preserve state but ensures correctness.

### 2.4 Phase 4: Multiple Catch Alternatives (Test 3)

**Goal**: Support multiple catch clauses at the same predicate level

**Changes Required**:

1. **Track catch phase in choicepoint**:
```python
# Add phase tracking to CATCH choicepoints
catch_cp = Choicepoint(
    kind=ChoicepointKind.CATCH,
    payload={
        "phase": "GOAL",  # or "RECOVERY"
        "catcher": catcher_arg,
        "recovery": recovery_arg,
        # ... other fields ...
    }
)
```

2. **Resume catch alternatives correctly**:
```python
# In backtrack logic
elif cp.kind == ChoicepointKind.CATCH:
    phase = cp.payload.get("phase")
    if phase == "RECOVERY":
        # Recovery failed, continue backtracking
        continue
    else:
        # GOAL phase - never resumed for alternatives
        # This CP only activates on throw
        continue
```

### 2.5 Phase 5: Recursive Predicate Edge Cases (Test 5)

**Goal**: Correct frame stack management for recursive predicates

**Changes Required**:

1. **Track recursion depth in catch frames**:
```python
# Add recursion tracking to catch setup
def _builtin_catch(self, args):
    # Count frames for the same predicate
    recursion_depth = 0
    if self.frame_stack:
        current_pred = self.frame_stack[-1].pred
        for frame in self.frame_stack:
            if frame.pred == current_pred:
                recursion_depth += 1

    catch_cp = Choicepoint(
        payload={
            "recursion_depth": recursion_depth,
            # ... other fields ...
        }
    )
```

2. **Restore frames considering recursion**:
```python
# In _handle_throw, validate frame restoration
def _handle_throw(self, ball):
    # ... restore to catch baselines ...

    # Validate frame stack consistency
    expected_depth = cp.payload.get("recursion_depth", 0)
    actual_depth = sum(1 for f in self.frame_stack
                      if f.pred == self.frame_stack[-1].pred)

    if actual_depth != expected_depth:
        # Repair frame stack if needed
        self._repair_frame_stack(expected_depth)
```

### 2.6 Phase 6: ISO Ball Copy Semantics (Test 6)

**Goal**: Document design choice, provide ISO-compatible mode if needed

**Resolution**: This is a documented design choice. PyLog uses reification at throw time for efficiency.

**Optional Enhancement**:
```python
# Add ISO mode flag
if self.mode == "iso":
    # Use copy_term semantics
    ball = copy_term(ball, self.store)
else:
    # Use reification (current behavior)
    ball = self._reify_term(ball)
```

## 3. Phased Implementation Approach

### Phase A: Foundation (Tests 2, 3)
- **Duration**: 2 days
- **Focus**: Cut barrier enforcement, catch alternatives
- **Deliverable**: Tests 2 and 3 passing

### Phase B: State Management (Tests 1, 4, 7)
- **Duration**: 3 days
- **Focus**: Trail completeness, streaming cursor preservation
- **Deliverable**: Tests 1, 4, and 7 passing

### Phase C: Edge Cases (Tests 5, 6)
- **Duration**: 2 days
- **Focus**: Recursive predicates, ISO compatibility documentation
- **Deliverable**: Test 5 passing, Test 6 documented/optional

### Phase D: Integration & Testing
- **Duration**: 1 day
- **Focus**: Full test suite validation, performance verification
- **Deliverable**: All tests passing, no regressions

## 4. Risk Assessment

### 4.1 Performance Impact
- **Cursor cloning**: O(1) space per cursor, minimal time overhead
- **Enhanced trailing**: Slight overhead for complex structures
- **Risk Level**: Low - changes are localized to exception paths

### 4.2 Compatibility Impact
- **Existing code**: No breaking changes to public APIs
- **Backtracking**: Enhanced trail management improves correctness
- **Risk Level**: Low - fixes are additive, not destructive

### 4.3 Complexity Impact
- **Code complexity**: Moderate increase in catch/throw logic
- **Maintenance**: Well-documented changes with clear boundaries
- **Risk Level**: Medium - requires careful testing

## 5. Testing Strategy

### 5.1 Unit Tests
- Existing XFailed tests will be enabled
- Add regression tests for each fixed scenario
- Property-based tests for trail consistency

### 5.2 Integration Tests
- Full Prolog program test suite must pass
- Stress tests with deep recursion and many choicepoints
- Streaming cursor tests with various exception patterns

### 5.3 Performance Tests
- Measure overhead of cursor cloning
- Benchmark exception handling paths
- Verify no degradation in normal execution

## 6. Success Criteria

1. All 7 XFailed tests pass consistently
2. No regression in existing test suite
3. Performance overhead < 5% for exception paths
4. Clear documentation of design choices
5. Code coverage > 95% for modified components

## 7. Alternative Approaches Considered

### 7.1 Full State Snapshot
- **Pros**: Simple, complete restoration
- **Cons**: High memory overhead, performance impact
- **Decision**: Rejected - too expensive for normal execution

### 7.2 Lazy Cursor Restoration
- **Pros**: No upfront cost, restore only on demand
- **Cons**: Complex bookkeeping, potential for inconsistency
- **Decision**: Rejected - correctness concerns

### 7.3 Separate Exception Stack
- **Pros**: Clean separation of concerns
- **Cons**: Major architectural change, compatibility break
- **Decision**: Rejected - too invasive for current stage

## 8. Conclusion

This plan provides a systematic approach to fixing PyLog's exception handling issues while maintaining the integrity of the existing architecture. The phased implementation allows for incremental progress with validation at each stage. The conservative approach to streaming cursor management ensures correctness while the enhanced trail management provides complete state restoration.

The fixes are surgical, focused on the specific failing scenarios without disrupting the core engine design. This maintains PyLog's goals of being a pedagogical tool with clear, debuggable execution while achieving full Prolog compatibility for exception handling.