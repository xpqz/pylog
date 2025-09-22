# Catch/Throw Implementation Gap Analysis

## Issue #102 - ISO Prolog Exception Handling

This document analyzes the current partial implementation of catch/throw in PyLog and identifies specific gaps that need to be addressed for full ISO Prolog compliance.

## Current Implementation Status

### What's Already Working ✅

1. **Basic catch/throw mechanism** - Simple exception catching and handling works
2. **Nested catches** - Inner catch handlers can intercept exceptions before outer ones
3. **Variable catchers** - Unbound variables as catchers match any thrown ball
4. **Exception propagation** - Uncaught exceptions properly raise `PrologThrow` to Python
5. **CATCH choicepoints** - Basic choicepoint structure exists for catch frames
6. **Trail infrastructure** - Trail-based state restoration framework is in place

### Test Results Summary

Running `prolog/tests/unit/test_exception_handling.py`:
- **6 tests PASS** - Basic functionality works
- **4 tests XFAIL** - Critical gaps identified

## Critical Implementation Gaps ❌

### 1. State Restoration Bug
**Test**: `test_catch_restores_state` (XFAIL)

**Problem**: Variable bindings made before `throw/1` are not properly undone when catching the exception.

**Example**:
```prolog
?- catch((X = modified, throw(error)), error, var(X)).
% Expected: succeeds (X should be unbound after catch)
% Actual: fails (X remains bound to 'modified')
```

**Root Cause**:
- Current implementation only saves trail position, not the complete state context
- Doesn't properly capture the baseline stamp before executing Goal
- Trail's `_var_stamps` tracking causes "already trailed" logic to skip entries after stamp rewind
- Trial unification pollutes state even with unwinding due to stamp window issues

**Fix Required**:
- Capture complete baseline state: trail position + current stamp
- Clear or adjust `Trail._var_stamps` when switching to older stamp via `set_current_stamp()`
- Ensure stamp windows are correctly managed during restoration to avoid trailing suppression

### 2. Unification Failure Propagation
**Test**: `test_catch_unification_failure` (XFAIL)

**Problem**: When the catcher pattern doesn't unify with the thrown ball, the exception should propagate up but currently fails silently in DevEngine mode.

**Example**:
```prolog
?- catch(throw(error(type1)), error(type2), true).
% Expected: throws uncaught exception (patterns don't match)
% Actual: silently fails (in DevEngine mode)
```

**Root Cause**:
- Base `Engine` correctly re-raises `PrologThrow` when `_handle_throw()` returns `False` (engine.py:455-465)
- However, `DevEngine` intentionally converts uncaught `PrologThrow` to failure (patches.py)
- Test expects ISO behavior but gets development mode behavior

**Fix Required**:
- Clarify expected behavior: ISO compliance requires propagation
- Tests should use base `Engine` not `DevEngine` for ISO compliance testing
- Or make DevEngine behavior configurable

### 3. Streaming Compatibility Issue
**Test**: `test_catch_with_streaming` (XFAIL)

**Problem**: Catch/throw doesn't work correctly when streaming clause selection is enabled.

**Example**:
```prolog
?- catch((member(X, [1,2,3]), check(X)), error(E), handle(E)).
% With streaming enabled, fails to properly catch exceptions
```

**Root Cause**:
- `StreamingClauseCursor` maintains internal iteration state
- This state isn't saved/restored during exception handling
- Cursor position gets lost when unwinding

**Fix Required**:
- Save streaming cursor state in catch choicepoint payload
- Restore cursor state when catching an exception
- Handle interaction between streaming and backtracking properly

### 4. Cut Interaction Bug
**Test**: `test_catch_with_cut` (XFAIL)

**Problem**: Cut (`!`) within a catch scope doesn't properly respect the catch boundary.

**Example**:
```prolog
?- catch((choice(X), !, check(X)), error(_), X = caught).
% Cut should not prevent the catch from working
```

**Root Cause**:
- Catch doesn't establish a proper cut barrier
- Cut can incorrectly prune choicepoints beyond the catch scope
- Cut barrier management isn't integrated with catch frames

**Fix Required**:
- Establish cut barrier when entering catch scope (like if-then-else does)
- Ensure cuts within catch Goal don't escape the catch boundary
- Properly restore cut barriers during exception handling

### 5. Two-Phase Unification Missing

**Problem**: Current implementation does trial unification but pollutes the store even when undoing.

**Current Flawed Approach**:
```python
# Current implementation in _handle_throw()
trial_top = self.trail.position()
ok = self._unify(ball, catcher)  # Pollutes store!
self.trail.unwind_to(trial_top, self.store)  # Incomplete cleanup
```

**Issues**:
- Regular unification creates bindings and modifies union-find structure
- Trail's `_var_stamps` remembers "already trailed" status, causing subsequent operations to skip trailing
- When `set_current_stamp(cp.stamp)` moves to older window, var-stamps aren't reconciled
- Can cause bindings to leak across catch boundaries

**Fix Required - Two-Phase Approach**:

**Phase 1 - Matching Check**:
- Check if ball matches catcher WITHOUT modifying store
- Options:
  - Implement read-only unification that doesn't write to store/trail
  - Use temporary store copy with isolated trail for trial
- Return boolean result only

**Phase 2 - Binding Creation**:
- Only executed if Phase 1 succeeds
- Restore to baseline state first (with proper stamp management)
- Re-unify to create proper bindings for recovery goal

## Architectural Issues

### Stamp Management
- Current implementation switches stamps via `set_current_stamp()` but doesn't reconcile `_var_stamps`
- Trail tracks "already trailed" via `_var_stamps[(kind, varid)] >= _write_stamp` comparison
- When rewinding to older stamp, stale entries in `_var_stamps` cause trailing to be skipped
- **Location**: `Trail.set_current_stamp()` at runtime.py:284-291

### Trail Var-Stamp Discipline
- Each mutation checks if variable was "already trailed in this window" before adding trail entry
- After stamp rewind, this check gives false positives due to stale `_var_stamps` entries
- **Fix needed**: Clear `_var_stamps` when moving to older stamp, or track per-stamp generations
- **Location**: Trail trailing logic at runtime.py:236-257

### Trial Unification Side Effects
- Current trial uses real `_unify()` which modifies store even with subsequent unwind
- Trail unwind restores values but `_var_stamps` pollution affects future operations
- **Location**: Trial unification at engine.py:543-548

### Cut Barrier Management
- Catch doesn't establish a frame or barrier, allowing cuts to escape
- Cut consults current frame's `cut_barrier` and can prune CATCH choicepoint
- **Fix options**:
  - Push frame when entering catch Goal with appropriate cut_barrier
  - Make CATCH choicepoint act as barrier in `_dispatch_cut()`
- **Location**: Cut dispatch at engine.py:1087-1137

### Streaming Cursor State
- StreamingClauseCursor maintains iteration state not captured in CATCH payload
- No save/restore mechanism for cursor state during exception handling
- **Location**: Catch choicepoint creation at engine.py:586-610

## Implementation Priority

1. **Priority 1 - State Restoration** (test_catch_restores_state)
   - Most fundamental issue
   - Breaks basic exception handling semantics
   - Required for any serious use of catch/throw

2. **Priority 2 - Unification Failure** (test_catch_unification_failure)
   - Silent failures are dangerous
   - Violates ISO Prolog semantics
   - Easy to fix once identified

3. **Priority 3 - Two-Phase Unification**
   - Correctness issue for complex patterns
   - Prevents proper state isolation
   - Foundation for robust exception handling

4. **Priority 4 - Cut Barrier** (test_catch_with_cut)
   - Control flow isolation issue
   - Important for program correctness
   - Similar to existing if-then-else handling

5. **Priority 5 - Streaming Compatibility** (test_catch_with_streaming)
   - Performance optimization compatibility
   - Not critical for correctness
   - Can disable streaming as workaround

## Testing Strategy

### Existing Test Coverage
- Basic catch/throw scenarios ✅
- Nested exception handlers ✅
- Variable catchers ✅
- Uncaught exception propagation ✅
- Tracer integration ✅

### Missing Test Coverage
- State restoration with complex bindings ❌
- Catcher pattern matching edge cases ❌
- Cut and catch interaction ❌
- Streaming with exceptions ❌
- Performance under exception load ❌

### Recommended Additional Tests
1. Compound ball patterns with partial unification
2. Deeply nested catch handlers with different patterns
3. Exception handling during backtracking
4. Catch with attributed variables
5. Memory/trail growth under repeated exceptions
6. Interaction with other control structures (if-then-else, soft-cut)

## Concrete Implementation Steps

### Step 1: Fix Trail Var-Stamp Management
```python
# In Trail.set_current_stamp() at runtime.py:284
def set_current_stamp(self, stamp: int) -> None:
    """Switch to different stamp window."""
    assert stamp >= 0
    self._write_stamp = stamp
    # NEW: Clear var-stamps when moving to older window
    if stamp < self._write_stamp:
        self._var_stamps.clear()  # Reset "already trailed" tracking
```

### Step 2: Implement Read-Only Unification
```python
# New method in engine.py
def _match_only(self, term1: Term, term2: Term) -> bool:
    """Check if terms unify WITHOUT modifying store/trail."""
    # Option A: Implement visitor pattern for structural matching
    # Option B: Clone store and use isolated unification
    temp_store = self.store.clone()  # Need to implement clone()
    temp_trail = Trail()
    return unify(term1, term2, temp_store, temp_trail, self.occurs_check)
```

### Step 3: Fix Catch Trial Logic
```python
# Replace in _handle_throw() at engine.py:543-548
# OLD: ok = self._unify(ball, cp.payload.get("catcher"))
# NEW: Use two-phase approach
if self._match_only(ball, cp.payload.get("catcher")):
    # Match succeeded - restore and bind for real
    self.trail.unwind_to(cp.trail_top, self.store)
    self.trail.set_current_stamp(cp.stamp)
    ok = self._unify(ball, cp.payload.get("catcher"))
    assert ok  # Must succeed since match_only passed
    # ... continue with recovery
```

### Step 4: Add Cut Barrier for Catch
```python
# In _builtin_catch() at engine.py:2586
# Before pushing Goal, establish cut barrier
catch_frame = Frame(
    pred=("catch", 3),
    cut_barrier=len(self.cp_stack)  # Cuts stop here
)
self.frame_stack.append(catch_frame)
# Remember to pop frame in cleanup
```

### Step 5: Save/Restore Streaming State
```python
# In CATCH choicepoint payload at engine.py:2593
payload = {
    "phase": "GOAL",
    "catcher": catcher_arg,
    "recovery": recovery_arg,
    "cp_height": base_cp_height,
    # NEW: Save streaming cursor state if active
    "cursor_state": self._save_cursor_state() if self.use_streaming else None
}
```

## Next Steps

1. **Immediate**: Fix Trail var-stamp clearing in `set_current_stamp()`
2. **Short-term**: Implement match-only unification for clean trial
3. **Medium-term**: Add cut barriers and fix streaming state
4. **Long-term**: Performance optimization and stress testing

## Notes for Implementers

- The `PrologThrow` exception class is already well-designed
- Trail infrastructure is solid, just needs proper usage
- Consider following the if-then-else implementation pattern for cut barriers
- Study `_restore_to_choicepoint()` for proper state restoration pattern
- Test against SWI-Prolog for ISO compliance validation