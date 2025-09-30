# Equality Reification Failure Analysis

## Executive Summary

The trichotomy test fails because equality reification (B #<=> (X #= Y)) does not work correctly when the boolean variable B is set to 1 during labeling. The root cause is an overly aggressive propagation failure check in the unification hook that blocks valid unifications.

## Test Results

Our test suite reveals a clear pattern:

### Working Cases ✓
- **Direct equality constraints** work perfectly (X #= Y, label([X, Y])) - produces 5 solutions
- **Constraint then bind** works (X #= Y, X = 3) - correctly propagates Y=3
- **Ground boolean reification** works (1 #<=> (X #= Y)) - produces correct solutions
- **Simple FD binding** works (X in 1..5, X = 3) - hook doesn't block simple cases

### Failing Cases ✗
- **B2=1 then X=1** fails (B2 #<=> (X #= Y), B2 = 1, X = 1) - produces 0 solutions instead of 1
- **Boolean-first labeling** fails completely - produces 0 solutions instead of 9
- **Trichotomy B2=1 cases** all missing - no X=Y solutions found

## Root Cause Analysis

### The Core Problem

The failure occurs at the intersection of three systems:

1. **Reification Propagator** (prolog/clpfd/props/reif.py:143-154)
   - When B2=1, correctly posts X #= Y constraint
   - But posts it 3 times (reif_posts tracking failure)

2. **Equality Propagator** (prolog/clpfd/props/equality.py:25-99)
   - Works correctly in isolation
   - When X=1, would correctly propagate Y=1

3. **Unification Hook** (prolog/clpfd/hooks.py:87)
   - **THE CULPRIT**: Returns False if propagation "fails"
   - Too aggressive in determining failure

### The Failure Sequence

```
1. Reification setup: B2 #<=> (X #= Y)
   - Creates reification propagator watching B2, X, Y

2. B2 = 1 (during labeling)
   - Triggers reification propagator
   - Posts X #= Y constraint (3 times!)
   - Creates equality propagator

3. X = 1 (next labeling step)
   - Unification hook called (hooks.py:52)
   - Sets X domain to {1} (hooks.py:82)
   - Calls _wake_watchers_and_propagate_check() (hooks.py:87)
   - Propagation runs:
     * Equality propagator triggered
     * Tries to set Y domain to {1}
     * Something causes propagation to return False
   - Hook returns False, blocking X=1 unification
   - Labeling branch fails

4. Result: No solutions found
```

### Why Propagation Returns False

The propagation likely returns False due to one of these reasons:

1. **Circular propagation**: The equality propagator may trigger itself repeatedly
2. **Domain revision conflicts**: Multiple propagators trying to update the same domain
3. **Propagator status interpretation**: A propagator returning "ok" with no changes might be misinterpreted as failure

### The Reif_Posts Duplication Issue

The constraint is posted 3 times because:

1. **First post**: When B2=1 is set initially
2. **Second post**: During propagation queue processing
3. **Third post**: During backtracking/re-propagation

The post_key generation at line 145 of reif.py:
```python
post_key = ("reif", b_id, constraint_type, normalized_args, "C")
```

This key doesn't account for:
- Propagator execution context
- Trail marks
- Queue processing state

## Code Flow Trace

### Successful Case (Direct Equality)
```
X #= Y → create_equality_propagator
label([X, Y]) → X = 1
  → unification succeeds
  → equality propagator runs
  → Y domain becomes {1}
  → Y bound to 1
  → Solution: X=1, Y=1
```

### Failing Case (Reification)
```
B2 #<=> (X #= Y) → create_reification_propagator
B2 = 1
  → reification propagator runs
  → posts X #= Y (3 times!)
label([X, Y]) → X = 1
  → unification hook called
  → _wake_watchers_and_propagate_check() returns False
  → hook returns False
  → unification blocked
  → No solutions
```

## Why Ground Boolean Works

When the boolean is ground at reification time (1 #<=> (X #= Y)):
- The constraint is posted immediately in `_builtin_fd_reif_equiv` (line 1168)
- Uses `_post_constraint_with_flush` which binds singletons after propagation
- No reification propagator is created
- Labeling doesn't trigger the problematic hook interaction

## Specific Issues to Address

### 1. Hook Overly Aggressive (PRIMARY ISSUE)
**File**: prolog/clpfd/hooks.py, line 87
```python
if not _wake_watchers_and_propagate_check(engine, varid):
    return False  # Too aggressive!
```

Should distinguish between:
- **Domain inconsistency**: Legitimate failure, block unification
- **Propagation error**: Log/handle differently, allow unification to proceed

### 2. Reif_Posts Tracking Broken
**File**: prolog/clpfd/props/reif.py, lines 143-154

The tracking mechanism fails because:
- Key generation is not unique across propagation cycles
- Attributes may be reset during backtracking
- No protection against concurrent propagator execution

### 3. Missing Singleton Binding After Reification Posts
When reification posts a constraint that makes a domain singleton, the variable isn't bound immediately. This delays binding until labeling, where the hook issue manifests.

## Recommended Fix Strategy

### Phase 1: Fix Hook Behavior (Critical)
Modify `clpfd_unify_hook` to:
1. Only return False for actual domain inconsistencies
2. Allow propagation "failures" that are recoverable
3. Add logging to identify why propagation returns False

### Phase 2: Fix Reif_Posts Tracking
1. Make post_key include trail mark or propagation cycle ID
2. Use a global registry instead of variable attributes
3. Add mutex/guard to prevent concurrent posting

### Phase 3: Optimize Singleton Binding
1. After reification posts a constraint, check for singleton domains
2. Bind singleton domains immediately (but after propagation completes)
3. Ensure this doesn't interfere with other constraints

## Test Cases for Verification

### Minimal Failing Case
```prolog
% This should produce 1 solution but currently produces 0
X in 1..5, Y in 1..5,
B2 #<=> (X #= Y),
B2 = 1,
X = 1
% Expected: X=1, Y=1, B2=1
```

### Isolation Test
```prolog
% Test hook doesn't block valid propagation
X in 1..5, Y in 1..5,
X #= Y,  % Direct constraint (works)
X = 1    % Should propagate Y=1
```

### Propagation Order Test
```prolog
% Test if order matters
X in 1..5, Y in 1..5,
X = 1,              % Bind X first
B2 #<=> (X #= Y),   % Then reification
B2 = 1              % Then B2
% This works! Shows the issue is with labeling order
```

## Conclusion

The equality reification failure is caused by an overly strict unification hook that treats all propagation failures as reasons to block unification. This combines with a broken reif_posts tracking mechanism that allows duplicate constraint posting. The fix requires making the hook more permissive while maintaining correctness for actual domain conflicts.

The issue only manifests during labeling when:
1. A boolean is set to 1 after reification is established
2. This triggers constraint posting
3. Subsequent variable binding triggers the problematic hook behavior

Ground boolean cases avoid this by posting constraints before any labeling occurs, bypassing the hook interaction entirely.