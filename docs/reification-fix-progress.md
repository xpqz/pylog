# Reification Fix Progress Report

## Executive Summary

Significant progress has been made on fixing the equality reification failure in the trichotomy test. The root cause has been identified and partially fixed, but some issues remain.

## Root Cause Identified

The primary failure was caused by the equality propagator calling `bind_root_to_term` during propagation, which led to "Variable already bound" errors when labeling attempted to bind the same variable. This created a conflict between the propagator's binding and the labeling process.

### The Failure Chain
1. B2 #<=> (X #= Y) sets up reification
2. B2 = 1 during labeling triggers the reification propagator
3. Reification posts X #= Y constraint
4. Label tries X = 1
5. Hook wakes equality propagator during unification
6. Equality propagator sees X domain is {1}, tries to bind X to 1
7. But X is already being bound by the unification → "Variable already bound" error
8. Unification fails, no solutions found

## Fixes Applied

### 1. Safe Binding Implementation
Created `prolog/clpfd/safe_bind.py` with `safe_bind_singleton()` function that:
- Checks if variable is already bound before attempting to bind
- If bound to same value, returns success (idempotent)
- If bound to different value, returns failure (constraint violation)
- Only binds if variable is truly unbound

### 2. Updated Equality Propagator
Modified `prolog/clpfd/props/equality.py` to use safe binding:
- Replaced direct `bind_root_to_term` calls with `safe_bind_singleton`
- Prevents "already bound" exceptions during propagation
- Maintains correctness while allowing concurrent binding attempts

### 3. Post-Propagation Singleton Binding
Added singleton binding in `_post_constraint_with_flush` for ground boolean cases:
- After propagation completes, checks for singleton domains
- Binds variables with singleton domains
- Ensures variables get bound even without explicit labeling

## Test Results

### Tests Now Passing ✓
- **Direct equality constraints**: X #= Y with labeling works correctly
- **Ground boolean reification**: 1 #<=> (X #= Y) produces correct solutions
- **Constraint then bind**: X #= Y, X = 3 now correctly binds Y = 3
- **Simple FD binding**: Basic X = value unification works

### Tests Still Failing ✗
- **Boolean-first labeling**: B2 #<=> (X #= Y), label([B2, X, Y]) produces 0 solutions
- **Trichotomy test**: All B2=1 (equality) cases missing
- **Minimal reproduction**: B2 #<=> (X #= Y), B2 = 1, X = 1 fails

## Remaining Issues

### 1. Triple Posting Problem
The equality constraint is posted 3 times when B2=1:
```
[FD_EQ #1] Posted: X=Var(id=0), Y=Var(id=1)
[FD_EQ #2] Posted: X=Var(id=0), Y=Var(id=1)
[FD_EQ #3] Posted: X=Var(id=0), Y=Var(id=1)
```

This indicates the `reif_posts` tracking mechanism is broken:
- Post key generation may not be unique
- Attributes might be reset during backtracking
- No protection against concurrent posting

### 2. Missing Watchers
The reification propagator is not registering watchers:
```python
Var 0 attributes:
  Active watchers: []
Var 1 attributes:
  Active watchers: []
Var 2 attributes:
  Active watchers: []
```

Without watchers, the propagator is never triggered when B2 changes during labeling.

### 3. Propagator Not Running
The reification propagator runs when initially set up but never again during labeling:
- Initial run establishes the constraint
- But when B2 is labeled to 1, propagator doesn't run
- Constraint is posted but propagator isn't re-triggered

## Why Ground Boolean Works

When the boolean is ground at reification time (1 #<=> (X #= Y)):
- Takes a different code path (lines 1164-1171 in builtins_clpfd.py)
- Posts constraint immediately without creating propagator
- Uses `_post_constraint_with_flush` which includes singleton binding
- Avoids the problematic propagator/watcher mechanism entirely

## Next Steps

### Phase 1: Fix Watcher Registration
- Debug why `add_watcher` calls don't result in registered watchers
- Ensure propagator ID is valid and propagator is properly registered
- Verify watchers are preserved across trail operations

### Phase 2: Fix Triple Posting
- Improve post_key generation to be truly unique
- Consider using global registry instead of variable attributes
- Add mutex/guard against concurrent posting

### Phase 3: Verify Propagator Triggering
- Ensure propagator runs when B value changes
- Check if propagation queue is processing correctly
- Verify cause tracking works properly

## Code Locations

### Key Files Modified
- `prolog/clpfd/props/equality.py` - Added safe binding
- `prolog/engine/builtins_clpfd.py` - Added post-propagation singleton binding
- `prolog/clpfd/safe_bind.py` - New safe binding helper

### Problem Areas
- `prolog/clpfd/props/reif.py:143-154` - Triple posting occurs here
- `prolog/engine/builtins_clpfd.py:1193-1200` - Watcher registration failing
- `prolog/clpfd/hooks.py:87` - Hook propagation check (now less aggressive)

## Conclusion

The core issue of premature binding during propagation has been solved with the safe binding approach. However, the reification propagator infrastructure needs fixing to properly track constraints and trigger during labeling. Once watchers are properly registered and the triple posting is resolved, the trichotomy test should pass with all 25 expected solutions.