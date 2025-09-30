# Reification Implementation Status - 2025-09-28

## Current State

### What's Been Implemented
1. **Idempotent Posting Solution**: Modified `/Users/stefan/work/pylog/prolog/clpfd/props/reif.py` to implement idempotent constraint posting:
   - Removed dead closure state (`last_entailment`, `posted_for_b_value`)
   - Added tracking of posted constraints via trailed attributes on Boolean variables
   - Store posted keys in `attrs['reif_posts']` and `attrs['impl_posts']` sets
   - Keys are normalized tuples to ensure consistency across variable references

2. **Partial Success**: The main test case from the original bug report now works:
   - SWI-Prolog: 46 solutions for `X in 1..10, Y in 1..10, B1 #<=> (X #< 5), B2 #<=> (Y #> 3), B1 #= B2`
   - PyLog (after fix): Also produces 46 solutions ✓

## Current Problem

### The Three-Boolean Test Failure
The test `test_mixed_integer_boolean_labeling` is failing:
```prolog
X in 1..5, Y in 1..5,
B1 #<=> (X #< Y),
B2 #<=> (X #= Y),
B3 #<=> (X #> Y),
label([B1, B2, B3, X, Y])
```

**Expected**: 25 solutions where exactly one of B1, B2, B3 is 1 for each X,Y pair
**SWI-Prolog**: Correctly produces 25 solutions
**PyLog**: Produces 42 solutions (25 valid + 17 invalid)

### Analysis of Invalid Solutions
The 17 invalid solutions include cases where:
1. All three Booleans are 0 (e.g., X=1, Y=1, B1=0, B2=0, B3=0)
2. Multiple Booleans are 1 (e.g., X=3, Y=3, B1=1, B2=1, B3=0)

This violates the fundamental property that X<Y, X=Y, and X>Y are mutually exclusive and exhaustive.

## Root Cause Discovery

### Critical Finding: Boolean Variables Not Being Set
When testing simple cases without labeling:
```prolog
X = 1, Y = 1,
B1 #<=> (X #< Y),  % Should set B1 = 0 (1 < 1 is false)
B2 #<=> (X #= Y),  % Should set B2 = 1 (1 = 1 is true)
B3 #<=> (X #> Y)   % Should set B3 = 0 (1 > 1 is false)
```

**Result**: B1, B2, B3 remain unbound (returned as Var objects, not Int)

This reveals that **the reification propagator is not running when X and Y are already bound**.

## What I've Tried

### 1. Idempotent Posting Implementation (Partially Successful)
- Modified `create_reification_propagator` to track posted constraints
- Used trailed attributes to make tracking choice-point aware
- This fixed the main test case but not the three-Boolean case

### 2. Test Simplification for Debugging
Created several test scripts to isolate the issue:
- `test_three_bools.pl` - SWI-Prolog baseline (confirms 25 solutions)
- `test_three_bools.py` - PyLog test (shows 42 solutions, 17 invalid)
- `test_reif_simple.py` - Simplified test revealing propagator not running

### 3. Investigation of Propagator Registration
Attempted to trace how reification constraints register propagators:
- Could not find where `#<=>` operator is implemented as a builtin
- Could not locate the connection between parsing `B #<=> (X #< Y)` and creating propagators

## The Missing Link

### What Should Happen
When `B #<=> (X #< Y)` is processed:
1. A reification propagator should be created and registered
2. The propagator should watch variables B, X, and Y
3. When X or Y changes (including initial binding), propagator should run
4. Propagator should update B based on entailment of `X #< Y`

### What's Actually Happening
The propagator appears to not be registered or not triggered when:
- X and Y are bound before the reification constraint is posted
- Or the propagator isn't watching the right variables

## Hypothesis

The issue is likely in one of these areas:

1. **Missing Builtin Implementation**: The `#<=>` operator may not be properly implemented as a builtin that creates and registers the reification propagator.

2. **Propagator Registration**: The propagator might not be registered to watch all relevant variables (B, X, and Y).

3. **Initial Propagation**: When variables are already bound, there may be no initial propagation run to establish constraint consistency.

## Next Steps

1. **Find the Implementation**: Locate where `#<=>` is implemented as a builtin
2. **Verify Registration**: Ensure propagator is registered for all variables involved
3. **Check Initial Propagation**: Verify that propagators run when constraints are first posted
4. **Fix the Gap**: Ensure Boolean variables are properly determined when constrained variables are bound

## Test Files Created
- `/tmp/test_three_bools.pl` - SWI-Prolog test
- `/Users/stefan/work/pylog/test_three_bools.py` - Full PyLog test showing 42 solutions
- `/Users/stefan/work/pylog/test_reif_simple.py` - Simplified test showing Booleans not being set
- `/Users/stefan/work/pylog/test_three_bools_debug.py` - Debug version with tracing

## Key Code Locations
- `/Users/stefan/work/pylog/prolog/clpfd/props/reif.py` - Reification propagator implementation (modified)
- `/Users/stefan/work/pylog/prolog/clpfd/hooks.py` - CLP(FD) unification hooks
- `/Users/stefan/work/pylog/prolog/tests/unit/test_clpfd_reification_integration.py` - Integration tests