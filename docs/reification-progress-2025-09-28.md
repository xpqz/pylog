# Reification Implementation Progress - 2025-09-28

## What Was Fixed

### 1. Idempotent Posting ✅
- Implemented idempotent constraint posting using trailed attributes on Boolean variables
- Keys stored in `attrs['reif_posts']` and `attrs['impl_posts']` sets
- Choice-point aware through trailing mechanism

### 2. Singleton Domain Binding ✅
- Fixed critical issue where Boolean variables with singleton domains weren't being bound
- Added `bind_root_to_term` calls when domain becomes singleton {0} or {1}
- This ensures variables are properly ground for labeling

### 3. Dead Code Removal ✅
- Removed all dead closure state (`posted_true`, `posted_false`, `last_entailment`, etc.)
- Clean, stateless propagator design

## Current Status

### Main Test Case: FIXED ✅
Original bug report test now works correctly:
```prolog
X in 1..10, Y in 1..10,
B1 #<=> (X #< 5), B2 #<=> (Y #> 3),
B1 #= B2, label([X, Y, B1, B2])
```
- SWI-Prolog: 46 solutions
- PyLog: 46 solutions ✅

### Three-Boolean Test: PARTIAL ⚠️
Complex test with three mutually exclusive constraints:
```prolog
X in 1..5, Y in 1..5,
B1 #<=> (X #< Y), B2 #<=> (X #= Y), B3 #<=> (X #> Y),
label([B1, B2, B3, X, Y])
```
- SWI-Prolog: 25 solutions (all valid)
- PyLog: 18 solutions (all valid, but missing 7)

### Missing Solutions Analysis
PyLog is missing these 7 solutions where B1=1 (X < Y):
1. X=1, Y=2 (B1=1, B2=0, B3=0)
2. X=1, Y=3 (B1=1, B2=0, B3=0)
3. X=1, Y=4 (B1=1, B2=0, B3=0)
4. X=1, Y=5 (B1=1, B2=0, B3=0)
5. X=2, Y=5 (B1=1, B2=0, B3=0)
6. X=3, Y=5 (B1=1, B2=0, B3=0)
7. X=4, Y=5 (B1=1, B2=0, B3=0)

All missing solutions have B1=1, suggesting an issue with how the B1 #<=> (X #< Y) constraint explores its search space when B1 is set to 1.

## What's Working Well

1. **No Invalid Solutions**: All 18 solutions PyLog produces are valid (exactly one Boolean is 1)
2. **Singleton Binding**: Variables are properly bound when domains become singleton
3. **Idempotent Posting**: No duplicate constraint registration
4. **Clean Code**: Dead state removed, clean propagator design

## Remaining Issue

The issue appears to be in the search tree exploration. When labeling tries B1=1, it should explore all X,Y combinations where X < Y. Currently it's missing some branches, particularly those involving X=1.

This could be:
1. A pruning issue where valid branches are incorrectly eliminated
2. A propagation ordering issue where constraints interact incorrectly
3. A labeling strategy issue where some branches aren't explored

## Files Modified

1. `/Users/stefan/work/pylog/prolog/clpfd/props/reif.py`
   - Added imports at top (Int, bind_root_to_term)
   - Added idempotent posting tracking
   - Added singleton domain binding
   - Removed dead closure state

## Test Results Summary

| Test | Expected | Actual | Status |
|------|----------|--------|--------|
| Main reification (46 solutions) | 46 | 46 | ✅ |
| Three booleans (25 solutions) | 25 | 18 | ⚠️ |
| Simple X=1,Y=1 binding | B values bound | B values bound | ✅ |
| No invalid solutions | 0 invalid | 0 invalid | ✅ |

## Next Steps

1. Debug why X=1 branches are missing in three-boolean test
2. Verify other integration tests still pass
3. Add specific regression tests for the fixes