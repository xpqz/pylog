# Remaining XFAIL Analysis - Minimal Fixes

## Summary
8 tests marked XFAIL, but one (`test_non_matching_catcher_propagates`) is XPASS in base Engine.
Focus: Minimal, targeted fixes leveraging existing infrastructure.

## Group 1: Complex State Restoration (3 tests)

### test_complex_unification_undone
**Issue**: Multiple variables unified in compound terms before throw
```prolog
f(X, Y, Z) = f(1, 2, 3),
g(A, B) = g(X, Y),
throw(reset)
```
**Expected**: All vars (X,Y,Z,A,B) should be unbound after catch
**Likely Cause**:
- All bindings ARE being trailed (we fixed var_stamps clearing)
- But union-find parent changes may not all be trailed
- Check if `unify_struct` trails all parent changes when unifying args

### test_list_unification_restoration
**Issue**: X bound to 'original', then rebound to list, should restore to 'original'
```prolog
X = original,
catch((X = [1,2,3|T], T = [4,5], throw(error)), error, Result = X)
```
**Expected**: Result = original
**Likely Cause**:
- X's binding to 'original' happens BEFORE catch CP created
- Inside catch, X gets rebound to list structure
- The rebinding should be trailed, but may not be restoring correctly
- Check trail entries for rebinding already-bound variables

### test_variable_aliasing_restoration
**Issue**: Alias chain X=Y, Y=Z, Z=value should be undone
```prolog
catch((X = Y, Y = Z, Z = value, throw(error)), error, (var(X), var(Y), var(Z)))
```
**Expected**: All three vars unbound after catch
**Likely Cause**:
- Union-find parent updates for aliasing may not all be trailed
- Path compression (if enabled) might skip trailing some parents
- Check `TrailAdapter.union` to ensure both parent AND rank changes are trailed

## Group 2: Cut Semantics (1 test)

### test_catch_creates_cut_barrier
**Issue**: Cut inside catch shouldn't escape catch scope
```prolog
(catch(!, _, fail) ; X = survived)
```
**Expected**: X = survived (disjunction alternative should remain)
**Current**: We added frame with cut_barrier in _builtin_catch
**Check**: Is the cut actually respecting the barrier? Check `_dispatch_cut` logic

## Group 3: Choice Point Management (2 tests)

### test_catch_at_choice_points
**Issue**: Assertion failure during CP restoration
```prolog
test_choice(option1).
test_choice(option2) :- throw(error).
test_choice(option3).
```
**Likely Cause**:
- CP restoration assertion in `_restore_to_choicepoint`
- May be restoring to wrong CP height
- Check the assertion condition and CP stack management

### test_streaming_cursor_restoration
**Issue**: Streaming cursor state lost during exception
**Current Design**: Catching prunes in-scope CPs (including streaming)
**Resolution**: Update test expectations - don't expect cursor restoration

## Group 4: Recursion (1 test)

### test_recursive_predicate_with_catch
**Issue**: Recursive countdown with catch/throw
```prolog
countdown(0) :- throw(done).
countdown(N) :- N > 0, N1 is N - 1, countdown(N1).
```
**Likely Cause**: Same as test_catch_at_choice_points - CP restoration issue

## Group 5: ISO Semantics (1 test)

### test_iso_ball_copy_semantics
**Issue**: Test expects Y=unbound, gets Y=modified
**Current**: Ball is reified at throw time, not copied
**Resolution**: This is a design choice - document as PyLog semantics differ from ISO here

## Targeted Fixes

### Priority 1: Check Trailing Coverage
1. Verify `TrailAdapter.union` trails BOTH parent and rank changes
2. Check if rebinding already-bound vars is properly trailed
3. Ensure no untrailed writes in `unify_struct` arg loop

### Priority 2: Fix CP Restoration Assertion
1. Find the failing assertion in `_restore_to_choicepoint`
2. Likely issue with CP height calculation or goal stack restoration
3. May need to adjust CP payload for CATCH type

### Priority 3: Clarify Semantics
1. Streaming: Document that catch prunes streaming cursors
2. Ball copy: Document PyLog uses reification, not copy
3. Cut barrier: Verify _dispatch_cut checks frame barriers correctly

## Next Steps

1. Run tests individually with debug output to pinpoint exact failure points
2. Add trail entry logging to see what's being trailed vs not
3. Check assertions in CP restoration code
4. Update test expectations where semantics differ from ISO