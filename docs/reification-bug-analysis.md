# Analysis: CLP(FD) Reification Bug (Issue #167)

## Executive Summary

The PyLog CLP(FD) reification system has a critical bug where Boolean variables in reification constraints (e.g., `B #<=> (X #< Y)`) are not correctly updated when the constrained variables (X, Y) are assigned values through the labeling process. This leads to incorrect solutions that violate the reification constraints.

## The Problem

### Expected Behavior
When we have constraints like:
```prolog
X in 1..10, Y in 1..10,
B1 #<=> (X #< 5),    % B1 is 1 iff X < 5
B2 #<=> (Y #> 3),    % B2 is 1 iff Y > 3
B1 #= B2,            % B1 and B2 must be equal
label([X, Y, B1, B2])
```

SWI-Prolog correctly produces 46 solutions where:
- When X < 5 and Y > 3: B1 = 1, B2 = 1 (28 solutions)
- When X ≥ 5 and Y ≤ 3: B1 = 0, B2 = 0 (18 solutions)

### Actual Behavior (Before Fix)
PyLog was producing 90 solutions, including invalid ones like:
- X = 2, Y = 1 with B1 = 1, B2 = 1 (incorrect: Y ≤ 3 so B2 should be 0)
- X = 5, Y = 4 with B1 = 0, B2 = 0 (incorrect: Y > 3 so B2 should be 1)

## Root Cause Analysis

### The Core Issue
The reification propagator uses internal flags (`posted_true` and `posted_false`) to track whether it has already posted the underlying constraint or its negation. These flags:
1. Are not trailed (persist across backtracking)
2. Prevent re-evaluation of constraints when variables change values

When labeling creates choice points and explores different values:
1. First branch: X=1 is tried, propagator runs, sets flags
2. Later branches: X=2, X=3, etc. are tried, but propagator doesn't re-evaluate because flags are already set
3. The Boolean variable B doesn't get updated to reflect the new constraint state

### Why This Happens During Labeling

The labeling process creates a search tree with choice points:
```
label([X, Y, B1, B2])
  ├─ X=1
  │   ├─ Y=1
  │   │   ├─ B1=0/1 (tries both)
  │   │   └─ B2=0/1 (tries both)
  │   ├─ Y=2
  │   ...
  ├─ X=2
  ...
```

When exploring X=2 after X=1, the reification propagator should re-evaluate, but the `posted_true`/`posted_false` flags prevent this.

## Attempted Solutions

### Attempt 1: Move Unification Hook Timing
**Hypothesis**: The unification hook is called before binding, so propagators don't see the new value.

**Changes**:
- Moved `dispatch_attr_hooks` call in `unify.py` to after `bind_root_to_term`
- Updated `clpfd_unify_hook` to not narrow domains (since variable already bound)

**Result**: FAILED - Produced 400 solutions instead of 46. The timing change broke other assumptions.

**Why it failed**: The hook needs to run before binding to validate the unification and set up domain changes. Running after binding was too late.

### Attempt 2: Remove Posted Flags (Partial Success)
**Hypothesis**: The flags preventing re-posting are the root cause.

**Changes**:
- Replaced `posted_true`/`posted_false` flags with entailment tracking
- Only post constraints when entailment is UNKNOWN
- Allow re-evaluation on each propagator run

**Result**: PARTIAL SUCCESS
- Main test case now produces correct 46 solutions
- But introduced new issue: 42 solutions instead of 25 in another test

**Why it partially worked**: Removing flags allows re-evaluation, but may cause duplicate propagator posting.

### Attempt 3: Fix Implication Propagators Too
**Hypothesis**: Implication propagators have the same flag issue.

**Changes**:
- Applied same fix to `create_implication_propagator`
- Removed `posted` flag, use entailment state instead

**Result**: MIXED
- Fixed some tests
- Still have failures in complex scenarios

## Current State

### What's Working
1. Basic reification with simple constraints works correctly
2. The main test case (X in 1..10, Y in 1..10) produces correct 46 solutions
3. Entailment detection works properly

### What's Still Broken
1. Complex scenarios with multiple Boolean variables produce duplicates
2. Test expecting 25 solutions gets 42 (likely due to duplicate propagators)
3. Some mutual exclusion constraints aren't enforced

## Deep Dive: The Propagator State Problem

### The Fundamental Design Issue
The propagator design assumes that once a constraint is posted to the propagation network, it doesn't need to be re-posted. This works for simple constraints but breaks for reification where:

1. **Variable bindings change during search**: When labeling tries X=1, then backtracks and tries X=2, the constraint `X #< 5` has different truth values
2. **Boolean variables need updates**: B in `B #<=> (X #< 5)` should track these changes
3. **Propagator state isn't choice-point aware**: The flags persist across backtracking

### Why SWI-Prolog Works
SWI-Prolog likely:
1. Uses attributed variables with unification hooks that properly trigger on each binding
2. Has choice-point-aware propagator state
3. Re-evaluates constraints when variables change during search

### PyLog's Architectural Limitation
PyLog's propagation queue and propagator design doesn't properly handle the interaction between:
1. Choice points created by labeling
2. Propagator state that persists across backtracking
3. The need to re-evaluate constraints in different search branches

## Proposed Solutions

### Solution 1: Trail Propagator State (Recommended)
Make the `posted_true`/`posted_false` flags trailed so they're undone on backtracking:
- Pro: Maintains propagator design philosophy
- Pro: Allows proper re-evaluation in different branches
- Con: Requires adding trail support for propagator state

### Solution 2: Stateless Propagators
Remove all persistent state from propagators:
- Pro: Simpler conceptually
- Con: Risk of infinite loops without careful design
- Con: May post duplicate constraints

### Solution 3: Context-Aware Propagation
Track the "context" (current variable bindings) when posting:
- Only re-post if context has changed
- Pro: Avoids duplicates while allowing re-evaluation
- Con: Complex to implement correctly

## Recommendations

1. **Immediate Fix**: Refine the current approach by:
   - Adding duplicate detection to prevent multiple propagator registration
   - Tracking what was posted for which variable states

2. **Long-term Fix**: Redesign propagator state management to be choice-point aware:
   - Trail propagator state
   - Or make propagators stateless with external state management

3. **Testing**: Add specific tests for:
   - Labeling order variations
   - Complex Boolean constraint networks
   - Backtracking scenarios with reification

## Test Cases Revealing the Bug

### Minimal Failing Case
```prolog
X in 1..3, Y in 1..3,
B1 #<=> (X #< 2),
B2 #<=> (Y #> 2),
B1 #= B2,
label([X, Y, B1, B2])
```
Produces X=3, Y=3, B1=0, B2=0 (incorrect: Y>2 so B2 should be 1)

### Why This Case Fails
1. During labeling, when X=1, Y=1 is tried first
2. B1 domain becomes {1} (X<2 true), B2 domain becomes {0} (Y≤2)
3. B1 #= B2 fails, backtrack
4. When later trying X=3, Y=3:
   - B1 correctly becomes 0 (X≥2)
   - B2 should become 1 (Y>2) but doesn't due to stale propagator state

## Conclusion

The reification bug stems from a fundamental mismatch between:
- How propagators manage internal state (persistent across backtracking)
- How labeling creates and explores choice points
- The need for constraints to be re-evaluated in different search contexts

The partial fix (removing flags) addresses the immediate issue but introduces other problems. A proper fix requires rethinking how propagator state interacts with the search process, likely by making state choice-point aware through the trail mechanism.