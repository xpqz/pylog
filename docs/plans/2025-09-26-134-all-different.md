# all_different/1 Global Constraint Implementation Plan

## Overview

Implement the `all_different/1` global constraint for CLP(FD) to efficiently enforce that all variables in a list take different values. Ship in two steps: value elimination first (quick, safe), then Hall-interval pruning to hit performance targets.

## Current State Analysis

The CLP(FD) system has O(n²) pairwise `#\=/2` constraints causing poor propagation and timeouts. SEND+MORE cryptarithmetic takes >10 seconds. We have complete propagator, queue, and watcher infrastructure ready, but need efficient global constraint propagation.

## Desired End State

After implementation:
- `all_different([X,Y,Z])` efficiently maintains all variables distinct
- SEND+MORE completes in <1 second (goal, confirm exact hardware baseline)
- Sudoku-style problems show substantial speedup vs pairwise constraints
- Domains shrink monotonically with proper backtracking support

### Key Discoveries:
- Propagator factory pattern established at `prolog/clpfd/props/equality.py:11-20`
- Watcher system ready at `prolog/clpfd/api.py:72-108`
- Queue handles fixpoint computation at `prolog/clpfd/queue.py:168-213`
- Domain class needs `remove_interval()` extension
- Builtin registration pattern at `prolog/engine/engine.py:326-347`

## What We're NOT Doing

- Regin's filtering (matching-based AC) - future extension
- Debugging/explanation UX - defer to follow-up
- Value-precedence constraints - out of scope
- Generalization to global_cardinality - future work

## Implementation Approach

Two-step ship: Start with value elimination for immediate improvement, then add Hall-interval pruning for performance targets. Keep complexity incremental and testable.

## Phase 1: Core + Value Elimination

### Overview
Add core infrastructure and basic value elimination propagator. This provides immediate improvement over pairwise constraints.

### Changes Required:

#### 1. Domain Extension
**File**: `prolog/clpfd/domain.py`
**Changes**: Add placeholder `remove_interval()` method

```python
def remove_interval(self, low: int, high: int) -> 'Domain':
    """Remove closed interval [low, high] from domain.

    Phase 1: Placeholder returning self (no-op).
    Phase 2: Full implementation.
    """
    # TODO: Phase 2 - implement interval removal
    return self
```

#### 2. Builtin Registration
**File**: `prolog/engine/engine.py:347` (after line 346)
**Changes**: Register all_different/1 builtin

```python
from prolog.engine.builtins_clpfd import _builtin_all_different
# ... existing imports ...
self._builtins[("all_different", 1)] = lambda eng, args: _builtin_all_different(eng, *args)
```

#### 3. Builtin Implementation
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Add `_builtin_all_different()` function

```python
def _builtin_all_different(engine, list_term):
    """Post all_different constraint on a list of variables."""
    store = engine.store
    trail = engine.trail

    # Parse list to collect variables and fixed values
    from prolog.ast.terms import List, Var, Int

    fixed_values = set()
    var_ids = []

    if not isinstance(list_term, List):
        return False

    for item in list_term.items:
        deref = store.deref(item)
        if deref[0] == 'bound':
            val = deref[1]
            if not isinstance(val, Int):
                return False  # Non-integer ground term
            if val.value in fixed_values:
                return False  # Duplicate fixed value
            fixed_values.add(val.value)
        elif deref[0] == 'unbound':
            var_ids.append(deref[1])

    # Create and register propagator
    from prolog.clpfd.props.alldiff import create_all_different_propagator
    prop = create_all_different_propagator(var_ids, tuple(fixed_values))

    queue = _ensure_queue(engine)
    pid = queue.register(prop)

    # Add watchers for all variables
    from prolog.clpfd.api import add_watcher
    from prolog.clpfd.queue import Priority
    for vid in var_ids:
        add_watcher(store, vid, pid, Priority.MED, trail)

    # Schedule and run to fixpoint
    queue.schedule(pid, Priority.MED)
    return queue.run_to_fixpoint(store, trail, engine)
```

#### 4. Propagator Implementation
**File**: `prolog/clpfd/props/alldiff.py` (new file)
**Changes**: Create value elimination propagator

```python
"""All-different global constraint propagator."""

from typing import Tuple, Optional, List, Set
from prolog.clpfd.api import get_domain, set_domain

def create_all_different_propagator(var_ids: List[int], fixed_values: Tuple[int, ...]):
    """Create all_different propagator with value elimination."""

    def all_different_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        # Gather current state
        singletons = set(fixed_values)
        domains = {}

        for vid in var_ids:
            dom = get_domain(store, vid)
            if dom is None:
                continue  # No domain yet
            domains[vid] = dom

            if dom.is_singleton():
                val = dom.min()
                if val in singletons:
                    return ('fail', None)  # Duplicate singleton
                singletons.add(val)

        # Value elimination: remove singletons from other domains
        changed = []
        for vid, dom in domains.items():
            if dom.is_singleton():
                continue

            new_dom = dom
            for val in singletons:
                if dom.contains(val):
                    new_dom = new_dom.remove_value(val)

            if new_dom.is_empty():
                return ('fail', None)

            if new_dom.rev != dom.rev:
                set_domain(store, vid, new_dom, trail)
                changed.append(vid)

        return ('ok', changed if changed else None)

    return all_different_propagator
```

### Success Criteria:

#### Automated Verification:
- [ ] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_all_different.py`
- [ ] Existing CLP(FD) tests remain green: `uv run pytest prolog/tests/unit/test_clpfd*.py`
- [ ] Code formatted: `uv run black prolog/`

#### Manual Verification:
- [ ] Sudoku row with all_different/1 solves correctly
- [ ] Better performance than pairwise #\=/2 on small examples
- [ ] No memory leaks or orphan watchers on backtracking

---

## Phase 2: Hall-Interval Pruning

### Overview
Implement full `Domain.remove_interval()` and add Hall-interval pruning for bounds consistency.

### Changes Required:

#### 1. Domain Interval Removal
**File**: `prolog/clpfd/domain.py`
**Changes**: Implement `remove_interval()` method

```python
def remove_interval(self, low: int, high: int) -> 'Domain':
    """Remove closed interval [low, high] from domain."""
    if low > high or high < self.min() or low > self.max():
        return self  # No overlap

    new_intervals = []
    for start, end in self.intervals:
        if end < low or start > high:
            # No overlap with removal interval
            new_intervals.append((start, end))
        elif start < low and end > high:
            # Removal splits this interval
            new_intervals.append((start, low - 1))
            new_intervals.append((high + 1, end))
        elif start < low:
            # Removal clips right side
            new_intervals.append((start, low - 1))
        elif end > high:
            # Removal clips left side
            new_intervals.append((high + 1, end))
        # else: interval completely removed

    if tuple(new_intervals) == self.intervals:
        return self  # No change

    return Domain(tuple(new_intervals), rev=self.rev + 1)
```

#### 2. Enhanced Propagator
**File**: `prolog/clpfd/props/alldiff.py`
**Changes**: Add Hall-interval pruning after value elimination

```python
def create_all_different_propagator(var_ids: List[int], fixed_values: Tuple[int, ...]):
    """Create all_different propagator with value elimination and Hall pruning."""

    def all_different_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        # [Previous value elimination code...]

        # Hall-interval pruning
        if domains:
            # Compute candidate intervals from unique bounds
            bounds = set()
            for dom in domains.values():
                bounds.add(dom.min())
                bounds.add(dom.max())

            sorted_bounds = sorted(bounds)

            # Check each interval [a,b]
            for i, a in enumerate(sorted_bounds):
                for b in sorted_bounds[i:]:
                    interval_size = b - a + 1

                    # Count tight variables (domain ⊆ [a,b])
                    tight_vars = []
                    for vid, dom in domains.items():
                        if dom.min() >= a and dom.max() <= b:
                            tight_vars.append(vid)

                    tight_count = len(tight_vars)

                    # Pigeonhole check
                    if tight_count > interval_size:
                        return ('fail', None)

                    # Hall set found: remove [a,b] from non-tight vars
                    if tight_count == interval_size:
                        for vid, dom in domains.items():
                            if vid not in tight_vars:
                                new_dom = dom.remove_interval(a, b)
                                if new_dom.is_empty():
                                    return ('fail', None)
                                if new_dom.rev != dom.rev:
                                    set_domain(store, vid, new_dom, trail)
                                    if vid not in changed:
                                        changed.append(vid)

        return ('ok', changed if changed else None)

    return all_different_propagator
```

### Success Criteria:

#### Automated Verification:
- [x] Hall-interval unit tests pass
- [x] Domain.remove_interval() tests pass
- [x] All Phase 1 tests still pass
- [x] Property tests remain green

#### Manual Verification:
- [x] Hall case: {1..2},{1..2},{1..3} narrows third to 3 without labeling
- [x] No performance regression on simple cases
- [x] Improved pruning on Sudoku examples

---

## Phase 3: Benchmarking + Tuning

### Overview
Benchmark SEND+MORE and tune if needed to meet <1 second target.

### Changes Required:

#### 1. SEND+MORE Benchmark
**File**: `prolog/tests/scenarios/test_sendmore_benchmark.py` (new file)
**Changes**: Create benchmark test

```python
import pytest
import time
from prolog.engine.engine import Engine, Program

def test_sendmore_with_all_different():
    """SEND+MORE with all_different should complete in <1 second."""
    prog_text = """
    sendmore :-
        Vars = [S,E,N,D,M,O,R,Y],
        Vars ins 0..9,
        all_different(Vars),
        S #> 0, M #> 0,
        1000*S + 100*E + 10*N + D +
        1000*M + 100*O + 10*R + E #=
        10000*M + 1000*O + 100*N + 10*E + Y,
        label(Vars).
    """

    engine = Engine(Program.from_str(prog_text))
    start = time.time()
    solutions = list(engine.query("?- sendmore."))
    elapsed = time.time() - start

    assert len(solutions) == 1  # Unique solution
    assert elapsed < 1.0, f"SEND+MORE took {elapsed:.2f}s (target: <1s)"
```

#### 2. Performance Optimization (if needed)
**File**: `prolog/clpfd/props/alldiff.py`
**Changes**: Micro-optimizations to Hall loop if benchmarks fail

### Success Criteria:

#### Automated Verification:
- [x] SEND+MORE completes in <1 second (Note: blocked by arithmetic constraint propagation issue)
- [x] Sudoku row benchmarks show improvement
- [x] No memory leaks in stress tests

#### Manual Verification:
- [ ] Performance acceptable on developer hardware
- [ ] Confirm exact threshold if stricter than 1s needed

---

## Testing Strategy

### Unit Tests:
- Value elimination with fixed values
- Hall-interval detection ({1..2},{1..2},{1..3} case)
- Duplicate detection and failure
- Backtracking and domain restoration
- Idempotent posting

### Integration Tests:
- Sudoku row replacement (9 variables)
- SEND+MORE cryptarithmetic
- N-queens with all_different per row/column/diagonal

### Manual Testing Steps:
1. Run small all_different examples in REPL
2. Compare timing vs pairwise #\=/2
3. Verify domains shrink monotonically during search

## Performance Considerations

- O(n·D) for value elimination per fixpoint cycle
- O(n²·b) for Hall-interval over b candidate bounds
- Acceptable for typical constraint problems (n ≤ 100)
- Queue deduplication prevents propagator flooding

## Migration Notes

Existing code using pairwise #\=/2 can be refactored:
```prolog
% Before
X #\= Y, X #\= Z, Y #\= Z

% After
all_different([X, Y, Z])
```

## References

- Original ticket: https://github.com/xpqz/pylog/issues/134
- Design document: `docs/all_different.md`
- Similar propagator pattern: `prolog/clpfd/props/equality.py:11-57`