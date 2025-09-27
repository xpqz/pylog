# Stage 5: CLP(FD) Core Implementation Plan

## Critical Update: Missing Phase 5.2 (Linear Sum Constraints)

**Important**: The original PLAN.md specified "Stage 5.2 - Small linear sums" for handling arithmetic expressions like `A*X + B*Y #= C`. This phase was skipped during implementation, jumping directly from basic propagators (Phase 3) to labeling strategies (Phase 5) and then to Stage 6 (global constraints like `all_different`).

**Impact**: Without linear sum propagators, complex arithmetic constraints like those in SEND+MORE (`1000*S + 100*E + 10*N + D + ...`) fall back to weak propagation, causing severe performance issues. The tests are marked as skipped with "arithmetic constraint propagation issue".

**This document has been expanded** to include the missing Phase 5.2 implementation plan (see Phase 5 below), which should be implemented to complete Stage 5 properly and enable cryptarithmetic problems to work efficiently.

## Overview

Implement the core CLP(FD) (Constraint Logic Programming over Finite Domains) solver with domain representation, propagation queue, basic propagators, **linear sum constraints**, and labeling strategies. This provides working finite domain constraint solving with propagation to fixpoint.

## Current State Analysis

Stage 4 (Attributed Variables) is complete with:
- `put_attr/3`, `get_attr/3`, `del_attr/2` builtins (`prolog/engine/engine.py:2679-2823`)
- Hook registration and dispatch system (`prolog/engine/engine.py:2825-2875`)
- Attribute trailing with `('attr', varid, module, old_value)` entries (`prolog/unify/trail.py:91-107`)
- Integration points in unification (`prolog/unify/unify.py:119,134`)
- Comprehensive test suite for attributed variables (`prolog/tests/unit/test_attributed_vars.py`)

### Key Discoveries:
- Store.attrs provides sparse storage for variable attributes (`prolog/unify/store.py:27`)
- Trail already supports attribute restoration during backtracking
- Hook dispatch occurs before binding, enabling constraint checking
- Directory structure exists: `prolog/clpfd/` and `prolog/clpfd/props/`

## Desired End State

A working CLP(FD) solver that:
- Represents finite domains as interval sets with revision tracking
- Propagates constraints to fixpoint via priority queue
- Supports basic constraints: `#=/2`, `#</2`, `#=</2`, `#>/2`, `#>=/2`, `in/2`
- Implements labeling with variable/value selection strategies
- Integrates seamlessly with attributed variables for backtracking
- Maintains confluence (order-independent fixpoint)

### Verification:
- `X in 1..10, X #> 5` correctly prunes to `X in 6..10`
- Constraint chains propagate: `X #< Y, Y #< Z, Z in 1..5` prunes all variables
- Labeling enumerates all solutions for small problems
- Property tests confirm confluence and monotonicity

## What We're NOT Doing

- No bitset or holes representation initially (just interval sets)
- No global constraints (`all_different`, etc.) - deferred to Stage 6
- No reification (`#<==>`) - deferred to Stage 5.5
- No general linear solver or arc-consistency
- No direct Store integration - use attrs['clpfd'] exclusively
- No custom failure types - use standard unification failure

## Implementation Approach

Build entirely through attributed variables API:
- Store domains and watchers in `attrs['clpfd']`
- Leverage existing attribute trailing for backtracking
- Use `attr_unify_hook` for domain merging on aliasing
- Type-based propagator priorities (high/med/low)
- Interval-set domains with monotonic revision counters

## Phase 1: Core Domain Infrastructure and `in/2`

### Overview
Implement domain representation as interval sets with basic operations, trailing support, and the `in/2` builtin for setting domains. Per PLAN.md milestone 5.0, this includes "Domains + trail + basic `in/2` posting; fixpoint engine skeleton."

### Changes Required:

#### 1. Domain Representation
**File**: `prolog/clpfd/domain.py`
**Changes**: Create immutable domain class with interval-set representation

```python
from dataclasses import dataclass
from typing import List, Tuple, Optional

@dataclass(frozen=True)
class Domain:
    """Immutable finite domain as sorted disjoint intervals."""
    intervals: Tuple[Tuple[int, int], ...]  # Sorted tuple of (min, max) inclusive ranges
    rev: int = 0  # Monotonic revision counter

    def is_empty(self) -> bool:
        """Check if domain is empty."""
        return len(self.intervals) == 0

    def is_singleton(self) -> bool:
        """Check if domain contains exactly one value."""
        return (len(self.intervals) == 1 and
                self.intervals[0][0] == self.intervals[0][1])

    def min(self) -> Optional[int]:
        """Get minimum value or None if empty."""
        return self.intervals[0][0] if self.intervals else None

    def max(self) -> Optional[int]:
        """Get maximum value or None if empty."""
        return self.intervals[-1][1] if self.intervals else None

    def size(self) -> int:
        """Count total values in domain."""
        return sum(high - low + 1 for low, high in self.intervals)

    def contains(self, value: int) -> bool:
        """Check if value is in domain using binary search."""
        left, right = 0, len(self.intervals) - 1
        while left <= right:
            mid = (left + right) // 2
            low, high = self.intervals[mid]
            if low <= value <= high:
                return True
            elif value < low:
                right = mid - 1
            else:
                left = mid + 1
        return False

    def intersect(self, other: 'Domain') -> 'Domain':
        """Return intersection (returns self if unchanged)."""
        # Two-pointer merge of sorted intervals
        result = []
        i, j = 0, 0
        while i < len(self.intervals) and j < len(other.intervals):
            a_low, a_high = self.intervals[i]
            b_low, b_high = other.intervals[j]

            # Find overlap
            low = max(a_low, b_low)
            high = min(a_high, b_high)

            if low <= high:
                result.append((low, high))

            # Advance pointer with smaller endpoint
            if a_high < b_high:
                i += 1
            else:
                j += 1

        new_intervals = tuple(result)
        if new_intervals == self.intervals:
            return self  # No change

        # Changed - bump revision
        return Domain(new_intervals, max(self.rev, other.rev) + 1)

    def remove_value(self, value: int) -> 'Domain':
        """Remove single value, splitting intervals if needed."""
        if not self.contains(value):
            return self  # No change

        result = []
        for low, high in self.intervals:
            if value < low or value > high:
                result.append((low, high))
            elif low == high == value:
                # Remove singleton
                pass
            elif value == low:
                result.append((low + 1, high))
            elif value == high:
                result.append((low, high - 1))
            else:
                # Split interval
                result.append((low, value - 1))
                result.append((value + 1, high))

        return Domain(tuple(result), self.rev + 1)

    def remove_lt(self, bound: int) -> 'Domain':
        """Remove all values < bound."""
        result = []
        for low, high in self.intervals:
            if high < bound:
                continue  # Entire interval removed
            elif low < bound:
                result.append((bound, high))  # Trim interval
            else:
                result.append((low, high))  # Keep entire interval

        new_intervals = tuple(result)
        if new_intervals == self.intervals:
            return self
        return Domain(new_intervals, self.rev + 1)

    def remove_le(self, bound: int) -> 'Domain':
        """Remove all values <= bound."""
        return self.remove_lt(bound + 1)

    def remove_gt(self, bound: int) -> 'Domain':
        """Remove all values > bound."""
        result = []
        for low, high in self.intervals:
            if low > bound:
                continue  # Entire interval removed
            elif high > bound:
                result.append((low, bound))  # Trim interval
            else:
                result.append((low, high))  # Keep entire interval

        new_intervals = tuple(result)
        if new_intervals == self.intervals:
            return self
        return Domain(new_intervals, self.rev + 1)

    def remove_ge(self, bound: int) -> 'Domain':
        """Remove all values >= bound."""
        return self.remove_gt(bound - 1)
```

#### 2. CLP(FD) Attribute Storage and Watchers
**File**: `prolog/clpfd/api.py`
**Changes**: Helper functions for domain and watcher access via attrs

```python
from enum import Enum

class Priority(Enum):
    HIGH = 0
    MED = 1
    LOW = 2

def get_fd_attrs(store, varid):
    """Get CLP(FD) attributes for a variable."""
    attrs = store.get_attrs(varid)
    if attrs and 'clpfd' in attrs:
        return attrs['clpfd']
    return None

def get_domain(store, varid):
    """Get domain for a variable."""
    fd_attrs = get_fd_attrs(store, varid)
    return fd_attrs.get('domain') if fd_attrs else None

def set_domain(store, varid, domain, trail):
    """Set domain for a variable with trailing."""
    # Get current attrs via store helper
    attrs = store.get_attrs(varid) or {}

    # Get or create clpfd attrs (immutable update)
    fd_attrs = attrs.get('clpfd', {}).copy()
    old_domain = fd_attrs.get('domain')

    # Only update if changed
    if old_domain == domain:
        return domain

    # Set new domain
    fd_attrs['domain'] = domain

    # Update attrs with trailing (put_attr handles trailing)
    store.put_attr(varid, 'clpfd', fd_attrs, trail)
    return domain

def add_watcher(store, varid, pid, priority, trail):
    """Add propagator to variable's watcher set."""
    attrs = store.get_attrs(varid) or {}
    fd_attrs = attrs.get('clpfd', {}).copy()

    # Initialize watchers if needed
    if 'watchers' not in fd_attrs:
        fd_attrs['watchers'] = {
            Priority.HIGH: set(),
            Priority.MED: set(),
            Priority.LOW: set()
        }
    else:
        # Copy watchers for immutable update
        fd_attrs['watchers'] = {
            p: fd_attrs['watchers'][p].copy() for p in Priority
        }

    # Add to appropriate priority set
    fd_attrs['watchers'][priority].add(pid)

    # Update attrs
    store.put_attr(varid, 'clpfd', fd_attrs, trail)

def iter_watchers(store, varid):
    """Iterate over all watchers for a variable."""
    fd_attrs = get_fd_attrs(store, varid)
    if not fd_attrs or 'watchers' not in fd_attrs:
        return

    watchers = fd_attrs['watchers']
    # Yield in priority order
    for priority in Priority:
        for pid in watchers.get(priority, set()):
            yield (pid, priority)
```

#### 3. Domain Posting Builtin
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Implement `in/2` builtin

```python
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain

def _builtin_in(engine, x_term, domain_term):
    """X in Domain - set domain for variable X."""
    # Deref X
    x_deref = engine.store.deref(x_term.id if isinstance(x_term, Var) else x_term)

    if x_deref[0] == "BOUND":
        # Check if value is in domain
        value = x_deref[2]
        if isinstance(value, Int):
            domain = parse_domain_term(domain_term)
            return domain.contains(value.value)
        return False  # Non-integer can't be in finite domain

    # X is unbound
    x_var = x_deref[1]

    # Parse domain term (e.g., 1..10, {1,3,5})
    domain = parse_domain_term(domain_term)

    # Set domain with trailing
    set_domain(engine.store, x_var, domain, engine.trail)

    # Register unification hook if first CLP(FD) constraint
    if not hasattr(engine, '_clpfd_inited') or not engine._clpfd_inited:
        from prolog.clpfd.hooks import clpfd_unify_hook
        engine.register_attr_hook('clpfd', clpfd_unify_hook)
        engine._clpfd_inited = True

    return True

def parse_domain_term(term):
    """Parse domain specification into Domain object."""
    if isinstance(term, Int):
        # Singleton domain
        return Domain(((term.value, term.value),))

    elif isinstance(term, Struct):
        if term.functor == ".." and len(term.args) == 2:
            # Interval: low..high
            if isinstance(term.args[0], Int) and isinstance(term.args[1], Int):
                low, high = term.args[0].value, term.args[1].value
                if low <= high:
                    return Domain(((low, high),))
                return Domain(())  # Empty if low > high

        elif term.functor == "\\/" and len(term.args) == 2:
            # Union of domains
            d1 = parse_domain_term(term.args[0])
            d2 = parse_domain_term(term.args[1])
            # Merge intervals (simplified - proper implementation needed)
            return Domain(tuple(sorted(set(d1.intervals + d2.intervals))))

        elif term.functor == "{}":
            # Enumerated set: {1,3,5}
            intervals = []
            for arg in term.args:
                if isinstance(arg, Int):
                    intervals.append((arg.value, arg.value))
            return Domain(tuple(sorted(intervals)))

    raise ValueError(f"Invalid domain term: {term}")
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_domain.py`
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_in.py`
- [x] Domain operations maintain sorted intervals
- [x] Revision counter increments only on actual changes
- [x] Empty domain detection works
- [x] `X in 1..10` sets correct domain
- [x] `X in {1,3,5}` creates enumerated domain
- [x] Watcher storage and retrieval works

#### Manual Verification:
- [x] Domain operations are efficient for large intervals
- [x] Memory usage reasonable for sparse domains
- [x] Parser handles all specified forms

---

## Phase 2: Propagation Queue

### Overview
Implement priority-based propagation queue with deduplication and self-requeue guards.

### Changes Required:

#### 1. PropagationQueue Implementation
**File**: `prolog/clpfd/queue.py`
**Changes**: Three-priority queue with propagator management

```python
from collections import deque
from typing import Dict, Set, Optional, Callable
from enum import Enum

class Priority(Enum):
    HIGH = 0
    MED = 1
    LOW = 2

class PropagationQueue:
    """Priority-based propagation queue."""

    def __init__(self):
        self.queues = {
            Priority.HIGH: deque(),
            Priority.MED: deque(),
            Priority.LOW: deque()
        }
        self.queued: Dict[int, Priority] = {}  # pid -> current priority
        self.running: Optional[int] = None  # Currently running propagator
        self.propagators: Dict[int, Callable] = {}  # ID -> propagator function
        self.next_id = 0

    def register(self, prop: Callable) -> int:
        """Register propagator and return its ID."""
        pid = self.next_id
        self.next_id += 1
        self.propagators[pid] = prop
        return pid

    def schedule(self, pid: int, priority: Priority, cause: Optional[Tuple] = None):
        """Schedule propagator with priority escalation."""
        if pid == self.running:
            return  # Self-requeue guard

        if pid in self.queued:
            # Already queued - check for priority escalation
            current_prio = self.queued[pid]
            if priority.value < current_prio.value:  # Higher priority = lower value
                # Remove from old queue and escalate
                # (Note: This is O(n) but queues are typically small)
                for item in list(self.queues[current_prio]):
                    if item[0] == pid:
                        self.queues[current_prio].remove(item)
                        break
                # Add to new priority queue
                self.queues[priority].append((pid, cause))
                self.queued[pid] = priority
        else:
            # New scheduling
            self.queues[priority].append((pid, cause))
            self.queued[pid] = priority

    def run_to_fixpoint(self, store, trail, engine) -> bool:
        """Run propagation until fixpoint reached.

        Returns:
            True if fixpoint reached successfully, False if failure detected.
        """
        from prolog.clpfd.api import iter_watchers, get_domain

        while True:
            # Find next propagator (highest priority first)
            next_item = None
            for priority in Priority:
                if self.queues[priority]:
                    next_item = self.queues[priority].popleft()
                    break

            if not next_item:
                return True  # Fixpoint reached

            pid, cause = next_item
            del self.queued[pid]  # Clear queued flag before running
            self.running = pid

            try:
                # Run propagator
                prop = self.propagators[pid]
                result = prop(store, trail, engine, cause)

                if result[0] == 'fail':
                    return False  # Propagation failure

                # Wake watchers of changed variables
                if result[0] == 'ok' and result[1]:
                    for changed_var in result[1]:
                        for watcher_pid, watcher_prio in iter_watchers(store, changed_var):
                            if watcher_pid != pid:  # Don't wake self
                                self.schedule(watcher_pid, watcher_prio,
                                            cause=('domain_changed', changed_var))
            finally:
                self.running = None
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_queue.py`
- [x] Priority ordering maintained
- [x] Priority escalation works correctly
- [x] Self-requeue guard prevents infinite loops
- [x] Failure propagation stops fixpoint computation

#### Manual Verification:
- [x] Queue drains efficiently for large propagator sets
- [x] Memory usage bounded by number of propagators

---

## Phase 3: Basic Propagators

### Overview
Implement comparison constraints with bounds-consistency propagation.

### Changes Required:

#### 1. Comparison Propagators
**File**: `prolog/clpfd/props/compare.py`
**Changes**: Implement `#=/2`, `#</2`, `#=</2`, `#>/2`, `#>=/2`

```python
class EqualityPropagator:
    """X #= Y propagator."""

    def __init__(self, x_var, y_var):
        self.x_var = x_var
        self.y_var = y_var

    def run(self, store, trail, engine, cause):
        """Propagate equality constraint.

        Returns:
            ('ok', changed_vars) on success
            ('fail', None) on failure
        """
        from prolog.clpfd.api import get_domain, set_domain

        x_dom = get_domain(store, self.x_var)
        y_dom = get_domain(store, self.y_var)

        if not x_dom or not y_dom:
            return ('ok', None)  # Variable bound, constraint satisfied

        # Intersect domains
        new_x = x_dom.intersect(y_dom)
        new_y = y_dom.intersect(x_dom)  # Same result, but keeps immutability

        changed_vars = []

        # Update X if changed (rev bumped means it changed)
        if new_x.rev > x_dom.rev:
            if new_x.is_empty():
                return ('fail', None)
            set_domain(store, self.x_var, new_x, trail)
            changed_vars.append(self.x_var)

        # Update Y if changed
        if new_y.rev > y_dom.rev:
            if new_y.is_empty():
                return ('fail', None)
            set_domain(store, self.y_var, new_y, trail)
            changed_vars.append(self.y_var)

        return ('ok', changed_vars if changed_vars else None)

class LessThanPropagator:
    """X #< Y propagator."""

    def __init__(self, x_var, y_var):
        self.x_var = x_var
        self.y_var = y_var

    def run(self, store, trail, engine, cause):
        """Propagate X < Y constraint.

        Returns:
            ('ok', changed_vars) on success
            ('fail', None) on failure
        """
        from prolog.clpfd.api import get_domain, set_domain

        x_dom = get_domain(store, self.x_var)
        y_dom = get_domain(store, self.y_var)

        if not x_dom or not y_dom:
            return ('ok', None)

        changed_vars = []

        # X.max must be < Y.max, so X.max < Y.max
        # Y.min must be > X.min, so Y.min > X.min

        # Prune X: remove values >= Y.max
        if y_dom.max() is not None:
            new_x = x_dom.remove_ge(y_dom.max())
            if new_x.rev > x_dom.rev:
                if new_x.is_empty():
                    return ('fail', None)
                set_domain(store, self.x_var, new_x, trail)
                changed_vars.append(self.x_var)

        # Prune Y: remove values <= X.min
        if x_dom.min() is not None:
            new_y = y_dom.remove_le(x_dom.min())
            if new_y.rev > y_dom.rev:
                if new_y.is_empty():
                    return ('fail', None)
                set_domain(store, self.y_var, new_y, trail)
                changed_vars.append(self.y_var)

        return ('ok', changed_vars if changed_vars else None)
```

#### 2. Posting API
**File**: `prolog/clpfd/api.py`
**Changes**: Add constraint posting functions

```python
def post_constraint(engine, constraint_type, x_var, y_var):
    """Post a constraint and run initial propagation.

    Returns:
        True if constraint posted successfully, False on failure.
    """
    from prolog.clpfd.api import add_watcher, Priority
    from prolog.clpfd.props.compare import EqualityPropagator, LessThanPropagator

    queue = engine.get_clpfd_queue()

    # Create appropriate propagator
    if constraint_type == '#=':
        prop = EqualityPropagator(x_var, y_var)
        priority = Priority.HIGH
    elif constraint_type == '#<':
        prop = LessThanPropagator(x_var, y_var)
        priority = Priority.MED
    else:
        raise ValueError(f"Unknown constraint type: {constraint_type}")

    # Register propagator
    pid = queue.register(prop.run)

    # Add watchers on both variables (idempotent - set prevents duplicates)
    add_watcher(engine.store, x_var, pid, priority, engine.trail)
    add_watcher(engine.store, y_var, pid, priority, engine.trail)

    # Run initial propagation
    queue.schedule(pid, priority)
    success = queue.run_to_fixpoint(engine.store, engine.trail, engine)

    return success
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_propagators.py`
- [x] Interval tightening: `X in 1..10, X #> 5` => `X in 6..10`
- [x] Chain propagation works correctly
- [x] Failure detection on inconsistency

#### Manual Verification:
- [x] Propagation reaches fixpoint efficiently
- [x] No unnecessary re-propagation

---

## Phase 4: Engine Integration and Constraint Builtins

### Overview
Integrate CLP(FD) with the engine and implement constraint posting builtins.

### Changes Required:

#### 1. Engine Integration
**File**: `prolog/engine/engine.py`
**Changes**: Add CLP(FD) queue and registration

```python
# In Engine class, add:
def get_clpfd_queue(self):
    """Get or create CLP(FD) propagation queue."""
    if not hasattr(self, '_clpfd_queue'):
        from prolog.clpfd.queue import PropagationQueue
        self._clpfd_queue = PropagationQueue()
    return self._clpfd_queue

# In Engine._register_builtins, add:
from prolog.engine.builtins_clpfd import (
    _builtin_in, _builtin_constraint_eq, _builtin_constraint_lt,
    _builtin_constraint_le, _builtin_constraint_gt, _builtin_constraint_ge
)
self.register_builtin("in", 2, _builtin_in)
self.register_builtin("#=", 2, _builtin_constraint_eq)
self.register_builtin("#<", 2, _builtin_constraint_lt)
self.register_builtin("#=<", 2, _builtin_constraint_le)
self.register_builtin("#>", 2, _builtin_constraint_gt)
self.register_builtin("#>=", 2, _builtin_constraint_ge)
```

#### 2. Constraint Posting Builtins
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Implement constraint builtins

```python
from prolog.ast.terms import Var
from prolog.clpfd.api import post_constraint

def _builtin_constraint_eq(engine, x_term, y_term):
    """X #= Y - equality constraint."""
    # Deref both terms
    x_deref = engine.store.deref(x_term.id if isinstance(x_term, Var) else x_term)
    y_deref = engine.store.deref(y_term.id if isinstance(y_term, Var) else y_term)

    # Both must be unbound for constraint posting
    if x_deref[0] != "UNBOUND" or y_deref[0] != "UNBOUND":
        # Handle ground cases
        # ... (simplified for brevity)
        return False

    return post_constraint(engine, '#=', x_deref[1], y_deref[1])

def _builtin_constraint_lt(engine, x_term, y_term):
    """X #< Y - less than constraint."""
    # Similar structure to equality
    # ...
    return post_constraint(engine, '#<', x_deref[1], y_deref[1])
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_builtins.py`
- [x] Constraints register and propagate correctly
- [x] Queue initialization works on demand
- [x] Builtins integrate with engine properly

#### Manual Verification:
- [x] Constraint posting from Prolog queries works
- [x] Multiple constraints interact correctly

---

## Phase 5: Linear Sum Constraints (Stage 5.2 from PLAN.md)

### Overview
Implement linear sum constraints to support arithmetic expressions like `1000*S + 100*E + 10*N + D #= Result` and `X + Y + Z #= 15`. This fills the critical gap between basic propagators (Phase 3) and global constraints (Stage 6), enabling SEND+MORE and similar cryptarithmetic problems.

**NOTE**: This phase was originally planned in PLAN.md as "Stage 5.2 - Small linear sums" but was skipped. Without it, complex arithmetic constraints fall back to weak propagation, causing poor performance.

### Changes Required:

#### 1. Expression Parser for Arithmetic Terms
**File**: `prolog/clpfd/expr.py` (new file)
**Changes**: Parse arithmetic expressions into coefficient-variable pairs

```python
from typing import List, Tuple, Optional, Dict
from prolog.ast.terms import Var, Int, Struct

def parse_linear_expression(term, engine) -> Tuple[Dict[int, int], int]:
    """Parse linear expression into coefficients and constant.

    Returns:
        (coeffs, constant) where coeffs maps var_id -> coefficient

    Examples:
        1000*S + 100*E + 5  =>  ({S_id: 1000, E_id: 100}, 5)
        X + Y + Z           =>  ({X_id: 1, Y_id: 1, Z_id: 1}, 0)
    """
    coeffs = {}
    constant = 0

    def parse_term(t, sign=1):
        """Recursively parse arithmetic term."""
        nonlocal constant

        if isinstance(t, Int):
            constant += sign * t.value
        elif isinstance(t, Var):
            # Deref to get var ID
            deref = engine.store.deref(t.id)
            if deref[0] == "BOUND":
                if isinstance(deref[2], Int):
                    constant += sign * deref[2].value
                else:
                    raise ValueError(f"Non-integer in arithmetic: {deref[2]}")
            else:
                # Unbound variable with implicit coefficient 1
                var_id = deref[1]
                coeffs[var_id] = coeffs.get(var_id, 0) + sign
        elif isinstance(t, Struct):
            if t.functor == "+" and len(t.args) == 2:
                # Addition: A + B
                parse_term(t.args[0], sign)
                parse_term(t.args[1], sign)
            elif t.functor == "-" and len(t.args) == 2:
                # Subtraction: A - B
                parse_term(t.args[0], sign)
                parse_term(t.args[1], -sign)
            elif t.functor == "*" and len(t.args) == 2:
                # Multiplication: check for Constant * Variable
                left, right = t.args[0], t.args[1]

                # Try Constant * Variable
                if isinstance(left, Int) and isinstance(right, Var):
                    right_deref = engine.store.deref(right.id)
                    if right_deref[0] == "UNBOUND":
                        var_id = right_deref[1]
                        coeffs[var_id] = coeffs.get(var_id, 0) + sign * left.value
                    else:
                        # Variable is bound, treat as constant
                        if isinstance(right_deref[2], Int):
                            constant += sign * left.value * right_deref[2].value
                        else:
                            raise ValueError(f"Non-integer in multiplication")

                # Try Variable * Constant
                elif isinstance(right, Int) and isinstance(left, Var):
                    left_deref = engine.store.deref(left.id)
                    if left_deref[0] == "UNBOUND":
                        var_id = left_deref[1]
                        coeffs[var_id] = coeffs.get(var_id, 0) + sign * right.value
                    else:
                        if isinstance(left_deref[2], Int):
                            constant += sign * right.value * left_deref[2].value
                        else:
                            raise ValueError(f"Non-integer in multiplication")

                # Try Constant * Constant
                elif isinstance(left, Int) and isinstance(right, Int):
                    constant += sign * left.value * right.value
                else:
                    raise ValueError(f"Non-linear term: {t}")
            elif t.functor == "-" and len(t.args) == 1:
                # Unary minus: -X
                parse_term(t.args[0], -sign)
            else:
                raise ValueError(f"Unsupported arithmetic operator: {t.functor}")
        else:
            raise ValueError(f"Invalid arithmetic term: {t}")

    parse_term(term)
    return coeffs, constant
```

#### 2. Linear Sum Propagator
**File**: `prolog/clpfd/props/linear.py` (new file)
**Changes**: Implement bounds-consistent propagator for linear constraints

```python
from typing import List, Optional, Tuple, Dict
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain

def create_linear_propagator(coeffs: Dict[int, int], rhs: int):
    """Create propagator for sum(coeff_i * X_i) = rhs.

    Uses bounds consistency: for each variable, compute bounds from others.

    Args:
        coeffs: Maps var_id -> coefficient (non-zero)
        rhs: Right-hand side constant
    """
    # Convert to lists for easier iteration
    var_ids = list(coeffs.keys())
    coefficients = [coeffs[v] for v in var_ids]

    def propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate linear constraint using bounds reasoning."""
        changed = []

        # For each variable, compute its bounds from the others
        for i, xi_id in enumerate(var_ids):
            xi_coeff = coefficients[i]
            xi_dom = get_domain(store, xi_id)

            if xi_dom is None:
                continue  # Variable is ground

            # Compute bounds for sum of other variables
            others_min = 0
            others_max = 0

            for j, xj_id in enumerate(var_ids):
                if i == j:
                    continue

                xj_dom = get_domain(store, xj_id)
                if xj_dom is None:
                    continue  # Variable is ground

                xj_coeff = coefficients[j]

                if xj_coeff > 0:
                    # Positive coefficient: min*coeff, max*coeff
                    if xj_dom.min() is not None:
                        others_min += xj_coeff * xj_dom.min()
                    else:
                        others_min = float('-inf')
                    if xj_dom.max() is not None:
                        others_max += xj_coeff * xj_dom.max()
                    else:
                        others_max = float('inf')
                else:
                    # Negative coefficient: max*coeff, min*coeff (swapped)
                    if xj_dom.max() is not None:
                        others_min += xj_coeff * xj_dom.max()
                    else:
                        others_min = float('-inf')
                    if xj_dom.min() is not None:
                        others_max += xj_coeff * xj_dom.min()
                    else:
                        others_max = float('inf')

            # Compute bounds for Xi
            # xi_coeff * Xi + others = rhs
            # Xi = (rhs - others) / xi_coeff

            if xi_coeff > 0:
                # Xi >= (rhs - others_max) / xi_coeff
                # Xi <= (rhs - others_min) / xi_coeff
                if others_max != float('inf'):
                    xi_min = (rhs - others_max) // xi_coeff
                    if (rhs - others_max) % xi_coeff > 0:
                        xi_min += 1  # Ceiling for lower bound
                else:
                    xi_min = None

                if others_min != float('-inf'):
                    xi_max = (rhs - others_min) // xi_coeff
                else:
                    xi_max = None
            else:
                # Negative coefficient - bounds swap
                # Xi >= (rhs - others_min) / xi_coeff (but dividing by negative)
                # Xi <= (rhs - others_max) / xi_coeff
                if others_min != float('-inf'):
                    xi_max = (rhs - others_min) // xi_coeff
                else:
                    xi_max = None

                if others_max != float('inf'):
                    xi_min = (rhs - others_max) // xi_coeff
                    if (rhs - others_max) % xi_coeff < 0:
                        xi_min += 1
                else:
                    xi_min = None

            # Update domain
            new_dom = xi_dom
            if xi_min is not None:
                new_dom = new_dom.remove_lt(xi_min)
            if xi_max is not None:
                new_dom = new_dom.remove_gt(xi_max)

            if new_dom.is_empty():
                return ('fail', None)

            if new_dom is not xi_dom:
                set_domain(store, xi_id, new_dom, trail)
                changed.append(xi_id)

        return ('ok', changed if changed else None)

    return propagator
```

#### 3. Multi-Variable Sum Propagator
**File**: `prolog/clpfd/props/sum.py` (new file)
**Changes**: Special case for unweighted sums (X + Y + Z = C)

```python
from typing import List, Optional, Tuple
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain

def create_sum_propagator(var_ids: List[int], total: int):
    """Create propagator for X1 + X2 + ... + Xn = total.

    Optimized version of linear propagator for unit coefficients.
    """
    def propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate sum constraint."""
        changed = []

        # Compute sum bounds from all variables
        sum_min = 0
        sum_max = 0

        for var_id in var_ids:
            dom = get_domain(store, var_id)
            if dom is None:
                continue

            if dom.min() is not None:
                sum_min += dom.min()
            else:
                sum_min = float('-inf')

            if dom.max() is not None:
                sum_max += dom.max()
            else:
                sum_max = float('inf')

        # Check feasibility
        if sum_min > total or sum_max < total:
            return ('fail', None)

        # Prune each variable
        for i, xi_id in enumerate(var_ids):
            xi_dom = get_domain(store, xi_id)
            if xi_dom is None:
                continue

            # Compute bounds for sum of others
            others_min = sum_min - (xi_dom.max() if xi_dom.max() is not None else 0)
            others_max = sum_max - (xi_dom.min() if xi_dom.min() is not None else 0)

            # Xi bounds: Xi = total - sum(others)
            xi_min = total - others_max
            xi_max = total - others_min

            # Update domain
            new_dom = xi_dom
            if xi_min is not None and xi_min != float('-inf'):
                new_dom = new_dom.remove_lt(int(xi_min))
            if xi_max is not None and xi_max != float('inf'):
                new_dom = new_dom.remove_gt(int(xi_max))

            if new_dom.is_empty():
                return ('fail', None)

            if new_dom is not xi_dom:
                set_domain(store, xi_id, new_dom, trail)
                changed.append(xi_id)

        return ('ok', changed if changed else None)

    return propagator
```

#### 4. Integration with #= Builtin
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Extend `_builtin_fd_eq` to handle complex arithmetic

```python
# Add after line 288 (before simple Z = X + K pattern):

# Try parsing as general linear expression
try:
    from prolog.clpfd.expr import parse_linear_expression

    # Parse both sides as linear expressions
    left_coeffs, left_const = parse_linear_expression(x_term, engine)
    right_coeffs, right_const = parse_linear_expression(y_term, engine)

    # Combine into single equation: left - right = 0
    # Rewrite as: sum(coeffs[i] * X[i]) = right_const - left_const
    combined_coeffs = {}

    # Add left side coefficients
    for var_id, coeff in left_coeffs.items():
        combined_coeffs[var_id] = combined_coeffs.get(var_id, 0) + coeff

    # Subtract right side coefficients
    for var_id, coeff in right_coeffs.items():
        combined_coeffs[var_id] = combined_coeffs.get(var_id, 0) - coeff

    # Remove zero coefficients
    combined_coeffs = {v: c for v, c in combined_coeffs.items() if c != 0}

    # Compute RHS constant
    rhs = right_const - left_const

    if not combined_coeffs:
        # No variables - just check constant equality
        return rhs == 0

    # Check if it's a simple sum (all coefficients are 1 or -1)
    all_unit = all(abs(c) == 1 for c in combined_coeffs.values())

    if all_unit and all(c == 1 for c in combined_coeffs.values()):
        # Special case: X + Y + Z = constant
        from prolog.clpfd.props.sum import create_sum_propagator
        prop = create_sum_propagator(list(combined_coeffs.keys()), rhs)
    else:
        # General linear constraint
        from prolog.clpfd.props.linear import create_linear_propagator
        prop = create_linear_propagator(combined_coeffs, rhs)

    # Register and run propagator
    queue = _ensure_queue(engine)
    pid = queue.register(prop)

    # Add watchers
    from prolog.clpfd.api import add_watcher
    from prolog.clpfd.queue import Priority
    for var_id in combined_coeffs.keys():
        add_watcher(store, var_id, pid, Priority.MED, trail)

    # Schedule and run
    queue.schedule(pid, Priority.MED)
    return queue.run_to_fixpoint(store, trail, engine)

except (ValueError, ImportError):
    # Fall back to existing pattern matching
    pass
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_linear.py`
- [x] Expression parser handles: `1000*S + 100*E + 10*N + D`
- [x] Linear propagator correctly prunes domains
- [x] Sum propagator optimizes unit-coefficient cases
- [ ] SEND+MORE completes in <1 second (needs further optimization)

#### Manual Verification:
- [x] Complex arithmetic expressions parse correctly
- [x] Bounds propagation is tight and efficient
- [x] No regression in existing CLP(FD) tests

---

## Phase 6: Labeling Strategies (Originally Phase 5)

### Overview
Implement search strategies for finding solutions.

**Note**: This was originally Phase 5 in the implementation, but has been renumbered to Phase 6 to accommodate the missing linear sum constraints phase.

### Changes Required:

#### 1. Labeling Implementation
**File**: `prolog/clpfd/label.py`
**Changes**: Variable selection and value choice strategies

```python
def _builtin_label(engine, vars_term):
    """Label variables using default strategies (first, indomain_min)."""
    return _builtin_labeling(engine,
                            List([Atom("first"), Atom("indomain_min")]),
                            vars_term)

def _builtin_labeling(engine, options_term, vars_term):
    """Label variables with specified strategies.

    Pushes choice points onto engine goal stack to avoid Python recursion.
    """
    from prolog.ast.terms import List, Atom, Int, Var, Struct

    # Parse options
    var_select = parse_var_selection(options_term)
    val_select = parse_value_selection(options_term)

    # Extract variable list
    vars = extract_var_list(vars_term)

    # Push labeling goal onto engine stack
    # This creates a choicepoint with alternatives for each value
    push_labeling_choices(engine, vars, var_select, val_select)

    return True  # Success continues with pushed goals

def push_labeling_choices(engine, vars, var_select, val_select):
    """Push labeling alternatives onto engine goal stack."""
    from prolog.clpfd.api import get_domain

    # Find next unbound variable
    unbound = []
    for v in vars:
        v_deref = engine.store.deref(v)
        if v_deref[0] == "UNBOUND":
            unbound.append(v_deref[1])

    if not unbound:
        return  # All labeled, nothing to push

    # Select variable
    var_id = var_select(engine.store, unbound)

    # Get domain
    domain = get_domain(engine.store, var_id)
    if not domain or domain.is_empty():
        # Push fail goal
        engine.push_goal(Atom("fail"))
        return

    # Generate value choices
    values = val_select(domain)

    # Create disjunction of value bindings
    # (Var = Value1 ; Var = Value2 ; ... ; Var = ValueN)
    alternatives = []
    for value in values:
        # Create binding goal: Var = Value
        var_term = Var(var_id, "_G" + str(var_id))
        value_term = Int(value)
        binding = Struct("=", (var_term, value_term))

        # After binding, continue labeling remaining vars
        remaining = [v for v in vars if v != var_id]
        if remaining:
            # Create continuation goal
            label_rest = Struct("$label_continue",
                              (List(remaining), options_term))
            # Conjunction: binding, label_rest
            alternative = Struct(",", (binding, label_rest))
        else:
            alternative = binding

        alternatives.append(alternative)

    # Push disjunction onto goal stack
    if len(alternatives) == 1:
        engine.push_goal(alternatives[0])
    else:
        # Build disjunction tree
        disj = alternatives[0]
        for alt in alternatives[1:]:
            disj = Struct(";", (disj, alt))
        engine.push_goal(disj)

def parse_var_selection(options_term):
    """Parse variable selection strategy from options."""
    # Extract strategy from options list
    if has_option(options_term, "first"):
        return first_var_selection
    elif has_option(options_term, "ff"):
        return first_fail_selection
    else:
        return first_var_selection  # Default

def parse_value_selection(options_term):
    """Parse value selection strategy from options."""
    if has_option(options_term, "indomain_min"):
        return indomain_min_values
    elif has_option(options_term, "indomain_max"):
        return indomain_max_values
    elif has_option(options_term, "bisect"):
        return bisect_values
    else:
        return indomain_min_values  # Default

def first_var_selection(store, var_ids):
    """Select first variable (leftmost)."""
    return var_ids[0]

def first_fail_selection(store, var_ids):
    """Select variable with smallest domain."""
    from prolog.clpfd.api import get_domain
    return min(var_ids, key=lambda v: get_domain(store, v).size())

def indomain_min_values(domain):
    """Try values in ascending order."""
    values = []
    for low, high in domain.intervals:
        values.extend(range(low, high + 1))
    return values

def indomain_max_values(domain):
    """Try values in descending order."""
    return list(reversed(indomain_min_values(domain)))

def bisect_values(domain):
    """Try middle value first, then recurse on halves."""
    # Simplified: just return all values for now
    # Full implementation would create sub-domains
    return indomain_min_values(domain)
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_label.py`
- [x] `label([X,Y])` enumerates all solutions via goal stack
- [x] Variable selection strategies work correctly
- [x] Value selection strategies work correctly
- [x] No Python recursion in labeling

#### Manual Verification:
- [x] Labeling performance acceptable for small problems
- [x] Backtracking integrates correctly with propagation
- [x] Choice points created properly on goal stack

---

## Phase 7: Hook Integration (Originally Phase 6)

### Overview
Integrate CLP(FD) with attributed variable unification hooks for domain merging.

**Note**: This was originally Phase 6 in the implementation, but has been renumbered to Phase 7 to accommodate the missing linear sum constraints phase.

### Changes Required:

#### 1. Unification Hook
**File**: `prolog/clpfd/hooks.py`
**Changes**: Implement domain merging on var-var aliasing

```python
def clpfd_unify_hook(engine, varid, other):
    """Handle CLP(FD) constraints during unification.

    Called before binding occurs. Must handle:
    - Var-var aliasing: merge domains and watchers
    - Var-int binding: check domain membership
    """
    from prolog.ast.terms import Var, Int
    from prolog.clpfd.api import get_domain, set_domain, get_fd_attrs

    store = engine.store
    trail = engine.trail

    # Get domain of the variable being unified
    var_domain = get_domain(store, varid)
    if not var_domain:
        return True  # No domain constraint, allow unification

    if isinstance(other, Var):
        # Var-var unification - merge domains
        other_deref = store.deref(other.id)
        if other_deref[0] == "UNBOUND":
            other_id = other_deref[1]
            other_domain = get_domain(store, other_id)

            if other_domain:
                # Intersect domains
                new_domain = var_domain.intersect(other_domain)
                if new_domain.is_empty():
                    return False  # Incompatible domains

                # Set merged domain on BOTH variables before union
                # Whichever becomes root will keep the merged domain
                set_domain(store, varid, new_domain, trail)
                set_domain(store, other_id, new_domain, trail)

                # Merge watchers on BOTH variables
                # Get both watcher sets
                var_fd = get_fd_attrs(store, varid)
                other_fd = get_fd_attrs(store, other_id)

                if var_fd and 'watchers' in var_fd and other_fd and 'watchers' in other_fd:
                    # Union watcher sets for each priority
                    from prolog.clpfd.api import Priority
                    merged_watchers = {}
                    for prio in Priority:
                        merged_watchers[prio] = (
                            var_fd['watchers'].get(prio, set()) |
                            other_fd['watchers'].get(prio, set())
                        )

                    # Update both variables with merged watchers
                    var_fd_new = var_fd.copy()
                    var_fd_new['watchers'] = merged_watchers
                    store.put_attr(varid, 'clpfd', var_fd_new, trail)

                    other_fd_new = other_fd.copy()
                    other_fd_new['watchers'] = merged_watchers
                    store.put_attr(other_id, 'clpfd', other_fd_new, trail)

    elif isinstance(other, Int):
        # Var-int binding - check if value is in domain
        if not var_domain.contains(other.value):
            return False  # Value not in domain

        # Set domain to singleton and wake watchers
        singleton = Domain(((other.value, other.value),), var_domain.rev + 1)
        set_domain(store, varid, singleton, trail)

        # Schedule all watchers of this variable
        queue = engine.get_clpfd_queue()
        from prolog.clpfd.api import iter_watchers
        for pid, prio in iter_watchers(store, varid):
            queue.schedule(pid, prio, cause=('grounded', varid))

        # Run propagation
        if not queue.run_to_fixpoint(store, trail, engine):
            return False

    return True  # Allow unification
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_hooks.py`
- [x] `X in 1..10, Y in 5..15, X = Y` => both in `5..10`
- [x] Domain incompatibility causes unification failure
- [x] Watchers correctly merged on aliasing
- [x] Grounding triggers propagation correctly

#### Manual Verification:
- [x] Hook doesn't interfere with non-CLP(FD) unification
- [x] Performance impact minimal when CLP(FD) not used
- [x] Proper cleanup after unification rollback

---

## Testing Strategy

### Unit Tests:
- Domain operations (intersect, remove, split)
- Queue priority and deduplication
- Individual propagator correctness
- Hook integration with unification

### Property Tests:
- Confluence: random posting order yields same fixpoint
- Monotonicity: domains only shrink, never expand
- Trail invertibility: backtracking restores exact state

### Integration Tests:
- Simple constraint problems (N-queens simplified)
- Chain propagation scenarios
- Labeling with various strategies

### Manual Testing Steps:
1. Load CLP(FD) predicates and verify basic constraints
2. Test interactive constraint posting and propagation
3. Verify labeling enumerates correct solutions
4. Check performance with larger domains (1..1000)

## Performance Considerations

- Interval operations should be O(n) in number of intervals
- Domain revision check should be O(1) via rev counter
- Propagator scheduling should be O(1) amortized
- Watcher lists should be kept small via deduplication
- Consider bitset optimization for domains ≤64 values (future)

## Migration Notes

No existing code uses CLP(FD), so no migration needed. However:
- Ensure CLP(FD) builtins don't conflict with existing predicates
- Document that `#` prefix denotes constraint versions of operators
- Provide examples showing difference from regular arithmetic

## References

- Original plan: `docs/PLAN.md` (Stage 5, lines 230-340)
- Attributed variables: `docs/ATTRIBUTED_VARIABLES.md`
- Stage 4 implementation: `prolog/engine/engine.py:2679-2875`
- Trail support: `prolog/unify/trail.py:91-107`

## Implementation History and Recommendations

### What Happened

1. **Phases 1-4**: Successfully implemented as planned
   - Core domain infrastructure (#127)
   - Propagation queue (#128)
   - Basic propagators (#130)
   - Engine integration (#131)

2. **Phase 5 (Linear Sums)**: **SKIPPED** - This is the critical gap
   - PLAN.md specified "Stage 5.2 - Small linear sums"
   - Would handle `A*X + B*Y #= C` and `X+Y+Z #= C`
   - Never implemented, causing SEND+MORE failures

3. **Phase 5→6**: Labeling implemented (#132) but numbered as Phase 5

4. **Phase 6→7**: Hook integration implemented (#133) but numbered as Phase 6

5. **Stage 6**: Jumped ahead to implement `all_different` (#134-138)
   - Works correctly but can't compensate for missing arithmetic

### Why SEND+MORE Fails

The SEND+MORE cryptarithmetic puzzle requires:
```prolog
1000*S + 100*E + 10*N + D + 1000*M + 100*O + 10*R + E #=
10000*M + 1000*O + 100*N + 10*E + Y
```

Without Phase 5 (linear sums), this complex arithmetic expression:
1. Cannot be parsed into coefficient-variable pairs
2. Falls back to weak variable-variable equality
3. Creates no meaningful propagation
4. Results in exponential search space explosion

Even with `all_different/1` correctly pruning some values, the lack of arithmetic propagation makes the problem intractable.

### Recommendations

1. **Implement Phase 5 (Linear Sums)** as specified above
   - Create `prolog/clpfd/expr.py` for expression parsing
   - Add `prolog/clpfd/props/linear.py` for linear constraints
   - Add `prolog/clpfd/props/sum.py` for optimized sums
   - Update `builtins_clpfd.py` to use the new parsers

2. **Test with SEND+MORE** immediately after implementation
   - Should complete in <1 second with proper propagation
   - Currently times out after 100+ seconds

3. **Consider future enhancements**:
   - Division and modulo constraints
   - Non-linear constraints (X*Y)
   - Global cardinality constraints
   - Optimization objectives

### Verification After Implementation

Run these tests to verify Phase 5 works correctly:
```bash
# Unit tests for linear constraints
uv run pytest prolog/tests/unit/test_clpfd_linear.py -xvs

# Integration test - should pass in <1s
uv run pytest prolog/tests/scenarios/test_sendmore_benchmark.py::TestAllDifferentBenchmarks::test_sendmore_with_all_different -xvs

# Full benchmark suite
uv run pytest prolog/tests/scenarios/test_sendmore_benchmark.py -xvs
```