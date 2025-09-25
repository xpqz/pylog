# Stage 5 CLP(FD) Implementation Guardrails

## Critical Safety Checklist

### 🔴 Attribute Immutability
```python
# ❌ WRONG - Mutates in place, breaks trailing
fd_attrs = store.get_attrs(varid)['clpfd']
fd_attrs['domain'] = new_domain  # BAD!

# ✅ CORRECT - Replace entire value
fd_attrs = store.get_attrs(varid).get('clpfd', {}).copy()
fd_attrs['domain'] = new_domain
store.put_attr(varid, 'clpfd', fd_attrs, trail)  # Trails the change
```

**Rule**: Never mutate `attrs['clpfd']` or its contents. Always create new dict/frozenset and replace via `put_attr`.

### 🔴 Domain Revision Semantics
```python
# ❌ WRONG - Always bumps rev
def intersect(self, other):
    new_intervals = compute_intersection(...)
    return Domain(new_intervals, max(self.rev, other.rev) + 1)  # BAD!

# ✅ CORRECT - Only bumps on change
def intersect(self, other):
    new_intervals = compute_intersection(...)
    if new_intervals == self.intervals:
        return self  # No change, same object
    return Domain(new_intervals, max(self.rev, other.rev) + 1)
```

**Rule**: Bump `rev` only on strict narrowing. Return `self` when unchanged.

### 🔴 Var-Var Aliasing Hook
```python
# ❌ WRONG - Only updates one side
set_domain(store, varid, merged_domain, trail)

# ✅ CORRECT - Updates BOTH before union
set_domain(store, varid, merged_domain, trail)
set_domain(store, other_id, merged_domain, trail)
# Also merge watchers on BOTH
```

**Rule**: Set identical `clpfd` attrs on BOTH variables before union. Winner keeps it, loser's is discarded.

### 🔴 Watcher Storage
```python
# Data structure (immutable)
attrs['clpfd'] = {
    'domain': Domain(...),  # Immutable Domain object
    'watchers': {
        Priority.HIGH: frozenset([pid1, pid2]),
        Priority.MED: frozenset([pid3]),
        Priority.LOW: frozenset()
    }
}
```

**Rule**: Watchers as frozensets, always on root after deref, dedup by pid.

### 🔴 Queue Self-Requeue
```python
class PropagationQueue:
    def __init__(self):
        self.running = None
        self.reschedule = set()  # Collect self-wakes during run

    def schedule(self, pid, priority, cause=None):
        if pid == self.running:
            self.reschedule.add((pid, priority))  # Defer
            return
        # ... normal scheduling

    def run_to_fixpoint(self, ...):
        # After prop.run()
        for pid, prio in self.reschedule:
            self.schedule(pid, prio)  # Now safe to requeue
        self.reschedule.clear()
```

**Rule**: Don't drop self-requeues. Defer until after current run completes.

### 🔴 Failure Propagation
```python
# ❌ WRONG - Throws exception
if domain.is_empty():
    raise ConstraintFailure()  # BAD!

# ✅ CORRECT - Return failure signal
if domain.is_empty():
    return ('fail', None)  # Queue stops, returns False
```

**Rule**: Signal failure in-band. Let engine backtracking handle trail unwinding.

### 🔴 When to Run Queue
```python
# ✅ After CLP(FD) builtins
def _builtin_constraint_eq(engine, x, y):
    success = post_constraint(engine, '#=', x_var, y_var)
    if success:
        success = engine.get_clpfd_queue().run_to_fixpoint(...)
    return success

# ❌ Inside unify hook (reentrancy risk)
def clpfd_unify_hook(engine, varid, other):
    # ... domain merge ...
    queue.run_to_fixpoint(...)  # RISKY!
```

**Rule**: Run queue after builtins, not inside unification hooks.

### 🔴 Less-Than Bounds
```python
# X #< Y means X.max < Y.max AND Y.min > X.min
# ✅ CORRECT
new_x = x_dom.remove_ge(y_dom.max())    # X values must be < Y.max
new_y = y_dom.remove_le(x_dom.min())    # Y values must be > X.min

# X #=< Y means X.max <= Y.max AND Y.min >= X.min
# ✅ CORRECT
new_x = x_dom.remove_gt(y_dom.max())    # X values must be <= Y.max
new_y = y_dom.remove_lt(x_dom.min())    # Y values must be >= X.min
```

**Rule**: Test bounds propagation with tight intervals like `X in 5..5, Y in 5..5`.

### 🔴 Labeling Without Recursion
```python
# ❌ WRONG - Python recursion
def label_vars(engine, vars, ...):
    # ... select var and value ...
    if unify_with_int(engine, var, value):
        return label_vars(engine, rest, ...)  # BAD!

# ✅ CORRECT - Push to goal stack
def push_labeling_choices(engine, vars, ...):
    # Build disjunction: (X=1 ; X=2 ; X=3)
    alternatives = [Struct("=", (var, Int(val))) for val in values]
    disjunction = build_disjunction_tree(alternatives)
    engine.push_goal(disjunction)
```

**Rule**: Push choices as goals, let engine handle backtracking.

## Quick Implementation Order

1. **Immutable Domain class** with proper `rev` semantics
2. **FD attribute helpers** that maintain immutability
3. **Queue with reschedule set** and priority escalation
4. **Unify hook** that merges on BOTH sides
5. **Basic propagators** returning `('ok'/'fail', changes)`
6. **Labeling as goal-pushing builtin**

## Testing Guards

### Backtracking Test
```python
# Post constraint in failing branch
mark = trail.mark()
post_constraint(...)
undo_to(mark, trail, store)
# Assert: domains/watchers fully restored
```

### Confluence Test
```python
# Random posting order, same result
for seed in range(100):
    shuffle(constraints, seed)
    post_all(constraints)
    snapshot = canonical_snapshot(store)  # Deref to roots, ignore rev
    assert snapshot == expected
```

### Memory Test
```python
# No leak on repeated post/backtrack
for _ in range(1000):
    mark = trail.mark()
    post_many_constraints()
    undo_to(mark, trail, store)
assert store.attrs == {}  # All cleaned up
```