# PyLog Performance Guide

## Overview

This guide covers performance considerations and optimization strategies for PyLog applications.

## Indexing

### First-Argument Indexing

PyLog automatically indexes clauses by their first argument for faster clause selection:

```python
# Good: First argument varies, enabling indexing
fact(a, 1).
fact(b, 2).
fact(c, 3).

# Query uses index to jump directly to matching clause
?- fact(b, X).  # Only examines fact(b, 2)
```

```python
# Poor: First argument identical, no indexing benefit
process(data, a, 1).
process(data, b, 2).
process(data, c, 3).

# Query must try all clauses
?- process(data, b, X).  # Examines all three clauses
```

### Enabling/Disabling Indexing

```python
# Enable indexing (default)
engine = Engine(program, use_indexing=True)

# Disable for debugging or small programs
engine = Engine(program, use_indexing=False)
```

## Tracing Performance

### Minimize Trace Overhead

```python
# Most efficient: No tracing
engine = Engine(program, trace=False)

# Selective tracing with spypoints
engine = Engine(program, trace=True)
engine.tracer.spy('bottleneck/2')  # Only trace specific predicates

# Full tracing (slowest)
engine = Engine(program, trace=True)
engine.tracer.config.trace_all = True
```

### Optimize Trace Output

```python
# Fast: Minimal bindings
config = TracerConfig(bindings_policy='none')

# Medium: Variable names only
config = TracerConfig(bindings_policy='names')

# Slow: Full bindings with values
config = TracerConfig(bindings_policy='names_values')
```

### Efficient Sink Selection

```python
# Fast: Batched JSONL output
sink = JSONLTraceSink('trace.jsonl', batch_size=1000)

# Medium: Memory collection (bounded)
sink = MemoryTraceSink(max_events=10000)

# Slow: Pretty printing to terminal
sink = PrettyTraceSink(sys.stdout)
```

## Query Optimization

### Clause Ordering

Place most selective clauses first:

```prolog
% Good: Fail fast on common case
validate(X) :- integer(X), X > 0, expensive_check(X).

% Poor: Expensive check runs even for invalid input
validate(X) :- expensive_check(X), integer(X), X > 0.
```

### Cut Usage

Use cut to prevent unnecessary backtracking:

```prolog
% Good: Stop after first match
first([H|_], H) :- !.
first([_|T], X) :- first(T, X).

% Poor: Creates unnecessary choicepoints
first([H|_], H).
first([_|T], X) :- first(T, X).
```

### Tail Recursion

Structure recursive predicates for tail-call optimization:

```prolog
% Good: Tail recursive with accumulator
sum_list(List, Sum) :- sum_list(List, 0, Sum).
sum_list([], Acc, Acc).
sum_list([H|T], Acc, Sum) :-
    Acc1 is Acc + H,
    sum_list(T, Acc1, Sum).

% Poor: Not tail recursive
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Sum1),
    Sum is Sum1 + H.
```

## Memory Management

### Choicepoint Pruning

```prolog
% Use once/1 to limit solutions
?- once(member(X, [1,2,3,4,5])).

% Use cut to prune choicepoints
process(Data) :-
    validate(Data), !,
    transform(Data).
```

### Trail Management

The trail records all bindings for backtracking:

```python
# Monitor trail size during execution
if engine.tracer:
    engine.tracer.config.enable_internal_events = True
    # Internal events show trail operations
```

## Benchmarking

### Simple Timer

```python
import time

start = time.perf_counter()
solutions = list(engine.run(goals))
elapsed = time.perf_counter() - start
print(f"Query took {elapsed:.3f} seconds")
```

### Trace Analysis

```python
from prolog.debug.sinks import MemoryTraceSink
from collections import Counter

sink = MemoryTraceSink()
engine.tracer.add_sink(sink)

solutions = list(engine.run(goals))

# Count predicate calls
calls = Counter(e.pred_id for e in sink.events if e.port == Port.CALL)
print("Most called predicates:", calls.most_common(5))

# Find deepest recursion
max_depth = max(e.frame_depth for e in sink.events)
print(f"Max frame depth: {max_depth}")
```

## Common Bottlenecks

### 1. Excessive Backtracking

**Symptom**: Many REDO events in trace

**Solution**: Add cuts or reorder clauses

### 2. Deep Recursion

**Symptom**: High frame_depth or goal_height in trace

**Solution**: Convert to tail recursion with accumulator

### 3. Large Lists

**Symptom**: Slow list operations

**Solution**: Use difference lists or DCGs

```prolog
% Slow: Repeated append
build_list([], []).
build_list([H|T], Result) :-
    build_list(T, Rest),
    append([H], Rest, Result).

% Fast: Difference lists
build_list(List, Result) :-
    build_list_dl(List, Result-[]).

build_list_dl([], X-X).
build_list_dl([H|T], [H|R]-X) :-
    build_list_dl(T, R-X).
```

### 4. Repeated Computation

**Symptom**: Same goal computed multiple times

**Solution**: Use tabling (memoization) or restructure query

```prolog
% Slow: Fibonacci without memoization
fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1, N2 is N - 2,
    fib(N1, F1), fib(N2, F2),
    F is F1 + F2.

% Fast: With accumulator pattern
fib(N, F) :- fib_acc(N, 0, 1, F).
fib_acc(0, A, _, A).
fib_acc(N, A, B, F) :-
    N > 0,
    N1 is N - 1,
    C is A + B,
    fib_acc(N1, B, C, F).
```

## Profiling Workflow

1. **Establish Baseline**: Run without tracing
2. **Add Selective Tracing**: Use spypoints on suspect predicates
3. **Analyze Patterns**: Look for excessive calls, deep recursion
4. **Apply Optimizations**: Indexing, cuts, tail recursion
5. **Verify Improvements**: Compare before/after metrics

## Best Practices

1. **Start Simple**: Don't optimize prematurely
2. **Measure First**: Use tracing to identify actual bottlenecks
3. **Test Correctness**: Ensure optimizations preserve semantics
4. **Document Trade-offs**: Note why optimizations were applied
5. **Profile Realistically**: Use representative data sets

## Configuration Checklist

For maximum performance:

```python
engine = Engine(
    program,
    use_indexing=True,      # Enable clause indexing
    trace=False,            # Disable tracing in production
    occurs_check=False      # Disable occurs check if safe
)
```

For debugging:

```python
engine = Engine(
    program,
    use_indexing=True,      # Keep realistic performance
    trace=True              # Enable tracing
)

# Configure minimal overhead tracing
engine.tracer.config = TracerConfig(
    trace_builtins=False,   # Skip builtin tracing
    bindings_policy='none', # No variable bindings
    enable_timestamps=False # No timestamps
)

# Use selective spypoints
engine.tracer.spy('problem_predicate/2')
```

## See Also

- [ARCH.md](ARCH.md) - Architecture and design decisions
- [DEBUG_API.md](DEBUG_API.md) - Tracing and debugging tools
- [TRACE_FORMAT.md](TRACE_FORMAT.md) - Trace event details