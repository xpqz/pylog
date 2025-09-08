# Debugging Epic: First-Class Tracing Infrastructure

## Overview
Implement structured, machine-parseable tracing for the PyLog engine to enable reproducible debugging, CI integration, and confident validation of ISO semantics. The tracing system should have zero overhead when disabled and provide comprehensive visibility into engine execution.

## Goals
- Create NDJSON-based structured trace output
- Enable selective tracing with minimal performance impact
- Add runtime invariant checking with panic dumps
- Provide human-readable trace analysis tools
- Integrate tracing into the test suite for regression detection

## Implementation Plan

### Phase 1: Core Infrastructure

#### 1.1 Structured Trace Format (NDJSON)
Implement single-line JSON output per trace event with core fields:

```json
{
  "ts": 1736375,            // monotonic step counter
  "phase": "run|backtrack|throw",
  "port": "CALL|EXIT|REDO|FAIL", // if applicable
  "pred": "foo/2",          // or null for CONTROL/builtins
  "goal_id": 1234,          // monotonic
  "frame_h": 7,             // frame stack height
  "cp_h": 5,                // choicepoint stack height
  "goal_h": 42,             // goal stack height
  "stamp": 911,             // trail current stamp
  "ev": "cp_push|cp_pop|trail_write|trail_unwind|catch_begin|catch_switch|catch_end|cut|unify",
  "payload": {...}          // event-specific data
}
```

#### 1.2 Event Types and Payloads

**Choicepoint events:**
- `cp_push/cp_pop`: `{ "kind":"PRED|DISJ|CATCH|ITE", "owner_frame": 7, "phase":"GOAL|RECOVERY", "next": 2 }`

**Trail events:**
- `trail_write`: `{ "var": 123, "old_stamp": 900, "new_stamp": 911 }`
- `trail_unwind`: `{ "from_pos": 100, "to_pos": 50 }`

**Catch/throw events:**
- `catch_begin`: `{ "catch_id": 999, "baselines": {...} }`
- `catch_switch`: `{ "to": "RECOVERY", "catch_id": 999 }`
- `catch_end`: `{ "catch_id": 999, "result": "success|fail" }`

**Control flow:**
- `cut`: `{ "barrier_cp_h": 4, "pruned": 3 }`
- `unify`: `{ "lhs": "X", "rhs": "a", "result": true }` (optional at level 3)

#### 1.3 Trace Levels and Filtering

Environment variable: `PYLOG_TRACE=ports,cp,trail,catch` or `off`

**Levels:**
- Level 0: `off` (default) - no tracing
- Level 1: `ports` - CALL/EXIT/REDO/FAIL only
- Level 2: `+cp,throw,cut` - add choicepoint and control flow
- Level 3: `+trail,unify` - add detailed trail and unification

### Phase 2: Performance Optimization

#### 2.1 Zero-Overhead When Disabled
```python
class Trace:
    __slots__ = ("fp", "mask", "enabled", "ring", "ring_max", "step")
    
    def emit(self, ev, **kw):
        if not self.enabled: return  # Fast path
        # ... actual emission
```

#### 2.2 Efficient Storage
- Use `__slots__` for TraceEvent to avoid dict overhead
- Direct JSON serialization: `fp.write(json.dumps(...) + "\n")`
- Optional ring buffer (50k events) for memory-constrained environments
- Dump on failure for post-mortem analysis

### Phase 3: Runtime Invariants

#### 3.1 Fast Invariants (every step)
```python
assert len(self.frame_stack) >= (self.cp_stack[-1].frame_height if cp_stack else 0)
assert self.trail.current_stamp == (self.cp_stack[-1].stamp if cp_stack else self.trail.current_stamp)
```

#### 3.2 Expensive Invariants (every N steps)
- Stack monotonicity checks
- No dangling POP_FRAME sentinels
- No CP above cut barrier
- Trail consistency validation

#### 3.3 Panic Dumps
On assertion failure:
- Dump current stack states
- Export last 2000 trace events
- Save to CI artifact file

### Phase 4: Analysis Tools

#### 4.1 CLI Tool: `scripts/pylog-trace`
```bash
# Filter NDJSON traces
pylog-trace trace.jsonl --ports --pred foo/2 --since-ts 1000

# Pretty-print ports
# Output: #1234 CALL foo/2 [F7 CP5 G42 σ911]
pylog-trace trace.jsonl --pretty
```

#### 4.2 Test Integration
```python
@pytest.fixture
def trace_to(tmp_path, mask="ports,cp"):
    """Fixture to capture traces during tests."""
    # Enable tracing with specified mask
    # Return path to trace file
```

#### 4.3 Golden Trace Tests
Create reference traces for critical scenarios:
- Simple backtracking
- `catch((p(X); throw(t)), t, r(Y))`
- Cut committing within recovery (ISO)
- Nested catch with multiple handlers

### Phase 5: Extended Features

#### 5.1 Builtin and Meta Tracing
- Emit `builtin` event: `{ "name": "arg/3", "det": true }`
- Meta-predicates: `meta_enter/meta_exit` with wrapped goal ID
- Track `call/1`, `once/1` execution

#### 5.2 "Explain One Solution" Mode
Debug flag that records only the path for the first solution:
- Ports traversed
- CP transitions taken
- Key trail writes
- Useful for CI artifacts and quick debugging

### Phase 6: Future Extensions

#### 6.1 FD/Constraint Tracing
Design for future constraint debugging:
- `enqueue`: `{ "prop": "sum/3", "priority": 2 }`
- `propagate`: `{ "prop": "sum/3", "changes": 3 }`
- `reduce_domain`: `{ "var": 123, "from": "[1..10]", "to": "[1..5]", "reason": "sum/3" }`
- `fail`: `{ "reason": "empty domain", "var": 123 }`

Keep behind `PYLOG_TRACE=fd` flag.

## Implementation Order

### Sprint 1: Minimal Viable Tracing
1. Land NDJSON emitter with ports/cp/catch events
2. Add pytest fixture for per-test trace capture
3. Convert 2-3 tests to assert golden traces

### Sprint 2: Invariants and Analysis
1. Add fast invariants with panic dump
2. Implement `pylog-trace` CLI tool
3. Add trace-based property tests

### Sprint 3: Extended Coverage
1. Add trail/unify events (when needed for debugging)
2. Implement builtin/meta tracing
3. Add "explain one solution" mode

## Success Metrics
- Zero performance impact when tracing disabled
- Machine-diffable traces for CI regression detection
- Quick triage of enumeration issues via CP pop/redo analysis
- Confident validation of ISO semantics (e.g., Recovery's cut behavior)
- Early warning on regressions through invariants and golden traces

## Code Integration Points

### Engine modifications needed:
```python
# In Engine.__init__
self.trace = Trace(mask=os.environ.get('PYLOG_TRACE', 'off'))

# At CP creation
cp.stamp = self.trail.next_stamp()
self.trace.emit("cp_push", kind=cp.kind, phase=cp.payload.get("phase"))

# On throw caught
self.trace.emit("catch_switch", to="RECOVERY", catch_id=catch_frame["catch_id"])

# On cut
self.trace.emit("cut", barrier_cp_h=barrier, pruned=len(self.cp_stack)-barrier)

# On trail write
self.trace.emit("trail_write", var=varid, old_stamp=old, new_stamp=new)
```

## Expected Benefits
- **Reproducible debugging**: Every bug can be traced and replayed
- **CI confidence**: Golden traces catch semantic regressions
- **Performance visibility**: See exactly where enumeration stops
- **ISO compliance**: Validate cut/catch/backtracking semantics
- **Future-proof**: Foundation for FD and constraint debugging

## Notes
- Keep trace events small and focused
- Use enums for event types to enable switch statements in analysis tools
- Consider binary format (MessagePack) for production if JSON becomes too large
- Ring buffer mode useful for long-running queries where full trace would be massive