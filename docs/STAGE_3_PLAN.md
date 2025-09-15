# Stage 3: Debug & Observability Implementation Plan

*Comprehensive debugging, tracing, and observability infrastructure for understanding and reproducing Prolog execution behavior.*

---

## Overview

Stage 3 introduces a complete debug and observability layer that allows developers to see the machine at work, understand execution flow, and reproduce failures reliably. The implementation provides both human-readable and machine-parseable outputs while maintaining minimal performance overhead when disabled.

## Goals

1. **Complete visibility** into engine execution via ports tracer
2. **Reproducible debugging** through deterministic traces and snapshots
3. **Performance analysis** via metrics and counters
4. **Low overhead** when tracing is disabled (≤5%)
5. **Machine-readable output** for tooling and analysis
6. **Integration points** for CI and debugging workflows

## Trace Invariants

The tracer maintains strict invariants that tests will verify:

1. **step_id** strictly increases by 1 per emitted event
2. **frame_depth** == len(frame_stack) at all times
3. **cp_depth** == len(cp_stack) at all times  
4. **goal_height** == len(goal_stack) at all times
5. **Port sequences** follow valid transitions:
   - Successful goal: CALL → EXIT
   - Failed goal: CALL → FAIL
   - Non-deterministic: CALL → EXIT → REDO → (EXIT|FAIL) → ...
6. **Determinism**: With timestamps disabled, identical runs produce byte-for-byte identical traces

## Architecture

### Core Components

#### 1. Ports Tracer System
```python
class TraceEvent:
    """Single trace event with all metadata."""
    version: int = 1          # Schema version
    run_id: str               # UUID for this query run
    step_id: int              # Global monotonic counter
    port: str                 # 'call'|'exit'|'redo'|'fail'
    goal: Term                # Current goal being traced
    goal_pretty: str          # Pretty-printed goal
    goal_canonical: str       # Canonical form for parsing
    frame_depth: int          # Frame stack depth (not recursion!)
    cp_depth: int             # Choicepoint stack depth
    goal_height: int          # Goal stack height
    write_stamp: int          # Write stamp from engine
    pred_id: str              # Interned "name/arity"
    bindings: Optional[Dict]  # Variable bindings (configurable)
    monotonic_ns: Optional[int]  # For timing (humans only)

class InternalEvent(TraceEvent):
    """Extended debug events (optional, off by default)."""
    kind: str  # 'cp_push'|'cp_pop'|'frame_push'|'frame_pop'|'cut_commit'|'catch_switch'
    details: Dict  # Event-specific data

class PortsTracer:
    """Main tracer managing events and output."""
    def __init__(self, engine: Engine):
        self.engine = engine
        self.step_counter = 0
        self.run_id = str(uuid.uuid4())
        self.filters = TraceFilters()
        self.sinks = []
        self.spypoints = set()  # Set of (name, arity) tuples
        self.bindings_policy = 'none'  # 'none'|'names'|'names_values'
        self.max_term_depth = 4
        self.max_items_per_list = 10
        self.enable_internal_events = False
    
    def emit_event(self, port: str, goal: Term, **metadata):
        """Emit a trace event through configured sinks."""
```

#### 2. Snapshot System
```python
@dataclass
class EngineSnapshot:
    """Complete engine state at a point in time."""
    run_id: str
    step_id: int
    # Heights and tops
    store_size: int
    trail_length: int
    trail_top: int
    goal_height: int
    goal_top: int
    frame_height: int
    frame_top: int
    cp_depth: int
    cp_top: int
    catch_stack_depth: int
    write_stamp: int
    # Detailed structures
    choicepoints: List[CPSnapshot]
    frames: List[FrameSnapshot]
    # Metrics
    candidates_considered: int  # From indexing
    memory_bytes: Optional[int]

@dataclass
class CPSnapshot:
    """Choicepoint details."""
    kind: str  # 'choice'|'cut_barrier'|'catch'
    goal_height: int
    frame_height: int
    trail_top: int
    stamp: int
    pred_id: str

@dataclass
class FrameSnapshot:
    """Frame details."""
    frame_id: int
    pred_id: str
    goal_height_at_entry: int

class SnapshotManager:
    """Manage snapshots and diffs."""
    def snapshot(self, engine: Engine) -> EngineSnapshot:
        """Capture current engine state."""
    
    def diff(self, before: EngineSnapshot, after: EngineSnapshot) -> SnapshotDiff:
        """Compute typed difference between snapshots."""

@dataclass
class SnapshotDiff:
    """Structured diff between snapshots."""
    heights: Dict[str, int]  # Deltas for each height
    trail: Dict[str, Any]    # Trail changes
    cp: Dict[str, Any]       # {"delta": +N, "popped": [...]}
    frames: Dict[str, Any]   # Frame changes
```

#### 3. Metrics & Counters
```python
@dataclass
class PredMetrics:
    """Per-predicate metrics."""
    calls: int = 0
    exits: int = 0
    fails: int = 0
    redos: int = 0
    unifications: int = 0
    backtracks: int = 0

class EngineMetrics:
    """Performance and behavior metrics."""
    # Global counters
    unifications_attempted: int = 0
    unifications_succeeded: int = 0
    backtracks_taken: int = 0
    cuts_executed: int = 0
    alternatives_pruned: int = 0
    exceptions_thrown: int = 0
    exceptions_caught: int = 0
    candidates_considered: int = 0  # From indexing
    candidates_yielded: int = 0     # From indexing
    
    # Per-predicate
    per_pred: Dict[str, PredMetrics] = field(default_factory=dict)
    
    def reset(self):
        """Reset all counters for new query."""
        # Clear all fields
    
    def to_dict(self) -> Dict[str, Any]:
        """Export metrics as dictionary."""
```

#### 4. Output Sinks with Backpressure
```python
class TraceSink(ABC):
    """Abstract base for trace output destinations."""
    def __init__(self, buffer_size: int = 1000):
        self.buffer = collections.deque(maxlen=buffer_size)
        self.events_dropped_total = 0
        self.events_dropped_recent = 0
        self.drop_reason = None
    
    @abstractmethod
    def write_event(self, event: TraceEvent):
        """Write event with backpressure handling."""

class PrettyTraceSink(TraceSink):
    """Human-readable single-line trace output."""
    def write_event(self, event: TraceEvent):
        # Format: [123] call(5): append([1,2], [3], _G123) @ frame=2 cp=1
        pass

class JSONLTraceSink(TraceSink):
    """Machine-readable JSONL output."""
    def write_event(self, event: TraceEvent):
        # Compact format with schema version
        obj = {
            "v": 1,
            "rid": event.run_id,
            "sid": event.step_id,
            "p": self._encode_port(event.port),  # 0=call,1=exit,2=redo,3=fail
            "pid": event.pred_id,
            "fd": event.frame_depth,
            "cd": event.cp_depth,
            "gh": event.goal_height,
            "ws": event.write_stamp,
            "g": event.goal_pretty,
            "gc": event.goal_canonical
        }
        # Optional fields
        if event.bindings:
            obj["b"] = event.bindings
        if event.monotonic_ns:
            obj["t"] = event.monotonic_ns

class FileTraceSink(TraceSink):
    """File output with rotation support."""
    def __init__(self, path: Path, max_size_mb: int = 100, max_files: int = 5):
        super().__init__()
        self.path = path
        self.max_size = max_size_mb * 1024 * 1024
        self.max_files = max_files
        self.batch_size = 100
        self.batch = []
    
    def rotate_if_needed(self):
        """Rotate file.log → file.log.1 → ... → file.log.N"""
```

#### 5. Filters with Precedence
```python
class TraceFilters:
    """Composable trace filters with defined precedence."""
    def __init__(self):
        self.port_filter = None      # Set of allowed ports
        self.pred_filter = None      # Set of allowed predicates
        self.depth_range = None      # (min, max) frame depth
        self.sampling_rate = 1       # 1 = all, N = 1/N events
        self.sample_counter = 0
    
    def should_emit(self, event: TraceEvent) -> bool:
        """Apply filters in order: port → pred → depth → sampling."""
        # 1. Port filter
        if self.port_filter and event.port not in self.port_filter:
            return False
        
        # 2. Predicate/spypoint filter
        if self.pred_filter and event.pred_id not in self.pred_filter:
            return False
        
        # 3. Depth filter
        if self.depth_range:
            min_d, max_d = self.depth_range
            if not (min_d <= event.frame_depth <= max_d):
                return False
        
        # 4. Sampling
        if self.sampling_rate > 1:
            self.sample_counter += 1
            if self.sample_counter % self.sampling_rate != 0:
                return False
        
        return True
```

### Integration Points

#### Engine Modifications
```python
class Engine:
    def __init__(self, ..., trace=False, debug=False):
        # Existing initialization
        self.tracer = PortsTracer(self) if trace else None
        self.metrics = EngineMetrics() if debug else None
        self._global_step_id = 0
    
    def _trace_port(self, port: str, goal: Term):
        """Emit trace event if tracing enabled."""
        if self.tracer:
            self._global_step_id += 1
            self.tracer.emit_event(
                port=port,
                goal=goal,
                step_id=self._global_step_id,
                frame_depth=len(self.frame_stack),
                cp_depth=len(self.choicepoints),
                goal_height=len(self.goal_stack),
                write_stamp=self.write_stamp,
                # ... other metadata
            )
    
    def _trace_internal(self, kind: str, **details):
        """Emit internal debug event if enabled."""
        if self.tracer and self.tracer.enable_internal_events:
            self._global_step_id += 1
            self.tracer.emit_internal_event(kind, **details)
    
    def _dispatch_predicate(self, goal):
        """Modified to include trace ports."""
        self._trace_port('call', goal.term)
        
        # Existing predicate dispatch logic
        success = self._original_dispatch(goal)
        
        if success:
            self._trace_port('exit', goal.term)
        else:
            self._trace_port('fail', goal.term)
        
        return success
    
    def _push_choicepoint(self, cp):
        """Modified to trace internal events."""
        self._trace_internal('cp_push', kind=cp.kind, pred_id=cp.pred_id)
        # Existing logic
    
    def _execute_cut(self):
        """Modified to trace cut commits."""
        pruned_count = self._count_prunable_cps()
        self._trace_internal('cut_commit', pruned=pruned_count)
        # Existing cut logic
```

#### REPL Commands
```python
class PrologREPL:
    def __init__(self):
        # Existing initialization
        self.trace_enabled = False
        self.spypoints = set()
        self.trace_config = {
            'bindings': 'none',
            'max_depth': 4,
            'sampling': 1
        }
    
    def cmd_trace(self, arg: str):
        """Enable/disable/configure tracing."""
        parts = arg.split()
        if parts[0] == 'on':
            self.engine.tracer = PortsTracer(self.engine)
            self.trace_enabled = True
        elif parts[0] == 'off':
            self.engine.tracer = None
            self.trace_enabled = False
        elif parts[0] == 'json':
            # trace json "out.jsonl"
            path = parts[1] if len(parts) > 1 else "trace.jsonl"
            sink = JSONLTraceSink(FileTraceSink(Path(path)))
            self.engine.tracer.add_sink(sink)
        elif parts[0] == 'pretty':
            # trace pretty "out.log"
            path = parts[1] if len(parts) > 1 else "trace.log"
            sink = PrettyTraceSink(FileTraceSink(Path(path)))
            self.engine.tracer.add_sink(sink)
        elif parts[0] == 'sample':
            # trace sample 10
            rate = int(parts[1]) if len(parts) > 1 else 10
            self.engine.tracer.filters.sampling_rate = rate
    
    def cmd_spy(self, predicate: str):
        """Add spypoint: spy append/3"""
        name, arity = parse_predicate_spec(predicate)
        self.spypoints.add((name, arity))
        if self.engine.tracer:
            self.engine.tracer.spypoints.add((name, arity))
    
    def cmd_spys(self):
        """List all spypoints."""
        for name, arity in sorted(self.spypoints):
            print(f"  {name}/{arity}")
    
    def cmd_snapshot(self):
        """Take engine snapshot."""
        manager = SnapshotManager()
        snap = manager.snapshot(self.engine)
        print(f"Snapshot #{snap.step_id} captured:")
        print(f"  Store: {snap.store_size} cells")
        print(f"  Trail: {snap.trail_length} entries")
        print(f"  Goals: {snap.goal_height} pending")
        print(f"  Choicepoints: {snap.cp_depth} active")
```

## JSONL Schema

The JSONL format is compact and versioned for machine parsing:

```json
{
  "v": 1,                    // Schema version
  "rid": "uuid-here",        // Run ID
  "sid": 123,                // Step ID
  "p": 0,                    // Port: 0=call, 1=exit, 2=redo, 3=fail
  "pid": "append/3",         // Predicate ID
  "fd": 2,                   // Frame depth
  "cd": 1,                   // CP depth
  "gh": 5,                   // Goal height
  "ws": 42,                  // Write stamp
  "g": "append([1,2],[3],_G1)",     // Pretty goal
  "gc": "','(append(...),is(...))"  // Canonical form
}
```

Optional fields:
- `"b"`: Bindings object (when enabled)
- `"t"`: Monotonic nanoseconds (when timestamps enabled)
- `"k"`: Kind for internal events
- `"d"`: Details for internal events

Full examples will be documented in `docs/TRACE_FORMAT.md`.

## Implementation Phases

### Phase 1: Core Infrastructure (3.0)
**Files**: `prolog/debug/tracer.py`, `prolog/debug/events.py`
- [ ] TraceEvent dataclass with all fields
- [ ] PortsTracer with basic event emission
- [ ] Integration points in Engine
- [ ] Call/Exit/Redo/Fail port detection
- [ ] Step counter and invariant tracking
- [ ] Frame depth derivation from stacks

### Phase 2: Metrics & Counters (3.2)
**Files**: `prolog/debug/metrics.py`
- [ ] EngineMetrics class
- [ ] Global counter integration
- [ ] Per-predicate metrics
- [ ] Unification tracking
- [ ] Backtrack counting
- [ ] Cut and pruning metrics
- [ ] Exception counting
- [ ] Indexing statistics integration
- [ ] Clear reset() semantics

### Phase 3: Snapshots & Diffs (3.1)
**Files**: `prolog/debug/snapshot.py`
- [ ] EngineSnapshot with heights and tops
- [ ] CPSnapshot and FrameSnapshot
- [ ] Snapshot capture from engine state
- [ ] Typed diff computation (SnapshotDiff)
- [ ] JSON serialization of snapshots
- [ ] Human-readable diff output

### Phase 4: Output Sinks (3.4)
**Files**: `prolog/debug/sinks.py`
- [ ] TraceSink with ring buffer
- [ ] Backpressure handling
- [ ] Drop counters and policies
- [ ] PrettyTraceSink for humans
- [ ] JSONLTraceSink with compact schema
- [ ] FileTraceSink with rotation
- [ ] Batch writing (flush every N events or M ms)

### Phase 5: Filters & Spypoints (3.0 continued)
**Files**: `prolog/debug/filters.py`
- [ ] TraceFilters with precedence
- [ ] Port filtering
- [ ] Predicate/spypoint filtering
- [ ] Depth range filtering
- [ ] Sampling (1/N events)
- [ ] Filter combination tests

### Phase 6: REPL Integration (3.4)
**Files**: Updates to `prolog/repl.py`
- [ ] trace on/off/json/pretty commands
- [ ] spy/unspy/spys commands
- [ ] snapshot command
- [ ] metrics command
- [ ] trace sample N command
- [ ] Configuration loading
- [ ] Term depth capping in REPL

### Phase 7: Internal Debug Events (3.0 extended)
**Files**: Updates to `prolog/debug/events.py`, `prolog/engine/`
- [ ] InternalEvent class
- [ ] cp_push/cp_pop events
- [ ] frame_push/frame_pop events
- [ ] cut_commit with pruning count
- [ ] catch_switch events
- [ ] Optional enable flag

### Phase 8: Tooling & CI (3.5)
**Files**: `tools/replay_trace.py`, `.github/workflows/`
- [ ] Trace replay tool (reconstruct last 200 steps)
- [ ] CI artifact collection
- [ ] Failure reproduction scripts
- [ ] Performance regression detection

### Phase 9: Exporters (3.3)
**Files**: `prolog/debug/exporters.py`
- [ ] Call graph exporter (static analysis)
- [ ] Constraint graph exporter (placeholder)
- [ ] DOT format generation
- [ ] Graph layout hints

### Phase 10: Documentation & Testing
**Files**: `docs/TRACE_FORMAT.md`, tests
- [ ] JSONL schema documentation with examples
- [ ] Trace format examples
- [ ] Performance overhead tests
- [ ] Determinism tests
- [ ] Invariant verification tests
- [ ] Integration test suite

## Performance Considerations

### Overhead Targets
- **Tracing disabled**: ≤5% overhead (null checks only)
- **Pretty tracing**: ≤20% overhead
- **JSONL tracing**: ≤30% overhead
- **Full metrics**: ≤10% overhead

### Optimization Strategies
1. **Lazy evaluation**: Don't format unless needed
2. **Buffered output**: Batch writes to sinks
3. **Interned strings**: Predicate names cached
4. **Minimal allocations**: Reuse event objects
5. **Conditional compilation**: Use `__debug__` flag

### Bindings Policy
- **Default**: No bindings (`bindings_policy='none'`)
- **Optional projections**:
  - `'names'`: Variable names only (no values)
  - `'names_values'`: Include values with caps:
    - `max_term_depth`: Maximum nesting depth
    - `max_items_per_list`: Maximum list items shown
- **Determinism**: Stable variable naming using existing fresh-var map

## Testing Strategy

### Critical Tests

#### 1. Port Sequence Invariants
- Successful goal: CALL → EXIT
- Failed goal: CALL → FAIL  
- Non-deterministic: CALL → EXIT → REDO → (EXIT|FAIL) → ...
- Verify step_id strictly increases

#### 2. Backtracking Determinism
- Run same query twice
- JSONL traces must be byte-for-byte identical (timestamps disabled)

#### 3. Indexing + Trace Sanity
- With indexing on/off, event stream identical
- Only metrics differ (candidates_considered/yielded)

#### 4. Cut & Exception Events
- With internal events on: verify cut_commit and catch_switch
- With internal events off: pure 4-port stream unchanged

#### 5. Snapshot/Diff No-Growth
- Over pure backtracking (e.g., member/2)
- Take snapshots every K steps
- Assert no monotonic growth in heights/trail

#### 6. Sink Backpressure
- Fill buffer with slow sink
- Assert drop counters increase
- Engine progress unaffected

#### 7. Filter Precedence
- Combine spy + port + depth + sampling
- Assert filtered JSONL matches expected subset

#### 8. Metrics Correctness
- Seeded workload
- Hand-compute expected counts
- Verify all counters match

### Unit Tests
- Event creation and filtering
- Sink output formats
- Snapshot/diff accuracy
- Metrics counting
- Filter combinations
- Invariant checks

### Integration Tests
- End-to-end tracing of queries
- REPL command testing
- File rotation behavior
- Performance overhead measurement
- Determinism verification

### Performance Tests
- Overhead with tracing disabled
- Overhead with various trace levels
- Large trace file handling
- Memory usage under load
- Published in CI logs

## Acceptance Criteria

1. **Performance**
   - [ ] ≤5% overhead with tracing disabled
   - [ ] ≤20% overhead with pretty tracing
   - [ ] ≤30% overhead with JSONL tracing
   - [ ] Measured and published in CI

2. **Functionality**
   - [ ] All four ports (call/exit/redo/fail) traced correctly
   - [ ] Spypoints work as expected
   - [ ] Filters reduce output appropriately
   - [ ] Snapshots capture complete state
   - [ ] Internal events toggleable

3. **Output Quality**
   - [ ] Pretty format readable by humans
   - [ ] JSONL format parseable by tools
   - [ ] No Python object IDs leak
   - [ ] Deterministic output for same input (timestamps disabled)
   - [ ] Byte-for-byte equality for identical runs

4. **Integration**
   - [ ] REPL commands work intuitively
   - [ ] CI integration captures failures
   - [ ] Replay tool reconstructs last 200 steps

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| High performance overhead | High | Careful profiling, lazy evaluation |
| Non-deterministic output | Medium | No timestamps by default, stable var naming |
| Memory leaks from tracing | High | Bounded buffers, rotation, drop policy |
| Complex integration | Medium | Incremental phases, extensive testing |
| Breaking existing code | Low | Feature flags, backward compatibility |
| Sink backpressure | Medium | Ring buffers, drop counters |

## Implementation Order (Low Risk → High Value)

1. **3.0 Minimal tracer**: step_id, 4 ports, pred_id, heights; pretty sink only; off by default
2. **3.2 Metrics**: global counters, per-predicate; expose in REPL; zero overhead when disabled
3. **3.1 Snapshots/diffs**: heights + tops + CP/frames data; typed diffs; JSON export
4. **3.4 JSONL sink**: versioned schema; file sink with buffering & rotation
5. **Filters & spypoints**: precedence rules; sampling, depth, ports, pred masks
6. **Replay tool**: consume JSONL; validate step sequence; reconstruct last 200 steps
7. **Exporters**: call graph from program; constraint graph placeholder
8. **Internal events**: cp/cut/catch markers (debug-only, off by default)

## Example Usage

### Basic Tracing
```prolog
?- trace on.
Tracing enabled.

?- append([1,2], [3], X).
[1] call: append([1,2], [3], _G1) @ frame=1 cp=0
[2] call: append([2], [3], _G2) @ frame=2 cp=0
[3] call: append([], [3], _G3) @ frame=3 cp=0
[4] exit: append([], [3], [3]) @ frame=3 cp=0
[5] exit: append([2], [3], [2,3]) @ frame=2 cp=0
[6] exit: append([1,2], [3], [1,2,3]) @ frame=1 cp=0
X = [1,2,3].
```

### Spypoint
```prolog
?- spy append/3.
Spypoint set on append/3.

?- append(X, Y, [1,2,3]).
[1] call: append(_G1, _G2, [1,2,3]) @ frame=1
[2] exit: append([], [1,2,3], [1,2,3]) @ frame=1
X = [], Y = [1,2,3] ;
[3] redo: append(_G1, _G2, [1,2,3]) @ frame=1
[4] exit: append([1], [2,3], [1,2,3]) @ frame=1
X = [1], Y = [2,3] ;
...
```

### Snapshot
```prolog
?- snapshot.
Snapshot #42 captured:
  Store: 145 cells (top=145)
  Trail: 89 entries (top=89)
  Goals: 3 pending (top=3)
  Frames: 2 active (top=2)
  Choicepoints: 2 active (top=2)
  Memory: 14.3 KB
```

### JSONL Tracing
```prolog
?- trace json "debug.jsonl".
JSONL tracing to debug.jsonl enabled.

?- trace sample 10.
Sampling 1/10 events.
```

## Notes

- Stage 3 provides critical infrastructure for debugging and understanding program behavior
- The tracer design follows standard Prolog 4-port model with optional internal events
- All output formats are designed to be deterministic and reproducible
- Performance overhead is carefully managed through lazy evaluation and buffering
- Integration with existing engine is through well-defined hooks
- The engine is **iterative**, not recursive - frame_depth tracks frame stack height
- Determinism guaranteed only with timestamps disabled
- Bindings are off by default to minimize overhead and output size
- Filter precedence is clearly defined: port → pred → depth → sampling
- The implementation enables future stages (especially constraint debugging)