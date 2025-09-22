# Trace Format Specification

## Schema Version 1

This document defines the JSONL trace format for PyLog's debug infrastructure.

## Core Design Principles

- **Compact keys** for minimal overhead
- **Optional fields** omitted when None/empty
- **Determinism** - timestamps optional, determinism relies on step ordering
- **Versioned** - schema version in every record

## JSONL Format

Each line is a valid JSON object representing one trace event.

### Required Fields

| Key | Type | Description |
|-----|------|-------------|
| `v` | int | Schema version (currently 1) |
| `rid` | string | Run ID (UUID for this query execution) |
| `sid` | int | Step ID (monotonic, post-filter only) |
| `p` | int | Port: 0=call, 1=exit, 2=redo, 3=fail |
| `pid` | string | Predicate ID ("name/arity") |
| `fd` | int | Frame depth (frame stack height) |
| `cd` | int | Choicepoint depth (CP stack height) |
| `gh` | int | Goal height (goal stack height) |
| `ws` | int | Write stamp (engine's write counter) |
| `g` | string | Goal pretty-printed form |

### Optional Fields

| Key | Type | Description |
|-----|------|-------------|
| `qid` | int | Query ID (sequence within run) |
| `gc` | string | Goal canonical form (for parsing) |
| `b` | object | Variable bindings (when enabled) |
| `t` | int | Monotonic nanoseconds (when timestamps enabled) |

### Internal Event Fields (Debug Only)

| Key | Type | Description |
|-----|------|-------------|
| `k` | string | Kind of internal event |
| `d` | object | Event-specific details |

## Port Semantics

### Frame Depth (`fd`) Behavior
**Important**: Frame depth has specific semantics in PyLog:
- **CALL events**: Emitted *before* pushing a new frame, so `fd` reflects parent's depth
- **EXIT events**: Uses parent frame's depth (frame already popped)
- **REDO events**: Frame recreated, shows current depth
- **FAIL events**: Shows depth at failure point

For measuring recursion depth, use:
- Goal height (`gh`) - reflects actual goal stack depth
- Count of CALL events for a specific predicate
- Do NOT rely on `fd` for recursion depth

## Example Traces

### Basic 4-Port Trace

Query: `?- append([1,2], [3], X).`

```json
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":1,"p":0,"pid":"append/3","fd":0,"cd":0,"gh":1,"ws":0,"g":"append([1,2],[3],X)"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":2,"p":0,"pid":"append/3","fd":1,"cd":1,"gh":2,"ws":5,"g":"append([2],[3],_G234)"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":3,"p":0,"pid":"append/3","fd":2,"cd":2,"gh":3,"ws":10,"g":"append([],[3],_G456)"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":4,"p":1,"pid":"append/3","fd":2,"cd":2,"gh":2,"ws":15,"g":"append([],[3],[3])"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":5,"p":1,"pid":"append/3","fd":1,"cd":1,"gh":1,"ws":20,"g":"append([2],[3],[2,3])"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":6,"p":1,"pid":"append/3","fd":0,"cd":0,"gh":0,"ws":25,"g":"append([1,2],[3],[1,2,3])"}
```

### Backtracking Example

Query: `?- member(X, [1,2,3]).`

```json
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":1,"p":0,"pid":"member/2","fd":0,"cd":0,"gh":1,"ws":0,"g":"member(X,[1,2,3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":2,"p":1,"pid":"member/2","fd":0,"cd":1,"gh":0,"ws":5,"g":"member(1,[1,2,3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":3,"p":2,"pid":"member/2","fd":0,"cd":1,"gh":1,"ws":6,"g":"member(X,[1,2,3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":4,"p":0,"pid":"member/2","fd":1,"cd":1,"gh":2,"ws":10,"g":"member(X,[2,3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":5,"p":1,"pid":"member/2","fd":1,"cd":2,"gh":1,"ws":15,"g":"member(2,[2,3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":6,"p":2,"pid":"member/2","fd":1,"cd":2,"gh":2,"ws":16,"g":"member(X,[2,3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":7,"p":0,"pid":"member/2","fd":2,"cd":2,"gh":3,"ws":20,"g":"member(X,[3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":8,"p":1,"pid":"member/2","fd":2,"cd":3,"gh":2,"ws":25,"g":"member(3,[3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":9,"p":2,"pid":"member/2","fd":2,"cd":3,"gh":3,"ws":26,"g":"member(X,[3])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":10,"p":0,"pid":"member/2","fd":3,"cd":3,"gh":4,"ws":30,"g":"member(X,[])"}
{"v":1,"rid":"b2e3d4c5-6f7a-8b9c-0d1e-2f3a4b5c6d7e","sid":11,"p":3,"pid":"member/2","fd":3,"cd":3,"gh":3,"ws":31,"g":"member(X,[])"}
```

## Parsing Traces

### Python Example
```python
import json

def parse_trace_file(filename):
    """Parse JSONL trace file into structured events."""
    events = []
    with open(filename, 'r') as f:
        for line in f:
            if line.strip():
                event = json.loads(line)
                # Convert port number to name
                port_names = {0: 'CALL', 1: 'EXIT', 2: 'REDO', 3: 'FAIL'}
                event['port_name'] = port_names.get(event['p'], 'UNKNOWN')
                events.append(event)
    return events

def analyze_trace(events):
    """Basic trace analysis."""
    stats = {
        'total_events': len(events),
        'predicates': set(),
        'max_depth': 0,
        'max_goal_height': 0,
        'port_counts': {0: 0, 1: 0, 2: 0, 3: 0}
    }

    for event in events:
        stats['predicates'].add(event.get('pid', 'unknown'))
        stats['max_depth'] = max(stats['max_depth'], event.get('fd', 0))
        stats['max_goal_height'] = max(stats['max_goal_height'], event.get('gh', 0))
        stats['port_counts'][event['p']] += 1

    return stats
```

## Port Sequence Validation

Valid port sequences follow these rules:
1. CALL (0) must precede EXIT (1) or FAIL (3)
2. REDO (2) can only occur after EXIT (1)
3. After FAIL (3), no more ports for that goal at that depth

## Performance Considerations

- Compact keys minimize JSON size (30-50% smaller than verbose)
- Batching reduces I/O overhead
- Ring buffer prevents unbounded memory growth
- File rotation prevents disk exhaustion
- Step IDs assigned post-filter to maintain sequential ordering

## Configuration

Tracer configuration affects output:
- `bindings_policy`: 'none' (default), 'names', 'names_values'
- `max_term_depth`: 4 (default) - truncation depth for complex terms
- `max_items_per_list`: 10 (default) - max list items shown
- `enable_internal_events`: False (default) - include internal events
- `enable_timestamps`: False (default) - include monotonic timestamps

## Version History

### Version 1 (Current)
- Initial JSONL format specification
- Compact key encoding
- Optional timestamp support
- Internal event support (debug only)
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":11,"p":1,"pid":"member/2","fd":1,"cd":1,"gh":2,"ws":51,"g":"member(1,[1,2,3])"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":12,"p":2,"pid":"member/2","fd":1,"cd":1,"gh":2,"ws":52,"g":"member(X,[1,2,3])"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":13,"p":1,"pid":"member/2","fd":1,"cd":1,"gh":2,"ws":53,"g":"member(2,[1,2,3])"}
```

## Port Encoding

The port field `p` uses integer encoding for compactness:

- `0` = CALL (entering a goal)
- `1` = EXIT (goal succeeded)
- `2` = REDO (backtracking into goal)
- `3` = FAIL (goal failed)

## Parsing Approach

```python
import json

def parse_trace(filepath):
    """Parse JSONL trace file."""
    events = []
    with open(filepath, 'r') as f:
        for line in f:
            event = json.loads(line)
            # Decode port
            port_map = {0: 'call', 1: 'exit', 2: 'redo', 3: 'fail'}
            event['port'] = port_map[event['p']]
            events.append(event)
    return events
```

## Determinism Guarantees

When timestamps are disabled (`t` field omitted), traces are byte-for-byte deterministic for identical runs. The step_id (`sid`) provides ordering and only increments for events that pass filters.

## Binding Policies

The `b` field appears based on the configured bindings policy:

- `none`: No `b` field
- `names`: `b` contains variable names only
- `names_values`: `b` contains names and values (with depth/length caps applied)

## Notes

- All fields use minimal keys to reduce size
- The format is designed for streaming (one complete JSON object per line)
- Schema version allows future evolution
- Internal events (with `k` and `d` fields) are off by default