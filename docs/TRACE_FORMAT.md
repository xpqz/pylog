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

## Example Traces

### Basic 4-Port Trace
```json
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":1,"p":0,"pid":"append/3","fd":1,"cd":0,"gh":3,"ws":42,"g":"append([1,2],[3],_G1)"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":2,"p":0,"pid":"append/3","fd":2,"cd":0,"gh":4,"ws":43,"g":"append([2],[3],_G2)"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":3,"p":0,"pid":"append/3","fd":3,"cd":0,"gh":5,"ws":44,"g":"append([],[3],_G3)"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":4,"p":1,"pid":"append/3","fd":3,"cd":0,"gh":5,"ws":45,"g":"append([],[3],[3])"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":5,"p":1,"pid":"append/3","fd":2,"cd":0,"gh":4,"ws":46,"g":"append([2],[3],[2,3])"}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":6,"p":1,"pid":"append/3","fd":1,"cd":0,"gh":3,"ws":47,"g":"append([1,2],[3],[1,2,3])"}
```

### With Bindings
```json
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":1,"p":0,"pid":"append/3","fd":1,"cd":0,"gh":3,"ws":42,"g":"append([1,2],[3],X)","b":{"X":"_G1"}}
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":6,"p":1,"pid":"append/3","fd":1,"cd":0,"gh":3,"ws":47,"g":"append([1,2],[3],X)","b":{"X":"[1,2,3]"}}
```

### With Backtracking (REDO)
```json
{"v":1,"rid":"a3f2c8b1-4d5e-6f7a-8b9c-0d1e2f3a4b5c","sid":10,"p":0,"pid":"member/2","fd":1,"cd":1,"gh":2,"ws":50,"g":"member(X,[1,2,3])"}
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