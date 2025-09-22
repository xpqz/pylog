# Debug API Documentation

## Overview

PyLog's debug infrastructure provides comprehensive tracing and observability features for Prolog execution. The system follows a 4-port model (CALL, EXIT, REDO, FAIL) and supports multiple output formats and filtering options.

## Core Components

### PortsTracer

The main tracer class that generates trace events during Prolog execution.

```python
from prolog.debug.tracer import PortsTracer

class PortsTracer:
    def __init__(self, engine: Engine):
        """Initialize tracer attached to an engine."""

    def add_sink(self, sink: TraceSink) -> None:
        """Add a trace sink to receive events."""

    def remove_sink(self, sink: TraceSink) -> None:
        """Remove a trace sink."""

    def spy(self, pred_id: str) -> None:
        """Add a spypoint on predicate (e.g., 'append/3')."""

    def nospy(self, pred_id: str) -> None:
        """Remove a spypoint."""

    def nospyall(self) -> None:
        """Remove all spypoints."""
```

### TraceSink Protocol

Base protocol for trace output handlers.

```python
from typing import Protocol

class TraceSink(Protocol):
    def accept(self, event: TraceEvent) -> None:
        """Process a trace event."""

    def flush(self) -> None:
        """Flush any buffered events."""

    def close(self) -> None:
        """Close the sink and flush remaining events."""
```

### JSONLTraceSink

Outputs trace events in JSONL format (one JSON object per line).

```python
from prolog.debug.sinks import JSONLTraceSink

sink = JSONLTraceSink(
    output: Union[TextIO, str, Path],
    batch_size: int = 100,  # Buffer size before flush
    max_file_size: Optional[int] = None,  # File rotation
    max_files: int = 10  # Number of rotated files to keep
)
```

### PrettyTraceSink

Human-readable trace output with indentation.

```python
from prolog.debug.sinks import PrettyTraceSink

sink = PrettyTraceSink(
    output: TextIO,
    show_bindings: bool = False,  # Display variable bindings
    max_depth: int = 10,  # Maximum indentation depth
    indent_size: int = 2  # Spaces per indent level
)
```

### MemoryTraceSink

Collects events in memory for testing/analysis.

```python
from prolog.debug.sinks import MemoryTraceSink

sink = MemoryTraceSink(
    max_events: Optional[int] = None  # Ring buffer size
)

# Access collected events
events = sink.events  # List[TraceEvent]
```

## Trace Event Structure

Each trace event contains:

| Field | Type | Description |
|-------|------|-------------|
| `port` | Port | CALL, EXIT, REDO, or FAIL |
| `goal` | Term | The current goal being traced |
| `pred_id` | str | Predicate identifier (e.g., "append/3") |
| `frame_depth` | int | Current frame stack depth |
| `choice_depth` | int | Current choicepoint stack depth |
| `goal_height` | int | Current goal stack height |
| `write_stamp` | int | Store mutation counter |
| `bindings` | Optional[Dict] | Variable bindings (if enabled) |
| `timestamp_ns` | Optional[int] | Monotonic timestamp in nanoseconds |

## Configuration

### TracerConfig

```python
from prolog.debug.tracer import TracerConfig

config = TracerConfig(
    trace_builtins: bool = True,  # Trace built-in predicates
    trace_all: bool = False,  # Trace all predicates (ignores spypoints)
    bindings_policy: str = 'none',  # 'none', 'names', 'names_values'
    max_term_depth: int = 4,  # Term truncation depth
    max_items_per_list: int = 10,  # Max list items shown
    enable_timestamps: bool = False,  # Include timestamps
    enable_internal_events: bool = False,  # Include internal events
)

tracer = PortsTracer(engine, config=config)
```

## Usage Examples

### Basic Tracing

```python
from prolog.parser import parser
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.debug.sinks import PrettyTraceSink
import sys

# Parse program
clauses = parser.parse_program("""
    append([], L, L).
    append([H|T], L, [H|R]) :- append(T, L, R).
""")

# Create engine with tracing
engine = Engine(Program(tuple(clauses)), trace=True)

# Add pretty sink
sink = PrettyTraceSink(sys.stdout)
engine.tracer.add_sink(sink)

# Run query
goals = parser.parse_query("?- append([1,2], [3], X).")
solutions = list(engine.run(goals))
```

Output:
```
  1 CALL append([1,2], [3], _G1)
    2 CALL append([2], [3], _G2)
      3 CALL append([], [3], _G3)
      4 EXIT append([], [3], [3])
    5 EXIT append([2], [3], [2,3])
  6 EXIT append([1,2], [3], [1,2,3])
```

### Selective Tracing with Spypoints

```python
# Only trace specific predicates
engine.tracer.spy('member/2')
engine.tracer.spy('append/3')

# Run query - only spied predicates are traced
goals = parser.parse_query("?- member(X, [1,2,3]).")
solutions = list(engine.run(goals))
```

### Machine-Readable JSONL Output

```python
from prolog.debug.sinks import JSONLTraceSink

# Output to file
sink = JSONLTraceSink('trace.jsonl')
engine.tracer.add_sink(sink)

# Run query
solutions = list(engine.run(goals))

# Close to flush remaining events
sink.close()
```

### Memory Collection for Testing

```python
from prolog.debug.sinks import MemoryTraceSink

# Collect events in memory
sink = MemoryTraceSink()
engine.tracer.add_sink(sink)

# Run query
solutions = list(engine.run(goals))

# Analyze events
for event in sink.events:
    if event.port == Port.CALL:
        print(f"Called: {event.pred_id}")
```

### Multiple Sinks

```python
# Use multiple sinks simultaneously
pretty_sink = PrettyTraceSink(sys.stdout)
jsonl_sink = JSONLTraceSink('trace.jsonl')
memory_sink = MemoryTraceSink()

engine.tracer.add_sink(pretty_sink)
engine.tracer.add_sink(jsonl_sink)
engine.tracer.add_sink(memory_sink)

# All sinks receive the same events
solutions = list(engine.run(goals))
```

## Performance Considerations

1. **Batching**: JSONL sink batches events before writing (default: 100 events)
2. **File Rotation**: Automatic rotation prevents unbounded file growth
3. **Ring Buffer**: Memory sink can use fixed-size ring buffer
4. **Filtering**: Spypoints reduce event volume
5. **Bindings Policy**: 'none' is fastest, 'names_values' is most detailed

## Integration with Testing

```python
import pytest
from prolog.debug.sinks import MemoryTraceSink

def test_predicate_calls():
    # Setup engine with memory sink
    sink = MemoryTraceSink()
    engine.tracer.add_sink(sink)

    # Run query
    solutions = list(engine.run(goals))

    # Verify trace events
    call_events = [e for e in sink.events if e.port == Port.CALL]
    assert len(call_events) == 3
    assert call_events[0].pred_id == 'append/3'
```

## See Also

- [TRACE_FORMAT.md](TRACE_FORMAT.md) - JSONL format specification
- [ARCH.md](ARCH.md) - Architecture overview
- [prolog/debug/](../prolog/debug/) - Implementation source