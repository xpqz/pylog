# Tracing and debugging

See also:

- Getting Started → [REPL](../getting-started/repl.md#tracing-from-the-repl)
- Reference → [REPL commands](../reference/repl-commands.md)

PyLog includes a 4‑port tracer (CALL/EXIT/REDO/FAIL) with multiple output formats. This guide shows how to enable tracing, choose an output sink, and analyze traces.

Quick tips for debugging logic:

- Break problems into small predicates and test each in isolation
- Inspect domains after posting constraints before labelling
- Prefer declarative checks over print‑style debugging

The tracer is designed to add near‑zero overhead when disabled and deterministic output when timestamps are off.

Enabling the tracer
-------------------

Create an engine with tracing enabled and attach one or more sinks:

```python
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.debug.sinks import PrettyTraceSink, JSONLTraceSink, FileTraceSink
import sys

program = Program(())  # your compiled clauses
engine = Engine(program, trace=True)

# Human‑readable output
pretty = PrettyTraceSink(output=sys.stdout)
engine.tracer.add_sink(pretty)

# JSONL to file (with rotation)
file_sink = FileTraceSink('trace.jsonl', format='jsonl', max_size_mb=50, max_files=3)
engine.tracer.add_sink(file_sink)

# Run a query – sinks receive CALL/EXIT/REDO/FAIL events
solutions = list(engine.run(your_goals))

# Close any file sinks when done
file_sink.close()
```

Using the tracer from the REPL
------------------------------

You can control tracing interactively without writing any Python. From the REPL:

- `trace on` — enable pretty trace to stdout
- `trace off` — disable tracing
- `trace pretty FILE` — pretty trace to file
- `trace json FILE` — JSON Lines trace to file
- `trace sample N` — sample 1 in N events (reduces volume)

Spypoints (filter which predicates are traced):

- `spy name/arity` — add a spypoint (e.g. `spy member/2`)
- `unspy name/arity` — remove a spypoint
- `spys` — list active spypoints
- `untrace` — clear all spypoints

Example session:

```text
?- trace on.
Tracing enabled (pretty format to stdout)
true.

?- spy append/3.
Spypoint added: append/3
true.

?- append([1],[2],X).
CALL append/3 ...
EXIT append/3 ...
X = [1,2]
 .
```

See Getting Started → REPL and Guides → REPL tips & shortcuts for a fuller tour
of interactive commands.

Trace sinks
-----------

- PrettyTraceSink
  - Human‑readable, indented events: `[sid] PORT name/arity: goal`
  - Useful during development and in the REPL

- JSONLTraceSink
  - Machine‑readable JSON Lines; accepts a writable stream
  - Typically used via FileTraceSink (below)

- FileTraceSink
  - File output with batching and rotation
  - Options: `format='jsonl'|'pretty'`, `max_size_mb`, `max_files`, `batch_size`

- CollectorSink
  - In‑memory event collection (for tests/analysis)
  - Access collected events via `collector.events`

JSONL schema (overview)
-----------------------

Events use compact keys for low overhead. Required keys:

- `v` (int): schema version (1)
- `rid` (str): run UUID for the query
- `sid` (int): monotonic step id (post‑filter)
- `p` (int): port (0=call, 1=exit, 2=redo, 3=fail)
- `pid` (str): predicate id, e.g. `append/3`
- `g` (str): pretty goal
- `fd` (int): frame depth; `cd` (int): choicepoint depth; `gh` (int): goal height; `ws` (int): write stamp

Optional keys (omitted if not set):

- `ts` (int): monotonic timestamp (ns)
- `b` (object): bindings, when enabled

See Reference → Trace format for the full specification and examples.

CI integration
--------------

For CI runs, you can gate tracing and collect artifacts on failures:

```python
import os
from pathlib import Path
from prolog.debug.sinks import FileTraceSink
from prolog.debug.ci_integration import is_tracing_enabled, capture_failure_artifacts

trace_path = Path('trace.jsonl')
sink = None
if is_tracing_enabled():  # respects PYLOG_TRACE=1|true|yes|on
    sink = FileTraceSink(str(trace_path), format='jsonl', max_size_mb=25, max_files=2)
    engine.tracer.add_sink(sink)

try:
    solutions = list(engine.run(goals))
finally:
    if sink:
        sink.close()
        # On failure, copy the tail of the trace and optional snapshot
        capture_failure_artifacts(trace_file=trace_path, output_dir=Path('artifacts'))
```

Replay and validation
---------------------

Use the built‑in replay tool to load and validate traces:

```bash
uv run python prolog/tools/replay_trace.py trace.jsonl --check-invariants --last 200
```

This checks for monotonic step ids and basic port‑sequence invariants and can emit a JSON summary.

Performance and tuning
----------------------

- Batching and rotation
  - `FileTraceSink(batch_size=...)` controls flush cadence
  - Rotation prevents unbounded file growth (`max_size_mb`, `max_files`)

- Bindings and truncation
  - Bindings emission is configurable (planned). Pretty and filters include truncation caps: `max_term_depth`, `max_items_per_list`

- Depth semantics
  - `fd` (frame depth) reflects implementation details; prefer `gh` (goal height) for logical depth

Limitations and roadmap
-----------------------

- Filtering and spypoints exist in the codebase but are not fully wired into the tracer yet; expect this in a later stage.
- Timestamps and internal events are optional and off by default.

Reference
---------

- Reference → Trace format: compact JSONL schema and examples
- prolog/debug modules: `tracer.py`, `sinks.py`, `filters.py`, `invariant_checker.py`, `replay.py`
