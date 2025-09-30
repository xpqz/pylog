# PyLog

[![CI](https://github.com/xpqz/pylog/actions/workflows/ci.yml/badge.svg)](https://github.com/xpqz/pylog/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/xpqz/pylog/branch/main/graph/badge.svg)](https://codecov.io/gh/xpqz/pylog)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)
[![Python 3.11+](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)

A tree-walking Prolog interpreter in Python with CLP(FD) (Constraint Logic Programming over Finite Domains).

## Overview

PyLog is designed for learning and debuggability first, with stable interfaces that survive later implementation stages. The system uses explicit stacks (no Python recursion), centralized mutation via `bind()`, and includes built-in observability features.

## Features

### Core Engine
- Pure Prolog core with ISO builtins
- Explicit stack-based execution (no recursion limits)
- First-argument and type-based indexing for performance
- Exception handling with catch/throw
- Cut (!) operator with proper scope barriers

### Debug & Observability (Stage 3)
- **4-Port Tracer**: Full debugging with CALL, EXIT, REDO, FAIL ports
- **Spypoints**: Selective predicate tracing
- **Trace Formats**:
  - JSONL format for machine processing (see [docs/TRACE_FORMAT.md](docs/TRACE_FORMAT.md))
  - Pretty format for human reading with indentation
- **Trace Sinks**: Pluggable output handlers (file, memory, network)
- **Determinism Control**: Configurable bindings, timestamps, internal events
- **Performance Profiling**: Event counters and timing support

### Extensions
- Attributed variables for extensibility
- CLP(FD) solver with propagation and reification
- Constraint graph visualization

## Quick Start

```python
from prolog.parser import parser
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program

# Parse a Prolog program
clauses = parser.parse_program("""
    parent(tom, bob).
    parent(bob, pat).
    grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
""")

# Create engine and run query
engine = Engine(Program(tuple(clauses)))
goals = parser.parse_query("?- grandparent(tom, Z).")
solutions = list(engine.run(goals))
print(solutions)  # [{'Z': Atom('pat')}]
```

## Installation

```bash
# Install uv package manager
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install project and dependencies
uv sync

# Run tests
uv run pytest
```

## Debugging

Enable tracing to see execution details:

```python
# Create engine with tracing enabled
engine = Engine(Program(tuple(clauses)), trace=True)

# Add a sink to capture trace events
from prolog.debug.sinks import PrettyTraceSink
import sys
sink = PrettyTraceSink(sys.stdout)
engine.tracer.add_sink(sink)

# Run query - will output trace to stdout
solutions = list(engine.run(goals))
```

## Development

See [docs/ARCH.md](docs/ARCH.md) for architecture details and [docs/PLAN.md](docs/PLAN.md) for the implementation roadmap.

### Contributing

**⚠️ Critical**: Before contributing, read our [Test Integrity Rules](CLAUDE.md#-critical-test-integrity-rules). **Tests are sacred** - never modify tests to make them pass.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development guidelines and [prolog/tests/README.md](prolog/tests/README.md) for testing practices.

## License

MIT