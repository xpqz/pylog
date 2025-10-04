# PyLog

[![CI](https://github.com/xpqz/pylog/actions/workflows/ci.yml/badge.svg)](https://github.com/xpqz/pylog/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/xpqz/pylog/branch/main/graph/badge.svg)](https://codecov.io/gh/xpqz/pylog)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)
[![Python 3.11+](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)

PyLog is a Prolog interpreter implemented in Python with CLP(FD) support. It focuses on correctness, transparency, and observability: explicit stacks (no Python recursion), robust catch/throw semantics, and a 4‑port tracer with machine‑readable output.

## Highlights

- Prolog engine with ISO‑style core and proper cut `!/0` and exception handling
- Parser + reader with operator support (including CLP(FD) operators)
- CLP(FD) over integers: domains, linear constraints, reification, labeling strategies
- 4‑port tracer (CALL, EXIT, REDO, FAIL), spypoints, and JSONL/pretty sinks
- Interactive REPL with history, completion, and tracing controls
- Command‑line runner to consult files and execute goals

## Installation

PyLog targets Python 3.11+ and uses `uv` for dependency management.

```bash
# Install uv
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install project and dependencies
uv sync

# Run tests
uv run pytest
```

## Usage

### REPL

Start the REPL:

```bash
pylog
# or
python -m prolog.repl
```

Consult files and query:

```text
?- consult('prolog/examples/hanoi.pl').
Loaded: prolog/examples/hanoi.pl
true.

?- hanoi(3, left, right, middle, Moves).
Moves = [move(left,right),move(left,middle),move(right,middle),
         move(left,right),move(middle,left),move(middle,right),
         move(left,right)]
 .
```

Interactive consult:

```text
?- consult(user).
|: parent(tom, bob).
|: parent(bob, ann).
|: grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
|: .
true.
?- listing(parent/2).
parent(tom, bob).
parent(bob, ann).
```

### Command‑line

Run a goal without entering the REPL:

```bash
# Consult a file, run a goal, print first answer, exit
pylog prolog/examples/hanoi.pl -g "hanoi(3,left,right,middle,Moves)" --once --noninteractive

# Run a goal and print all answers
pylog -g "member(X,[a,b])" --all --noninteractive

# Enable pretty tracing while running
pylog -g "append([1],[2],X)" --once --trace --noninteractive
```

`-g/--goal` takes the query without the `?-` prefix and without the trailing period. Use `--once` (default) or `--all`. `--noninteractive` prevents the REPL from starting after the run.

### Python API

```python
from prolog.parser import parser
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program

clauses = parser.parse_program("""
    parent(tom, bob).
    parent(bob, pat).
    grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
""")

engine = Engine(Program(tuple(clauses)))
goals = parser.parse_query("?- grandparent(tom, Z).")
solutions = list(engine.run(goals))
```

Enable tracing:

```python
from prolog.debug.sinks import PrettyTraceSink
import sys

engine = Engine(Program(tuple(clauses)), trace=True)
engine.tracer.add_sink(PrettyTraceSink(sys.stdout))
solutions = list(engine.run(goals))
```

## Documentation

User documentation lives under `mkdocs/docs/`:

- Getting Started → REPL: `mkdocs/docs/getting-started/repl.md`
- Guides → Tracing and Debugging: `mkdocs/docs/guides/tracing-and-debugging.md`
- CLP(FD) → Labeling and Reification: `mkdocs/docs/clpfd/`
- Cookbook examples (Sudoku, SEND+MORE, N‑queens): `mkdocs/docs/cookbook/`
 - Reference → CLI: `mkdocs/docs/reference/cli.md`

## Contributing

Before contributing, please read the test integrity rules in [CLAUDE.md](CLAUDE.md). Tests define the specification; do not change them to make code pass. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines and [prolog/tests/README.md](prolog/tests/README.md) for test organization.

## License

MIT
