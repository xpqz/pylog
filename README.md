# PyLog

[![CI](https://github.com/xpqz/pylog/actions/workflows/ci.yml/badge.svg)](https://github.com/xpqz/pylog/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/xpqz/pylog/branch/main/graph/badge.svg)](https://codecov.io/gh/xpqz/pylog)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)
[![Python 3.11+](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)

A tree-walking Prolog interpreter in Python with CLP(FD) (Constraint Logic Programming over Finite Domains).

## Overview

PyLog is designed for learning and debuggability first, with stable interfaces that survive later implementation stages. The system uses explicit stacks (no Python recursion), centralized mutation via `bind()`, and includes built-in observability features.

## Features

- Pure Prolog core with ISO builtins
- Explicit stack-based execution (no recursion limits)
- Attributed variables for extensibility
- CLP(FD) solver with propagation and reification
- Built-in debugger with call/exit/redo/fail ports
- Constraint graph visualization

## Installation

```bash
# Install uv package manager
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install project and dependencies
uv sync

# Run tests
uv run pytest
```

## Development

See [docs/ARCH.md](docs/ARCH.md) for architecture details and [docs/PLAN.md](docs/PLAN.md) for the implementation roadmap.

## License

MIT