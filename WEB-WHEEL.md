# Web Wheel Build Process

This document explains how to build minimal web wheels for PyLog that are suitable for use with Pyodide in web browsers.

## Overview

PyLog provides two wheel configurations:

1. **Standard wheel** (`pylog-0.1.0-standard-py3-none-any.whl`): Full dependencies including `prompt-toolkit` and `pygments` for desktop REPL functionality.

2. **Web wheel** (`pylog-0.1.0-web-py3-none-any.whl`): Minimal dependencies with only `lark>=1.1.0` for web/Pyodide use.

## Building Both Wheels

Use the provided build script:

```bash
./build-wheels.sh
```

This will create both wheels in the `dist/` directory.

## Manual Build Process

### Standard Wheel

```bash
uv build --wheel
```

### Web Wheel

```bash
# Temporarily use web configuration
cp pyproject.toml pyproject-full.toml
cp pyproject-web.toml pyproject.toml

# Build with minimal dependencies
uv run --with build python -m build --wheel

# Restore full configuration
cp pyproject-full.toml pyproject.toml
```

## Web Wheel Characteristics

- **Size**: ~797KB (vs ~810KB for standard wheel)
- **Dependencies**: Only `lark>=1.1.0`
- **Missing Components**: REPL module (`prolog.repl`) excluded from package
- **Use Case**: Pyodide/web environments where terminal dependencies are not available

## Core Functionality Available in Web Wheel

The web wheel includes all core PyLog functionality:

- Prolog parser and AST
- Unification engine with occurs check
- CLP(FD) constraint solver
- Indexing and optimization
- Debug and tracing infrastructure
- All built-in predicates

## What's Excluded from Web Wheel

- `prolog.repl` module (terminal-based REPL)
- `prompt-toolkit` dependency
- `pygments` dependency
- `pylog` console script

## Usage in Pyodide

```python
# Install the web wheel in Pyodide
await micropip.install('./assets/pylog-0.1.0-web-py3-none-any.whl')

# Use core functionality
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.parser.parser import parse_query

# Create engine and run queries
engine = Engine(Program([]))
goals = parse_query('?- X = 42.')
solutions = list(engine.run(goals))
print(solutions)  # [{'X': Int(value=42)}]
```

## File Structure

- `pyproject.toml`: Standard configuration with full dependencies
- `pyproject-web.toml`: Web configuration with minimal dependencies
- `build-wheels.sh`: Automated build script for both wheels
- `WEB-WHEEL.md`: This documentation file

## Acceptance Criteria Met

✅ **Minimal web wheel**: Only `lark>=1.1.0` dependency, suitable for Pyodide
✅ **Local build**: `build-wheels.sh` produces small, Pyodide-compatible wheel
✅ **Desktop REPL functional**: Full install with `prompt-toolkit`/`pygments` works correctly
✅ **Dependency split**: Clean separation between core and terminal dependencies
✅ **Fail fast**: Missing dependencies cause immediate import errors, no graceful degradation