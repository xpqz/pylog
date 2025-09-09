# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is PyLog, a tree-walking Prolog interpreter in Python with an eventual CLP(FD) (Constraint Logic Programming over Finite Domains) layer. The system is designed for learning and debuggability first, with stable interfaces that survive later stages.

## Project Structure

```
prolog/
  parser/                # Lark grammar and reader for operators
    grammar.lark
    reader.py
  ast/
    terms.py             # Core term types: Atom, Int, Var, Struct, List
    pretty.py
  unify/
    store.py             # Cells, Store, deref(), bind(), trail operations
    unify.py
    occurs.py
  engine/
    engine.py            # Main run loop, goal stack, choicepoints, cut, throw/catch
    indexing.py          # First-argument and type switching
    builtins_iso.py      # ISO builtins: true/fail/!/=/\=, var/nonvar, etc.
    tracer.py            # Debugging with call/exit/redo/fail ports
  attrs/
    api.py               # Attributed variables: put_attr/get_attr/del_attr
    hooks.py             # Registration and attr_unify dispatch
  clpfd/                # Constraint Logic Programming over Finite Domains
    domain.py            # Domain representation with intervals/holes/bitsets
    queue.py             # PropagationQueue with priorities
    props/               # Propagator implementations
      linear.py
      compare.py
      sum.py
      alldiff.py
      reif.py            # Reification support
    label.py             # Labeling strategies
    explain.py           # Debugging: who pruned what
  debug/
    snapshot.py          # Store/trail/choicepoints dump
    graphviz.py          # export_constraint_graph()
  tests/
    unit/                # Small orthogonal tests
    scenarios/           # Multi-file programs
```

## Development Commands

### Tools
- **Package management**: `uv` (fast Python package manager)
- **Code formatting**: `black` (run before committing)

### Running Tests
```bash
# Run all tests
uv run pytest

# Run specific test file
uv run pytest tests/unit/test_unify.py

# Run with verbose output
uv run pytest -v

# Run specific test function
uv run pytest tests/unit/test_unify.py::test_occurs_check
```

### Code Formatting
```bash
# Format all Python files
uv run black .

# Check formatting without changing files
uv run black --check .
```

### Development Setup
```bash
# Install uv if not already installed
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install project and dependencies
uv sync

# Add a new dependency
uv add <package-name>

# Add a development dependency
uv add --dev <package-name>
```

## Architecture Principles

### Core Design Rules
1. **No Python recursion** - All execution uses explicit stacks to avoid recursion limit issues
2. **Centralized mutation** - Only `bind()` mutates the store; every backtrackable change is trailed
3. **Stable interfaces** - Terms, store, trail, and engine hooks remain stable across all stages
4. **Engine purity** - CLP(FD) integrates via attributed variables, not engine modifications

### Key Data Structures

#### Terms (AST)
- `Atom(name: str)` - Prolog atoms
- `Int(value: int)` - Integer values  
- `Var(id: int, hint: str)` - Variables identified by integer ID
- `Struct(functor: str, args: tuple)` - Compound terms
- `List(items: tuple, tail: Term)` - Prolog lists

#### Store and Trail
- `Store` manages variable cells with union-find structure
- Trail entries track all mutations for backtracking:
  - `('parent', varid, old_parent)` - Union-find parent changes
  - `('bind', varid, old_cell)` - Variable bindings
  - `('attr', varid, module, old_value)` - Attribute changes
  - `('domain', varid, old_domain)` - Domain updates

### Implementation Stages

The project follows a staged development plan where each stage builds on stable interfaces:

- **Stage -1**: Unifier Workbench - Core unification engine
- **Stage 0**: Explicit stacks and choicepoints
- **Stage 1**: Minimal ISO builtins (operator-free)
- **Stage 1.5**: Operator support via reader
- **Stage 2**: Indexing for performance
- **Stage 3**: Debug and observability tools
- **Stage 4**: Attributed variables mechanism
- **Stage 5**: CLP(FD) core with propagation
- **Stage 5.5**: Reification support
- **Stage 6**: Global constraints (all_different, etc.)

### Testing Strategy

- **Property tests** for unification correctness (idempotence, symmetry)
- **Trail inversion** tests ensure correct backtracking
- **Stress tests** with millions of choicepoints
- **Propagation confluence** tests for CLP(FD)
- **Occurs-check modes** for both on/off behavior
- **SWI-Prolog baseline tests** - When writing unit tests for Prolog semantics, add corresponding baseline tests using the `swi` fixture to validate against SWI-Prolog behavior. Mark these with `@pytest.mark.swi_baseline`. This ensures PyLog conforms to ISO/SWI semantics.

## Implementation Guidelines

### Project Rules
1. **No workarounds** - Fix root causes, not symptoms
2. **Dependencies at top** - All imports at file top, no conditional imports, fail fast on missing dependencies
3. **No backwards compatibility** - Focus on clean design for new features
4. **No backup files** - Git handles versioning, no suffixes or backup copies
5. **Direct communication** - No unnecessary affirmations or compliments
6. **Frequent commits** - Commit working code frequently, small logical changes

### When Adding New Features
1. Keep files small and single-purpose
2. Expose stable function signatures in skeletons
3. Always include unit tests alongside code changes
4. Use tracer/snapshots for debugging

### Debugging with SWI-Prolog Ground Truth
When debugging execution traces, you can use `trace_dump.pl` to compare PyLog's behavior with SWI-Prolog:

1. Load the trace module and your test program:
```prolog
?- consult(trace_dump), consult(your_test).
```

2. Run a query with JSON trace output:
```prolog
?- with_trace_json((your_goal, fail ; true), 'trace.jsonl').
```

3. The output file contains one JSON object per trace event with ports (call/exit/redo/fail), goals, and timing.

This is useful for verifying that PyLog's backtracking and unification behavior matches standard Prolog semantics.

### Unification and Binding
- Use `deref()` for iterative union-find walks
- Only `bind()` should mutate the store
- Trail every mutation for correct backtracking
- Support both occurs-check on/off modes

### CLP(FD) Development
- Domains use adaptive representation (interval/holes/bitset)
- Propagators must be monotonic (only prune, never expand)
- Use revision counters to detect changes
- Implement self-requeue guards to prevent loops

### Performance Considerations
- Path compression only during mutation (trailed)
- Bitset representation for domains ≤64 values
- First-argument indexing for clause selection
- Trail segmentation per choicepoint for fast undo

### Process
- Don't back files up by copying! We use git for versioning.
- For each new development stage, create a new git branch first.
- We practice TDD: 
    - write tests first that demonstrate the desired behaviour
    - pause for human review of the tests
    - progress the implementation until the tests succeed. 
    - NEVER tweak a test to "fit" the behaviour, unless the test is demonstrably broken.
- Maintain progress in docs/TODO.md
- NEVER EVER CHANGE THE DEFAULT BRANCH ON GIT OR GITHUB!
- When creating PRs or commits, DO NOT mention Claude, Anthropic, or AI assistance in the message

### GitHub Issue Workflow
Follow this process for each GitHub issue:

1. **Pick an issue** - Note its ID number
2. **Create branch** - Name format: `{ID}-{slug-derived-from-issue-title}`
   - Example: `9-parser-grammar-basic-terms`
3. **Write tests FIRST** - STOP after writing tests for human review
4. **Commit approved tests** - Only after review approval
5. **Implement until tests pass** - Make the tests green
6. **Run complete test suite** - No regressions tolerated!
7. **Create PR** - Make an orderly PR, squashing commits if necessary. DON'T  mention Claude or AI in the PR message
8. **Verify CI** - Ensure all CI tests pass fully
9. **Await PR review** - Wait for human review
10. **Merge and update** - After approval, merge PR and update the epic
- Don't use /tmp and other locations outside the current repository
