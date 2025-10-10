# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is PyLog, a tree-walking Prolog interpreter in Python with an eventual CLP(FD) (Constraint Logic Programming over Finite Domains) layer. The system is designed for learning and debuggability first, with stable interfaces that survive later stages.

## ⚠️ CRITICAL: Test Integrity Rules

**NEVER MODIFY TESTS TO MAKE THEM PASS - Period.**

### Ironclad Test Integrity Rules:

1. **Tests are Sacred**: Tests define the expected behavior. If a test fails, the code is wrong, not the test.

2. **Only Three Valid Reasons to Change a Test**:
   - **Provably incorrect logic**: The test itself contains a mathematical or logical error
   - **Invalid assumptions**: The test assumes behavior that contradicts the specification
   - **Typos/syntax errors**: Clear mistakes in test code (like wrong variable names)

3. **Required Evidence for Test Changes**:
   - **Written justification**: Must document exactly what was wrong with the original test
   - **Reference to specification**: Show how the test contradicts documented behavior
   - **Alternative verification**: Demonstrate the correct behavior through independent means

4. **Forbidden "Fixes"**:
   - ❌ Changing expected values because they don't match actual output
   - ❌ Reducing test scope because full test fails
   - ❌ Simplifying test cases because they're "too complex"
   - ❌ Removing assertions because they fail
   - ❌ Making tests "more realistic" when they expose bugs

5. **Mandatory Process When Tests Fail**:
   - **Step 1**: Assume the test is correct and the implementation is wrong
   - **Step 2**: Investigate why the implementation doesn't meet the test's expectations
   - **Step 3**: Fix the implementation to satisfy the test
   - **Step 4**: Only if Step 3 is impossible, then question if the test is wrong
   - **Step 5**: If changing a test, require explicit approval with written justification

### Red Flag Phrases That Should Trigger Immediate Stop:
- "Let me simplify this test..."
- "This test is too complex, let me make it more realistic..."
- "The test expects X but that's not how it actually works..."
- "Let me adjust the expected values..."
- "This test is causing issues, let me fix it..."

### Correct Mindset:
- **Tests are the specification in executable form**
- **Failing tests reveal implementation gaps, not test problems**
- **Complex tests often catch the most important bugs**
- **Test failures are valuable information about what needs to be built**

## Project Structure

```
prolog/              <-- ALL CODE UNDER prolog/
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
    unit/                <-- UNIT TESTS GO HERE (prolog/tests/unit/)
    scenarios/           <-- SCENARIO TESTS GO HERE (prolog/tests/scenarios/)
```

**NEVER use tests/ at root level - all tests MUST go under prolog/tests/**

## Development Commands

### Tools
- **Package management**: `uv` (fast Python package manager)
- **Code formatting**: `black` (run before committing)

### Test File Locations
**IMPORTANT**: All unit tests MUST go in `prolog/tests/unit/` not `tests/unit/`
- Test files should be named `test_*.py`
- Unit tests go in: `prolog/tests/unit/`
- Integration/scenario tests go in: `prolog/tests/scenarios/`
- NEVER create a `tests/` directory at the repository root

### Running Tests
```bash
# Run all tests
uv run pytest

# Run specific test file (note the prolog/ prefix)
uv run pytest prolog/tests/unit/test_unify.py

# Run with verbose output
uv run pytest -v

# Run specific test function
uv run pytest prolog/tests/unit/test_unify.py::test_occurs_check
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
5. **Conformance** - don't speculate about Prolog behavior, verify. You have access to the swi-prolog implementation, available as `swipl`. 

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
2. **🚨 CRITICAL: All imports at file top** - NO conditional imports anywhere, including tests. Import everything needed upfront, fail fast on missing dependencies
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
- NEVER use --no-verify when committing! Always let pre-commit hooks run and fix any issues they find

### GitHub Issue Workflow

In any git and GitHub messaging (commit messages, PR messages, issues, comments etc), we maintain a terse, professional tone:

1. **Never make unproven claims**: don't make claims about the validity, effectiveness or awesomeness of your changes in a commit or message. By definition, that is determined by the CI results, which you can't see yet. Explain what was done, and why.
2. **Never use emoji symbols**: we're not 14-year-olds on Instagram here. No green ticks, no red crosses, no smileys, no symbols.
3. **Brevity**: issues and commit messages are written for co-workers. Respect their time. Obviously, be complete, but express yourself in a professional, concise tone.

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
- You MUST stop for reviews before ANY implementation
