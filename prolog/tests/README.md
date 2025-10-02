# PyLog Test Suite

This directory contains the comprehensive test suite for PyLog, the tree-walking Prolog interpreter.

## ⚠️ CRITICAL: Test Integrity Rules

- **TESTS ARE SACRED - DO NOT MODIFY TO MAKE THEM PASS.**
- **DO NOT MARK AS XFAIL WITHOUT EXPLICIT CONFIRMATION**

See docs/xfail.md

### Quick Reference - When Tests Fail:

1. **ASSUME THE TEST IS CORRECT** - The implementation is wrong, not the test
2. **INVESTIGATE THE ROOT CAUSE** - Why doesn't the implementation meet the test's expectations?
3. **FIX THE IMPLEMENTATION** - Make the code satisfy the test requirements
4. **NEVER "SIMPLIFY" FAILING TESTS** - They reveal missing functionality

### Only Valid Reasons to Change a Test:
- ✅ Provably incorrect logic (mathematical/logical error in the test)
- ✅ Invalid assumptions (contradicts documented specification)
- ✅ Clear typos/syntax errors

### Forbidden Actions:
- ❌ Changing expected values to match actual output
- ❌ Reducing test scope because full test fails
- ❌ "Simplifying" complex test cases
- ❌ Removing failing assertions

**For complete rules, see the "Test Integrity Rules" section in `/CLAUDE.md`**

## Test Organization

```
prolog/tests/
├── unit/          # Unit tests for individual components
├── scenarios/     # Integration/scenario tests
└── benchmarks/    # Performance benchmarks
```

## Running Tests

```bash
# Run all tests
uv run pytest

# Run specific test file
uv run pytest prolog/tests/unit/test_specific.py

# Run with verbose output
uv run pytest -v

# Run specific test function
uv run pytest prolog/tests/unit/test_file.py::test_function
```

## Test Categories

### Unit Tests (`unit/`)
- Individual component testing
- Fast execution
- Isolated functionality
- Mathematical correctness

### Scenario Tests (`scenarios/`)
- End-to-end integration
- Real-world use cases
- Cross-component interactions

### Benchmarks (`benchmarks/`)
- Performance regression testing
- Scalability validation
- Resource usage monitoring

## Writing New Tests

When adding tests:

1. **Follow naming conventions**: `test_*.py` files, `test_*` functions
2. **Use appropriate categories**: Unit tests in `unit/`, integration in `scenarios/`
3. **Write clear assertions**: Tests should clearly express expected behavior
4. **Include edge cases**: Test boundary conditions and error cases
5. **Document complex tests**: Explain the scenario being tested
6. **UNDER NO CIRCUMSTANCES ADD IMPORTS ANYWHERE EXCEPT AT THE TOP OF THE FILE**

## SWI-Prolog Baseline Tests

For Prolog semantics tests, add corresponding baseline tests using the `swi` fixture to validate against SWI-Prolog behavior. Mark these with `@pytest.mark.swi_baseline`.

## Remember: Tests Define the Specification

The test suite is the executable specification of PyLog's behavior. Treat it as sacred documentation that must be preserved and respected.
