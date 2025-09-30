# Expected Failures (XFAIL) in PyLog

This document enumerates all tests marked as expected failures (`@pytest.mark.xfail`) in the PyLog codebase, along with their reasons and categorization.

## Summary

**Total XFAIL Tests**: 22 across 6 functional categories

## Complete Enumeration

### 1. Catch/Throw Exception Handling (10 tests)

Exception handling in PyLog is partially implemented but has several complex edge cases around state restoration and interaction with other language features.

#### Core Exception Mechanism Issues (4 tests)

| Test | File | Reason |
|------|------|--------|
| `test_catch_with_streaming` | `test_exception_handling.py` | Catch with streaming cursors not fully implemented |
| `test_catch_unification_failure` | `test_exception_handling.py` | Catch unification failure case not fully handled |
| `test_catch_with_cut` | `test_exception_handling.py` | Catch with cut interaction has edge cases |
| `test_catch_restores_state` | `test_exception_handling.py` | State restoration in catch has remaining issues |

#### Advanced Exception Semantics (6 tests)

| Test | File | Reason |
|------|------|--------|
| `test_list_unification_restoration` | `test_catch_throw_comprehensive.py` | List unification restoration still has issues |
| `test_catch_creates_cut_barrier` | `test_catch_throw_comprehensive.py` | Cut barrier semantics not fully implemented |
| `test_catch_at_choice_points` | `test_catch_throw_comprehensive.py` | Catch at choice points not fully working |
| `test_streaming_cursor_restoration` | `test_catch_throw_comprehensive.py` | **Design choice**: catch prunes in-scope CPs including streaming cursors |
| `test_recursive_predicate_with_catch` | `test_catch_throw_comprehensive.py` | Recursive predicates with catch have edge cases |
| `test_iso_ball_copy_semantics` | `test_catch_throw_comprehensive.py` | **Design choice**: PyLog uses reification not ISO copy semantics |

### 2. REPL/Debug Integration (5 tests)

The REPL debugging features require full implementation of the trace and spy systems.

#### Trace Implementation (4 tests)

| Test | File | Reason |
|------|------|--------|
| `test_trace_recursive_predicate` | `test_repl_debug_integration.py` | Requires full trace implementation |
| `test_trace_with_backtracking` | `test_repl_debug_integration.py` | Requires full trace implementation |
| `test_json_trace_format` | `test_repl_debug_integration.py` | Requires full trace implementation |
| `test_trace_sampling` | `test_repl_debug_integration.py` | Requires full trace implementation |

#### Spy Points (1 test)

| Test | File | Reason |
|------|------|--------|
| `test_multiple_spypoints` | `test_repl_debug_integration.py` | Requires full spy implementation |

### 3. CLP(FD) Reification Scenarios (3 tests)

Advanced reification scenarios require additional operators and constraint handling capabilities.

#### Missing Operators (2 tests)

| Test | File | Reason |
|------|------|--------|
| `test_optional_task_scheduling` | `test_clpfd_reification_scenarios.py` | Requires `#\/` (disjunction) operator not yet implemented |
| `test_resource_constrained_scheduling` | `test_clpfd_reification_scenarios.py` | Requires `#\/` (disjunction) operator not yet implemented |

#### Complex Constraints (1 test)

| Test | File | Reason |
|------|------|--------|
| `test_path_with_optional_edges` | `test_clpfd_reification_scenarios.py` | Complex path constraints not fully working |

### 4. CLP(FD) All-Different Constraint (3 tests)

The `all_different` constraint requires integration with the unification hook system for automatic propagation.

| Test | File | Reason |
|------|------|--------|
| `test_unification_after_posting` | `test_clpfd_all_different.py` | Requires hook integration for all_different constraint |
| `test_unify_to_int_propagation` | `test_clpfd_all_different.py` | Requires hook integration for automatic propagation on unification |
| `test_aliasing_after_posting` | `test_clpfd_all_different.py` | Requires hook integration for all_different constraint |

### 5. Trace Filter Integration (1 test)

| Test | File | Reason |
|------|------|--------|
| `test_step_id_increments_only_for_emitted_events_integration` | `test_filters.py` | PortsTracer not yet filter-aware; enable when integrated |

### 6. Metrics Integration (1 test)

| Test | File | Reason |
|------|------|--------|
| `test_metrics_track_exceptions_integration` | `test_metrics.py` | throw/catch integration not yet wired in engine |

## Categorization by Implementation Status

### Technical Implementation Gaps (15 tests)

These tests fail due to incomplete implementation of core systems:

- **Exception handling mechanics**: 10 tests (core catch/throw system)
- **CLP(FD) hook integration**: 3 tests (all_different constraint)
- **Trace system integration**: 1 test (filter awareness)
- **Metrics integration**: 1 test (exception tracking)

### Missing Features (5 tests)

These tests require features that are planned but not yet implemented:

- **REPL debug features**: 5 tests (full trace + spy implementation)

### Design Decisions (2 tests)

These tests represent intentional divergence from ISO Prolog standards:

- **`test_streaming_cursor_restoration`**: PyLog's design choice to prune choice points including streaming cursors
- **`test_iso_ball_copy_semantics`**: PyLog uses reification instead of ISO's copy semantics for exception balls

## Implementation Priority

Based on the categorization, the recommended implementation order is:

1. **Exception Handling System** (10 tests) - Core language feature affecting multiple areas
2. **CLP(FD) Hook Integration** (3 tests) - Critical for constraint propagation
3. **REPL Debug Features** (5 tests) - Important for development experience
4. **Integration Gaps** (2 tests) - System integration improvements

## Running XFAIL Tests

To run and inspect the expected failures:

```bash
# Run all XFAIL tests to see current status
uv run pytest -v --tb=short | grep XFAIL

# Run specific category (example: exception handling)
uv run pytest prolog/tests/unit/test_exception_handling.py prolog/tests/unit/test_catch_throw_comprehensive.py -v

# Skip XFAIL tests entirely
uv run pytest --runxfail
```

## Notes

- **Design Decision Tests**: The 2 tests marked as design decisions (`test_streaming_cursor_restoration`, `test_iso_ball_copy_semantics`) may remain XFAIL permanently as they represent intentional deviations from ISO Prolog.
- **Implementation Tests**: The remaining 20 tests represent genuine implementation gaps that should be addressed as the project progresses.
- **Dependencies**: Many tests have dependencies on each other - fixing the core exception handling system will likely enable several other tests to pass.