# CRITICAL: CLP(FD) Performance Issues - Orders of Magnitude Slower Than SWI-Prolog

## Summary

PyLog's CLP(FD) implementation has severe performance issues that make it orders of magnitude slower than SWI-Prolog for constraint satisfaction problems. Problems that SWI-Prolog solves in < 1ms take PyLog 10+ seconds or hang completely.

## Performance Comparison

### SWI-Prolog Baseline (Manual Testing)
```prolog
% 8 variables with all_different - Time: < 1ms
S in 1..9, E in 0..9, N in 0..9, D in 0..9,
M in 1..9, O in 0..9, R in 0..9, Y in 0..9,
all_different([S, E, N, D, M, O, R, Y]),
labeling([ff], [S, E, N, D, M, O, R, Y]).

% 9 variables with all_different - Time: < 1ms
A in 1..9, B in 1..9, C in 1..9, D in 1..9, E in 1..9,
F in 1..9, G in 1..9, H in 1..9, I in 1..9,
all_different([A, B, C, D, E, F, G, H, I]),
labeling([ff], [A, B, C, D, E, F, G, H, I]).
```

### PyLog Performance (Current Implementation)
```python
# 4 variables with all_different - Time: 219ms
S in 1..9, E in 0..9, N in 0..9, D in 0..9,
all_different([S, E, N, D]),
labeling([first_fail], [S, E, N, D]).

# 8 variables with all_different - Time: >10s (timeout/hang)
# 9 variables with all_different - Time: >10s (timeout/hang)
```

## Affected Tests

The following tests currently timeout or hang:

1. `test_send_more_money_digits` in `prolog/tests/scenarios/test_clpfd_scenarios.py`
2. `test_magic_square_3x3_sum` in `prolog/tests/scenarios/test_clpfd_scenarios.py`

## Root Cause Analysis Needed

The performance gap suggests fundamental algorithmic issues in one or more of:

### 1. `all_different` Constraint Implementation
- **File**: `prolog/clpfd/props/alldiff.py`
- **Suspected Issues**:
  - Inefficient arc consistency algorithm
  - Poor domain pruning strategies
  - Missing advanced propagation techniques (Hall intervals, etc.)
  - Excessive constraint graph updates

### 2. Labeling/Search Implementation
- **File**: `prolog/clpfd/label.py`
- **Suspected Issues**:
  - Inefficient variable ordering (`first_fail` strategy)
  - Poor value selection heuristics
  - Expensive backtracking operations
  - Missing constraint propagation during search

### 3. CLP(FD) Infrastructure
- **Files**: `prolog/clpfd/queue.py`, `prolog/clpfd/api.py`, `prolog/engine/engine.py`
- **Suspected Issues**:
  - Expensive propagation queue operations
  - Inefficient domain representation/operations
  - Poor integration with unification/trail system
  - Excessive memory allocations

### 4. Unification/Trail System
- **Files**: `prolog/unify/store.py`, `prolog/engine/runtime.py`
- **Suspected Issues**:
  - Expensive trailing operations during constraint propagation
  - Inefficient variable dereferencing
  - Poor memory management for constraint state

## Performance Requirements

Based on SWI-Prolog baselines, PyLog should achieve:

- **4-variable all_different**: < 10ms (currently 219ms - **22x slower**)
- **8-variable all_different**: < 100ms (currently >10s - **>100x slower**)
- **9-variable all_different**: < 100ms (currently >10s - **>100x slower**)

## Investigation Steps

1. **Profile constraint posting**: Time spent in `all_different` constraint creation
2. **Profile propagation**: Time spent in propagation queue operations
3. **Profile labeling**: Time spent in variable/value selection and search
4. **Profile unification**: Time spent in trailing and dereferencing
5. **Compare algorithms**: Study SWI-Prolog's CLP(FD) implementation strategies

## Test Infrastructure

SWI-Prolog baseline tests have been added in:
- `prolog/tests/scenarios/test_clpfd_scenarios.py::TestPerformanceBaselines`

These establish performance targets and can track optimization progress.

## Priority

**CRITICAL** - This makes CLP(FD) practically unusable for real-world constraint problems. The performance gap indicates fundamental algorithmic issues that need immediate investigation and resolution.

## Files to Investigate

```
prolog/clpfd/props/alldiff.py      # All-different constraint implementation
prolog/clpfd/label.py              # Labeling/search strategies
prolog/clpfd/queue.py              # Propagation queue management
prolog/clpfd/api.py                # Domain operations and CLP(FD) API
prolog/clpfd/domain.py             # Domain representation and operations
prolog/unify/store.py              # Variable store and trailing
prolog/engine/engine.py            # Engine integration with CLP(FD)
prolog/engine/runtime.py           # Trail operations
```

## Acceptance Criteria

- [ ] 4-variable all_different problems solve in < 10ms
- [ ] 8-variable all_different problems solve in < 500ms
- [ ] 9-variable all_different problems solve in < 1s
- [ ] All scenario tests in `test_clpfd_scenarios.py` pass without timeouts
- [ ] Performance within 10x of SWI-Prolog for comparable problems

---

*Discovered during testing of issue #126 CLP(FD) implementation.*