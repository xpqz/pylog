## Summary

Implements **Stage 5 Phase 5: CLP(FD) Labeling Strategies** for systematic search through constraint variable domains. This PR also addresses a critical architectural issue with dual tokenizers that was preventing CLP(FD) operators from parsing correctly.

## Problem Solved

1. **Missing labeling functionality**: PyLog's CLP(FD) system could set up constraints but had no way to systematically search for solutions by trying values from variable domains.

2. **Dual tokenizer architecture**: The Reader class had its own hardcoded tokenizer separate from the Lark grammar, requiring operators to be defined in two places and causing CLP(FD) operators to fail parsing.

3. **Backtracking bug**: Initial implementation lost solutions when backtracking through labeling alternatives due to empty goal stack windows between branches.

## User-Facing Changes

### New Predicates
- `label/1`: Label variables with default strategies (first variable, minimum value)
- `labeling/2`: Label variables with specified strategies

### Supported Strategies

**Variable Selection:**
- `first`: Select variables in order (default)
- `first_fail` (`ff`): Select variable with smallest domain first
- `most_constrained`: Select variable with most constraints
- `smallest`: Alias for first_fail
- `largest`: Select variable with largest domain first

**Value Selection:**
- `indomain_min`: Try minimum value first (default)
- `indomain_max`: Try maximum value first
- `indomain_middle`: Try middle value first, then alternate outward
- `indomain_random`: Try values in random order
- `indomain_split`: Binary search style (middle first, then recursively split)

**Random Seed Control:**
- `seed(N)`: Configure random seed for `indomain_random` strategy
- Example: `labeling([indomain_random, seed(12345)], [X, Y])`
- Without seed: uses deterministic default (42) for reproducible tests

### Example Usage

```prolog
% Basic labeling
?- X in 1..3, Y in 1..3, X #< Y, label([X, Y]).
X = 1, Y = 2 ;
X = 1, Y = 3 ;
X = 2, Y = 3.

% With strategies
?- X in 1..10, labeling([first_fail, indomain_max], [X]).
X = 10 ;
X = 9 ;
...

% Reproducible randomization
?- X in 1..5, labeling([indomain_random, seed(42)], [X]).
```

## Technical Changes

### Architecture Fixes

1. **Unified Tokenizer** (`reader.py`)
   - Replaced hardcoded `PATTERNS` list with dynamic generation from `operators.py`
   - Made `operators.py` the single source of truth for all operators
   - CLP(FD) operators (`in`, `..`, `#=`, `#<`, etc.) now parse correctly

2. **Labeling Implementation** (`label.py`)
   - Non-recursive implementation using explicit goal stack
   - Builds disjunctions with embedded continuations: `(X=V1, label(Vars)) ; (X=V2, label(Vars))`
   - Avoids empty goal stack windows that caused lost solutions
   - Deterministic single-value optimization

3. **Engine Integration** (`engine.py`)
   - Registered `label/1` and `labeling/2` builtins
   - No special control flow needed - uses standard disjunction machinery

### Files Changed

- **New Files:**
  - `prolog/clpfd/label.py`: Core labeling implementation (386 lines)
  - `prolog/tests/unit/test_clpfd_labeling.py`: Comprehensive test suite (218 lines)
  - `prolog/tests/unit/test_clpfd_labeling_simple.py`: Direct API tests (227 lines)

- **Modified Files:**
  - `prolog/parser/reader.py`: Dynamic tokenizer generation
  - `prolog/parser/operators.py`: Added CLP(FD) operators
  - `prolog/engine/engine.py`: Registered labeling builtins
  - `prolog/engine/builtins_clpfd.py`: Fixed `in/2` empty domain handling
  - `docs/ARCH.md`, `docs/vm-loop.md`: Updated documentation

## Breaking Changes

None. This is a pure addition of new functionality.

## Migration Guide

N/A - No existing functionality changed.

## How To Verify It

- [x] Run labeling tests: `uv run pytest prolog/tests/unit/test_clpfd_labeling.py -v`
  - ✅ 19 tests pass
- [x] Run full test suite: `uv run pytest prolog/tests/unit/`
  - ✅ 6165 passed, 7 skipped, 17 xfailed
- [ ] Manual testing of labeling strategies (examples above)
- [x] Verify CLP(FD) operators parse:
  ```python
  from prolog.parser.reader import Reader
  reader = Reader()
  reader.read_term("X in 1..10")  # Should work
  reader.read_term("X #= Y + 1")  # Should work
  ```
  - ✅ All CLP(FD) operators parse correctly

## Performance Impact

- Tokenizer generation at startup: negligible overhead (~1ms)
- Labeling performance: O(d^n) where d = domain size, n = number of variables (expected for exhaustive search)
- Memory: minimal - uses existing goal stack without additional data structures

## Security Considerations

None identified.

## Changelog Entry

```
### Added
- CLP(FD) labeling with `label/1` and `labeling/2` predicates
- Variable selection strategies: first, first_fail, most_constrained, smallest, largest
- Value selection strategies: indomain_min, indomain_max, indomain_middle, indomain_random, indomain_split
- Configurable random seed for indomain_random strategy via seed(N) option
- Dynamic tokenizer generation from operators.py for consistent operator handling

### Fixed
- CLP(FD) operators now parse correctly (in, .., #=, #<, etc.)
- Backtracking through labeling alternatives now finds all solutions
- Empty domain handling in in/2 predicate
```

## Related Issues

Fixes #123

## Review Notes

The key insight for fixing the backtracking bug was to embed continuations within each disjunction branch rather than pushing them separately. This ensures the goal stack never becomes empty between branches, which was causing the lost solutions.

The tokenizer unification was necessary because the Reader's hardcoded patterns didn't include CLP(FD) operators, causing parsing failures. The new dynamic generation ensures all operators defined in `operators.py` are automatically recognized.