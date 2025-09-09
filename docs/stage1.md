# Stage 1: Minimal ISO Builtins (Operator-free)

## Overview

Stage 1 implements the core Prolog functionality with minimal ISO builtins, deliberately using operator-free syntax to establish a solid foundation before introducing operator handling in Stage 1.5.

## Completed Features

### Core Engine
- **Goal stack execution**: Non-recursive implementation using explicit stacks
- **Backtracking**: Choice points with proper trail management
- **Unification**: Full unification with occurs check support
- **Cut (!/)**: Removes choice points up to cut parent
- **Variable binding**: Union-find based store with path compression

### ISO Builtins Implemented

#### Control Flow
- `true/0`: Always succeeds
- `fail/0`: Always fails
- `!/0`: Cut - removes choice points
- `','/2`: Conjunction (and)
- `';'/2`: Disjunction (or)
- `'->'/2`: If-then construct
- `call/1`: Meta-call predicate
- `once/1`: Deterministic execution (first solution only)

#### Term Inspection
- `var/1`: Test if variable
- `nonvar/1`: Test if not variable
- `atom/1`: Test if atom
- `integer/1`: Test if integer
- `compound/1`: Test if compound term
- `atomic/1`: Test if atomic (atom or integer)

#### Term Comparison
- `=/2`: Unification
- `\\=/2`: Not unifiable
- `==/2`: Structural equality
- `\\==/2`: Structural inequality
- `@</2`, `@=</2`, `@>/2`, `@>=/2`: Term ordering

#### Term Construction/Deconstruction
- `functor/3`: Get/construct functor and arity
- `arg/3`: Access argument of compound term
- `=../2`: Univ - convert between term and list
- `copy_term/2`: Create copy with fresh variables

#### Arithmetic
- `is/2`: Arithmetic evaluation
- `</2`, `=</2`, `>/2`, `>=/2`: Numeric comparison
- `=:=/2`, `=\\=/2`: Arithmetic equality/inequality
- Arithmetic operations: `+`, `-`, `*`, `/`, `mod`, `abs`, `min`, `max`

#### Exception Handling
- `throw/1`: Throw an exception
- `catch/3`: Catch and handle exceptions

### Library Predicates

#### List Operations
- `append/3`: List concatenation/decomposition
- `member/2`: List membership/generation
- `reverse/2`: List reversal

#### Numeric
- `between/3`: Integer range generation/checking

### Error Handling (Dev Mode)

Stage 1 operates in "dev mode" for better developer experience:
- **Undefined predicates**: Return false instead of throwing existence_error
- **Type errors**: Fail gracefully instead of throwing type_error
- **Arithmetic errors**: Fail instead of throwing evaluation_error
- **Uncaught exceptions**: Fail with proper cleanup

This approach prioritizes development speed and debugging ease. ISO-compliant error handling will be added in a future stage.

## Operator-Free Syntax

Stage 1 requires all operators to be written in quoted functor form:

```prolog
% Arithmetic
?- is(X, '+'(2, 3)).          % X = 5

% Comparison
?- '>'(5, 3).                 % true
?- '=<'(X, 10).               % Constraints X =< 10

% Lists use standard notation
?- append([1,2], [3,4], L).   % L = [1,2,3,4]
```

This approach:
1. Simplifies the parser implementation
2. Makes operator precedence explicit
3. Provides a solid foundation for Stage 1.5 operator support

## Testing

### Test Coverage
- **Unit tests**: Comprehensive coverage of all builtins
- **Error handling tests**: Dev mode behavior verification
- **Performance tests**: Benchmarks for parser, engine, and predicates
- **Acceptance tests**: End-to-end validation of Stage 1 requirements

### Running Tests
```bash
# Run all Stage 1 tests
uv run pytest prolog/tests/unit/test_stage1_validation.py

# Run error handling tests
uv run pytest prolog/tests/unit/test_error_handling.py

# Run performance benchmarks
uv run pytest prolog/tests/unit/test_performance.py
```

## REPL Usage

The Stage 1 REPL provides an interactive environment:

```bash
# Start the REPL
uv run python -m prolog.repl

# Example session
?- append([1,2], [3,4], X).
X = [1, 2, 3, 4]

?- member(X, [a,b,c]).
X = a ;
X = b ;
X = c

?- catch(throw(error), E, true).
E = error
```

Features:
- Tab completion for predicates
- Multi-line query support
- Solution navigation (`;` for next, `.` to stop)
- Query history
- Syntax highlighting

## Implementation Notes

### Architecture Principles
- **No Python recursion**: All execution uses explicit stacks
- **Centralized mutation**: Only `bind()` mutates the store
- **Stable interfaces**: Term types and engine hooks remain stable
- **Engine purity**: Features integrate via hooks, not engine changes

### Performance Characteristics
- Parser: ~2000 facts/second
- Unification: O(n) with path compression
- Backtracking: O(1) choice point creation/restoration
- Memory: Efficient trail segmentation per choice point

## Next Steps (Stage 1.5)

- Operator support via reader
- Precedence and associativity handling
- Infix, prefix, and postfix operators
- User-defined operators via `op/3`

## References

- ISO Prolog Standard (ISO/IEC 13211-1:1995)
- SWI-Prolog documentation for library predicates
- The Art of Prolog (Sterling & Shapiro) for implementation techniques