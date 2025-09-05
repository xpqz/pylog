# Stage 1: Minimal ISO Builtins (operator-free)

## Overview
Make PyLog a usable Prolog core with essential ISO predicates, fact/rule parsing, and proper solution presentation. This stage implements the minimal set required by PLAN.md while maintaining operator-free syntax.

## Status: NOT STARTED

## Objectives
1. Implement operator-free parser and pretty-printer
2. Complete core ISO builtins (=../2, functor/3, arg/3)
3. Implement throw/1 and catch/3 exception system
4. Define error behavior (dev mode initially, ISO mode later)
5. Validate with library predicates (append/3, member/2, reverse/2, between/3)

## Deliverables

### 1. Parser Implementation (operator-free)
- **Location**: `prolog/parser/`
- **Components**:
  - Lark grammar for atoms, integers, variables, lists, structures, clauses
  - Parser module to convert text → AST
  - Support for facts, rules, and queries
  - No operator precedence (all operators written as functors)
  - Round-trip property with pretty-printer (modulo spacing)
  
### 2. Pretty Printer
- **Location**: `prolog/ast/pretty.py`
- **Features**:
  - Stable variable names by creation order
  - List notation `[H|T]` and `[1,2,3]`
  - Structure formatting `foo(bar, baz)`
  - Solution bindings `X = value`
  - Basic handling only (cyclic terms deferred)

### 3. Core ISO Builtins

#### Already Implemented (Stage 0) ✅
- `true/0` (det) - always succeeds
- `fail/0` (det) - always fails
- `!/0` (det) - cut
- `call/1` (nondet) - meta-call
- `=/2` (semidet) - unification
- `\=/2` (semidet) - negated unification
- `var/1` (semidet) - variable test
- `nonvar/1` (semidet) - non-variable test
- `atom/1` (semidet) - atom test
- `integer/1` (semidet) - integer test
- `is/2` (det) - arithmetic evaluation
- `</2`, `>/2`, `=</2`, `>=/2` (semidet) - arithmetic comparison
- `=:=/2`, `=\=/2` (semidet) - arithmetic equality

#### New Builtins for Stage 1
- **Term Manipulation**:
  - `=../2` (semidet) - structure ↔ list conversion (univ)
  - `functor/3` (semidet) - extract/construct functor and arity
  - `arg/3` (semidet) - access structure arguments
  
- **Control**:
  - `once/1` (semidet) - find first solution only (implemented as `once(G) :- call(G), !.`)
  
- **Exception System**:
  - `throw/1` (det) - throw exception
  - `catch/3` (nondet) - catch exception with proper unwinding

### 4. Library Predicates (Prolog, not engine)
- **Location**: `prolog/lib/lists.pl` (new file)
- **Predicates**:
  - `append/3` - list concatenation
  - `member/2` - list membership
  - `reverse/2` - list reversal
  - `length/2` - list length
  - `between/3` - integer generation
- **Note**: These are acceptance tests, not engine builtins

### 5. Basic REPL
- **Location**: `prolog/repl.py` (new file)
- **Features**:
  - Load Prolog files
  - Run queries interactively
  - Show solutions with pretty-printer
  - `;` for next solution, `.` to stop
  - Trace mode hooks deferred to Stage 3

## Implementation Plan

### Phase 1: Parser and Pretty Printer
1. Create Lark grammar for operator-free Prolog
2. Implement pretty printer for basic terms
3. Test round-trip property (parse → pretty → parse)
4. Support clause parsing (facts and rules)

### Phase 2: Core Term Builtins
1. Implement `=../2` (univ) with proper trailing
2. Implement `functor/3` with determinism contracts
3. Implement `arg/3` with bounds checking
4. Implement `once/1` as thin wrapper
5. Add comprehensive tests

### Phase 3: Exception System
1. Implement `throw/1` with engine state
2. Implement `catch/3` with unwind semantics
3. Test exception propagation through choicepoints
4. Test backtracking resumption after handler

### Phase 4: Library Predicates
1. Write `append/3`, `member/2`, `reverse/2` in Prolog
2. Write `between/3` and `length/2`
3. Use as acceptance tests for engine
4. Verify solution order and completeness

### Phase 5: Basic REPL
1. Implement file loading
2. Add interactive query mode
3. Integrate pretty printer for solutions
4. Test with library predicates

### Phase 6: Integration and Documentation
1. Test operator-free programs end-to-end
2. Document error behavior (dev vs ISO mode)
3. Verify acceptance criteria from PLAN.md
4. Update documentation

## Architecture Considerations

### Error Policy (Dev Mode → ISO Mode)
- **Stage 1 (Dev Mode)**:
  - Undefined predicates: fail silently
  - Type errors: fail or minimal error
  - Arithmetic errors: fail or exception
  
- **Future (ISO Mode)**:
  - `instantiation_error` for unbound required args
  - `type_error(callable, X)` for non-callable in call/1
  - `existence_error(procedure, F/A)` for undefined predicates
  - `domain_error`, `evaluation_error` for arithmetic

### Determinism Contracts
Each builtin specifies its determinism:
- **det**: exactly one solution (true/0, fail/0, !/0, is/2, throw/1)
- **semidet**: at most one solution (=/2, \=/2, var/1, functor/3)
- **nondet**: zero or more solutions (call/1, catch/3)

### Trail Discipline
Builtins that may bind variables must trail:
- `=/2` - trails all bindings
- `=../2` - trails when constructing
- `functor/3` - trails when constructing
- `arg/3` - never binds (only extracts)

### Parser/AST Stability
- AST structure locked for Stage 1
- Stage 1.5 adds operator reader layer only
- No AST changes between stages

## Out of Scope (Explicitly Deferred)

### Deferred to Stage 1.5/2
- Operator support (reader layer)
- Meta-predicates: `findall/3`, `bagof/3`, `setof/3`
- Term ordering: `@</2`, `@>/2`, `@=</2`, `@>=/2`
- Term equality: `==/2`, `\==/2`
- Three-way comparison: `compare/3`

### Deferred to Stage 3+
- Full trace/debug system
- `halt/0`, `halt/1`
- Cyclic term handling in pretty-printer
- `copy_term/2` (requires attrs/cycles decision)

### Deferred to Stage 4+
- Attributed variables
- Constraint domains

## Testing Strategy

### Unit Tests
- Each builtin gets dedicated tests
- Test success and failure modes
- Test determinism guarantees
- Test trail correctness

### Parser/Pretty-Printer Tests
- Round-trip for all term types
- Clause parsing (facts and rules)
- Error messages for malformed input
- Pretty-printer stability

### Library Predicate Tests
These serve as acceptance tests:
- `append([1,2], [3,4], X)` → `X = [1,2,3,4]`
- `member(X, [1,2,3])` → three solutions
- `reverse([1,2,3], X)` → `X = [3,2,1]`
- `between(1, 3, X)` → three solutions
- `length([1,2,3], N)` → `N = 3`

### Exception Tests
- `catch(throw(ball), X, true)` → `X = ball`
- Backtracking through catch frames
- Nested catch/throw
- State restoration after catch

## Success Criteria (from PLAN.md)

1. **Parser**: Operator-free programs parse correctly
2. **Pretty-Printer**: Round-trip property holds
3. **Builtins**: `=../2`, `functor/3`, `arg/3` work correctly
4. **Exceptions**: `throw/1` and `catch/3` have proper semantics
5. **Library Tests**: `append/3`, `member/2`, `reverse/2`, `between/3` pass
6. **Control**: `once/1` determinism behavior is correct
7. **REPL**: Can load files and run queries interactively

## Dependencies
- Stage 0 must be complete ✅
- Lark parser library

## Risks & Mitigations
- **Risk**: Parser complexity for lists/structures
  - **Mitigation**: Start with minimal grammar, extend based on tests
  
- **Risk**: Exception unwinding complexity
  - **Mitigation**: Careful state management, extensive testing
  
- **Risk**: Pretty-printer edge cases
  - **Mitigation**: Defer complex cases (cycles) to later stages

## Notes for Future Stages
- Stage 1.5 adds operator reader (no AST changes)
- Stage 2 adds indexing (no semantic changes)
- Stage 3 adds debug/trace system
- Keep builtin interfaces stable

## Definition of Done
- [ ] Parser handles operator-free Prolog
- [ ] Pretty-printer round-trips with parser
- [ ] Core term builtins implemented (`=../2`, `functor/3`, `arg/3`)
- [ ] Exception system working (`throw/1`, `catch/3`)
- [ ] Library predicates pass as acceptance tests
- [ ] Basic REPL functional
- [ ] Documentation complete
- [ ] All tests passing