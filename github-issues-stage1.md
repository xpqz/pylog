# GitHub Issues for Stage 1

## Epic Issue

### Title: Epic: Stage 1 - Minimal ISO Builtins (operator-free)

### Body:
This epic tracks the implementation of Stage 1 as defined in [STAGE_1_PLAN.md](docs/STAGE_1_PLAN.md).

## Objectives
- [ ] Implement operator-free parser and pretty-printer
- [ ] Complete core ISO builtins (=../2, functor/3, arg/3)
- [ ] Implement throw/1 and catch/3 exception system
- [ ] Validate with library predicates (append/3, member/2, reverse/2, between/3)
- [ ] Create basic REPL for interactive development

## Success Criteria (from PLAN.md)
- [ ] `append/3`, `member/2`, `reverse/2`, `between/3` pass
- [ ] `catch/3` captures thrown terms correctly
- [ ] `once/1` determinism behavior is correct
- [ ] Parser handles operator-free programs
- [ ] Pretty-printer round-trips with parser

## Issues
- [ ] #1 Parser Grammar and Basic Terms
- [ ] #2 Parser Module and Clause Parsing  
- [ ] #3 Pretty Printer and Round-trip
- [ ] #4 Implement =../2 (univ)
- [ ] #5 Implement functor/3
- [ ] #6 Implement arg/3 and once/1
- [ ] #7 Basic throw/catch
- [ ] #8 Advanced catch semantics
- [ ] #9 Library Predicates
- [ ] #10 Basic REPL
- [ ] #11 Error Handling and Polish

---

## Issue 1: Parser Grammar and Basic Terms

### Title: Implement Lark grammar for basic terms and lists

### Body:
Implement the foundational Lark grammar for parsing Prolog terms in operator-free syntax.

Part of #epic Stage 1

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Basic Terms
- [ ] Write test: Parse atoms (lowercase start, alphanumeric/underscore)
- [ ] Write test: Parse quoted atoms 'foo bar', 'Hello World!'
- [ ] Write test: Parse atoms with escapes 'can\\'t', 'line\\nbreak'
- [ ] Write test: Parse atoms with special chars '[strange]', '[]'
- [ ] Write test: Parse integers (positive, negative, zero)
- [ ] Write test: Parse variables (uppercase/underscore start)
- [ ] Write test: Parse anonymous variable (_)
- [ ] Write test: Multiple _ are distinct (no unification)
- [ ] Create grammar.lark with atom/int/var rules
- [ ] Add quoted atom rules with escape handling
- [ ] Verify basic term parsing

### List Syntax
- [ ] Write test: Parse empty list []
- [ ] Write test: Parse list [1,2,3]
- [ ] Write test: Parse list with tail [H|T]
- [ ] Write test: Parse nested lists [[1,2],[3,4]]
- [ ] Write test: Parse improper list [1|2]
- [ ] Add list rules to grammar
- [ ] Verify list parsing

### Structure Syntax
- [ ] Write test: Parse 0-arity structure foo
- [ ] Write test: Parse structure foo(bar)
- [ ] Write test: Parse multi-arg structure foo(a,b,c)
- [ ] Write test: Parse nested structure foo(bar(baz))
- [ ] Write test: Parse structure with mixed args foo(1,X,[a,b])
- [ ] Add structure rules to grammar
- [ ] Verify structure parsing

## Acceptance Criteria
- Grammar handles all basic Prolog terms
- Quoted atoms with escapes work correctly
- Anonymous variables are properly handled
- All tests pass

---

## Issue 2: Parser Module and Clause Parsing

### Title: Implement parser module for clauses and queries

### Body:
Build the parser module that converts Lark parse trees to our AST, with support for clauses, facts, rules, and queries.

Part of #epic Stage 1
Depends on #1

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Clause Syntax Grammar
- [ ] Write test: Parse fact parent(tom,bob).
- [ ] Write test: Parse rule head :- body.
- [ ] Write test: Parse multi-goal body head :- g1, g2, g3.
- [ ] Write test: Parse query ?- goal.
- [ ] Write test: Parse directive :- goal.
- [ ] Add clause/query/directive rules to grammar
- [ ] Verify clause parsing

### Parser Module Implementation
- [ ] Write test: parse_term returns AST for atom
- [ ] Write test: parse_term returns AST for integer
- [ ] Write test: parse_term returns AST for variable
- [ ] Write test: parse_term returns AST for list
- [ ] Write test: parse_term returns AST for structure
- [ ] Implement parser.py with parse_term function
- [ ] Implement AST transformer (Lark tree → Term objects)
- [ ] Verify term parsing

### Clause Parser
- [ ] Write test: parse_clause for fact
- [ ] Write test: parse_clause for rule
- [ ] Write test: parse_program for multiple clauses
- [ ] Write test: parse_query for goals
- [ ] Write test: Error on malformed input with line number
- [ ] Implement parse_clause, parse_program, parse_query
- [ ] Verify clause parsing

## Acceptance Criteria
- Can parse complete Prolog programs (operator-free)
- Facts, rules, and queries parse correctly
- Good error messages with line numbers
- AST generation is correct

---

## Issue 3: Pretty Printer and Round-trip Testing

### Title: Implement pretty printer with round-trip property

### Body:
Create the pretty printer for readable output and ensure parse/pretty round-trip property holds.

Part of #epic Stage 1
Depends on #2

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Basic Term Printing
- [ ] Write test: pretty(Atom("foo")) → "foo"
- [ ] Write test: pretty(Atom("foo bar")) → "'foo bar'" (needs quotes)
- [ ] Write test: pretty(Atom("can't")) → "'can\\'t'" (escape quotes)
- [ ] Write test: pretty(Atom("[]")) → "'[]'" (reserved chars)
- [ ] Write test: pretty(Int(42)) → "42"
- [ ] Write test: pretty(Int(-5)) → "-5"
- [ ] Write test: pretty(Var(0, "X")) → "X" (stable names)
- [ ] Write test: pretty(Var(1, "Y")) → "Y"
- [ ] Write test: pretty(Var(0)) → "_G0" (generated names)
- [ ] Write test: Anonymous vars _ don't get stable names
- [ ] Implement pretty.py with pretty() function
- [ ] Implement variable name tracking (by ID)
- [ ] Implement quoted atom detection and escaping
- [ ] Verify basic pretty printing

### List Printing
- [ ] Write test: pretty([]) → "[]"
- [ ] Write test: pretty([1,2,3]) → "[1, 2, 3]"
- [ ] Write test: pretty([H|T]) → "[H|T]"
- [ ] Write test: pretty([[1,2],[3]]) → "[[1, 2], [3]]"
- [ ] Write test: pretty([1|2]) → "[1|2]" (improper)
- [ ] Implement list pretty printing
- [ ] Verify list formatting

### Structure and Clause Printing
- [ ] Write test: pretty(foo) → "foo" (atom)
- [ ] Write test: pretty(foo(bar)) → "foo(bar)"
- [ ] Write test: pretty(foo(a,b)) → "foo(a, b)"
- [ ] Write test: pretty(foo(bar(baz))) → "foo(bar(baz))"
- [ ] Write test: Pretty print fact
- [ ] Write test: Pretty print rule with body
- [ ] Write test: Pretty print solution bindings X = value
- [ ] Write test: Pretty print multiple solutions
- [ ] Implement structure pretty printing
- [ ] Implement clause and solution formatting
- [ ] Verify structure formatting
- [ ] Verify output readability

### Round-Trip Testing
- [ ] Write property test: parse(pretty(term)) == term
- [ ] Write property test: pretty(parse(text)) == normalize(text)
- [ ] Write property test: Random term generation and round-trip
- [ ] Test round-trip for all term types
- [ ] Test round-trip for quoted atoms with escapes
- [ ] Test round-trip for clauses
- [ ] Test round-trip preserves semantics
- [ ] Test anonymous variable round-trip (distinct _)
- [ ] Fix any round-trip failures

## Acceptance Criteria
- Pretty printer produces readable Prolog syntax
- Round-trip property holds for all terms
- Variable names are stable
- Quoted atoms handled correctly

---

## Issue 4: Implement =../2 (univ)

### Title: Implement =../2 for structure ↔ list conversion

### Body:
Implement the univ operator (=../2) with all modes: decomposition, construction, and bidirectional.

Part of #epic Stage 1
Depends on #3

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Decomposition Mode
- [ ] Write test: foo(a,b,c) =.. [foo,a,b,c]
- [ ] Write test: foo =.. [foo] (0-arity)
- [ ] Write test: 42 =.. [42] (atomic)
- [ ] Write test: [] =.. [[]] (empty list special case)
- [ ] Write test: [a,b] =.. ['.',a,[b]] (list structure)
- [ ] Write test: Var fails when unbound on left
- [ ] Implement decomposition in builtin_univ
- [ ] Verify decomposition mode

### Construction Mode
- [ ] Write test: X =.. [foo,a,b] binds X to foo(a,b)
- [ ] Write test: X =.. [foo] binds X to foo (atom)
- [ ] Write test: X =.. [42] binds X to 42
- [ ] Write test: X =.. [[]] binds X to []
- [ ] Write test: X =.. ['.', a, [b]] binds X to [a,b]
- [ ] Write test: Fail on non-list right side
- [ ] Write test: Fail on empty list right side
- [ ] Write test: Trail all bindings properly
- [ ] Implement construction in builtin_univ
- [ ] Verify construction mode

### Bidirectional Mode
- [ ] Write test: foo(X) =.. [foo, Y] unifies X and Y
- [ ] Write test: Struct =.. [F|Args] with unbound F
- [ ] Test determinism (semidet)
- [ ] Write test: No choicepoint remains after success
- [ ] Verify all modes work

## Acceptance Criteria
- All three modes work correctly
- Proper trailing for bindings
- Determinism is semidet
- Special cases (lists, atoms) handled

---

## Issue 5: Implement functor/3

### Title: Implement functor/3 for functor/arity manipulation

### Body:
Implement functor/3 with extraction, construction, and checking modes.

Part of #epic Stage 1
Depends on #3

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Extraction Mode
- [ ] Write test: functor(foo(a,b), F, A) binds F=foo, A=2
- [ ] Write test: functor(foo, F, A) binds F=foo, A=0
- [ ] Write test: functor(42, F, A) binds F=42, A=0
- [ ] Write test: functor([], F, A) binds F=[], A=0
- [ ] Write test: functor([a,b], F, A) binds F='.', A=2
- [ ] Write test: Fail on unbound first arg
- [ ] Implement extraction in builtin_functor
- [ ] Verify extraction mode

### Construction Mode
- [ ] Write test: functor(X, foo, 2) binds X to foo(_,_)
- [ ] Write test: functor(X, foo, 0) binds X to foo
- [ ] Write test: functor(X, 42, 0) binds X to 42
- [ ] Write test: functor(X, [], 0) binds X to []
- [ ] Write test: Fail on negative arity
- [ ] Write test: Fail on non-atom functor for arity > 0
- [ ] Write test: Trail all bindings properly
- [ ] Implement construction in builtin_functor
- [ ] Verify construction mode

### Checking Mode
- [ ] Write test: functor(foo(a,b), foo, 2) succeeds
- [ ] Write test: functor(foo(a,b), foo, 3) fails
- [ ] Write test: functor(foo(a,b), bar, 2) fails
- [ ] Test determinism (semidet)
- [ ] Write test: No choicepoint remains after success
- [ ] Verify checking mode

## Acceptance Criteria
- All three modes work correctly
- Fresh variables created in construction mode
- Proper trailing for bindings
- Determinism is semidet

---

## Issue 6: Implement arg/3 and once/1

### Title: Implement arg/3 for argument access and once/1 for deterministic call

### Body:
Implement arg/3 for structure argument access and once/1 as a deterministic meta-predicate.

Part of #epic Stage 1
Depends on #3

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Arg/3 Implementation
- [ ] Write test: arg(1, foo(a,b,c), X) binds X=a
- [ ] Write test: arg(2, foo(a,b,c), X) binds X=b
- [ ] Write test: arg(3, foo(a,b,c), X) binds X=c
- [ ] Write test: arg(1, foo(a), a) succeeds (checking)
- [ ] Write test: arg(1, foo(a), b) fails
- [ ] Write test: Fail on N < 1
- [ ] Write test: Fail on N > arity
- [ ] Write test: Fail on non-compound first arg
- [ ] Write test: Fail on non-integer N
- [ ] Write test: No trailing needed (read-only)
- [ ] Implement builtin_arg
- [ ] Test determinism (semidet)
- [ ] Write test: No choicepoint remains after success
- [ ] Verify all arg/3 tests pass

### Once/1 Implementation
- [ ] Write test: once(member(X,[1,2,3])) gives one solution
- [ ] Write test: once(fail) fails
- [ ] Write test: once(true) succeeds once
- [ ] Write test: once((X=1; X=2)) gives X=1 only
- [ ] Write test: Verify no choicepoint left after success
- [ ] Implement as: once(G) :- call(G), !.
- [ ] Test determinism (semidet)
- [ ] Verify once/1 behavior

## Acceptance Criteria
- arg/3 correctly accesses structure arguments
- arg/3 is read-only (no trailing)
- once/1 cuts after first solution
- Both are semidet

---

## Issue 7: Basic throw/catch Implementation

### Title: Implement basic throw/1 and catch/3 exception handling

### Body:
Implement the foundation of the exception system with throw/1 and basic catch/3.

Part of #epic Stage 1
Depends on #6

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Throw/1 Implementation
- [ ] Write test: throw(error) propagates error term
- [ ] Write test: throw(X) with unbound X fails gracefully
- [ ] Write test: throw(foo(bar)) preserves structure
- [ ] Write test: throw/1 unwinds goal stack
- [ ] Write test: throw/1 unwinds choicepoints
- [ ] Implement builtin_throw with engine state
- [ ] Add exception field to engine
- [ ] Modify main loop to check exception
- [ ] Verify basic throw works

### Catch/3 Basic Implementation
- [ ] Write test: catch(true, _, fail) succeeds
- [ ] Write test: catch(fail, _, true) fails
- [ ] Write test: catch(throw(ball), ball, true) succeeds
- [ ] Write test: catch(throw(ball), X, true) binds X=ball
- [ ] Write test: catch(throw(ball), other, true) rethrows
- [ ] Write test: Recovery goal executes on catch
- [ ] Implement builtin_catch with catch frame
- [ ] Implement exception matching
- [ ] Verify basic catch works

## Acceptance Criteria
- Exceptions propagate correctly
- catch/3 intercepts matching exceptions
- Non-matching exceptions rethrow
- Recovery goal executes

---

## Issue 8: Advanced catch/3 Semantics

### Title: Implement advanced catch/3 backtracking and state management

### Body:
Complete the exception system with proper choicepoint management and backtracking semantics.

Part of #epic Stage 1
Depends on #7

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Advanced Behavior
- [ ] Write test: Nested catch/throw
- [ ] Write test: catch((X=1; X=2), _, true) gives both solutions
- [ ] Write test: catch preserves choicepoints before throw
- [ ] Write test: catch removes choicepoints after throw point
- [ ] Write test: Success through catch leaves prior choicepoints intact
- [ ] Write test: Caught exception prunes only post-throw segment
- [ ] Write test: Backtracking through catch on success
- [ ] Write test: No backtracking through catch on caught exception
- [ ] Write test: Trail properly unwound to catch point
- [ ] Implement catch frame management
- [ ] Implement proper state restoration
- [ ] Verify all exception tests pass

### Error Integration
- [ ] Write test: Arithmetic errors throw evaluation_error
- [ ] Write test: Type errors throw type_error
- [ ] Write test: catch can intercept builtin errors
- [ ] Add error generation to relevant builtins
- [ ] Document error policy (dev mode)
- [ ] Verify error handling works

## Acceptance Criteria
- Proper choicepoint management
- Trail correctly unwound
- Backtracking semantics correct
- Builtin errors can be caught

---

## Issue 9: Library Predicates

### Title: Implement standard library predicates in Prolog

### Body:
Write the standard library predicates (append/3, member/2, etc.) in Prolog as acceptance tests for the engine.

Part of #epic Stage 1
Depends on #8

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### List Utilities
- [ ] Write append/3 in Prolog
- [ ] Test: append([1,2],[3,4],[1,2,3,4])
- [ ] Test: append([1,2],[3,4],X) generates
- [ ] Test: append(X,Y,[1,2,3]) generates splits
- [ ] Test: append([],[],X) binds X=[]
- [ ] Verify append/3 modes

- [ ] Write member/2 in Prolog  
- [ ] Test: member(2,[1,2,3]) succeeds
- [ ] Test: member(X,[1,2,3]) generates all
- [ ] Test: member(4,[1,2,3]) fails
- [ ] Verify member/2 modes

- [ ] Write reverse/2 in Prolog (using accumulator)
- [ ] Test: reverse([1,2,3],[3,2,1]) succeeds
- [ ] Test: reverse([1,2,3],X) generates
- [ ] Test: reverse([],[])
- [ ] Verify reverse/2 correctness

- [ ] Write length/2 in Prolog
- [ ] Test: length([1,2,3],3) succeeds
- [ ] Test: length([1,2,3],N) binds N=3
- [ ] Test: length(L,3) generates list with unbound vars
- [ ] Verify length/2 modes

### Arithmetic Utilities
- [ ] Write between/3 in Prolog
- [ ] Test: between(1,3,X) generates 1,2,3
- [ ] Test: between(1,3,2) succeeds
- [ ] Test: between(1,3,4) fails  
- [ ] Test: between(3,1,X) fails (low > high)
- [ ] Verify between/3 behavior

### Test File Creation
- [ ] Create prolog/lib/lists.pl with all predicates
- [ ] Create test file that loads and tests library
- [ ] Verify all library predicates work
- [ ] Use as acceptance tests for engine

## Acceptance Criteria
- All standard predicates work correctly
- Multiple modes supported where applicable
- Serve as comprehensive engine tests
- File structure ready for REPL loading

---

## Issue 10: Basic REPL Implementation

### Title: Create basic REPL for interactive Prolog development

### Body:
Implement a basic Read-Eval-Print Loop for loading files and running queries interactively.

Part of #epic Stage 1
Depends on #9

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### File Loading
- [ ] Write test: Load facts from .pl file
- [ ] Write test: Load rules from .pl file  
- [ ] Write test: Load multiple files
- [ ] Write test: Handle file not found
- [ ] Write test: Handle parse errors with line numbers
- [ ] Implement load_file function
- [ ] Verify file loading works

### Query Interface
- [ ] Write test: Execute simple query
- [ ] Write test: Show first solution
- [ ] Write test: ';' shows next solution
- [ ] Write test: '.' stops searching
- [ ] Write test: Show 'true' for success
- [ ] Write test: Show 'false' for failure
- [ ] Write test: Pretty print solutions
- [ ] Implement query loop
- [ ] Verify query interface works

### REPL Main Loop
- [ ] Implement prompt and input reading
- [ ] Implement query parsing and execution
- [ ] Implement solution display with pretty printer
- [ ] Implement ';' and '.' handling
- [ ] Add help command
- [ ] Add quit command
- [ ] Test interactive session
- [ ] Verify REPL usability

### Integration
- [ ] Create example Prolog programs
- [ ] Test loading library predicates
- [ ] Test running complex queries
- [ ] Document REPL usage
- [ ] Create demo session
- [ ] Verify Stage 1 acceptance criteria

## Acceptance Criteria
- Can load Prolog files
- Interactive query execution works
- Solutions displayed with pretty printer
- User-friendly interface

---

## Issue 11: Error Handling and Polish

### Title: Implement error handling policy and final integration

### Body:
Define dev mode error behavior, ensure consistency, and complete Stage 1 documentation.

Part of #epic Stage 1
Depends on #10

## Tasks
From [TODO-1.md](docs/TODO-1.md):

### Error Handling
- [ ] Document dev mode error behavior
- [ ] Write test: Undefined predicate fails silently
- [ ] Write test: foo(X) with no foo/1 clauses returns false
- [ ] Write test: Undefined predicates don't throw in dev mode
- [ ] Test undefined predicate handling (fail)
- [ ] Test type errors in builtins
- [ ] Test arithmetic errors
- [ ] Document undefined predicate policy clearly
- [ ] Plan ISO mode for future (existence_error)
- [ ] Verify error consistency

### Performance Testing
- [ ] Benchmark parser on large files
- [ ] Benchmark pretty printer on complex terms
- [ ] Test =../2 with large structures
- [ ] Test library predicates performance
- [ ] Document any performance issues
- [ ] Verify acceptable performance

### Documentation
- [ ] Document parser grammar format
- [ ] Document pretty printer output format
- [ ] Document each builtin's behavior
- [ ] Document library predicate semantics
- [ ] Document REPL commands
- [ ] Create user guide
- [ ] Update README with Stage 1 features

### Final Validation
- [ ] Run all unit tests
- [ ] Run integration tests
- [ ] Test round-trip properties
- [ ] Test library predicates as acceptance
- [ ] Verify PLAN.md acceptance criteria:
  - [ ] append/3, member/2, reverse/2, between/3 pass
  - [ ] catch/3 captures thrown terms correctly  
  - [ ] once/1 determinism behavior correct
  - [ ] Parser handles operator-free programs
  - [ ] Pretty printer produces readable output
- [ ] Stage 1 complete

## Acceptance Criteria
- All tests passing
- Error behavior documented and consistent
- Performance acceptable
- Documentation complete
- PLAN.md criteria met

### Labels: 
`stage-1`, `polish`, `documentation`