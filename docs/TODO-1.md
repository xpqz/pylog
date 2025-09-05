# TODO: Stage 1 (Minimal ISO Builtins - operator-free)

## Phase 1: Parser and Pretty Printer

### 1. Lark Grammar Definition
- [ ] Write test: Parse atoms (lowercase start, alphanumeric/underscore)
- [ ] Write test: Parse quoted atoms 'foo bar', 'Hello World!'
- [ ] Write test: Parse atoms with escapes 'can\'t', 'line\nbreak'
- [ ] Write test: Parse atoms with special chars '[strange]', '[]'
- [ ] Write test: Parse integers (positive, negative, zero)
- [ ] Write test: Parse variables (uppercase/underscore start)
- [ ] Write test: Parse anonymous variable (_)
- [ ] Write test: Multiple _ are distinct (no unification)
- [ ] Create grammar.lark with atom/int/var rules
- [ ] Add quoted atom rules with escape handling
- [ ] Verify basic term parsing

#### List Syntax
- [ ] Write test: Parse empty list []
- [ ] Write test: Parse list [1,2,3]
- [ ] Write test: Parse list with tail [H|T]
- [ ] Write test: Parse nested lists [[1,2],[3,4]]
- [ ] Write test: Parse improper list [1|2]
- [ ] Add list rules to grammar
- [ ] Verify list parsing

#### Structure Syntax
- [ ] Write test: Parse 0-arity structure foo
- [ ] Write test: Parse structure foo(bar)
- [ ] Write test: Parse multi-arg structure foo(a,b,c)
- [ ] Write test: Parse nested structure foo(bar(baz))
- [ ] Write test: Parse structure with mixed args foo(1,X,[a,b])
- [ ] Add structure rules to grammar
- [ ] Verify structure parsing

#### Clause Syntax
- [ ] Write test: Parse fact parent(tom,bob).
- [ ] Write test: Parse rule head :- body.
- [ ] Write test: Parse multi-goal body head :- g1, g2, g3.
- [ ] Write test: Parse query ?- goal.
- [ ] Write test: Parse directive :- goal.
- [ ] Add clause/query/directive rules to grammar
- [ ] Verify clause parsing

### 2. Parser Module Implementation
- [ ] Write test: parse_term returns AST for atom
- [ ] Write test: parse_term returns AST for integer
- [ ] Write test: parse_term returns AST for variable
- [ ] Write test: parse_term returns AST for list
- [ ] Write test: parse_term returns AST for structure
- [ ] Implement parser.py with parse_term function
- [ ] Implement AST transformer (Lark tree → Term objects)
- [ ] Verify term parsing

#### Clause Parser
- [ ] Write test: parse_clause for fact
- [ ] Write test: parse_clause for rule
- [ ] Write test: parse_program for multiple clauses
- [ ] Write test: parse_query for goals
- [ ] Write test: Error on malformed input with line number
- [ ] Implement parse_clause, parse_program, parse_query
- [ ] Verify clause parsing

### 3. Pretty Printer Implementation

#### Basic Term Printing
- [ ] Write test: pretty(Atom("foo")) → "foo"
- [ ] Write test: pretty(Atom("foo bar")) → "'foo bar'" (needs quotes)
- [ ] Write test: pretty(Atom("can't")) → "'can\'t'" (escape quotes)
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

#### List Printing
- [ ] Write test: pretty([]) → "[]"
- [ ] Write test: pretty([1,2,3]) → "[1, 2, 3]"
- [ ] Write test: pretty([H|T]) → "[H|T]"
- [ ] Write test: pretty([[1,2],[3]]) → "[[1, 2], [3]]"
- [ ] Write test: pretty([1|2]) → "[1|2]" (improper)
- [ ] Implement list pretty printing
- [ ] Verify list formatting

#### Structure Printing
- [ ] Write test: pretty(foo) → "foo" (atom)
- [ ] Write test: pretty(foo(bar)) → "foo(bar)"
- [ ] Write test: pretty(foo(a,b)) → "foo(a, b)"
- [ ] Write test: pretty(foo(bar(baz))) → "foo(bar(baz))"
- [ ] Implement structure pretty printing
- [ ] Verify structure formatting

#### Clause and Solution Printing
- [ ] Write test: Pretty print fact
- [ ] Write test: Pretty print rule with body
- [ ] Write test: Pretty print solution bindings X = value
- [ ] Write test: Pretty print multiple solutions
- [ ] Implement clause and solution formatting
- [ ] Verify output readability

### 4. Round-Trip Testing
- [ ] Write property test: parse(pretty(term)) == term
- [ ] Write property test: pretty(parse(text)) == normalize(text)
- [ ] Write property test: Random term generation and round-trip
- [ ] Test round-trip for all term types
- [ ] Test round-trip for quoted atoms with escapes
- [ ] Test round-trip for clauses
- [ ] Test round-trip preserves semantics
- [ ] Test anonymous variable round-trip (distinct _)
- [ ] Fix any round-trip failures

## Phase 2: Core Term Builtins

### 1. Univ (=../2) Implementation

#### Decomposition Mode
- [ ] Write test: foo(a,b,c) =.. [foo,a,b,c]
- [ ] Write test: foo =.. [foo] (0-arity)
- [ ] Write test: 42 =.. [42] (atomic)
- [ ] Write test: [] =.. [[]] (empty list special case)
- [ ] Write test: [a,b] =.. ['.',a,[b]] (list structure)
- [ ] Write test: Var fails when unbound on left
- [ ] Implement decomposition in builtin_univ
- [ ] Verify decomposition mode

#### Construction Mode
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

#### Bidirectional Mode
- [ ] Write test: foo(X) =.. [foo, Y] unifies X and Y
- [ ] Write test: Struct =.. [F|Args] with unbound F
- [ ] Test determinism (semidet)
- [ ] Write test: No choicepoint remains after success
- [ ] Verify all modes work

### 2. Functor/3 Implementation

#### Extraction Mode
- [ ] Write test: functor(foo(a,b), F, A) binds F=foo, A=2
- [ ] Write test: functor(foo, F, A) binds F=foo, A=0
- [ ] Write test: functor(42, F, A) binds F=42, A=0
- [ ] Write test: functor([], F, A) binds F=[], A=0
- [ ] Write test: functor([a,b], F, A) binds F='.', A=2
- [ ] Write test: Fail on unbound first arg
- [ ] Implement extraction in builtin_functor
- [ ] Verify extraction mode

#### Construction Mode
- [ ] Write test: functor(X, foo, 2) binds X to foo(_,_)
- [ ] Write test: functor(X, foo, 0) binds X to foo
- [ ] Write test: functor(X, 42, 0) binds X to 42
- [ ] Write test: functor(X, [], 0) binds X to []
- [ ] Write test: Fail on negative arity
- [ ] Write test: Fail on non-atom functor for arity > 0
- [ ] Write test: Trail all bindings properly
- [ ] Implement construction in builtin_functor
- [ ] Verify construction mode

#### Checking Mode
- [ ] Write test: functor(foo(a,b), foo, 2) succeeds
- [ ] Write test: functor(foo(a,b), foo, 3) fails
- [ ] Write test: functor(foo(a,b), bar, 2) fails
- [ ] Test determinism (semidet)
- [ ] Write test: No choicepoint remains after success
- [ ] Verify checking mode

### 3. Arg/3 Implementation
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

### 4. Once/1 Implementation
- [ ] Write test: once(member(X,[1,2,3])) gives one solution
- [ ] Write test: once(fail) fails
- [ ] Write test: once(true) succeeds once
- [ ] Write test: once((X=1; X=2)) gives X=1 only
- [ ] Write test: Verify no choicepoint left after success
- [ ] Implement as: once(G) :- call(G), !.
- [ ] Test determinism (semidet)
- [ ] Verify once/1 behavior

## Phase 3: Exception System

### 1. Throw/1 Implementation
- [ ] Write test: throw(error) propagates error term
- [ ] Write test: throw(X) with unbound X fails gracefully
- [ ] Write test: throw(foo(bar)) preserves structure
- [ ] Write test: throw/1 unwinds goal stack
- [ ] Write test: throw/1 unwinds choicepoints
- [ ] Implement builtin_throw with engine state
- [ ] Add exception field to engine
- [ ] Modify main loop to check exception
- [ ] Verify basic throw works

### 2. Catch/3 Basic Implementation
- [ ] Write test: catch(true, _, fail) succeeds
- [ ] Write test: catch(fail, _, true) fails
- [ ] Write test: catch(throw(ball), ball, true) succeeds
- [ ] Write test: catch(throw(ball), X, true) binds X=ball
- [ ] Write test: catch(throw(ball), other, true) rethrows
- [ ] Write test: Recovery goal executes on catch
- [ ] Implement builtin_catch with catch frame
- [ ] Implement exception matching
- [ ] Verify basic catch works

### 3. Catch/3 Advanced Behavior
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

### 4. Error Integration
- [ ] Write test: Arithmetic errors throw evaluation_error
- [ ] Write test: Type errors throw type_error
- [ ] Write test: catch can intercept builtin errors
- [ ] Add error generation to relevant builtins
- [ ] Document error policy (dev mode)
- [ ] Verify error handling works

## Phase 4: Library Predicates (Prolog code)

### 1. List Utilities
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

### 2. Arithmetic Utilities
- [ ] Write between/3 in Prolog
- [ ] Test: between(1,3,X) generates 1,2,3
- [ ] Test: between(1,3,2) succeeds
- [ ] Test: between(1,3,4) fails  
- [ ] Test: between(3,1,X) fails (low > high)
- [ ] Verify between/3 behavior

### 3. Control Utilities
- [ ] Define once/1 if not builtin: once(G) :- call(G), !.
- [ ] Test once/1 with library predicates
- [ ] Verify cuts properly

### 4. Test File Creation
- [ ] Create prolog/lib/lists.pl with all predicates
- [ ] Create test file that loads and tests library
- [ ] Verify all library predicates work
- [ ] Use as acceptance tests for engine

## Phase 5: Basic REPL

### 1. File Loading
- [ ] Write test: Load facts from .pl file
- [ ] Write test: Load rules from .pl file  
- [ ] Write test: Load multiple files
- [ ] Write test: Handle file not found
- [ ] Write test: Handle parse errors with line numbers
- [ ] Implement load_file function
- [ ] Verify file loading works

### 2. Query Interface
- [ ] Write test: Execute simple query
- [ ] Write test: Show first solution
- [ ] Write test: ';' shows next solution
- [ ] Write test: '.' stops searching
- [ ] Write test: Show 'true' for success
- [ ] Write test: Show 'false' for failure
- [ ] Write test: Pretty print solutions
- [ ] Implement query loop
- [ ] Verify query interface works

### 3. REPL Main Loop
- [ ] Implement prompt and input reading
- [ ] Implement query parsing and execution
- [ ] Implement solution display with pretty printer
- [ ] Implement ';' and '.' handling
- [ ] Add help command
- [ ] Add quit command
- [ ] Test interactive session
- [ ] Verify REPL usability

### 4. Integration
- [ ] Create example Prolog programs
- [ ] Test loading library predicates
- [ ] Test running complex queries
- [ ] Document REPL usage
- [ ] Create demo session
- [ ] Verify Stage 1 acceptance criteria

## Phase 6: Integration and Polish

### 1. Error Handling
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

### 2. Performance Testing
- [ ] Benchmark parser on large files
- [ ] Benchmark pretty printer on complex terms
- [ ] Test =../2 with large structures
- [ ] Test library predicates performance
- [ ] Document any performance issues
- [ ] Verify acceptable performance

### 3. Documentation
- [ ] Document parser grammar format
- [ ] Document pretty printer output format
- [ ] Document each builtin's behavior
- [ ] Document library predicate semantics
- [ ] Document REPL commands
- [ ] Create user guide
- [ ] Update README with Stage 1 features

### 4. Final Validation
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

## Notes
- All builtins must specify determinism (det/semidet/nondet)
- All binding operations must trail properly
- Parser remains operator-free (operators in Stage 1.5)
- AST structure is locked after Stage 1
- Library predicates are tests, not engine code
- Keep error handling simple in dev mode