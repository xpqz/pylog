# TODO: Stage 1.5 (Operators via Reader)

## Phase 1: Grammar Tokenization (Operator-Agnostic)

### 1. Operator Token Recognition (Longest-Match)
- [ ] Write test: Lex ',' as COMMA token
- [ ] Write test: Lex ';' as SEMICOLON token
- [ ] Write test: Lex '->' as ARROW token
- [ ] Write test: Lex '=' as EQUALS token
- [ ] Write test: Lex arithmetic ops (+, -, *, /, //, mod)
- [ ] Write test: Lex comparison ops (<, >, =<, >=, =:=, =\=)
- [ ] Write test: Lex structural ops (==, \==, @<, @>, @=<, @>=)
- [ ] Write test: '=\=' lexes as single token (not '=' then '\' then '=')
- [ ] Write test: '=<' lexes as single token (not '=' then '<')
- [ ] Write test: '@=<' lexes as single token (not '@' then '=' then '<')
- [ ] Write test: '>=' lexes as single token (not '>' then '=')
- [ ] Write test: '//' lexes as single token (not '/' then '/')
- [ ] Add operator tokens to grammar.lark with longest-match priority
- [ ] Verify tokenization works

### 2. Token Stream with Positions
- [ ] Write test: Tokens include start/end positions
- [ ] Write test: Operator tokens preserve source location
- [ ] Write test: Parenthesis tokens tracked
- [ ] Write test: Token positions point to token start/end (not whitespace)
- [ ] Write test: 'A,B' vs 'A , B' produce different spans but same AST
- [ ] Update grammar to emit position-aware tokens
- [ ] Keep grammar free of precedence rules
- [ ] Verify token position tracking

### 3. Grammar Backwards Compatibility
- [ ] Write test: Existing atom parsing still works
- [ ] Write test: Existing struct parsing still works  
- [ ] Write test: Quoted operators parse as atoms: ','(A,B) still valid
- [ ] Write test: '+'(1,2) parses as canonical form when quoted
- [ ] Write test: Operator chars in quotes handled correctly
- [ ] Write test: Lists remain untouched: [H|T] and [a,b,c] unchanged
- [ ] Write test: 'member(X, [1,2,3]), X > 0' mixes lists and operators
- [ ] Write test: 'reverse([1|T], R), R = [_, _ | _]' complex list patterns
- [ ] Write test: Canonical input remains unchanged (idempotence)
- [ ] Ensure all Stage 1 tests still pass
- [ ] Verify no grammar regressions

## Phase 2: Operator Table and Reader Core

### 1. Static Operator Table (Single Source of Truth)
- [ ] Write test: Assert exact triple (',' , 1000, 'xfy')
- [ ] Write test: Assert exact triple (';' , 1100, 'xfy')
- [ ] Write test: Assert exact triple ('->', 1050, 'xfy')
- [ ] Write test: Assert exact triple ('+' , 500, 'yfx') for infix
- [ ] Write test: Assert exact triple ('+' , 200, 'fy') for prefix
- [ ] Write test: Assert exact triple ('-' , 500, 'yfx') for infix
- [ ] Write test: Assert exact triple ('-' , 200, 'fy') for prefix
- [ ] Write test: Assert exact triple ('**', 200, 'xfy') right-assoc
- [ ] Write test: Assert all comparison ops are 700, xfx (non-chainable)
- [ ] Write test: Assert all arithmetic ops have correct precedence/type
- [ ] Write test: Assert term order ops (@<, @=<, etc.) are 700, xfx
- [ ] Write test: 'mod' is word-token operator at precedence 400, yfx
- [ ] Write test: 'mod'(X,Y) as quoted atom still parses
- [ ] Create operators.py with complete operator table
- [ ] Add one-to-one canonical mapping for each operator
- [ ] Mark which operators lack Stage 1 runtime support
- [ ] Verify all operator lookups work

### 2. Pratt Parser Implementation
- [ ] Write test: Higher precedence binds tighter (2+3*4)
- [ ] Write test: Equal precedence follows associativity
- [ ] Write test: Parentheses override precedence
- [ ] Write test: Prefix operator precedence works
- [ ] Write test: Token positions preserved through parsing
- [ ] Implement Pratt parser in reader.py
- [ ] Preserve source spans through transformations
- [ ] Handle operator precedence from table
- [ ] Handle associativity from table
- [ ] Verify precedence parsing

### 3. AST Transformation
- [ ] Write test: 'A, B' transforms to Struct(',', (A, B))
- [ ] Write test: 'A; B' transforms to Struct(';', (A, B))
- [ ] Write test: 'A -> B' transforms to Struct('->', (A, B))
- [ ] Write test: 'X = Y' transforms to Struct('=', (X, Y))
- [ ] Write test: '1 + 2' transforms to Struct('+', (Int(1), Int(2)))
- [ ] Write test: '-X' transforms to Struct('-', (X,))
- [ ] Implement transform_operators function
- [ ] Convert operator nodes to canonical Structs
- [ ] Verify transformations work

## Phase 3: Control Flow Operators

### 1. Conjunction (,) Operator
- [ ] Write test: 'A, B' parses as ','(A, B)
- [ ] Write test: 'A, B, C' right-associates as ','(A, ','(B, C))
- [ ] Write test: Conjunction in clause body works
- [ ] Write test: Parentheses change grouping
- [ ] Implement conjunction transformation
- [ ] Verify conjunction precedence (1000)
- [ ] Test with existing Stage 1 engine

### 2. Disjunction (;) Operator
- [ ] Write test: 'A; B' parses as ';'(A, B)
- [ ] Write test: 'A; B; C' right-associates
- [ ] Write test: ';' has lower precedence than ','
- [ ] Write test: 'A, B; C' groups as ';'(','(A, B), C)
- [ ] Implement disjunction transformation
- [ ] Verify disjunction precedence (1100)
- [ ] Test with existing Stage 1 engine

### 3. If-Then-Else Construct
- [ ] Write test: '(A -> B; C)' parses correctly
- [ ] Write test: '->' requires ';' for else branch
- [ ] Write test: Nested if-then-else works
- [ ] Write test: '->' precedence between ',' and ';'
- [ ] Implement if-then transformation
- [ ] Verify precedence interactions
- [ ] Test execution with Stage 1 engine
- [ ] Note: Soft cut (*->) deferred to future stage

## Phase 4: Comparison Operators

### 1. Equality and Disequality Operators
- [ ] Write test: 'X = Y' transforms to '='(X, Y)
- [ ] Write test: 'X \= Y' transforms to '\\='(X, Y)
- [ ] Write test: 'X = Y = Z' is syntax error (xfx non-chainable)
- [ ] Write test: Error message: "xfx operator '=' cannot chain; add parentheses"
- [ ] Write test: '(X = Y), (Y = Z)' is valid (two separate goals)
- [ ] Write test: = precedence vs arithmetic ops
- [ ] Implement equality operator transforms
- [ ] Verify with Stage 1 =/2 builtin

### 2. Structural Comparison
- [ ] Write test: 'X == Y' transforms to '=='(X, Y)
- [ ] Write test: 'X \== Y' transforms to '\\=='(X, Y)
- [ ] Write test: 'X == Y == Z' is syntax error (xfx non-chainable)
- [ ] Write test: 'X @< Y' transforms to '@<'(X, Y)
- [ ] Write test: 'X @> Y' transforms to '@>'(X, Y)
- [ ] Write test: 'X @=< Y' transforms to '@=<'(X, Y)
- [ ] Write test: 'X @>= Y' transforms to '@>='(X, Y)
- [ ] Write test: Term order ops are xfx (non-chainable)
- [ ] Implement structural comparison transforms
- [ ] Test all comparison operators

### 3. Arithmetic Comparison
- [ ] Write test: 'X < Y' transforms to '<'(X, Y)
- [ ] Write test: 'X > Y' transforms to '>'(X, Y)
- [ ] Write test: 'X =< Y' transforms to '=<'(X, Y)
- [ ] Write test: 'X >= Y' transforms to '>='(X, Y)
- [ ] Write test: 'X =:= Y' transforms to '=:='(X, Y)
- [ ] Write test: 'X =\= Y' transforms to '=\\='(X, Y)
- [ ] Write test: 'A < B < C' is syntax error (xfx non-chainable)
- [ ] Write test: 'X =:= Y =:= Z' is syntax error (xfx non-chainable)
- [ ] Write test: 'X =:= Y, Y =:= Z' is valid (two separate goals)
- [ ] Implement arithmetic comparison transforms
- [ ] Verify with Stage 1 arithmetic builtins

## Phase 5: Arithmetic Operators

### 1. Basic Arithmetic
- [ ] Write test: 'X + Y' transforms to '+'(X, Y)
- [ ] Write test: 'X - Y' transforms to '-'(X, Y)
- [ ] Write test: 'X * Y' transforms to '*'(X, Y)
- [ ] Write test: 'X / Y' transforms to '/'(X, Y)
- [ ] Write test: 'X // Y' transforms to '//'(X, Y) (mark as Stage 1 unsupported)
- [ ] Write test: 'X mod Y' transforms to 'mod'(X, Y) (mark as Stage 1 unsupported)
- [ ] Write test: 'X ** Y' transforms to '**'(X, Y) (mark as Stage 1 unsupported)
- [ ] Document policy for unsupported ops (parse ok, runtime fail)
- [ ] Implement arithmetic operator transforms
- [ ] Verify operator precedence

### 2. Precedence and Associativity
- [ ] Write test: '1 + 2 * 3' parses as '+'(1, '*'(2, 3))
- [ ] Write test: '1 * 2 + 3' parses as '+'('*'(1, 2), 3)
- [ ] Write test: '1 - 2 - 3' left-associates as '-'('-'(1, 2), 3)
- [ ] Write test: '2 ** 3 ** 2' right-associates as '**'(2, '**'(3, 2))
- [ ] Write test: Parentheses override precedence
- [ ] Verify all precedence rules
- [ ] Verify all associativity rules

### 3. Unary Operators and Negative Numbers
- [ ] Write test: '-X' transforms to '-'(X)
- [ ] Write test: '+X' transforms to '+'(X)
- [ ] Write test: Negative numeral policy: -3 as Int(-3) or '-'(Int(3))
- [ ] Write test: '1--1' parses correctly per policy
- [ ] Write test: 'X is -3 + 1' evaluates correctly per policy
- [ ] Write test: '- -X' parses correctly
- [ ] Write test: 'X - -Y' parses as '-'(X, '-'(Y))
- [ ] Write test: '1 - -1' parses as '-'(1, '-'(1))
- [ ] Write test: '1+-2' is error with "add space" hint
- [ ] Document and implement negative numeral policy
- [ ] Implement unary minus handling
- [ ] Implement unary plus handling
- [ ] Handle precedence of prefix operators
- [ ] Ensure pretty printer handles -3 correctly in operator mode
- [ ] Verify unary operator behaviour

## Phase 6: Integration and Testing

### 1. Parser Integration
- [ ] Write test: parse() applies reader automatically
- [ ] Write test: Operator and canonical forms equivalent
- [ ] Write test: All Stage 1 tests pass unchanged
- [ ] Update parser.py to use reader
- [ ] Add reader transform to pipeline
- [ ] Verify backwards compatibility

### 2. Comprehensive Precedence Tests
- [ ] Write test: All operators in single expression
- [ ] Write test: Deeply nested operators
- [ ] Write test: Edge cases for each precedence level
- [ ] Write property test: Random expressions parse deterministically
- [ ] Write property test: Parentheses preserve semantics
- [ ] Create precedence test suite
- [ ] Document precedence table

### 3. Semantic Equivalence Tests
- [ ] Write test: 'A, B' executes same as ','(A, B)
- [ ] Write test: 'A; B' executes same as ';'(A, B)
- [ ] Write test: '(A -> B; C)' executes same as canonical
- [ ] Write test: 'A, (B ; C)' backtracking order matches canonical
- [ ] Write test: '(A -> B ; C)' if-then-else semantics match
- [ ] Write test: Arithmetic expressions evaluate correctly
- [ ] Write test: Comparison operators work correctly
- [ ] Write test: Unsupported ops (//, mod, **) parse but fail at runtime
- [ ] Write test: Runtime failure is dev-mode fail (not ISO error)
- [ ] For each operator, verify execution unchanged
- [ ] Create equivalence test suite

### 4. Error Handling with Spans
- [ ] Write test: Unmatched parentheses error includes position
- [ ] Write test: Invalid operator usage shows column number
- [ ] Write test: 'A , ; B' shows "missing right argument for ',' before ';'"
- [ ] Write test: Error suggests fix ("add parentheses around...")
- [ ] Write test: Precedence conflicts show specific location
- [ ] Write test: "Unexpected ';' at column 9" style messages
- [ ] Write test: '=\\=' is single token, not parse error
- [ ] Write test: Quoted operators '+'(1,2) still parse
- [ ] Implement span-preserving error handling
- [ ] Add fix suggestions to error messages
- [ ] Verify helpful error reporting

### 5. Additional Edge Case Tests
- [ ] Write test: Greedy tokenization: '=\\=' is one token, not three
- [ ] Write test: Canonical input unchanged: ','(A,B) stays ','(A,B)
- [ ] Write test: Mixed lists/ops: 'member(X,[1,2,3]), X > 0' parses correctly
- [ ] Write test: 'reverse([1|T], R), R = [_, _ | _]' complex patterns work
- [ ] Write test: Whitespace irrelevant but spans differ: 'A,B' vs 'A , B'
- [ ] Write test: Error spans point to tokens, not whitespace
- [ ] Write test: 'mod' word-token: 'X mod Y' is operator, 'mod'(X,Y) is quoted
- [ ] Write test: Arity matters: ','(A,B,C) stays canonical (wrong arity)

## Phase 7: Pretty Printer Enhancement

### 1. Operator Detection
- [ ] Write test: Detect Struct(',', (A, B)) as conjunction (arity 2)
- [ ] Write test: Detect Struct(';', (A, B)) as disjunction (arity 2)
- [ ] Write test: Detect all operator canonical forms
- [ ] Write test: Ignore ','(A, B, C) - wrong arity, stays canonical
- [ ] Write test: Ignore 'mod' as atom, only 'mod'(X, Y) is operator
- [ ] Write test: Don't detect non-operator Structs
- [ ] Implement is_operator_struct function with arity checking
- [ ] Create operator pattern matching
- [ ] Verify operator detection

### 2. Operator Pretty Printing with Parenthesization
- [ ] Write test: pretty(Struct(',', (A, B))) → 'A, B'
- [ ] Write test: pretty(Struct('+', (X, Y))) → 'X + Y'
- [ ] Write test: Nested operators print with parens as needed
- [ ] Write test: Child enclosed iff precedence lower
- [ ] Write test: Child enclosed if equal precedence and misleading assoc
- [ ] Write test: 'A ; (B , C)' vs '(A ; B) , C' parenthesization
- [ ] Write test: '- - X' prints correctly
- [ ] Write test: '-X + Y * -Z' round-trips with correct parens
- [ ] Write test: '1 + 2 * 3' vs '(1 + 2) * 3' parenthesization
- [ ] Write test: Pretty print -3 as '-3' not '-(3)' in operator mode
- [ ] Implement operator pretty printing mode
- [ ] Implement parenthesization rule (equal precedence edge cases)
- [ ] Verify operator printing

### 3. Mode Selection
- [ ] Write test: Canonical mode prints Struct forms
- [ ] Write test: Operator mode prints operator forms
- [ ] Write test: Mode flag controls output style
- [ ] Add pretty printer mode parameter
- [ ] Implement mode switching
- [ ] Document both modes

### 4. Round-Trip Testing
- [ ] Write test: parse(pretty(term)) preserves semantics
- [ ] Write test: Operator round-trip works
- [ ] Write test: Canonical round-trip works (idempotence)
- [ ] Write test: Mixed prefix/infix: '-X + Y * -Z' round-trips
- [ ] Write test: Whitespace irrelevant: 'A,B' vs 'A , B' same AST
- [ ] Write property test: Random terms round-trip
- [ ] Write property test: parse_op(s) == parse_canonical(pretty_canonical(parse_op(s)))
- [ ] Verify round-trip for all operators
- [ ] Fix any round-trip failures

## Phase 8: Documentation and Examples

### 1. Operator Documentation
- [ ] Document each operator's syntax
- [ ] Document precedence table
- [ ] Document associativity rules
- [ ] Document canonical forms
- [ ] Create operator reference guide
- [ ] Add examples for each operator

### 2. Migration Guide
- [ ] Document canonical vs operator forms
- [ ] Show equivalent expressions
- [ ] Explain when to use each form
- [ ] Document pretty printer modes
- [ ] Create migration examples
- [ ] Update README with Stage 1.5

### 3. Test Programs
- [ ] Convert Stage 1 tests to use operators
- [ ] Create operator showcase programs
- [ ] Test complex nested expressions
- [ ] Create precedence demonstration
- [ ] Add to test suite
- [ ] Verify all examples work

## Phase 9: SWI Baseline and Final Validation

### 1. SWI Baseline Tests
- [ ] Write SWI test: Assert 'A , B ; C' vs 'A ; B , C' are different ASTs
- [ ] Write SWI test: Assert 'A ; B , C' vs '(A ; B) , C' are different
- [ ] Write SWI test: Assert canonical forms differ for precedence tests
- [ ] Write SWI test: '1 + 2 * 3' vs '(1 + 2) * 3' precedence
- [ ] Write SWI test: '2 ** 3 ** 2' right associativity
- [ ] Write SWI test: 'A -> B ; C' precedence
- [ ] Write SWI test: List/operator mix 'member(X, [1,2,3]), X > 0'
- [ ] Write SWI test: xfail runtime tests for unsupported ops (//, mod, **)
- [ ] Create SWI baseline corpus
- [ ] Verify PyLog matches SWI precedence/associativity
- [ ] Skip/xfail runtime tests for Stage 1 unsupported operators
- [ ] Document any intentional differences

### 2. Acceptance Criteria Verification
- [ ] If-then-else equivalent to canonical clauses
- [ ] Parentheses vs precedence produce identical AST
- [ ] All operators parse with correct precedence
- [ ] Associativity handled correctly
- [ ] Operator forms execute identically to canonical
- [ ] Error messages include character positions
- [ ] Pretty printer parenthesization correct

### 3. Performance Validation
- [ ] Benchmark reader transformation time
- [ ] Compare operator vs canonical parsing speed
- [ ] Test with large expressions
- [ ] Verify no execution performance impact
- [ ] Document any performance considerations

### 4. Integration Testing
- [ ] Run full Stage 1 test suite
- [ ] Run with operator-based tests
- [ ] Test REPL with operator input
- [ ] Test file loading with operators
- [ ] Verify complete backwards compatibility
- [ ] Test core precedence examples from plan
- [ ] Stage 1.5 complete

## Notes
- Reader is pure transformation layer using Pratt parser
- Grammar remains operator-agnostic (tokenization only, longest-match)
- Single source of truth: all precedence/associativity in operator table
- No engine modifications allowed
- xfx operators are non-chainable (syntax error if chained)
- Negative numeral policy must be consistent across reader/evaluator/pretty
- Unsupported operators (//, mod, **) parse but fail at runtime (dev-mode)
- Operator table is static (no op/3 directive)
- Must maintain exact semantic equivalence
- Pretty printer should support both modes with correct parenthesization
- All Stage 1 interfaces remain unchanged
- Soft-cut (*->) and univ operator (=..) deferred to future stages
- Error messages must include character positions and fix suggestions
- 'mod' is a word-token operator, still usable as quoted atom