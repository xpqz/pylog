# Stage 1.5: Operators via Reader

## Overview
Stage 1.5 adds comfortable operator syntax without modifying the core parser grammar or execution engine. This is achieved through a reader layer that transforms operator expressions into the canonical Struct/Atom forms already handled by the engine.

## Goals
1. Support standard ISO operators (`,`, `;`, `->`, arithmetic comparisons)
2. Maintain exact semantic equivalence with canonical forms
3. Preserve all Stage 1 interfaces unchanged
4. Enable familiar syntax for user comfort

## Architecture

### Reader Layer Design
```
Input Text → Lark Parser (operator-agnostic) → Token Stream → Reader (Pratt Parser) → Canonical AST → Engine
```

The grammar remains operator-agnostic, only tokenizing operators and parentheses. The Reader applies the operator table using a Pratt parser, transforming flat token streams into canonical Struct forms.

### Key Principles
1. **Pure transformation** - Reader only restructures AST, no semantic changes
2. **Single source of truth** - All precedence/associativity in the operator table
3. **No engine changes** - Engine continues to work with canonical forms only
4. **Reversible** - Pretty printer can optionally use operators or canonical forms
5. **Span preservation** - Reader keeps token positions for error reporting

## Operator Table

### Control Flow Operators
| Operator | Precedence | Type | Associativity | Canonical Form | Notes |
|----------|------------|------|---------------|----------------|-------|
| `,`      | 1000       | xfy  | right         | `','(A, B)` | |
| `;`      | 1100       | xfy  | right         | `';'(A, B)` | |
| `->`     | 1050       | xfy  | right         | `'->'(A, B)` | |

### Comparison Operators
| Operator | Precedence | Type | Associativity | Canonical Form |
|----------|------------|------|---------------|----------------|
| `=`      | 700        | xfx  | none          | `'='(A, B)` |
| `\=`     | 700        | xfx  | none          | `'\\='(A, B)` |
| `==`     | 700        | xfx  | none          | `'=='(A, B)` |
| `\==`    | 700        | xfx  | none          | `'\\=='(A, B)` |
| `@<`     | 700        | xfx  | none          | `'@<'(A, B)` |
| `@=<`    | 700        | xfx  | none          | `'@=<'(A, B)` |
| `@>`     | 700        | xfx  | none          | `'@>'(A, B)` |
| `@>=`    | 700        | xfx  | none          | `'@>='(A, B)` |

### Arithmetic Comparison
| Operator | Precedence | Type | Associativity | Canonical Form |
|----------|------------|------|---------------|----------------|
| `<`      | 700        | xfx  | none          | `'<'(A, B)` |
| `=<`     | 700        | xfx  | none          | `'=<'(A, B)` |
| `>`      | 700        | xfx  | none          | `'>'(A, B)` |
| `>=`     | 700        | xfx  | none          | `'>='(A, B)` |
| `=:=`    | 700        | xfx  | none          | `'=:='(A, B)` |
| `=\=`    | 700        | xfx  | none          | `'=\\='(A, B)` |

### Arithmetic Operators
| Operator | Precedence | Type | Associativity | Canonical Form | Notes |
|----------|------------|------|---------------|----------------|-------|
| `+`      | 500        | yfx  | left          | `'+'(A, B)` | |
| `-`      | 500        | yfx  | left          | `'-'(A, B)` | |
| `*`      | 400        | yfx  | left          | `'*'(A, B)` | |
| `/`      | 400        | yfx  | left          | `'/'(A, B)` | Float division |
| `//`     | 400        | yfx  | left          | `'//'(A, B)` | Integer division |
| `mod`    | 400        | yfx  | left          | `'mod'(A, B)` | |
| `**`     | 200        | xfy  | right         | `'**'(A, B)` | Exponentiation |
| `-`      | 200        | fy   | prefix        | `'-'(A)` | Unary minus |
| `+`      | 200        | fy   | prefix        | `'+'(A)` | Unary plus |

## Implementation Plan

### Phase 1: Grammar Tokenization
1. Extend grammar.lark to recognize operators as tokens only
2. Keep grammar operator-agnostic (no precedence in grammar)
3. Preserve token positions for error reporting

### Phase 2: Reader Implementation  
1. Create `prolog/parser/reader.py` with Pratt parser
2. Create `prolog/parser/operators.py` with static operator table
3. Transform token stream to canonical Struct forms
4. Preserve source spans through transformations

### Phase 3: Integration
1. Update parser.py to apply reader transform
2. Ensure all tests still pass with canonical forms
3. Add operator-based test variants

### Phase 4: Pretty Printer Update
1. Add operator mode to pretty printer
2. Detect canonical operator forms
3. Implement precedence-aware parenthesization
4. Support both operator and canonical output modes

## Testing Strategy

### Semantic Equivalence Tests
```python
# Both should produce identical results:
query1 = parse("X = 1, Y = 2")          # Operator form
query2 = parse("','('='(X,1), '='(Y,2))")  # Canonical form
assert execute(query1) == execute(query2)
```

### Precedence Tests
```python
# Verify correct precedence parsing
assert parse("A, B ; C") == parse("';'(','(A, B), C)")
assert parse("A ; B, C") == parse("';'(A, ','(B, C))")
```

### Associativity Tests
```python
# Left associative
assert parse("1 + 2 + 3") == parse("'+'('+'(1, 2), 3)")

# Right associative  
assert parse("A, B, C") == parse("','(A, ','(B, C))")
```

### If-Then-Else Tests
```python
# Standard if-then-else
assert parse("(A -> B ; C)") == parse("';'('->'(A, B), C)")

# Nested conditions
assert parse("((A -> B) ; (C -> D))") == parse("';'('->'(A, B), '->'(C, D))")
```

## Acceptance Criteria

1. **Operator Parsing**
   - All operators from table parse correctly
   - Precedence rules applied properly
   - Associativity handled correctly
   - Parentheses override precedence
   - SWI baseline tests pass for precedence/associativity

2. **Semantic Preservation**
   - Operator forms produce identical execution results to canonical forms
   - No changes to engine behaviour
   - All Stage 1 tests still pass
   - Property: `parse_op(s) == parse_canonical(pretty_canonical(parse_op(s)))`

3. **Round-trip Capability**
   - Parse → Pretty → Parse preserves semantics
   - Parenthesization rule: enclose child iff precedence lower or equal with misleading associativity
   - Can toggle between operator and canonical display

4. **Error Handling**
   - Clear errors with character spans (e.g., "Unexpected ';' at column 9")
   - Suggest fixes ("add parentheses around...")
   - Token positions preserved through Reader

## Non-Goals

1. **User-defined operators** - Not in this stage
2. **Engine operator awareness** - Engine works only with canonical forms
3. **Operator directive (op/3)** - Static table only
4. **Soft-cut (*->)** - Requires engine support, deferred to future stage
5. **Univ operator (=..)** - Remains canonical-only in Stage 1.5

## File Structure

```
prolog/parser/
  grammar.lark      # Extended with operator tokens only (no precedence)
  reader.py         # New: Pratt parser for operator precedence
  parser.py         # Updated to apply reader transform
  operators.py      # New: Static operator table (single source of truth)
```

## Example Transformations

### Simple Conjunction
```
% Input
foo(X), bar(X)

% After reader
','(foo(X), bar(X))
```

### If-Then-Else
```
% Input  
(X > 0 -> positive(X) ; negative(X))

% After reader
';'('->'('>'(X, 0), positive(X)), negative(X))
```

### Arithmetic Expression
```
% Input
X is Y + Z * 2

% After reader
is(X, '+'(Y, '*'(Z, 2)))
```

### Complex Goal
```
% Input
member(X, L), X > 0, (X mod 2 =:= 0 -> even(X) ; odd(X))

% After reader
','(member(X, L), ','('>'(X, 0), ';'('->'('=:='(mod(X, 2), 0), even(X)), odd(X))))
```

## Success Metrics

1. All operator expressions parse correctly
2. Execution semantics unchanged from Stage 1
3. User code can use natural operator syntax
4. Pretty printer can display both forms with correct parenthesization
5. No performance regression
6. Clean separation of concerns (reader/engine)
7. SWI baseline corpus agrees on precedence/associativity
8. Error messages include character positions and fix suggestions

## Testing Additions

### Core Precedence Tests
```python
# Arithmetic precedence
assert parse("1 + 2 * 3") == parse("'+'(1, '*'(2, 3))")
assert parse("2 ** 3 ** 2") == parse("'**'(2, '**'(3, 2))")  # right-assoc

# Control flow precedence  
assert parse("A , B ; C") == parse("';'(','(A, B), C)")
assert parse("A ; B , C") == parse("';'(A, ','(B, C))")
assert parse("A -> B ; C") == parse("';'('->'(A, B), C)")

# Unary operators
assert parse("- - X")  # Should parse without errors
assert parse("1 - -1") == parse("'-'(1, '-'(1))")
```

### SWI Baseline Tests
Create small corpus to verify agreement with SWI on:
- `A , B ; C` vs `A ; B , C`
- `1 + 2 * 3` vs `(1 + 2) * 3`
- `2 ** 3 ** 2` (right associativity)
- List/operator interaction: `member(X, [1,2,3]), X > 0`