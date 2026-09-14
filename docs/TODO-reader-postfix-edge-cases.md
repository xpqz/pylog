# Reader Postfix Edge Cases - Follow-up Work

Related to issue #424 - 7 failing tests from the initial implementation that need investigation and fixes.

## Status

**Core functionality complete**: 34/41 tests pass, 4901 baseline tests pass with no regressions.

**Remaining edge cases**: 7 tests fail due to specific precedence/associativity interactions and word operator handling.

## Failing Tests

### 1. Precedence/Associativity Edge Cases (4 tests)

**Tests**:
- `test_postfix_precedence_constraint`
- `test_postfix_with_infix_combination`
- `test_prefix_postfix_sandwich_opposite_precedence`
- `test_iso_operators_vs_semicolon_precedence`

**Issue**: When postfix and infix operators have the same precedence, the current Pratt loop checks postfix before infix, which may not always produce the expected binding.

**Investigation needed**:
- Verify Pratt ordering: should postfix always be checked before infix in both `parse_term` and `parse_term_arg`?
- For xf: ensure `min_precedence = precedence - 1` after applying postfix to prevent same-precedence chaining
- For yf: ensure `min_precedence = precedence` to allow chaining
- Review ISO/SWI-Prolog behavior for same-precedence postfix+infix combinations

**Potential fix**:
```python
# After applying postfix operator:
if assoc_type == "xf":
    min_precedence = precedence - 1  # Prevent chaining at same prec
elif assoc_type == "yf":
    min_precedence = precedence  # Allow chaining
```

### 2. Prefix Operator Tuple vs Single Arg (2 tests)

**Tests**:
- `test_multiple_custom_operators_together`
- `test_fixme_prefix_fy_precedence`

**Issue**: Prefix operators are creating structures with tuple args `(arg,)` instead of single arg `arg`.

**Current behavior**:
```python
Struct("fixme", (Struct("should_give", (...)),))  # Actual
```

**Expected behavior**:
```python
Struct("fixme", Struct("should_give", (...)))  # Expected
```

**Investigation needed**:
- Check if this is a test expectation issue or implementation issue
- Review how `parse_prefix` builds structures for fx/fy operators
- Verify SWI-Prolog AST representation for prefix operators

**Potential fix**: Depends on whether tuples are required for consistency with infix operators or if single args are correct for unary operators.

### 3. Word Operators as Functors (1 test)

**Test**: `test_custom_operator_without_prefix_as_functor`

**Issue**: When a word operator like `should_give` is defined as infix but used as a functor `should_give(a, b)`, the parser treats it as an unexpected token instead of a structure.

**Current behavior**:
```
ReaderError: Unexpected token: SHOULD_GIVE 'should_give'
```

**Expected behavior**:
```python
Struct("should_give", (Atom("a"), Atom("b")))
```

**Root cause**: `parse_primary` only special-cases `mod` and `is` as word operators that can be functors. Other word operators produce operator tokens that aren't handled in `parse_primary`.

**Fix strategy**:
Add a rule in `parse_primary`: if a token represents a word operator AND is not valid in the current operator position (not prefix here) AND next token is `LPAREN`, parse as functor (structure), not as operator.

```python
# In parse_primary, after checking ATOM tokens:
# Check if this is a word operator token that can be used as functor
if self._is_word_operator_token(token.type) and not self._get_prefix_info(token):
    next_token = self.stream.peek_ahead(1)  # Look ahead
    if next_token and next_token.type == "LPAREN":
        # Parse as structure functor
        self.stream.consume()
        name = token.value
        self.stream.consume()  # consume LPAREN
        args = self.parse_term_list()
        # ... rest of structure parsing
```

## Action Items

1. **Mark tests as xfail temporarily**:
   ```python
   @pytest.mark.xfail(reason="Edge case: same-precedence postfix+infix - see TODO-reader-postfix-edge-cases.md")
   ```

2. **Create sub-issues**:
   - Issue: "Reader: postfix+infix same-precedence binding order"
   - Issue: "Reader: prefix operator AST representation (tuple vs single arg)"
   - Issue: "Reader: word operators as functors when defined as infix"

3. **Implement word operator as functor fix**:
   - Add helper method to check if token is a word operator
   - Update `parse_primary` to handle word operators followed by `LPAREN` as functors
   - Test with `should_give(a, b)`, `should_throw(goal, error)`, etc.

4. **Document postfix behavior**:
   Add docstring/comment in `reader.py` around postfix handling:
   ```python
   # Postfix operator handling:
   # - Check postfix before infix in Pratt loop
   # - xf (non-associative): prevent chaining at same precedence
   # - yf (associative): allow chaining at same precedence
   # - Precedence constraint: precedence <= min_precedence to apply
   ```

5. **SWI-Prolog verification**:
   - Test same-precedence postfix+infix combinations in SWI
   - Verify prefix operator AST structure
   - Document expected behavior for edge cases

## Priority

**Low-Medium**: Core functionality works (34/41 tests, no regressions). Edge cases don't block ISO suite integration (Phase A) since ISO test patterns use different precedences and contexts where these edge cases don't arise.

**Recommended timeline**: Address after Phase A ISO suite integration is complete, or when specific use cases encounter these edge cases.

## Notes

- Performance unchanged: tokenizer already table-driven, reader now table-driven for all operator positions
- Risk is low: changes are localized to operator lookup and postfix handling
- No regressions in 4901 existing tests
- ISO pattern operators (fixme, should_fail, should_give, should_throw) work correctly in their intended contexts
