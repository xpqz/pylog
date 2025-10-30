"""Tests for dynamic operator lookup and postfix operators - Issue #424.

This test suite verifies that the Reader supports:
1. Dynamic operator recognition (not hardcoded token types)
2. Postfix operators (xf and yf)
3. Custom operator injection via temporary operator table modification

Test scenarios based on ISO test patterns:
- fixme (prefix, fy, 1200)
- should_fail (postfix, xf, 1110)
- should_give (infix, xfx, 1110)
- should_throw (infix, xfx, 1110)
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.parser.reader import Reader, ReaderError
from prolog.parser import operators


class TestPostfixOperators:
    """Test postfix operator support (xf and yf)."""

    def test_postfix_xf_operator_basic(self):
        """Postfix xf operator should be applied to left expression."""
        # Temporarily add should_fail as postfix operator
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )

            reader = Reader()
            result = reader.read_term("foo should_fail")

            # should_fail is postfix, so: should_fail(foo)
            expected = Struct("should_fail", (Atom("foo"),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_yf_operator_basic(self):
        """Postfix yf operator should be applied to left expression."""
        # Temporarily add test_yf as postfix operator
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("test_yf", "postfix")] = (1100, "yf", "test_yf")

            reader = Reader()
            result = reader.read_term("bar test_yf")

            # test_yf is postfix, so: test_yf(bar)
            expected = Struct("test_yf", (Atom("bar"),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_with_structure_argument(self):
        """Postfix operator applied to a structure."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )

            reader = Reader()
            result = reader.read_term("foo(X) should_fail")

            expected = Struct("should_fail", (Struct("foo", (Var(0, "X"),)),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_precedence_constraint(self):
        """Postfix operator respects precedence constraints."""
        original = operators._operator_table.copy()
        try:
            # Add postfix operator with precedence 500
            operators._operator_table[("pf_op", "postfix")] = (500, "xf", "pf_op")

            reader = Reader()
            # The + has precedence 500, so: +(1, pf_op(2))
            result = reader.read_term("1 + 2 pf_op")

            expected = Struct("+", (Int(1), Struct("pf_op", (Int(2),))))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_xf_associativity(self):
        """Postfix xf operator does not allow chaining of same precedence."""
        original = operators._operator_table.copy()
        try:
            # Add two postfix operators with same precedence
            operators._operator_table[("pf1", "postfix")] = (1100, "xf", "pf1")
            operators._operator_table[("pf2", "postfix")] = (1100, "xf", "pf2")

            reader = Reader()
            # xf should not chain: X pf1 pf2 should fail or require parens
            with pytest.raises(ReaderError, match="postfix.*precedence"):
                reader.read_term("X pf1 pf2")
        finally:
            operators._operator_table = original

    def test_postfix_yf_associativity_allows_chaining(self):
        """Postfix yf operator allows chaining of same precedence."""
        original = operators._operator_table.copy()
        try:
            # Add yf postfix operator
            operators._operator_table[("pf_yf", "postfix")] = (1100, "yf", "pf_yf")

            reader = Reader()
            # yf should allow chaining: X pf_yf pf_yf → pf_yf(pf_yf(X))
            result = reader.read_term("X pf_yf pf_yf")

            expected = Struct("pf_yf", (Struct("pf_yf", (Var(0, "X"),)),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_with_infix_combination(self):
        """Postfix and infix operators working together."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )

            reader = Reader()
            # foo should_give bar should_fail
            # Same precedence, so: should_give(foo, should_fail(bar))
            result = reader.read_term("foo should_give bar should_fail")

            expected = Struct(
                "should_give", (Atom("foo"), Struct("should_fail", (Atom("bar"),)))
            )
            assert result == expected
        finally:
            operators._operator_table = original


class TestDynamicOperatorLookup:
    """Test that operator recognition is dynamic, not hardcoded."""

    def test_custom_infix_operator_recognition(self):
        """Custom infix operator should be recognized without code changes."""
        original = operators._operator_table.copy()
        try:
            # Add custom infix operator not in hardcoded list
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )

            reader = Reader()
            result = reader.read_term("foo should_give bar")

            expected = Struct("should_give", (Atom("foo"), Atom("bar")))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_custom_prefix_operator_recognition(self):
        """Custom prefix operator should be recognized without code changes."""
        original = operators._operator_table.copy()
        try:
            # Add custom prefix operator
            operators._operator_table[("fixme", "prefix")] = (1200, "fy", "fixme")

            reader = Reader()
            result = reader.read_term("fixme foo(X)")

            expected = Struct("fixme", (Struct("foo", (Var(0, "X"),)),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_word_operator_tokenization(self):
        """Word operators should produce tokens that can be looked up."""
        original = operators._operator_table.copy()
        try:
            # Add word operators
            operators._operator_table[("should_throw", "infix")] = (
                1110,
                "xfx",
                "should_throw",
            )

            reader = Reader()
            result = reader.read_term("goal should_throw error(E)")

            expected = Struct(
                "should_throw", (Atom("goal"), Struct("error", (Var(0, "E"),)))
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_multiple_custom_operators_together(self):
        """Multiple custom operators can be used together."""
        original = operators._operator_table.copy()
        try:
            # Add multiple ISO test operators
            operators._operator_table[("fixme", "prefix")] = (1200, "fy", "fixme")
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )

            reader = Reader()
            # fixme (foo should_give bar) should_fail
            result = reader.read_term("fixme foo should_give bar")

            expected = Struct(
                "fixme", (Struct("should_give", (Atom("foo"), Atom("bar"))))
            )
            assert result == expected
        finally:
            operators._operator_table = original


class TestOperatorPrecedenceWithPostfix:
    """Test precedence rules with postfix operators."""

    def test_postfix_lower_precedence_than_infix(self):
        """Postfix with lower precedence binds looser than infix."""
        original = operators._operator_table.copy()
        try:
            # Postfix with precedence 1100, + has precedence 500
            operators._operator_table[("pf", "postfix")] = (1100, "xf", "pf")

            reader = Reader()
            # 1 + 2 pf → pf(+(1, 2))
            result = reader.read_term("1 + 2 pf")

            expected = Struct("pf", (Struct("+", (Int(1), Int(2))),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_higher_precedence_than_infix(self):
        """Postfix with higher precedence binds tighter than infix."""
        original = operators._operator_table.copy()
        try:
            # Postfix with precedence 400 (tighter than + at 500)
            operators._operator_table[("pf", "postfix")] = (400, "xf", "pf")

            reader = Reader()
            # 1 + 2 pf → +(1, pf(2))
            result = reader.read_term("1 + 2 pf")

            expected = Struct("+", (Int(1), Struct("pf", (Int(2),))))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_parentheses_override_postfix_precedence(self):
        """Parentheses can override postfix operator precedence."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("pf", "postfix")] = (500, "xf", "pf")

            reader = Reader()
            # (1 + 2) pf → pf(+(1, 2))
            result = reader.read_term("(1 + 2) pf")

            expected = Struct("pf", (Struct("+", (Int(1), Int(2))),))
            assert result == expected
        finally:
            operators._operator_table = original


class TestISOTestPatternOperators:
    """Test operators used in ISO test patterns."""

    def test_fixme_prefix_operator(self):
        """fixme prefix operator (fy, 1200)."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("fixme", "prefix")] = (1200, "fy", "fixme")

            reader = Reader()
            result = reader.read_term("fixme foo(X) should_give bar")

            # fixme has highest precedence, wraps entire expression
            # Note: should_give would need to be defined for full parse
            # For now just test fixme works
            assert isinstance(result, Struct)
            assert result.functor == "fixme"
        finally:
            operators._operator_table = original

    def test_should_fail_postfix_operator(self):
        """should_fail postfix operator (xf, 1110)."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )

            reader = Reader()
            result = reader.read_term("(foo(X), bar(Y)) should_fail")

            expected = Struct(
                "should_fail",
                (
                    Struct(
                        ",",
                        (Struct("foo", (Var(0, "X"),)), Struct("bar", (Var(1, "Y"),))),
                    ),
                ),
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_should_give_infix_operator(self):
        """should_give infix operator (xfx, 1110)."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )

            reader = Reader()
            result = reader.read_term("foo(X) should_give bar(X)")

            expected = Struct(
                "should_give",
                (Struct("foo", (Var(0, "X"),)), Struct("bar", (Var(0, "X"),))),
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_should_throw_infix_operator(self):
        """should_throw infix operator (xfx, 1110)."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_throw", "infix")] = (
                1110,
                "xfx",
                "should_throw",
            )

            reader = Reader()
            result = reader.read_term("foo(X) should_throw error(type_error)")

            expected = Struct(
                "should_throw",
                (Struct("foo", (Var(0, "X"),)), Struct("error", (Atom("type_error"),))),
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_iso_test_pattern_combination(self):
        """All ISO test operators can be used together."""
        original = operators._operator_table.copy()
        try:
            # Add all ISO test operators
            operators._operator_table[("fixme", "prefix")] = (1200, "fy", "fixme")
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )
            operators._operator_table[("should_throw", "infix")] = (
                1110,
                "xfx",
                "should_throw",
            )

            reader = Reader()

            # Test various combinations
            result1 = reader.read_term("foo should_give bar")
            assert result1.functor == "should_give"

            result2 = reader.read_term("foo should_throw error")
            assert result2.functor == "should_throw"

            result3 = reader.read_term("foo should_fail")
            assert result3.functor == "should_fail"

            result4 = reader.read_term("fixme foo should_give bar")
            assert result4.functor == "fixme"
        finally:
            operators._operator_table = original


class TestOperatorTableIsolation:
    """Test that operator table modifications are properly isolated."""

    def test_operator_table_restoration(self):
        """Operator table should be restorable after modifications."""
        original = operators._operator_table.copy()
        original_keys = set(original.keys())

        try:
            # Add temporary operator
            operators._operator_table[("temp_op", "infix")] = (1100, "xfx", "temp_op")
            assert ("temp_op", "infix") in operators._operator_table
        finally:
            operators._operator_table = original

        # Verify restoration
        assert set(operators._operator_table.keys()) == original_keys
        assert ("temp_op", "infix") not in operators._operator_table

    def test_multiple_temporary_modifications(self):
        """Multiple temporary modifications should work independently."""
        original = operators._operator_table.copy()

        try:
            # First modification
            operators._operator_table[("op1", "infix")] = (1100, "xfx", "op1")
            reader = Reader()
            result1 = reader.read_term("a op1 b")
            assert result1.functor == "op1"

            # Second modification
            operators._operator_table[("op2", "postfix")] = (1100, "xf", "op2")
            result2 = reader.read_term("a op2")
            assert result2.functor == "op2"
        finally:
            operators._operator_table = original


class TestEdgeCases:
    """Test edge cases for dynamic operators and postfix."""

    def test_postfix_at_end_of_expression(self):
        """Postfix operator at end of expression."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("end_op", "postfix")] = (1100, "xf", "end_op")

            reader = Reader()
            result = reader.read_term("foo(bar(X)) end_op")

            expected = Struct(
                "end_op", (Struct("foo", (Struct("bar", (Var(0, "X"),)),)),)
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_with_list_argument(self):
        """Postfix operator applied to a list."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("list_op", "postfix")] = (1100, "xf", "list_op")

            reader = Reader()
            result = reader.read_term("[1, 2, 3] list_op")

            expected = Struct(
                "list_op", (PrologList((Int(1), Int(2), Int(3)), Atom("[]")),)
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_postfix_error_missing_left_operand(self):
        """Postfix operator without left operand should error."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("pf_op", "postfix")] = (1100, "xf", "pf_op")

            reader = Reader()
            # This should fail - postfix needs a left operand
            with pytest.raises(ReaderError):
                reader.read_term("pf_op")
        finally:
            operators._operator_table = original

    def test_dynamic_operator_not_found_still_errors(self):
        """Undefined operators should still raise errors."""
        reader = Reader()

        # unknown_op is not in operator table, should fail
        with pytest.raises(ReaderError, match="[Uu]nknown|[Uu]nexpected"):
            reader.read_term("foo unknown_op bar")

    def test_postfix_at_start_of_input_fails(self):
        """Postfix operator at start of input should fail (no left operand)."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("pf_op", "postfix")] = (1100, "xf", "pf_op")

            reader = Reader()
            # pf_op at start has no left operand
            # It will be tokenized as PF_OP and treated as an unexpected token in primary
            with pytest.raises(ReaderError, match="[Uu]nexpected"):
                reader.read_term("pf_op")
        finally:
            operators._operator_table = original

    def test_parentheses_with_postfix(self):
        """Parenthesized expression followed by postfix."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("pf", "postfix")] = (500, "xf", "pf")

            reader = Reader()
            # (a) pf should be same as a pf
            result1 = reader.read_term("(a) pf")
            result2 = reader.read_term("a pf")

            expected = Struct("pf", (Atom("a"),))
            assert result1 == expected
            assert result2 == expected
        finally:
            operators._operator_table = original

    def test_prefix_postfix_sandwich(self):
        """Prefix and postfix operators together based on precedence."""
        original = operators._operator_table.copy()
        try:
            # Prefix - at 200, postfix at 400
            operators._operator_table[("pf", "postfix")] = (400, "xf", "pf")

            reader = Reader()
            # -X pf: prefix - has precedence 200 (tighter), so: pf(-(X))
            result = reader.read_term("-X pf")

            expected = Struct("pf", (Struct("-", (Var(0, "X"),)),))
            assert result == expected
        finally:
            operators._operator_table = original

    def test_prefix_postfix_sandwich_opposite_precedence(self):
        """Prefix and postfix with postfix tighter."""
        original = operators._operator_table.copy()
        try:
            # Postfix at 100 (tighter than prefix - at 200)
            operators._operator_table[("pf", "postfix")] = (100, "xf", "pf")

            reader = Reader()
            # -X pf: postfix tighter, but - is prefix so it applies first to X
            # then pf applies to the result: pf(-(X))
            # Actually: prefix always consumes its argument first, then postfix
            # So this is still pf(-(X))
            result = reader.read_term("-X pf")

            expected = Struct("pf", (Struct("-", (Var(0, "X"),)),))
            assert result == expected
        finally:
            operators._operator_table = original


class TestISOPrecedenceInteractions:
    """Test ISO test operator precedence vs standard operators."""

    def test_iso_operators_vs_semicolon_precedence(self):
        """ISO test operators at 1110 vs semicolon at 1100."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )

            reader = Reader()
            # (X=1;X=2) should_give true
            # ; has precedence 1100 (tighter), should_give at 1110 (looser)
            # So: should_give(;(=(X,1), =(X,2)), true)
            result = reader.read_term("(X=1;X=2) should_give true")

            expected = Struct(
                "should_give",
                (
                    Struct(
                        ";",
                        (
                            Struct("=", (Var(0, "X"), Int(1))),
                            Struct("=", (Var(0, "X"), Int(2))),
                        ),
                    ),
                    Atom("true"),
                ),
            )
            assert result == expected
        finally:
            operators._operator_table = original

    def test_should_fail_xf_rejects_chained_suffix(self):
        """should_fail as xf should reject chaining at same precedence."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("should_fail", "postfix")] = (
                1110,
                "xf",
                "should_fail",
            )

            reader = Reader()
            # X should_fail should_fail should error
            with pytest.raises(ReaderError, match="postfix|precedence|chain"):
                reader.read_term("X should_fail should_fail")
        finally:
            operators._operator_table = original

    def test_fixme_prefix_fy_precedence(self):
        """fixme as fy (1200) wraps entire expression."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("fixme", "prefix")] = (1200, "fy", "fixme")
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )

            reader = Reader()
            # fixme foo should_give bar
            # fixme has precedence 1200 (loosest), wraps entire expression
            result = reader.read_term("fixme foo should_give bar")

            expected = Struct(
                "fixme", (Struct("should_give", (Atom("foo"), Atom("bar"))))
            )
            assert result == expected
        finally:
            operators._operator_table = original


class TestWordOperatorsAsFunctors:
    """Test that existing word operators can still be used as functors/atoms."""

    def test_mod_as_functor(self):
        """mod can be used as a functor (existing behavior)."""
        reader = Reader()

        # mod(5, 3) should parse as a structure
        result = reader.read_term("mod(5, 3)")

        expected = Struct("mod", (Int(5), Int(3)))
        assert result == expected

    def test_is_as_functor(self):
        """is can be used as a functor (existing behavior)."""
        reader = Reader()

        # is(X, 5) should parse as a structure
        result = reader.read_term("is(X, 5)")

        expected = Struct("is", (Var(0, "X"), Int(5)))
        assert result == expected

    def test_mod_as_atom(self):
        """mod can be used as a standalone atom."""
        reader = Reader()

        # mod by itself is an atom
        result = reader.read_term("mod")

        expected = Atom("mod")
        assert result == expected

    def test_is_as_atom(self):
        """is can be used as a standalone atom."""
        reader = Reader()

        # is by itself is an atom
        result = reader.read_term("is")

        expected = Atom("is")
        assert result == expected

    def test_custom_operator_without_prefix_as_functor(self):
        """Custom operator (not defined as prefix) can be used as functor."""
        original = operators._operator_table.copy()
        try:
            # Add should_give as infix only (not prefix)
            operators._operator_table[("should_give", "infix")] = (
                1110,
                "xfx",
                "should_give",
            )

            reader = Reader()
            # should_give(a, b) should parse as structure (no prefix definition)
            result = reader.read_term("should_give(a, b)")

            expected = Struct("should_give", (Atom("a"), Atom("b")))
            assert result == expected
        finally:
            operators._operator_table = original


class TestDynamicOperatorXFXChaining:
    """Test xfx non-chainable enforcement for dynamic operators."""

    def test_dynamic_xfx_operator_non_chainable(self):
        """Dynamic xfx operators should enforce non-chainable rule."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("custom_eq", "infix")] = (
                700,
                "xfx",
                "custom_eq",
            )

            reader = Reader()
            # X custom_eq Y custom_eq Z should fail
            with pytest.raises(ReaderError, match="xfx.*chain|non-chain"):
                reader.read_term("X custom_eq Y custom_eq Z")
        finally:
            operators._operator_table = original

    def test_dynamic_xfx_with_parentheses_ok(self):
        """Dynamic xfx with parentheses should work."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("custom_eq", "infix")] = (
                700,
                "xfx",
                "custom_eq",
            )

            reader = Reader()
            # (X custom_eq Y) custom_eq Z with parens should work
            result = reader.read_term("(X custom_eq Y) custom_eq Z")

            expected = Struct(
                "custom_eq",
                (Struct("custom_eq", (Var(0, "X"), Var(1, "Y"))), Var(2, "Z")),
            )
            assert result == expected
        finally:
            operators._operator_table = original


class TestErrorMessages:
    """Test specific error messages for dynamic/postfix operators."""

    def test_missing_right_arg_for_dynamic_infix(self):
        """Missing right argument for dynamic infix operator has good error."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("custom_op", "infix")] = (
                1100,
                "xfx",
                "custom_op",
            )

            reader = Reader()
            with pytest.raises(ReaderError, match="[Mm]issing.*right.*custom_op"):
                reader.read_term("foo custom_op")
        finally:
            operators._operator_table = original

    def test_xfx_chain_error_mentions_operator_name(self):
        """xfx chain error should mention the operator name."""
        original = operators._operator_table.copy()
        try:
            operators._operator_table[("my_xfx", "infix")] = (700, "xfx", "my_xfx")

            reader = Reader()
            with pytest.raises(ReaderError, match="my_xfx"):
                reader.read_term("a my_xfx b my_xfx c")
        finally:
            operators._operator_table = original
