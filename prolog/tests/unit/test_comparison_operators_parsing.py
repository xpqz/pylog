r"""Tests for comparison operator parsing - Issue #39.

This module tests the parsing and transformation of comparison operators:
- Equality/disequality: =, \=
- Structural comparison: ==, \==
- Term order: @<, @>, @=<, @>=
- Arithmetic comparison: <, >, =<, >=, =:=, =\=

All comparison operators are precedence 700, xfx (non-chainable).
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.parser.reader import Reader, ReaderError
from prolog.parser.parser import parse_clause, parse_query


class TestEqualityOperators:
    r"""Test equality (=) and disequality (\=) operators."""
    
    def test_unification_operator_infix(self):
        """Test = transforms to canonical '='(X, Y)."""
        reader = Reader()
        result = reader.read_term("X = Y")
        assert result == Struct("=", (Var(0, "X"), Var(1, "Y")))
    
    def test_unification_with_structures(self):
        """Test = with complex terms."""
        reader = Reader()
        result = reader.read_term("foo(X) = bar(Y, Z)")
        expected = Struct("=", (
            Struct("foo", (Var(0, "X"),)),
            Struct("bar", (Var(1, "Y"), Var(2, "Z")))
        ))
        assert result == expected
    
    def test_not_unifiable_operator(self):
        r"""Test \= transforms to canonical '\='(X, Y)."""
        reader = Reader()
        result = reader.read_term("X \\= Y")
        assert result == Struct("\\=", (Var(0, "X"), Var(1, "Y")))
    
    def test_equality_xfx_no_chaining(self):
        """Test that = cannot chain (xfx non-associative)."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        assert "non-chainable" in str(exc_info.value).lower()
    
    def test_disequality_xfx_no_chaining(self):
        r"""Test that \= cannot chain."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X \\= Y \\= Z")
        assert "non-chainable" in str(exc_info.value).lower()
    
    def test_equality_with_parentheses_allowed(self):
        """Test that parentheses allow multiple equalities."""
        reader = Reader()
        result = reader.read_term("(X = Y), (Y = Z)")
        expected = Struct(",", (
            Struct("=", (Var(0, "X"), Var(1, "Y"))),
            Struct("=", (Var(1, "Y"), Var(2, "Z")))
        ))
        assert result == expected
    
    def test_equality_in_clause(self):
        """Test = in clause body."""
        result = parse_clause("equal(X, Y) :- X = Y.")
        assert result.head == Struct("equal", (Var(0, "X"), Var(1, "Y")))
        assert result.body == [Struct("=", (Var(0, "X"), Var(1, "Y")))]


class TestStructuralComparison:
    """Test structural comparison operators."""
    
    def test_structural_equality(self):
        """Test == transforms to canonical '=='(X, Y)."""
        reader = Reader()
        result = reader.read_term("X == Y")
        assert result == Struct("==", (Var(0, "X"), Var(1, "Y")))
    
    def test_structural_inequality(self):
        r"""Test \== transforms to canonical '\=='(X, Y)."""
        reader = Reader()
        result = reader.read_term("X \\== Y")
        assert result == Struct("\\==", (Var(0, "X"), Var(1, "Y")))
    
    def test_structural_equality_xfx_no_chaining(self):
        """Test that == cannot chain."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X == Y == Z")
        assert "non-chainable" in str(exc_info.value).lower()
    
    def test_structural_inequality_xfx_no_chaining(self):
        r"""Test that \== cannot chain."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X \\== Y \\== Z")
        assert "non-chainable" in str(exc_info.value).lower()


class TestTermOrderOperators:
    """Test term order comparison operators."""
    
    def test_term_less_than(self):
        """Test @< transforms correctly."""
        reader = Reader()
        result = reader.read_term("X @< Y")
        assert result == Struct("@<", (Var(0, "X"), Var(1, "Y")))
    
    def test_term_greater_than(self):
        """Test @> transforms correctly."""
        reader = Reader()
        result = reader.read_term("X @> Y")
        assert result == Struct("@>", (Var(0, "X"), Var(1, "Y")))
    
    def test_term_less_equal(self):
        """Test @=< transforms correctly."""
        reader = Reader()
        result = reader.read_term("X @=< Y")
        assert result == Struct("@=<", (Var(0, "X"), Var(1, "Y")))
    
    def test_term_greater_equal(self):
        """Test @>= transforms correctly."""
        reader = Reader()
        result = reader.read_term("X @>= Y")
        assert result == Struct("@>=", (Var(0, "X"), Var(1, "Y")))
    
    def test_term_order_xfx_no_chaining(self):
        """Test that term order operators cannot chain."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X @< Y @< Z")
        assert "non-chainable" in str(exc_info.value).lower()
        
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X @=< Y @=< Z")
        assert "non-chainable" in str(exc_info.value).lower()


class TestArithmeticComparison:
    """Test arithmetic comparison operators."""
    
    def test_less_than(self):
        """Test < transforms correctly."""
        reader = Reader()
        result = reader.read_term("X < Y")
        assert result == Struct("<", (Var(0, "X"), Var(1, "Y")))
    
    def test_greater_than(self):
        """Test > transforms correctly."""
        reader = Reader()
        result = reader.read_term("X > Y")
        assert result == Struct(">", (Var(0, "X"), Var(1, "Y")))
    
    def test_less_equal(self):
        """Test =< transforms correctly."""
        reader = Reader()
        result = reader.read_term("X =< Y")
        assert result == Struct("=<", (Var(0, "X"), Var(1, "Y")))
    
    def test_greater_equal(self):
        """Test >= transforms correctly."""
        reader = Reader()
        result = reader.read_term("X >= Y")
        assert result == Struct(">=", (Var(0, "X"), Var(1, "Y")))
    
    def test_arithmetic_equality(self):
        """Test =:= transforms correctly."""
        reader = Reader()
        result = reader.read_term("X =:= Y")
        assert result == Struct("=:=", (Var(0, "X"), Var(1, "Y")))
    
    def test_arithmetic_inequality(self):
        r"""Test =\= transforms correctly."""
        reader = Reader()
        result = reader.read_term("X =\\= Y")
        assert result == Struct("=\\=", (Var(0, "X"), Var(1, "Y")))
    
    def test_arithmetic_comparison_xfx_no_chaining(self):
        """Test that arithmetic comparisons cannot chain."""
        reader = Reader()
        
        # Test < cannot chain
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("A < B < C")
        assert "non-chainable" in str(exc_info.value).lower()
        
        # Test =< cannot chain
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("A =< B =< C")
        assert "non-chainable" in str(exc_info.value).lower()
        
        # Test =:= cannot chain
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X =:= Y =:= Z")
        assert "non-chainable" in str(exc_info.value).lower()
    
    def test_arithmetic_comparison_with_parentheses(self):
        """Test that parentheses allow multiple comparisons."""
        reader = Reader()
        result = reader.read_term("(X =:= Y), (Y =:= Z)")
        expected = Struct(",", (
            Struct("=:=", (Var(0, "X"), Var(1, "Y"))),
            Struct("=:=", (Var(1, "Y"), Var(2, "Z")))
        ))
        assert result == expected
    
    def test_comparison_with_numbers(self):
        """Test comparisons with integer literals."""
        reader = Reader()
        result = reader.read_term("3 < 5")
        assert result == Struct("<", (Int(3), Int(5)))
        
        result = reader.read_term("X =:= 42")
        assert result == Struct("=:=", (Var(0, "X"), Int(42)))


class TestPrecedenceInteractions:
    """Test precedence interactions between comparison and other operators."""
    
    def test_comparison_vs_arithmetic_precedence(self):
        """Test that arithmetic (500/400) binds tighter than comparison (700)."""
        reader = Reader()
        
        # X < Y + Z should parse as X < (Y + Z)
        result = reader.read_term("X < Y + Z")
        expected = Struct("<", (
            Var(0, "X"),
            Struct("+", (Var(1, "Y"), Var(2, "Z")))
        ))
        assert result == expected
        
        # A * B =:= C should parse as (A * B) =:= C
        result = reader.read_term("A * B =:= C")
        expected = Struct("=:=", (
            Struct("*", (Var(0, "A"), Var(1, "B"))),
            Var(2, "C")
        ))
        assert result == expected
    
    def test_comparison_vs_conjunction_precedence(self):
        """Test that comparison (700) binds tighter than conjunction (1000)."""
        reader = Reader()
        
        # X = Y, Z = W should parse as (X = Y) , (Z = W)
        result = reader.read_term("X = Y, Z = W")
        expected = Struct(",", (
            Struct("=", (Var(0, "X"), Var(1, "Y"))),
            Struct("=", (Var(2, "Z"), Var(3, "W")))
        ))
        assert result == expected
    
    def test_mixed_comparison_operators(self):
        """Test different comparison operators in same expression."""
        reader = Reader()
        
        # With conjunction
        result = reader.read_term("X < Y, Y =< Z")
        expected = Struct(",", (
            Struct("<", (Var(0, "X"), Var(1, "Y"))),
            Struct("=<", (Var(1, "Y"), Var(2, "Z")))
        ))
        assert result == expected
        
        # With disjunction
        result = reader.read_term("X == Y ; X = Y")
        expected = Struct(";", (
            Struct("==", (Var(0, "X"), Var(1, "Y"))),
            Struct("=", (Var(0, "X"), Var(1, "Y")))
        ))
        assert result == expected


class TestComparisonInClauses:
    """Test comparison operators in clause contexts."""
    
    def test_comparison_in_rule_body(self):
        """Test comparisons in rule bodies."""
        result = parse_clause("less(X, Y) :- X < Y.")
        assert result.head == Struct("less", (Var(0, "X"), Var(1, "Y")))
        assert result.body == [Struct("<", (Var(0, "X"), Var(1, "Y")))]
    
    def test_multiple_comparisons_in_body(self):
        """Test multiple comparisons in clause body."""
        result = parse_clause("between(X, Y, Z) :- X =< Y, Y =< Z.")
        assert result.head == Struct("between", (Var(0, "X"), Var(1, "Y"), Var(2, "Z")))
        assert result.body == [
            Struct("=<", (Var(0, "X"), Var(1, "Y"))),
            Struct("=<", (Var(1, "Y"), Var(2, "Z")))
        ]
    
    def test_comparison_in_query(self):
        """Test comparisons in queries."""
        result = parse_query("?- X = 3, X < 5.")
        assert result == [
            Struct("=", (Var(0, "X"), Int(3))),
            Struct("<", (Var(0, "X"), Int(5)))
        ]


class TestErrorMessages:
    """Test that error messages are helpful for comparison operators."""
    
    def test_chaining_error_message_helpful(self):
        """Test that chaining error messages mention the operator and suggest parentheses."""
        reader = Reader()
        
        # Test with =
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        error_msg = str(exc_info.value).lower()
        assert ("=" in error_msg and "non-chainable" in error_msg)
        
        # Test with <
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("A < B < C")
        error_msg = str(exc_info.value).lower()
        assert ("<" in error_msg and "non-chainable" in error_msg)


class TestCanonicalForms:
    """Test that operators transform to their canonical quoted atom forms."""
    
    def test_canonical_form_equality(self):
        """Test canonical forms for equality operators."""
        reader = Reader()
        
        # = should have canonical form '='
        result = reader.read_term("X = Y")
        assert isinstance(result, Struct)
        assert result.functor == "="
        
        # \= should have canonical form '\\='
        result = reader.read_term("X \\= Y")
        assert isinstance(result, Struct)
        assert result.functor == "\\="
    
    def test_canonical_form_structural(self):
        """Test canonical forms for structural comparison."""
        reader = Reader()
        
        result = reader.read_term("X == Y")
        assert isinstance(result, Struct)
        assert result.functor == "=="
        
        result = reader.read_term("X \\== Y")
        assert isinstance(result, Struct)
        assert result.functor == "\\=="
    
    def test_canonical_form_arithmetic(self):
        """Test canonical forms for arithmetic comparison."""
        reader = Reader()
        
        result = reader.read_term("X < Y")
        assert isinstance(result, Struct)
        assert result.functor == "<"
        
        result = reader.read_term("X =< Y")
        assert isinstance(result, Struct)
        assert result.functor == "=<"
        
        result = reader.read_term("X =:= Y")
        assert isinstance(result, Struct)
        assert result.functor == "=:="
        
        result = reader.read_term("X =\\= Y")
        assert isinstance(result, Struct)
        assert result.functor == "=\\="
    
    def test_canonical_form_term_order(self):
        """Test canonical forms for term order operators."""
        reader = Reader()
        
        result = reader.read_term("X @< Y")
        assert isinstance(result, Struct)
        assert result.functor == "@<"
        
        result = reader.read_term("X @=< Y")
        assert isinstance(result, Struct)
        assert result.functor == "@=<"


class TestEdgeCases:
    """Test edge cases and special scenarios."""
    
    def test_comparison_with_lists(self):
        """Test comparisons with list terms."""
        reader = Reader()
        result = reader.read_term("[1,2] = [X,Y]")
        from prolog.ast.terms import List as PrologList
        expected = Struct("=", (
            PrologList((Int(1), Int(2)), Atom("[]")),
            PrologList((Var(0, "X"), Var(1, "Y")), Atom("[]"))
        ))
        assert result == expected
    
    def test_comparison_with_atoms(self):
        """Test comparisons with atoms."""
        reader = Reader()
        result = reader.read_term("abc @< def")
        assert result == Struct("@<", (Atom("abc"), Atom("def")))
    
    def test_whitespace_handling(self):
        """Test that whitespace doesn't affect parsing."""
        reader = Reader()
        
        # Tight spacing
        result1 = reader.read_term("X=Y")
        # Spaced out
        result2 = reader.read_term("X = Y")
        # Extra spaces
        result3 = reader.read_term("X  =  Y")
        
        expected = Struct("=", (Var(0, "X"), Var(1, "Y")))
        assert result1 == expected
        assert result2 == expected
        assert result3 == expected
    
    def test_all_comparison_ops_at_precedence_700(self):
        """Verify all comparison operators are at precedence 700."""
        from prolog.parser.operators import OPERATOR_TABLE
        
        comparison_ops = [
            ('=', 'infix'), ('\\=', 'infix'),
            ('==', 'infix'), ('\\==', 'infix'),
            ('@<', 'infix'), ('@>', 'infix'), ('@=<', 'infix'), ('@>=', 'infix'),
            ('<', 'infix'), ('>', 'infix'), ('=<', 'infix'), ('>=', 'infix'),
            ('=:=', 'infix'), ('=\\=', 'infix')
        ]
        
        for op, pos in comparison_ops:
            info = OPERATOR_TABLE.get((op, pos))
            assert info is not None, f"Operator {op} not in table"
            precedence, op_type, _ = info
            assert precedence == 700, f"Operator {op} has precedence {precedence}, expected 700"
            assert op_type == 'xfx', f"Operator {op} has type {op_type}, expected xfx"


class TestAdditionalCases:
    """Additional test cases from review feedback."""
    
    def test_mixed_xfx_chain_errors(self):
        """Test that mixing different xfx operators in a chain is rejected."""
        reader = Reader()
        test_cases = [
            "X = Y == Z",
            "X < Y =< Z", 
            "A =:= B =\\= C",
            "X @< Y @=< Z"
        ]
        for src in test_cases:
            with pytest.raises(ReaderError) as exc_info:
                reader.read_term(src)
            assert "non-chainable" in str(exc_info.value).lower()
    
    def test_greedy_tokenization_no_spaces(self):
        """Test tokenization handles multi-char operators without spaces."""
        reader = Reader()
        
        # Basic cases
        assert reader.read_term("X=\\=Y") == Struct("=\\=", (Var(0, "X"), Var(1, "Y")))
        assert reader.read_term("X\\==Y") == Struct("\\==", (Var(0, "X"), Var(1, "Y")))
        assert reader.read_term("X@=<Y") == Struct("@=<", (Var(0, "X"), Var(1, "Y")))
        
        # Additional tight tokenization tests for >= and @>=
        assert reader.read_term("X>=Y") == Struct(">=", (Var(0, "X"), Var(1, "Y")))
        assert reader.read_term("X@>=Y") == Struct("@>=", (Var(0, "X"), Var(1, "Y")))
        
        # Tricky case: "=\\==" is actually invalid (can't have unary \\==)
        # This should raise an error
        with pytest.raises(ReaderError):
            reader.read_term("X=\\==Y")
    
    def test_term_order_binds_tighter_than_conjunction(self):
        """Test term order operators bind tighter than conjunction."""
        reader = Reader()
        result = reader.read_term("X @< Y, Z @< W")
        expected = Struct(",", (
            Struct("@<", (Var(0, "X"), Var(1, "Y"))),
            Struct("@<", (Var(2, "Z"), Var(3, "W")))
        ))
        assert result == expected
    
    def test_term_order_with_disjunction(self):
        """Test term order operators with disjunction."""
        reader = Reader()
        result = reader.read_term("X @=< Y ; X @>= Y")
        expected = Struct(";", (
            Struct("@=<", (Var(0, "X"), Var(1, "Y"))),
            Struct("@>=", (Var(0, "X"), Var(1, "Y")))
        ))
        assert result == expected
    
    def test_comparison_inside_functor_argument(self):
        """Test comparison operators inside structure arguments."""
        reader = Reader()
        result = reader.read_term("f(X < Y)")
        expected = Struct("f", (Struct("<", (Var(0, "X"), Var(1, "Y"))),))
        assert result == expected
    
    def test_parentheses_allow_multiple_term_order_comparisons(self):
        """Test parentheses enable multiple term order comparisons."""
        reader = Reader()
        result = reader.read_term("(X @< Y), (Y @< Z)")
        expected = Struct(",", (
            Struct("@<", (Var(0, "X"), Var(1, "Y"))),
            Struct("@<", (Var(1, "Y"), Var(2, "Z")))
        ))
        assert result == expected
    
    def test_comparison_in_clause_head_position(self):
        """Test that infix comparisons in head position parse as structures."""
        # The reader accepts these syntactically as regular structures
        # Semantic validation would happen at a different layer
        result = parse_clause("X = Y.")
        assert result.head == Struct("=", (Var(0, "X"), Var(1, "Y")))
        assert result.body == []
        
        result = parse_clause("X < Y.")
        assert result.head == Struct("<", (Var(0, "X"), Var(1, "Y")))
        assert result.body == []
    
    def test_whitespace_tabs_newlines(self):
        """Test whitespace handling with tabs and newlines."""
        reader = Reader()
        tight = reader.read_term("X=Y")
        spaced = reader.read_term("X \t =\n Y")
        assert tight == spaced == Struct("=", (Var(0, "X"), Var(1, "Y")))
    
    def test_canonical_form_remaining_ops(self):
        """Test canonical forms for remaining operators."""
        reader = Reader()
        
        result = reader.read_term("X @> Y")
        assert isinstance(result, Struct)
        assert result.functor == "@>"
        
        result = reader.read_term("X >= Y")
        assert isinstance(result, Struct)
        assert result.functor == ">="
        
        result = reader.read_term("X > Y")
        assert isinstance(result, Struct)
        assert result.functor == ">"
    
    def test_error_position_reporting(self):
        """Test that chaining errors report position of second operator."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        
        error = exc_info.value
        # The second = is at position 6 (0-indexed)
        assert error.position == 6
        assert "=" in str(error)
        assert "non-chainable" in str(error).lower()