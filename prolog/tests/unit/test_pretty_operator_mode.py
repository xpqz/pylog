"""
Test pretty printer operator mode enhancements (Stage 1.5).

Tests cover:
1. Operator detection - recognizing canonical operator forms
2. Operator mode pretty printing
3. Parenthesization rules for precedence and associativity
4. Round-trip property preservation
5. Negative number handling
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.pretty import pretty
from prolog.parser.reader import Reader
from prolog.parser.operators import get_operator_info


class TestOperatorDetection:
    """Test detection of canonical operator forms."""
    
    def test_detect_infix_comma(self):
        """Detect Struct(',', (A, B)) as comma operator."""
        term = Struct(",", (Atom("a"), Atom("b")))
        # When operator mode is implemented, this should print as "a, b"
        # For now we'll check the current canonical output
        assert pretty(term) == "','(a, b)"
        
    def test_detect_infix_semicolon(self):
        """Detect Struct(';', (A, B)) as semicolon operator."""
        term = Struct(";", (Atom("a"), Atom("b")))
        assert pretty(term) == "';'(a, b)"
        
    def test_detect_infix_arrow(self):
        """Detect Struct('->', (A, B)) as if-then operator."""
        term = Struct("->", (Atom("cond"), Atom("then")))
        assert pretty(term) == "'->'(cond, then)"
        
    def test_detect_arithmetic_ops(self):
        """Detect arithmetic operators in canonical form."""
        add_term = Struct("+", (Int(1), Int(2)))
        mul_term = Struct("*", (Int(3), Int(4)))
        assert pretty(add_term) == "'+'(1, 2)"
        assert pretty(mul_term) == "'*'(3, 4)"
        
    def test_detect_comparison_ops(self):
        """Detect comparison operators in canonical form."""
        eq_term = Struct("=", (Var(1, "X"), Int(5)))
        lt_term = Struct("<", (Var(2, "Y"), Int(10)))
        assert "=" in pretty(eq_term)
        assert "<" in pretty(lt_term)
        
    def test_detect_unary_minus(self):
        """Detect Struct('-', (X,)) as unary minus."""
        term = Struct("-", (Int(3),))
        assert pretty(term) == "'-'(3)"
        
    def test_detect_unary_plus(self):
        """Detect Struct('+', (X,)) as unary plus."""  
        term = Struct("+", (Int(5),))
        assert pretty(term) == "'+'(5)"
        
    def test_detect_negation_as_failure(self):
        """Detect Struct('\\+', (X,)) as negation."""
        term = Struct("\\+", (Atom("fail"),))
        assert pretty(term) == "'\\\\+'(fail)"
        
    def test_ignore_wrong_arity(self):
        """Operators with wrong arity remain canonical."""
        # Comma with 3 args - not an operator
        term = Struct(",", (Atom("a"), Atom("b"), Atom("c")))
        assert pretty(term) == "','(a, b, c)"
        
        # Plus with 3 args - not an operator
        term2 = Struct("+", (Int(1), Int(2), Int(3)))
        assert pretty(term2) == "'+'(1, 2, 3)"
        
    def test_detect_mod_word_operator(self):
        """Detect 'mod' as word-token operator."""
        term = Struct("mod", (Int(10), Int(3)))
        assert pretty(term) == "mod(10, 3)"
        
    def test_detect_is_operator(self):
        """Detect 'is' as operator."""
        term = Struct("is", (Var(1, "X"), Struct("+", (Int(1), Int(2)))))
        result = pretty(term)
        assert "is" in result


class TestOperatorModePrinting:
    """Test pretty printing in operator mode."""
    
    def test_operator_mode_flag(self):
        """Test operator_mode parameter enables operator printing."""
        term = Struct(",", (Atom("a"), Atom("b")))
        # When implemented, should have:
        # assert pretty(term, operator_mode=True) == "a, b"
        # assert pretty(term, operator_mode=False) == "','(a, b)"
        
    def test_simple_infix_operators(self):
        """Test simple infix operators print correctly."""
        # When operator mode is implemented:
        # assert pretty(Struct("+", (Int(1), Int(2))), operator_mode=True) == "1 + 2"
        # assert pretty(Struct("*", (Int(3), Int(4))), operator_mode=True) == "3 * 4"
        pass
        
    def test_nested_operators_same_precedence(self):
        """Test nested operators with same precedence."""
        # 1 + 2 + 3 should be ((1 + 2) + 3) due to left associativity
        term = Struct("+", (Struct("+", (Int(1), Int(2))), Int(3)))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "1 + 2 + 3"
        
    def test_nested_operators_different_precedence(self):
        """Test nested operators with different precedence."""
        # 1 + 2 * 3 should be 1 + (2 * 3)
        term = Struct("+", (Int(1), Struct("*", (Int(2), Int(3)))))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "1 + 2 * 3"
        
    def test_prefix_operators(self):
        """Test prefix operators print correctly."""
        # -X, +Y, \+Z
        neg_term = Struct("-", (Var(1, "X"),))
        pos_term = Struct("+", (Var(2, "Y"),))
        not_term = Struct("\\+", (Var(3, "Z"),))
        # When implemented:
        # assert pretty(neg_term, operator_mode=True) == "-X"
        # assert pretty(pos_term, operator_mode=True) == "+Y"
        # assert pretty(not_term, operator_mode=True) == "\\+Z"
        
    def test_mixed_prefix_infix(self):
        """Test mixed prefix and infix operators."""
        # -X + Y
        term = Struct("+", (Struct("-", (Var(1, "X"),)), Var(2, "Y")))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "-X + Y"
        
    def test_control_flow_operators(self):
        """Test control flow operators (comma, semicolon, arrow)."""
        # (A -> B ; C)
        term = Struct(";", (
            Struct("->", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "(a -> b ; c)"
        
    def test_negative_integer_handling(self):
        """Test negative integers print as literals, not operators."""
        # -3 should print as "-3" not "-(3)"
        term = Int(-3)
        assert pretty(term) == "-3"
        
        # But -(X) where X is a variable should use operator
        neg_var = Struct("-", (Var(1, "X"),))
        # When implemented:
        # assert pretty(neg_var, operator_mode=True) == "-X"


class TestParenthesizationRules:
    """Test parenthesization for correct precedence and associativity."""
    
    def test_lower_precedence_needs_parens(self):
        """Child with lower precedence needs parentheses."""
        # (1 + 2) * 3 - plus has lower precedence than multiply
        term = Struct("*", (Struct("+", (Int(1), Int(2))), Int(3)))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "(1 + 2) * 3"
        
    def test_higher_precedence_no_parens(self):
        """Child with higher precedence doesn't need parentheses."""
        # 1 + 2 * 3 - multiply has higher precedence than plus
        term = Struct("+", (Int(1), Struct("*", (Int(2), Int(3)))))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "1 + 2 * 3"
        
    def test_same_precedence_correct_assoc_no_parens(self):
        """Same precedence with correct associativity needs no parens."""
        # 1 + 2 + 3 - left associative, left child doesn't need parens
        term = Struct("+", (Struct("+", (Int(1), Int(2))), Int(3)))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "1 + 2 + 3"
        
    def test_same_precedence_wrong_assoc_needs_parens(self):
        """Same precedence with wrong associativity needs parens."""
        # 1 + (2 + 3) - left associative, right child needs parens
        term = Struct("+", (Int(1), Struct("+", (Int(2), Int(3)))))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "1 + (2 + 3)"
        
    def test_right_assoc_operators(self):
        """Right-associative operators like comma."""
        # A, B, C should be A, (B, C) but prints without parens
        term = Struct(",", (Atom("a"), Struct(",", (Atom("b"), Atom("c")))))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "a, b, c"
        
        # But (A, B), C needs parens on left
        term2 = Struct(",", (Struct(",", (Atom("a"), Atom("b"))), Atom("c")))
        # When implemented:
        # assert pretty(term2, operator_mode=True) == "(a, b), c"
        
    def test_power_right_associative(self):
        """Power operator is right-associative."""
        # 2 ** 3 ** 4 should be 2 ** (3 ** 4)
        term = Struct("**", (Int(2), Struct("**", (Int(3), Int(4)))))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "2 ** 3 ** 4"
        
        # But (2 ** 3) ** 4 needs parens
        term2 = Struct("**", (Struct("**", (Int(2), Int(3))), Int(4)))
        # When implemented:
        # assert pretty(term2, operator_mode=True) == "(2 ** 3) ** 4"
        
    def test_xfx_operators_need_parens(self):
        """Non-chainable (xfx) operators always need parens when nested."""
        # X = Y = Z is illegal, must be explicit about grouping
        # (X = Y) = Z
        term = Struct("=", (Struct("=", (Var(1, "X"), Var(2, "Y"))), Var(3, "Z")))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "(X = Y) = Z"
        
    def test_mixed_control_flow_precedence(self):
        """Test precedence of control flow operators."""
        # A ; B , C should be A ; (B , C) since comma binds tighter
        term = Struct(";", (Atom("a"), Struct(",", (Atom("b"), Atom("c")))))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "a ; b, c"
        
        # (A ; B) , C needs parens
        term2 = Struct(",", (Struct(";", (Atom("a"), Atom("b"))), Atom("c")))
        # When implemented:
        # assert pretty(term2, operator_mode=True) == "(a ; b), c"
        
    def test_if_then_else_parenthesization(self):
        """Test if-then-else parenthesization."""
        # Standard if-then-else: (A -> B ; C)
        term = Struct(";", (Struct("->", (Atom("a"), Atom("b"))), Atom("c")))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "(a -> b ; c)"
        
    def test_complex_expression_parens(self):
        """Test complex expression with multiple precedence levels."""
        # -X + Y * -Z
        term = Struct("+", (
            Struct("-", (Var(1, "X"),)),
            Struct("*", (Var(2, "Y"), Struct("-", (Var(3, "Z"),))))
        ))
        # When implemented:
        # assert pretty(term, operator_mode=True) == "-X + Y * -Z"


class TestRoundTripProperty:
    """Test that parse(pretty(term)) preserves semantics."""
    
    def test_simple_operators_round_trip(self):
        """Simple operators round-trip correctly."""
        reader = Reader()
        expressions = [
            "1 + 2",
            "3 * 4", 
            "X = Y",
            "A, B",
            "C ; D",
        ]
        
        for expr in expressions:
            term = reader.read_term(expr)
            # When operator mode is implemented:
            # pretty_str = pretty(term, operator_mode=True)
            # reparsed = reader.read_term(pretty_str)
            # assert reparsed == term
            
    def test_nested_operators_round_trip(self):
        """Nested operators round-trip correctly."""
        reader = Reader()
        expressions = [
            "1 + 2 * 3",
            "(1 + 2) * 3",
            "A, B, C",
            "(A, B), C",
            "X = Y + Z",
        ]
        
        for expr in expressions:
            term = reader.read_term(expr)
            # When operator mode is implemented:
            # pretty_str = pretty(term, operator_mode=True)
            # reparsed = reader.read_term(pretty_str)
            # assert reparsed == term
            
    def test_prefix_operators_round_trip(self):
        """Prefix operators round-trip correctly."""
        reader = Reader()
        expressions = [
            "-X",
            "+Y",
            "\\+fail",
            "-3",  # Should stay as Int(-3)
            "-(X + Y)",
        ]
        
        for expr in expressions:
            term = reader.read_term(expr)
            # When operator mode is implemented:
            # pretty_str = pretty(term, operator_mode=True)
            # reparsed = reader.read_term(pretty_str)
            # assert reparsed == term
            
    def test_control_flow_round_trip(self):
        """Control flow operators round-trip correctly."""
        reader = Reader()
        expressions = [
            "(A -> B ; C)",
            "A ; B, C",
            "(A ; B), C",
            "((A -> B) ; (C -> D))",
        ]
        
        for expr in expressions:
            term = reader.read_term(expr)
            # When operator mode is implemented:
            # pretty_str = pretty(term, operator_mode=True)
            # reparsed = reader.read_term(pretty_str)
            # assert reparsed == term
            
    def test_mixed_expressions_round_trip(self):
        """Complex mixed expressions round-trip correctly."""
        reader = Reader()
        expressions = [
            "X is Y + Z * 2",
            "member(X, L), X > 0, (X mod 2 =:= 0 -> even(X) ; odd(X))",
            "-X + Y * -Z",
            "2 ** 3 ** 4",
            "(2 ** 3) ** 4",
        ]
        
        for expr in expressions:
            term = reader.read_term(expr)
            # When operator mode is implemented:
            # pretty_str = pretty(term, operator_mode=True)
            # reparsed = reader.read_term(pretty_str)
            # assert reparsed == term
            
    def test_canonical_mode_unchanged(self):
        """Canonical mode still works as before."""
        term = Struct(",", (Atom("a"), Atom("b")))
        # Canonical mode should always produce canonical form
        assert pretty(term) == "','(a, b)"
        # When implemented:
        # assert pretty(term, operator_mode=False) == "','(a, b)"
        
    def test_mode_switching(self):
        """Can switch between operator and canonical modes."""
        term = Struct("+", (Int(1), Struct("*", (Int(2), Int(3)))))
        
        # Canonical mode
        canonical = pretty(term)
        assert canonical == "'+'(1, '*'(2, 3))"
        
        # When operator mode is implemented:
        # operator = pretty(term, operator_mode=True) 
        # assert operator == "1 + 2 * 3"
        
        # Both should parse to same term
        reader = Reader()
        # term_canonical = reader.read_term(canonical)
        # term_operator = reader.read_term(operator)
        # assert term_canonical == term_operator == term


class TestEdgeCases:
    """Test edge cases and special situations."""
    
    def test_empty_operators(self):
        """Handle operators with no arguments gracefully."""
        # Zero-arity structures that happen to have operator names
        term = Struct("+", ())
        assert pretty(term) == "'+'"
        
    def test_operators_in_functors(self):
        """Operators used as functor names in structures."""
        # foo(+, -, *)
        term = Struct("foo", (Atom("+"), Atom("-"), Atom("*")))
        assert pretty(term) == "foo('+', '-', '*')"
        
    def test_quoted_operators(self):
        """Quoted operators in atom position."""
        term = Atom("+")
        assert pretty(term) == "'+'"
        
    def test_list_with_operators(self):
        """Lists containing operator structures."""
        from prolog.ast.terms import List as PrologList
        
        # [1 + 2, 3 * 4]
        term = PrologList((
            Struct("+", (Int(1), Int(2))),
            Struct("*", (Int(3), Int(4)))
        ))
        
        result = pretty(term)
        assert "[" in result and "]" in result
        # When operator mode is implemented:
        # assert pretty(term, operator_mode=True) == "[1 + 2, 3 * 4]"
        
    def test_deeply_nested_operators(self):
        """Deeply nested operator structures."""
        # ((1 + 2) * (3 + 4)) / ((5 + 6) * (7 + 8))
        term = Struct("/", (
            Struct("*", (
                Struct("+", (Int(1), Int(2))),
                Struct("+", (Int(3), Int(4)))
            )),
            Struct("*", (
                Struct("+", (Int(5), Int(6))),
                Struct("+", (Int(7), Int(8)))
            ))
        ))
        
        # Should handle deep nesting correctly
        result = pretty(term)
        assert "/" in result or "'/" in result
        
    def test_variables_in_operators(self):
        """Variables in operator positions."""
        # X + Y * Z
        term = Struct("+", (
            Var(1, "X"),
            Struct("*", (Var(2, "Y"), Var(3, "Z")))
        ))
        
        result = pretty(term)
        assert "X" in result and "Y" in result and "Z" in result


class TestOperatorModeIntegration:
    """Integration tests for operator mode with rest of system."""
    
    def test_operator_mode_with_var_names(self):
        """Operator mode respects var_names parameter."""
        var_names = {1: "MyVar", 2: "Other"}
        term = Struct("+", (Var(1), Var(2)))
        
        result = pretty(term, var_names=var_names)
        assert "MyVar" in result and "Other" in result
        
    def test_operator_mode_preserves_hints(self):
        """Variable hints are preserved in operator mode."""
        term = Struct("=", (Var(1, "Result"), Struct("+", (Int(1), Int(2)))))
        
        result = pretty(term)
        assert "Result" in result
        
    def test_anonymous_vars_in_operators(self):
        """Anonymous variables work in operator expressions."""
        term = Struct("=", (Var(1, "_"), Int(42)))
        
        result = pretty(term)
        assert "_" in result