"""Tests for arithmetic operators parsing - Issue #40.

Tests cover binary arithmetic operators (+, -, *, /, //, mod, **) and
unary prefix operators (+, -) with correct precedence and associativity.

Test requirements from issue:
- Binary: +, -, *, / (all supported in Stage 1)
- Binary: //, mod, ** (parse ok, runtime unsupported)
- Unary: - (prefix), + (prefix)
- Multiplication/division at precedence 400, yfx (left-assoc)
- Addition/subtraction at precedence 500, yfx (left-assoc)
- Exponentiation at precedence 200, xfy (right-assoc)
- Unary +/- at precedence 200, fy (prefix)
- Negative literal policy consistency
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.parser.reader import Reader, ReaderError


class TestArithmeticOperatorsParsing:
    """Test arithmetic operator transformations."""
    
    def test_multiplication_precedence_400(self):
        """Multiplication has precedence 400."""
        reader = Reader()
        result = reader.read_term("X * Y")
        expected = Struct("*", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_division_precedence_400(self):
        """Division has precedence 400."""
        reader = Reader()
        result = reader.read_term("X / Y")
        expected = Struct("/", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_addition_precedence_500(self):
        """Addition has precedence 500."""
        reader = Reader()
        result = reader.read_term("X + Y")
        expected = Struct("+", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_subtraction_precedence_500(self):
        """Subtraction has precedence 500."""
        reader = Reader()
        result = reader.read_term("X - Y")
        expected = Struct("-", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_multiplication_binds_tighter_than_addition(self):
        """Multiplication binds tighter than addition: 1 + 2 * 3 → +(1, *(2, 3))."""
        reader = Reader()
        result = reader.read_term("1 + 2 * 3")
        expected = Struct("+", (Int(1), Struct("*", (Int(2), Int(3)))))
        assert result == expected
    
    def test_division_binds_tighter_than_subtraction(self):
        """Division binds tighter than subtraction: 10 - 6 / 2 → -(10, /(6, 2))."""
        reader = Reader()
        result = reader.read_term("10 - 6 / 2")
        expected = Struct("-", (Int(10), Struct("/", (Int(6), Int(2)))))
        assert result == expected
    
    def test_multiplication_left_associative(self):
        """Multiplication is left-associative: 2 * 3 * 4 → *(*(2, 3), 4)."""
        reader = Reader()
        result = reader.read_term("2 * 3 * 4")
        expected = Struct("*", (Struct("*", (Int(2), Int(3))), Int(4)))
        assert result == expected
    
    def test_division_left_associative(self):
        """Division is left-associative: 12 / 3 / 2 → /(/(12, 3), 2)."""
        reader = Reader()
        result = reader.read_term("12 / 3 / 2")
        expected = Struct("/", (Struct("/", (Int(12), Int(3))), Int(2)))
        assert result == expected
    
    def test_addition_left_associative(self):
        """Addition is left-associative: 1 + 2 + 3 → +(+(1, 2), 3)."""
        reader = Reader()
        result = reader.read_term("1 + 2 + 3")
        expected = Struct("+", (Struct("+", (Int(1), Int(2))), Int(3)))
        assert result == expected
    
    def test_subtraction_left_associative(self):
        """Subtraction is left-associative: 10 - 3 - 2 → -(-(10, 3), 2)."""
        reader = Reader()
        result = reader.read_term("10 - 3 - 2")
        expected = Struct("-", (Struct("-", (Int(10), Int(3))), Int(2)))
        assert result == expected
    
    def test_exponentiation_precedence_200(self):
        """Exponentiation has precedence 200."""
        reader = Reader()
        result = reader.read_term("X ** Y")
        expected = Struct("**", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_exponentiation_right_associative(self):
        """Exponentiation is right-associative: 2 ** 3 ** 4 → **(2, **(3, 4))."""
        reader = Reader()
        result = reader.read_term("2 ** 3 ** 4")
        expected = Struct("**", (Int(2), Struct("**", (Int(3), Int(4)))))
        assert result == expected
    
    def test_power_binds_tighter_than_multiplication(self):
        """Power binds tighter than multiplication: 2 * 3 ** 4 → *(2, **(3, 4))."""
        reader = Reader()
        result = reader.read_term("2 * 3 ** 4")
        expected = Struct("*", (Int(2), Struct("**", (Int(3), Int(4)))))
        assert result == expected
    
    def test_unary_minus_precedence_200(self):
        """Unary minus has precedence 200."""
        reader = Reader()
        result = reader.read_term("-X")
        expected = Struct("-", (Var(0, "X"),))
        assert result == expected
    
    def test_unary_plus_precedence_200(self):
        """Unary plus has precedence 200."""
        reader = Reader()
        result = reader.read_term("+X")
        expected = Struct("+", (Var(0, "X"),))
        assert result == expected
    
    def test_unary_minus_binds_tighter_than_multiplication(self):
        """Unary minus binds tighter than multiplication: -X * Y → *(-(X), Y)."""
        reader = Reader()
        result = reader.read_term("-X * Y")
        expected = Struct("*", (Struct("-", (Var(0, "X"),)), Var(1, "Y")))
        assert result == expected
    
    def test_unary_plus_binds_tighter_than_addition(self):
        """Unary plus binds tighter than addition: +X + Y → +(+(X), Y)."""
        reader = Reader()
        result = reader.read_term("+X + Y")
        expected = Struct("+", (Struct("+", (Var(0, "X"),)), Var(1, "Y")))
        assert result == expected
    
    def test_negative_literal_policy(self):
        """Negative literals: -3 → Int(-3) (not Struct('-', (Int(3),)))."""
        reader = Reader()
        result = reader.read_term("-3")
        expected = Int(-3)
        assert result == expected
    
    def test_positive_literal_policy(self):
        """Positive literals: +3 → Int(3) (not Struct('+', (Int(3),)))."""
        reader = Reader()
        result = reader.read_term("+3")
        expected = Int(3)
        assert result == expected
    
    def test_subtraction_with_negative_literal(self):
        """1 - -1 → -(1, -1)."""
        reader = Reader()
        result = reader.read_term("1 - -1")
        expected = Struct("-", (Int(1), Int(-1)))
        assert result == expected
    
    def test_addition_with_positive_literal(self):
        """1 + +1 → +(1, 1)."""
        reader = Reader()
        result = reader.read_term("1 + +1")
        expected = Struct("+", (Int(1), Int(1)))
        assert result == expected
    
    def test_double_unary_minus(self):
        """- - X → -(-(X))."""
        reader = Reader()
        result = reader.read_term("- - X")
        expected = Struct("-", (Struct("-", (Var(0, "X"),)),))
        assert result == expected
    
    def test_double_unary_plus(self):
        """+ + X → +(+(X))."""
        reader = Reader()
        result = reader.read_term("+ + X")
        expected = Struct("+", (Struct("+", (Var(0, "X"),)),))
        assert result == expected
    
    def test_integer_division_operator(self):
        """Integer division operator: X // Y."""
        reader = Reader()
        result = reader.read_term("X // Y")
        expected = Struct("//", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_integer_division_precedence_400(self):
        """Integer division has precedence 400, same as multiplication."""
        reader = Reader()
        result = reader.read_term("1 + 2 // 3")
        expected = Struct("+", (Int(1), Struct("//", (Int(2), Int(3)))))
        assert result == expected
    
    def test_integer_division_left_associative(self):
        """Integer division is left-associative: 12 // 3 // 2 → //(//(12, 3), 2)."""
        reader = Reader()
        result = reader.read_term("12 // 3 // 2")
        expected = Struct("//", (Struct("//", (Int(12), Int(3))), Int(2)))
        assert result == expected
    
    def test_mod_operator(self):
        """Modulo operator: X mod Y."""
        reader = Reader()
        result = reader.read_term("X mod Y")
        expected = Struct("mod", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    def test_mod_precedence_400(self):
        """Mod has precedence 400, same as multiplication."""
        reader = Reader()
        result = reader.read_term("1 + 2 mod 3")
        expected = Struct("+", (Int(1), Struct("mod", (Int(2), Int(3)))))
        assert result == expected
    
    def test_mod_left_associative(self):
        """Mod is left-associative: 10 mod 3 mod 2 → mod(mod(10, 3), 2)."""
        reader = Reader()
        result = reader.read_term("10 mod 3 mod 2")
        expected = Struct("mod", (Struct("mod", (Int(10), Int(3))), Int(2)))
        assert result == expected
    
    def test_parentheses_override_precedence(self):
        """Parentheses override precedence: (1 + 2) * 3 → *(+(1, 2), 3)."""
        reader = Reader()
        result = reader.read_term("(1 + 2) * 3")
        expected = Struct("*", (Struct("+", (Int(1), Int(2))), Int(3)))
        assert result == expected
    
    def test_complex_arithmetic_expression(self):
        """Complex expression: 1 + 2 * 3 - 4 / 2 → -(+(1, *(2, 3)), /(4, 2))."""
        reader = Reader()
        result = reader.read_term("1 + 2 * 3 - 4 / 2")
        # ((1 + (2 * 3)) - (4 / 2))
        expected = Struct("-", (
            Struct("+", (Int(1), Struct("*", (Int(2), Int(3))))),
            Struct("/", (Int(4), Int(2)))
        ))
        assert result == expected
    
    def test_mixed_precedence_with_power(self):
        """Complex expression with power: 2 ** 3 * 4 + 5."""
        reader = Reader()
        result = reader.read_term("2 ** 3 * 4 + 5")
        # (((2 ** 3) * 4) + 5)
        expected = Struct("+", (
            Struct("*", (Struct("**", (Int(2), Int(3))), Int(4))),
            Int(5)
        ))
        assert result == expected
    
    def test_unary_minus_vs_power_precedence(self):
        """-2 ** 3 → -(2 ** 3) due to tie-breaking."""
        reader = Reader()
        result = reader.read_term("-2 ** 3")
        expected = Struct("-", (Struct("**", (Int(2), Int(3))),))
        assert result == expected
    
    def test_parenthesized_unary_minus_with_power(self):
        """(-2) ** 3 → **(-2, 3)."""
        reader = Reader()
        result = reader.read_term("(-2) ** 3")
        expected = Struct("**", (Int(-2), Int(3)))
        assert result == expected
    
    def test_arithmetic_in_structures(self):
        """Arithmetic expressions in structure arguments."""
        reader = Reader()
        result = reader.read_term("f(X + Y, Z * W)")
        expected = Struct("f", (
            Struct("+", (Var(0, "X"), Var(1, "Y"))),
            Struct("*", (Var(2, "Z"), Var(3, "W")))
        ))
        assert result == expected
    
    def test_arithmetic_in_lists(self):
        """Arithmetic expressions in lists."""
        reader = Reader()
        result = reader.read_term("[1 + 2, 3 * 4, 5 - 6]")
        expected = List((
            Struct("+", (Int(1), Int(2))),
            Struct("*", (Int(3), Int(4))),
            Struct("-", (Int(5), Int(6)))
        ), Atom("[]"))
        assert result == expected
    
    def test_arithmetic_with_is_operator(self):
        """Arithmetic with 'is' evaluation operator."""
        reader = Reader()
        result = reader.read_term("X is 2 + 3 * 4")
        expected = Struct("is", (
            Var(0, "X"),
            Struct("+", (Int(2), Struct("*", (Int(3), Int(4)))))
        ))
        assert result == expected
    
    def test_unsupported_operators_warning(self, caplog):
        """Unsupported operators should parse but warn in dev mode."""
        import logging
        reader = Reader()
        
        # // is unsupported in Stage 1
        with caplog.at_level(logging.WARNING):
            result = reader.read_term("X // Y")
        assert result == Struct("//", (Var(0, "X"), Var(1, "Y")))
        assert any("//" in msg for msg in caplog.messages)
        
        caplog.clear()
        
        # mod is unsupported in Stage 1
        with caplog.at_level(logging.WARNING):
            result = reader.read_term("X mod Y")
        assert result == Struct("mod", (Var(0, "X"), Var(1, "Y")))
        assert any("mod" in msg for msg in caplog.messages)
        
        caplog.clear()
        
        # ** is unsupported in Stage 1
        with caplog.at_level(logging.WARNING):
            result = reader.read_term("X ** Y")
        assert result == Struct("**", (Var(0, "X"), Var(1, "Y")))
        assert any("**" in msg for msg in caplog.messages)
    
    def test_strict_mode_unsupported_operators(self):
        """Strict mode should raise errors for unsupported operators."""
        reader = Reader(strict_unsupported=True)
        
        # // is unsupported
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X // Y")
        assert "//" in str(exc_info.value)
        
        # mod is unsupported
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X mod Y")
        assert "mod" in str(exc_info.value)
        
        # ** is unsupported
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X ** Y")
        assert "**" in str(exc_info.value)
    
    def test_whitespace_insensitivity(self):
        """Whitespace should not affect parsing."""
        reader = Reader()
        spaced = "X + Y * Z - W / V"
        tight = "X+Y*Z-W/V"
        assert reader.read_term(spaced) == reader.read_term(tight)
    
    def test_mod_requires_spaces(self):
        """mod operator requires token boundaries."""
        reader = Reader()
        
        # With spaces: mod operator
        result = reader.read_term("X mod Y")
        assert result == Struct("mod", (Var(0, "X"), Var(1, "Y")))
        
        # Without spaces: variable name
        result = reader.read_term("XmodY")
        assert isinstance(result, Var) and result.hint == "XmodY"
    
    def test_mixed_arithmetic_and_comparison(self):
        """Arithmetic mixed with comparison operators."""
        reader = Reader()
        result = reader.read_term("X + Y =< Z * W")
        # =< has precedence 700, so binds last
        expected = Struct("=<", (
            Struct("+", (Var(0, "X"), Var(1, "Y"))),
            Struct("*", (Var(2, "Z"), Var(3, "W")))
        ))
        assert result == expected
    
    def test_arithmetic_with_conjunction(self):
        """Arithmetic in conjunction context."""
        reader = Reader()
        result = reader.read_term("X is Y + Z, W is X * 2")
        # , has precedence 1000, binds last
        expected = Struct(",", (
            Struct("is", (Var(0, "X"), Struct("+", (Var(1, "Y"), Var(2, "Z"))))),
            Struct("is", (Var(3, "W"), Struct("*", (Var(0, "X"), Int(2)))))
        ))
        assert result == expected
    
    # Edge case additions
    
    def test_tight_pow_and_idiv(self):
        """Tight (no-space) forms for multi-char operators."""
        reader = Reader()
        assert reader.read_term("X**Y") == Struct("**", (Var(0, "X"), Var(1, "Y")))
        assert reader.read_term("X//Y") == Struct("//", (Var(0, "X"), Var(1, "Y")))
        # XmodY is a variable, not an operator (mod requires spaces)
        result = reader.read_term("XmodY")
        assert isinstance(result, Var) and result.hint == "XmodY"
    
    def test_unary_minus_with_division(self):
        """Unary minus around division operations."""
        reader = Reader()
        assert reader.read_term("-X / Y") == Struct("/", (Struct("-", (Var(0, "X"),)), Var(1, "Y")))
        assert reader.read_term("1 / -2") == Struct("/", (Int(1), Int(-2)))
    
    def test_unary_minus_power_then_mul_chain(self):
        """Unary minus with power then multiplication chain."""
        reader = Reader()
        # -2 ** 3 * 4  ==> (-(2 ** 3)) * 4
        assert reader.read_term("-2 ** 3 * 4") == Struct("*", (
            Struct("-", (Struct("**", (Int(2), Int(3))),)),
            Int(4)
        ))
    
    def test_parenthesized_unary_minus_simple(self):
        """Parenthesized unary minus (non-power case)."""
        reader = Reader()
        assert reader.read_term("(-X)") == Struct("-", (Var(0, "X"),))
    
    def test_addition_with_negative_literal(self):
        """Addition with negative literal."""
        reader = Reader()
        assert reader.read_term("1 + -2") == Struct("+", (Int(1), Int(-2)))
    
    def test_mod_word_boundary_on_one_side(self):
        """mod word-boundary edge cases."""
        reader = Reader()
        # No space before 'mod' => variable 'Xmod'
        result = reader.read_term("Xmod Y")
        assert isinstance(result, Var) and result.hint == "Xmod"
        
        # No space after 'mod' => not a mod operator
        result = reader.read_term("X modY")
        assert not (isinstance(result, Struct) and result.functor == "mod")
    
    def test_power_three_right_assoc(self):
        """Deeper right-associative power chain."""
        reader = Reader()
        assert reader.read_term("2 ** 3 ** 2 ** 1") == Struct("**", (
            Int(2),
            Struct("**", (Int(3), Struct("**", (Int(2), Int(1)))))
        ))
    
    @pytest.mark.parametrize("spaced,tight", [
        ("X ** Y + Z", "X**Y+Z"),
        ("X // Y - Z", "X//Y-Z"),
        ("A mod B * C", "A mod B*C"),  # still needs a space around 'mod'
    ])
    def test_whitespace_insensitivity_multi_ops(self, spaced, tight):
        """Whitespace-insensitivity for multi-char operators."""
        reader = Reader()
        assert reader.read_term(spaced) == reader.read_term(tight)
    
    def test_is_with_unary_and_power(self):
        """is/2 with unary and power operators."""
        reader = Reader()
        assert reader.read_term("X is -2 ** 3") == Struct("is", (
            Var(0, "X"),
            Struct("-", (Struct("**", (Int(2), Int(3))),))
        ))
    
    def test_parentheses_nested_override(self):
        """Parentheses override with nested arithmetic."""
        reader = Reader()
        assert reader.read_term("(1 + (2 * 3)) - (4 / (1 + 1))") == Struct("-", (
            Struct("+", (Int(1), Struct("*", (Int(2), Int(3))))),
            Struct("/", (Int(4), Struct("+", (Int(1), Int(1)))))
        ))