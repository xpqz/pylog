"""Tests for the Prolog reader module with Pratt parser - Issue #37.

The reader module implements a Pratt parser that transforms operator 
expressions to canonical AST forms using the operator table for 
precedence and associativity.

Test Coverage:
- Operator precedence handling
- Associativity (left/right/non-associative)
- xfx non-chainable enforcement
- Parenthesis override of precedence
- Unary operators
- Negative numeral policy
- Error reporting with positions
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.parser.reader import Reader, ReaderError


class TestPrattParser:
    """Test the Pratt parser implementation."""

    def test_higher_precedence_binds_tighter(self):
        """Higher precedence operators bind tighter: 1 + 2 * 3 → +(1, *(2, 3))."""
        reader = Reader()
        # * has precedence 400, + has precedence 500 (lower number = higher precedence)
        result = reader.read_term("1 + 2 * 3")
        expected = Struct("+", (Int(1), Struct("*", (Int(2), Int(3)))))
        assert result == expected

    def test_right_associative_conjunction(self):
        """Right associative operators: A , B , C → ,(A, ,(B, C))."""
        reader = Reader()
        # , is xfy (right-associative)
        result = reader.read_term("a , b , c")
        expected = Struct(",", (Atom("a"), Struct(",", (Atom("b"), Atom("c")))))
        assert result == expected

    def test_left_associative_subtraction(self):
        """Left associative operators: 1 - 2 - 3 → -(-(1, 2), 3)."""
        reader = Reader()
        # - is yfx (left-associative)
        result = reader.read_term("1 - 2 - 3")
        expected = Struct("-", (Struct("-", (Int(1), Int(2))), Int(3)))
        assert result == expected

    def test_xfx_non_chainable(self):
        """xfx operators cannot chain: X = Y = Z is a syntax error."""
        reader = Reader()
        # = is xfx (non-associative)
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        assert "non-chainable" in str(exc_info.value).lower()
        assert "=" in str(exc_info.value)

    def test_parentheses_override_precedence(self):
        """Parentheses override precedence: (1 + 2) * 3 → *(+(1, 2), 3)."""
        reader = Reader()
        result = reader.read_term("(1 + 2) * 3")
        expected = Struct("*", (Struct("+", (Int(1), Int(2))), Int(3)))
        assert result == expected

    def test_unary_minus(self):
        """Unary minus operator: -X → -(X)."""
        reader = Reader()
        result = reader.read_term("-X")
        # X is variable with ID 0 and hint "X"
        expected = Struct("-", (Var(0, "X"),))
        assert result == expected

    def test_unary_plus(self):
        """Unary plus operator: +X → +(X)."""
        reader = Reader()
        result = reader.read_term("+X")
        expected = Struct("+", (Var(0, "X"),))
        assert result == expected

    def test_double_unary_minus(self):
        """Double unary minus: - - X → -(-(X))."""
        reader = Reader()
        result = reader.read_term("- - X")
        expected = Struct("-", (Struct("-", (Var(0, "X"),)),))
        assert result == expected

    def test_negative_literal(self):
        """Negative literals: -3 → Int(-3) (not -(3))."""
        reader = Reader()
        result = reader.read_term("-3")
        # Negative literal policy: -3 is Int(-3), not Struct("-", (Int(3),))
        expected = Int(-3)
        assert result == expected

    def test_subtraction_with_negative(self):
        """Subtraction with negative: 1 - -1 → -(1, -1)."""
        reader = Reader()
        result = reader.read_term("1 - -1")
        expected = Struct("-", (Int(1), Int(-1)))
        assert result == expected

    def test_negation_as_failure(self):
        """Negation as failure operator: \\+p → \\+(p)."""
        reader = Reader()
        result = reader.read_term("\\+p")
        expected = Struct("\\+", (Atom("p"),))
        assert result == expected

    def test_if_then_else(self):
        """If-then-else: p -> q ; r → ;(->(p, q), r)."""
        reader = Reader()
        result = reader.read_term("p -> q ; r")
        expected = Struct(";", (
            Struct("->", (Atom("p"), Atom("q"))),
            Atom("r")
        ))
        assert result == expected

    def test_complex_expression(self):
        """Complex expression with multiple operators."""
        reader = Reader()
        result = reader.read_term("X = Y + Z * W")
        # = has precedence 700, + has 500, * has 400
        # So: X = (Y + (Z * W))
        expected = Struct("=", (
            Var(0, "X"),
            Struct("+", (
                Var(1, "Y"),
                Struct("*", (Var(2, "Z"), Var(3, "W")))
            ))
        ))
        assert result == expected

    def test_arithmetic_evaluation(self):
        """Arithmetic evaluation operator: X is 2 + 3."""
        reader = Reader()
        result = reader.read_term("X is 2 + 3")
        expected = Struct("is", (
            Var(0, "X"),
            Struct("+", (Int(2), Int(3)))
        ))
        assert result == expected

    def test_comparison_operators(self):
        """Various comparison operators."""
        reader = Reader()
        
        # Less than
        result = reader.read_term("X < Y")
        assert result == Struct("<", (Var(0, "X"), Var(1, "Y")))
        
        # Less or equal
        result = reader.read_term("X =< Y")
        assert result == Struct("=<", (Var(0, "X"), Var(1, "Y")))
        
        # Structural equality
        result = reader.read_term("X == Y")
        assert result == Struct("==", (Var(0, "X"), Var(1, "Y")))
        
        # Arithmetic equality
        result = reader.read_term("X =:= Y")
        assert result == Struct("=:=", (Var(0, "X"), Var(1, "Y")))

    def test_power_right_associative(self):
        """Power operator is right-associative: 2 ** 3 ** 4 → **(2, **(3, 4))."""
        reader = Reader()
        result = reader.read_term("2 ** 3 ** 4")
        expected = Struct("**", (Int(2), Struct("**", (Int(3), Int(4)))))
        assert result == expected

    def test_mod_operator(self):
        """Modulo operator: X mod Y."""
        reader = Reader()
        result = reader.read_term("X mod Y")
        expected = Struct("mod", (Var(0, "X"), Var(1, "Y")))
        assert result == expected

    def test_structure_with_operators(self):
        """Structures can contain operator expressions."""
        reader = Reader()
        result = reader.read_term("f(X + Y, Z * W)")
        expected = Struct("f", (
            Struct("+", (Var(0, "X"), Var(1, "Y"))),
            Struct("*", (Var(2, "Z"), Var(3, "W")))
        ))
        assert result == expected

    def test_list_with_operators(self):
        """Lists can contain operator expressions."""
        reader = Reader()
        result = reader.read_term("[X + Y, Z * W]")
        expected = List((
            Struct("+", (Var(0, "X"), Var(1, "Y"))),
            Struct("*", (Var(2, "Z"), Var(3, "W")))
        ), Atom("[]"))
        assert result == expected

    def test_operator_as_atom(self):
        """Operators can be used as atoms when quoted."""
        reader = Reader()
        result = reader.read_term("'+'")
        expected = Atom("+")
        assert result == expected

    def test_operator_as_functor(self):
        """Operators can be functors in canonical form."""
        reader = Reader()
        result = reader.read_term("'+'(1, 2)")
        expected = Struct("+", (Int(1), Int(2)))
        assert result == expected

    def test_mixed_prefix_infix(self):
        """Mixed prefix and infix operators."""
        reader = Reader()
        result = reader.read_term("-X + Y")
        # -X has precedence 200 (unary), + has 500
        # So: +(-(X), Y)
        expected = Struct("+", (Struct("-", (Var(0, "X"),)), Var(1, "Y")))
        assert result == expected

    def test_error_position_tracking(self):
        """Errors should include character positions pointing to the offending token."""
        reader = Reader()
        src = "X = Y = Z"
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(src)
        error = exc_info.value
        
        # Error should have position information
        assert hasattr(error, 'position') or hasattr(error, 'column')
        
        # Should identify the problematic operator
        assert "=" in str(error)
        
        # If position is available, it should point to the second =
        # Positions: X(0) ' '(1) =(2) ' '(3) Y(4) ' '(5) =(6)
        if hasattr(error, 'position'):
            assert error.position >= 6  # Should be at or after the second =
        elif hasattr(error, 'column'):
            # 1-based column counting
            assert error.column >= 7

    def test_unknown_operator(self):
        """Unknown operators should raise an error."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X @@ Y")  # @@ is not defined
        assert "unknown operator" in str(exc_info.value).lower() or "unexpected" in str(exc_info.value).lower()

    def test_clause_with_operators(self):
        """Parse clauses with operator expressions."""
        reader = Reader()
        result = reader.read_clause("max(X, Y, Z) :- X > Y, Z = X ; Z = Y.")
        # Expected: max(X, Y, Z) :- (X > Y, Z = X) ; Z = Y
        # The conjunction , binds tighter than disjunction ;
        expected_head = Struct("max", (Var(0, "X"), Var(1, "Y"), Var(2, "Z")))
        expected_body = Struct(";", (
            Struct(",", (
                Struct(">", (Var(0, "X"), Var(1, "Y"))),
                Struct("=", (Var(2, "Z"), Var(0, "X")))
            )),
            Struct("=", (Var(2, "Z"), Var(1, "Y")))
        ))
        assert result.head == expected_head
        assert result.body == expected_body

    def test_query_with_operators(self):
        """Parse queries with operator expressions."""
        reader = Reader()
        result = reader.read_query("?- X = 1 + 2, Y is X * 3.")
        # Two goals: X = 1 + 2 and Y is X * 3
        assert len(result) == 2
        assert result[0] == Struct("=", (
            Var(0, "X"),
            Struct("+", (Int(1), Int(2)))
        ))
        assert result[1] == Struct("is", (
            Var(1, "Y"),
            Struct("*", (Var(0, "X"), Int(3)))
        ))

    def test_unsupported_operator_warning(self, caplog):
        """Unsupported operators should parse but warn in dev mode."""
        import logging
        reader = Reader()
        # //, mod, ** are marked as unsupported in Stage 1
        with caplog.at_level(logging.WARNING):
            result = reader.read_term("X // Y")
        expected = Struct("//", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
        # Should have logged a warning about unsupported operator
        assert any("unsupported" in msg.lower() or "//" in msg for msg in caplog.messages)

    def test_variable_consistency(self):
        """Variables with same name should have same ID."""
        reader = Reader()
        result = reader.read_term("X = Y, Y = X")
        # X should have consistent ID, Y should have consistent ID
        conj = result  # This is the top-level conjunction
        assert isinstance(conj, Struct)
        assert conj.functor == ","
        left = conj.args[0]  # X = Y
        right = conj.args[1]  # Y = X
        
        # Extract variable IDs (these should be Structs with Var args)
        assert isinstance(left, Struct) and left.functor == "="
        assert isinstance(right, Struct) and right.functor == "="
        
        x_id_1 = left.args[0].id  # X in first equation
        y_id_1 = left.args[1].id  # Y in first equation
        y_id_2 = right.args[0].id  # Y in second equation
        x_id_2 = right.args[1].id  # X in second equation
        
        assert x_id_1 == x_id_2  # Same X
        assert y_id_1 == y_id_2  # Same Y

    def test_anonymous_variables_unique(self):
        """Each anonymous variable _ should get a unique ID."""
        reader = Reader()
        result = reader.read_term("f(_, _, _)")
        # Each _ should have a different ID
        assert isinstance(result, Struct)
        args = result.args
        ids = [arg.id for arg in args]
        assert len(ids) == len(set(ids))  # All unique
    
    def test_mixed_anonymous_and_named_vars(self):
        """Anonymous vars get unique IDs while named vars stay stable."""
        reader = Reader()
        result = reader.read_term("f(_, X, _, X, _)")
        assert isinstance(result, Struct)
        args = result.args
        # Check _ vars are unique
        anon_ids = [args[0].id, args[2].id, args[4].id]
        assert len(anon_ids) == len(set(anon_ids))  # All unique
        # Check X vars are same
        assert args[1].id == args[3].id

    def test_prefix_disambiguation(self):
        """Prefix operators should be disambiguated from infix."""
        reader = Reader()
        # This should parse as +(-(1)) not as partial infix
        result = reader.read_term("+ -1")
        expected = Struct("+", (Int(-1),))
        assert result == expected
    
    def test_then_right_associative(self):
        """Arrow operator chains right-associatively."""
        reader = Reader()
        result = reader.read_term("a -> b -> c")
        expected = Struct("->", (Atom("a"), Struct("->", (Atom("b"), Atom("c")))))
        assert result == expected
    
    def test_or_right_associative(self):
        """Disjunction chains right-associatively."""
        reader = Reader()
        result = reader.read_term("a ; b ; c")
        expected = Struct(";", (Atom("a"), Struct(";", (Atom("b"), Atom("c")))))
        assert result == expected
    
    def test_and_binds_tighter_than_or(self):
        """Conjunction binds tighter than disjunction."""
        reader = Reader()
        # a, b ; c, d → ;(,(a, b), ,(c, d))
        result = reader.read_term("a, b ; c, d")
        expected = Struct(";", (
            Struct(",", (Atom("a"), Atom("b"))),
            Struct(",", (Atom("c"), Atom("d")))
        ))
        assert result == expected
        
        # (a ; b), c → ,( ;(a, b), c)
        result = reader.read_term("(a ; b), c")
        expected = Struct(",", (Struct(";", (Atom("a"), Atom("b"))), Atom("c")))
        assert result == expected
    
    def test_unary_minus_vs_power_tie_break(self):
        """Unary minus vs power precedence tie-breaking.
        
        At precedence 200, unary minus and power have same precedence.
        De-facto Prolog parses -2 ** 3 as -(2 ** 3).
        """
        reader = Reader()
        # -2 ** 3 → -(2 ** 3)
        result = reader.read_term("-2 ** 3")
        expected = Struct("-", (Struct("**", (Int(2), Int(3))),))
        assert result == expected
        
        # (-2) ** 3 → **(-2, 3)
        result = reader.read_term("(-2) ** 3")
        expected = Struct("**", (Int(-2), Int(3)))
        assert result == expected
    
    def test_positive_literal(self):
        """Positive literal policy: +3 → Int(3)."""
        reader = Reader()
        result = reader.read_term("+3")
        # Following SWI convention: +3 is Int(3), not Struct("+", (Int(3),))
        expected = Int(3)
        assert result == expected
    
    @pytest.mark.parametrize("src", [
        "X =:= Y =:= Z",
        "X =< Y =< Z", 
        "X == Y == Z",
        "X \\= Y \\= Z"
    ])
    def test_xfx_non_chainable_family(self, src):
        """All xfx operators should reject chaining."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(src)
        assert "non-chainable" in str(exc_info.value).lower()
    
    def test_term_order_operators(self):
        """Term order comparison operators."""
        reader = Reader()
        result = reader.read_term("X @=< Y")
        expected = Struct("@=<", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
        
        result = reader.read_term("X @> Y")
        expected = Struct("@>", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
    
    @pytest.mark.parametrize("spaced,tight", [
        ("X = Y + Z * W", "X=Y+Z*W"),
        ("A ; B , C", "A;B,C"),
        ("-2 ** 3", "-2**3"),
    ])
    def test_whitespace_insensitivity(self, spaced, tight):
        """Whitespace should not affect parsing."""
        reader = Reader()
        assert reader.read_term(spaced) == reader.read_term(tight)
    
    def test_list_tail_with_operator_expression(self):
        """List tail can be an operator expression."""
        reader = Reader()
        result = reader.read_term("[H | T + U]")
        # [H|T+U] → [H | +(T, U)]
        expected = List((Var(0, "H"),), Struct("+", (Var(1, "T"), Var(2, "U"))))
        assert result == expected
    
    def test_parenthesized_prefix(self):
        """Parenthesized prefix operator."""
        reader = Reader()
        result = reader.read_term("(-X)")
        expected = Struct("-", (Var(0, "X"),))
        assert result == expected
    
    def test_word_operator_boundary(self):
        """Word operators need token boundaries."""
        reader = Reader()
        # 'mod' is a word operator, needs spaces
        result = reader.read_term("X mod Y")
        expected = Struct("mod", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
        
        # Without spaces, XmodY is just an atom
        result = reader.read_term("XmodY")
        assert result == Atom("XmodY") or isinstance(result, Var)  # Might be seen as variable