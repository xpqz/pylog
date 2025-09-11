"""Tests for span-preserving error handling in the reader - Issue #42.

This test module validates that the reader properly preserves character positions
through transformations and provides helpful error messages with precise locations.

Test Coverage:
- Token position preservation through Pratt parser
- Error messages with character positions (0-based)
- Fix suggestions for common errors
- xfx chaining errors with positions
- Unmatched parentheses with locations
- Missing arguments with column numbers
- Greedy tokenization edge cases
- Multi-line spans and Unicode handling
- Tab character position handling
"""

import pytest
from prolog.parser.reader import Reader, ReaderError
from prolog.ast.terms import Atom, Int, Var, Struct


class TestSpanPreservation:
    """Test that token positions are preserved through reader transformations."""
    
    def test_error_includes_position(self):
        """ReaderError should include character position."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        
        error = exc_info.value
        # Error should have position attribute
        assert hasattr(error, 'position')
        assert error.position is not None
        # Position should point to second '=' (at position 6, 0-based)
        assert error.position == 6
        
    def test_error_includes_token(self):
        """ReaderError should include the problematic token."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        
        error = exc_info.value
        assert hasattr(error, 'token')
        assert error.token == "="
        
    def test_error_message_format(self):
        """Error messages should include position in standard format."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        
        error = exc_info.value
        error_str = str(error)
        # Should include "at position N" in the message
        assert "at position 6" in error_str
        
    def test_unmatched_paren_position(self):
        """Unmatched parenthesis error should show exact position."""
        reader = Reader()
        text = "(a, b"
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(text)
        
        error = exc_info.value
        # Position should be where we expected closing paren (end of input)
        assert error.position == len(text)  # 5
        assert "unmatched" in str(error).lower() or "expected" in str(error).lower()
        
    def test_extra_closing_paren_position(self):
        """Extra closing parenthesis should show its position."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("a, b)")
        
        error = exc_info.value
        # Position should point to the unexpected ')'
        assert error.position == 4  # Position of ')'
        assert error.token == ")"
        
    def test_missing_argument_error(self):
        """Missing argument should show position of problematic operator."""
        reader = Reader()
        text = "a, "
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(text)
        
        error = exc_info.value
        # Should indicate missing right argument for ','
        assert "missing" in str(error).lower() or "expected" in str(error).lower()
        assert error.token == ","
        assert error.position == text.index(",")  # 1
        
    def test_unknown_operator_position(self):
        """Unknown operator should show its position."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            # Using @@ which is not a defined operator
            reader.read_term("X @@ Y")
        
        error = exc_info.value
        assert error.position == 2  # Position of @@
        assert error.token == "@@"
        assert "unknown" in str(error).lower() or "undefined" in str(error).lower()


class TestXfxChainingErrors:
    """Test error reporting for xfx operator chaining."""
    
    def test_xfx_equals_chaining_error(self):
        """Chaining = operator should give helpful error."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        
        error = exc_info.value
        error_str = str(error)
        # Should mention that = is non-chainable
        assert "non-chainable" in error_str.lower()
        assert "=" in error_str
        # Should suggest parentheses
        assert "parenthes" in error_str.lower()
        assert error.position == 6  # Position of second '='
        
    def test_xfx_comparison_chaining_error(self):
        """Chaining comparison operators should give helpful error."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X < Y < Z")
        
        error = exc_info.value
        error_str = str(error)
        assert "non-chainable" in error_str.lower()
        assert "<" in error_str
        assert error.position == 6  # Position of second '<'
        
    def test_mixed_xfx_chaining_error(self):
        """Mixing xfx operators should give clear error."""
        reader = Reader()
        text = "X = Y < Z"
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(text)
        
        error = exc_info.value
        # Either operator could be reported as problematic
        assert error.token in ["=", "<"]
        # If = is reported, position 2; if < is reported, position 6
        if error.token == "=":
            assert error.position == 2
        else:
            assert error.position == 6


class TestFixSuggestions:
    """Test that error messages include actionable fix suggestions."""
    
    def test_xfx_chaining_suggests_parentheses(self):
        """xfx chaining error should suggest adding parentheses."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X = Y = Z")
        
        error_str = str(exc_info.value)
        # Should suggest parentheses as a fix
        assert "parenthes" in error_str.lower()
        
    def test_missing_space_suggestion(self):
        """Ambiguous operator sequences should suggest spacing."""
        reader = Reader()
        # 1+-2 could be interpreted as 1 + (-2) or a parsing error
        # The reader should handle this gracefully
        result = reader.read_term("1+-2")
        # Should parse as 1 + (-2)
        expected = Struct("+", (Int(1), Int(-2)))
        assert result == expected
        
    def test_unmatched_paren_suggestion(self):
        """Unmatched parenthesis should suggest where to add/remove."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("(a, b, c")
        
        error_str = str(exc_info.value)
        # Should mention closing parenthesis
        assert ")" in error_str or "closing" in error_str.lower()
        
    def test_unknown_operator_suggestion(self):
        """Unknown operator should suggest checking operator table."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X @@ Y")
        
        error_str = str(exc_info.value)
        # Should mention the operator is unknown/undefined
        assert "unknown" in error_str.lower() or "undefined" in error_str.lower()
        assert "@@" in error_str


class TestEdgeCases:
    """Test edge cases in tokenization and error reporting."""
    
    def test_greedy_tokenization_equality_operators(self):
        """Test that =\\= is tokenized as one operator, not three."""
        reader = Reader()
        # =\\= is arithmetic inequality
        result = reader.read_term("X =\\= Y")
        expected = Struct("=\\=", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
        
    def test_canonical_form_preserved(self):
        """Canonical input like ','(a,b) should parse unchanged."""
        reader = Reader()
        result = reader.read_term("','(a, b)")
        expected = Struct(",", (Atom("a"), Atom("b")))
        assert result == expected
        
    def test_quoted_operators_parse(self):
        """Quoted operators like '+'(1,2) should still parse."""
        reader = Reader()
        result = reader.read_term("'+'(1, 2)")
        expected = Struct("+", (Int(1), Int(2)))
        assert result == expected
        
    def test_whitespace_positions_differ(self):
        """Different whitespace should not affect parsing but positions differ."""
        reader = Reader()
        
        # Parse without spaces
        result1 = reader.read_term("a,b")
        expected = Struct(",", (Atom("a"), Atom("b")))
        assert result1 == expected
        
        # Parse with spaces
        result2 = reader.read_term("a , b")
        assert result2 == expected
        
        # Results should be equal but error positions would differ
        assert result1 == result2
        
    def test_error_after_whitespace(self):
        """Error position should point to token, not whitespace."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("X =    Y = Z")  # Extra spaces before Y
        
        error = exc_info.value
        # Position should point to second '=', not the spaces
        assert error.token == "="
        # The second = is at position 9 (after the spaces)
        assert error.position == 9


class TestComplexErrorScenarios:
    """Test error reporting in complex expressions."""
    
    def test_nested_expression_error(self):
        """Errors in nested expressions should have accurate positions."""
        reader = Reader()
        text = "f(X = Y = Z, W)"
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(text)
        
        error = exc_info.value
        # Should point to the problematic second '='
        assert error.token == "="
        assert error.position == 8  # second '='
        
    def test_multiple_errors_report_first(self):
        """When multiple errors exist, report the first one."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term("(X = Y = Z")  # Both xfx chaining and unmatched paren
        
        error = exc_info.value
        # Should report whichever error is encountered first
        assert error.position is not None
        
    def test_error_in_list(self):
        """Errors in list syntax should have positions."""
        reader = Reader()
        text = "[A = B = C]"
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(text)
        
        error = exc_info.value
        assert error.token == "="
        assert error.position == 7  # second '='
        
    def test_error_in_clause_body(self):
        """Errors in clause bodies should have positions."""
        reader = Reader()
        text = "p(X) :- X = Y = Z."
        with pytest.raises(ReaderError) as exc_info:
            reader.read_clause(text)
        
        error = exc_info.value
        assert error.token == "="
        assert error.position == 14  # second '='


class TestErrorRecovery:
    """Test that errors leave the reader in a clean state."""
    
    def test_reader_reusable_after_error(self):
        """Reader should be reusable after encountering an error."""
        reader = Reader()
        
        # First parse should fail
        with pytest.raises(ReaderError):
            reader.read_term("X = Y = Z")
        
        # Second parse should work
        result = reader.read_term("X = Y")
        expected = Struct("=", (Var(0, "X"), Var(1, "Y")))
        assert result == expected
        
    def test_multiple_consecutive_errors(self):
        """Multiple error attempts should each report correctly."""
        reader = Reader()
        
        # First error
        with pytest.raises(ReaderError) as exc1:
            reader.read_term("X = Y = Z")
        assert exc1.value.position == 6
        
        # Second different error
        with pytest.raises(ReaderError) as exc2:
            reader.read_term("(A, B")
        assert exc2.value.position is not None
        
        # Third error
        with pytest.raises(ReaderError) as exc3:
            reader.read_term("X @@ Y")
        assert exc3.value.token == "@@"


class TestRegressionGuards:
    """Guard against regressions in key improvements."""
    
    def test_eof_position_helper_is_used(self):
        """EOF helper provides consistent position calculation."""
        reader = Reader()
        with pytest.raises(ReaderError) as e:
            reader.read_term("(a, b")  # unmatched
        assert e.value.position == len("(a, b")
        
    def test_xfx_non_chainable_message_uniform(self):
        """xfx error messages have uniform format."""
        reader = Reader()
        with pytest.raises(ReaderError) as e:
            reader.read_term("x = y = z")
        msg = str(e.value).lower()
        # Check all expected parts are present
        assert "xfx" in msg
        assert "non-chainable" in msg
        assert "parenthes" in msg
        assert "=" in msg  # The operator should be mentioned


class TestMultiLineAndSpecialCharacters:
    """Test position handling across newlines, tabs, and Unicode."""
    
    def test_error_position_across_newlines(self):
        """Position counts characters including newlines, not just column."""
        reader = Reader()
        # Error on second line
        text = "X = Y,\nZ = W = V"
        with pytest.raises(ReaderError) as exc:
            reader.read_term(text)
        error = exc.value
        # Count positions: X(0) space(1) =(2) space(3) Y(4) ,(5) \n(6) Z(7) space(8) =(9) space(10) W(11) space(12) =(13)
        assert error.token == "="
        assert error.position == 13  # second '=' on second line
        
    def test_error_after_tabs(self):
        """Position must point to the token, not the whitespace."""
        reader = Reader()
        text = "X =\t\tY = Z"
        with pytest.raises(ReaderError) as exc:
            reader.read_term(text)
        assert exc.value.token == "="
        assert exc.value.position == text.rfind("=")  # second '=' index
        
    def test_unicode_positions(self):
        """Positions count code points (not bytes)."""
        reader = Reader()
        # Use a valid Prolog expression with Unicode in a string
        text = "x = y, '世界' = z"  # Unicode in quoted atom
        result = reader.read_term(text)
        # Should parse successfully
        assert result is not None
        
        # Now test error positioning with Unicode
        text2 = "x = y = '世界'"  # xfx chaining error after Unicode
        with pytest.raises(ReaderError) as exc:
            reader.read_term(text2)
        # Position should be at second '=' which is at position 6
        assert exc.value.position == 6
        assert exc.value.token == "="
        
    def test_greedy_tokenization_overlaps_tight(self):
        """Test greedy tokenization with no spaces between operators."""
        reader = Reader()
        # These should parse as single operators
        assert reader.read_term("X=\\=Y") == Struct("=\\=", (Var(0, "X"), Var(1, "Y")))
        assert reader.read_term("X\\==Y") == Struct("\\==", (Var(0, "X"), Var(1, "Y")))
        assert reader.read_term("X@=<Y") == Struct("@=<", (Var(0, "X"), Var(1, "Y")))
        
        # "=\\==" cannot parse because \\== has no prefix form
        # This should give an error
        with pytest.raises(ReaderError) as exc:
            reader.read_term("X=\\==Y")
        # The error should be about unexpected token after =
        assert "unexpected" in str(exc.value).lower()
        
    def test_mixed_xfx_chain_error_positions(self):
        """Mixed xfx operators should have precise error positions."""
        reader = Reader()
        text = "X = Y < Z"
        with pytest.raises(ReaderError) as exc:
            reader.read_term(text)
        # Consistently blame the second operator or the one that causes the conflict
        assert exc.value.token in ["=", "<"]
        if exc.value.token == "=":
            assert exc.value.position == 2
        else:  # "<"
            assert exc.value.position == 6
            
    def test_error_line_column_if_exposed(self):
        """If ReaderError exposes line/column, verify they're consistent."""
        reader = Reader()
        text = "a = b\nc = d = e"
        with pytest.raises(ReaderError) as exc:
            reader.read_term(text)
        err = exc.value
        
        # Currently, 'line' is None (reserved for future)
        assert hasattr(err, "line")
        assert err.line is None  # Not implemented yet
        
        # Currently, 'column' is just an alias for 'position' (absolute position)
        assert hasattr(err, "column")
        assert err.column == err.position  # They're the same
        
        # The error happens at position 6 (the 'c' after newline)
        # because after parsing "a = b", we have leftover tokens
        # Or at position 12 (second '=') if it's the xfx chaining error
        assert err.position in [6, 12]