"""
Tests for Stage 1.5: Operator tokenization
Tests that operators are correctly tokenized with longest-match priority
and that token positions are preserved.
"""

import pytest
from lark import Lark, Token
from prolog.parser.grammar import grammar_text

# Mark all tests in this file for Stage 1.5
pytestmark = pytest.mark.stage15


class TestOperatorTokenization:
    """Test operator token recognition with longest-match priority."""
    
    def setup_method(self):
        """Create parser for testing tokenization."""
        # Create a simple test grammar that accepts any sequence of tokens
        # This allows us to test tokenization without parser constraints
        import pathlib
        grammar_path = pathlib.Path(__file__).parent.parent.parent / "parser" / "grammar.lark"
        full_grammar = grammar_path.read_text()
        
        # Extract just the terminal definitions for tokenization testing
        # Add a simple rule that accepts any token sequence
        test_grammar = full_grammar + "\n\n// Test rule for tokenization\ntoken_stream: (_ANY)*\n_ANY: /./\n"
        
        # Use the full grammar text directly which includes all tokens
        self.parser = Lark(grammar_text, start='program')
    
    def test_comma_token(self):
        """Test that ',' is recognized as a single COMMA token."""
        # This should tokenize properly once operators are added to grammar
        tokens = list(self.parser.lex("foo, bar"))
        # We expect tokens like: NAME COMMA NAME
        comma_tokens = [t for t in tokens if t.value == ',']
        assert len(comma_tokens) == 1
        assert comma_tokens[0].type in ['COMMA', 'OPERATOR']  # Flexible for implementation
    
    def test_semicolon_token(self):
        """Test that ';' is recognized as a single SEMICOLON token."""
        tokens = list(self.parser.lex("foo; bar"))
        semicolon_tokens = [t for t in tokens if t.value == ';']
        assert len(semicolon_tokens) == 1
        assert semicolon_tokens[0].type in ['SEMICOLON', 'OPERATOR']
    
    def test_arrow_token(self):
        """Test that '->' is recognized as a single ARROW token."""
        tokens = list(self.parser.lex("a -> b"))
        arrow_tokens = [t for t in tokens if t.value == '->']
        assert len(arrow_tokens) == 1
        assert arrow_tokens[0].type in ['ARROW', 'OPERATOR']
    
    def test_equals_token(self):
        """Test that '=' is recognized as a single EQUALS token."""
        tokens = list(self.parser.lex("X = Y"))
        equals_tokens = [t for t in tokens if t.value == '=']
        assert len(equals_tokens) == 1
        assert equals_tokens[0].type in ['EQUALS', 'OPERATOR']
    
    def test_arithmetic_operators(self):
        """Test that arithmetic operators are tokenized correctly."""
        # Test each arithmetic operator
        ops = ['+', '-', '*', '/', '//', 'mod']
        for op in ops:
            if op == 'mod':
                text = f"X {op} Y"  # mod is a word token
            else:
                text = f"X{op}Y"  # Others can be without spaces
            tokens = list(self.parser.lex(text))
            op_tokens = [t for t in tokens if t.value == op]
            assert len(op_tokens) == 1, f"Failed to tokenize {op}"
    
    def test_comparison_operators(self):
        """Test that comparison operators are tokenized correctly."""
        ops = ['<', '>', '=<', '>=', '=:=', '=\\=']
        for op in ops:
            text = f"X {op} Y"
            tokens = list(self.parser.lex(text))
            op_tokens = [t for t in tokens if t.value == op]
            assert len(op_tokens) == 1, f"Failed to tokenize {op}"
    
    def test_structural_operators(self):
        """Test that structural comparison operators are tokenized correctly."""
        ops = ['==', '\\==', '@<', '@>', '@=<', '@>=']
        for op in ops:
            text = f"X {op} Y"
            tokens = list(self.parser.lex(text))
            op_tokens = [t for t in tokens if t.value == op]
            assert len(op_tokens) == 1, f"Failed to tokenize {op}"
    
    def test_longest_match_eq_backslash_eq(self):
        """Test that '=\\=' is tokenized as single token, not '=' then '\\' then '='."""
        tokens = list(self.parser.lex("X =\\= Y"))
        # Should be: NAME OPERATOR NAME
        op_tokens = [t for t in tokens if '=' in t.value or '\\' in t.value]
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '=\\='
    
    def test_longest_match_eq_less(self):
        """Test that '=<' is tokenized as single token, not '=' then '<'."""
        tokens = list(self.parser.lex("X =< Y"))
        op_tokens = [t for t in tokens if '=' in t.value or '<' in t.value]
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '=<'
    
    def test_longest_match_at_eq_less(self):
        """Test that '@=<' is tokenized as single token, not '@' then '=' then '<'."""
        tokens = list(self.parser.lex("X @=< Y"))
        op_tokens = [t for t in tokens if '@' in t.value or '=' in t.value or '<' in t.value]
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '@=<'
    
    def test_longest_match_greater_eq(self):
        """Test that '>=' is tokenized as single token, not '>' then '='."""
        tokens = list(self.parser.lex("X >= Y"))
        op_tokens = [t for t in tokens if '>' in t.value or '=' in t.value]
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '>='
    
    def test_longest_match_slash_slash(self):
        """Test that '//' is tokenized as single token, not '/' then '/'."""
        tokens = list(self.parser.lex("X // Y"))
        op_tokens = [t for t in tokens if '/' in t.value]
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '//'
    
    def test_token_positions(self):
        """Test that tokens include start/end positions."""
        text = "foo, bar"
        tokens = list(self.parser.lex(text))
        
        # Each token should have position info
        for token in tokens:
            assert hasattr(token, 'start_pos') or hasattr(token, 'pos_in_stream')
            # Different Lark versions may use different attributes
    
    def test_token_positions_preserved(self):
        """Test that operator tokens preserve source location."""
        text = "X = Y"
        tokens = list(self.parser.lex(text))
        
        # Find the equals token
        equals_tokens = [t for t in tokens if t.value == '=']
        assert len(equals_tokens) == 1
        
        # Check it has position info
        token = equals_tokens[0]
        assert hasattr(token, 'start_pos') or hasattr(token, 'pos_in_stream')
    
    def test_parenthesis_tokens(self):
        """Test that parenthesis tokens are tracked."""
        text = "(a, b)"
        tokens = list(self.parser.lex(text))
        
        lparen = [t for t in tokens if t.value == '(']
        rparen = [t for t in tokens if t.value == ')']
        
        assert len(lparen) == 1
        assert len(rparen) == 1
    
    def test_positions_not_whitespace(self):
        """Test that token positions point to token start/end, not whitespace."""
        text1 = "X,Y"
        text2 = "X , Y"
        
        tokens1 = list(self.parser.lex(text1))
        tokens2 = list(self.parser.lex(text2))
        
        # Both should have same number of tokens (whitespace ignored)
        assert len(tokens1) == len(tokens2)
        
        # But positions should differ
        comma1 = [t for t in tokens1 if t.value == ','][0]
        comma2 = [t for t in tokens2 if t.value == ','][0]
        
        # The positions should be different due to spacing
        if hasattr(comma1, 'start_pos'):
            assert comma1.start_pos != comma2.start_pos
    
    def test_lists_untouched(self):
        """Test that list syntax [H|T] and [a,b,c] remains untouched."""
        # Lists should still parse as before, not as operators
        text1 = "[H|T]"
        text2 = "[1,2,3]"
        
        # These should tokenize without treating | or , as standalone operators
        tokens1 = list(self.parser.lex(text1))
        tokens2 = list(self.parser.lex(text2))
        
        # The brackets should be present
        assert any(t.value == '[' for t in tokens1)
        assert any(t.value == ']' for t in tokens1)
        assert any(t.value == '[' for t in tokens2)
        assert any(t.value == ']' for t in tokens2)
    
    def test_quoted_operators_as_atoms(self):
        """Test that quoted operators parse as atoms: ','(A,B)."""
        text = "','(A, B)"
        tokens = list(self.parser.lex(text))
        
        # The ',' should be parsed as a quoted atom, not an operator
        # This means it should be part of a quoted string or atom token
        quoted_tokens = [t for t in tokens if t.type in ['ATOM', 'QUOTED_ATOM', 'NAME']]
        assert any(',' in t.value for t in quoted_tokens)
    
    def test_canonical_plus(self):
        """Test that '+'(1,2) parses as canonical form when quoted."""
        text = "'+'(1, 2)"
        tokens = list(self.parser.lex(text))
        
        # The '+' should be parsed as a quoted atom
        quoted_tokens = [t for t in tokens if t.type in ['ATOM', 'QUOTED_ATOM', 'NAME']]
        assert any('+' in t.value for t in quoted_tokens)
    
    def test_mod_word_token(self):
        """Test that 'mod' is recognized as a word-token operator."""
        text = "X mod Y"
        tokens = list(self.parser.lex(text))
        
        # 'mod' should be tokenized as an operator
        mod_tokens = [t for t in tokens if t.value == 'mod']
        assert len(mod_tokens) == 1
    
    def test_mod_quoted_atom(self):
        """Test that 'mod'(X,Y) with quotes still parses as atom."""
        text = "'mod'(X, Y)"
        tokens = list(self.parser.lex(text))
        
        # 'mod' in quotes should be an atom, not an operator
        quoted_tokens = [t for t in tokens if t.type in ['ATOM', 'QUOTED_ATOM', 'NAME']]
        assert any('mod' in t.value for t in quoted_tokens)
    
    def test_backwards_compatibility(self):
        """Test that existing atom and struct parsing still works."""
        # Test plain atoms
        text = "foo"
        tokens = list(self.parser.lex(text))
        assert any(t.value == 'foo' for t in tokens)
        
        # Test structures (once operators are added, these should still work)
        text = "foo(bar, baz)"
        tokens = list(self.parser.lex(text))
        assert any(t.value == 'foo' for t in tokens)
        assert any(t.value == 'bar' for t in tokens)
        assert any(t.value == 'baz' for t in tokens)
    
    def test_canonical_input_unchanged(self):
        """Test that canonical input remains unchanged (idempotence)."""
        # Canonical forms should tokenize the same way
        canonical_forms = [
            "','(A, B)",
            "';'(A, B)",
            "'->'(A, B)",
            "'='(X, Y)",
            "'+'(1, 2)"
        ]
        
        for text in canonical_forms:
            tokens = list(self.parser.lex(text))
            # Should successfully tokenize without errors
            assert len(tokens) > 0
    
    def test_mixed_lists_and_operators(self):
        """Test that 'member(X, [1,2,3]), X > 0' mixes lists and operators correctly."""
        text = "member(X, [1,2,3]), X > 0"
        tokens = list(self.parser.lex(text))
        
        # Should have both list brackets and operator tokens
        assert any(t.value == '[' for t in tokens)
        assert any(t.value == ']' for t in tokens)
        assert any(t.value == ',' for t in tokens)  # Comma as operator
        assert any(t.value == '>' for t in tokens)  # Greater than operator
    
    def test_complex_list_patterns(self):
        """Test that 'reverse([1|T], R), R = [_, _ | _]' works with complex patterns."""
        text = "reverse([1|T], R), R = [_, _ | _]"
        tokens = list(self.parser.lex(text))
        
        # Should handle list syntax and operators
        assert any(t.value == '[' for t in tokens)
        assert any(t.value == '|' for t in tokens)  # In list context
        assert any(t.value == ',' for t in tokens)  # As operator
        assert any(t.value == '=' for t in tokens)  # As operator
    
    # Additional test cases from review
    
    @pytest.mark.parametrize("text,expected", [
        ("X =\\== Y", ["=", "\\=="]),
        ("X =< = Y", ["=<", "="]),
        ("X @=<Y", ["@=<"]),
        ("X@=< Y", ["@=<"]),
    ])
    def test_longest_match_overlaps(self, text, expected):
        """Test greedy tokenization with tricky overlaps."""
        tokens = [t.value for t in self.parser.lex(text) if t.value.strip()]
        # Keep only operator-ish tokens for comparison
        ops = [t for t in tokens if any(ch in t for ch in "=<>@\\")]
        assert ops == expected
    
    @pytest.mark.parametrize("text,op", [("X**Y", "**"), ("X//Y", "//")])
    def test_pow_and_idiv_no_spaces(self, text, op):
        """Test that ** and // work without spaces."""
        tokens = list(self.parser.lex(text))
        assert sum(1 for t in tokens if t.value == op) == 1
    
    @pytest.mark.parametrize("text,ops", [
        ("+X", ["+"]),
        ("-X", ["-"]),
        ("1--1", ["-", "-"]),
        ("- - X", ["-", "-"]),
    ])
    def test_unary_plus_minus_present(self, text, ops):
        """Test that unary + and - tokens are present."""
        toks = [t.value for t in self.parser.lex(text)]
        assert [t for t in toks if t in ["+", "-"]] == ops
    
    def test_mod_does_not_match_substring(self):
        """Test that 'mod' doesn't match in 'modulus'."""
        tokens = [t.value for t in self.parser.lex("X modulus Y")]
        assert "mod" not in tokens
    
    @pytest.mark.parametrize("text,needle", [
        ("'=..'(X,Y)", "=.."),
        ("'@=<'(A,B)", "@=<"),
        ("'=\\\\='(A,B)", "=\\="),
    ])
    def test_quoted_operator_like_atoms(self, text, needle):
        """Test that quoted operator-like strings remain atoms."""
        toks = [t for t in self.parser.lex(text)]
        quoted = [t for t in toks if t.type in {'ATOM', 'QUOTED_ATOM', 'NAME'}]
        assert any(needle in t.value for t in quoted)
    
    def test_line_comment_excludes_operators(self):
        """Test that operators in comments are ignored."""
        toks = [t.value for t in self.parser.lex("X = Y % =< should be ignored\n Z = W")]
        assert toks.count("=<") == 0
        assert toks.count("=") >= 2
    
    def test_token_position_monotonicity(self):
        """Test that token positions are monotonic."""
        toks = list(self.parser.lex("a, b; c"))
        # Lark may have line/column; if present, ensure monotonic start positions
        positions = []
        for t in toks:
            if hasattr(t, "start_pos"):
                positions.append(t.start_pos)
            elif hasattr(t, "pos_in_stream"):
                positions.append(t.pos_in_stream)
        if positions:  # Only test if positions exist
            assert positions == sorted(positions)
    
    def test_token_boundaries_around_punctuation(self):
        """Test that X@=<Y produces single @=< token."""
        tokens = list(self.parser.lex("X@=<Y"))
        op_tokens = [t for t in tokens if '@' in t.value or '=' in t.value or '<' in t.value]
        assert len(op_tokens) == 1
        assert op_tokens[0].value == '@=<'
    
    def test_arithmetic_completeness(self):
        """Test that ** is included in arithmetic operators."""
        text = "X ** Y"
        tokens = list(self.parser.lex(text))
        op_tokens = [t for t in tokens if t.value == '**']
        assert len(op_tokens) == 1