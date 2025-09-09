"""
Test operator tokenization at the lexer level without parser constraints.
"""

import pytest
from lark import Lark
from lark.lexer import Token

# Create a test grammar that includes all operators as terminals
# but has a permissive grammar rule that accepts any token sequence
TEST_GRAMMAR = r"""
// Test start rule - accepts any sequence of tokens
start: any*
any: ATOM | VARIABLE | SIGNED_INT | QUOTED_ATOM
   | COMMA | SEMICOLON | ARROW | EQUALS | LT | GT | PLUS | MINUS | STAR | SLASH
   | DOUBLE_SLASH | EQ_COLON_EQ | EQ_BACKSLASH_EQ | BACKSLASH_EQ | BACKSLASH_EQEQ
   | AT_LT | AT_GT | AT_EQ_LT | AT_GT_EQ | EQ_LT | GT_EQ | DOUBLE_EQ | DOUBLE_STAR
   | BACKSLASH_PLUS | MOD | IS
   | LPAREN | RPAREN | LBRACKET | RBRACKET | DOT | PIPE

// Atoms and basic tokens
ATOM: /[a-z][a-zA-Z0-9_]*/
QUOTED_ATOM: "'" /[^']*/ "'"
SIGNED_INT: /-?\d+/
VARIABLE: /[A-Z_][a-zA-Z0-9_]*/

// Multi-character operators (higher priority via .2 suffix)
ARROW.2: "->"
DOUBLE_SLASH.2: "//"
EQ_COLON_EQ.2: "=:="
EQ_BACKSLASH_EQ.2: "=\\="
BACKSLASH_EQEQ.2: "\\=="
BACKSLASH_EQ.2: "\\="
AT_GT_EQ.2: "@>="
AT_EQ_LT.2: "@=<"
AT_LT.2: "@<"
AT_GT.2: "@>"
EQ_LT.2: "=<"
GT_EQ.2: ">="
DOUBLE_EQ.2: "=="
DOUBLE_STAR.2: "**"
BACKSLASH_PLUS.2: "\\+"

// Single-character operators (default priority)
COMMA: ","
SEMICOLON: ";"
EQUALS: "="
LT: "<"
GT: ">"
PLUS: "+"
MINUS: "-"
STAR: "*"
SLASH: "/"

// Word operators
MOD: "mod"
IS: "is"

// Parentheses and brackets
LPAREN: "("
RPAREN: ")"
LBRACKET: "["
RBRACKET: "]"

// Other
DOT: "."
PIPE: "|"

// Whitespace - ignored
%ignore /\s+/
"""

pytestmark = pytest.mark.stage15


class TestOperatorLexing:
    """Test pure lexing of operators without parser constraints."""
    
    def setup_method(self):
        """Create a lexer with the test grammar."""
        self.lexer = Lark(TEST_GRAMMAR, start='start')
    
    def get_tokens(self, text):
        """Get list of token values from text."""
        tree = self.lexer.parse(text)
        # Get all tokens from the tree
        return [token.value for token in tree.scan_values(lambda v: isinstance(v, Token))]
    
    def test_single_operators(self):
        """Test that single operators are recognized."""
        ops = [',', ';', '=', '<', '>', '+', '-', '*', '/']
        for op in ops:
            text = f"X {op} Y"
            tokens = self.get_tokens(text)
            assert op in tokens, f"Operator {op} not found in tokens"
    
    def test_multi_char_operators(self):
        """Test multi-character operators."""
        ops = ['->', '//', '=:=', '=\\=', '\\=', '\\==', '@<', '@>', '@=<', '@>=', '=<', '>=', '==', '**', '\\+']
        for op in ops:
            text = f"X {op} Y"
            tokens = self.get_tokens(text)
            assert op in tokens, f"Operator {op} not found in tokens"
    
    def test_longest_match(self):
        """Test that longest match takes priority."""
        # Test =\= is one token, not = then \=
        tokens = self.get_tokens("X =\\= Y")
        assert '=\\=' in tokens
        assert tokens.count('=') == 0  # Should not have separate =
        
        # Test =< is one token
        tokens = self.get_tokens("X =< Y")
        assert '=<' in tokens
        
        # Test // is one token
        tokens = self.get_tokens("X // Y")
        assert '//' in tokens
        assert tokens.count('/') == 0  # Should not have separate /
    
    def test_word_operators(self):
        """Test word operators like 'mod' and 'is'."""
        tokens = self.get_tokens("X mod Y")
        assert 'mod' in tokens
        
        tokens = self.get_tokens("X is Y")
        assert 'is' in tokens
    
    def test_no_spaces_needed(self):
        """Test that operators work without spaces (where appropriate)."""
        tokens = self.get_tokens("X+Y")
        assert '+' in tokens
        assert 'X' in tokens
        assert 'Y' in tokens
        
        tokens = self.get_tokens("X**Y")
        assert '**' in tokens
        
        tokens = self.get_tokens("X//Y")
        assert '//' in tokens