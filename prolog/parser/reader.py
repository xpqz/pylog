"""Reader module with Pratt parser for operator expressions - Issue #37.

This module implements a Pratt parser that transforms operator expressions
from tokenized input into canonical AST forms. It uses the operator table
as the single source of truth for precedence and associativity.

The reader operates as a pure transformation layer between the grammar
(which only tokenizes operators) and the AST (which uses canonical forms).
"""

import logging
import re
from typing import Optional, Union, List, Tuple, Dict, Any
from dataclasses import dataclass

from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.parser.operators import get_operator_info, is_xfx_operator, is_stage1_supported

logger = logging.getLogger(__name__)


class ReaderError(Exception):
    """Exception raised for reader/parser errors.
    
    Attributes:
        message: The error message describing what went wrong
        position: Character position in the input where the error occurred (0-based)
        column: Alias for position for compatibility
        token: The token/lexeme that caused the error (e.g., "@@" for unknown operator)
        lexeme: Alias for token for consistency
    """
    
    def __init__(self, message: str, position: Optional[int] = None, token: Optional[str] = None):
        super().__init__(message)
        self.message = message
        self.position = position
        self.column = position  # Alias for compatibility
        self.token = token
        self.lexeme = token  # Alias for consistency
    
    def __str__(self):
        if self.position is not None:
            return f"ReaderError at position {self.position}: {self.message}"
        return f"ReaderError: {self.message}"


@dataclass
class Token:
    """Token with type and value."""
    type: str
    value: str
    position: int = 0


class Tokenizer:
    """Tokenizer for Prolog text."""
    
    # Token patterns in order of priority
    PATTERNS = [
        # Comments (skip)
        (r'%.*', None),
        (r'/\*(.|\n)*?\*/', None),
        
        # Multi-character operators (must come before single-char)
        (r':-', 'COLON_MINUS'),
        (r'\?-', 'QUESTION_MINUS'),
        (r'->', 'ARROW'),
        (r'//', 'DOUBLE_SLASH'),
        (r'=:=', 'EQ_COLON_EQ'),
        (r'=\\=', 'EQ_BACKSLASH_EQ'),
        (r'\\=', 'BACKSLASH_EQ'),
        (r'\\==', 'BACKSLASH_EQEQ'),
        (r'@<', 'AT_LT'),
        (r'@>', 'AT_GT'),
        (r'@=<', 'AT_EQ_LT'),
        (r'@>=', 'AT_GT_EQ'),
        (r'=<', 'EQ_LT'),
        (r'>=', 'GT_EQ'),
        (r'==', 'DOUBLE_EQ'),
        (r'\*\*', 'DOUBLE_STAR'),
        (r'\\\+', 'BACKSLASH_PLUS'),
        
        # Single-character operators and punctuation
        (r',', 'COMMA'),
        (r';', 'SEMICOLON'),
        (r'=', 'EQUALS'),
        (r'<', 'LT'),
        (r'>', 'GT'),
        (r'\+', 'PLUS'),
        (r'-', 'MINUS'),
        (r'\*', 'STAR'),
        (r'/', 'SLASH'),
        (r'\.', 'DOT'),
        (r'\|', 'PIPE'),
        (r'\(', 'LPAREN'),
        (r'\)', 'RPAREN'),
        (r'\[', 'LBRACKET'),
        (r'\]', 'RBRACKET'),
        
        # Word operators
        (r'\bmod\b', 'MOD'),
        (r'\bis\b', 'IS'),
        
        # Quoted atoms
        (r"'([^'\\\\]|\\\\.)*'", 'QUOTED_ATOM'),
        
        # Numbers (including negative)
        (r'-?\d+', 'SIGNED_INT'),
        
        # Variables (uppercase or underscore start)
        (r'[A-Z_][a-zA-Z0-9_]*', 'VARIABLE'),
        
        # Atoms (lowercase start or special)
        (r'[a-z][a-zA-Z0-9_]*', 'ATOM'),
        (r'!', 'ATOM'),
        
        # Whitespace (skip)
        (r'\s+', None),
    ]
    
    def __init__(self):
        # Compile patterns
        self.compiled_patterns = [(re.compile(p), t) for p, t in self.PATTERNS]
    
    def tokenize(self, text: str) -> List[Token]:
        """Tokenize input text."""
        tokens = []
        pos = 0
        
        while pos < len(text):
            matched = False
            
            for pattern, token_type in self.compiled_patterns:
                match = pattern.match(text, pos)
                if match:
                    if token_type:  # Not a skip token
                        tokens.append(Token(token_type, match.group(), pos))
                    pos = match.end()
                    matched = True
                    break
            
            if not matched:
                # Try to extract the problematic token for better error
                end = pos + 1
                while end < len(text) and not text[end].isspace():
                    end += 1
                problem_token = text[pos:end]
                raise ReaderError(
                    f"Unexpected token: {problem_token}",
                    position=pos,
                    token=problem_token
                )
        
        return tokens


class TokenStream:
    """Stream of tokens with lookahead capability."""
    
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0
        self.var_map: Dict[str, int] = {}
        self.next_var_id = 0
    
    def peek(self) -> Optional[Token]:
        """Look at current token without consuming."""
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None
    
    def consume(self) -> Optional[Token]:
        """Consume and return current token."""
        if self.pos < len(self.tokens):
            token = self.tokens[self.pos]
            self.pos += 1
            return token
        return None
    
    def at_end(self) -> bool:
        """Check if at end of stream."""
        return self.pos >= len(self.tokens)
    
    def get_position(self) -> int:
        """Get current position in stream."""
        if self.pos < len(self.tokens):
            return self.tokens[self.pos].position
        return -1
    
    def get_var_id(self, name: str) -> int:
        """Get or create variable ID for name."""
        if name == "_":
            # Each _ gets a unique ID
            var_id = self.next_var_id
            self.next_var_id += 1
            return var_id
        
        if name not in self.var_map:
            self.var_map[name] = self.next_var_id
            self.next_var_id += 1
        return self.var_map[name]


class PrattParser:
    """Pratt parser for operator expressions."""
    
    def __init__(self, stream: TokenStream, strict_unsupported: bool = False):
        self.stream = stream
        self.strict_unsupported = strict_unsupported
    
    def parse_term(self, min_precedence: int = 1200) -> Term:
        """Parse a term with operator precedence."""
        left = self.parse_prefix()
        
        while not self.stream.at_end():
            token = self.stream.peek()
            if not token:
                break
            
            # Check for infix operator
            op_info = self._get_infix_info(token)
            if not op_info:
                break
            
            precedence, assoc_type, canonical = op_info
            
            # Check precedence
            if precedence > min_precedence:
                break
            
            # Consume the operator
            self.stream.consume()
            
            # Handle unsupported operators
            if not is_stage1_supported(token.value, 'infix'):
                if self.strict_unsupported:
                    raise ReaderError(
                        f"Unsupported operator '{token.value}' in Stage 1",
                        position=token.position,
                        token=token.value
                    )
                else:
                    logger.warning(f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)")
            
            # Determine right-side precedence based on associativity
            if assoc_type == 'xfx':
                # Non-associative - right side must be strictly less precedence
                next_min = precedence - 1
            elif assoc_type == 'yfx':
                # Left-associative - right side must be strictly less
                next_min = precedence - 1
            else:  # xfy
                # Right-associative - right side can be same precedence
                next_min = precedence
            
            # Parse right side
            right = self.parse_term(next_min)
            
            # For xfx operators, check if the right side tries to use the same operator
            # This happens when we have X = Y = Z
            if assoc_type == 'xfx':
                # Check if there's another operator of same precedence waiting
                next_token = self.stream.peek()
                if next_token:
                    next_op_info = self._get_infix_info(next_token)
                    if next_op_info and next_op_info[0] == precedence:
                        # Same precedence operator trying to chain - this is an error
                        pos = self.stream.get_position()
                        raise ReaderError(
                            f"Operator '{next_token.value}' is non-chainable (xfx)",
                            position=pos,
                            token=next_token.value
                        )
            
            # Build structure
            functor = token.value
            left = Struct(functor, (left, right))
        
        return left
    
    def parse_prefix(self) -> Term:
        """Parse a prefix expression or primary term."""
        token = self.stream.peek()
        
        if not token:
            raise ReaderError("Unexpected end of input")
        
        # Check for prefix operator
        op_info = self._get_prefix_info(token)
        if op_info:
            precedence, assoc_type, canonical = op_info
            self.stream.consume()
            
            # Special case: negative/positive numeral
            if token.value in ['-', '+']:
                next_token = self.stream.peek()
                if next_token and next_token.type == 'SIGNED_INT':
                    # Check if it's a literal (no space between) and no higher precedence operator follows
                    if token.position + len(token.value) == next_token.position:
                        # Look ahead for power operator
                        saved_pos = self.stream.pos
                        self.stream.consume()  # Skip the number
                        following = self.stream.peek()
                        self.stream.pos = saved_pos  # Restore position
                        
                        # If there's a ** operator following, treat - as operator not literal
                        if following and following.type == 'DOUBLE_STAR':
                            pass  # Don't consume, treat as operator
                        else:
                            # It's a literal number
                            self.stream.consume()
                            value = int(next_token.value)
                            if token.value == '-':
                                return Int(-abs(value))
                            else:
                                return Int(abs(value))
            
            # Handle unsupported operators
            if not is_stage1_supported(token.value, 'prefix'):
                if self.strict_unsupported:
                    raise ReaderError(
                        f"Unsupported operator '{token.value}' in Stage 1",
                        position=token.position,
                        token=token.value
                    )
                else:
                    logger.warning(f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)")
            
            # For fy, allow same precedence; for fx, require less
            if assoc_type == 'fy':
                next_min = precedence
            else:  # fx
                next_min = precedence - 1
            
            arg = self.parse_term(next_min)
            return Struct(token.value, (arg,))
        
        # Not a prefix operator, parse primary
        return self.parse_primary()
    
    def parse_primary(self) -> Term:
        """Parse a primary term (atom, int, var, list, struct, or parenthesized)."""
        token = self.stream.peek()
        
        if not token:
            raise ReaderError("Unexpected end of input")
        
        # Parenthesized expression
        if token.type == 'LPAREN':
            self.stream.consume()
            term = self.parse_term()
            next_token = self.stream.consume()
            if not next_token or next_token.type != 'RPAREN':
                raise ReaderError("Expected closing parenthesis")
            return term
        
        # List
        if token.type == 'LBRACKET':
            return self.parse_list()
        
        # Variable
        if token.type == 'VARIABLE':
            self.stream.consume()
            var_id = self.stream.get_var_id(token.value)
            return Var(var_id, token.value)
        
        # Integer
        if token.type == 'SIGNED_INT':
            self.stream.consume()
            return Int(int(token.value))
        
        # Atom or structure
        if token.type in ['ATOM', 'QUOTED_ATOM']:
            self.stream.consume()
            
            # Process quoted atom
            if token.type == 'QUOTED_ATOM':
                name = token.value[1:-1]  # Remove quotes
                name = name.replace("\\'", "'")
                name = name.replace("\\n", "\n")
                name = name.replace("\\t", "\t")
                name = name.replace("\\\\", "\\")
            else:
                name = token.value
            
            # Check for structure
            next_token = self.stream.peek()
            if next_token and next_token.type == 'LPAREN':
                self.stream.consume()
                args = self.parse_term_list()
                closing = self.stream.consume()
                if not closing or closing.type != 'RPAREN':
                    raise ReaderError("Expected closing parenthesis in structure")
                if not args:
                    raise ReaderError(f"Empty parentheses not allowed - use plain atom '{name}' instead")
                return Struct(name, tuple(args))
            
            return Atom(name)
        
        # Word operators that can appear as atoms
        if token.type in ['MOD', 'IS']:
            self.stream.consume()
            
            # Check if it's being used as a functor
            next_token = self.stream.peek()
            if next_token and next_token.type == 'LPAREN':
                self.stream.consume()
                args = self.parse_term_list()
                closing = self.stream.consume()
                if not closing or closing.type != 'RPAREN':
                    raise ReaderError("Expected closing parenthesis in structure")
                if not args:
                    raise ReaderError(f"Empty parentheses not allowed - use plain atom '{token.value}' instead")
                return Struct(token.value, tuple(args))
            
            return Atom(token.value)
        
        raise ReaderError(
            f"Unexpected token: {token.type} '{token.value}'", 
            position=token.position,
            token=token.value
        )
    
    def parse_list(self) -> PrologList:
        """Parse a list."""
        opening = self.stream.consume()
        if opening.type != 'LBRACKET':
            raise ReaderError("Expected opening bracket")
        
        # Empty list
        next_token = self.stream.peek()
        if next_token and next_token.type == 'RBRACKET':
            self.stream.consume()
            return PrologList((), Atom("[]"))
        
        # Parse elements
        elements = []
        tail = None
        
        while True:
            # Parse element but stop at commas - they're separators in lists
            elements.append(self.parse_term_arg())
            
            next_token = self.stream.peek()
            if not next_token:
                raise ReaderError("Unexpected end of list")
            
            if next_token.type == 'COMMA':
                self.stream.consume()
                continue
            elif next_token.type == 'PIPE':
                self.stream.consume()
                tail = self.parse_term_arg()
                next_token = self.stream.peek()
                if not next_token or next_token.type != 'RBRACKET':
                    raise ReaderError("Expected closing bracket after tail")
                self.stream.consume()
                break
            elif next_token.type == 'RBRACKET':
                self.stream.consume()
                tail = Atom("[]")
                break
            else:
                raise ReaderError(f"Unexpected token in list: {next_token.type}")
        
        return PrologList(tuple(elements), tail)
    
    def parse_term_list(self) -> List[Term]:
        """Parse comma-separated list of terms (for structures and goals)."""
        terms = []
        
        # Handle empty argument list
        next_token = self.stream.peek()
        if next_token and next_token.type == 'RPAREN':
            return terms
        
        while True:
            # Parse term but stop at commas - they're separators here, not operators
            term = self.parse_term_arg()
            terms.append(term)
            
            next_token = self.stream.peek()
            if not next_token:
                break
            
            if next_token.type == 'COMMA':
                self.stream.consume()
                continue
            else:
                break
        
        return terms
    
    def parse_term_arg(self) -> Term:
        """Parse a term as an argument (comma has no operator meaning)."""
        # Save the parse_term method
        original_parse = self.parse_term
        
        # Temporarily replace it to stop at commas
        def parse_term_no_comma(min_precedence: int = 1200) -> Term:
            left = self.parse_prefix()
            
            while not self.stream.at_end():
                token = self.stream.peek()
                if not token:
                    break
                
                # Stop at comma - it's a separator in argument lists
                if token.type == 'COMMA':
                    break
                
                # Check for infix operator
                op_info = self._get_infix_info(token)
                if not op_info:
                    break
                
                precedence, assoc_type, canonical = op_info
                
                # Check precedence
                if precedence > min_precedence:
                    break
                
                # Consume the operator
                self.stream.consume()
                
                # Handle unsupported operators
                if not is_stage1_supported(token.value, 'infix'):
                    if self.strict_unsupported:
                        raise ReaderError(
                            f"Unsupported operator '{token.value}' in Stage 1",
                            position=token.position,
                            token=token.value
                        )
                    else:
                        logger.warning(f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)")
                
                # Determine right-side precedence based on associativity
                if assoc_type == 'xfx':
                    next_min = precedence - 1
                elif assoc_type == 'yfx':
                    next_min = precedence - 1
                else:  # xfy
                    next_min = precedence
                
                # Parse right side (recursively with no-comma version)
                self.parse_term = parse_term_no_comma
                right = self.parse_term(next_min)
                
                # For xfx operators, check if trying to chain
                if assoc_type == 'xfx':
                    next_token = self.stream.peek()
                    if next_token and next_token.type != 'COMMA':
                        next_op_info = self._get_infix_info(next_token)
                        if next_op_info and next_op_info[0] == precedence:
                            pos = self.stream.get_position()
                            raise ReaderError(
                                f"Operator '{next_token.value}' is non-chainable (xfx)",
                                position=pos,
                                token=next_token.value
                            )
                
                # Build structure
                functor = token.value
                left = Struct(functor, (left, right))
            
            return left
        
        # Use the no-comma version
        self.parse_term = parse_term_no_comma
        result = self.parse_term()
        
        # Restore original
        self.parse_term = original_parse
        
        return result
    
    def _get_infix_info(self, token: Token) -> Optional[Tuple[int, str, str]]:
        """Get operator info for infix position."""
        if token.type in ['COMMA', 'SEMICOLON', 'EQUALS', 'LT', 'GT', 
                          'PLUS', 'MINUS', 'STAR', 'SLASH', 'ARROW',
                          'DOUBLE_SLASH', 'EQ_COLON_EQ', 'EQ_BACKSLASH_EQ',
                          'BACKSLASH_EQ', 'BACKSLASH_EQEQ', 'AT_LT', 'AT_GT',
                          'AT_EQ_LT', 'AT_GT_EQ', 'EQ_LT', 'GT_EQ', 
                          'DOUBLE_EQ', 'DOUBLE_STAR', 'MOD', 'IS']:
            return get_operator_info(token.value, 'infix')
        return None
    
    def _get_prefix_info(self, token: Token) -> Optional[Tuple[int, str, str]]:
        """Get operator info for prefix position."""
        if token.type in ['PLUS', 'MINUS', 'BACKSLASH_PLUS']:
            return get_operator_info(token.value, 'prefix')
        return None


class Reader:
    """Reader with Pratt parser for reading operator expressions into canonical AST.
    
    The reader transforms operator expressions using precedence and
    associativity rules from the operator table. It handles:
    - Infix, prefix, and postfix operators
    - Precedence (higher precedence binds tighter)
    - Associativity (left/right/non-associative)
    - xfx non-chainable enforcement
    - Negative numeral policy
    
    Args:
        strict_unsupported: If True, raise ReaderError on unsupported operators
                          instead of just logging warnings (default: False)
    """
    
    def __init__(self, strict_unsupported: bool = False):
        """Initialize the reader.
        
        Args:
            strict_unsupported: If True, unsupported operators raise errors instead of warnings
        """
        self.tokenizer = Tokenizer()
        self.strict_unsupported = strict_unsupported
    
    def read_term(self, text: str) -> Term:
        """Read a term from text, handling operator expressions.
        
        Args:
            text: The Prolog text to parse
            
        Returns:
            The parsed term in canonical AST form with operators transformed
            to canonical structures (e.g., "1+2" becomes Struct("+", (Int(1), Int(2))))
            
        Raises:
            ReaderError: If the text cannot be parsed, contains unknown operators,
                       or uses unsupported operators in strict mode
        """
        try:
            # Tokenize
            tokens = self.tokenizer.tokenize(text)
            
            # Filter out DOT tokens if present at end
            if tokens and tokens[-1].type == 'DOT':
                tokens = tokens[:-1]
            
            if not tokens:
                raise ReaderError("Empty input")
            
            # Parse with Pratt parser
            stream = TokenStream(tokens)
            parser = PrattParser(stream, self.strict_unsupported)
            result = parser.parse_term()
            
            # Check for leftover tokens
            if not stream.at_end():
                leftover = stream.peek()
                raise ReaderError(f"Unexpected token after expression: {leftover.value}")
            
            return result
            
        except ReaderError:
            raise
        except Exception as e:
            raise ReaderError(f"Parse error: {e}")
    
    def read_clause(self, text: str) -> Clause:
        """Read a clause from text.
        
        Args:
            text: The Prolog clause text to parse (e.g., "foo(X) :- bar(X).")
            
        Returns:
            A Clause object with head and optional body, with operators in
            canonical form
            
        Raises:
            ReaderError: If the text cannot be parsed as a clause or contains
                       syntax errors
        """
        try:
            # Tokenize
            tokens = self.tokenizer.tokenize(text)
            
            # Check for trailing DOT
            if not tokens or tokens[-1].type != 'DOT':
                raise ReaderError("Clause must end with period")
            
            # Find :- separator if present
            colon_minus_idx = None
            for i, token in enumerate(tokens):
                if token.type == 'COLON_MINUS':
                    colon_minus_idx = i
                    break
            
            # Remove trailing DOT
            tokens = tokens[:-1]
            
            if colon_minus_idx is not None:
                # Rule: head :- body
                head_tokens = tokens[:colon_minus_idx]
                body_tokens = tokens[colon_minus_idx+1:]
                
                if not head_tokens:
                    raise ReaderError("Empty head in rule")
                if not body_tokens:
                    raise ReaderError("Empty body in rule")
                
                # Parse head
                head_stream = TokenStream(head_tokens)
                head_parser = PrattParser(head_stream, self.strict_unsupported)
                head = head_parser.parse_term()
                
                # Validate head (must be atom or structure, not variable/int/list)
                if not isinstance(head, (Atom, Struct)):
                    raise ReaderError("Invalid clause head - must be atom or structure")
                
                # Parse body with same variable context
                body_stream = TokenStream(body_tokens)
                # Copy variable state from head to body
                body_stream.var_map = head_stream.var_map.copy()
                body_stream.next_var_id = head_stream.next_var_id
                body_parser = PrattParser(body_stream, self.strict_unsupported)
                body_term = body_parser.parse_term()
                
                # Flatten conjunction into list for Clause
                body_goals = []
                self._flatten_conjunction(body_term, body_goals)
                
                return Clause(head, body_goals)
            else:
                # Fact: just head
                stream = TokenStream(tokens)
                parser = PrattParser(stream, self.strict_unsupported)
                head = parser.parse_term()
                
                # Validate head (must be atom or structure, not variable/int/list)
                if not isinstance(head, (Atom, Struct)):
                    raise ReaderError("Invalid clause head - must be atom or structure")
                
                return Clause(head, [])
            
        except ReaderError:
            raise
        except Exception as e:
            raise ReaderError(f"Failed to parse clause: {e}")
    
    def read_query(self, text: str) -> List[Term]:
        """Read a query from text.
        
        Args:
            text: The Prolog query text to parse (including ?-, e.g., "?- foo(X), bar(X).")
            
        Returns:
            List of goal terms with operators in canonical form. Conjunctions
            are flattened into a list.
            
        Raises:
            ReaderError: If the text cannot be parsed as a query or doesn't
                       start with ?-
        """
        try:
            # Tokenize
            tokens = self.tokenizer.tokenize(text)
            
            # Find and skip ?-
            if tokens and tokens[0].type == 'QUESTION_MINUS':
                tokens = tokens[1:]
            else:
                raise ReaderError("Query must start with ?-")
            
            # Check for trailing DOT
            if not tokens or tokens[-1].type != 'DOT':
                raise ReaderError("Query must end with period")
            
            # Remove trailing DOT
            tokens = tokens[:-1]
            
            if not tokens:
                raise ReaderError("Empty query")
            
            # Parse goals
            stream = TokenStream(tokens)
            parser = PrattParser(stream, self.strict_unsupported)
            goal_term = parser.parse_term()
            
            # Flatten conjunction into list
            goals = []
            self._flatten_conjunction(goal_term, goals)
            
            return goals
            
        except ReaderError:
            raise
        except Exception as e:
            raise ReaderError(f"Failed to parse query: {e}")
    
    def _flatten_conjunction(self, term: Term, goals: List[Term]) -> None:
        """Flatten a conjunction into a list of goals."""
        if isinstance(term, Struct) and term.functor == ',':
            self._flatten_conjunction(term.args[0], goals)
            self._flatten_conjunction(term.args[1], goals)
        else:
            goals.append(term)
    
    def read_program(self, text: str) -> List[Clause]:
        """Read a Prolog program from text.
        
        Args:
            text: The Prolog program text to parse (multiple clauses)
            
        Returns:
            List of Clause objects with operators in canonical form
            
        Raises:
            ReaderError: If the text cannot be parsed as a program
        """
        if not text.strip():
            return []
        
        clauses = []
        
        # Remove comments first but preserve line structure
        lines = text.split('\n')
        cleaned_lines = []
        for line in lines:
            # Remove line comments
            if '%' in line:
                line = line[:line.index('%')]
            cleaned_lines.append(line)
        cleaned_text = '\n'.join(cleaned_lines).strip()
        
        if not cleaned_text:
            return []
        
        # Parse clauses by finding periods at the top level (not inside parens/brackets)
        current_clause = []
        paren_depth = 0
        bracket_depth = 0
        in_quoted_atom = False
        escape_next = False
        
        i = 0
        while i < len(cleaned_text):
            char = cleaned_text[i]
            
            # Handle escape sequences in quoted atoms
            if escape_next:
                current_clause.append(char)
                escape_next = False
                i += 1
                continue
            
            # Track quoted atoms
            if char == "'" and not escape_next:
                in_quoted_atom = not in_quoted_atom
                current_clause.append(char)
                i += 1
                continue
            
            # Handle escapes in quoted atoms
            if in_quoted_atom and char == '\\':
                escape_next = True
                current_clause.append(char)
                i += 1
                continue
            
            # Skip tracking inside quoted atoms
            if in_quoted_atom:
                current_clause.append(char)
                i += 1
                continue
            
            # Track parentheses and brackets
            if char == '(':
                paren_depth += 1
            elif char == ')':
                paren_depth -= 1
            elif char == '[':
                bracket_depth += 1
            elif char == ']':
                bracket_depth -= 1
            elif char == '.' and paren_depth == 0 and bracket_depth == 0:
                # Found a clause terminator at top level
                current_clause.append(char)
                clause_text = ''.join(current_clause).strip()
                if clause_text:
                    try:
                        clause = self.read_clause(clause_text)
                        clauses.append(clause)
                    except ReaderError as e:
                        # Add context about which clause failed
                        raise ReaderError(f"Error in clause: {e}")
                current_clause = []
                i += 1
                continue
            
            current_clause.append(char)
            i += 1
        
        # Check for incomplete clause
        remaining = ''.join(current_clause).strip()
        if remaining:
            raise ReaderError(f"Incomplete clause at end of program: {remaining[:50]}")
        
        return clauses