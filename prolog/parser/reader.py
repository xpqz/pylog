"""Reader module with Pratt parser for operator expressions - Issue #37.

This module implements a Pratt parser that transforms operator expressions
from tokenized input into canonical AST forms. It uses the operator table
as the single source of truth for precedence and associativity.

The reader operates as a pure transformation layer between the grammar
(which only tokenizes operators) and the AST (which uses canonical forms).
"""

import logging
import re
from typing import Optional, List, Tuple, Dict
from dataclasses import dataclass

from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.parser.operators import (
    get_operator_info,
    is_stage1_supported,
    get_all_operators,
)

logger = logging.getLogger(__name__)


class ReaderError(Exception):
    """Exception raised for reader/parser errors.

    Attributes:
        message (str): The error message describing what went wrong
        position (int | None): Character position in input (0-based code-point index)
        column (int | None): Alias for position for compatibility (same as position)
        line (None): Line number (not yet implemented, reserved for future)
        token (str | None): The problematic token/lexeme (e.g., "@@" for unknown operator)
        lexeme (str | None): Alias for token for consistency

    Position Policy:
        - 0-based indexing counting from start of input
        - Counts Unicode code points (not bytes)
        - Includes newlines in the count
        - Points to the exact character where error was detected
        - EOF errors report position after last token

    Note: Line/column tracking within lines is not yet implemented.
    The 'line' field is always None and 'column' is just an alias for 'position'.
    """

    def __init__(
        self, message: str, position: Optional[int] = None, token: Optional[str] = None
    ):
        super().__init__(message)
        self.message = message
        self.position = position
        self.column = position  # Alias for compatibility
        self.line = None  # Reserved for future line tracking
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


def _generate_tokenizer_patterns():
    """Generate tokenizer patterns from operator table.

    This function dynamically generates token patterns from the operator table,
    ensuring that the tokenizer always matches what's defined in operators.py.
    """
    patterns = []

    # Comments (skip) - must come first
    patterns.append((r"%.*", None))
    patterns.append((r"/\*(.|\n)*?\*/", None))

    # Get all operators from the operator table
    operator_table = get_all_operators()

    # Sort operators by length (longest first) for greedy matching
    # This ensures e.g., "=\\=" matches before "="
    operators_by_length = {}
    for (op, pos), info in operator_table.items():
        length = len(op)
        if length not in operators_by_length:
            operators_by_length[length] = []
        if op not in [item[0] for item in operators_by_length[length]]:
            operators_by_length[length].append((op, pos))

    # Add special operators not in the table
    patterns.append((r":-", "COLON_MINUS"))
    patterns.append((r"\?-", "QUESTION_MINUS"))

    # Process operators from longest to shortest
    for length in sorted(operators_by_length.keys(), reverse=True):
        for op, pos in operators_by_length[length]:
            # Escape special regex characters
            escaped_op = re.escape(op)

            # Create token name from operator
            token_name = _operator_to_token_name(op)

            # Word operators need word boundaries
            if op.isalpha():
                pattern = r"\b" + escaped_op + r"\b"
            else:
                pattern = escaped_op

            # Check if pattern already added (some ops may be both infix and prefix)
            if not any(p[0] == pattern for p in patterns):
                patterns.append((pattern, token_name))

    # Add non-operator tokens
    patterns.extend(
        [
            # Punctuation
            (r"\.", "DOT"),
            (r"\|", "PIPE"),
            (r"\(", "LPAREN"),
            (r"\)", "RPAREN"),
            (r"\[", "LBRACKET"),
            (r"\]", "RBRACKET"),
            # Quoted atoms
            (r"'([^'\\\\]|\\\\.)*'", "QUOTED_ATOM"),
            # Numbers (including negative)
            (r"-?\d+", "SIGNED_INT"),
            # Variables (uppercase or underscore start)
            (r"[A-Z_][a-zA-Z0-9_]*", "VARIABLE"),
            # Atoms (lowercase start or special)
            (r"[a-z][a-zA-Z0-9_]*", "ATOM"),
            (r"!", "ATOM"),
            # Whitespace (skip)
            (r"\s+", None),
        ]
    )

    return patterns


def _operator_to_token_name(op):
    """Convert operator symbol to token name."""
    # Map of special cases
    special_names = {
        ",": "COMMA",
        ";": "SEMICOLON",
        "=": "EQUALS",
        "<": "LT",
        ">": "GT",
        "+": "PLUS",
        "-": "MINUS",
        "*": "STAR",
        "/": "SLASH",
        "->": "ARROW",
        "//": "DOUBLE_SLASH",
        "**": "DOUBLE_STAR",
        "=:=": "EQ_COLON_EQ",
        "=\\=": "EQ_BACKSLASH_EQ",
        "\\=": "BACKSLASH_EQ",
        "\\==": "BACKSLASH_EQEQ",
        "==": "DOUBLE_EQ",
        "@<": "AT_LT",
        "@>": "AT_GT",
        "@=<": "AT_EQ_LT",
        "@>=": "AT_GT_EQ",
        ">=": "GT_EQ",
        "=<": "EQ_LT",
        "\\+": "BACKSLASH_PLUS",
        "mod": "MOD",
        "is": "IS",
        "in": "IN",
        "..": "DOT_DOT",
        "#=": "HASH_EQ",
        "#\\=": "HASH_BACKSLASH_EQ",
        "#<": "HASH_LT",
        "#>": "HASH_GT",
        "#=<": "HASH_EQ_LT",
        "#>=": "HASH_GT_EQ",
        "#<=>": "HASH_LT_EQ_GT",
        "#==>": "HASH_EQ_EQ_GT",
        "#<==": "HASH_LT_EQ_EQ",
        "#\\/": "HASH_BACKSLASH_SLASH",
    }

    return special_names.get(op, op.upper())


class Tokenizer:
    """Tokenizer for Prolog text."""

    def __init__(self):
        # Generate patterns dynamically from operator table
        patterns = _generate_tokenizer_patterns()
        # Compile patterns
        self.compiled_patterns = [(re.compile(p), t) for p, t in patterns]

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
                # Check if it looks like an operator
                if (
                    problem_token
                    and not problem_token[0].isalnum()
                    and problem_token not in ["(", ")", "[", "]", ".", "|"]
                ):
                    raise ReaderError(
                        f"Unknown operator '{problem_token}'",
                        position=pos,
                        token=problem_token,
                    )
                else:
                    raise ReaderError(
                        f"Unexpected token: '{problem_token}'",
                        position=pos,
                        token=problem_token,
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

    def eof_position(self) -> int:
        """Get position at end of input (after last token).

        Returns:
            Position immediately after the last token, or 0 if no tokens.
        """
        if self.tokens:
            last_token = self.tokens[-1]
            return last_token.position + len(last_token.value)
        return 0

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

            # First, check for postfix operator
            postfix_info = self._get_postfix_info(token)
            if postfix_info:
                precedence, assoc_type, canonical = postfix_info

                # Check precedence constraint
                if precedence > min_precedence:
                    break

                # Consume the postfix operator
                self.stream.consume()

                # Handle unsupported operators
                if not is_stage1_supported(token.value, "postfix"):
                    if self.strict_unsupported:
                        raise ReaderError(
                            f"Unsupported operator '{token.value}' in Stage 1",
                            position=token.position,
                            token=token.value,
                        )
                    else:
                        logger.warning(
                            f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)"
                        )

                # Build postfix structure
                left = Struct(token.value, (left,))

                # For xf (non-associative), don't allow chaining at same precedence
                # Update min_precedence to prevent same-precedence postfix operators
                if assoc_type == "xf":
                    # Check if there's another postfix operator at same precedence
                    next_token = self.stream.peek()
                    if next_token:
                        next_postfix_info = self._get_postfix_info(next_token)
                        if next_postfix_info and next_postfix_info[0] == precedence:
                            raise ReaderError(
                                f"xf postfix operator '{token.value}' cannot chain with '{next_token.value}' at same precedence; add parentheses",
                                position=next_token.position,
                                token=next_token.value,
                            )
                # For yf (associative), allow chaining by continuing the loop

                # Continue to check for more postfix or infix operators
                continue

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
            if not is_stage1_supported(token.value, "infix"):
                if self.strict_unsupported:
                    raise ReaderError(
                        f"Unsupported operator '{token.value}' in Stage 1",
                        position=token.position,
                        token=token.value,
                    )
                else:
                    logger.warning(
                        f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)"
                    )

            # Determine right-side precedence based on associativity
            if assoc_type == "xfx":
                # Non-associative - right side must be strictly less precedence
                next_min = precedence - 1
            elif assoc_type == "yfx":
                # Left-associative - right side must be strictly less
                next_min = precedence - 1
            else:  # xfy
                # Right-associative - right side can be same precedence
                next_min = precedence

            # Parse right side
            saved_op = token  # Save operator for better error messages
            try:
                right = self.parse_term(next_min)
            except ReaderError as e:
                # If end of input after operator, give better message
                if "end of input" in str(e).lower():
                    raise ReaderError(
                        f"Missing right argument for operator '{saved_op.value}'",
                        position=saved_op.position,
                        token=saved_op.value,
                    )
                raise

            # For xfx operators, check if the right side tries to use the same operator
            # This happens when we have X = Y = Z
            if assoc_type == "xfx":
                # Check if there's another operator of same precedence waiting
                next_token = self.stream.peek()
                if next_token:
                    next_op_info = self._get_infix_info(next_token)
                    if next_op_info and next_op_info[0] == precedence:
                        # Same precedence operator trying to chain - this is an error
                        raise ReaderError(
                            f"xfx operator '{next_token.value}' is non-chainable; add parentheses",
                            position=next_token.position,
                            token=next_token.value,
                        )

            # Build structure
            functor = token.value
            left = Struct(functor, (left, right))

        return left

    def parse_prefix(self) -> Term:
        """Parse a prefix expression or primary term."""
        token = self.stream.peek()

        if not token:
            # Position at end of input
            raise ReaderError(
                "Unexpected end of input; expected a term",
                position=self.stream.eof_position(),
            )

        # Check for prefix operator
        op_info = self._get_prefix_info(token)
        if op_info:
            precedence, assoc_type, canonical = op_info
            self.stream.consume()

            # Special case: negative/positive numeral
            if token.value in ["-", "+"]:
                next_token = self.stream.peek()
                if next_token and next_token.type == "SIGNED_INT":
                    # Check if it's a literal (no space between) and no higher precedence operator follows
                    if token.position + len(token.value) == next_token.position:
                        # Look ahead for power operator
                        saved_pos = self.stream.pos
                        self.stream.consume()  # Skip the number
                        following = self.stream.peek()
                        self.stream.pos = saved_pos  # Restore position

                        # If there's a ** operator following, treat - as operator not literal
                        if following and following.type == "DOUBLE_STAR":
                            pass  # Don't consume, treat as operator
                        else:
                            # It's a literal number
                            self.stream.consume()
                            value = int(next_token.value)
                            if token.value == "-":
                                return Int(-abs(value))
                            else:
                                return Int(abs(value))

            # Handle unsupported operators
            if not is_stage1_supported(token.value, "prefix"):
                if self.strict_unsupported:
                    raise ReaderError(
                        f"Unsupported operator '{token.value}' in Stage 1",
                        position=token.position,
                        token=token.value,
                    )
                else:
                    logger.warning(
                        f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)"
                    )

            # For fy, allow same precedence; for fx, require less
            if assoc_type == "fy":
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
            # Position at end of input
            raise ReaderError(
                "Unexpected end of input; expected a term",
                position=self.stream.eof_position(),
            )

        # Parenthesized expression
        if token.type == "LPAREN":
            self.stream.consume()
            term = self.parse_term()
            next_token = self.stream.consume()
            if not next_token or next_token.type != "RPAREN":
                # Position at end of input or at the unexpected token
                pos = (
                    self.stream.eof_position()
                    if self.stream.at_end()
                    else self.stream.get_position()
                )
                raise ReaderError(
                    "Expected closing parenthesis ')'",
                    position=pos,
                    token=next_token.value if next_token else None,
                )
            return term

        # List
        if token.type == "LBRACKET":
            return self.parse_list()

        # Variable
        if token.type == "VARIABLE":
            self.stream.consume()
            var_id = self.stream.get_var_id(token.value)
            return Var(var_id, token.value)

        # Integer
        if token.type == "SIGNED_INT":
            self.stream.consume()
            return Int(int(token.value))

        # Atom or structure
        if token.type in ["ATOM", "QUOTED_ATOM"]:
            self.stream.consume()

            # Process quoted atom
            if token.type == "QUOTED_ATOM":
                name = token.value[1:-1]  # Remove quotes
                name = name.replace("\\'", "'")
                name = name.replace("\\n", "\n")
                name = name.replace("\\t", "\t")
                name = name.replace("\\\\", "\\")
            else:
                name = token.value

            # Check for structure
            next_token = self.stream.peek()
            if next_token and next_token.type == "LPAREN":
                self.stream.consume()
                args = self.parse_term_list()
                closing = self.stream.consume()
                if not closing or closing.type != "RPAREN":
                    pos = (
                        self.stream.eof_position()
                        if self.stream.at_end()
                        else self.stream.get_position()
                    )
                    raise ReaderError(
                        f"Expected closing parenthesis ')' for structure '{name}'",
                        position=pos,
                        token=closing.value if closing else None,
                    )
                if not args:
                    raise ReaderError(
                        f"Empty parentheses not allowed - use plain atom '{name}' instead",
                        position=token.position,
                        token=name,
                    )
                return Struct(name, tuple(args))

            return Atom(name)

        # Word operators that can appear as atoms
        if token.type in ["MOD", "IS"]:
            self.stream.consume()

            # Check if it's being used as a functor
            next_token = self.stream.peek()
            if next_token and next_token.type == "LPAREN":
                self.stream.consume()
                args = self.parse_term_list()
                closing = self.stream.consume()
                if not closing or closing.type != "RPAREN":
                    pos = (
                        self.stream.eof_position()
                        if self.stream.at_end()
                        else self.stream.get_position()
                    )
                    raise ReaderError(
                        f"Expected closing parenthesis ')' for structure '{token.value}'",
                        position=pos,
                        token=closing.value if closing else None,
                    )
                if not args:
                    raise ReaderError(
                        f"Empty parentheses not allowed - use plain atom '{token.value}' instead",
                        position=token.position,
                        token=token.value,
                    )
                return Struct(token.value, tuple(args))

            return Atom(token.value)

        raise ReaderError(
            f"Unexpected token: {token.type} '{token.value}'",
            position=token.position,
            token=token.value,
        )

    def parse_list(self) -> PrologList:
        """Parse a list."""
        opening = self.stream.consume()
        if opening.type != "LBRACKET":
            raise ReaderError("Expected opening bracket")

        # Empty list
        next_token = self.stream.peek()
        if next_token and next_token.type == "RBRACKET":
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
                raise ReaderError(
                    "Unexpected end of list; expected ']' or more elements",
                    position=self.stream.eof_position(),
                )

            if next_token.type == "COMMA":
                self.stream.consume()
                continue
            elif next_token.type == "PIPE":
                self.stream.consume()
                tail = self.parse_term_arg()
                next_token = self.stream.peek()
                if not next_token or next_token.type != "RBRACKET":
                    pos = (
                        self.stream.eof_position()
                        if self.stream.at_end()
                        else self.stream.get_position()
                    )
                    raise ReaderError(
                        "Expected closing bracket ']' after list tail",
                        position=pos,
                        token=next_token.value if next_token else None,
                    )
                self.stream.consume()
                break
            elif next_token.type == "RBRACKET":
                self.stream.consume()
                tail = Atom("[]")
                break
            else:
                raise ReaderError(
                    f"Unexpected token in list: '{next_token.value}'; expected ',' or ']'",
                    position=next_token.position,
                    token=next_token.value,
                )

        return PrologList(tuple(elements), tail)

    def parse_term_list(self) -> List[Term]:
        """Parse comma-separated list of terms (for structures and goals)."""
        terms = []

        # Handle empty argument list
        next_token = self.stream.peek()
        if next_token and next_token.type == "RPAREN":
            return terms

        while True:
            # Parse term but stop at commas - they're separators here, not operators
            term = self.parse_term_arg()
            terms.append(term)

            next_token = self.stream.peek()
            if not next_token:
                break

            if next_token.type == "COMMA":
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
                if token.type == "COMMA":
                    break

                # First, check for postfix operator
                postfix_info = self._get_postfix_info(token)
                if postfix_info:
                    precedence, assoc_type, canonical = postfix_info

                    # Check precedence constraint
                    if precedence > min_precedence:
                        break

                    # Consume the postfix operator
                    self.stream.consume()

                    # Handle unsupported operators
                    if not is_stage1_supported(token.value, "postfix"):
                        if self.strict_unsupported:
                            raise ReaderError(
                                f"Unsupported operator '{token.value}' in Stage 1",
                                position=token.position,
                                token=token.value,
                            )
                        else:
                            logger.warning(
                                f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)"
                            )

                    # Build postfix structure
                    left = Struct(token.value, (left,))

                    # For xf (non-associative), check for chaining at same precedence
                    if assoc_type == "xf":
                        next_token = self.stream.peek()
                        if next_token and next_token.type != "COMMA":
                            next_postfix_info = self._get_postfix_info(next_token)
                            if next_postfix_info and next_postfix_info[0] == precedence:
                                raise ReaderError(
                                    f"xf postfix operator '{token.value}' cannot chain with '{next_token.value}' at same precedence; add parentheses",
                                    position=next_token.position,
                                    token=next_token.value,
                                )

                    # Continue to check for more postfix or infix operators
                    continue

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
                if not is_stage1_supported(token.value, "infix"):
                    if self.strict_unsupported:
                        raise ReaderError(
                            f"Unsupported operator '{token.value}' in Stage 1",
                            position=token.position,
                            token=token.value,
                        )
                    else:
                        logger.warning(
                            f"Unsupported operator '{token.value}' used (will parse but may fail at runtime)"
                        )

                # Determine right-side precedence based on associativity
                if assoc_type == "xfx":
                    next_min = precedence - 1
                elif assoc_type == "yfx":
                    next_min = precedence - 1
                else:  # xfy
                    next_min = precedence

                # Parse right side (recursively with no-comma version)
                self.parse_term = parse_term_no_comma
                saved_op = token  # Save operator for better error messages
                try:
                    right = self.parse_term(next_min)
                except ReaderError as e:
                    # If end of input after operator, give better message
                    if "end of input" in str(e).lower():
                        raise ReaderError(
                            f"Missing right argument for operator '{saved_op.value}'",
                            position=saved_op.position,
                            token=saved_op.value,
                        )
                    raise

                # For xfx operators, check if trying to chain
                if assoc_type == "xfx":
                    next_token = self.stream.peek()
                    if next_token and next_token.type != "COMMA":
                        next_op_info = self._get_infix_info(next_token)
                        if next_op_info and next_op_info[0] == precedence:
                            raise ReaderError(
                                f"xfx operator '{next_token.value}' cannot chain; add parentheses",
                                position=next_token.position,
                                token=next_token.value,
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
        """Get operator info for infix position.

        Uses dynamic lookup from operator table instead of hardcoded token types.
        This allows custom operators to be recognized without code changes.
        """
        # Try to get operator info for this token's value
        # This works for any operator in the table, not just hardcoded ones
        return get_operator_info(token.value, "infix")

    def _get_prefix_info(self, token: Token) -> Optional[Tuple[int, str, str]]:
        """Get operator info for prefix position.

        Uses dynamic lookup from operator table instead of hardcoded token types.
        This allows custom operators to be recognized without code changes.
        """
        # Try to get operator info for this token's value
        # This works for any operator in the table, not just hardcoded ones
        return get_operator_info(token.value, "prefix")

    def _get_postfix_info(self, token: Token) -> Optional[Tuple[int, str, str]]:
        """Get operator info for postfix position.

        Uses dynamic lookup from operator table.
        Returns (precedence, associativity, canonical) tuple if found.
        """
        return get_operator_info(token.value, "postfix")


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
                          instead of just logging warnings (default: False).
                          Useful for CI to flip warnings → errors without code changes.
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
            if tokens and tokens[-1].type == "DOT":
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
                raise ReaderError(
                    f"Unexpected token after expression: '{leftover.value}'",
                    position=leftover.position,
                    token=leftover.value,
                )

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
            if not tokens or tokens[-1].type != "DOT":
                raise ReaderError("Clause must end with period")

            # Find :- separator if present
            colon_minus_idx = None
            for i, token in enumerate(tokens):
                if token.type == "COLON_MINUS":
                    colon_minus_idx = i
                    break

            # Remove trailing DOT
            tokens = tokens[:-1]

            if colon_minus_idx is not None:
                # Rule: head :- body
                head_tokens = tokens[:colon_minus_idx]
                body_tokens = tokens[colon_minus_idx + 1 :]

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
            if tokens and tokens[0].type == "QUESTION_MINUS":
                tokens = tokens[1:]
            else:
                raise ReaderError("Query must start with ?-")

            # Check for trailing DOT
            if not tokens or tokens[-1].type != "DOT":
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
        if isinstance(term, Struct) and term.functor == ",":
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

        # Parse clauses by finding periods at the top level (not inside parens/brackets)
        current_clause = []
        paren_depth = 0
        bracket_depth = 0
        in_quoted_atom = False
        escape_next = False

        i = 0
        while i < len(text):
            char = text[i]

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
            if in_quoted_atom and char == "\\":
                escape_next = True
                current_clause.append(char)
                i += 1
                continue

            # Skip tracking inside quoted atoms
            if in_quoted_atom:
                current_clause.append(char)
                i += 1
                continue

            # Handle comments (% starts a line comment when not in quoted atom)
            if char == "%":
                # Skip to end of line
                while i < len(text) and text[i] != "\n":
                    i += 1
                # Don't skip the newline itself
                continue

            # Track parentheses and brackets
            if char == "(":
                paren_depth += 1
            elif char == ")":
                paren_depth -= 1
            elif char == "[":
                bracket_depth += 1
            elif char == "]":
                bracket_depth -= 1
            elif char == "." and paren_depth == 0 and bracket_depth == 0:
                # Check if this is part of a .. operator
                if i + 1 < len(text) and text[i + 1] == ".":
                    # It's a .. operator, not a clause terminator
                    current_clause.append(char)  # Add first .
                    current_clause.append(text[i + 1])  # Add second .
                    i += 2  # Skip both dots
                    continue
                # Found a clause terminator at top level
                current_clause.append(char)
                clause_text = "".join(current_clause).strip()
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
        remaining = "".join(current_clause).strip()
        if remaining:
            raise ReaderError(f"Incomplete clause at end of program: {remaining[:50]}")

        return clauses
