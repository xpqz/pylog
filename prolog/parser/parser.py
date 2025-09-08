"""Parser module for converting Prolog text to AST - Issue #10.

This module provides functions to parse Prolog text into AST objects
using the Lark grammar defined in grammar.lark.
"""

import pathlib
from typing import List, Dict, Any
from lark import Lark, Tree, Token, Transformer
from lark.exceptions import UnexpectedCharacters, UnexpectedToken

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause


class ParseError(Exception):
    """Exception raised for parse errors."""

    def __init__(self, message: str, line: int = None, column: int = None):
        super().__init__(message)
        self.message = message
        self.line = line
        self.column = column

    def __str__(self):
        if self.line is not None and self.column is not None:
            return (
                f"ParseError at line {self.line}, column {self.column}: {self.message}"
            )
        elif self.line is not None:
            return f"ParseError at line {self.line}: {self.message}"
        else:
            return f"ParseError: {self.message}"


class PrologTransformer(Transformer):
    """Transform Lark parse tree to Prolog AST.

    Variable consistency: Variables with the same name within a single
    parse get the same ID. Anonymous variables (_) each get unique IDs.
    """

    def __init__(self):
        super().__init__()
        self.var_map: Dict[str, int] = {}
        self.next_var_id = 0

    def _get_var_id(self, name: str) -> int:
        """Get or create variable ID for name.

        Anonymous variables (_) always get fresh IDs.
        Named variables get consistent IDs within the parse.
        """
        if name == "_":
            # Each _ gets a unique ID
            var_id = self.next_var_id
            self.next_var_id += 1
            return var_id

        if name not in self.var_map:
            self.var_map[name] = self.next_var_id
            self.next_var_id += 1
        return self.var_map[name]

    # Terminal transformations
    def ATOM(self, token: Token) -> Atom:
        """Transform ATOM token to Atom term."""
        return Atom(token.value)

    def QUOTED_ATOM(self, token: Token) -> Atom:
        """Transform quoted atom, processing escape sequences."""
        # Remove surrounding quotes
        content = token.value[1:-1]

        # Process escape sequences
        content = content.replace("\\'", "'")
        content = content.replace("\\n", "\n")
        content = content.replace("\\t", "\t")
        content = content.replace("\\\\", "\\")

        return Atom(content)

    def SIGNED_INT(self, token: Token) -> Int:
        """Transform integer token to Int term."""
        return Int(int(token.value))

    def VARIABLE(self, token: Token) -> Var:
        """Transform variable token to Var term."""
        name = token.value
        var_id = self._get_var_id(name)
        return Var(var_id, name)

    # Non-terminal transformations
    def atom(self, children):
        """Atom can be regular or quoted."""
        return children[0]

    def integer(self, children):
        """Integer term."""
        return children[0]

    def variable(self, children):
        """Variable term."""
        return children[0]

    def empty_list(self, children):
        """Empty list []."""
        return PrologList((), Atom("[]"))

    def proper_list(self, children):
        """Proper list [a,b,c]."""
        items = children[0]  # term_list
        return PrologList(tuple(items), Atom("[]"))

    def list_with_tail(self, children):
        """List with tail [H|T] or [a,b|T]."""
        if len(children) == 1:
            # No term_list, just tail: should not happen with our grammar
            tail = children[0]
            return PrologList((), tail)
        else:
            # Has term_list and tail
            items = children[0]  # term_list
            tail = children[1]
            return PrologList(tuple(items), tail)

    def term_list(self, children):
        """List of terms separated by commas."""
        return list(children)

    def structure(self, children):
        """Structure foo(args)."""
        functor = children[0]
        args = children[1]  # term_list
        if not isinstance(functor, Atom):
            # Should not happen with our grammar
            raise ParseError(f"Invalid functor: {functor}")
        return Struct(functor.name, tuple(args))

    def term(self, children):
        """A term is atom, integer, variable, list, or structure."""
        return children[0]

    def callable(self, children):
        """Callable term for clause heads (atom or structure)."""
        return children[0]

    def fact(self, children):
        """Fact: callable followed by period."""
        head = children[0]
        return Clause(head, [])

    def rule(self, children):
        """Rule: callable :- goal_list ."""
        head = children[0]
        body = children[1]  # goal_list
        return Clause(head, body)

    def goal_list(self, children):
        """List of goals separated by commas."""
        return list(children)

    def clause(self, children):
        """Clause is either fact or rule."""
        return children[0]

    def query(self, children):
        """Query: ?- goal_list ."""
        return children[0]  # Return the goal list

    def directive(self, children):
        """Directive: :- goal_list ."""
        return children[0]  # Return the goal list

    def program(self, children):
        """Program is a sequence of clauses."""
        # All children should be clauses now that grammar restricts it
        return list(children)


# Load grammar once
GRAMMAR_PATH = pathlib.Path(__file__).parent / "grammar.lark"
_grammar_cache = None


def _get_grammar():
    """Get cached grammar instance."""
    global _grammar_cache
    if _grammar_cache is None:
        _grammar_cache = GRAMMAR_PATH.read_text()
    return _grammar_cache


def parse_term(text: str) -> Any:
    """Parse a single Prolog term.

    Args:
        text: Prolog term text

    Returns:
        AST term (Atom, Int, Var, Struct, or List)

    Raises:
        ParseError: If the text cannot be parsed as a term
    """
    try:
        parser = Lark(_get_grammar(), start="term", parser="lalr")
        tree = parser.parse(text)
        transformer = PrologTransformer()
        return transformer.transform(tree)
    except (UnexpectedCharacters, UnexpectedToken) as e:
        line = getattr(e, "line", None)
        column = getattr(e, "column", None)

        # Try to extract useful context
        context = ""
        if hasattr(e, "get_context"):
            context = e.get_context(text)
        elif "expected" in str(e).lower() or "unexpected" in str(e).lower():
            context = str(e)
        else:
            context = f"Failed to parse: {text[:50]}"

        raise ParseError(context, line, column)


def parse_clause(text: str) -> Clause:
    """Parse a single Prolog clause (fact or rule).

    Args:
        text: Clause text ending with period

    Returns:
        Clause object

    Raises:
        ParseError: If the text cannot be parsed as a clause
    """
    try:
        parser = Lark(_get_grammar(), start="clause", parser="lalr")
        tree = parser.parse(text)
        transformer = PrologTransformer()
        return transformer.transform(tree)
    except (UnexpectedCharacters, UnexpectedToken) as e:
        line = getattr(e, "line", None)
        column = getattr(e, "column", None)

        # Provide context about what went wrong
        if "42." in text:
            msg = "Integer cannot be a clause head"
        elif "[" in text and "]." in text:
            msg = "List cannot be a clause head"
        elif text.strip() and not text.strip().endswith("."):
            msg = "Clause must end with period"
        else:
            msg = str(e)

        raise ParseError(msg, line, column)


def parse_query(text: str) -> List[Any]:
    """Parse a Prolog query.

    Args:
        text: Query text starting with ?- and ending with period

    Returns:
        List of goal terms

    Raises:
        ParseError: If the text cannot be parsed as a query
    """
    try:
        parser = Lark(_get_grammar(), start="query", parser="lalr")
        tree = parser.parse(text)
        transformer = PrologTransformer()
        return transformer.transform(tree)
    except (UnexpectedCharacters, UnexpectedToken) as e:
        line = getattr(e, "line", None)
        column = getattr(e, "column", None)

        # Provide helpful context
        if not text.strip().startswith("?-"):
            msg = "Query must start with ?-"
        elif not text.strip().endswith("."):
            msg = "Query must end with period"
        else:
            msg = str(e)

        raise ParseError(msg, line, column)


def parse_program(text: str) -> List[Clause]:
    """Parse a complete Prolog program.

    Programs can only contain clauses (facts and rules).
    Queries and directives are not allowed in programs.

    Args:
        text: Program text with multiple clauses

    Returns:
        List of Clause objects in source order

    Raises:
        ParseError: If the text cannot be parsed as a program
    """
    if not text.strip():
        return []

    # Remove comments but preserve line structure for error reporting
    lines = text.split("\n")
    processed_lines = []
    for line in lines:
        # Remove line comments but keep the newline
        if "%" in line:
            line = line[: line.index("%")]
        processed_lines.append(line)
    processed_text = "\n".join(processed_lines)

    if not processed_text.strip():
        return []

    try:
        parser = Lark(_get_grammar(), start="program", parser="lalr")
        tree = parser.parse(processed_text)
        transformer = PrologTransformer()
        result = transformer.transform(tree)

        # Handle case where single clause is returned
        if isinstance(result, Clause):
            return [result]
        return result

    except (UnexpectedCharacters, UnexpectedToken) as e:
        line = getattr(e, "line", None)
        column = getattr(e, "column", None)

        # Check for common issues
        if "?-" in text:
            raise ParseError("Queries not allowed in programs", line, column)
        elif text.strip().startswith(":-"):
            raise ParseError("Directives not allowed in programs", line, column)

        # Try to provide context
        context = str(e)
        if "expected" in context.lower() or "unexpected" in context.lower():
            raise ParseError(context, line, column)
        else:
            raise ParseError(f"Parse error in program", line, column)
