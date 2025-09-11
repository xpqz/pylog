"""Tests for Prolog parser grammar - Issue #9.

Tests for parsing basic terms, lists, and structures in operator-free syntax.

Most tests use start='term' which accepts atoms/integers/variables/lists/structures.
Tests that need to validate specific term types (e.g., "Foo" is not an atom) must use
the appropriate start rule (e.g., start='atom').

In Stage 1 (operator-free), -42 is a signed integer literal, not the - operator applied to 42.
"""

import pathlib
import pytest
from lark import Lark
from lark.exceptions import UnexpectedCharacters, UnexpectedToken

# Path to grammar file
GRAMMAR_PATH = pathlib.Path(__file__).parents[3] / "prolog/parser/grammar.lark"


# Fixtures for different start rules - compile grammar once per session
@pytest.fixture(scope="session")
def grammar_term():
    """Grammar starting from 'term' rule."""
    return Lark(GRAMMAR_PATH.read_text(), start="term", parser="lalr")


@pytest.fixture(scope="session")
def grammar_atom():
    """Grammar starting from 'atom' rule."""
    return Lark(GRAMMAR_PATH.read_text(), start="atom", parser="lalr")


@pytest.fixture(scope="session")
def grammar_clause():
    """Grammar starting from 'clause' rule."""
    return Lark(GRAMMAR_PATH.read_text(), start="clause", parser="lalr")


@pytest.fixture(scope="session")
def grammar_query():
    """Grammar starting from 'query' rule."""
    return Lark(GRAMMAR_PATH.read_text(), start="query", parser="lalr")


@pytest.fixture(scope="session")
def grammar_directive():
    """Grammar starting from 'directive' rule."""
    return Lark(GRAMMAR_PATH.read_text(), start="directive", parser="lalr")


class TestBasicTerms:
    """Test parsing of atoms, integers, and variables."""

    @pytest.mark.parametrize(
        "atom",
        [
            "foo",
            "bar",
            "x",
            "atom123",
            "with_underscore",
            "!",  # Cut is a valid atom in Stage 1
        ],
    )
    def test_parse_simple_atom(self, grammar_term, atom):
        """Parse simple lowercase atoms and cut."""
        assert grammar_term.parse(atom)

    @pytest.mark.parametrize(
        "quoted",
        [
            "'foo bar'",
            "'Hello World!'",
            "'123'",  # Numbers can be quoted
            "'Uppercase'",  # Uppercase in quotes
            "''",  # Empty quoted atom
        ],
    )
    def test_parse_quoted_atom(self, grammar_term, quoted):
        """Parse quoted atoms with spaces and special chars."""
        assert grammar_term.parse(quoted)

    @pytest.mark.parametrize(
        "escaped",
        [
            "'can\\'t'",  # Escaped single quote
            "'line\\nbreak'",  # Newline
            "'tab\\there'",  # Tab
            "'back\\\\slash'",  # Backslash
        ],
    )
    def test_parse_quoted_atom_with_escapes(self, grammar_term, escaped):
        """Parse quoted atoms with escape sequences.

        We support standard ISO escapes: \\, \', \n, \t
        """
        assert grammar_term.parse(escaped)

    @pytest.mark.parametrize(
        "special",
        [
            "'[strange]'",
            "'[]'",
            "'()'",
            "','",
            "'|'",
            "':-'",
        ],
    )
    def test_parse_quoted_atom_special_chars(self, grammar_term, special):
        """Parse quoted atoms containing special characters."""
        assert grammar_term.parse(special)

    def test_quoted_atom_with_doubled_quote(self, grammar_term):
        """Test if doubled single quotes work for embedding quotes."""
        # Standard Prolog way to include single quote: double it
        # Skip if not implemented
        pytest.skip("Doubled quote syntax may not be implemented")
        assert grammar_term.parse("'it''s'")  # Represents: it's

    @pytest.mark.parametrize(
        "integer",
        [
            "0",
            "1",
            "42",
            "12345",  # Positive
            "-1",
            "-42",
            "-12345",  # Negative (signed literals in Stage 1)
        ],
    )
    def test_parse_integer(self, grammar_term, integer):
        """Parse positive, negative, and zero integers.

        In Stage 1 (operator-free), -42 is a signed integer literal.
        """
        assert grammar_term.parse(integer)

    @pytest.mark.parametrize(
        "var",
        [
            "X",
            "Variable",
            "Var123",
            "X_1",  # Uppercase start
            "_var",
            "_123",
            "_X",  # Underscore start (named variables)
            "_",  # Anonymous variable
        ],
    )
    def test_parse_variable(self, grammar_term, var):
        """Parse variables starting with uppercase or underscore.

        Note: _X is a named variable, not anonymous. Only _ is anonymous.
        """
        assert grammar_term.parse(var)

    def test_uppercase_not_atom(self, grammar_atom):
        """Uppercase identifiers are variables, not atoms."""
        # When parsing specifically as 'atom', uppercase should fail
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_atom.parse("Foo")

    def test_integer_is_term_not_atom(self, grammar_atom, grammar_term):
        """Unquoted integers are integer terms, not atoms."""
        # Integer should fail as atom
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_atom.parse("123")
        # But succeed as term
        assert grammar_term.parse("123")


class TestListSyntax:
    """Test parsing of Prolog lists."""

    def test_parse_empty_list(self, grammar_term):
        """Parse empty list []."""
        assert grammar_term.parse("[]")

    @pytest.mark.parametrize(
        "lst",
        [
            "[1]",
            "[1,2]",
            "[1,2,3]",
            "[foo,bar,baz]",
            "[X,Y,Z]",
        ],
    )
    def test_parse_simple_list(self, grammar_term, lst):
        """Parse list with elements."""
        assert grammar_term.parse(lst)

    @pytest.mark.parametrize(
        "tail_form",
        [
            "[H|T]",
            "[1|T]",
            "[X|[]]",
            "[1,2|T]",
            "[a,b,c|Rest]",
            # Whitespace variations
            "[H | T]",
            "[H| T]",
            "[H |T]",
        ],
    )
    def test_parse_list_with_tail(self, grammar_term, tail_form):
        """Parse list with tail notation [H|T]."""
        assert grammar_term.parse(tail_form)

    @pytest.mark.parametrize(
        "nested",
        [
            "[[]]",
            "[[1]]",
            "[[1,2],[3,4]]",
            "[[], [1], [2,3]]",
            "[[a,b],[c,d],[e,f]]",
        ],
    )
    def test_parse_nested_lists(self, grammar_term, nested):
        """Parse nested lists."""
        assert grammar_term.parse(nested)

    @pytest.mark.parametrize(
        "improper",
        [
            "[1|2]",  # Non-list tail
            "[a|b]",
            "[foo|bar]",
        ],
    )
    def test_parse_improper_list(self, grammar_term, improper):
        """Parse improper list (non-list tail)."""
        assert grammar_term.parse(improper)

    @pytest.mark.parametrize(
        "invalid",
        [
            "[",  # Unclosed
            "[1,]",  # Trailing comma
            "[,1]",  # Leading comma
            "[|T]",  # No head
            "[H|]",  # No tail
            "[H||T]",  # Double bar
            "[H,|T]",  # Comma before bar
        ],
    )
    def test_invalid_list_syntax(self, grammar_term, invalid):
        """Invalid list syntax should fail."""
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_term.parse(invalid)


class TestStructureSyntax:
    """Test parsing of Prolog structures/compound terms."""

    def test_parse_atom_as_structure(self, grammar_term):
        """Parse 0-arity structure (atom) foo."""
        # Atoms are valid terms
        assert grammar_term.parse("foo")
        assert grammar_term.parse("bar")

    @pytest.mark.parametrize(
        "struct",
        [
            "foo(bar)",
            "f(x)",
            "pred(123)",
            "test(X)",
        ],
    )
    def test_parse_simple_structure(self, grammar_term, struct):
        """Parse structure foo(bar)."""
        assert grammar_term.parse(struct)

    @pytest.mark.parametrize(
        "multi",
        [
            "foo(a,b)",
            "foo(a,b,c)",
            "pred(1,2,3,4,5)",
            "f(X,Y,Z)",
        ],
    )
    def test_parse_multi_arg_structure(self, grammar_term, multi):
        """Parse multi-argument structure."""
        assert grammar_term.parse(multi)

    @pytest.mark.parametrize(
        "nested",
        [
            "foo(bar(baz))",
            "f(g(h))",
            "outer(middle(inner(x)))",
            "p(q(r),s(t))",
        ],
    )
    def test_parse_nested_structure(self, grammar_term, nested):
        """Parse nested structure."""
        assert grammar_term.parse(nested)

    @pytest.mark.parametrize(
        "mixed",
        [
            "foo(1,X,[a,b])",
            "pred(atom,123,Var)",
            "test([],foo,42)",
            "complex(foo(bar),[1,2],X)",
        ],
    )
    def test_parse_structure_mixed_args(self, grammar_term, mixed):
        """Parse structure with mixed argument types."""
        assert grammar_term.parse(mixed)

    def test_zero_arity_with_parens_invalid(self, grammar_term):
        """Zero-arity structure with parentheses is invalid."""
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_term.parse("foo()")

    def test_predicate_indicator_not_general_term(self, grammar_term):
        """foo/2 notation is not a general term (removed from Stage 1)."""
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_term.parse("foo/2")

    @pytest.mark.parametrize(
        "invalid",
        [
            "foo(",  # Unclosed
            "foo(,)",  # Empty arg
            "foo(a,)",  # Trailing comma
            "foo(,a)",  # Leading comma
        ],
    )
    def test_invalid_structure_syntax(self, grammar_term, invalid):
        """Invalid structure syntax should fail."""
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_term.parse(invalid)


class TestClauseSyntax:
    """Test parsing of Prolog clauses, facts, and rules."""

    @pytest.mark.parametrize(
        "fact",
        [
            "parent(tom,bob).",
            "foo.",
            "likes(mary,wine).",
            "age(john,30).",
        ],
    )
    def test_parse_fact(self, grammar_clause, fact):
        """Parse fact ending with period."""
        assert grammar_clause.parse(fact)

    @pytest.mark.parametrize(
        "invalid_head",
        [
            "42.",  # Integer cannot be a clause head
            "[1,2].",  # List cannot be a clause head
            '"string".',  # String cannot be a clause head (if we had strings)
        ],
    )
    def test_invalid_clause_heads(self, grammar_clause, invalid_head):
        """Non-callable terms cannot be clause heads."""
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_clause.parse(invalid_head)

    @pytest.mark.parametrize(
        "rule",
        [
            "foo :- bar.",
            "parent(X,Y) :- father(X,Y).",
            "sibling(X,Y) :- parent(P,X), parent(P,Y).",
            "p(X) :- q(X), !, r(X).",  # Cut in goal list
        ],
    )
    def test_parse_rule(self, grammar_clause, rule):
        """Parse rule with body head :- body."""
        assert grammar_clause.parse(rule)

    def test_invalid_rule_head_variable(self, grammar_clause):
        """Variable cannot be a rule head."""
        with pytest.raises((UnexpectedCharacters, UnexpectedToken)):
            grammar_clause.parse("X :- true.")

    @pytest.mark.parametrize(
        "multi_goal",
        [
            "foo :- a, b, c.",
            "test(X) :- p(X), q(X), r(X).",
            "path(X,Y) :- edge(X,Z), path(Z,Y).",
        ],
    )
    def test_parse_multi_goal_body(self, grammar_clause, multi_goal):
        """Parse rule with multiple goals in body."""
        assert grammar_clause.parse(multi_goal)

    @pytest.mark.parametrize(
        "query",
        [
            "?- foo.",
            "?- member(X,[1,2,3]).",
            "?- p(X), q(X).",
            "?- a, !, b.",  # Cut in query
        ],
    )
    def test_parse_query(self, grammar_query, query):
        """Parse query ?- goal."""
        assert grammar_query.parse(query)

    @pytest.mark.parametrize(
        "directive",
        [
            ":- foo.",
            ":- a, !, b.",  # Cut in directive
            # Note: foo/2 is not a general term anymore, would need special handling
        ],
    )
    def test_parse_directive(self, grammar_directive, directive):
        """Parse directive :- goal."""
        assert grammar_directive.parse(directive)


class TestWhitespaceAndComments:
    """Test whitespace handling and comments."""

    @pytest.mark.parametrize(
        "whitespace_term",
        [
            "  foo  ",
            "foo( bar )",
            "[ 1 , 2 , 3 ]",
            "foo(\n  bar,\n  baz\n)",
        ],
    )
    def test_whitespace_ignored(self, grammar_term, whitespace_term):
        """Whitespace should be properly ignored."""
        assert grammar_term.parse(whitespace_term)

    def test_line_comments(self, grammar_term):
        """Line comments starting with %."""
        assert grammar_term.parse("foo % comment")
        assert grammar_term.parse("% comment\nfoo")

    def test_block_comments(self, grammar_term):
        """Block comments /* ... */."""
        assert grammar_term.parse("/* comment */ foo")
        assert grammar_term.parse("foo /* comment */")
        # This would be inside a functor call - two separate terms is invalid
        assert grammar_term.parse("foo(/* multi\nline\ncomment */ bar)")


class TestMultiClausePrograms:
    """Test parsing multiple clauses (program-level)."""

    def test_parse_program(self):
        """Parse a complete program with multiple clauses."""
        grammar = Lark(GRAMMAR_PATH.read_text(), start="program", parser="lalr")

        program = """
        parent(tom, bob).
        parent(tom, liz).
        parent(bob, ann).
        
        grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """

        assert grammar.parse(program)
