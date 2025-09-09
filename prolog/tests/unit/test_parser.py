"""Tests for the Prolog parser module - Issue #10.

The parser module converts text to AST using the Lark grammar.
It provides functions for parsing terms, clauses, queries, and programs.

Stage 1 Policy Notes:
- Operator-free syntax only (-5 is a signed integer literal, not -(5))
- Programs accept only clauses (facts/rules), not queries or directives
- Quoted atoms can be functors (ISO-compliant)
- Escape sequences supported: \', \\, \n, \t
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.ast.clauses import Clause
from prolog.parser.parser import (
    parse_term,
    parse_clause,
    parse_query,
    parse_program,
    ParseError,
)


class TestParseTerm:
    """Test parsing individual terms to AST."""

    @pytest.mark.parametrize(
        "text,expected",
        [
            ("foo", Atom("foo")),
            ("bar", Atom("bar")),
            ("!", Atom("!")),
            ("'foo bar'", Atom("foo bar")),
            ("'123'", Atom("123")),
            ("42", Int(42)),
            ("0", Int(0)),
            ("-5", Int(-5)),
            ("[]", List((), Atom("[]"))),
        ],
    )
    def test_parse_term_basic(self, text, expected):
        """Test parsing of basic terms."""
        assert parse_term(text) == expected

    def test_parse_atom(self):
        """Parse simple atom."""
        result = parse_term("foo")
        assert result == Atom("foo")

    def test_parse_quoted_atom(self):
        """Parse quoted atom with spaces."""
        result = parse_term("'foo bar'")
        assert result == Atom("foo bar")

    def test_parse_quoted_atom_with_escapes(self):
        """Parse quoted atom with escape sequences."""
        result = parse_term("'can\\'t'")
        assert result == Atom("can't")

        result = parse_term("'line\\nbreak'")
        assert result == Atom("line\nbreak")

        result = parse_term("'tab\\there'")
        assert result == Atom("tab\there")

        result = parse_term("'back\\\\slash'")
        assert result == Atom("back\\slash")

    def test_parse_cut_atom(self):
        """Parse cut (!) as atom."""
        result = parse_term("!")
        assert result == Atom("!")

    def test_parse_integer(self):
        """Parse integers."""
        assert parse_term("42") == Int(42)
        assert parse_term("0") == Int(0)
        assert parse_term("-5") == Int(-5)

    def test_parse_variable(self):
        """Parse variables."""
        # Variables get unique IDs, check type and name
        result = parse_term("X")
        assert isinstance(result, Var)
        assert result.hint == "X"

        result = parse_term("_foo")
        assert isinstance(result, Var)
        assert result.hint == "_foo"

        # Variable with digits
        result = parse_term("X1")
        assert isinstance(result, Var)
        assert result.hint == "X1"

    def test_parse_anonymous_variable(self):
        """Parse anonymous variable _."""
        result = parse_term("_")
        assert isinstance(result, Var)
        # Anonymous variables should get unique IDs

    def test_multiple_anonymous_variables_distinct(self):
        """Multiple _ should create distinct variables."""
        # Parse a structure with multiple _
        result = parse_term("foo(_, _)")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 2
        # Each _ should be a different variable
        assert isinstance(result.args[0], Var)
        assert isinstance(result.args[1], Var)
        assert result.args[0].id != result.args[1].id

    def test_parse_empty_list(self):
        """Parse empty list []."""
        result = parse_term("[]")
        assert result == List((), Atom("[]"))

    def test_parse_proper_list(self):
        """Parse list [1,2,3]."""
        result = parse_term("[1,2,3]")
        assert isinstance(result, List)
        assert len(result.items) == 3
        assert result.items[0] == Int(1)
        assert result.items[1] == Int(2)
        assert result.items[2] == Int(3)
        assert result.tail == Atom("[]")

    def test_parse_list_with_tail(self):
        """Parse list with tail [H|T]."""
        result = parse_term("[H|T]")
        assert isinstance(result, List)
        assert len(result.items) == 1
        assert isinstance(result.items[0], Var)
        assert result.items[0].hint == "H"
        assert isinstance(result.tail, Var)
        assert result.tail.hint == "T"

    def test_parse_list_multiple_elements_with_tail(self):
        """Parse [1,2|T]."""
        result = parse_term("[1,2|T]")
        assert isinstance(result, List)
        assert len(result.items) == 2
        assert result.items[0] == Int(1)
        assert result.items[1] == Int(2)
        assert isinstance(result.tail, Var)
        assert result.tail.hint == "T"

    def test_parse_improper_list(self):
        """Parse improper list [1|2]."""
        result = parse_term("[1|2]")
        assert isinstance(result, List)
        assert len(result.items) == 1
        assert result.items[0] == Int(1)
        assert result.tail == Int(2)

    def test_parse_nested_list(self):
        """Parse nested list [[1,2],[3]]."""
        result = parse_term("[[1,2],[3]]")
        assert isinstance(result, List)
        assert len(result.items) == 2

        # First element: [1,2]
        first = result.items[0]
        assert isinstance(first, List)
        assert len(first.items) == 2
        assert first.items[0] == Int(1)
        assert first.items[1] == Int(2)

        # Second element: [3]
        second = result.items[1]
        assert isinstance(second, List)
        assert len(second.items) == 1
        assert second.items[0] == Int(3)

    def test_parse_structure(self):
        """Parse structure foo(bar)."""
        result = parse_term("foo(bar)")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 1
        assert result.args[0] == Atom("bar")

    def test_parse_structure_multiple_args(self):
        """Parse structure foo(1, X, bar)."""
        result = parse_term("foo(1, X, bar)")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 3
        assert result.args[0] == Int(1)
        assert isinstance(result.args[1], Var)
        assert result.args[1].hint == "X"
        assert result.args[2] == Atom("bar")

    def test_parse_nested_structure(self):
        """Parse nested structure foo(bar(baz))."""
        result = parse_term("foo(bar(baz))")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 1

        inner = result.args[0]
        assert isinstance(inner, Struct)
        assert inner.functor == "bar"
        assert len(inner.args) == 1
        assert inner.args[0] == Atom("baz")

    def test_parse_structure_with_list(self):
        """Parse structure containing list foo([1,2])."""
        result = parse_term("foo([1,2])")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 1

        lst = result.args[0]
        assert isinstance(lst, List)
        assert len(lst.items) == 2
        assert lst.items[0] == Int(1)
        assert lst.items[1] == Int(2)

    def test_quoted_atom_as_functor(self):
        """Test quoted atoms can be functors (ISO-compliant)."""
        result = parse_term("'foo bar'(X)")
        assert isinstance(result, Struct)
        assert result.functor == "foo bar"
        assert len(result.args) == 1
        assert isinstance(result.args[0], Var)

    def test_structure_whitespace_handling(self):
        """Test structure parsing with various whitespace."""
        result = parse_term("foo( 1 , X , bar )")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 3
        assert result.args[0] == Int(1)
        assert isinstance(result.args[1], Var)
        assert result.args[2] == Atom("bar")

    def test_structure_trailing_comma_rejected(self):
        """Trailing comma in structure is invalid."""
        with pytest.raises(ParseError):
            parse_term("foo(1,)")

    def test_parse_term_with_comments(self):
        """Parse term ignoring comments."""
        # Line comment after term
        result = parse_term("foo % trailing comment\n")
        assert result == Atom("foo")

        # Line comment before term
        result = parse_term("% comment\nbar")
        assert result == Atom("bar")

        # Block comment (if supported)
        result = parse_term("/*block*/baz")
        assert result == Atom("baz")

        # Block comment with term
        result = parse_term("qux /*inline comment*/ ")
        assert result == Atom("qux")

    def test_parse_term_with_whitespace(self):
        """Parse term with various whitespace."""
        result = parse_term("  foo  ")
        assert result == Atom("foo")

        result = parse_term("foo( bar , baz )")
        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 2

    def test_parse_term_error_on_malformed(self):
        """ParseError on malformed input."""
        with pytest.raises(ParseError):
            parse_term("foo(")

        with pytest.raises(ParseError):
            parse_term("[1,]")

        with pytest.raises(ParseError):
            parse_term("Foo Bar")  # Two tokens

    def test_zero_arity_parens_rejected(self):
        """Zero-arity structures with parentheses are invalid."""
        with pytest.raises(ParseError):
            parse_term("foo()")

    def test_predicate_indicator_not_term(self):
        """Predicate indicator foo/2 is not a general term."""
        with pytest.raises(ParseError):
            parse_term("foo/2")

    def test_list_bar_edge_cases(self):
        """Test edge cases for list bar notation."""
        # Invalid bar syntax
        invalid_lists = ["[|T]", "[H|]", "[H||T]", "[H,|T]", "[H | | T]", "[1,2| |T]"]
        for invalid in invalid_lists:
            with pytest.raises(ParseError):
                parse_term(invalid)

        # Valid: whitespace around bar
        result = parse_term("[H | T]")
        assert isinstance(result, List)
        assert isinstance(result.tail, Var)

    def test_named_underscore_is_variable(self):
        """Named underscore like _X is a variable, not anonymous."""
        result = parse_term("_X")
        assert isinstance(result, Var)
        assert result.hint == "_X"

        # _foo is also a named variable
        result = parse_term("_foo")
        assert isinstance(result, Var)
        assert result.hint == "_foo"

    def test_malformed_tails(self):
        """Test various malformed tail notations."""
        with pytest.raises(ParseError):
            parse_term("[1,2||T]")  # Double bar

        with pytest.raises(ParseError):
            parse_term("[1,2|,T]")  # Bar comma


class TestVariableConsistency:
    """Test that variables with same name get same ID within a parse."""

    def test_same_variable_same_id_in_term(self):
        """Same variable name gets same ID within a term."""
        result = parse_term("foo(X, X)")
        assert isinstance(result, Struct)
        var1 = result.args[0]
        var2 = result.args[1]
        assert isinstance(var1, Var)
        assert isinstance(var2, Var)
        assert var1.id == var2.id  # Same variable

    def test_different_variables_different_ids(self):
        """Different variable names get different IDs."""
        result = parse_term("foo(X, Y)")
        assert isinstance(result, Struct)
        var1 = result.args[0]
        var2 = result.args[1]
        assert isinstance(var1, Var)
        assert isinstance(var2, Var)
        assert var1.id != var2.id  # Different variables

    def test_variable_consistency_in_list(self):
        """Variable consistency in lists."""
        result = parse_term("[X, Y, X]")
        assert isinstance(result, List)
        # Check all are variables first
        assert all(isinstance(item, Var) for item in result.items)
        # Then check IDs
        assert result.items[0].id == result.items[2].id  # Same X
        assert result.items[0].id != result.items[1].id  # X != Y


class TestParseClause:
    """Test parsing clauses (facts and rules)."""

    def test_parse_fact_atom(self):
        """Parse fact with atom head."""
        result = parse_clause("foo.")
        assert isinstance(result, Clause)
        assert result.head == Atom("foo")
        assert result.body == []

    def test_parse_fact_structure(self):
        """Parse fact with structure head."""
        result = parse_clause("parent(tom, bob).")
        assert isinstance(result, Clause)
        assert isinstance(result.head, Struct)
        assert result.head.functor == "parent"
        assert len(result.head.args) == 2
        assert result.head.args[0] == Atom("tom")
        assert result.head.args[1] == Atom("bob")
        assert result.body == []

    def test_parse_fact_quoted_atom_head(self):
        """Test quoted atoms as clause heads (ISO-compliant)."""
        # Quoted atom as fact
        result = parse_clause("'spaced atom'.")
        assert isinstance(result, Clause)
        assert result.head == Atom("spaced atom")
        assert result.body == []

        # Quoted atom as functor in fact
        result = parse_clause("'foo bar'(x).")
        assert isinstance(result, Clause)
        assert isinstance(result.head, Struct)
        assert result.head.functor == "foo bar"

    def test_parse_rule_simple(self):
        """Parse simple rule."""
        result = parse_clause("foo :- bar.")
        assert isinstance(result, Clause)
        assert result.head == Atom("foo")
        assert len(result.body) == 1
        assert result.body[0] == Atom("bar")

    def test_parse_rule_multiple_goals(self):
        """Parse rule with multiple goals."""
        result = parse_clause("foo :- bar, baz, qux.")
        assert isinstance(result, Clause)
        assert result.head == Atom("foo")
        assert len(result.body) == 3
        assert result.body[0] == Atom("bar")
        assert result.body[1] == Atom("baz")
        assert result.body[2] == Atom("qux")

    def test_parse_rule_with_variables(self):
        """Parse rule with variables."""
        result = parse_clause("parent(X,Y) :- father(X,Y).")
        assert isinstance(result, Clause)

        # Check head
        assert isinstance(result.head, Struct)
        assert result.head.functor == "parent"
        x_head = result.head.args[0]
        y_head = result.head.args[1]

        # Check body
        assert len(result.body) == 1
        assert isinstance(result.body[0], Struct)
        assert result.body[0].functor == "father"
        x_body = result.body[0].args[0]
        y_body = result.body[0].args[1]

        # Variables should be consistent
        assert isinstance(x_head, Var) and isinstance(x_body, Var)
        assert isinstance(y_head, Var) and isinstance(y_body, Var)
        assert x_head.id == x_body.id
        assert y_head.id == y_body.id

    def test_parse_rule_with_cut(self):
        """Parse rule containing cut."""
        result = parse_clause("foo(X) :- bar(X), !, baz(X).")
        assert isinstance(result, Clause)
        assert len(result.body) == 3
        assert result.body[1] == Atom("!")

    def test_parse_cut_only_body(self):
        """Parse clause with body containing only cut."""
        result = parse_clause("p :- !.")
        assert isinstance(result, Clause)
        assert result.head == Atom("p")
        assert len(result.body) == 1
        assert result.body[0] == Atom("!")

    def test_parse_clause_error_on_invalid(self):
        """ParseError on invalid clause syntax."""
        with pytest.raises(ParseError):
            parse_clause("foo")  # No period

        with pytest.raises(ParseError):
            parse_clause("42.")  # Integer can't be clause head

        with pytest.raises(ParseError):
            parse_clause("[a].")  # List can't be clause head

        with pytest.raises(ParseError):
            parse_clause("X :- true.")  # Variable can't be clause head

        with pytest.raises(ParseError):
            parse_clause("foo().")  # Zero-arity with parens invalid


class TestParseQuery:
    """Test parsing queries."""

    def test_parse_query_single_goal(self):
        """Parse query with single goal."""
        result = parse_query("?- foo.")
        assert len(result) == 1
        assert result[0] == Atom("foo")

    def test_parse_query_multiple_goals(self):
        """Parse query with multiple goals."""
        result = parse_query("?- foo, bar, baz.")
        assert len(result) == 3
        assert result[0] == Atom("foo")
        assert result[1] == Atom("bar")
        assert result[2] == Atom("baz")

    def test_parse_query_with_structure(self):
        """Parse query with structure."""
        result = parse_query("?- member(X, [1,2,3]).")
        assert len(result) == 1
        assert isinstance(result[0], Struct)
        assert result[0].functor == "member"

    def test_parse_query_with_cut(self):
        """Parse query containing cut."""
        result = parse_query("?- foo, !, bar.")
        assert len(result) == 3
        assert result[0] == Atom("foo")
        assert result[1] == Atom("!")
        assert result[2] == Atom("bar")

    def test_parse_query_cut_only(self):
        """Parse query with just cut."""
        result = parse_query("?- !.")
        assert len(result) == 1
        assert result[0] == Atom("!")

    def test_parse_query_error_on_malformed(self):
        """ParseError on malformed query."""
        with pytest.raises(ParseError):
            parse_query("foo.")  # Missing ?-

        with pytest.raises(ParseError):
            parse_query("?- foo")  # Missing period

        with pytest.raises(ParseError):
            parse_query("?-")  # No goals


class TestParseProgram:
    """Test parsing complete programs."""

    def test_parse_program_single_clause(self):
        """Parse program with single clause."""
        result = parse_program("foo.")
        assert len(result) == 1
        assert isinstance(result[0], Clause)
        assert result[0].head == Atom("foo")

    def test_parse_program_multiple_facts(self):
        """Parse program with multiple facts."""
        program = """
        parent(tom, bob).
        parent(tom, liz).
        parent(bob, ann).
        """
        result = parse_program(program)
        assert len(result) == 3

        # All should be facts (no body)
        for clause in result:
            assert isinstance(clause, Clause)
            assert clause.body == []

        # Check specific facts
        assert result[0].head.functor == "parent"
        assert result[0].head.args[0] == Atom("tom")
        assert result[0].head.args[1] == Atom("bob")

    def test_parse_program_mixed_facts_rules(self):
        """Parse program with facts and rules."""
        program = """
        % Facts
        father(tom, bob).
        father(bob, pat).
        
        % Rule
        grandfather(X, Z) :- father(X, Y), father(Y, Z).
        """
        result = parse_program(program)
        assert len(result) == 3

        # First two are facts
        assert result[0].body == []
        assert result[1].body == []

        # Third is a rule
        assert len(result[2].body) == 2
        assert result[2].body[0].functor == "father"
        assert result[2].body[1].functor == "father"

    def test_parse_program_preserves_order(self):
        """Parse program preserves clause order."""
        program = """
        first.
        second.
        third.
        """
        result = parse_program(program)
        assert len(result) == 3
        assert result[0].head == Atom("first")
        assert result[1].head == Atom("second")
        assert result[2].head == Atom("third")

    def test_parse_program_handles_blank_lines_comments(self):
        """Parse program handles blank lines and comments."""
        program = """
        % This is a comment
        foo.
        
        % Another comment
        
        bar.
        
        """
        result = parse_program(program)
        assert len(result) == 2
        assert result[0].head == Atom("foo")
        assert result[1].head == Atom("bar")

    def test_parse_program_empty(self):
        """Parse empty program."""
        result = parse_program("")
        assert result == []

        result = parse_program("% just comments")
        assert result == []

    def test_parse_program_with_queries(self):
        """Parse program with embedded queries."""
        program = """
        foo.
        ?- foo.
        bar.
        """
        # Queries should be filtered out or handled specially
        # Depending on implementation, might return only clauses
        # or might raise error for embedded queries
        # For now, assume queries are not allowed in programs
        with pytest.raises(ParseError):
            parse_program(program)

    def test_parse_program_with_directives(self):
        """Parse program with directives."""
        program = """
        foo.
        :- bar.
        baz.
        """
        # Directives should be filtered out or handled specially
        # For now, assume directives are not allowed in programs
        with pytest.raises(ParseError):
            parse_program(program)

    def test_parse_program_error_on_malformed(self):
        """ParseError on malformed program."""
        with pytest.raises(ParseError):
            parse_program("foo")  # Missing period

        with pytest.raises(ParseError):
            parse_program("foo. bar")  # Missing period on second

        with pytest.raises(ParseError):
            parse_program("42.")  # Invalid clause head


class TestParseError:
    """Test error handling and error messages."""

    def test_parse_error_has_line_number(self):
        """ParseError includes line number."""
        program = """
        foo.
        bar(
        """
        with pytest.raises(ParseError) as exc_info:
            parse_program(program)

        error = exc_info.value
        assert hasattr(error, "line")
        assert error.line >= 3  # Error on or after line 3 (more robust)

    def test_parse_error_has_column(self):
        """ParseError includes column number."""
        with pytest.raises(ParseError) as exc_info:
            parse_term("foo(")

        error = exc_info.value
        assert hasattr(error, "column")

    def test_parse_error_has_message(self):
        """ParseError has helpful message."""
        with pytest.raises(ParseError) as exc_info:
            parse_term("foo(")

        error = exc_info.value
        assert str(error)  # Should have string representation
        assert "foo(" in str(error) or "unexpected" in str(error).lower()


class TestRoundTrip:
    """Test that parsing preserves semantics."""

    def test_atoms_preserve_value(self):
        """Atoms preserve their string value."""
        assert parse_term("foo") == Atom("foo")
        assert parse_term("'foo bar'") == Atom("foo bar")
        assert parse_term("'123'") == Atom("123")

    def test_integers_preserve_value(self):
        """Integers preserve their numeric value."""
        assert parse_term("42") == Int(42)
        assert parse_term("-5") == Int(-5)
        assert parse_term("0") == Int(0)

    def test_lists_preserve_structure(self):
        """Lists preserve their structure."""
        # Empty list
        result = parse_term("[]")
        assert isinstance(result, List)
        assert result.items == ()
        assert result.tail == Atom("[]")

        # Proper list
        result = parse_term("[1,2,3]")
        assert isinstance(result, List)
        assert len(result.items) == 3
        assert result.tail == Atom("[]")

        # List with tail
        result = parse_term("[1|X]")
        assert isinstance(result, List)
        assert len(result.items) == 1
        assert isinstance(result.tail, Var)


class TestQuotedOperatorAtoms:
    """Test that quoted operator-like atoms remain atoms/functors, not operators."""
    
    def test_quoted_operator_atoms_as_functors(self):
        """Test that quoted operators like '@=<'(A,B) parse as regular functors."""
        # These should parse as Struct with operator name as functor
        # Not as operator expressions (since we don't have operator parsing yet)
        
        # Test various quoted operators as functors
        result = parse_term("'@=<'(a, b)")
        assert isinstance(result, Struct)
        assert result.functor == "@=<"
        assert len(result.args) == 2
        assert result.args[0] == Atom("a")
        assert result.args[1] == Atom("b")
        
        result = parse_term("'=..'(term, list)")
        assert isinstance(result, Struct)
        assert result.functor == "=.."
        assert len(result.args) == 2
        
        result = parse_term("'+'(1, 2)")
        assert isinstance(result, Struct)
        assert result.functor == "+"
        assert len(result.args) == 2
        assert result.args[0] == Int(1)
        assert result.args[1] == Int(2)
        
        result = parse_term("','(a, b)")
        assert isinstance(result, Struct)
        assert result.functor == ","
        assert len(result.args) == 2
    
    def test_quoted_operators_as_atoms(self):
        """Test that standalone quoted operators parse as atoms."""
        result = parse_term("'@=<'")
        assert isinstance(result, Atom)
        assert result.name == "@=<"
        
        result = parse_term("'=..'")
        assert isinstance(result, Atom)
        assert result.name == "=.."
        
        result = parse_term("'*->'")
        assert isinstance(result, Atom)
        assert result.name == "*->"
    
    def test_quoted_operators_in_clauses(self):
        """Test quoted operators work correctly in clauses."""
        # A fact using quoted operator as functor
        result = parse_clause("'='(X, X).")
        assert isinstance(result, Clause)
        assert isinstance(result.head, Struct)
        assert result.head.functor == "="
        assert len(result.head.args) == 2
        
        # A rule using quoted operators
        result = parse_clause("test(X) :- '>'(X, 0), '<'(X, 10).")
        assert isinstance(result, Clause)
        assert len(result.body) == 2
        assert isinstance(result.body[0], Struct)
        assert result.body[0].functor == ">"
        assert isinstance(result.body[1], Struct)
        assert result.body[1].functor == "<"
