"""Tests for the Prolog pretty printer - Issue #11.

The pretty printer converts AST back to readable Prolog text.
It should maintain round-trip property: parse(pretty(term)) == term.
"""

from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.ast.clauses import Clause
from prolog.ast.pretty import (
    pretty,
    pretty_clause,
    pretty_query,
    pretty_solution,
    pretty_program,
)
from prolog.parser.parser import parse_term, parse_clause


class TestPrettyBasicTerms:
    """Test pretty printing of basic terms."""
    
    def test_pretty_atom_simple(self):
        """Pretty print simple atoms."""
        assert pretty(Atom("foo")) == "foo"
        assert pretty(Atom("bar")) == "bar"
        assert pretty(Atom("x")) == "x"
        assert pretty(Atom("!")) == "!"
    
    def test_pretty_atom_needs_quotes(self):
        """Pretty print atoms that need quotes."""
        # Atoms with spaces need quotes
        assert pretty(Atom("foo bar")) == "'foo bar'"
        
        # Atoms with special characters need quotes
        assert pretty(Atom("Hello World!")) == "'Hello World!'"
        assert pretty(Atom("123")) == "'123'"  # Numeric string needs quotes
        assert pretty(Atom("[]")) == "'[]'"  # Reserved syntax needs quotes
        assert pretty(Atom("Uppercase")) == "'Uppercase'"  # Starts with uppercase
    
    def test_pretty_atom_with_escapes(self):
        """Pretty print atoms with characters that need escaping."""
        assert pretty(Atom("can't")) == "'can\\'t'"  # Escape single quote
        assert pretty(Atom("line\nbreak")) == "'line\\nbreak'"  # Escape newline
        assert pretty(Atom("tab\there")) == "'tab\\there'"  # Escape tab
        assert pretty(Atom("back\\slash")) == "'back\\\\slash'"  # Escape backslash
    
    def test_pretty_integer(self):
        """Pretty print integers."""
        assert pretty(Int(42)) == "42"
        assert pretty(Int(0)) == "0"
        assert pretty(Int(-5)) == "-5"
        assert pretty(Int(123456)) == "123456"
    
    def test_pretty_variable(self):
        """Pretty print variables with stable names."""
        # Variables should get consistent names within a session
        v1 = Var(0, "X")
        v2 = Var(1, "Y")
        assert pretty(v1) == "X"
        assert pretty(v2) == "Y"
        
        # Variable without hint gets generated name
        v3 = Var(12345)
        name = pretty(v3)
        assert name.startswith("_G")
        assert name[2:].isdigit()  # Rest should be digits
        
        # Named underscores keep their names
        v4 = Var(3, "_foo")
        assert pretty(v4) == "_foo"
    
    def test_pretty_anonymous_variable(self):
        """Pretty print anonymous variables."""
        # Anonymous variable should print as _
        anon = Var(999, "_")
        assert pretty(anon) == "_"


class TestPrettyLists:
    """Test pretty printing of lists."""
    
    def test_pretty_empty_list(self):
        """Pretty print empty list."""
        assert pretty(List((), Atom("[]"))) == "[]"
    
    def test_pretty_proper_list(self):
        """Pretty print proper lists."""
        # [1, 2, 3]
        lst = List((Int(1), Int(2), Int(3)), Atom("[]"))
        assert pretty(lst) == "[1, 2, 3]"
        
        # [foo, bar]
        lst = List((Atom("foo"), Atom("bar")), Atom("[]"))
        assert pretty(lst) == "[foo, bar]"
        
        # [X, Y, Z]
        lst = List((Var(0, "X"), Var(1, "Y"), Var(2, "Z")), Atom("[]"))
        assert pretty(lst) == "[X, Y, Z]"
    
    def test_pretty_list_with_tail(self):
        """Pretty print lists with tail notation."""
        # [H|T]
        lst = List((Var(0, "H"),), Var(1, "T"))
        assert pretty(lst) == "[H|T]"
        
        # [1, 2|T]
        lst = List((Int(1), Int(2)), Var(0, "T"))
        assert pretty(lst) == "[1, 2|T]"
        
        # [a, b, c|Rest]
        lst = List((Atom("a"), Atom("b"), Atom("c")), Var(0, "Rest"))
        assert pretty(lst) == "[a, b, c|Rest]"
    
    def test_pretty_improper_list(self):
        """Pretty print improper lists."""
        # [1|2] - non-list tail
        lst = List((Int(1),), Int(2))
        assert pretty(lst) == "[1|2]"
        
        # [a|foo]
        lst = List((Atom("a"),), Atom("foo"))
        assert pretty(lst) == "[a|foo]"
    
    def test_pretty_nested_list(self):
        """Pretty print nested lists."""
        # [[1, 2], [3]]
        inner1 = List((Int(1), Int(2)), Atom("[]"))
        inner2 = List((Int(3),), Atom("[]"))
        outer = List((inner1, inner2), Atom("[]"))
        assert pretty(outer) == "[[1, 2], [3]]"
        
        # [[], [a]]
        empty = List((), Atom("[]"))
        single = List((Atom("a"),), Atom("[]"))
        outer = List((empty, single), Atom("[]"))
        assert pretty(outer) == "[[], [a]]"
    
    def test_pretty_long_list(self):
        """Pretty print long list without unwanted formatting."""
        # 20-element list
        items = tuple(Int(i) for i in range(20))
        lst = List(items, Atom("[]"))
        expected = "[" + ", ".join(str(i) for i in range(20)) + "]"
        assert pretty(lst) == expected


class TestPrettyStructures:
    """Test pretty printing of structures."""
    
    def test_pretty_structure_simple(self):
        """Pretty print simple structures."""
        # foo(bar)
        s = Struct("foo", (Atom("bar"),))
        assert pretty(s) == "foo(bar)"
        
        # pred(123)
        s = Struct("pred", (Int(123),))
        assert pretty(s) == "pred(123)"
        
        # test(X)
        s = Struct("test", (Var(0, "X"),))
        assert pretty(s) == "test(X)"
    
    def test_pretty_structure_multiple_args(self):
        """Pretty print structures with multiple arguments."""
        # foo(a, b, c)
        s = Struct("foo", (Atom("a"), Atom("b"), Atom("c")))
        assert pretty(s) == "foo(a, b, c)"
        
        # pred(1, 2, 3, 4, 5)
        s = Struct("pred", (Int(1), Int(2), Int(3), Int(4), Int(5)))
        assert pretty(s) == "pred(1, 2, 3, 4, 5)"
        
        # f(X, Y, Z)
        s = Struct("f", (Var(0, "X"), Var(1, "Y"), Var(2, "Z")))
        assert pretty(s) == "f(X, Y, Z)"
    
    def test_pretty_structure_nested(self):
        """Pretty print nested structures."""
        # foo(bar(baz))
        inner = Struct("bar", (Atom("baz"),))
        outer = Struct("foo", (inner,))
        assert pretty(outer) == "foo(bar(baz))"
        
        # outer(middle(inner(x)))
        innermost = Struct("inner", (Atom("x"),))
        middle = Struct("middle", (innermost,))
        outer = Struct("outer", (middle,))
        assert pretty(outer) == "outer(middle(inner(x)))"
    
    def test_pretty_structure_with_mixed_args(self):
        """Pretty print structures with mixed argument types."""
        # foo(1, X, [a, b])
        lst = List((Atom("a"), Atom("b")), Atom("[]"))
        s = Struct("foo", (Int(1), Var(0, "X"), lst))
        assert pretty(s) == "foo(1, X, [a, b])"
    
    def test_pretty_structure_quoted_functor(self):
        """Pretty print structures with quoted functors."""
        # 'foo bar'(x)
        s = Struct("foo bar", (Atom("x"),))
        assert pretty(s) == "'foo bar'(x)"
        
        # '123'(a, b)
        s = Struct("123", (Atom("a"), Atom("b")))
        assert pretty(s) == "'123'(a, b)"
        
        # Reserved syntax as functor
        s = Struct("[]", ())
        assert pretty(s) == "'[]'"  # Zero-arity with reserved functor
        
        s = Struct(":-", (Atom("a"), Atom("b")))
        assert pretty(s) == "':-'(a, b)"


class TestPrettyClauses:
    """Test pretty printing of clauses."""
    
    def test_pretty_fact_atom(self):
        """Pretty print fact with atom head."""
        clause = Clause(Atom("foo"), ())
        assert pretty_clause(clause) == "foo."
    
    def test_pretty_fact_structure(self):
        """Pretty print fact with structure head."""
        head = Struct("parent", (Atom("tom"), Atom("bob")))
        clause = Clause(head, ())
        assert pretty_clause(clause) == "parent(tom, bob)."
    
    def test_pretty_rule_simple(self):
        """Pretty print simple rule."""
        head = Atom("foo")
        body = (Atom("bar"),)
        clause = Clause(head, body)
        assert pretty_clause(clause) == "foo :- bar."
    
    def test_pretty_rule_multiple_goals(self):
        """Pretty print rule with multiple goals."""
        head = Atom("foo")
        body = (Atom("bar"), Atom("baz"), Atom("qux"))
        clause = Clause(head, body)
        assert pretty_clause(clause) == "foo :- bar, baz, qux."
    
    def test_pretty_rule_with_variables(self):
        """Pretty print rule with variables."""
        head = Struct("parent", (Var(0, "X"), Var(1, "Y")))
        body = (Struct("father", (Var(0, "X"), Var(1, "Y"))),)
        clause = Clause(head, body)
        assert pretty_clause(clause) == "parent(X, Y) :- father(X, Y)."
    
    def test_pretty_rule_with_cut(self):
        """Pretty print rule containing cut."""
        head = Struct("foo", (Var(0, "X"),))
        body = (
            Struct("bar", (Var(0, "X"),)),
            Atom("!"),
            Struct("baz", (Var(0, "X"),))
        )
        clause = Clause(head, body)
        assert pretty_clause(clause) == "foo(X) :- bar(X), !, baz(X)."
    
    def test_pretty_rule_cut_only(self):
        """Pretty print rule with only cut in body (green cut)."""
        clause = Clause(Atom("p"), (Atom("!"),))
        assert pretty_clause(clause) == "p :- !."


class TestPrettyQuery:
    """Test pretty printing of queries."""
    
    def test_pretty_query_single_goal(self):
        """Pretty print query with single goal."""
        goals = [Atom("foo")]
        assert pretty_query(goals) == "?- foo."
    
    def test_pretty_query_multiple_goals(self):
        """Pretty print query with multiple goals."""
        goals = [Atom("foo"), Atom("bar"), Atom("baz")]
        assert pretty_query(goals) == "?- foo, bar, baz."
    
    def test_pretty_query_with_structure(self):
        """Pretty print query with structure."""
        goals = [Struct("member", (Var(0, "X"), List((Int(1), Int(2), Int(3)), Atom("[]"))))]
        assert pretty_query(goals) == "?- member(X, [1, 2, 3])."
    
    def test_pretty_query_with_cut(self):
        """Pretty print query containing cut."""
        goals = [Atom("foo"), Atom("!"), Atom("bar")]
        assert pretty_query(goals) == "?- foo, !, bar."
    
    def test_pretty_query_whitespace_normalization(self):
        """Query formatting should normalize whitespace."""
        goals = [Struct("f", (Int(1),)), Struct("g", (Atom("a"), Atom("b")))]
        assert pretty_query(goals) == "?- f(1), g(a, b)."


class TestPrettySolution:
    """Test pretty printing of solutions."""
    
    def test_pretty_solution_single_binding(self):
        """Pretty print solution with single binding."""
        bindings = {"X": Int(42)}
        assert pretty_solution(bindings) == "X = 42"
    
    def test_pretty_solution_multiple_bindings(self):
        """Pretty print solution with multiple bindings."""
        bindings = {
            "X": Int(1),
            "Y": Atom("foo"),
            "Z": List((Atom("a"), Atom("b")), Atom("[]"))
        }
        # Should be in sorted order by variable name
        assert pretty_solution(bindings) == "X = 1, Y = foo, Z = [a, b]"
    
    def test_pretty_solution_empty(self):
        """Pretty print empty solution (success with no bindings)."""
        assert pretty_solution({}) == "true"
    
    def test_pretty_solution_unbound_variable(self):
        """Pretty print solution with unbound variable."""
        # Unbound variables remain as variables
        bindings = {"X": Var(0, "X")}
        assert pretty_solution(bindings) == "X = X"
        
        # Variable bound to another variable
        bindings = {"X": Var(1, "Y")}
        assert pretty_solution(bindings) == "X = Y"
    
    def test_pretty_solution_with_quoted_values(self):
        """Pretty print solution with values that need quoting."""
        bindings = {
            "Z": Atom("a b"),  # Needs quotes
            "A": List((Int(1),), Atom("[]"))
        }
        # Should be sorted by variable name
        assert pretty_solution(bindings) == "A = [1], Z = 'a b'"


class TestPrettyProgram:
    """Test pretty printing of programs."""
    
    def test_pretty_program_single_clause(self):
        """Pretty print program with single clause."""
        clauses = [Clause(Atom("foo"), ())]
        assert pretty_program(clauses) == "foo."
    
    def test_pretty_program_multiple_clauses(self):
        """Pretty print program with multiple clauses."""
        clauses = [
            Clause(Struct("parent", (Atom("tom"), Atom("bob"))), ()),
            Clause(Struct("parent", (Atom("tom"), Atom("liz"))), ()),
            Clause(
                Struct("grandparent", (Var(0, "X"), Var(2, "Z"))),
                (
                    Struct("parent", (Var(0, "X"), Var(1, "Y"))),
                    Struct("parent", (Var(1, "Y"), Var(2, "Z")))
                )
            )
        ]
        expected = """parent(tom, bob).
parent(tom, liz).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z)."""
        assert pretty_program(clauses) == expected
    
    def test_pretty_program_empty(self):
        """Pretty print empty program."""
        assert pretty_program([]) == ""


class TestRoundTrip:
    """Test round-trip property: parse(pretty(term)) == term."""
    
    def test_round_trip_atoms(self):
        """Round-trip test for atoms."""
        atoms = [
            Atom("foo"),
            Atom("bar"),
            Atom("!"),
            Atom("foo bar"),
            Atom("123"),
            Atom("Uppercase"),
            Atom("can't"),
        ]
        for atom in atoms:
            text = pretty(atom)
            parsed = parse_term(text)
            assert parsed == atom, f"Round-trip failed for {atom}"
    
    def test_round_trip_integers(self):
        """Round-trip test for integers."""
        integers = [Int(0), Int(42), Int(-5), Int(999999)]
        for integer in integers:
            text = pretty(integer)
            parsed = parse_term(text)
            assert parsed == integer, f"Round-trip failed for {integer}"
    
    def test_round_trip_variables(self):
        """Round-trip test for variables."""
        # Note: Variable IDs may differ, but names should be preserved
        variables = [
            Var(0, "X"),
            Var(1, "Y"),
            Var(2, "Variable"),
            Var(3, "_foo"),
        ]
        for var in variables:
            text = pretty(var)
            parsed = parse_term(text)
            # Check that it's a variable with the same hint
            assert isinstance(parsed, Var)
            assert parsed.hint == var.hint
    
    def test_round_trip_lists(self):
        """Round-trip test for lists."""
        lists = [
            List((), Atom("[]")),  # []
            List((Int(1), Int(2), Int(3)), Atom("[]")),  # [1,2,3]
            List((Var(0, "H"),), Var(1, "T")),  # [H|T]
            List((Int(1),), Int(2)),  # [1|2] improper
        ]
        for lst in lists:
            text = pretty(lst)
            parsed = parse_term(text)
            # Check structure matches
            assert isinstance(parsed, List)
            assert len(parsed.items) == len(lst.items)
            
            # Check tail type and value
            assert parsed.tail.__class__ is lst.tail.__class__
            if isinstance(lst.tail, Atom):
                assert parsed.tail.name == lst.tail.name
            elif isinstance(lst.tail, Var):
                assert parsed.tail.hint == lst.tail.hint
            elif isinstance(lst.tail, Int):
                assert parsed.tail.value == lst.tail.value
    
    def test_round_trip_structures(self):
        """Round-trip test for structures."""
        structures = [
            Struct("foo", (Atom("bar"),)),
            Struct("pred", (Int(1), Int(2), Int(3))),
            Struct("test", (Var(0, "X"), Var(1, "Y"))),
            Struct("foo bar", (Atom("x"),)),  # Quoted functor
        ]
        for struct in structures:
            text = pretty(struct)
            parsed = parse_term(text)
            assert isinstance(parsed, Struct)
            assert parsed.functor == struct.functor
            assert len(parsed.args) == len(struct.args)
    
    def test_round_trip_clauses(self):
        """Round-trip test for clauses."""
        clauses = [
            Clause(Atom("foo"), ()),
            Clause(Struct("parent", (Atom("tom"), Atom("bob"))), ()),
            Clause(Atom("foo"), (Atom("bar"), Atom("baz"))),
            Clause(
                Struct("p", (Var(0, "X"),)),
                (Struct("q", (Var(0, "X"),)), Atom("!"), Struct("r", (Var(0, "X"),)))
            ),
        ]
        for clause in clauses:
            text = pretty_clause(clause)
            parsed = parse_clause(text)
            assert isinstance(parsed, Clause)
            
            # Check head matches
            if isinstance(clause.head, Atom):
                assert isinstance(parsed.head, Atom)
                assert parsed.head.name == clause.head.name
            elif isinstance(clause.head, Struct):
                assert isinstance(parsed.head, Struct)
                assert parsed.head.functor == clause.head.functor
                assert len(parsed.head.args) == len(clause.head.args)
            
            # Check body matches
            assert len(parsed.body) == len(clause.body)
            for g1, g2 in zip(parsed.body, clause.body):
                assert isinstance(g1, type(g2))
                if isinstance(g1, Atom):
                    assert g1.name == g2.name
                elif isinstance(g1, Struct):
                    assert g1.functor == g2.functor
                    assert len(g1.args) == len(g2.args)


class TestQuotingLogic:
    """Test the logic for determining when atoms need quotes."""
    
    def test_needs_quotes_spaces(self):
        """Atoms with spaces need quotes."""
        assert pretty(Atom("foo bar")) == "'foo bar'"
        assert pretty(Atom("hello world")) == "'hello world'"
    
    def test_needs_quotes_special_chars(self):
        """Atoms with special characters need quotes."""
        assert pretty(Atom("foo!")) == "'foo!'"
        assert pretty(Atom("a-b")) == "'a-b'"
        assert pretty(Atom("x+y")) == "'x+y'"
    
    def test_needs_quotes_uppercase_start(self):
        """Atoms starting with uppercase need quotes."""
        assert pretty(Atom("Foo")) == "'Foo'"
        assert pretty(Atom("Variable")) == "'Variable'"
    
    def test_needs_quotes_numeric(self):
        """Atoms that look like numbers need quotes."""
        assert pretty(Atom("123")) == "'123'"
        assert pretty(Atom("-5")) == "'-5'"
    
    def test_needs_quotes_reserved(self):
        """Atoms that conflict with syntax need quotes."""
        assert pretty(Atom("[]")) == "'[]'"
        assert pretty(Atom("()")) == "'()'"
        assert pretty(Atom(":-")) == "':-'"
        assert pretty(Atom(",")) == "','"
        assert pretty(Atom("|")) == "'|'"
        assert pretty(Atom("_")) == "'_'"  # Underscore alone needs quotes
    
    def test_no_quotes_needed(self):
        """Simple lowercase atoms don't need quotes."""
        assert pretty(Atom("foo")) == "foo"
        assert pretty(Atom("bar123")) == "bar123"
        assert pretty(Atom("test_var")) == "test_var"
        assert pretty(Atom("a")) == "a"


class TestEscaping:
    """Test character escaping in quoted atoms."""
    
    def test_escape_single_quote(self):
        """Single quotes must be escaped in quoted atoms."""
        assert pretty(Atom("can't")) == "'can\\'t'"
        assert pretty(Atom("it's")) == "'it\\'s'"
    
    def test_escape_backslash(self):
        """Backslashes must be escaped."""
        assert pretty(Atom("a\\b")) == "'a\\\\b'"
        assert pretty(Atom("\\")) == "'\\\\'"
    
    def test_escape_newline_tab(self):
        """Newlines and tabs should be escaped."""
        assert pretty(Atom("line\nbreak")) == "'line\\nbreak'"
        assert pretty(Atom("\t")) == "'\\t'"
        assert pretty(Atom("a\tb\nc")) == "'a\\tb\\nc'"
    
    def test_no_escape_needed(self):
        """Most characters don't need escaping."""
        assert pretty(Atom("foo bar")) == "'foo bar'"
        assert pretty(Atom("a/b")) == "'a/b'"
        assert pretty(Atom("x=y")) == "'x=y'"