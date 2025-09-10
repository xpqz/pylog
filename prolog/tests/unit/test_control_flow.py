"""Tests for control flow operators - Issue #38.

This module tests the transformation of control flow operators (conjunction,
disjunction, and if-then-else) to their canonical forms, verifying precedence,
associativity, and integration with the Stage 1 engine.

Test Coverage:
- Conjunction (,) operator with precedence 1000, xfy associativity
- Disjunction (;) operator with precedence 1100, xfy associativity  
- If-then (->) operator with precedence 1050, xfy associativity
- Mixed precedence interactions
- Nested control flow structures
- Integration with Stage 1 engine execution
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct
from prolog.ast.clauses import Clause
from prolog.parser.reader import Reader, ReaderError
from prolog.parser.parser import parse_clause, parse_program, parse_query
from prolog.engine.engine import Engine
from prolog.unify.store import Store


def _assert_engine_clean(engine):
    """Assert the engine is in a clean state with no residual data."""
    if hasattr(engine, "trail"):
        assert engine.trail.position() == 0, "Trail not clean"
    if hasattr(engine, "goal_stack"):
        assert engine.goal_stack.height() == 0, "Goal stack not empty"
    if hasattr(engine, "cp_stack"):
        assert len(engine.cp_stack) == 0, "Choicepoint stack not empty"
    if hasattr(engine, "frame_stack"):
        assert len(engine.frame_stack) == 0, "Frame stack not empty"


class TestConjunctionOperator:
    """Test conjunction (,) operator transformation and execution."""
    
    def test_simple_conjunction(self):
        """Simple conjunction: a, b → ','(a, b)."""
        reader = Reader()
        result = reader.read_term("a, b")
        expected = Struct(",", (Atom("a"), Atom("b")))
        assert result == expected
    
    def test_conjunction_right_associative(self):
        """Conjunction is right-associative: a, b, c → ','(a, ','(b, c))."""
        reader = Reader()
        result = reader.read_term("a, b, c")
        expected = Struct(",", (
            Atom("a"), 
            Struct(",", (Atom("b"), Atom("c")))
        ))
        assert result == expected
    
    def test_conjunction_chain_long(self):
        """Long conjunction chain: a, b, c, d, e → proper right association."""
        reader = Reader()
        result = reader.read_term("a, b, c, d, e")
        expected = Struct(",", (
            Atom("a"),
            Struct(",", (
                Atom("b"),
                Struct(",", (
                    Atom("c"),
                    Struct(",", (Atom("d"), Atom("e")))
                ))
            ))
        ))
        assert result == expected
    
    def test_conjunction_with_structures(self):
        """Conjunction with structures: f(x), g(y) → ','(f(x), g(y))."""
        reader = Reader()
        result = reader.read_term("f(x), g(y)")
        expected = Struct(",", (
            Struct("f", (Atom("x"),)),
            Struct("g", (Atom("y"),))
        ))
        assert result == expected
    
    def test_conjunction_precedence_1000(self):
        """Verify conjunction has precedence 1000."""
        from prolog.parser.operators import get_operator_info
        info = get_operator_info(",", "infix")
        assert info is not None
        assert info[0] == 1000  # precedence
        assert info[1] == "xfy"  # xfy type (parses as right-branching tree)
        assert info[2] == "','"  # canonical form


class TestDisjunctionOperator:
    """Test disjunction (;) operator transformation and execution."""
    
    def test_simple_disjunction(self):
        """Simple disjunction: a; b → ';'(a, b)."""
        reader = Reader()
        result = reader.read_term("a; b")
        expected = Struct(";", (Atom("a"), Atom("b")))
        assert result == expected
    
    def test_disjunction_right_associative(self):
        """Disjunction is right-associative: a; b; c → ';'(a, ';'(b, c))."""
        reader = Reader()
        result = reader.read_term("a; b; c")
        expected = Struct(";", (
            Atom("a"),
            Struct(";", (Atom("b"), Atom("c")))
        ))
        assert result == expected
    
    def test_disjunction_chain_long(self):
        """Long disjunction chain: a; b; c; d → proper right association."""
        reader = Reader()
        result = reader.read_term("a; b; c; d")
        expected = Struct(";", (
            Atom("a"),
            Struct(";", (
                Atom("b"),
                Struct(";", (Atom("c"), Atom("d")))
            ))
        ))
        assert result == expected
    
    def test_disjunction_precedence_1100(self):
        """Verify disjunction has precedence 1100."""
        from prolog.parser.operators import get_operator_info
        info = get_operator_info(";", "infix")
        assert info is not None
        assert info[0] == 1100  # precedence
        assert info[1] == "xfy"  # right-associative
        assert info[2] == "';'"  # canonical form


class TestIfThenOperator:
    """Test if-then (->) operator transformation."""
    
    def test_simple_if_then(self):
        """Simple if-then: a -> b → '->'(a, b)."""
        reader = Reader()
        result = reader.read_term("a -> b")
        expected = Struct("->", (Atom("a"), Atom("b")))
        assert result == expected
    
    def test_if_then_right_associative(self):
        """If-then is right-associative: a -> b -> c → '->'(a, '->'(b, c))."""
        reader = Reader()
        result = reader.read_term("a -> b -> c")
        expected = Struct("->", (
            Atom("a"),
            Struct("->", (Atom("b"), Atom("c")))
        ))
        assert result == expected
    
    def test_if_then_precedence_1050(self):
        """Verify if-then has precedence 1050."""
        from prolog.parser.operators import get_operator_info
        info = get_operator_info("->", "infix")
        assert info is not None
        assert info[0] == 1050  # precedence
        assert info[1] == "xfy"  # right-associative
        assert info[2] == "'->'"  # canonical form


class TestIfThenElse:
    """Test if-then-else construct (-> combined with ;)."""
    
    def test_basic_if_then_else(self):
        """Basic if-then-else: (a -> b; c) → ';'('->'(a, b), c)."""
        reader = Reader()
        result = reader.read_term("a -> b; c")
        expected = Struct(";", (
            Struct("->", (Atom("a"), Atom("b")),),
            Atom("c")
        ))
        assert result == expected
    
    def test_nested_if_then_else(self):
        """Nested if-then-else: (a -> b; c -> d; e)."""
        reader = Reader()
        result = reader.read_term("a -> b; c -> d; e")
        expected = Struct(";", (
            Struct("->", (Atom("a"), Atom("b")),),
            Struct(";", (
                Struct("->", (Atom("c"), Atom("d")),),
                Atom("e")
            ))
        ))
        assert result == expected
    
    def test_if_then_else_with_conjunction(self):
        """If-then-else with conjunction: (a, b -> c, d; e, f)."""
        reader = Reader()
        result = reader.read_term("a, b -> c, d; e, f")
        # -> has precedence 1050, , has 1000 (binds tighter), ; has 1100
        # So: ((a, b) -> (c, d)); (e, f)
        expected = Struct(";", (
            Struct("->", (
                Struct(",", (Atom("a"), Atom("b"))),
                Struct(",", (Atom("c"), Atom("d")))
            )),
            Struct(",", (Atom("e"), Atom("f")))
        ))
        assert result == expected
    
    def test_parenthesized_if_then_else(self):
        """Parentheses enforce grouping: (a -> b); c vs a -> (b; c)."""
        reader = Reader()
        
        # With parentheses around if-then
        result1 = reader.read_term("(a -> b); c")
        expected1 = Struct(";", (
            Struct("->", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        assert result1 == expected1
        
        # With parentheses around disjunction
        result2 = reader.read_term("a -> (b; c)")
        expected2 = Struct("->", (
            Atom("a"),
            Struct(";", (Atom("b"), Atom("c")))
        ))
        assert result2 == expected2


class TestMixedPrecedence:
    """Test mixed control flow operator precedence."""
    
    def test_conjunction_binds_tighter_than_disjunction(self):
        """Conjunction (1000) binds tighter than disjunction (1100)."""
        reader = Reader()
        
        # a, b; c → ;(,(a, b), c)
        result = reader.read_term("a, b; c")
        expected = Struct(";", (
            Struct(",", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        assert result == expected
        
        # a; b, c → ;(a, ,(b, c))
        result = reader.read_term("a; b, c")
        expected = Struct(";", (
            Atom("a"),
            Struct(",", (Atom("b"), Atom("c")))
        ))
        assert result == expected
    
    def test_conjunction_binds_tighter_than_if_then(self):
        """Conjunction (1000) binds tighter than if-then (1050)."""
        reader = Reader()
        
        # a, b -> c → ->(,(a, b), c)
        result = reader.read_term("a, b -> c")
        expected = Struct("->", (
            Struct(",", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        assert result == expected
        
        # a -> b, c → ->(a, ,(b, c))
        result = reader.read_term("a -> b, c")
        expected = Struct("->", (
            Atom("a"),
            Struct(",", (Atom("b"), Atom("c")))
        ))
        assert result == expected
    
    def test_if_then_binds_tighter_than_disjunction(self):
        """If-then (1050) binds tighter than disjunction (1100)."""
        reader = Reader()
        
        # a -> b; c → ;(->(a, b), c)
        result = reader.read_term("a -> b; c")
        expected = Struct(";", (
            Struct("->", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        assert result == expected
    
    def test_complex_precedence_chain(self):
        """Complex chain: a, b -> c, d; e, f -> g; h."""
        reader = Reader()
        result = reader.read_term("a, b -> c, d; e, f -> g; h")
        expected = Struct(";", (
            Struct("->", (
                Struct(",", (Atom("a"), Atom("b"))),
                Struct(",", (Atom("c"), Atom("d")))
            )),
            Struct(";", (
                Struct("->", (
                    Struct(",", (Atom("e"), Atom("f"))),
                    Atom("g")
                )),
                Atom("h")
            ))
        ))
        assert result == expected
    
    def test_parentheses_override_precedence(self):
        """Parentheses can override natural precedence."""
        reader = Reader()
        
        # Without parens: a, b; c groups as ;(,(a,b), c)
        result1 = reader.read_term("a, b; c")
        expected1 = Struct(";", (
            Struct(",", (Atom("a"), Atom("b"))),
            Atom("c")
        ))
        assert result1 == expected1
        
        # With parens: a, (b; c) groups as ,(a, ;(b,c))
        result2 = reader.read_term("a, (b; c)")
        expected2 = Struct(",", (
            Atom("a"),
            Struct(";", (Atom("b"), Atom("c")))
        ))
        assert result2 == expected2


class TestControlFlowInClauses:
    """Test control flow operators in clauses and queries."""
    
    def test_clause_with_conjunction_body(self):
        """Clause with conjunction in body: head :- a, b, c."""
        text = "head :- a, b, c."
        clause = parse_clause(text)
        assert isinstance(clause, Clause)
        assert clause.head == Atom("head")
        assert len(clause.body) == 1
        
        # Body should be [','(a, ','(b, c))]
        body_goal = clause.body[0]
        assert isinstance(body_goal, Struct)
        assert body_goal.functor == ","
        assert body_goal.args[0] == Atom("a")
        
        rest = body_goal.args[1]
        assert isinstance(rest, Struct)
        assert rest.functor == ","
        assert rest.args == (Atom("b"), Atom("c"))
    
    def test_clause_with_disjunction_body(self):
        """Clause with disjunction in body: head :- a; b; c."""
        text = "head :- a; b; c."
        clause = parse_clause(text)
        assert isinstance(clause, Clause)
        assert clause.head == Atom("head")
        assert len(clause.body) == 1
        
        # Body should be [';'(a, ';'(b, c))]
        body_goal = clause.body[0]
        assert isinstance(body_goal, Struct)
        assert body_goal.functor == ";"
        assert body_goal.args[0] == Atom("a")
        
        rest = body_goal.args[1]
        assert isinstance(rest, Struct)
        assert rest.functor == ";"
        assert rest.args == (Atom("b"), Atom("c"))
    
    def test_clause_with_if_then_else_body(self):
        """Clause with if-then-else in body: head :- (a -> b; c)."""
        text = "head :- (a -> b; c)."
        clause = parse_clause(text)
        assert isinstance(clause, Clause)
        assert clause.head == Atom("head")
        assert len(clause.body) == 1
        
        # Body should be [';'('->'(a, b), c)]
        body_goal = clause.body[0]
        assert isinstance(body_goal, Struct)
        assert body_goal.functor == ";"
        
        if_then = body_goal.args[0]
        assert isinstance(if_then, Struct)
        assert if_then.functor == "->"
        assert if_then.args == (Atom("a"), Atom("b"))
        
        assert body_goal.args[1] == Atom("c")
    
    def test_query_with_control_flow(self):
        """Query with control flow: ?- a, b; c -> d."""
        text = "?- a, b; c -> d."
        goals = parse_query(text)
        
        # Should be [';'(','(a, b), '->'(c, d))]
        assert len(goals) == 1
        goal = goals[0]
        assert isinstance(goal, Struct)
        assert goal.functor == ";"
        
        left = goal.args[0]
        assert isinstance(left, Struct)
        assert left.functor == ","
        assert left.args == (Atom("a"), Atom("b"))
        
        right = goal.args[1]
        assert isinstance(right, Struct)
        assert right.functor == "->"
        assert right.args == (Atom("c"), Atom("d"))


class TestEngineIntegration:
    """Test control flow operators execute correctly with Stage 1 engine."""
    
    def test_conjunction_execution(self):
        """Test conjunction executes both goals in sequence."""
        program_text = """
        fact1.
        fact2.
        test :- fact1, fact2.
        """
        program = parse_program(program_text)
        engine = Engine(program)
        store = Store()
        
        # Query: ?- test.
        results = list(engine.query([Struct("test", ())], store))
        assert len(results) == 1  # Should succeed once
        _assert_engine_clean(engine)
    
    def test_disjunction_execution(self):
        """Test disjunction tries alternatives."""
        program_text = """
        fact1.
        fact2.
        test1 :- fact1; fact2.
        test2 :- fact3; fact1.
        """
        program = parse_program(program_text)
        engine = Engine(program)
        store = Store()
        
        # Query: ?- test1. (both alternatives exist)
        results = list(engine.query([Struct("test1", ())], store))
        assert len(results) == 2  # Should succeed twice
        _assert_engine_clean(engine)
        
        # Query: ?- test2. (only second alternative exists)
        store2 = Store()
        results = list(engine.query([Struct("test2", ())], store2))
        assert len(results) == 1  # Should succeed once
        _assert_engine_clean(engine)
    
    def test_if_then_else_execution(self):
        """Test if-then-else conditional execution."""
        program_text = """
        cond_true.
        result1.
        result2.
        test1 :- (cond_true -> result1; result2).
        test2 :- (cond_false -> result1; result2).
        """
        program = parse_program(program_text)
        engine = Engine(program)
        store = Store()
        
        # Query: ?- test1. (condition succeeds, use then branch)
        results = list(engine.query([Struct("test1", ())], store))
        assert len(results) == 1
        _assert_engine_clean(engine)
        
        # Query: ?- test2. (condition fails, use else branch)
        store2 = Store()
        results = list(engine.query([Struct("test2", ())], store2))
        assert len(results) == 1
        _assert_engine_clean(engine)
    
    def test_nested_control_flow_execution(self):
        """Test nested control flow structures execute correctly."""
        program_text = """
        a.
        b.
        c.
        test :- (a, b); c.
        """
        program = parse_program(program_text)
        engine = Engine(program)
        store = Store()
        
        # Query: ?- test.
        # Should succeed twice: once via (a,b), once via c
        results = list(engine.query([Struct("test", ())], store))
        assert len(results) == 2
        _assert_engine_clean(engine)
    
    def test_if_then_commits_on_first_success(self):
        """Test that if-then commits to first solution of condition."""
        program_text = """
        p(1).
        p(2).
        q(1).
        test(X) :- (p(X) -> q(X); true).
        """
        program = parse_program(program_text)
        engine = Engine(program)
        store = Store()
        
        # Query: ?- test(X).
        # X=1 succeeds via then-branch
        # X=2 does not take else because condition succeeds and commits
        results = list(engine.query([Struct("test", (Var(0, "X"),))], store))
        
        # Extract X values from results
        x_values = set()
        for result in results:
            if "X" in result:
                x_values.add(result["X"].value if hasattr(result["X"], "value") else result["X"])
        
        assert x_values == {1}, f"Expected {{1}}, got {x_values}"
        _assert_engine_clean(engine)
    
    def test_if_then_failure_does_not_fall_back_to_else(self):
        """Test that then-branch failure doesn't fall back to else."""
        program_text = """
        p(1).
        p(2).
        q(1).
        r(2).
        test(X) :- (p(X) -> q(X); r(X)).
        """
        program = parse_program(program_text)
        engine = Engine(program)
        store = Store()
        
        # Query: ?- test(X).
        # X=1: condition succeeds, then succeeds
        # X=2: condition succeeds, then fails, no else branch taken
        results = list(engine.query([Struct("test", (Var(0, "X"),))], store))
        
        # Extract X values from results
        x_values = set()
        for result in results:
            if "X" in result:
                x_values.add(result["X"].value if hasattr(result["X"], "value") else result["X"])
        
        assert x_values == {1}, f"Expected {{1}}, got {x_values}"
        _assert_engine_clean(engine)


class TestCanonicalFormEquivalence:
    """Test that operator forms are equivalent to canonical forms."""
    
    def test_conjunction_canonical_equivalence(self):
        """Test 'a, b' is equivalent to ','(a, b)."""
        reader = Reader()
        
        # Parse operator form
        operator_form = reader.read_term("a, b")
        
        # Parse canonical form
        canonical_form = reader.read_term("','(a, b)")
        
        # Should be identical
        assert operator_form == canonical_form
    
    def test_disjunction_canonical_equivalence(self):
        """Test 'a; b' is equivalent to ';'(a, b)."""
        reader = Reader()
        
        # Parse operator form
        operator_form = reader.read_term("a; b")
        
        # Parse canonical form
        canonical_form = reader.read_term("';'(a, b)")
        
        # Should be identical
        assert operator_form == canonical_form
    
    def test_if_then_canonical_equivalence(self):
        """Test 'a -> b' is equivalent to '->'(a, b)."""
        reader = Reader()
        
        # Parse operator form
        operator_form = reader.read_term("a -> b")
        
        # Parse canonical form
        canonical_form = reader.read_term("'->'(a, b)")
        
        # Should be identical
        assert operator_form == canonical_form
    
    def test_complex_canonical_equivalence(self):
        """Test complex expression equivalence to canonical form."""
        reader = Reader()
        
        # Parse operator form: a, b -> c; d
        operator_form = reader.read_term("a, b -> c; d")
        
        # Parse canonical form: ';'('->'(','(a, b), c), d)
        canonical_form = reader.read_term("';'('->'(','(a, b), c), d)")
        
        # Should be identical
        assert operator_form == canonical_form
    
    def test_if_then_else_then_conjunction_canonical_equivalence(self):
        """Test if-then-else followed by conjunction equivalence."""
        reader = Reader()
        
        # Parse operator form: (a -> b; c), d
        operator_form = reader.read_term("(a -> b; c), d")
        
        # Parse canonical form: ','(';'('->'(a,b), c), d)
        canonical_form = reader.read_term("','(';'('->'(a,b), c), d)")
        
        # Should be identical
        assert operator_form == canonical_form


class TestEdgeCases:
    """Test edge cases and error conditions."""
    
    def test_empty_conjunction_not_allowed(self):
        """Empty conjunction should not parse."""
        reader = Reader()
        with pytest.raises(ReaderError):
            reader.read_term(",")
    
    def test_empty_disjunction_not_allowed(self):
        """Empty disjunction should not parse."""
        reader = Reader()
        with pytest.raises(ReaderError):
            reader.read_term(";")
    
    def test_empty_if_then_not_allowed(self):
        """Empty if-then should not parse."""
        reader = Reader()
        with pytest.raises(ReaderError):
            reader.read_term("->")
    
    def test_trailing_conjunction_not_allowed(self):
        """Trailing conjunction should not parse."""
        reader = Reader()
        with pytest.raises(ReaderError):
            reader.read_term("a, b,")
    
    def test_if_without_then_not_allowed(self):
        """If without then should not parse."""
        reader = Reader()
        with pytest.raises(ReaderError):
            reader.read_term("a ->")
    
    @pytest.mark.parametrize("src", ["a , , b", "a ; ; b", "a -> ; b", "a ; -> b"])
    def test_malformed_operator_sequences_error(self, src):
        """Test malformed operator sequences produce errors with position info."""
        reader = Reader()
        with pytest.raises(ReaderError) as exc_info:
            reader.read_term(src)
        error = exc_info.value
        # Should have position/column info
        assert error.position is not None or error.column is not None
        # Should identify the problematic token
        assert error.lexeme in {",", ";", "->"} or error.token in {",", ";", "->"}
    
    def test_deeply_nested_control_flow(self):
        """Test deeply nested control flow structures parse correctly."""
        reader = Reader()
        
        # Deep nesting with all three operators
        text = "((a -> b; c), d); (e, (f -> g; h))"
        result = reader.read_term(text)
        
        # Verify it's a disjunction at the top level
        assert isinstance(result, Struct)
        assert result.functor == ";"
        
        # Left side should be a conjunction
        left = result.args[0]
        assert isinstance(left, Struct)
        assert left.functor == ","
        
        # Right side should also be a conjunction
        right = result.args[1]
        assert isinstance(right, Struct)
        assert right.functor == ","
    
    @pytest.mark.parametrize("tight,spaced", [
        ("a,b;c", "a , b ; c"),
        ("a->b;c", "a -> b ; c"),
        ("a,(b;c)", "a, (b ; c)"),
        ("(a->b),c", "(a -> b) , c"),
    ])
    def test_whitespace_insensitivity(self, tight, spaced):
        """Test that whitespace doesn't affect parsing."""
        reader = Reader()
        assert reader.read_term(tight) == reader.read_term(spaced)
    
    def test_commas_in_functor_and_list_are_separators(self):
        """Test that commas inside functors and lists are separators, not operators."""
        reader = Reader()
        # In structures: comma is separator
        result = reader.read_term("f(a,b,c)")
        assert result == Struct("f", (Atom("a"), Atom("b"), Atom("c")))
        
        # In lists: comma is separator 
        result = reader.read_term("[a,b,c]")
        from prolog.ast.terms import List as PrologList
        assert result == PrologList((Atom("a"), Atom("b"), Atom("c")), Atom("[]"))
    
    def test_right_assoc_chains_mixed(self):
        """Test mixed right-associative chains."""
        reader = Reader()
        result = reader.read_term("a -> b -> c ; d -> e")
        expected = Struct(";", (
            Struct("->", (Atom("a"), Struct("->", (Atom("b"), Atom("c"))))),
            Struct("->", (Atom("d"), Atom("e")))
        ))
        assert result == expected