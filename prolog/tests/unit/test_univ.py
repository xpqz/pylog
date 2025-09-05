"""Tests for =../2 (univ) builtin.

Tests structure ↔ list conversion for Stage 1.

Error policy: dev-mode. Ill-typed/insufficiently instantiated calls to =../2 
fail (return None) rather than throwing ISO errors. ISO errors may be introduced 
in a later stage.
"""

from prolog.ast.terms import Atom, Var, Struct, Int, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, program


class TestUnivDecomposition:
    """Test decomposing structures into lists."""
    
    def test_decompose_structure(self):
        """Test foo(a,b,c) =.. [foo,a,b,c]."""
        engine = Engine(program())
        
        # Query: foo(a,b,c) =.. L
        result = engine.run([
            Struct("=..", (
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Var(0, "L")
            ))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "L" in solution
        # L should be [foo, a, b, c]
        lst = solution["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 4
        assert lst.items[0] == Atom("foo")
        assert lst.items[1] == Atom("a")
        assert lst.items[2] == Atom("b")
        assert lst.items[3] == Atom("c")
        assert lst.tail == Atom("[]")
    
    def test_decompose_atom(self):
        """Test foo =.. [foo] (atom)."""
        engine = Engine(program())
        
        # Query: foo =.. L
        result = engine.run([
            Struct("=..", (Atom("foo"), Var(0, "L")))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "L" in solution
        # L should be [foo]
        lst = solution["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 1
        assert lst.items[0] == Atom("foo")
        assert lst.tail == Atom("[]")
    
    def test_decompose_atomic_integer(self):
        """Test 42 =.. [42] (atomic term)."""
        engine = Engine(program())
        
        # Query: 42 =.. L
        result = engine.run([
            Struct("=..", (Int(42), Var(0, "L")))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "L" in solution
        # L should be [42]
        lst = solution["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 1
        assert lst.items[0] == Int(42)
        assert lst.tail == Atom("[]")
    
    def test_decompose_empty_list(self):
        """Test [] =.. [[]] (empty list special case)."""
        engine = Engine(program())
        
        # Query: [] =.. L
        result = engine.run([
            Struct("=..", (Atom("[]"), Var(0, "L")))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "L" in solution
        # L should be [[]]
        lst = solution["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 1
        assert lst.items[0] == Atom("[]")
        assert lst.tail == Atom("[]")
    
    def test_decompose_list_structure(self):
        """Test [a,b] =.. ['.', a, [b]] (list as . structure)."""
        engine = Engine(program())
        
        # Query: [a,b] =.. L
        lst_ab = List((Atom("a"), Atom("b")), Atom("[]"))
        result = engine.run([
            Struct("=..", (lst_ab, Var(0, "L")))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "L" in solution
        # L should be ['.', a, [b]]
        lst = solution["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 3
        assert lst.items[0] == Atom(".")
        assert lst.items[1] == Atom("a")
        # Third item should be [b]
        assert isinstance(lst.items[2], List)
        assert len(lst.items[2].items) == 1
        assert lst.items[2].items[0] == Atom("b")
    
    def test_decompose_fails_with_unbound_left(self):
        """Test X =.. L fails when X is unbound."""
        engine = Engine(program())
        
        # Query: X =.. L (both unbound)
        result = engine.run([
            Struct("=..", (Var(0, "X"), Var(1, "L")))
        ])
        
        # Should fail
        assert len(result) == 0


class TestUnivConstruction:
    """Test constructing structures from lists."""
    
    def test_construct_structure(self):
        """Test X =.. [foo,a,b] binds X to foo(a,b)."""
        engine = Engine(program())
        
        # Query: X =.. [foo, a, b]
        lst = List((Atom("foo"), Atom("a"), Atom("b")), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        # X should be foo(a, b)
        x = result[0]["X"]
        assert isinstance(x, Struct)
        assert x.functor == "foo"
        assert len(x.args) == 2
        assert x.args[0] == Atom("a")
        assert x.args[1] == Atom("b")
    
    def test_construct_atom(self):
        """Test X =.. [foo] binds X to foo (atom)."""
        engine = Engine(program())
        
        # Query: X =.. [foo]
        lst = List((Atom("foo"),), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        # X should be foo (atom)
        assert result[0]["X"] == Atom("foo")
    
    def test_construct_integer(self):
        """Test X =.. [42] binds X to 42."""
        engine = Engine(program())
        
        # Query: X =.. [42]
        lst = List((Int(42),), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        # X should be 42
        assert result[0]["X"] == Int(42)
    
    def test_construct_empty_list(self):
        """Test X =.. [[]] binds X to []."""
        engine = Engine(program())
        
        # Query: X =.. [[]]
        lst = List((Atom("[]"),), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        # X should be []
        assert result[0]["X"] == Atom("[]")
    
    def test_construct_list_from_dot(self):
        """Test X =.. ['.', a, [b]] binds X to [a,b]."""
        engine = Engine(program())
        
        # Query: X =.. ['.', a, [b]]
        tail = List((Atom("b"),), Atom("[]"))
        lst = List((Atom("."), Atom("a"), tail), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        # X should be [a, b]
        x = result[0]["X"]
        assert isinstance(x, List)
        assert len(x.items) == 2
        assert x.items[0] == Atom("a")
        assert x.items[1] == Atom("b")
        assert x.tail == Atom("[]")
    
    def test_construct_fails_with_non_list(self):
        """Test X =.. foo fails (right side must be list)."""
        engine = Engine(program())
        
        # Query: X =.. foo (non-list on right)
        result = engine.run([
            Struct("=..", (Var(0, "X"), Atom("foo")))
        ])
        
        # Should fail
        assert len(result) == 0
    
    def test_construct_fails_with_empty_list(self):
        """Test X =.. [] fails (empty list on right)."""
        engine = Engine(program())
        
        # Query: X =.. []
        lst = List((), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        # Should fail
        assert len(result) == 0
    
    def test_construct_fails_atomic_with_args(self):
        """Test X =.. [42, a] fails (atomic terms cannot have arguments)."""
        engine = Engine(program())
        
        # Query: X =.. [42, a]
        lst = List((Int(42), Atom("a")), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        # Should fail (atomic terms cannot have arguments)
        assert len(result) == 0


class TestUnivBidirectional:
    """Test bidirectional unification in =../2."""
    
    def test_bidirectional_unification_observable(self):
        """Test foo(X) =.. [foo, Y], Y = a unifies X with a."""
        engine = Engine(program())
        
        # Query: foo(X) =.. [foo, Y], Y = a
        result = engine.run([
            Struct(",", (
                Struct("=..", (
                    Struct("foo", (Var(0, "X"),)),
                    List((Atom("foo"), Var(1, "Y")), Atom("[]"))
                )),
                Struct("=", (Var(1, "Y"), Atom("a")))
            ))
        ])
        
        assert len(result) > 0
        assert result[0]["Y"] == Atom("a")
        # X should also be a since unified with Y
        x = result[0]["X"]
        assert x == Atom("a")
    
    def test_check_mode(self):
        """Test foo(a,b) =.. [foo,a,b] succeeds (checking mode)."""
        engine = Engine(program())
        
        # Query: foo(a,b) =.. [foo,a,b]
        lst = List((Atom("foo"), Atom("a"), Atom("b")), Atom("[]"))
        result = engine.run([
            Struct("=..", (
                Struct("foo", (Atom("a"), Atom("b"))),
                lst
            ))
        ])
        
        assert len(result) > 0  # Should succeed
    
    def test_check_mode_fails(self):
        """Test foo(a,b) =.. [bar,a,b] fails."""
        engine = Engine(program())
        
        # Query: foo(a,b) =.. [bar,a,b]
        lst = List((Atom("bar"), Atom("a"), Atom("b")), Atom("[]"))
        result = engine.run([
            Struct("=..", (
                Struct("foo", (Atom("a"), Atom("b"))),
                lst
            ))
        ])
        
        assert len(result) == 0  # Should fail
    
    def test_partial_structure_with_unbound_functor(self):
        """Test Struct =.. [F|Args] with unbound F."""
        engine = Engine(program())
        
        # Query: foo(a,b) =.. [F|Args]
        result = engine.run([
            Struct("=..", (
                Struct("foo", (Atom("a"), Atom("b"))),
                List((Var(0, "F"),), Var(1, "Args"))
            ))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "F" in solution
        assert "Args" in solution
        assert solution["F"] == Atom("foo")
        # Args should be [a, b]
        args = solution["Args"]
        assert isinstance(args, List)
        assert len(args.items) == 2
        assert args.items[0] == Atom("a")
        assert args.items[1] == Atom("b")
        assert args.tail == Atom("[]")


class TestUnivDeterminism:
    """Test that =../2 is semidet (at most one solution)."""
    
    def test_no_choicepoint_on_success(self):
        """Test =../2 doesn't leave choicepoints on success."""
        engine = Engine(program())
        
        # Query: foo(a) =.. L, fail
        # If =../2 leaves a choicepoint, we'd backtrack into it
        result = engine.run([
            Struct(",", (
                Struct("=..", (
                    Struct("foo", (Atom("a"),)),
                    Var(0, "L")
                )),
                Atom("fail")
            ))
        ])
        
        # Should fail (due to fail/0) without errors  
        assert len(result) == 0
    
    def test_deterministic_construction(self):
        """Test construction mode is deterministic."""
        prog = program(mk_fact("counter"))
        engine = Engine(prog)
        
        # Query: (X =.. [foo, a] ; true), counter
        # This should give exactly two solutions if =../2 is deterministic:
        # 1. X = foo(a) with counter
        # 2. true with counter (from ; branch)
        results = engine.run([
            Struct(",", (
                Struct(";", (
                    Struct("=..", (Var(0, "X"), List((Atom("foo"), Atom("a")), Atom("[]")))),
                    Atom("true")
                )),
                Atom("counter")
            ))
        ])
        
        # Should get exactly two solutions
        assert len(results) == 2
        
        # First solution from =../2
        assert "X" in results[0]
        assert isinstance(results[0]["X"], Struct)
        assert results[0]["X"].functor == "foo"
        
        # Second solution from true branch (X remains a variable)
        # The true branch doesn't bind X, so it stays as a variable
        assert "X" in results[1]


class TestUnivTrailing:
    """Test that =../2 properly trails all bindings."""
    
    def test_trailing_on_backtrack(self):
        """Test bindings are properly undone on backtrack."""
        prog = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b"))
        )
        engine = Engine(prog)
        
        # Query: p(Y), X =.. [foo, Y]
        # This should bind X to foo(a), then backtrack and bind to foo(b)
        results = engine.run([
            Struct(",", (
                Struct("p", (Var(1, "Y"),)),
                Struct("=..", (Var(0, "X"), List((Atom("foo"), Var(1, "Y")), Atom("[]")))) 
            ))
        ])
        
        # Should get two solutions via backtracking
        assert len(results) == 2
        
        # First solution
        assert results[0]["Y"] == Atom("a")
        assert isinstance(results[0]["X"], Struct)
        assert results[0]["X"].functor == "foo"
        assert results[0]["X"].args[0] == Atom("a")
        
        # Second solution (after backtracking)
        assert results[1]["Y"] == Atom("b")
        assert isinstance(results[1]["X"], Struct)
        assert results[1]["X"].functor == "foo"
        assert results[1]["X"].args[0] == Atom("b")
    
    def test_trailing_undo_on_fail_then_redo(self):
        """Test bindings are undone when =.. succeeds but later goal fails."""
        prog = program(
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b"))
        )
        engine = Engine(prog)
        
        # Query: (q(Y), X =.. [foo, Y], fail) ; true
        # This should undo all bindings and reach true branch
        result = engine.run([
            Struct(";", (
                Struct(",", (
                    Struct(",", (
                        Struct("q", (Var(1, "Y"),)),
                        Struct("=..", (Var(0, "X"), List((Atom("foo"), Var(1, "Y")), Atom("[]")))),
                    )),
                    Atom("fail")
                )),
                Atom("true")
            ))
        ])
        
        assert len(result) > 0  # Should succeed via true branch


class TestUnivEdgeCases:
    """Test edge cases and error conditions."""
    
    def test_improper_list_construction_via_dot(self):
        """Test X =.. ['.', a, 2] constructs improper list [a|2]."""
        engine = Engine(program())
        
        # Query: X =.. ['.', a, 2]
        lst = List((Atom("."), Atom("a"), Int(2)), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        # X should be [a|2] (improper list)
        x = solution["X"]
        assert isinstance(x, List)
        assert len(x.items) == 1
        assert x.items[0] == Atom("a")
        assert x.tail == Int(2)  # Non-list tail makes it improper
    
    def test_improper_list_on_right(self):
        """Test X =.. [foo|bar] fails (improper list)."""
        engine = Engine(program())
        
        # Query: X =.. [foo|bar]
        lst = List((Atom("foo"),), Atom("bar"))  # Improper list
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        # Should fail (right side must be proper list)
        assert len(result) == 0
    
    def test_nested_structures(self):
        """Test decomposing nested structures."""
        engine = Engine(program())
        
        # Query: foo(bar(a), b) =.. L
        result = engine.run([
            Struct("=..", (
                Struct("foo", (Struct("bar", (Atom("a"),)), Atom("b"))),
                Var(0, "L")
            ))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "L" in solution
        lst = solution["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 3
        assert lst.items[0] == Atom("foo")
        # First argument should be bar(a) structure
        assert isinstance(lst.items[1], Struct)
        assert lst.items[1].functor == "bar"
        assert lst.items[2] == Atom("b")
        assert lst.tail == Atom("[]")
    
    def test_decompose_list_nested_dot_shape(self):
        """Test [a,b] decomposes to ['.', a, [b]] with proper nested structure."""
        engine = Engine(program())
        
        # Create [a, b]
        lst = List((Atom("a"), Atom("b")), Atom("[]"))
        
        # Query: [a,b] =.. L
        result = engine.run([
            Struct("=..", (lst, Var(0, "L")))
        ])
        
        assert len(result) > 0
        L = result[0]["L"]
        assert isinstance(L, List)
        assert len(L.items) == 3
        assert L.items[0] == Atom(".")
        assert L.items[1] == Atom("a")
        
        # Third item should be [b] which is itself a list structure
        tail_list = L.items[2]
        assert isinstance(tail_list, List)
        assert len(tail_list.items) == 1
        assert tail_list.items[0] == Atom("b")
        assert tail_list.tail == Atom("[]")
    
    def test_construct_empty_list_via_quoted_atom(self):
        """Test X =.. ['[]'] binds X to []."""
        engine = Engine(program())
        
        # Query: X =.. [[]]
        lst = List((Atom("[]"),), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        assert result[0]["X"] == Atom("[]")
    
    def test_decompose_empty_list_equals_quoted_atom(self):
        """Test [] =.. L gives L = ['[]']."""
        engine = Engine(program())
        
        # Query: [] =.. L
        result = engine.run([
            Struct("=..", (Atom("[]"), Var(0, "L")))
        ])
        
        assert len(result) > 0
        L = result[0]["L"]
        assert isinstance(L, List)
        assert len(L.items) == 1
        assert L.items[0] == Atom("[]")
        assert L.tail == Atom("[]")
    
    def test_construct_quoted_functor_multiple_args(self):
        """Test X =.. ['foo bar', a, b] binds X to 'foo bar'(a, b)."""
        engine = Engine(program())
        
        # Query: X =.. ['foo bar', a, b]
        lst = List((Atom("foo bar"), Atom("a"), Atom("b")), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        solution = result[0]
        assert "X" in solution
        X = solution["X"]
        assert isinstance(X, Struct)
        assert X.functor == "foo bar"
        assert len(X.args) == 2
        assert X.args[0] == Atom("a")
        assert X.args[1] == Atom("b")
    
    def test_construct_quoted_functor(self):
        """Test X =.. ['foo bar', x] binds X to 'foo bar'(x)."""
        engine = Engine(program())
        
        # Query: X =.. ['foo bar', x]
        lst = List((Atom("foo bar"), Atom("x")), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert len(result) > 0
        X = result[0]["X"]
        assert isinstance(X, Struct)
        assert X.functor == "foo bar"
        assert len(X.args) == 1
        assert X.args[0] == Atom("x")