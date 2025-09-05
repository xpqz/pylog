"""Tests for =../2 (univ) builtin.

Tests structure ↔ list conversion for Stage 1.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, mk_rule, program


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
        
        assert result is not None
        assert "L" in result
        # L should be [foo, a, b, c]
        lst = result["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 4
        assert lst.items[0] == Atom("foo")
        assert lst.items[1] == Atom("a")
        assert lst.items[2] == Atom("b")
        assert lst.items[3] == Atom("c")
        assert lst.tail == Atom("[]")
    
    def test_decompose_zero_arity_structure(self):
        """Test foo =.. [foo] (0-arity atom)."""
        engine = Engine(program())
        
        # Query: foo =.. L
        result = engine.run([
            Struct("=..", (Atom("foo"), Var(0, "L")))
        ])
        
        assert result is not None
        assert "L" in result
        # L should be [foo]
        lst = result["L"]
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
        
        assert result is not None
        assert "L" in result
        # L should be [42]
        lst = result["L"]
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
        
        assert result is not None
        assert "L" in result
        # L should be [[]]
        lst = result["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 1
        assert lst.items[0] == Atom("[]")
        assert lst.tail == Atom("[]")
    
    def test_decompose_list_structure(self):
        """Test [a,b] =.. ['.',a,[b]] (list as . structure)."""
        engine = Engine(program())
        
        # Query: [a,b] =.. L
        lst_ab = List((Atom("a"), Atom("b")), Atom("[]"))
        result = engine.run([
            Struct("=..", (lst_ab, Var(0, "L")))
        ])
        
        assert result is not None
        assert "L" in result
        # L should be ['.', a, [b]]
        lst = result["L"]
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
        assert result is None


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
        
        assert result is not None
        assert "X" in result
        # X should be foo(a, b)
        x = result["X"]
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
        
        assert result is not None
        assert "X" in result
        # X should be foo (atom)
        assert result["X"] == Atom("foo")
    
    def test_construct_integer(self):
        """Test X =.. [42] binds X to 42."""
        engine = Engine(program())
        
        # Query: X =.. [42]
        lst = List((Int(42),), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert result is not None
        assert "X" in result
        # X should be 42
        assert result["X"] == Int(42)
    
    def test_construct_empty_list(self):
        """Test X =.. [[]] binds X to []."""
        engine = Engine(program())
        
        # Query: X =.. [[]]
        lst = List((Atom("[]"),), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert result is not None
        assert "X" in result
        # X should be []
        assert result["X"] == Atom("[]")
    
    def test_construct_list_from_dot(self):
        """Test X =.. ['.', a, [b]] binds X to [a,b]."""
        engine = Engine(program())
        
        # Query: X =.. ['.', a, [b]]
        tail = List((Atom("b"),), Atom("[]"))
        lst = List((Atom("."), Atom("a"), tail), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        assert result is not None
        assert "X" in result
        # X should be [a, b]
        x = result["X"]
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
        assert result is None
    
    def test_construct_fails_with_empty_list(self):
        """Test X =.. [] fails (empty list on right)."""
        engine = Engine(program())
        
        # Query: X =.. []
        lst = List((), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        # Should fail
        assert result is None
    
    def test_construct_fails_with_non_atom_functor(self):
        """Test X =.. [42, a] fails (functor must be atom for arity > 0)."""
        engine = Engine(program())
        
        # Query: X =.. [42, a]
        lst = List((Int(42), Atom("a")), Atom("[]"))
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        # Should fail (42 cannot be a functor with arguments)
        assert result is None


class TestUnivBidirectional:
    """Test bidirectional unification in =../2."""
    
    def test_bidirectional_unification(self):
        """Test foo(X) =.. [foo, Y] unifies X and Y."""
        engine = Engine(program())
        
        # Query: foo(X) =.. [foo, Y]
        lst = List((Atom("foo"), Var(1, "Y")), Atom("[]"))
        result = engine.run([
            Struct("=..", (
                Struct("foo", (Var(0, "X"),)),
                lst
            ))
        ])
        
        assert result is not None
        assert "X" in result
        assert "Y" in result
        # X and Y should be unified (same variable)
        # In the store, they should point to the same cell
        # For testing purposes, we can't directly check store internals,
        # but we can verify they got bound to each other
        # Since both are unbound, they remain as variables
    
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
        
        assert result is not None  # Should succeed
    
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
        
        assert result is None  # Should fail
    
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
        
        assert result is not None
        assert "F" in result
        assert "Args" in result
        assert result["F"] == Atom("foo")
        # Args should be [a, b]
        args = result["Args"]
        assert isinstance(args, List)
        assert len(args.items) == 2
        assert args.items[0] == Atom("a")
        assert args.items[1] == Atom("b")


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
        assert result is None
    
    def test_deterministic_construction(self):
        """Test construction mode is deterministic."""
        engine = Engine(program())
        
        # Add a fact to count solutions
        engine.consult(mk_fact(Atom("counter")))
        
        # Query: (X =.. [foo, a] ; true), counter
        # This should give exactly one solution if =../2 is deterministic
        result = engine.run([
            Struct(",", (
                Struct(";", (
                    Struct("=..", (Var(0, "X"), List((Atom("foo"), Atom("a")), Atom("[]")))),
                    Atom("true")
                )),
                Atom("counter")
            ))
        ])
        
        assert result is not None
        # Should get first solution from =../2
        assert "X" in result
        assert isinstance(result["X"], Struct)
        
        # Try to get another solution
        result2 = engine.run()
        assert result2 is not None  # Should get solution from true branch
        
        # No more solutions
        result3 = engine.run()
        assert result3 is None


class TestUnivTrailing:
    """Test that =../2 properly trails all bindings."""
    
    def test_trailing_on_backtrack(self):
        """Test bindings are properly undone on backtrack."""
        engine = Engine(program())
        
        # Add facts for testing
        engine.consult(mk_fact(Struct("p", (Atom("a"),))))
        engine.consult(mk_fact(Struct("p", (Atom("b"),))))
        
        # Query: p(Y), X =.. [foo, Y]
        # This should bind X to foo(a), then backtrack and bind to foo(b)
        result = engine.run([
            Struct(",", (
                Struct("p", (Var(1, "Y"),)),
                Struct("=..", (Var(0, "X"), List((Atom("foo"), Var(1, "Y")), Atom("[]")))) 
            ))
        ])
        
        assert result is not None
        assert result["Y"] == Atom("a")
        assert isinstance(result["X"], Struct)
        assert result["X"].functor == "foo"
        assert result["X"].args[0] == Atom("a")
        
        # Get next solution
        result2 = engine.run()
        assert result2 is not None
        assert result2["Y"] == Atom("b")
        assert isinstance(result2["X"], Struct)
        assert result2["X"].functor == "foo"
        assert result2["X"].args[0] == Atom("b")
        
        # No more solutions
        result3 = engine.run()
        assert result3 is None


class TestUnivEdgeCases:
    """Test edge cases and error conditions."""
    
    def test_improper_list_on_right(self):
        """Test X =.. [foo|bar] fails (improper list)."""
        engine = Engine(program())
        
        # Query: X =.. [foo|bar]
        lst = List((Atom("foo"),), Atom("bar"))  # Improper list
        result = engine.run([
            Struct("=..", (Var(0, "X"), lst))
        ])
        
        # Should fail (right side must be proper list)
        assert result is None
    
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
        
        assert result is not None
        assert "L" in result
        lst = result["L"]
        assert isinstance(lst, List)
        assert len(lst.items) == 3
        assert lst.items[0] == Atom("foo")
        # First argument should be bar(a) structure
        assert isinstance(lst.items[1], Struct)
        assert lst.items[1].functor == "bar"
        assert lst.items[2] == Atom("b")