"""Tests for functor/3 builtin.

Tests functor/arity extraction and construction for Stage 1.

Error policy: dev-mode. Ill-typed/insufficiently instantiated calls to functor/3 
fail (return empty solution set) rather than throwing ISO errors.
"""

from prolog.ast.terms import Atom, Var, Struct, Int, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, program


class TestFunctorExtraction:
    """Test extracting functor and arity from terms."""
    
    def test_extract_from_structure(self):
        """Test functor(foo(a,b), F, A) binds F=foo, A=2."""
        engine = Engine(program())
        
        # Query: functor(foo(a,b), F, A)
        results = engine.run([
            Struct("functor", (
                Struct("foo", (Atom("a"), Atom("b"))),
                Var(0, "F"),
                Var(1, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Atom("foo")
        assert solution["A"] == Int(2)
    
    def test_extract_from_atom(self):
        """Test functor(foo, F, A) binds F=foo, A=0."""
        engine = Engine(program())
        
        # Query: functor(foo, F, A)
        results = engine.run([
            Struct("functor", (
                Atom("foo"),
                Var(0, "F"),
                Var(1, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Atom("foo")
        assert solution["A"] == Int(0)
    
    def test_extract_from_integer(self):
        """Test functor(42, F, A) binds F=42, A=0."""
        engine = Engine(program())
        
        # Query: functor(42, F, A)
        results = engine.run([
            Struct("functor", (
                Int(42),
                Var(0, "F"),
                Var(1, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Int(42)
        assert solution["A"] == Int(0)
    
    def test_extract_from_empty_list(self):
        """Test functor([], F, A) binds F=[], A=0."""
        engine = Engine(program())
        
        # Query: functor([], F, A)
        results = engine.run([
            Struct("functor", (
                Atom("[]"),
                Var(0, "F"),
                Var(1, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Atom("[]")
        assert solution["A"] == Int(0)
    
    def test_extract_from_list(self):
        """Test functor([a,b], F, A) binds F='.', A=2."""
        engine = Engine(program())
        
        # Query: functor([a,b], F, A)
        lst = List((Atom("a"), Atom("b")), Atom("[]"))
        results = engine.run([
            Struct("functor", (
                lst,
                Var(0, "F"),
                Var(1, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Atom(".")
        assert solution["A"] == Int(2)
    
    def test_extract_fails_with_unbound_first_arg(self):
        """Test functor(X, F, A) fails when X is unbound."""
        engine = Engine(program())
        
        # Query: functor(X, F, A) with X unbound
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Var(1, "F"),
                Var(2, "A")
            ))
        ])
        
        # Should fail in extraction mode with unbound first arg
        assert len(results) == 0


class TestFunctorConstruction:
    """Test constructing terms from functor and arity."""
    
    def test_construct_structure(self):
        """Test functor(X, foo, 2) binds X to foo(_,_)."""
        engine = Engine(program())
        
        # Query: functor(X, foo, 2)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Int(2)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        x = solution["X"]
        assert isinstance(x, Struct)
        assert x.functor == "foo"
        assert len(x.args) == 2
        # Arguments should be unbound variables
        assert all(isinstance(arg, Var) for arg in x.args)
    
    def test_construct_atom(self):
        """Test functor(X, foo, 0) binds X to foo."""
        engine = Engine(program())
        
        # Query: functor(X, foo, 0)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Int(0)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["X"] == Atom("foo")
    
    def test_construct_integer(self):
        """Test functor(X, 42, 0) binds X to 42."""
        engine = Engine(program())
        
        # Query: functor(X, 42, 0)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Int(42),
                Int(0)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["X"] == Int(42)
    
    def test_construct_empty_list(self):
        """Test functor(X, [], 0) binds X to []."""
        engine = Engine(program())
        
        # Query: functor(X, [], 0)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("[]"),
                Int(0)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["X"] == Atom("[]")
    
    def test_construct_fails_negative_arity(self):
        """Test functor(X, foo, -1) fails."""
        engine = Engine(program())
        
        # Query: functor(X, foo, -1)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Int(-1)
            ))
        ])
        
        # Should fail with negative arity
        assert len(results) == 0
    
    def test_construct_fails_non_atom_functor_positive_arity(self):
        """Test functor(X, 42, 2) fails (non-atom functor with arity > 0)."""
        engine = Engine(program())
        
        # Query: functor(X, 42, 2)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Int(42),
                Int(2)
            ))
        ])
        
        # Should fail - integers can't have positive arity
        assert len(results) == 0
    
    def test_construct_with_unbound_functor(self):
        """Test functor(X, F, 2) fails when F is unbound."""
        engine = Engine(program())
        
        # Query: functor(X, F, 2) with F unbound
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Var(1, "F"),
                Int(2)
            ))
        ])
        
        # Should fail - functor must be instantiated for construction
        assert len(results) == 0


class TestFunctorChecking:
    """Test checking mode where all arguments are bound."""
    
    def test_check_structure_success(self):
        """Test functor(foo(a,b), foo, 2) succeeds."""
        engine = Engine(program())
        
        # Query: functor(foo(a,b), foo, 2)
        results = engine.run([
            Struct("functor", (
                Struct("foo", (Atom("a"), Atom("b"))),
                Atom("foo"),
                Int(2)
            ))
        ])
        
        assert len(results) == 1  # Should succeed
    
    def test_check_structure_wrong_functor(self):
        """Test functor(foo(a,b), bar, 2) fails."""
        engine = Engine(program())
        
        # Query: functor(foo(a,b), bar, 2)
        results = engine.run([
            Struct("functor", (
                Struct("foo", (Atom("a"), Atom("b"))),
                Atom("bar"),
                Int(2)
            ))
        ])
        
        assert len(results) == 0  # Should fail
    
    def test_check_structure_wrong_arity(self):
        """Test functor(foo(a,b), foo, 3) fails."""
        engine = Engine(program())
        
        # Query: functor(foo(a,b), foo, 3)
        results = engine.run([
            Struct("functor", (
                Struct("foo", (Atom("a"), Atom("b"))),
                Atom("foo"),
                Int(3)
            ))
        ])
        
        assert len(results) == 0  # Should fail
    
    def test_check_atom_success(self):
        """Test functor(foo, foo, 0) succeeds."""
        engine = Engine(program())
        
        # Query: functor(foo, foo, 0)
        results = engine.run([
            Struct("functor", (
                Atom("foo"),
                Atom("foo"),
                Int(0)
            ))
        ])
        
        assert len(results) == 1  # Should succeed
    
    def test_check_integer_success(self):
        """Test functor(42, 42, 0) succeeds."""
        engine = Engine(program())
        
        # Query: functor(42, 42, 0)
        results = engine.run([
            Struct("functor", (
                Int(42),
                Int(42),
                Int(0)
            ))
        ])
        
        assert len(results) == 1  # Should succeed


class TestFunctorDeterminism:
    """Test that functor/3 is deterministic (semidet)."""
    
    def test_no_choicepoint_on_extraction(self):
        """Test functor/3 doesn't leave choicepoints on extraction."""
        engine = Engine(program())
        
        # Query: functor(foo(a), F, A), fail
        results = engine.run([
            Struct(",", (
                Struct("functor", (
                    Struct("foo", (Atom("a"),)),
                    Var(0, "F"),
                    Var(1, "A")
                )),
                Atom("fail")
            ))
        ])
        
        # Should fail cleanly without errors
        assert len(results) == 0
    
    def test_no_choicepoint_on_construction(self):
        """Test functor/3 doesn't leave choicepoints on construction."""
        prog = program(mk_fact("test"))
        engine = Engine(prog)
        
        # Query: (functor(X, foo, 1) ; true), test
        results = engine.run([
            Struct(",", (
                Struct(";", (
                    Struct("functor", (
                        Var(0, "X"),
                        Atom("foo"),
                        Int(1)
                    )),
                    Atom("true")
                )),
                Atom("test")
            ))
        ])
        
        # Should get exactly two solutions (one from functor, one from true)
        assert len(results) == 2
        # First solution should have X bound to foo(_)
        assert "X" in results[0]
        assert isinstance(results[0]["X"], Struct)
        # Second solution from true branch
        assert "X" in results[1]


class TestFunctorWithVariables:
    """Test functor/3 with partially instantiated arguments."""
    
    def test_extract_unifies_functor(self):
        """Test functor(foo(a), foo, A) binds A=1."""
        engine = Engine(program())
        
        # Query: functor(foo(a), foo, A)
        results = engine.run([
            Struct("functor", (
                Struct("foo", (Atom("a"),)),
                Atom("foo"),
                Var(0, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["A"] == Int(1)
    
    def test_extract_unifies_arity(self):
        """Test functor(foo(a,b), F, 2) binds F=foo."""
        engine = Engine(program())
        
        # Query: functor(foo(a,b), F, 2)
        results = engine.run([
            Struct("functor", (
                Struct("foo", (Atom("a"), Atom("b"))),
                Var(0, "F"),
                Int(2)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Atom("foo")
    
    def test_construct_with_variable_arity(self):
        """Test functor(X, foo, N) with N bound later."""
        engine = Engine(program())
        
        # Query: N = 2, functor(X, foo, N)
        results = engine.run([
            Struct(",", (
                Struct("=", (Var(1, "N"), Int(2))),
                Struct("functor", (
                    Var(0, "X"),
                    Atom("foo"),
                    Var(1, "N")
                ))
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["N"] == Int(2)
        x = solution["X"]
        assert isinstance(x, Struct)
        assert x.functor == "foo"
        assert len(x.args) == 2


class TestFunctorEdgeCases:
    """Test edge cases and special behaviors."""
    
    def test_quoted_functor(self):
        """Test functor with quoted atom functor."""
        engine = Engine(program())
        
        # Query: functor(X, 'foo bar', 2)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo bar"),
                Int(2)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        x = solution["X"]
        assert isinstance(x, Struct)
        assert x.functor == "foo bar"
        assert len(x.args) == 2
    
    def test_construct_zero_arity_struct_becomes_atom(self):
        """Test that constructing with arity 0 gives an atom, not a structure."""
        engine = Engine(program())
        
        # Query: functor(X, foo, 0)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Int(0)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        # X should be an atom, not a 0-arity structure
        assert solution["X"] == Atom("foo")
        assert isinstance(solution["X"], Atom)
        assert not isinstance(solution["X"], Struct)
    
    def test_max_arity(self):
        """Test construction with large arity."""
        engine = Engine(program())
        
        # Query: functor(X, foo, 10)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Int(10)
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        x = solution["X"]
        assert isinstance(x, Struct)
        assert x.functor == "foo"
        assert len(x.args) == 10
        # All arguments should be distinct unbound variables
        assert all(isinstance(arg, Var) for arg in x.args)
    
    def test_list_special_case(self):
        """Test that lists are treated specially with dot functor."""
        engine = Engine(program())
        
        # Query: functor([a], F, A)
        lst = List((Atom("a"),), Atom("[]"))
        results = engine.run([
            Struct("functor", (
                lst,
                Var(0, "F"),
                Var(1, "A")
            ))
        ])
        
        assert len(results) == 1
        solution = results[0]
        assert solution["F"] == Atom(".")
        assert solution["A"] == Int(2)


class TestFunctorWithNonIntegerArity:
    """Test functor/3 with non-integer arity arguments."""
    
    def test_non_integer_arity_fails(self):
        """Test functor(X, foo, bar) fails (non-integer arity)."""
        engine = Engine(program())
        
        # Query: functor(X, foo, bar)
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Atom("bar")
            ))
        ])
        
        # Should fail with non-integer arity
        assert len(results) == 0
    
    def test_unbound_arity_in_construction_fails(self):
        """Test functor(X, foo, N) fails when N is unbound."""
        engine = Engine(program())
        
        # Query: functor(X, foo, N) with N unbound
        results = engine.run([
            Struct("functor", (
                Var(0, "X"),
                Atom("foo"),
                Var(1, "N")
            ))
        ])
        
        # Should fail - arity must be instantiated for construction
        assert len(results) == 0