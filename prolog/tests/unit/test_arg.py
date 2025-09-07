"""Tests for arg/3 builtin.

Tests argument extraction and unification for Stage 1.

Error policy: dev-mode. Ill-typed/insufficiently instantiated calls to arg/3
fail (return empty solution set) rather than throwing ISO errors. ISO error
behavior (instantiation_error, type_error, domain_error) to be added in later stages.
"""

from prolog.ast.terms import Atom, Var, Struct, Int, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program


class TestArgExtraction:
    """Test extracting arguments from structures."""
    
    def test_extract_first_arg(self):
        """Test arg(1, foo(a,b,c), X) binds X=a."""
        engine = Engine(program())
        
        # Query: arg(1, foo(a,b,c), X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")
    
    def test_extract_middle_arg(self):
        """Test arg(2, foo(a,b,c), X) binds X=b."""
        engine = Engine(program())
        
        # Query: arg(2, foo(a,b,c), X)
        results = engine.run([
            Struct("arg", (
                Int(2),
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("b")
    
    def test_extract_last_arg(self):
        """Test arg(3, foo(a,b,c), X) binds X=c."""
        engine = Engine(program())
        
        # Query: arg(3, foo(a,b,c), X)
        results = engine.run([
            Struct("arg", (
                Int(3),
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("c")
    
    def test_extract_from_unary_struct(self):
        """Test arg(1, f(x), X) binds X=x."""
        engine = Engine(program())
        
        # Query: arg(1, f(x), X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Struct("f", (Atom("x"),)),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("x")
    
    def test_extract_complex_arg(self):
        """Test arg(2, foo(a, bar(b,c), d), X) binds X=bar(b,c)."""
        engine = Engine(program())
        
        # Query: arg(2, foo(a, bar(b,c), d), X)
        results = engine.run([
            Struct("arg", (
                Int(2),
                Struct("foo", (
                    Atom("a"),
                    Struct("bar", (Atom("b"), Atom("c"))),
                    Atom("d")
                )),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Struct("bar", (Atom("b"), Atom("c")))
    
    def test_extract_variable_arg(self):
        """Test extracting an unbound variable argument."""
        engine = Engine(program())
        
        # Query: arg(1, f(Y), X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Struct("f", (Var(1, "Y"),)),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        # X and Y should be unified
        x = results[0]["X"]
        y = results[0]["Y"]
        assert isinstance(x, Var) and isinstance(y, Var)
        assert x.id == y.id  # Should be the same variable


class TestArgChecking:
    """Test checking mode where third argument is bound."""
    
    def test_check_success(self):
        """Test arg(2, foo(a,b,c), b) succeeds."""
        engine = Engine(program())
        
        # Query: arg(2, foo(a,b,c), b)
        results = engine.run([
            Struct("arg", (
                Int(2),
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Atom("b")
            ))
        ])
        
        assert len(results) == 1
    
    def test_check_failure(self):
        """Test arg(2, foo(a,b,c), x) fails."""
        engine = Engine(program())
        
        # Query: arg(2, foo(a,b,c), x)
        results = engine.run([
            Struct("arg", (
                Int(2),
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Atom("x")
            ))
        ])
        
        assert len(results) == 0
    
    def test_check_with_unification(self):
        """Test arg(1, f(X), a) binds X=a."""
        engine = Engine(program())
        
        # Query: arg(1, f(X), a)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Struct("f", (Var(0, "X"),)),
                Atom("a")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")


class TestArgBoundaryConditions:
    """Test boundary conditions and error cases."""
    
    def test_arg_zero_fails(self):
        """Test arg(0, foo(a,b), X) fails (args are 1-indexed)."""
        engine = Engine(program())
        
        # Query: arg(0, foo(a,b), X)
        results = engine.run([
            Struct("arg", (
                Int(0),
                Struct("foo", (Atom("a"), Atom("b"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0
    
    def test_arg_negative_fails(self):
        """Test arg(-1, foo(a,b), X) fails."""
        engine = Engine(program())
        
        # Query: arg(-1, foo(a,b), X)
        results = engine.run([
            Struct("arg", (
                Int(-1),
                Struct("foo", (Atom("a"), Atom("b"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0
    
    def test_arg_too_large_fails(self):
        """Test arg(4, foo(a,b,c), X) fails (out of bounds)."""
        engine = Engine(program())
        
        # Query: arg(4, foo(a,b,c), X)
        results = engine.run([
            Struct("arg", (
                Int(4),
                Struct("foo", (Atom("a"), Atom("b"), Atom("c"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0
    
    def test_arg_on_atom_fails(self):
        """Test arg(1, foo, X) fails (atoms have no arguments)."""
        engine = Engine(program())
        
        # Query: arg(1, foo, X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Atom("foo"),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0
    
    def test_arg_on_integer_fails(self):
        """Test arg(1, 42, X) fails (integers have no arguments)."""
        engine = Engine(program())
        
        # Query: arg(1, 42, X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Int(42),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0
    
    def test_arg_on_list_fails(self):
        """Test arg(1, [a,b], X) fails (lists aren't structures for arg/3)."""
        engine = Engine(program())
        
        # Query: arg(1, [a,b], X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                List((Atom("a"), Atom("b")), Atom("[]")),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0


class TestArgWithVariables:
    """Test arg/3 with unbound variables."""
    
    def test_unbound_index_fails(self):
        """Test arg(N, foo(a,b), X) with unbound N fails."""
        engine = Engine(program())
        
        # Query: arg(N, foo(a,b), X)
        results = engine.run([
            Struct("arg", (
                Var(0, "N"),
                Struct("foo", (Atom("a"), Atom("b"))),
                Var(1, "X")
            ))
        ])
        
        assert len(results) == 0  # Dev-mode: fail on insufficient instantiation
    
    def test_unbound_struct_fails(self):
        """Test arg(1, S, X) with unbound S fails."""
        engine = Engine(program())
        
        # Query: arg(1, S, X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Var(0, "S"),
                Var(1, "X")
            ))
        ])
        
        assert len(results) == 0  # Dev-mode: fail on insufficient instantiation
    
    def test_non_integer_index_fails(self):
        """Test arg(foo, bar(a), X) fails (index must be integer)."""
        engine = Engine(program())
        
        # Query: arg(foo, bar(a), X)
        results = engine.run([
            Struct("arg", (
                Atom("foo"),
                Struct("bar", (Atom("a"),)),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 0  # Dev-mode: fail on type error


class TestArgDeterminism:
    """Test that arg/3 is deterministic (at most one solution)."""
    
    def test_no_choicepoint_on_extraction(self):
        """Test arg/3 doesn't create unnecessary choicepoints."""
        engine = Engine(program())
        
        # Query: arg(1, foo(a,b), X)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Struct("foo", (Atom("a"), Atom("b"))),
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        # Should be semidet - no additional solutions on backtracking
    
    def test_deterministic_check(self):
        """Test arg/3 in checking mode is deterministic."""
        engine = Engine(program())
        
        # Query: arg(1, foo(a,b), a)
        results = engine.run([
            Struct("arg", (
                Int(1),
                Struct("foo", (Atom("a"), Atom("b"))),
                Atom("a")
            ))
        ])
        
        assert len(results) == 1


class TestArgEdgeCases:
    """Test edge cases and special scenarios."""
    
    def test_deeply_nested_extraction(self):
        """Test extracting from deeply nested structures."""
        engine = Engine(program())
        
        deep = Struct("d", (Atom("deep"),))
        for i in range(10):
            deep = Struct(f"s{i}", (deep,))
        
        # Query: arg(1, outermost, X) where outermost contains deep nesting
        results = engine.run([
            Struct("arg", (
                Int(1),
                deep,
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        # Should extract the immediate argument without recursion issues
    
    def test_arg_with_large_arity(self):
        """Test arg/3 with structures having many arguments."""
        engine = Engine(program())
        
        # Create structure with 100 arguments
        args = tuple(Atom(f"a{i}") for i in range(100))
        big_struct = Struct("big", args)
        
        # Query: arg(50, big(...), X)
        results = engine.run([
            Struct("arg", (
                Int(50),
                big_struct,
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a49")  # 1-indexed, so 50th arg is a49
    
    def test_arg_preserves_sharing(self):
        """Test that arg/3 preserves variable sharing."""
        engine = Engine(program())
        
        # Query: arg(1, f(X), Y), arg(2, g(X, Z), Y)
        # This should unify X and Y
        results = engine.run([
            Struct(",", (
                Struct("arg", (
                    Int(1),
                    Struct("f", (Var(0, "X"),)),
                    Var(1, "Y")
                )),
                Struct("arg", (
                    Int(2),
                    Struct("g", (Var(0, "X"), Var(2, "Z"))),
                    Var(1, "Y")
                ))
            ))
        ])
        
        assert len(results) == 1
        # X and Y should be unified
        x = results[0]["X"]
        y = results[0]["Y"]
        assert isinstance(x, Var) and isinstance(y, Var)
        assert x.id == y.id