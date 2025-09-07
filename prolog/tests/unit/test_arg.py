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
        assert x.id == y.id


class TestArgAdditionalCases:
    """Additional test cases for comprehensive coverage."""
    
    def test_bound_index_succeeds_after_eq(self):
        """Test binding N before calling arg(N, ...) succeeds."""
        engine = Engine(program())
        
        # Query: N=1, then arg(N, f(a), X) → succeeds, X=a
        results = engine.run([
            Struct(",", (
                Struct("=", (Var(0, "N"), Int(1))),
                Struct("arg", (Var(0, "N"), Struct("f", (Atom("a"),)), Var(1, "X")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")
    
    def test_arg_on_dot_struct_succeeds(self):
        """Test arg/3 on explicit '.'/2 structure succeeds.
        
        Even though List objects are rejected, explicit dot structures work.
        This keeps semantics consistent.
        """
        engine = Engine(program())
        
        # Create explicit dot pair structure
        pair = Struct(".", (Atom("a"), Atom("[]")))
        
        # Query: arg(1, .(a,[]), X)
        results = engine.run([
            Struct("arg", (Int(1), pair, Var(0, "X")))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")
    
    def test_arg_then_other_goal_preserves_continuation(self):
        """Test arg/3 followed by another goal preserves continuation."""
        from prolog.tests.helpers import mk_fact
        
        engine = Engine(program(
            mk_fact("q")
        ))
        
        # Query: arg(1, f(a), X), q
        results = engine.run([
            Struct(",", (
                Struct("arg", (
                    Int(1),
                    Struct("f", (Atom("a"),)),
                    Var(0, "X")
                )),
                Atom("q")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")
    
    def test_arg_determinism_with_disjunction(self):
        """Test arg/3 is deterministic using disjunction."""
        engine = Engine(program())
        
        # Query: (arg(1, foo(a,b), X) ; true)
        # If arg/3 is deterministic, we get exactly 2 solutions (one from each branch)
        results = engine.run([
            Struct(";", (
                Struct("arg", (
                    Int(1),
                    Struct("foo", (Atom("a"), Atom("b"))),
                    Var(0, "X")
                )),
                Atom("true")
            ))
        ])
        
        # Should get 2 solutions: one from arg/3, one from true
        assert len(results) == 2
        # First solution has X bound to 'a'
        assert results[0]["X"] == Atom("a")
        # Second solution has X unbound (from true branch)
        assert isinstance(results[1].get("X", Var(0, "X")), Var)  # Should be the same variable


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
        """Test arg(1, [a,b], X) fails (lists aren't structures for arg/3).
        
        Stage-1 simplification: we do NOT treat lists as structures for arg/3.
        ISO-compatible behavior (arg(1,[a,b],X) -> X=a) will be enabled later.
        """
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
        
        # Query: arg(1, s9(...), X) where s9 contains s8 contains ... s0 contains d
        results = engine.run([
            Struct("arg", (
                Int(1),
                deep,
                Var(0, "X")
            ))
        ])
        
        assert len(results) == 1
        # Should extract the immediate argument (s8)
        x = results[0]["X"]
        assert isinstance(x, Struct)
        assert x.functor == "s8" and len(x.args) == 1
    
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


