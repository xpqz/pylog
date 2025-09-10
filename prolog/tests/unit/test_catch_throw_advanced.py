"""
Advanced tests for catch/throw exception handling.
Tests edge cases and error conditions not covered in test_catch.py.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine, PrologThrow
from prolog.tests.helpers import program, mk_fact, mk_rule


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestThrowEdgeCases:
    """Edge case tests for throw/1."""

    def test_throw_with_complex_structure(self, empty_engine):
        """Test throwing complex structured terms."""
        # throw(error(type_error(integer, abc), context(foo/2, "message")))
        error_term = Struct("error", (
            Struct("type_error", (Atom("integer"), Atom("abc"))),
            Struct("context", (
                Struct("/", (Atom("foo"), Int(2))),
                Atom("message")
            ))
        ))
        
        X = Var(0, "X")
        query = Struct("catch", (
            Struct("throw", (error_term,)),
            X,
            Atom("true")
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == error_term

    def test_throw_with_list(self, empty_engine):
        """Test throwing a list term."""
        list_term = List((Int(1), Int(2), Int(3)))
        
        X = Var(0, "X")
        query = Struct("catch", (
            Struct("throw", (list_term,)),
            X,
            Atom("true")
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == list_term

    def test_throw_preserves_variable_bindings(self, empty_engine):
        """Test that throw preserves variable bindings in the ball."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        
        # X = 42, throw(data(X, Y))
        # The thrown term should have X bound to 42, Y unbound
        query = Struct("catch", (
            Struct(",", (
                Struct("=", (X, Int(42))),
                Struct("throw", (Struct("data", (X, Y)),))
            )),
            Z,
            Atom("true")
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # Z should unify with data(42, Y) where Y is still unbound
        caught = solutions[0]["Z"]
        assert isinstance(caught, Struct)
        assert caught.functor == "data"
        assert caught.args[0] == Int(42)

    def test_throw_wrong_arity(self, empty_engine):
        """Test throw with wrong number of arguments."""
        # throw() - no arguments - should fail, not throw
        # Since throw() fails as a builtin, the catch goal just fails
        query1 = Struct("catch", (
            Struct("throw", ()),
            Var(0, "X"),
            Atom("true")
        ))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0  # throw() fails, whole catch fails
        
        # throw(a, b) - too many arguments - should fail, not throw  
        query2 = Struct("catch", (
            Struct("throw", (Atom("a"), Atom("b"))),
            Var(1, "Y"),
            Atom("true")
        ))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0  # throw fails, whole catch fails


class TestCatchAdvancedPatterns:
    """Advanced pattern matching in catch/3."""

    def test_catch_with_partial_unification(self, empty_engine):
        """Test catch with partial pattern matching."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        
        # Throw error(foo, X), catch error(Y, details)
        query = Struct("catch", (
            Struct("throw", (Struct("error", (Atom("foo"), X)),)),
            Struct("error", (Y, Atom("details"))),
            Struct("=", (Y, Atom("caught")))
        ))
        solutions = empty_engine.run([query])
        # Should not catch - details doesn't match X
        assert len(solutions) == 0

    def test_catch_pattern_with_nested_variables(self, empty_engine):
        """Test catch pattern with nested variable structures."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        
        # Throw f(g(1)), catch f(g(X))
        query = Struct("catch", (
            Struct("throw", (Struct("f", (Struct("g", (Int(1),)),)),)),
            Struct("f", (Struct("g", (X,)),)),
            Struct("=", (Y, X))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(1)

    def test_catch_with_aliased_variables(self, empty_engine):
        """Test catch with variables that become aliased."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        
        # X = Y, throw(pair(X, Y)), catch pair(A, A)
        # Should catch because X and Y are aliased
        query = Struct("catch", (
            Struct(",", (
                Struct("=", (X, Y)),
                Struct("throw", (Struct("pair", (X, Y)),))
            )),
            Struct("pair", (Z, Z)),  # Pattern requires both args same
            Atom("true")
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1


class TestCatchRecoveryEdgeCases:
    """Edge cases in recovery goal execution."""

    def test_recovery_with_cut_and_choicepoints(self):
        """Test recovery goal with cut and multiple choicepoints."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("p", Atom("c")),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        
        # catch(throw(e), e, (p(X), !, p(Y)))
        # Cut in recovery should only affect recovery's choicepoints
        query = Struct("catch", (
            Struct("throw", (Atom("e"),)),
            Atom("e"),
            Struct(",", (
                Struct("p", (X,)),
                Struct(",", (
                    Atom("!"),
                    Struct("p", (Y,))
                ))
            ))
        ))
        solutions = engine.run([query])
        # X = a (first), cut, then Y gets all three values
        assert len(solutions) == 3
        assert all(s["X"] == Atom("a") for s in solutions)
        assert solutions[0]["Y"] == Atom("a")
        assert solutions[1]["Y"] == Atom("b")
        assert solutions[2]["Y"] == Atom("c")

    def test_recovery_throws_different_exception(self, empty_engine):
        """Test recovery goal throwing a different exception."""
        X = Var(0, "X")
        
        # catch(catch(throw(e1), e1, throw(e2)), e2, X = caught)
        query = Struct("catch", (
            Struct("catch", (
                Struct("throw", (Atom("e1"),)),
                Atom("e1"),
                Struct("throw", (Atom("e2"),))
            )),
            Atom("e2"),
            Struct("=", (X, Atom("caught")))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("caught")

    def test_recovery_with_infinite_loop_potential(self):
        """Test recovery that could cause infinite loop if not careful."""
        p = program(
            mk_rule("loop", (Var(0, "X"),), Struct("loop", (Var(0, "X"),))),
        )
        engine = Engine(p)
        engine.max_steps = 100  # Limit execution steps
        
        X = Var(0, "X")
        # catch(throw(e), e, loop(X))
        # Should hit step limit rather than infinite loop
        query = Struct("catch", (
            Struct("throw", (Atom("e"),)),
            Atom("e"),
            Struct("loop", (X,))
        ))
        solutions = engine.run([query])
        assert len(solutions) == 0  # Fails due to step limit


class TestNestedCatchComplex:
    """Complex nested catch scenarios."""

    def test_triple_nested_catch(self, empty_engine):
        """Test three levels of nested catch."""
        # catch(
        #   catch(
        #     catch(throw(e3), e1, fail),
        #     e2,
        #     fail
        #   ),
        #   e3,
        #   true
        # )
        query = Struct("catch", (
            Struct("catch", (
                Struct("catch", (
                    Struct("throw", (Atom("e3"),)),
                    Atom("e1"),
                    Atom("fail")
                )),
                Atom("e2"),
                Atom("fail")
            )),
            Atom("e3"),
            Atom("true")
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_catch_with_multiple_recovery_solutions(self):
        """Test catch where recovery goal has multiple solutions."""
        p = program(
            mk_fact("recover", Int(1)),
            mk_fact("recover", Int(2)),
            mk_fact("recover", Int(3)),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        # catch(throw(e), e, recover(X))
        query = Struct("catch", (
            Struct("throw", (Atom("e"),)),
            Atom("e"),
            Struct("recover", (X,))
        ))
        solutions = engine.run([query])
        assert len(solutions) == 3
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
        assert solutions[2]["X"] == Int(3)


class TestCatchThrowWithOnce:
    """Test interaction between catch/throw and once/1."""

    def test_once_inside_catch(self, empty_engine):
        """Test once/1 inside catch goal."""
        p = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        # catch(once(p(X)), Y, fail)
        query = Struct("catch", (
            Struct("once", (Struct("p", (X,)),)),
            Y,
            Atom("fail")
        ))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)

    def test_catch_inside_once(self, empty_engine):
        """Test catch/3 inside once/1."""
        X = Var(0, "X")
        # once(catch(throw(e), e, X = 1))
        query = Struct("once", (
            Struct("catch", (
                Struct("throw", (Atom("e"),)),
                Atom("e"),
                Struct("=", (X, Int(1)))
            )),
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)

    def test_throw_inside_once(self, empty_engine):
        """Test throw inside once/1 is still caught."""
        X = Var(0, "X")
        # catch(once(throw(e)), e, X = caught)
        query = Struct("catch", (
            Struct("once", (Struct("throw", (Atom("e"),)),)),
            Atom("e"),
            Struct("=", (X, Atom("caught")))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("caught")