"""Tests for catch/3 builtin.

Tests exception catching for Stage 1.

catch(Goal, Catcher, Recovery) executes Goal. If Goal throws an exception
that unifies with Catcher, Recovery is executed. Otherwise the exception
propagates.

Error policy: dev-mode. ISO error behavior to be added in later stages.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestCatchBasicBehavior:
    """Test basic catch/3 functionality."""
    
    def test_catch_with_no_exception(self):
        """Test catch(true, _, fail) succeeds normally."""
        engine = Engine(program())
        
        # Query: catch(true, _, fail)
        # Goal succeeds without throwing, so Recovery is not executed
        results = engine.run([
            Struct("catch", (
                Atom("true"),
                Var(0, "_"),
                Atom("fail")
            ))
        ])
        
        assert len(results) == 1
    
    def test_catch_with_failing_goal(self):
        """Test catch(fail, _, true) fails."""
        engine = Engine(program())
        
        # Query: catch(fail, _, true)
        # Goal fails normally (no exception), so result is failure
        results = engine.run([
            Struct("catch", (
                Atom("fail"),
                Var(0, "_"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 0
    
    def test_catch_exact_match(self):
        """Test catch(throw(ball), ball, true) catches and recovers."""
        engine = Engine(program())
        
        # Query: catch(throw(ball), ball, true)
        # Exception matches exactly, Recovery executes
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Atom("ball"),)),
                Atom("ball"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
    
    def test_catch_with_variable_catcher(self):
        """Test catch(throw(ball), X, true) binds X=ball."""
        engine = Engine(program())
        
        # Query: catch(throw(ball), X, true)
        # Variable catcher matches anything and gets bound
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Atom("ball"),)),
                Var(0, "X"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("ball")
    
    def test_catch_no_match_rethrows(self):
        """Test catch(throw(ball), other, true) rethrows.
        
        In Stage-1 this means propagates out of run() as PrologThrow.
        """
        engine = Engine(program())
        
        # Query: catch(throw(ball), other, true)
        # Exception doesn't match, should rethrow
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("catch", (
                    Struct("throw", (Atom("ball"),)),
                    Atom("other"),
                    Atom("true")
                ))
            ])
        assert exc.value.ball == Atom("ball")
        # Check clean unwinding
        if hasattr(engine, 'cp_stack'):
            assert len(engine.cp_stack) == 0
        if hasattr(engine, 'trail'):
            assert engine.trail.position() == 0
    
    def test_catch_recovery_goal_executes(self):
        """Test recovery goal executes on catch."""
        p = program(
            mk_fact("recovered")
        )
        engine = Engine(p)
        
        # Query: catch(throw(error), error, recovered)
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Atom("error"),)),
                Atom("error"),
                Atom("recovered")
            ))
        ])
        
        assert len(results) == 1  # recovered succeeds
    
    def test_catch_recovery_goal_fails(self):
        """Test catch with failing recovery goal."""
        engine = Engine(program())
        
        # Query: catch(throw(error), error, fail)
        # Catches but recovery fails
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Atom("error"),)),
                Atom("error"),
                Atom("fail")
            ))
        ])
        
        assert len(results) == 0  # Recovery failed


class TestCatchWithUnification:
    """Test catch/3 with pattern matching via unification."""
    
    def test_catch_structure_pattern(self):
        """Test catching with structure pattern."""
        engine = Engine(program())
        
        # Query: catch(throw(error(type, foo)), error(Type, _), true)
        # Should catch and bind Type
        results = engine.run([
            Struct("catch", (
                Struct("throw", (
                    Struct("error", (Atom("type"), Atom("foo")))
                ,)),
                Struct("error", (Var(0, "Type"), Var(1, "_"))),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["Type"] == Atom("type")
    
    def test_catch_partial_pattern_match(self):
        """Test catch with partial pattern matching."""
        engine = Engine(program())
        
        # Query: catch(throw(foo(a,b)), foo(a,X), true)
        # Should match and bind X=b
        results = engine.run([
            Struct("catch", (
                Struct("throw", (
                    Struct("foo", (Atom("a"), Atom("b")))
                ,)),
                Struct("foo", (Atom("a"), Var(0, "X"))),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("b")
    
    def test_catch_pattern_mismatch(self):
        """Test catch pattern that doesn't unify.
        
        In Stage-1 this means propagates out of run() as PrologThrow.
        """
        engine = Engine(program())
        
        # Query: catch(throw(foo(a)), foo(b), true)
        # Pattern doesn't match, should rethrow
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("catch", (
                    Struct("throw", (Struct("foo", (Atom("a"),)),)),
                    Struct("foo", (Atom("b"),)),
                    Atom("true")
                ))
            ])
        assert exc.value.ball == Struct("foo", (Atom("a"),))


class TestNestedCatch:
    """Test nested catch/3 behavior."""
    
    def test_nested_catch_inner_handles(self):
        """Test inner catch handles matching exception."""
        engine = Engine(program())
        
        # Query: catch(catch(throw(inner), inner, true), outer, fail)
        # Inner catch handles it
        results = engine.run([
            Struct("catch", (
                Struct("catch", (
                    Struct("throw", (Atom("inner"),)),
                    Atom("inner"),
                    Atom("true")
                )),
                Atom("outer"),
                Atom("fail")
            ))
        ])
        
        assert len(results) == 1
    
    def test_nested_catch_outer_handles(self):
        """Test outer catch handles when inner doesn't match."""
        engine = Engine(program())
        
        # Query: catch(catch(throw(error), inner, fail), error, true)
        # Inner doesn't match, outer handles
        results = engine.run([
            Struct("catch", (
                Struct("catch", (
                    Struct("throw", (Atom("error"),)),
                    Atom("inner"),
                    Atom("fail")
                )),
                Atom("error"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
    
    def test_nested_catch_neither_matches(self):
        """Test exception propagates when neither catch matches.
        
        In Stage-1 this means propagates out of run() as PrologThrow.
        """
        engine = Engine(program())
        
        # Query: catch(catch(throw(other), inner, fail), outer, true)
        # Neither matches, should propagate
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("catch", (
                    Struct("catch", (
                        Struct("throw", (Atom("other"),)),
                        Atom("inner"),
                        Atom("fail")
                    )),
                    Atom("outer"),
                    Atom("true")
                ))
            ])
        assert exc.value.ball == Atom("other")
        # Check clean unwinding
        if hasattr(engine, 'cp_stack'):
            assert len(engine.cp_stack) == 0
    
    def test_recovery_throws_new_exception(self):
        """Test recovery goal can throw a new exception."""
        engine = Engine(program())
        
        # Query: catch(catch(throw(first), first, throw(second)), second, true)
        # First catch throws second, outer catch handles
        results = engine.run([
            Struct("catch", (
                Struct("catch", (
                    Struct("throw", (Atom("first"),)),
                    Atom("first"),
                    Struct("throw", (Atom("second"),))
                )),
                Atom("second"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1


class TestCatchWithBacktracking:
    """Test catch/3 interaction with backtracking."""
    
    def test_catch_with_multiple_solutions(self):
        """Test catch with goal that has multiple solutions."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("p", Atom("c"))
        )
        engine = Engine(p)
        
        # Query: catch(p(X), _, fail)
        # No exception, should get all solutions
        results = engine.run([
            Struct("catch", (
                Struct("p", (Var(0, "X"),)),
                Var(1, "_"),
                Atom("fail")
            ))
        ])
        
        assert len(results) == 3
        assert results[0]["X"] == Atom("a")
        assert results[1]["X"] == Atom("b")
        assert results[2]["X"] == Atom("c")
    
    def test_catch_throw_on_second_solution(self):
        """Test exception on backtracking."""
        p = program(
            mk_rule("p", (Var(0, "X"),),
                Struct(";", (
                    Struct("=", (Var(0, "X"), Atom("first"))),
                    Struct(",", (
                        Struct("=", (Var(0, "X"), Atom("second"))),
                        Struct("throw", (Atom("error"),))
                    ))
                ))
            )
        )
        engine = Engine(p)
        
        # Query: catch(p(X), error, (X = caught))
        # First solution succeeds, backtracking throws and is caught
        results = engine.run([
            Struct("catch", (
                Struct("p", (Var(0, "X"),)),
                Atom("error"),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        # Should get two results: first normal, second from catch
        assert len(results) == 2
        assert results[0]["X"] == Atom("first")
        assert results[1]["X"] == Atom("caught")
    
    def test_catch_with_cut_in_goal(self):
        """Test catch with cut in goal."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b"))
        )
        engine = Engine(p)
        
        # Query: catch((p(X), !), _, fail)
        # Cut prevents backtracking, only one solution
        results = engine.run([
            Struct("catch", (
                Struct(",", (
                    Struct("p", (Var(0, "X"),)),
                    Atom("!")
                )),
                Var(1, "_"),
                Atom("fail")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")


class TestCatchErrorCases:
    """Test error conditions for catch/3."""
    
    def test_catch_with_unbound_goal(self):
        """Test catch(G, _, _) with unbound G fails."""
        engine = Engine(program())
        
        # Query: catch(G, _, true) with unbound G
        results = engine.run([
            Struct("catch", (
                Var(0, "G"),
                Var(1, "_"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 0  # Dev-mode: fail on unbound goal
    
    def test_catch_with_integer_goal(self):
        """Test catch(42, _, _) fails."""
        engine = Engine(program())
        
        # Query: catch(42, _, true)
        results = engine.run([
            Struct("catch", (
                Int(42),
                Var(0, "_"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 0  # Dev-mode: fail on non-callable
    
    def test_catch_wrong_arity(self):
        """Test catch/2 and catch/4 are undefined."""
        engine = Engine(program())
        
        # Query: catch(goal, catcher)  (missing recovery)
        results = engine.run([
            Struct("catch", (Atom("goal"), Atom("catcher")))
        ])
        assert len(results) == 0
        
        # Query: catch(goal, catcher, recovery, extra)
        results = engine.run([
            Struct("catch", (
                Atom("goal"),
                Atom("catcher"),
                Atom("recovery"),
                Atom("extra")
            ))
        ])
        assert len(results) == 0


class TestCatchAdvancedSemantics:
    """Test advanced semantic properties of catch/3."""
    
    def test_catch_preserves_bindings(self):
        """Test bindings made before throw are preserved."""
        engine = Engine(program())
        
        # Query: catch((X=value, throw(error)), error, true)
        # X should remain bound after catch
        results = engine.run([
            Struct("catch", (
                Struct(",", (
                    Struct("=", (Var(0, "X"), Atom("value"))),
                    Struct("throw", (Atom("error"),))
                )),
                Atom("error"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("value")
    
    def test_catcher_bindings_available_in_recovery(self):
        """Test variables bound by catcher unification are available in recovery."""
        engine = Engine(program())
        
        # Query: catch(throw(foo(data)), foo(X), (Y=X))
        # X gets bound during catch, should be available in recovery
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Struct("foo", (Atom("data"),)),)),
                Struct("foo", (Var(0, "X"),)),
                Struct("=", (Var(1, "Y"), Var(0, "X")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("data")
        assert results[0]["Y"] == Atom("data")
    
    def test_catch_with_if_then_else(self):
        """Test catch interaction with if-then-else."""
        p = program(
            mk_fact("cond")
        )
        engine = Engine(p)
        
        # Query: catch((cond -> throw(error) ; true), error, true)
        # Condition succeeds, then throws, catch handles it
        results = engine.run([
            Struct("catch", (
                Struct("->", (
                    Atom("cond"),
                    Struct(";", (
                        Struct("throw", (Atom("error"),)),
                        Atom("true")
                    ))
                )),
                Atom("error"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
    
    def test_catch_if_then_else_else_path(self):
        """Test catch with if-then-else taking else branch."""
        # No cond/0 fact in program
        engine = Engine(program())
        
        # Query: catch((cond -> throw(error) ; true), error, fail)
        # cond fails, else branch succeeds, no exception
        results = engine.run([
            Struct("catch", (
                Struct("->", (
                    Atom("cond"),
                    Struct(";", (
                        Struct("throw", (Atom("error"),)),
                        Atom("true")
                    ))
                )),
                Atom("error"),
                Atom("fail")
            ))
        ])
        
        assert len(results) == 1  # Normal success from else branch
    
    def test_recovery_nondet_enumerates_all(self):
        """Test recovery goal can be nondeterministic."""
        p = program(
            mk_fact("r", Atom("a")),
            mk_fact("r", Atom("b"))
        )
        engine = Engine(p)
        
        # Query: catch(throw(t), t, r(Y))
        # Recovery goal has multiple solutions
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Atom("t"),)),
                Atom("t"),
                Struct("r", (Var(0, "Y"),))
            ))
        ])
        
        assert len(results) == 2
        assert results[0]["Y"] == Atom("a")
        assert results[1]["Y"] == Atom("b")
    
    def test_goal_does_not_resume_after_recovery(self):
        """Test goal doesn't resume after recovery."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b"))
        )
        engine = Engine(p)
        
        # Query: catch((p(X), throw(t) ; p(Y)), t, true)
        # Only first branch throws and recovers; Y branch must not run
        results = engine.run([
            Struct("catch", (
                Struct(";", (
                    Struct(",", (
                        Struct("p", (Var(0, "X"),)),
                        Struct("throw", (Atom("t"),))
                    )),
                    Struct("p", (Var(1, "Y"),))
                )),
                Atom("t"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1  # Only recovery, no Y branch
    
    def test_throw_inside_call_caught(self):
        """Test throw inside call/1 is caught."""
        engine = Engine(program())
        
        # Query: catch(call(throw(e)), e, true)
        results = engine.run([
            Struct("catch", (
                Struct("call", (Struct("throw", (Atom("e"),)),)),
                Atom("e"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
    
    def test_throw_inside_once_caught(self):
        """Test throw inside once/1 is caught."""
        engine = Engine(program())
        
        # Query: catch(once(throw(e)), e, true)
        results = engine.run([
            Struct("catch", (
                Struct("once", (Struct("throw", (Atom("e"),)),)),
                Atom("e"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1