"""Tests for throw/catch exception handling.

Projection policy: Query variables are included in results even if unbound.
Stage-1 policy: Dev-mode semantics - fail on non-callable/unbound goals, no ISO errors.
"""

import pytest
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Program


def assert_engine_clean(engine):
    """Assert that engine state is clean (no leftover stacks/trail)."""
    if hasattr(engine, 'cp_stack'):
        assert len(engine.cp_stack) == 0
    if hasattr(engine, 'goal_stack'):
        assert engine.goal_stack.height() == 0
    if hasattr(engine, 'frame_stack'):
        assert len(engine.frame_stack) == 0
    if hasattr(engine, 'trail'):
        assert engine.trail.position() == 0


class TestThrowCatch:
    """Test throw/catch exception handling."""
    
    def test_uncaught_throw(self):
        """Test that uncaught throw/1 raises PrologThrow."""
        engine = Engine(Program(()))
        
        # throw(test_ball) should raise PrologThrow
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("throw(test_ball)"))
        
        # Check the ball term
        assert exc_info.value.ball == Atom("test_ball")
    
    def test_uncaught_throw_with_structure(self):
        """Test that uncaught throw with compound term raises PrologThrow."""
        engine = Engine(Program(()))
        
        # throw(error(type_error, context)) should raise PrologThrow
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("throw(error(type_error, context))"))
        
        # Check the ball term is the expected structure
        ball = exc_info.value.ball
        assert isinstance(ball, Struct)
        assert ball.functor == "error"
        assert len(ball.args) == 2
        assert ball.args[0] == Atom("type_error")
        assert ball.args[1] == Atom("context")
    
    def test_throw_unbound_variable_fails(self):
        """Test that throw with unbound variable fails in Stage-1."""
        engine = Engine(Program(()))
        
        # throw(X) with unbound X should fail (Stage-1 policy)
        results = list(engine.query("throw(X)"))
        assert results == []  # Should fail, not throw
    
    def test_basic_catch_success(self):
        """Test that catch/3 with non-throwing goal succeeds normally."""
        engine = Engine(Program(()))
        
        # catch(true, _, fail) should succeed with the goal's success
        results = list(engine.query("catch(true, _, fail)"))
        assert len(results) == 1
        
        # catch('='(X,1), _, fail) should bind X=1
        results = list(engine.query("catch('='(X,1), _, fail)"))
        assert len(results) == 1
        assert results[0]["X"] == Int(1)
    
    def test_basic_catch_with_throw(self):
        """Test that catch/3 catches matching exception."""
        engine = Engine(Program(()))
        
        # catch(throw(ball), ball, true) should succeed via recovery
        results = list(engine.query("catch(throw(ball), ball, true)"))
        assert len(results) == 1
        
        # catch(throw(ball), ball, '='(X,caught)) should bind X=caught
        results = list(engine.query("catch(throw(ball), ball, '='(X,caught))"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_unifies_ball_with_catcher(self):
        """Test that catch unifies the thrown ball with the catcher."""
        engine = Engine(Program(()))
        
        # catch(throw(foo(1)), foo(X), '='(Y,X)) should bind X=1, Y=1
        results = list(engine.query("catch(throw(foo(1)), foo(X), '='(Y,X))"))
        assert len(results) == 1
        assert results[0]["X"] == Int(1)
        assert results[0]["Y"] == Int(1)
        
        # catch(throw(bar), X, '='(Y,X)) should bind X=bar, Y=bar
        results = list(engine.query("catch(throw(bar), X, '='(Y,X))"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("bar")
        assert results[0]["Y"] == Atom("bar")
    
    def test_catch_non_matching_propagates(self):
        """Test that non-matching exceptions propagate."""
        engine = Engine(Program(()))
        
        # catch(throw(foo), bar, true) should propagate foo
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("catch(throw(foo), bar, true)"))
        assert exc_info.value.ball == Atom("foo")
    
    def test_catch_with_failing_goal(self):
        """Test that catch with failing goal fails normally."""
        engine = Engine(Program(()))
        
        # catch(fail, _, true) should fail (goal fails, no throw)
        results = list(engine.query("catch(fail, _, true)"))
        assert results == []
    
    def test_catch_with_failing_recovery(self):
        """Test that catch with failing recovery fails."""
        engine = Engine(Program(()))
        
        # catch(throw(ball), ball, fail) should fail (recovery fails)
        results = list(engine.query("catch(throw(ball), ball, fail)"))
        assert results == []
    
    def test_catch_with_choicepoints_in_goal(self):
        """Test catch with choicepoints: catch(';'(';'('='(X,1), ','('='(X,2), throw(t))), '='(X,3)), t, '='(X,caught))."""
        engine = Engine(Program(()))
        
        # Should get X=1 (before throw) and X=caught (from recovery)
        results = list(engine.query("catch(';'(';'('='(X,1), ','('='(X,2), throw(t))), '='(X,3)), t, '='(X,caught))"))
        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Atom("caught")
    
    def test_catch_transparent_backtracking_on_success(self):
        """Test transparent backtracking when goal succeeds."""
        engine = Engine(Program(()))
        
        # Load p(1). p(2).
        engine.consult_string("p(1). p(2).")
        
        # catch(p(X), _, fail) should give both p(1) and p(2)
        results = list(engine.query("catch(p(X), _, fail)"))
        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Int(2)
    
    def test_catch_transparent_backtracking_on_catch(self):
        """Test transparent backtracking after catch."""
        engine = Engine(Program(()))
        
        # Load p(1). p(2). r(10). r(20).
        engine.consult_string("p(1). p(2). r(10). r(20).")
        
        # p(X), catch(throw(t), t, r(Y)) should give 2*2=4 solutions
        results = list(engine.query("p(X), catch(throw(t), t, r(Y))"))
        assert len(results) == 4
        # Check all combinations
        expected = [(1, 10), (1, 20), (2, 10), (2, 20)]
        actual = [(r["X"].value, r["Y"].value) for r in results]
        assert sorted(actual) == sorted(expected)
    
    def test_catch_recovery_fails_transparent(self):
        """Test transparent backtracking when recovery fails.
        
        When catch's recovery fails, the catch/3 call fails. Since it's the second
        goal in a conjunction, the entire conjunction fails for each p/1 alternative.
        This gives 0 solutions total, matching ISO/SWI semantics.
        """
        engine = Engine(Program(()))
        
        # Load p(1). p(2).
        engine.consult_string("p(1). p(2).")
        
        # p(X), catch(throw(t), t, fail) should give 0 solutions
        # (catch fails for each p/1 alternative)
        results = list(engine.query("p(X), catch(throw(t), t, fail)"))
        assert len(results) == 0
    
    @pytest.mark.swi_baseline
    def test_catch_recovery_fails_transparent_baseline(self, swi):
        """Validate transparent backtracking against SWI-Prolog."""
        prog = "p(1). p(2)."
        goal = "p(X), catch(throw(t), t, fail)"
        
        # SWI-Prolog should also give 0 solutions
        assert swi.count(prog, goal) == 0
    
    def test_nested_catch(self):
        """Test nested catch/3 calls."""
        engine = Engine(Program(()))
        
        # Inner catch handles the exception
        results = list(engine.query("catch(catch(throw(inner), inner, '='(X,1)), outer, '='(X,2))"))
        assert len(results) == 1
        assert results[0]["X"] == Int(1)
        
        # Outer catch handles the exception (inner doesn't match)
        results = list(engine.query("catch(catch(throw(outer), inner, '='(X,1)), outer, '='(X,2))"))
        assert len(results) == 1
        assert results[0]["X"] == Int(2)
        
        # Exception propagates through both
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("catch(catch(throw(other), inner, '='(X,1)), outer, '='(X,2))"))
        assert exc_info.value.ball == Atom("other")
    
    def test_cut_in_recovery(self):
        """Test that cut in recovery cuts in the surrounding scope (ISO)."""
        engine = Engine(Program(()))
        
        # Load r(10). r(20).
        engine.consult_string("r(10). r(20).")
        
        # catch(throw(t), t, ','(r(Y), !)) should give only 1 solution
        results = list(engine.query("catch(throw(t), t, ','(r(Y), !))"))
        assert len(results) == 1
        assert results[0]["Y"] == Int(10)
        
        # p(1). p(2). catch(throw(t), t, ','(r(Y), !)), p(X) should give 2 solutions (cut doesn't affect p)
        engine.consult_string("p(1). p(2).")
        results = list(engine.query("catch(throw(t), t, ','(r(Y), !)), p(X)"))
        assert len(results) == 2  # Y=10, X=1 and Y=10, X=2
    
    def test_cut_in_recovery_commits_in_caller_scope(self):
        """Test that cut in recovery prunes outer CPs (ISO behavior)."""
        engine = Engine(Program(()))
        
        # Load p(a). p(b). r(x). r(y).
        engine.consult_string("p(a). p(b). r(x). r(y).")
        
        # p(X), catch(throw(t), t, ','(r(Y), !))
        # ISO: cut inside Recovery commits within the caller scope => only first p × first r
        results = list(engine.query("p(X), catch(throw(t), t, ','(r(Y), !))"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")
        assert results[0]["Y"] == Atom("x")
    
    def test_throw_preserves_bindings_inside_ball(self):
        """Test that bindings inside the ball are preserved."""
        engine = Engine(Program(()))
        
        # Y is bound before constructing the ball; ball should carry 'v'
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("'='(Y,v), throw(err(Y))"))
        
        ball = exc_info.value.ball
        assert isinstance(ball, Struct)
        assert ball.functor == "err"
        assert ball.args[0] == Atom("v")
    
    def test_throw_leftmost_in_disjunction_of_throws(self):
        """Test leftmost selection in disjunction of throws."""
        engine = Engine(Program(()))
        
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("';'(throw(left), throw(right))"))
        assert exc_info.value.ball == Atom("left")
    
    def test_engine_reusable_after_throw(self):
        """Test that engine is reusable after an uncaught throw."""
        engine = Engine(Program(()))
        engine.consult_string("p(ok).")
        
        # First query throws
        with pytest.raises(PrologThrow):
            list(engine.query("throw(boom)"))
        
        # Second run should work normally with clean stacks/trail
        results = list(engine.query("p(X)"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("ok")
        assert_engine_clean(engine)
    
    def test_throw_inside_nested_call_still_propagates(self):
        """Test throw inside nested meta-calls."""
        engine = Engine(Program(()))
        
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("call(call(throw(meta2)))"))
        assert exc_info.value.ball == Atom("meta2")
    
    def test_throw_inside_nested_call_caught(self):
        """Test that throw inside nested call can be caught."""
        engine = Engine(Program(()))
        
        results = list(engine.query("catch(call(call(throw(e))), e, true)"))
        assert len(results) == 1
    
    def test_recovery_failure_transparent_to_outer_choicepoints(self):
        """Test that recovery failure is transparent to outer choicepoints.
        
        Transparency means outer choicepoints are still explored (p/1 alternatives 
        are tried), but since catch fails for each, the conjunction produces 0 solutions.
        """
        engine = Engine(Program(()))
        engine.consult_string("p(1). p(2).")
        
        # Each p/1 alternative is tried, but catch fails for each
        results = list(engine.query("p(X), catch(throw(t), t, fail)"))
        assert len(results) == 0
    
    def test_throw_in_disjunction_left_branch(self):
        """Test throw in left branch of disjunction."""
        engine = Engine(Program(()))
        
        # ';'(throw(t), '='(X,1)) should throw t
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("';'(throw(t), '='(X,1))"))
        assert exc_info.value.ball == Atom("t")
    
    def test_throw_in_disjunction_right_branch(self):
        """Test throw in right branch of disjunction after left fails."""
        engine = Engine(Program(()))
        
        # ';'(fail, throw(t)) should throw t
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("';'(fail, throw(t))"))
        assert exc_info.value.ball == Atom("t")
        # Check clean unwinding
        assert_engine_clean(engine)
    
    def test_throw_in_conjunction(self):
        """Test throw interrupts conjunction."""
        engine = Engine(Program(()))
        engine.consult_string("p(1). p(2).")
        
        # p(X), throw(t), p(Y) should throw t with X bound
        with pytest.raises(PrologThrow) as exc_info:
            list(engine.query("p(X), throw(t), p(Y)"))
        assert exc_info.value.ball == Atom("t")
    
    def test_transparent_backtracking_observable_via_disjunction(self):
        """Test that outer CPs remain and are explored even when catch fails.
        
        This test proves transparency by wrapping the failing catch in a disjunction
        that can succeed, allowing us to observe that p/1 alternatives are still 
        being explored without asserting non-ISO success for the plain conjunction.
        """
        engine = Engine(Program(()))
        engine.consult_string("p(1). p(2).")
        
        # p(X), (catch(throw(t), t, fail) ; true)
        # The catch fails but the disjunction succeeds via 'true'
        # This lets us observe that both p/1 alternatives are tried
        results = list(engine.query("p(X), ';'(catch(throw(t), t, fail), true)"))
        assert len(results) == 2
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Int(2)
    
    @pytest.mark.swi_baseline
    def test_transparent_backtracking_observable_baseline(self, swi):
        """Validate transparency observation against SWI-Prolog."""
        prog = "p(1). p(2)."
        goal = "p(X), (catch(throw(t), t, fail) ; true)"
        
        # SWI should give 2 solutions (both p/1 alternatives succeed)
        assert swi.count(prog, goal) == 2
        
        # Check the actual values
        values = swi.onevar(prog, goal, "X")
        assert values == ["1", "2"]
    
    def test_throw_cleans_unwinding(self):
        """Test that throw properly unwinds stacks."""
        engine = Engine(Program(()))
        engine.consult_string("p(1). p(2). q(a). q(b).")
        
        # Create choicepoints then throw
        with pytest.raises(PrologThrow):
            list(engine.query("p(X), q(Y), throw(error)"))
        
        # All stacks should be clean
        assert_engine_clean(engine)