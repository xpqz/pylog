"""Tests for throw/1 builtin.

Tests exception throwing for Stage 1.

Error policy: dev-mode. Ill-typed calls to throw/1 fail rather than
throwing ISO errors. ISO error behavior to be added in later stages.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestThrowBasicBehavior:
    """Test basic throw/1 functionality."""
    
    def test_throw_propagates_atom(self):
        """Test throw(error) propagates atom error term."""
        engine = Engine(program())
        
        # Query: throw(error)
        # Should raise PrologThrow with ball = Atom("error")
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("throw", (Atom("error"),))
            ])
        assert exc.value.ball == Atom("error")
    
    def test_throw_propagates_structure(self):
        """Test throw(foo(bar)) preserves structure."""
        engine = Engine(program())
        
        # Query: throw(foo(bar))
        # Should raise PrologThrow with structured ball
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("throw", (Struct("foo", (Atom("bar"),)),))
            ])
        assert exc.value.ball == Struct("foo", (Atom("bar"),))
    
    def test_throw_with_variable_term(self):
        """Test throw(X) after X is bound."""
        engine = Engine(program())
        
        # Query: X = error, throw(X)
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct(",", (
                    Struct("=", (Var(0, "X"), Atom("error"))),
                    Struct("throw", (Var(0, "X"),))
                ))
            ])
        assert exc.value.ball == Atom("error")
    
    def test_throw_unbound_variable_fails(self):
        """Test throw(X) with unbound X fails in dev-mode.
        
        Note: This is Stage-1 dev-mode policy. In full ISO mode,
        throw/1 would allow unbound variables. This behavior will
        change when we implement ISO error handling in later stages.
        """
        engine = Engine(program())
        
        # Query: throw(X) with unbound X
        # Stage-1 policy: require instantiated ball; ISO would allow variables
        # In dev-mode, this should fail rather than throw
        results = engine.run([
            Struct("throw", (Var(0, "X"),))
        ])
        
        assert len(results) == 0  # Dev-mode: fail on unbound
    
    def test_throw_in_conjunction_interrupts(self):
        """Test throw/1 interrupts conjunction execution."""
        p = program(
            mk_fact("q")
        )
        engine = Engine(p)
        
        # Query: throw(error), q
        # q should never execute
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct(",", (
                    Struct("throw", (Atom("error"),)),
                    Atom("q")
                ))
            ])
        assert exc.value.ball == Atom("error")
        # Check clean unwinding if engine exposes debug state
        if hasattr(engine, 'cp_stack'):
            assert len(engine.cp_stack) == 0
        if hasattr(engine, 'trail'):
            assert engine.trail.position() == 0
    
    def test_throw_in_disjunction_left_branch(self):
        """Test throw/1 in left branch of disjunction."""
        engine = Engine(program())
        
        # Query: (throw(error) ; true)
        # Should throw before trying right branch
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct(";", (
                    Struct("throw", (Atom("error"),)),
                    Atom("true")
                ))
            ])
        assert exc.value.ball == Atom("error")
    
    def test_throw_in_disjunction_right_branch(self):
        """Test throw/1 in right branch of disjunction."""
        engine = Engine(program())
        
        # Query: (fail ; throw(error))
        # Should throw from right branch
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct(";", (
                    Atom("fail"),
                    Struct("throw", (Atom("error"),))
                ))
            ])
        assert exc.value.ball == Atom("error")
        # Check clean unwinding
        if hasattr(engine, 'cp_stack'):
            assert len(engine.cp_stack) == 0
        if hasattr(engine, 'trail'):
            assert engine.trail.position() == 0


class TestThrowUnwinding:
    """Test that throw/1 properly unwinds execution state."""
    
    def test_throw_unwinds_goal_stack(self):
        """Test throw/1 unwinds pending goals."""
        p = program(
            mk_rule("p", (), 
                Struct(",", (
                    Struct("throw", (Atom("error"),)),
                    Atom("q")  # Should not execute
                ))
            ),
            mk_fact("q")
        )
        engine = Engine(p)
        
        # Query: p
        with pytest.raises(PrologThrow) as exc:
            engine.run([Atom("p")])
        assert exc.value.ball == Atom("error")
        # Goal stack should be empty after unwinding
        if hasattr(engine, 'goal_stack'):
            assert engine.goal_stack.height() == 0
    
    def test_throw_unwinds_choicepoints(self):
        """Test throw/1 unwinds choicepoints."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("p", Atom("c"))
        )
        engine = Engine(p)
        
        # Query: p(X), throw(error)
        # Should unwind choicepoints created by p(X)
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct(",", (
                    Struct("p", (Var(0, "X"),)),
                    Struct("throw", (Atom("error"),))
                ))
            ])
        assert exc.value.ball == Atom("error")
        # All choicepoints should be unwound
        if hasattr(engine, 'cp_stack'):
            assert len(engine.cp_stack) == 0
    
    def test_throw_from_nested_calls(self):
        """Test throw/1 from deeply nested predicate calls."""
        p = program(
            mk_rule("p", (), Atom("q")),
            mk_rule("q", (), Atom("r")),
            mk_rule("r", (), Struct("throw", (Atom("deep_error"),)))
        )
        engine = Engine(p)
        
        # Query: p
        # Should throw from within r, through q and p
        with pytest.raises(PrologThrow) as exc:
            engine.run([Atom("p")])
        assert exc.value.ball == Atom("deep_error")
    
    def test_throw_with_cut_interaction(self):
        """Test throw/1 interaction with cut."""
        p = program(
            mk_rule("p", (), 
                Struct(",", (
                    Atom("!"),
                    Struct("throw", (Atom("after_cut"),))
                ))
            )
        )
        engine = Engine(p)
        
        # Query: p
        # Cut executes but throw still propagates
        with pytest.raises(PrologThrow) as exc:
            engine.run([Atom("p")])
        assert exc.value.ball == Atom("after_cut")


class TestThrowErrorCases:
    """Test error conditions for throw/1."""
    
    def test_throw_integer_term(self):
        """Test throw(42) propagates integer."""
        engine = Engine(program())
        
        # Query: throw(42)
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("throw", (Int(42),))
            ])
        assert exc.value.ball == Int(42)
    
    def test_throw_complex_term(self):
        """Test throw with complex nested term."""
        engine = Engine(program())
        
        complex_term = Struct("error", (
            Atom("type_error"),
            Struct("context", (
                Atom("predicate"),
                Struct("/", (Atom("foo"), Int(2)))
            ))
        ))
        
        # Query: throw(error(type_error, context(...)))
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("throw", (complex_term,))
            ])
        assert exc.value.ball == complex_term
    
    def test_throw_wrong_arity(self):
        """Test throw/2 is undefined."""
        engine = Engine(program())
        
        # Query: throw(error, extra)
        # Should fail as undefined predicate
        results = engine.run([
            Struct("throw", (Atom("error"), Atom("extra")))
        ])
        
        assert len(results) == 0  # Undefined predicate
    
    def test_throw_no_args(self):
        """Test throw/0 is undefined."""
        engine = Engine(program())
        
        # Query: throw
        # Should fail as undefined predicate
        results = engine.run([Atom("throw")])
        
        assert len(results) == 0  # Undefined predicate


class TestThrowAdditionalCases:
    """Additional test cases for comprehensive coverage."""
    
    def test_throw_after_first_solution_stops_enumeration(self):
        """Test throw stops solution enumeration."""
        p = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2))
        )
        engine = Engine(p)
        
        # Query: p(X), throw(stop)
        # Should throw after first solution, not enumerate second
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct(",", (
                    Struct("p", (Var(0, "X"),)),
                    Struct("throw", (Atom("stop"),))
                ))
            ])
        assert exc.value.ball == Atom("stop")
    
    def test_throw_inside_call(self):
        """Test throw inside call/1 still propagates."""
        engine = Engine(program())
        
        # Query: call(throw(meta_error))
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("call", (Struct("throw", (Atom("meta_error"),)),))
            ])
        assert exc.value.ball == Atom("meta_error")
    
    def test_throw_inside_once(self):
        """Test throw inside once/1 still propagates."""
        engine = Engine(program())
        
        # Query: once(throw(once_error))
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("once", (Struct("throw", (Atom("once_error"),)),))
            ])
        assert exc.value.ball == Atom("once_error")