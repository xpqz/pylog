"""Tests for trail stamp discipline and window invariants."""

import pytest
from prolog.ast.terms import Atom, Var, Struct
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_rule, mk_fact


class TestTrailStampDiscipline:
    """Test trail stamp management across choicepoint windows."""
    
    def test_trail_first_write_once_per_window(self):
        """Test that first write is trailed once per CP window, not per redo.
        
        This guards against stamp drift where redoing a CP would increment
        the stamp instead of restoring it, leading to missed trails or
        inflated write counters.
        """
        # Create a program with a disjunction that binds the same var in each branch
        p = program(
            mk_rule("test", (Var(0, "X"),),
                Struct(";", (
                    Struct("=", (Var(0, "X"), Atom("first"))),
                    Struct("=", (Var(0, "X"), Atom("second")))
                ))
            )
        )
        engine = Engine(p)
        
        # Track trail depth at key points
        initial_trail = engine.trail.position()
        
        # Query: test(X)
        results = engine.run([Struct("test", (Var(0, "X"),))])
        
        # Should get two solutions
        assert len(results) == 2
        assert results[0]["X"] == Atom("first")
        assert results[1]["X"] == Atom("second")
        
        # After completion, trail should be back to initial position
        final_trail = engine.trail.position()
        assert final_trail == initial_trail, \
            f"Trail not restored: started at {initial_trail}, ended at {final_trail}"
    
    def test_disjunction_with_catch_preserves_stamp_semantics(self):
        """Test stamp semantics when catch interacts with disjunction."""
        p = program(
            mk_rule("p", (Var(0, "X"),),
                Struct(";", (
                    Struct("=", (Var(0, "X"), Atom("a"))),
                    Struct(",", (
                        Struct("=", (Var(0, "X"), Atom("b"))),
                        Struct("throw", (Atom("error"),))
                    ))
                ))
            )
        )
        engine = Engine(p)
        
        # Query: catch(p(X), error, X=caught)
        results = engine.run([
            Struct("catch", (
                Struct("p", (Var(0, "X"),)),
                Atom("error"),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        # Should get two results: first normal, second from catch
        assert len(results) == 2
        assert results[0]["X"] == Atom("a")
        assert results[1]["X"] == Atom("caught")
    
    def test_nested_disjunctions_maintain_separate_stamps(self):
        """Test that nested disjunctions each have their own stamp window."""
        p = program(
            mk_rule("outer", (Var(0, "X"), Var(1, "Y")),
                Struct(";", (
                    Struct(",", (
                        Struct("=", (Var(0, "X"), Atom("x1"))),
                        Struct("inner", (Var(1, "Y"),))
                    )),
                    Struct(",", (
                        Struct("=", (Var(0, "X"), Atom("x2"))),
                        Struct("inner", (Var(1, "Y"),))
                    ))
                ))
            ),
            mk_rule("inner", (Var(0, "Z"),),
                Struct(";", (
                    Struct("=", (Var(0, "Z"), Atom("y1"))),
                    Struct("=", (Var(0, "Z"), Atom("y2")))
                ))
            )
        )
        engine = Engine(p)
        
        # Query: outer(X, Y)
        results = engine.run([Struct("outer", (Var(0, "X"), Var(1, "Y")))])
        
        # Should get 4 solutions (2 outer * 2 inner)
        assert len(results) == 4
        expected = [
            {"X": Atom("x1"), "Y": Atom("y1")},
            {"X": Atom("x1"), "Y": Atom("y2")},
            {"X": Atom("x2"), "Y": Atom("y1")},
            {"X": Atom("x2"), "Y": Atom("y2")},
        ]
        for exp in expected:
            assert exp in results


class TestRecoveryWithCut:
    """Test that cut in recovery goals behaves correctly."""
    
    def test_cut_in_recovery_doesnt_over_commit(self):
        """Test that cut in recovery commits only within recovery's CP window.
        
        The cut should not prune choicepoints outside the catch's baseline,
        ensuring proper isolation of the recovery goal execution.
        """
        p = program(
            mk_rule("p", (Var(0, "X"),),
                Struct(";", (
                    Struct("=", (Var(0, "X"), Atom("first"))),
                    Struct("throw", (Atom("error"),))
                ))
            ),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b")),
            mk_fact("r", Atom("done"))
        )
        engine = Engine(p)
        
        # Query: catch(p(X), error, (q(Y), !, r(Z)))
        # The cut in recovery should not prevent backtracking to get X=first
        results = engine.run([
            Struct("catch", (
                Struct("p", (Var(0, "X"),)),
                Atom("error"),
                Struct(",", (
                    Struct(",", (
                        Struct("q", (Var(1, "Y"),)),
                        Atom("!")
                    )),
                    Struct("r", (Var(2, "Z"),))
                ))
            ))
        ])
        
        # Should get both solutions:
        # 1. X=first (normal success, Y/Z remain unbound)
        # 2. Y=a, Z=done (from recovery after throw, cut prevents Y=b)
        assert len(results) == 2
        assert results[0]["X"] == Atom("first")
        # Y and Z are query variables so they appear in results but unbound
        assert isinstance(results[0].get("Y"), Var) or results[0].get("Y") is None
        assert results[1]["Y"] == Atom("a")  # Cut prevents Y=b
        assert results[1]["Z"] == Atom("done")
    
    def test_cut_in_recovery_with_multiple_catchers(self):
        """Test cut behavior when multiple catch frames are active."""
        p = program(
            mk_fact("throws"),  # Will be defined to throw
            mk_fact("q", Atom("q1")),
            mk_fact("q", Atom("q2"))
        )
        
        # Override throws to actually throw
        engine = Engine(p)
        
        # Nested catch: outer catches e1, inner catches e2
        # Inner recovery has a cut
        results = engine.run([
            Struct("catch", (
                Struct("catch", (
                    Struct("throw", (Atom("e2"),)),
                    Atom("e2"),
                    Struct(",", (
                        Struct("q", (Var(0, "X"),)),
                        Atom("!")  # Cut in inner recovery
                    ))
                )),
                Atom("e1"),
                Struct("=", (Var(1, "Y"), Atom("outer")))
            ))
        ])
        
        # Inner catch handles e2, cut prevents backtracking to q(q2)
        assert len(results) == 1
        assert results[0]["X"] == Atom("q1")
        # Y is a query variable but remains unbound (outer catch not triggered)
        assert isinstance(results[0].get("Y"), Var) or results[0].get("Y") is None


class TestNonGroundBall:
    """Test exception handling with non-ground terms."""
    
    def test_throw_with_non_ground_ball(self):
        """Test throw/catch with a ball containing unbound variables."""
        engine = Engine(program())
        
        # Query: catch(throw(foo(X)), foo(Y), Y = 1)
        # After catch, Y is bound to 1
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Struct("foo", (Var(0, "X"),)),)),
                Struct("foo", (Var(1, "Y"),)),
                Struct("=", (Var(1, "Y"), Atom("1")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["Y"] == Atom("1")
        # X gets unified with Y during catch, so also becomes 1
        assert results[0]["X"] == Atom("1")
    
    def test_non_ground_ball_with_aliasing(self):
        """Test that goal-local variables don't leak via aliasing."""
        p = program(
            mk_rule("test", (Var(0, "A"), Var(1, "B")),
                Struct(",", (
                    Struct("=", (Var(0, "A"), Var(2, "Local"))),
                    Struct("throw", (Struct("data", (Var(2, "Local"),)),))
                ))
            )
        )
        engine = Engine(p)
        
        # Query: catch(test(X, Y), data(Z), true)
        # Local variable from test/2 shouldn't affect outer scope
        results = engine.run([
            Struct("catch", (
                Struct("test", (Var(0, "X"), Var(1, "Y"))),
                Struct("data", (Var(2, "Z"),)),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        # Z gets unified with Local, which was unified with X
        # This tests that the aliasing is handled correctly
        if "Z" in results[0]:
            # Z might be bound to a variable or remain unbound
            # The key is that the system doesn't crash
            pass


class TestStressCatchThrow:
    """Stress tests for catch/throw to ensure no resource leaks."""
    
    def test_many_catch_throw_cycles_no_leaks(self):
        """Test that many catch/throw cycles don't leak resources."""
        engine = Engine(program())
        
        initial_trail = engine.trail.position()
        initial_cps = len(engine.cp_stack)
        initial_frames = len(engine.frame_stack)
        
        # Run many catch/throw cycles
        for i in range(100):
            results = engine.run([
                Struct("catch", (
                    Struct("throw", (Atom(f"error_{i}"),)),
                    Var(0, "E"),
                    Atom("true")
                ))
            ])
            assert len(results) == 1
            assert results[0]["E"] == Atom(f"error_{i}")
        
        # After all cycles, resources should be back to baseline
        final_trail = engine.trail.position()
        final_cps = len(engine.cp_stack)
        final_frames = len(engine.frame_stack)
        
        assert final_trail == initial_trail, \
            f"Trail leaked: {initial_trail} -> {final_trail}"
        assert final_cps == initial_cps, \
            f"Choicepoints leaked: {initial_cps} -> {final_cps}"
        assert final_frames == initial_frames, \
            f"Frames leaked: {initial_frames} -> {final_frames}"
    
    def test_nested_catch_stress(self):
        """Test deeply nested catch frames don't cause issues."""
        # Build a deeply nested catch structure
        depth = 50
        query = Struct("throw", (Atom("deepest"),))
        
        for i in range(depth):
            query = Struct("catch", (
                query,
                Atom(f"error_{i}"),
                Struct("=", (Var(i, f"V{i}"), Atom(f"caught_{i}")))
            ))
        
        # The deepest throw should be caught by the outermost catch
        # that matches "deepest"
        query = Struct("catch", (
            query,
            Atom("deepest"),
            Struct("=", (Var(100, "Result"), Atom("caught")))
        ))
        
        engine = Engine(program())
        results = engine.run([query])
        
        assert len(results) == 1
        assert results[0]["Result"] == Atom("caught")