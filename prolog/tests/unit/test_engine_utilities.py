"""
Tests for engine utility methods and properties.
Covers query(), consult_string(), debug properties, and trace functionality.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact


class TestEngineQueryMethod:
    """Tests for the query() convenience method."""

    def test_query_simple(self):
        """Test query() method with simple query."""
        p = program(
            mk_fact("parent", Atom("alice"), Atom("bob")),
            mk_fact("parent", Atom("alice"), Atom("carol")),
        )
        engine = Engine(p)
        
        # Query without ?- and .
        solutions = engine.query("parent(alice, X)")
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("bob")
        assert solutions[1]["X"] == Atom("carol")

    def test_query_with_prefix_suffix(self):
        """Test query() handles queries that already have ?- and ."""
        p = program(mk_fact("fact", Atom("a")))
        engine = Engine(p)
        
        # Query already has ?- and .
        solutions = engine.query("?- fact(X).")
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")

    def test_query_with_conjunction(self):
        """Test query() with conjunction."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("q", Atom("b")),
        )
        engine = Engine(p)
        
        solutions = engine.query("p(X), q(Y)")
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")
        assert solutions[0]["Y"] == Atom("b")

    def test_query_empty_result(self):
        """Test query() that fails."""
        engine = Engine(program())
        solutions = engine.query("nonexistent(X)")
        assert len(solutions) == 0


class TestEngineConsultString:
    """Tests for consult_string() method."""

    def test_consult_string_adds_clauses(self):
        """Test consult_string() adds clauses to program."""
        engine = Engine(program())
        
        # Initially no clauses
        solutions = engine.query("fact(X)")
        assert len(solutions) == 0
        
        # Add clauses via consult_string
        engine.consult_string("fact(a). fact(b).")
        
        # Now queries should succeed
        solutions = engine.query("fact(X)")
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")

    def test_consult_string_preserves_existing(self):
        """Test consult_string() preserves existing clauses."""
        p = program(mk_fact("existing", Atom("old")))
        engine = Engine(p)
        
        # Add new clauses
        engine.consult_string("new(fact).")
        
        # Both old and new should work
        old_solutions = engine.query("existing(X)")
        assert len(old_solutions) == 1
        assert old_solutions[0]["X"] == Atom("old")
        
        new_solutions = engine.query("new(X)")
        assert len(new_solutions) == 1
        assert new_solutions[0]["X"] == Atom("fact")

    def test_consult_string_with_rules(self):
        """Test consult_string() with rules."""
        engine = Engine(program())
        
        engine.consult_string("""
            parent(alice, bob).
            parent(bob, carol).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)
        
        solutions = engine.query("grandparent(alice, Z)")
        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("carol")


class TestEngineDebugProperties:
    """Tests for debug properties."""

    def test_debug_trail_depth(self):
        """Test debug_trail_depth property."""
        engine = Engine(program())
        
        # Initially empty
        assert engine.debug_trail_depth == 0
        
        # After a query with unification, trail grows
        engine.program = program(mk_fact("p", Var(0, "X")))
        solutions = engine.run([Struct("p", (Atom("a"),))])
        # Trail is cleaned up after query
        assert engine.debug_trail_depth == 0

    def test_debug_cp_stack_size(self):
        """Test debug_cp_stack_size property."""
        engine = Engine(program())
        assert engine.debug_cp_stack_size == 0
        
        # During query execution, choicepoints may be created
        # But they're cleaned up after
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
        )
        engine.program = p
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert engine.debug_cp_stack_size == 0  # Cleaned up

    def test_debug_goal_stack_size(self):
        """Test debug_goal_stack_size property."""
        engine = Engine(program())
        assert engine.debug_goal_stack_size == 0

    def test_debug_frame_stack_size(self):
        """Test debug_frame_stack_size property."""
        engine = Engine(program())
        assert engine.debug_frame_stack_size == 0

    def test_debug_frame_pops(self):
        """Test debug_frame_pops property."""
        engine = Engine(program())
        assert engine.debug_frame_pops == 0

    def test_debug_trail_writes(self):
        """Test debug_trail_writes property."""
        engine = Engine(program())
        assert engine.debug_trail_writes == 0


class TestEngineWithTrace:
    """Tests for engine with trace enabled."""

    def test_trace_simple_query(self):
        """Test engine with trace enabled."""
        p = program(mk_fact("p", Atom("a")))
        engine = Engine(p, trace=True)
        
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 1
        
        # Trace log should be populated
        assert len(engine._trace_log) > 0
        # Should contain goal traces
        assert any("Goal:" in log for log in engine._trace_log)

    def test_trace_with_backtracking(self):
        """Test trace with backtracking."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
        )
        engine = Engine(p, trace=True)
        
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 2
        
        # Should have multiple trace entries
        assert len(engine._trace_log) > 2

    def test_trace_with_conjunction(self):
        """Test trace with conjunction."""
        p = program(
            mk_fact("p", Atom("a")),
            mk_fact("q", Atom("b")),
        )
        engine = Engine(p, trace=True)
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        conj = Struct(",", (
            Struct("p", (X,)),
            Struct("q", (Y,))
        ))
        
        solutions = engine.run([conj])
        assert len(solutions) == 1
        
        # Trace should show both goals
        trace_str = " ".join(engine._trace_log)
        # Goals are shown as Struct objects in trace
        assert "functor='p'" in trace_str
        assert "functor='q'" in trace_str


class TestEngineMaxSteps:
    """Tests for max_steps limiting."""

    def test_max_steps_limits_execution(self):
        """Test max_steps prevents infinite loops."""
        from prolog.tests.helpers import mk_rule
        
        # Create infinite loop: loop :- loop.
        p = program(
            mk_rule("loop", (), Atom("loop"))
        )
        engine = Engine(p, max_steps=10)
        
        solutions = engine.run([Atom("loop")])
        assert len(solutions) == 0  # Fails due to step limit

    def test_max_steps_allows_completion(self):
        """Test max_steps doesn't interfere with normal queries."""
        p = program(mk_fact("p", Atom("a")))
        engine = Engine(p, max_steps=100)
        
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")


class TestEngineOccursCheck:
    """Tests for occurs check mode."""

    def test_occurs_check_on(self):
        """Test engine with occurs check enabled."""
        engine = Engine(program(), occurs_check=True)
        
        X = Var(0, "X")
        # X = f(X) should fail with occurs check
        query = Struct("=", (X, Struct("f", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_occurs_check_off(self):
        """Test engine with occurs check disabled."""
        engine = Engine(program(), occurs_check=False)
        
        X = Var(0, "X")
        # X = f(X) might succeed without occurs check
        # (creating cyclic structure)
        query = Struct("=", (X, Struct("f", (X,))))
        solutions = engine.run([query])
        # Behavior depends on implementation
        # Just verify it doesn't crash
        assert isinstance(solutions, list)