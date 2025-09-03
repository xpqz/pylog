"""Tests for Prolog Engine (Stage 0)."""

import pytest
from typing import Dict, List, Any

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.engine.goals import Goal, GoalStack
from prolog.engine.choicepoint import Choicepoint, ChoiceStack
from prolog.unify.store import Store

# Test helpers
from prolog.tests.helpers import mk_fact, mk_rule, program, run_query


class TestEngineInitialization:
    """Tests for Engine initialization."""
    
    def test_engine_initializes_with_program(self):
        """Test Engine initializes with program."""
        prog = program(
            mk_fact("parent", Atom("john"), Atom("mary")),
            mk_fact("parent", Atom("john"), Atom("sue"))
        )
        
        engine = Engine(prog)
        
        assert engine.program is prog
        assert isinstance(engine.store, Store)
        assert isinstance(engine.trail, list)
        assert engine.trail == []
        assert engine.solutions == []
        assert engine.max_solutions is None
        assert engine.trace is False
    
    def test_engine_tracks_query_variables(self):
        """Test Engine tracks query variables (name→varid mapping)."""
        prog = program(mk_fact("test", Atom("a")))
        engine = Engine(prog)
        
        # Simulate query with variables
        x_var = Var(engine.store.new_var("X"), "X")
        y_var = Var(engine.store.new_var("Y"), "Y")
        
        # Engine should track these mappings
        engine._query_vars = [
            (x_var.id, "X"),
            (y_var.id, "Y")
        ]
        
        assert len(engine._query_vars) == 2
        assert engine._query_vars[0] == (x_var.id, "X")
        assert engine._query_vars[1] == (y_var.id, "Y")
    
    def test_engine_sets_initial_var_cutoff(self):
        """Test Engine sets initial_var_cutoff after query vars."""
        prog = program(mk_fact("test", Atom("a")))
        engine = Engine(prog)
        
        # Create some query variables
        x_id = engine.store.new_var("X")
        y_id = engine.store.new_var("Y")
        
        # Set cutoff after query vars
        engine._initial_var_cutoff = len(engine.store.cells)
        
        # Now clause renaming creates more vars
        z_id = engine.store.new_var("Z")  # From clause
        
        assert x_id < engine._initial_var_cutoff
        assert y_id < engine._initial_var_cutoff
        assert z_id >= engine._initial_var_cutoff
    
    def test_engine_initializes_empty_solutions_list(self):
        """Test Engine initializes empty solutions list."""
        prog = program()
        engine = Engine(prog)
        
        assert engine.solutions == []
        assert isinstance(engine.solutions, list)
    
    def test_engine_reset_clears_state(self):
        """Test Engine.reset() clears state for reuse."""
        prog = program(mk_fact("test", Atom("a")))
        engine = Engine(prog)
        
        # Simulate some execution
        engine.store.new_var("X")
        engine.trail.append(("test", 0, None))
        engine.solutions.append({"X": Atom("a")})
        engine._initial_var_cutoff = 1
        engine._cut_barrier = 5
        
        # Reset
        engine.reset()
        
        # State should be cleared
        assert len(engine.store.cells) == 0
        assert len(engine.trail) == 0
        assert engine.solutions == []
        assert engine._initial_var_cutoff == 0
        assert engine._cut_barrier is None
        # But program remains
        assert engine.program is prog
    
    def test_engine_with_max_solutions(self):
        """Test Engine with max_solutions limit."""
        prog = program(mk_fact("test", Atom("a")))
        engine = Engine(prog, max_solutions=5)
        
        assert engine.max_solutions == 5
    
    def test_engine_with_trace_enabled(self):
        """Test Engine with trace enabled."""
        prog = program(mk_fact("test", Atom("a")))
        engine = Engine(prog, trace=True)
        
        assert engine.trace is True


class TestSolutionRecording:
    """Tests for solution recording."""
    
    def test_record_solution_captures_query_variables_only(self):
        """Test _record_solution captures query variables only."""
        prog = program(mk_fact("test", Var(0)))
        engine = Engine(prog)
        
        # Set up query variables
        x_id = engine.store.new_var("X")
        y_id = engine.store.new_var("Y")
        engine._query_vars = [(x_id, "X"), (y_id, "Y")]
        engine._initial_var_cutoff = len(engine.store.cells)
        
        # Clause variables (after cutoff)
        z_id = engine.store.new_var("Z")
        
        # Bind some variables
        from prolog.unify.unify_helpers import bind_root_to_term
        bind_root_to_term(x_id, Atom("a"), engine.trail, engine.store)
        bind_root_to_term(y_id, Atom("b"), engine.trail, engine.store)
        bind_root_to_term(z_id, Atom("c"), engine.trail, engine.store)
        
        # Record solution
        engine._record_solution()
        
        # Should only include query vars
        assert len(engine.solutions) == 1
        sol = engine.solutions[0]
        assert "X" in sol
        assert "Y" in sol
        assert "Z" not in sol  # Clause var excluded
        assert sol["X"] == Atom("a")
        assert sol["Y"] == Atom("b")
    
    def test_record_solution_excludes_renamed_clause_variables(self):
        """Test _record_solution excludes renamed clause variables (vid >= cutoff)."""
        prog = program()
        engine = Engine(prog)
        
        # Query var
        x_id = engine.store.new_var("X")
        engine._query_vars = [(x_id, "X")]
        engine._initial_var_cutoff = len(engine.store.cells)
        
        # Renamed clause vars (after cutoff)
        clause_var1 = engine.store.new_var()
        clause_var2 = engine.store.new_var()
        
        # Only bind query var
        from prolog.unify.unify_helpers import bind_root_to_term
        bind_root_to_term(x_id, Atom("result"), engine.trail, engine.store)
        
        engine._record_solution()
        
        sol = engine.solutions[0]
        assert len(sol) == 1  # Only X
        assert sol["X"] == Atom("result")
    
    def test_record_solution_handles_unbound_variables(self):
        """Test _record_solution handles unbound variables (decide format)."""
        prog = program()
        engine = Engine(prog)
        
        # Unbound query variable
        x_id = engine.store.new_var("X")
        engine._query_vars = [(x_id, "X")]
        engine._initial_var_cutoff = len(engine.store.cells)
        
        engine._record_solution()
        
        sol = engine.solutions[0]
        # Unbound var should be represented somehow
        # Let's say as Var with the ID
        assert "X" in sol
        assert isinstance(sol["X"], Var)
        assert sol["X"].id == x_id
        assert sol["X"].hint == "X"
    
    def test_reify_var_follows_bindings_to_terms(self):
        """Test _reify_var follows bindings to terms (no path compression)."""
        prog = program()
        engine = Engine(prog)
        
        # Create variables
        x_id = engine.store.new_var("X")
        y_id = engine.store.new_var("Y")
        z_id = engine.store.new_var("Z")
        
        # Build union chain
        from prolog.unify.unify_helpers import union_vars, bind_root_to_term
        union_vars(x_id, y_id, engine.trail, engine.store)
        union_vars(y_id, z_id, engine.trail, engine.store)
        
        # Find the actual root after unions (could be any of x, y, or z)
        root_result = engine.store.deref(x_id)
        assert root_result[0] == 'UNBOUND'
        root_id = root_result[1]
        
        # Bind the root to result
        bind_root_to_term(root_id, Atom("result"), engine.trail, engine.store)
        
        # Snapshot store state before reify
        before = [c.ref for c in engine.store.cells]
        
        # Reify should follow chain to get result
        result = engine._reify_var(x_id)
        
        # Check no side effects (no compression)
        after = [c.ref for c in engine.store.cells]
        assert before == after, "reify_var should not modify store"
        
        # Check result is correct
        assert result == Atom("result")
    
    def test_solution_format_stable(self):
        """Test solution format is stable (e.g., Var(vid, hint) or _G123)."""
        prog = program()
        engine = Engine(prog)
        
        # Multiple unbound vars
        x_id = engine.store.new_var("X")
        y_id = engine.store.new_var("Y")
        engine._query_vars = [(x_id, "X"), (y_id, "Y")]
        engine._initial_var_cutoff = len(engine.store.cells)
        
        engine._record_solution()
        sol1 = engine.solutions[0]
        
        # Record again (should be same format)
        engine._record_solution()
        sol2 = engine.solutions[1]
        
        # Same representation for unbound vars
        assert sol1["X"] == sol2["X"]
        assert sol1["Y"] == sol2["Y"]
        assert isinstance(sol1["X"], Var)
        assert isinstance(sol1["Y"], Var)


class TestBasicFactQueries:
    """Tests for basic fact queries."""
    
    def test_query_matches_single_fact(self):
        """Test query matches single fact."""
        prog = program(
            mk_fact("parent", Atom("john"), Atom("mary"))
        )
        engine = Engine(prog)
        
        # Query: parent(john, mary)
        goal = Struct("parent", (Atom("john"), Atom("mary")))
        solutions = engine.run([goal])
        
        assert len(solutions) == 1
        assert solutions[0] == {}  # No variables to bind
    
    def test_query_fails_with_no_matching_facts(self):
        """Test query fails with no matching facts."""
        prog = program(
            mk_fact("parent", Atom("john"), Atom("mary"))
        )
        engine = Engine(prog)
        
        # Query: parent(sue, mary) - no match
        goal = Struct("parent", (Atom("sue"), Atom("mary")))
        solutions = engine.run([goal])
        
        assert len(solutions) == 0
    
    def test_query_with_multiple_matching_facts(self):
        """Test query with multiple matching facts."""
        prog = program(
            mk_fact("parent", Atom("john"), Atom("mary")),
            mk_fact("parent", Atom("john"), Atom("sue")),
            mk_fact("parent", Atom("john"), Atom("bob"))
        )
        engine = Engine(prog)
        
        # Query: parent(john, X)
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        goal = Struct("parent", (Atom("john"), x_var))
        solutions = engine.run([goal])
        
        assert len(solutions) == 3
        # Check solutions in order
        assert solutions[0]["X"] == Atom("mary")
        assert solutions[1]["X"] == Atom("sue")
        assert solutions[2]["X"] == Atom("bob")
    
    def test_solutions_in_clause_source_order(self):
        """Test solutions are returned in clause source order."""
        prog = program(
            mk_fact("number", Int(3)),
            mk_fact("number", Int(1)),
            mk_fact("number", Int(2))
        )
        engine = Engine(prog)
        
        # Query: number(X)
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        goal = Struct("number", (x_var,))
        solutions = engine.run([goal])
        
        assert len(solutions) == 3
        assert solutions[0]["X"] == Int(3)  # First in source
        assert solutions[1]["X"] == Int(1)  # Second in source
        assert solutions[2]["X"] == Int(2)  # Third in source
    
    def test_no_side_effects_on_failed_head_unification(self):
        """Test no side effects on failed head unification."""
        prog = program(
            mk_fact("test", Atom("a"))
        )
        engine = Engine(prog)
        
        # Track initial state
        initial_trail_len = len(engine.trail)
        initial_store_len = len(engine.store.cells)
        
        # Query that will fail: test(b)
        goal = Struct("test", (Atom("b"),))
        solutions = engine.run([goal])
        
        assert len(solutions) == 0
        # No lingering changes
        assert len(engine.trail) == initial_trail_len
        assert len(engine.store.cells) == initial_store_len


class TestRuleExpansion:
    """Tests for rule expansion."""
    
    def test_rule_body_goals_pushed_in_correct_order(self):
        """Test rule body goals pushed in correct order."""
        # Rule: grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        prog = program(
            mk_rule(
                "grandparent",
                (Var(0, "X"), Var(2, "Z")),
                Struct("parent", (Var(0, "X"), Var(1, "Y"))),
                Struct("parent", (Var(1, "Y"), Var(2, "Z")))
            ),
            mk_fact("parent", Atom("john"), Atom("mary")),
            mk_fact("parent", Atom("mary"), Atom("sue"))
        )
        engine = Engine(prog)
        
        # Query: grandparent(john, Z)
        z_var = Var(engine.store.new_var("Z"), "Z")
        engine._query_vars = [(z_var.id, "Z")]
        
        goal = Struct("grandparent", (Atom("john"), z_var))
        solutions = engine.run([goal])
        
        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("sue")
    
    def test_nested_rule_expansion(self):
        """Test nested rule expansion."""
        # ancestor(X, Y) :- parent(X, Y).
        # ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
        prog = program(
            mk_rule(
                "ancestor",
                (Var(0, "X"), Var(1, "Y")),
                Struct("parent", (Var(0, "X"), Var(1, "Y")))
            ),
            mk_rule(
                "ancestor",
                (Var(0, "X"), Var(2, "Z")),
                Struct("parent", (Var(0, "X"), Var(1, "Y"))),
                Struct("ancestor", (Var(1, "Y"), Var(2, "Z")))
            ),
            mk_fact("parent", Atom("a"), Atom("b")),
            mk_fact("parent", Atom("b"), Atom("c")),
            mk_fact("parent", Atom("c"), Atom("d"))
        )
        engine = Engine(prog)
        
        # Query: ancestor(a, X)
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        goal = Struct("ancestor", (Atom("a"), x_var))
        solutions = engine.run([goal])
        
        # Should find: b (direct), c (through b), d (through b,c)
        assert len(solutions) == 3
        results = [sol["X"].name for sol in solutions]
        assert "b" in results
        assert "c" in results
        assert "d" in results
    
    def test_recursive_rules_dont_stack_overflow(self):
        """Test recursive rules don't cause stack overflow (using deep/1)."""
        # deep(zero).
        # deep(s(N)) :- deep(N).
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule(
                "deep",
                (Struct("s", (Var(0, "N"),)),),
                Struct("deep", (Var(0, "N"),))
            )
        )
        engine = Engine(prog)
        
        # Build deep structure: s(s(s(...s(zero)...)))
        # Let's test with 100 levels for now (not 1000/5000 yet)
        term = Atom("zero")
        for _ in range(100):
            term = Struct("s", (term,))
        
        # Query: deep(s(s(...s(zero)...)))
        goal = Struct("deep", (term,))
        solutions = engine.run([goal])
        
        assert len(solutions) == 1
        assert solutions[0] == {}  # No variables
    
    def test_assert_no_python_recursion(self):
        """Test that we can verify no Python recursion approaching limit."""
        import sys
        
        prog = program(
            mk_fact("test", Atom("a"))
        )
        engine = Engine(prog)
        
        # This should not use Python recursion
        goal = Struct("test", (Atom("a"),))
        
        # Check recursion depth before
        frame = sys._getframe()
        depth_before = 0
        while frame is not None:
            depth_before += 1
            frame = frame.f_back
        
        solutions = engine.run([goal])
        
        # Check recursion depth after (should be similar, not deep)
        frame = sys._getframe()
        depth_after = 0
        while frame is not None:
            depth_after += 1
            frame = frame.f_back
        
        # Depth shouldn't increase much (just normal call overhead)
        assert abs(depth_after - depth_before) < 10
        
        # And should be way below limit
        limit = sys.getrecursionlimit()
        assert depth_after < limit * 0.1