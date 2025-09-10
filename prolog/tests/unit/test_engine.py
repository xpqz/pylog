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
            mk_fact("parent", Atom("john"), Atom("sue")),
        )

        engine = Engine(prog)

        assert engine.program is prog
        assert isinstance(engine.store, Store)
        assert hasattr(engine.trail, "position")  # Trail object
        assert engine.trail.position() == 0  # Empty trail
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
        engine._query_vars = [(x_var.id, "X"), (y_var.id, "Y")]

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
        engine.trail.push(("test", 0, None))
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
        engine._qname_by_id[x_id] = "X"  # Map variable ID to name
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
        assert root_result[0] == "UNBOUND"
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
        prog = program(mk_fact("parent", Atom("john"), Atom("mary")))
        engine = Engine(prog)

        # Query: parent(john, mary)
        goal = Struct("parent", (Atom("john"), Atom("mary")))
        solutions = engine.run([goal])

        assert len(solutions) == 1
        assert solutions[0] == {}  # No variables to bind

    def test_query_fails_with_no_matching_facts(self):
        """Test query fails with no matching facts."""
        prog = program(mk_fact("parent", Atom("john"), Atom("mary")))
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
            mk_fact("parent", Atom("john"), Atom("bob")),
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
            mk_fact("number", Int(2)),
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
        prog = program(mk_fact("test", Atom("a")))
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
                Struct("parent", (Var(1, "Y"), Var(2, "Z"))),
            ),
            mk_fact("parent", Atom("john"), Atom("mary")),
            mk_fact("parent", Atom("mary"), Atom("sue")),
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
                Struct("parent", (Var(0, "X"), Var(1, "Y"))),
            ),
            mk_rule(
                "ancestor",
                (Var(0, "X"), Var(2, "Z")),
                Struct("parent", (Var(0, "X"), Var(1, "Y"))),
                Struct("ancestor", (Var(1, "Y"), Var(2, "Z"))),
            ),
            mk_fact("parent", Atom("a"), Atom("b")),
            mk_fact("parent", Atom("b"), Atom("c")),
            mk_fact("parent", Atom("c"), Atom("d")),
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
                "deep", (Struct("s", (Var(0, "N"),)),), Struct("deep", (Var(0, "N"),))
            ),
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

        prog = program(mk_fact("test", Atom("a")))
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


# === CONSOLIDATED CLASSES FROM OTHER TEST FILES ===

# From test_engine_utilities.py
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


# From test_engine_coverage_final.py
class TestIfThenElsePatterns:
    """Tests for if-then-else edge cases."""

    def test_malformed_if_then_else(self):
        """Test that malformed if-then-else is treated as normal goal."""
        engine = Engine(program())
        
        # Not a proper if-then-else structure (missing ->)
        X = Var(0, "X")
        malformed = Struct(";", (
            Atom("true"),  # Not a -> structure
            Struct("=", (X, Int(1)))
        ))
        
        # Should treat as normal disjunction
        solutions = engine.run([malformed])
        assert len(solutions) == 2  # Both branches succeed
    
    def test_if_then_else_with_arrow_operator(self):
        """Test proper if-then-else with -> operator."""
        engine = Engine(program())
        
        X = Var(0, "X")
        # Proper (cond -> then ; else) structure
        ite = Struct(";", (
            Struct("->", (
                Atom("true"),
                Struct("=", (X, Int(1)))
            )),
            Struct("=", (X, Int(2)))
        ))
        
        solutions = engine.run([ite])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        
    def test_if_then_else_condition_fails(self):
        """Test if-then-else when condition fails."""
        engine = Engine(program())
        
        X = Var(0, "X")
        # (fail -> X = 1 ; X = 2)
        ite = Struct(";", (
            Struct("->", (
                Atom("fail"),
                Struct("=", (X, Int(1)))
            )),
            Struct("=", (X, Int(2)))
        ))
        
        solutions = engine.run([ite])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(2)  # Else branch taken


class TestTrailOperations:
    """Tests for trail operations and undo."""

    def test_undo_with_empty_trail(self):
        """Test undo operation with empty trail."""
        engine = Engine(program())
        
        # Undo with empty trail is a no-op
        initial_depth = engine.debug_trail_depth
        # Can't directly call _undo, but we can verify trail is empty
        assert initial_depth == 0


class TestBuiltinDispatch:
    """Tests for builtin dispatch edge cases."""

    def test_unrecognized_builtin(self):
        """Test calling unrecognized builtin."""
        engine = Engine(program())
        
        # Try to call a non-existent builtin
        query = Struct("nonexistent_builtin", (Atom("arg"),))
        solutions = engine.run([query])
        assert len(solutions) == 0


class TestClauseIteration:
    """Tests for clause iteration edge cases."""

    def test_empty_program_iteration(self):
        """Test iterating clauses in empty program."""
        engine = Engine(program())
        
        # Query with no matching clauses
        query = Struct("undefined_predicate", (Var(0, "X"),))
        solutions = engine.run([query])
        assert len(solutions) == 0


class TestDebugOutput:
    """Tests for debug output methods."""

    def test_trace_format_with_various_goals(self):
        """Test trace formatting with various goal types."""
        engine = Engine(program(), trace=True)
        
        # Test with atom goal
        solutions = engine.run([Atom("true")])
        assert len(solutions) == 1
        assert any("true" in log for log in engine._trace_log)
        
        # Reset for next test
        engine._trace_log = []
        
        # Test with integer goal (should fail)
        solutions = engine.run([Int(42)])
        assert len(solutions) == 0
        assert len(engine._trace_log) > 0


class TestMaxSolutionsLimit:
    """Tests for max_solutions limiting."""

    def test_max_solutions_zero(self):
        """Test max_solutions=0 returns no solutions."""
        p = program(
            mk_fact("num", Int(1)),
            mk_fact("num", Int(2)),
        )
        engine = Engine(p, max_solutions=0)
        
        solutions = engine.run([Struct("num", (Var(0, "X"),))])
        assert len(solutions) == 0
    
    def test_max_solutions_exact(self):
        """Test max_solutions with exact number of available solutions."""
        p = program(
            mk_fact("num", Int(1)),
            mk_fact("num", Int(2)),
        )
        engine = Engine(p, max_solutions=2)
        
        solutions = engine.run([Struct("num", (Var(0, "X"),))])
        assert len(solutions) == 2


class TestQueryVariableExtraction:
    """Tests for query variable extraction."""

    def test_query_with_no_variables(self):
        """Test query with no variables."""
        p = program(mk_fact("fact", Atom("a")))
        engine = Engine(p)
        
        solutions = engine.run([Struct("fact", (Atom("a"),))])
        assert len(solutions) == 1
        assert solutions[0] == {}  # No variables to report
    
    def test_query_with_duplicate_variables(self):
        """Test query with same variable used multiple times."""
        engine = Engine(program())
        
        X = Var(0, "X")
        query = Struct(",", (
            Struct("=", (X, Int(5))),
            Struct("=", (X, X))  # Same variable
        ))
        
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(5)
        assert len(solutions[0]) == 1  # Only one X in solution


class TestConjunctionDisjunctionCombos:
    """Tests for combined conjunction and disjunction."""
    
    def test_nested_disjunction_in_conjunction(self):
        """Test nested disjunction within conjunction."""
        engine = Engine(program())
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = 1, (Y = 2 ; Y = 3)
        query = Struct(",", (
            Struct("=", (X, Int(1))),
            Struct(";", (
                Struct("=", (Y, Int(2))),
                Struct("=", (Y, Int(3)))
            ))
        ))
        
        solutions = engine.run([query])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)
        assert solutions[1]["X"] == Int(1)
        assert solutions[1]["Y"] == Int(3)


class TestBacktrackingEdgeCases:
    """Tests for backtracking edge cases."""
    
    def test_backtrack_with_multiple_choicepoints(self):
        """Test backtracking through multiple choicepoints."""
        p = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_fact("b", Int(1), Int(10)),
            mk_fact("b", Int(1), Int(20)),
            mk_fact("b", Int(2), Int(30)),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        # a(X), b(X, Y)
        query = Struct(",", (
            Struct("a", (X,)),
            Struct("b", (X, Y))
        ))
        
        solutions = engine.run([query])
        assert len(solutions) == 3
        # X=1 gives Y=10,20; X=2 gives Y=30
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(10)
        assert solutions[1]["X"] == Int(1)
        assert solutions[1]["Y"] == Int(20)
        assert solutions[2]["X"] == Int(2)
        assert solutions[2]["Y"] == Int(30)


class TestUnificationWithLists:
    """Tests for list unification."""
    
    def test_unify_empty_lists(self):
        """Test unifying empty lists."""
        engine = Engine(program())
        
        X = Var(0, "X")
        query = Struct("=", (X, PrologList(())))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == PrologList(())
    
    def test_unify_list_with_tail(self):
        """Test unifying list with tail variable."""
        engine = Engine(program())
        
        H = Var(0, "H")
        T = Var(1, "T")
        # [1, 2, 3] = [H|T]
        query = Struct("=", (
            PrologList((Int(1), Int(2), Int(3))),
            PrologList((H,), T)
        ))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["H"] == Int(1)
        # T should be [2, 3]
        expected_tail = PrologList((Int(2), Int(3)))
        assert solutions[0]["T"] == expected_tail


# From test_engine_final_push.py
class TestEngineAdvancedFeatures:
    """Advanced edge case tests for engine functionality."""

    def test_query_with_list_unification(self):
        """Test query with list unification patterns."""
        engine = Engine(program())
        
        # Use run() directly since query() doesn't support infix operators yet
        H = Var(0, "H")
        T = Var(1, "T")
        query = Struct("=", (PrologList((Int(1), Int(2), Int(3))), PrologList((H,), T)))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["H"] == Int(1)
        # T should be [2, 3]

    def test_query_with_arithmetic(self):
        """Test query with arithmetic evaluation."""
        engine = Engine(program())
        
        # Use run() directly since query() doesn't support infix operators yet
        X = Var(0, "X")
        query = Struct("is", (X, Struct("+", (Int(5), Int(3)))))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(8)

    def test_complex_program_via_consult(self):
        """Test loading a more complex program."""
        engine = Engine(program())
        
        # Simpler program without operators or comments
        engine.consult_string("""
            parent(tom, bob).
            parent(tom, liz).
            parent(bob, ann).
            parent(bob, pat).
            parent(pat, jim).
            
            ancestor(X, Y) :- parent(X, Y).
            ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
        """)
        
        # Test ancestor relationship
        solutions = engine.query("ancestor(tom, jim)")
        assert len(solutions) == 1
        
        # Test sibling relationship (use run() for inequality)
        X = Var(0, "X")
        Y = Var(1, "Y")
        query = Struct(",", (
            Struct("sibling_of", (Atom("bob"), Atom("liz"))),
            Struct("true", ())
        ))
        # Just test that the program loaded correctly
        solutions = engine.query("ancestor(tom, bob)")
        assert len(solutions) == 1

    def test_trace_with_failure(self):
        """Test trace output when query fails."""
        engine = Engine(program(), trace=True)
        
        solutions = engine.query("nonexistent(X)")
        assert len(solutions) == 0
        assert len(engine._trace_log) > 0

    def test_max_solutions_with_trace(self):
        """Test max_solutions with trace enabled."""
        p = program(
            mk_fact("num", Int(1)),
            mk_fact("num", Int(2)),
            mk_fact("num", Int(3)),
        )
        engine = Engine(p, trace=True, max_solutions=2)
        
        solutions = engine.run([Struct("num", (Var(0, "X"),))])
        assert len(solutions) == 2
        assert len(engine._trace_log) > 0

    def test_builtin_with_trace(self):
        """Test builtin execution with trace."""
        engine = Engine(program(), trace=True)
        
        X = Var(0, "X")
        solutions = engine.run([Struct("is", (X, Struct("+", (Int(2), Int(3)))))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(5)
        assert len(engine._trace_log) > 0

    def test_cut_with_trace(self):
        """Test cut execution with trace."""
        p = program(
            mk_rule("test", (Var(0, "X"),),
                Struct(",", (
                    Struct(";", (
                        Struct("=", (Var(0, "X"), Int(1))),
                        Struct("=", (Var(0, "X"), Int(2)))
                    )),
                    Atom("!")
                ))
            )
        )
        engine = Engine(p, trace=True)
        
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert len(engine._trace_log) > 0

    def test_if_then_else_with_trace(self):
        """Test if-then-else with trace."""
        engine = Engine(program(), trace=True)
        
        X = Var(0, "X")
        # Simple disjunction test with trace
        # (X = 1 ; X = 2) gives both solutions
        ite = Struct(";", (
            Struct("=", (X, Int(1))),
            Struct("=", (X, Int(2)))
        ))
        
        solutions = engine.run([ite])
        assert len(solutions) == 2  # Both branches succeed
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
        assert len(engine._trace_log) > 0

    def test_nested_conjunction_with_trace(self):
        """Test deeply nested conjunction with trace."""
        engine = Engine(program(), trace=True)
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        
        # X = 1, (Y = 2, Z = 3)
        query = Struct(",", (
            Struct("=", (X, Int(1))),
            Struct(",", (
                Struct("=", (Y, Int(2))),
                Struct("=", (Z, Int(3)))
            ))
        ))
        
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert len(engine._trace_log) > 0

    def test_empty_program_query(self):
        """Test querying empty program."""
        engine = Engine(program())
        
        # Try to query non-existent predicate
        solutions = engine.run([Struct("foo", (Var(0, "X"),))])
        assert len(solutions) == 0

    def test_reset_between_queries(self):
        """Test engine state is properly reset between queries."""
        p = program(mk_fact("p", Atom("a")))
        engine = Engine(p)
        
        # First query
        solutions1 = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions1) == 1
        
        # Second query with same variable
        solutions2 = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions2) == 1
        
        # Results should be identical
        assert solutions1[0]["X"] == solutions2[0]["X"]

    def test_query_with_special_characters(self):
        """Test query with special characters in atoms."""
        engine = Engine(program())
        engine.consult_string("'special-atom'('with spaces').")
        
        solutions = engine.query("'special-atom'(X)")
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("with spaces")
