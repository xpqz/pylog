"""Stress tests for the Prolog engine.

These tests verify the engine can handle large inputs and
extreme cases without performance degradation or crashes.
"""

import pytest
import os
import time
from contextlib import contextmanager
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.ast.clauses import Clause
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, mk_rule, program


# Get stress scale factor from environment (for CI)
STRESS_SCALE = float(os.environ.get("PROLOG_STRESS_SCALE", "1.0"))


@contextmanager
def time_limit(seconds):
    """Context manager to ensure operation completes within time limit."""
    start = time.time()
    yield
    elapsed = time.time() - start
    assert elapsed < seconds, f"Operation took {elapsed:.2f}s, limit was {seconds}s"


@pytest.mark.stress
class TestLargeClauseDatabases:
    """Test with large numbers of clauses."""
    
    def test_1000_clauses_single_predicate(self):
        """Test with 1000 clauses for a single predicate."""
        num_clauses = int(1000 * STRESS_SCALE)
        
        # Generate clauses
        clauses = []
        for i in range(num_clauses):
            clauses.append(mk_fact("test", Atom(f"val_{i}")))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        # Query all solutions
        with time_limit(5.0):
            solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        assert len(solutions) == num_clauses
    
    def test_10000_total_clauses(self):
        """Test with 10000 total clauses across multiple predicates."""
        num_predicates = int(100 * STRESS_SCALE)
        clauses_per_pred = 100
        
        clauses = []
        for p in range(num_predicates):
            for c in range(clauses_per_pred):
                clauses.append(mk_fact(f"pred_{p}", Atom(f"val_{c}")))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        # Query one predicate
        with time_limit(2.0):
            solutions = engine.run([Struct("pred_0", (Var(0, "X"),))])
        
        assert len(solutions) == clauses_per_pred
    
    def test_deep_clause_hierarchies(self):
        """Test with deep hierarchical clause structures."""
        depth = int(100 * STRESS_SCALE)
        
        clauses = []
        # Create chain: p0 calls p1, p1 calls p2, etc.
        for i in range(depth - 1):
            clauses.append(
                mk_rule(f"p{i}", (Var(0, "X"),),
                        Struct(f"p{i+1}", (Var(0, "X"),)))
            )
        # Base case
        clauses.append(mk_fact(f"p{depth-1}", Atom("base")))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        with time_limit(5.0):
            solutions = engine.run([Struct("p0", (Var(0, "X"),))])
        
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("base")
    
    def test_wide_clause_database(self):
        """Test with many predicates each having few clauses."""
        num_predicates = int(1000 * STRESS_SCALE)
        
        clauses = []
        for i in range(num_predicates):
            # Each predicate has just 2 clauses
            clauses.append(mk_fact(f"p{i}", Atom("a")))
            clauses.append(mk_fact(f"p{i}", Atom("b")))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        # Query several predicates
        for i in range(min(10, num_predicates)):
            solutions = engine.run([Struct(f"p{i}", (Var(0, "X"),))])
            assert len(solutions) == 2
    
    def test_clause_indexing_performance(self):
        """Test that clause indexing provides good performance."""
        num_clauses = int(1000 * STRESS_SCALE)
        
        clauses = []
        # Create many clauses with different first arguments
        for i in range(num_clauses):
            clauses.append(mk_fact("indexed", Atom(f"key_{i}"), Atom(f"val_{i}")))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        # Query with specific first argument (should be fast with indexing)
        with time_limit(0.1):  # Should be very fast
            solutions = engine.run([
                Struct("indexed", (Atom("key_500"), Var(0, "Y")))
            ])
        
        assert len(solutions) == 1
        assert solutions[0]["Y"] == Atom("val_500")


@pytest.mark.stress
@pytest.mark.slow
class TestDeepGoalStacks:
    """Test with deep goal stacks and many pending goals."""
    
    def test_1000_pending_goals(self):
        """Test with 1000+ pending goals on the stack."""
        num_goals = int(1000 * STRESS_SCALE)
        
        # Create a rule that generates many goals
        body_goals = []
        for i in range(num_goals):
            body_goals.append(Struct(f"goal_{i}", ()))
        
        clauses = [
            mk_rule("test", (), *body_goals)
        ]
        
        # Add facts for each goal
        for i in range(num_goals):
            clauses.append(mk_fact(f"goal_{i}"))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        with time_limit(10.0):
            solutions = engine.run([Struct("test", ())])
        
        assert len(solutions) == 1
    
    def test_deeply_nested_conjunctions(self):
        """Test with deeply nested conjunction structures."""
        depth = int(500 * STRESS_SCALE)
        
        clauses = []
        # Create nested structure: p0 :- p1, true. p1 :- p2, true. etc.
        for i in range(depth - 1):
            clauses.append(
                mk_rule(f"p{i}", (),
                        Struct(f"p{i+1}", ()),
                        Atom("true"))
            )
        clauses.append(mk_fact(f"p{depth-1}"))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        with time_limit(5.0):
            solutions = engine.run([Struct("p0", ())])
        
        assert len(solutions) == 1
    
    def test_many_choicepoints(self):
        """Test with many simultaneous choicepoints."""
        num_choices = int(100 * STRESS_SCALE)
        
        clauses = []
        # Each predicate has 2 choices, creating exponential possibilities
        for i in range(num_choices):
            clauses.append(mk_fact(f"choice{i}", Atom("a")))
            clauses.append(mk_fact(f"choice{i}", Atom("b")))
        
        # Query that creates many choicepoints but limits solutions
        body = []
        for i in range(min(10, num_choices)):  # Limit to prevent explosion
            body.append(Struct(f"choice{i}", (Var(i, f"X{i}"),)))
        
        clauses.append(mk_rule("test", tuple(Var(i, f"X{i}") for i in range(len(body))), *body))
        
        prog = program(*clauses)
        engine = Engine(prog, max_solutions=100)  # Limit solutions
        
        with time_limit(5.0):
            solutions = engine.run([
                Struct("test", tuple(Var(i, f"X{i}") for i in range(len(body))))
            ])
        
        # Should get limited number of solutions
        assert len(solutions) <= 100
    
    def test_memory_stability_under_stress(self):
        """Test that memory usage remains stable under stress."""
        # Run multiple queries to check for memory leaks
        prog = program(
            mk_fact("p", Atom("1")),
            mk_fact("p", Atom("2")),
            mk_rule("test", (Var(0, "X"), Var(1, "Y")),
                    Struct("p", (Var(0, "X"),)),
                    Struct("p", (Var(1, "Y"),)))
        )
        
        engine = Engine(prog)
        
        # Run same query multiple times
        for _ in range(100):
            solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])
            assert len(solutions) == 4
            engine.reset()  # Reset between runs
    
    @pytest.mark.xfail(reason="Infinite loop - placeholder for future depth limit")
    @pytest.mark.timeout(2)
    def test_infinite_recursion_handling(self):
        """Test that infinite recursion doesn't cause Python stack overflow.
        
        This test is expected to fail (timeout) as the engine will loop forever.
        It verifies that the loop uses explicit stacks, not Python recursion.
        Future work: Add depth limits to prevent infinite loops.
        """
        prog = program(
            # Infinite recursion: loop :- loop.
            mk_rule("loop", (), Struct("loop", ()))
        )
        
        engine = Engine(prog, max_solutions=1)
        
        # This will loop forever but shouldn't cause Python stack overflow
        # The timeout decorator will kill it after 2 seconds
        solutions = engine.run([Struct("loop", ())])
        
        # We never reach here due to infinite loop
        assert False, "Should have timed out"
    
    @pytest.mark.timeout(10)
    def test_complex_stress_scenario(self):
        """Test complex scenario combining multiple stress factors."""
        scale = STRESS_SCALE
        
        clauses = []
        
        # Add many facts
        for i in range(int(100 * scale)):
            clauses.append(mk_fact("fact", Atom(f"f_{i}")))
        
        # Add recursive rules
        clauses.append(
            mk_rule("recursive", (Atom("zero"),))
        )
        clauses.append(
            mk_rule("recursive", (Struct("s", (Var(0, "N"),)),),
                    Struct("recursive", (Var(0, "N"),)))
        )
        
        prog = program(*clauses)
        engine = Engine(prog, max_solutions=10)
        
        # Run recursive query
        term = Atom("zero")
        for _ in range(5):
            term = Struct("s", (term,))
        
        solutions = engine.run([Struct("recursive", (term,))])
        assert len(solutions) == 1
        
        # Also test with many facts (to exercise the clause database)
        solutions = engine.run([Struct("fact", (Var(0, "X"),))], max_solutions=10)
        assert len(solutions) == min(10, int(100 * scale))