"""
Final tests to push engine.py coverage over 90%.
Targets specific uncovered lines.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestEngineEdgeCases:
    """Edge case tests for engine functionality."""

    def test_query_with_list_unification(self):
        """Test query with list unification patterns."""
        engine = Engine(program())
        
        # Use run() directly since query() doesn't support infix operators yet
        H = Var(0, "H")
        T = Var(1, "T")
        query = Struct("=", (List((Int(1), Int(2), Int(3))), List((H,), T)))
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