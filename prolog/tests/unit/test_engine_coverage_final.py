"""
Final targeted tests to achieve >90% coverage for engine.py.
Focuses on specific uncovered lines.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestIfThenElseEdgeCases:
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
        query = Struct("=", (X, List(())))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == List(())
    
    def test_unify_list_with_tail(self):
        """Test unifying list with tail variable."""
        engine = Engine(program())
        
        H = Var(0, "H")
        T = Var(1, "T")
        # [1, 2, 3] = [H|T]
        query = Struct("=", (
            List((Int(1), Int(2), Int(3))),
            List((H,), T)
        ))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["H"] == Int(1)
        # T should be [2, 3]
        expected_tail = List((Int(2), Int(3)))
        assert solutions[0]["T"] == expected_tail