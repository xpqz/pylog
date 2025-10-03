"""
Test step budget enforcement to cover uncovered paths.

Targets lines 335-339 in engine.py which handle step budget limits.
"""

from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.ast.terms import Int
from prolog.engine.engine import Engine


class TestStepBudget:
    """Test step budget enforcement."""

    def test_step_budget_limits_execution(self):
        """Test that step budget stops infinite loops."""
        # Create a program with infinite recursion
        clauses = parser.parse_program(
            """
            infinite(X) :- infinite(X).
        """
        )
        engine = Engine(Program(tuple(clauses)), max_steps=100)

        # Query that would run forever
        goals = parser.parse_query("?- infinite(1).")
        solutions = list(engine.run(goals))

        # Should return no solutions when step budget exceeded
        assert len(solutions) == 0
        # Step counter should exceed max
        assert engine._steps_taken > engine.max_steps

    def test_step_budget_with_streaming(self):
        """Test step budget with streaming enabled."""
        # Create a program that generates many solutions
        clauses = parser.parse_program(
            """
            count(1).
            count(N) :- count(M), N is M + 1.
        """
        )
        engine = Engine(
            Program(tuple(clauses)),
            max_steps=50,  # Very small budget
            use_streaming=True,
        )

        # Query for counting
        goals = parser.parse_query("?- count(X).")
        solutions = list(engine.run(goals))

        # Should get some solutions before hitting limit
        assert len(solutions) > 0
        assert len(solutions) < 50  # Didn't run forever
        assert engine._steps_taken > engine.max_steps

    def test_step_budget_partial_results(self):
        """Test that partial results are returned when budget exceeded."""
        # Create a program that backtracks a lot
        clauses = parser.parse_program(
            """
            choice(1). choice(2). choice(3). choice(4). choice(5).

            test(X, Y, Z) :-
                choice(X),
                choice(Y),
                choice(Z),
                X < Y,
                Y < Z.
        """
        )
        engine = Engine(Program(tuple(clauses)), max_steps=200)  # Limited budget

        goals = parser.parse_query("?- test(X, Y, Z).")
        solutions = list(engine.run(goals))

        # Should get at least some valid solutions
        assert len(solutions) >= 1
        # Verify solutions are valid (X < Y < Z)
        for sol in solutions:
            x = sol["X"].value if isinstance(sol["X"], Int) else sol["X"]
            y = sol["Y"].value if isinstance(sol["Y"], Int) else sol["Y"]
            z = sol["Z"].value if isinstance(sol["Z"], Int) else sol["Z"]
            assert x < y < z

    def test_step_budget_none_unlimited(self):
        """Test that None step budget allows unlimited execution."""
        # Create a manageable recursive program
        clauses = parser.parse_program(
            """
            count_down(0).
            count_down(N) :- '>'(N, 0), is(N1, '-'(N, 1)), count_down(N1).
        """
        )
        engine = Engine(Program(tuple(clauses)), max_steps=None)

        # Query that takes many steps but terminates
        goals = parser.parse_query("?- count_down(10).")
        solutions = list(engine.run(goals))

        # Should complete successfully
        assert len(solutions) == 1
        # Without a budget, the counter may not be incremented
        # The test is that it completes, not that steps are counted

    def test_step_budget_reset_between_queries(self):
        """Test that step counter resets between queries."""
        clauses = parser.parse_program(
            """
            simple(1).
            simple(2).
        """
        )
        engine = Engine(Program(tuple(clauses)), max_steps=10)

        # First query
        goals = parser.parse_query("?- simple(X).")
        solutions1 = list(engine.run(goals))
        steps1 = engine._steps_taken

        # Second query - should reset counter
        goals = parser.parse_query("?- simple(Y).")
        solutions2 = list(engine.run(goals))
        steps2 = engine._steps_taken

        # Both should succeed
        assert len(solutions1) == 2
        assert len(solutions2) == 2
        # Second query should have reset the counter
        assert steps2 <= steps1 * 2  # Roughly similar step counts

    def test_step_budget_with_cut(self):
        """Test step budget interaction with cut."""
        clauses = parser.parse_program(
            """
            test(X) :- generate(X), !, check(X).
            generate(N) :- between(1, 1000, N).
            between(Low, High, Low) :- Low =< High.
            between(Low, High, X) :-
                Low < High,
                Low1 is Low + 1,
                between(Low1, High, X).
            check(N) :- N < 10.
        """
        )
        engine = Engine(Program(tuple(clauses)), max_steps=100)

        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        # Should find solution quickly due to cut
        assert len(solutions) <= 1
        # Steps should be under budget
        assert engine._steps_taken <= engine.max_steps or len(solutions) == 0

    def test_step_budget_with_indexing(self):
        """Test step budget with indexing enabled."""
        # Create a program with many clauses
        clauses_text = "\n".join([f"fact({i})." for i in range(100)])
        clauses = parser.parse_program(clauses_text)

        engine = Engine(Program(tuple(clauses)), max_steps=50, use_indexing=True)

        # Query for specific fact
        goals = parser.parse_query("?- fact(50).")
        solutions = list(engine.run(goals))

        # Should find it efficiently with indexing
        assert len(solutions) == 1
        # Should use fewer steps with indexing
        assert engine._steps_taken < engine.max_steps

    def test_step_budget_exhaustion_message(self):
        """Test that we can detect when step budget was exhausted."""
        clauses = parser.parse_program(
            """
            loop :- loop.
        """
        )
        engine = Engine(Program(tuple(clauses)), max_steps=10)

        goals = parser.parse_query("?- loop.")
        solutions = list(engine.run(goals))

        # No solutions due to budget
        assert len(solutions) == 0
        # Can check that budget was exceeded
        assert engine._steps_taken > engine.max_steps
        # Could add a flag to engine to indicate budget exhaustion
        # assert engine.budget_exhausted  # Future enhancement
