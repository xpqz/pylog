"""Comprehensive integration tests for CLP(FD) reification.

This module tests the complete integration of reification with the existing
CLP(FD) system, including interaction with propagation, labeling, and backtracking.
"""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader
from prolog.ast.terms import Int


class TestReificationIntegrationBasic:
    """Basic integration tests for reification with CLP(FD) constraints."""

    def test_reification_with_all_constraint_types(self):
        """Test reification works with all supported constraint types."""
        reader = Reader()

        # Test #= constraint
        program = Program(())
        engine = Engine(program)
        query = reader.read_term("B #<=> (X #= 3), X in 1..5, label([B, X])")
        solutions = list(engine.solve(query))

        # Should have solutions with B=1,X=3 and B=0,X∈{1,2,4,5}
        assert len(solutions) == 5
        b1_x3 = any(s.get("B").value == 1 and s.get("X").value == 3 for s in solutions)
        assert b1_x3

        # Test #< constraint
        program = Program(())
        engine = Engine(program)
        query = reader.read_term("B #<=> (X #< 3), X in 1..5, label([B, X])")
        solutions = list(engine.solve(query))
        assert len(solutions) == 5

        # Test #=< constraint
        program = Program(())
        engine = Engine(program)
        query = reader.read_term("B #<=> (X #=< 3), X in 1..5, label([B, X])")
        solutions = list(engine.solve(query))
        assert len(solutions) == 5

        # Test #> constraint
        program = Program(())
        engine = Engine(program)
        query = reader.read_term("B #<=> (X #> 3), X in 1..5, label([B, X])")
        solutions = list(engine.solve(query))
        assert len(solutions) == 5

        # Test #>= constraint
        program = Program(())
        engine = Engine(program)
        query = reader.read_term("B #<=> (X #>= 3), X in 1..5, label([B, X])")
        solutions = list(engine.solve(query))
        assert len(solutions) == 5

        # Test #\= constraint
        program = Program(())
        engine = Engine(program)
        query = reader.read_term("B #<=> (X #\\= 3), X in 1..5, label([B, X])")
        solutions = list(engine.solve(query))
        assert len(solutions) == 5

    def test_mixed_reified_and_direct_constraints(self):
        """Test mixing reified and non-reified constraints."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Mix reified and direct constraints
        query = reader.read_term(
            """
            X in 1..10, Y in 1..10,
            X #< Y,
            B #<=> (Y #< 5),
            label([B, X, Y])
        """
        )

        solutions = list(engine.solve(query))

        # Verify all solutions satisfy X < Y
        for sol in solutions:
            assert sol["X"].value < sol["Y"].value

            # If Y < 5, then B = 1
            if sol["Y"].value < 5:
                assert sol["B"].value == 1
            else:
                assert sol["B"].value == 0

    def test_multiple_reifications_same_variables(self):
        """Test multiple reified constraints on the same variables."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..5,
            B1 #<=> (X #= 3),
            B2 #<=> (X #< 3),
            B3 #<=> (X #> 3),
            label([X, B1, B2, B3])
        """
        )

        solutions = list(engine.solve(query))

        for sol in solutions:
            x = sol["X"].value
            b1 = sol["B1"].value
            b2 = sol["B2"].value
            b3 = sol["B3"].value

            # Verify Boolean values are consistent with X
            assert b1 == (1 if x == 3 else 0)
            assert b2 == (1 if x < 3 else 0)
            assert b3 == (1 if x > 3 else 0)

            # Verify mutual exclusion: at most one can be true
            assert b1 + b2 + b3 <= 1


class TestReificationComplexNetworks:
    """Test complex reified constraint networks."""

    def test_chained_reifications(self):
        """Test chains of reified constraints with Boolean relationships."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # B1 iff (X < 5), B2 iff (Y > 3), B1 = B2
        query = reader.read_term(
            """
            X in 1..10, Y in 1..10,
            B1 #<=> (X #< 5),
            B2 #<=> (Y #> 3),
            B1 #= B2,
            label([X, Y, B1, B2])
        """
        )

        solutions = list(engine.solve(query))

        for sol in solutions:
            x = sol["X"].value
            y = sol["Y"].value
            b1 = sol["B1"].value
            b2 = sol["B2"].value

            # B1 and B2 must be equal
            assert b1 == b2

            # If B1=1, then X<5 and Y>3
            if b1 == 1:
                assert x < 5
                assert y > 3
            else:
                # If B1=0, then X>=5 and Y<=3
                assert x >= 5
                assert y <= 3

    def test_nested_reifications(self):
        """Test reification of Boolean expressions (simplified)."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Create a constraint that depends on another Boolean
        query = reader.read_term(
            """
            X in 1..5, Y in 1..5,
            B1 #<=> (X #= Y),
            B2 #<=> (B1 #= 1),
            label([X, Y, B1, B2])
        """
        )

        solutions = list(engine.solve(query))

        for sol in solutions:
            x = sol["X"].value
            y = sol["Y"].value
            b1 = sol["B1"].value
            b2 = sol["B2"].value

            # B1 = 1 iff X = Y
            assert b1 == (1 if x == y else 0)

            # B2 = 1 iff B1 = 1
            assert b2 == b1

    def test_circular_dependencies(self):
        """Test that circular reification dependencies are handled correctly."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Create constraints that could potentially loop
        # B1 affects X, X affects B2, B2 affects Y, Y affects B1
        query = reader.read_term(
            """
            X in 1..5, Y in 1..5,
            B1 #<=> (X #< 3),
            B2 #<=> (Y #> 3),
            B1 #==> (Y #>= 4),
            B2 #==> (X #=< 2),
            label([X, Y, B1, B2])
        """
        )

        solutions = list(engine.solve(query))

        # Should find valid solutions without infinite loops
        assert len(solutions) > 0

        for sol in solutions:
            x = sol["X"].value
            y = sol["Y"].value
            b1 = sol["B1"].value
            b2 = sol["B2"].value

            # Verify all constraints
            assert b1 == (1 if x < 3 else 0)
            assert b2 == (1 if y > 3 else 0)

            if b1 == 1:
                assert y >= 4
            if b2 == 1:
                assert x <= 2


class TestReificationWithLabeling:
    """Test reification interaction with labeling strategies."""

    def test_boolean_variable_labeling(self):
        """Test that Boolean variables can be labeled."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..3,
            B #<=> (X #= 2),
            label([B]),
            label([X])
        """
        )

        solutions = list(engine.solve(query))

        # Should get all valid combinations
        assert len(solutions) == 3

        # Check that labeling B first constrains X appropriately
        b0_solutions = [s for s in solutions if s["B"].value == 0]
        b1_solutions = [s for s in solutions if s["B"].value == 1]

        assert len(b0_solutions) == 2  # X ∈ {1, 3}
        assert len(b1_solutions) == 1  # X = 2

        for sol in b1_solutions:
            assert sol["X"].value == 2

    def test_mixed_integer_boolean_labeling(self):
        """Test labeling with mixed integer and Boolean variables."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..5, Y in 1..5,
            B1 #<=> (X #< Y),
            B2 #<=> (X #= Y),
            B3 #<=> (X #> Y),
            B1 + B2 + B3 #= 1,
            label([X, Y, B1, B2, B3])
        """
        )

        solutions = list(engine.solve(query))

        # All 25 combinations should be found
        assert len(solutions) == 25

        for sol in solutions:
            x = sol["X"].value
            y = sol["Y"].value
            b1 = sol["B1"].value
            b2 = sol["B2"].value
            b3 = sol["B3"].value

            # Exactly one Boolean should be 1
            assert b1 + b2 + b3 == 1

            if b1 == 1:
                assert x < y
            elif b2 == 1:
                assert x == y
            else:
                assert x > y

    def test_labeling_strategies_with_reification(self):
        """Test different labeling strategies work with reified constraints."""
        reader = Reader()

        # Test first-fail strategy
        program = Program(())
        engine = Engine(program)
        query = reader.read_term(
            """
            X in 1..10, Y in 1..10,
            B #<=> (X #< Y),
            labeling([ff], [X, Y, B])
        """
        )

        solutions = list(engine.solve(query))
        assert len(solutions) == 100  # All 10x10 combinations

        # Test min value strategy
        program = Program(())
        engine = Engine(program)
        query = reader.read_term(
            """
            X in 1..5,
            B #<=> (X #> 3),
            labeling([min], [X, B])
        """
        )

        first_solution = next(iter(engine.solve(query)))
        # With min strategy, should try X=1 first, which makes B=0
        assert first_solution["X"].value == 1
        assert first_solution["B"].value == 0


class TestReificationBacktracking:
    """Test backtracking behavior with reified constraints."""

    def test_state_restoration_after_failure(self):
        """Test that state is properly restored after backtracking."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..5,
            B #<=> (X #= 3),
            (B #= 1, X #= 4 ; B #= 0, label([X]))
        """
        )

        solutions = list(engine.solve(query))

        # First branch fails (B=1 implies X=3, but X=4 contradicts)
        # Second branch succeeds with B=0, X∈{1,2,4,5}
        assert len(solutions) == 4

        for sol in solutions:
            assert sol["B"].value == 0
            assert sol["X"].value != 3

    def test_watcher_cleanup_on_backtrack(self):
        """Test that watchers are properly cleaned up on backtracking."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Create constraints that add watchers, then backtrack
        query = reader.read_term(
            """
            X in 1..5, Y in 1..5,
            (
                B1 #<=> (X #< Y),
                B2 #<=> (X #= Y),
                B1 #= 1, B2 #= 1, fail
            ;
                X #= Y, label([X, Y])
            )
        """
        )

        solutions = list(engine.solve(query))

        # First branch fails (can't have both X<Y and X=Y)
        # Second branch succeeds with X=Y
        assert len(solutions) == 5

        for sol in solutions:
            assert sol["X"].value == sol["Y"].value

    def test_trail_correctness_with_reification(self):
        """Test that the trail correctly restores domains and Boolean values."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..10,
            B #<=> (X #< 5),
            (B #= 1, X #= 7, fail ; B #= 0, X #> 5, label([X]))
        """
        )

        solutions = list(engine.solve(query))

        # First branch fails (B=1 implies X<5, but X=7 contradicts)
        # Second branch succeeds with B=0 and X>5
        assert all(sol["B"].value == 0 for sol in solutions)
        assert all(sol["X"].value > 5 for sol in solutions)


class TestReificationEdgeCases:
    """Test edge cases and error conditions."""

    def test_entailment_detection_optimization(self):
        """Test that entailment is detected early to avoid unnecessary work."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Constraint is immediately entailed
        query = reader.read_term(
            """
            X in 10..20, Y in 1..5,
            B #<=> (X #> Y),
            B #= Res
        """
        )

        solution = next(iter(engine.solve(query)))
        # B should be immediately set to 1
        res = solution["Res"]
        assert isinstance(res, Int) and res.value == 1

        # Constraint is immediately disentailed
        program = Program(())
        engine = Engine(program)
        query = reader.read_term(
            """
            X in 1..5, Y in 10..20,
            B #<=> (X #> Y),
            B #= Res
        """
        )

        solution = next(iter(engine.solve(query)))
        # B should be immediately set to 0
        res = solution["Res"]
        assert isinstance(res, Int) and res.value == 0

    def test_ground_boolean_propagation(self):
        """Test propagation when Boolean is ground at posting time."""
        reader = Reader()

        # B=1 at posting time
        program = Program(())
        engine = Engine(program)
        query = reader.read_term(
            """
            X in 1..10,
            1 #<=> (X #= 5),
            X #= Res
        """
        )

        solution = next(iter(engine.solve(query)))
        res = solution["Res"]
        assert isinstance(res, Int) and res.value == 5

        # B=0 at posting time
        program = Program(())
        engine = Engine(program)
        query = reader.read_term(
            """
            X in 1..10,
            0 #<=> (X #= 5),
            label([X])
        """
        )

        solutions = list(engine.solve(query))
        assert len(solutions) == 9  # All except 5
        assert all(sol["X"].value != 5 for sol in solutions)

    def test_contradictory_reifications(self):
        """Test that contradictory reifications fail appropriately."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # B=1 and B=0 for the same constraint
        query = reader.read_term(
            """
            X in 1..5,
            B1 #<=> (X #= 3),
            B2 #<=> (X #= 3),
            B1 #= 1,
            B2 #= 0
        """
        )

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # No solution possible

    def test_reification_with_singleton_domains(self):
        """Test reification when variables have singleton domains."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 3..3, Y in 3..3,
            B #<=> (X #= Y),
            B #= Res
        """
        )

        solution = next(iter(engine.solve(query)))
        # X=3, Y=3, so X=Y is true
        res = solution["Res"]
        assert isinstance(res, Int) and res.value == 1

    def test_reification_with_empty_domains(self):
        """Test that reification handles empty domains gracefully."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Create an impossible constraint that leads to empty domain
        query = reader.read_term(
            """
            X in 1..5,
            X #< 1,
            B #<=> (X #= 3)
        """
        )

        solutions = list(engine.solve(query))
        assert len(solutions) == 0  # No solution due to empty domain


class TestReificationImplications:
    """Test forward and backward implication operators."""

    def test_forward_implication_behavior(self):
        """Test B #==> C (if B then C)."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..5, Y in 1..5,
            B #==> (X #< Y),
            label([B, X, Y])
        """
        )

        solutions = list(engine.solve(query))

        for sol in solutions:
            b = sol["B"].value
            x = sol["X"].value
            y = sol["Y"].value

            # If B=1, then X < Y must hold
            if b == 1:
                assert x < y
            # If B=0, X and Y can be anything

    def test_backward_implication_behavior(self):
        """Test B #<== C (if C then B).

        Backward implication only enforces one direction:
        - If constraint is true, B must be 1
        - If B is 0, constraint can be anything (not enforced to be false)

        This is weaker than equivalence. To properly test, we need to
        label X and Y first, then B.
        """
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Label X and Y first, then B - this ensures B is constrained properly
        query = reader.read_term(
            """
            X in 1..5, Y in 1..5,
            B #<== (X #< Y),
            label([X, Y, B])
        """
        )

        solutions = list(engine.solve(query))

        for sol in solutions:
            b = sol["B"].value
            x = sol["X"].value
            y = sol["Y"].value

            # If X < Y, then B must be 1
            if x < y:
                assert b == 1
            # If X >= Y, B can be 0 or 1

    def test_combined_implications(self):
        """Test combining forward and backward implications."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # B1 implies X<5, X<3 implies B2
        query = reader.read_term(
            """
            X in 1..10,
            B1 #==> (X #< 5),
            B2 #<== (X #< 3),
            B1 #= 1,
            label([X, B2])
        """
        )

        solutions = list(engine.solve(query))

        for sol in solutions:
            x = sol["X"].value
            b2 = sol["B2"].value

            # B1=1 forces X<5
            assert x < 5

            # If X<3, then B2=1
            if x < 3:
                assert b2 == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
