"""Unit tests for CLP(FD) reification builtins.

Tests the user-facing builtins for B #<=> C, B #==> C, and B #<== C.
"""

from prolog.engine.engine import Engine
from prolog.ast.terms import Int
from prolog.tests.helpers import program


class TestReificationEquivalenceBuiltin:
    """Test the B #<=> C builtin for Boolean-constraint equivalence."""

    def test_equivalence_with_unbound_boolean_and_equality(self):
        """Test B #<=> (X #= Y) with unbound B and constrained X, Y."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Create query: B #<=> (X #= 3), X in 1..5
        result = engine.query("B #<=> (X #= 3), X in 1..5")
        solutions = list(result)

        # B should be constrained to {0,1}
        # X should be in 1..5
        # But the specific values depend on labeling
        assert len(solutions) > 0
        # Can't assert specific values without labeling

    def test_equivalence_b_1_forces_constraint(self):
        """Test that B = 1 forces the constraint to hold."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 should force X = 3
        result = engine.query("B #<=> (X #= 3), X in 1..5, B = 1")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["X"] == Int(3)
        assert env["B"] == Int(1)

    def test_equivalence_b_0_forces_constraint_negation(self):
        """Test that B = 0 forces the constraint to not hold."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 0 should force X #\= 3
        result = engine.query("B #<=> (X #= 3), X in 1..5, B = 0")
        solutions = list(result)

        # X can be 1, 2, 4, or 5
        assert len(solutions) == 1
        # Without labeling, X should still have domain excluding 3
        # This tests domain propagation

    def test_equivalence_constraint_true_forces_b_1(self):
        """Test that constraint being true forces B = 1."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 3 should force B = 1
        result = engine.query("B #<=> (X #= 3), X = 3")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["B"] == Int(1)
        assert env["X"] == Int(3)

    def test_equivalence_constraint_false_forces_b_0(self):
        """Test that constraint being false forces B = 0."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 5 makes (X #= 3) false, so B must be 0
        result = engine.query("B #<=> (X #= 3), X = 5")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["B"] == Int(0)
        assert env["X"] == Int(5)

    def test_equivalence_with_less_than(self):
        """Test B #<=> (X #< Y) with less-than constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 should force X < 5
        result = engine.query("B #<=> (X #< 5), X in 1..10, B = 1")
        solutions = list(result)

        assert len(solutions) == 1
        # X should be constrained to 1..4

    def test_equivalence_with_not_equal(self):
        """Test B #<=> (X #\\= Y) with not-equal constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 0 should force X = 3 (negation of X #\= 3)
        result = engine.query("B #<=> (X #\\= 3), X in 1..5, B = 0")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["X"] == Int(3)
        assert env["B"] == Int(0)

    def test_equivalence_contradiction_fails(self):
        """Test that contradictory constraints fail."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 and X = 5 contradict B #<=> (X #= 3)
        result = engine.query("B #<=> (X #= 3), B = 1, X = 5")
        solutions = list(result)

        assert len(solutions) == 0

    def test_equivalence_with_constants(self):
        """Test B #<=> C where C uses integer constants."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B #<=> (3 #= 3) should force B = 1
        result = engine.query("B #<=> (3 #= 3)")
        solutions = list(result)

        assert len(solutions) == 1
        assert solutions[0]["B"] == Int(1)

        # B #<=> (3 #= 5) should force B = 0
        result = engine.query("B #<=> (3 #= 5)")
        solutions = list(result)

        assert len(solutions) == 1
        assert solutions[0]["B"] == Int(0)

    def test_equivalence_symmetric_contradiction(self):
        """Test symmetric contradiction: B=0 with entailed constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 0 contradicts entailed constraint (3 #= 3)
        result = engine.query("B #<=> (3 #= 3), B = 0")
        solutions = list(result)

        assert len(solutions) == 0  # Should fail


class TestReificationImplicationBuiltin:
    """Test the B #==> C builtin for forward implication."""

    def test_implication_b_1_forces_constraint(self):
        """Test that B = 1 forces constraint to hold."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 should force X = 3
        result = engine.query("B #==> (X #= 3), X in 1..5, B = 1")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["X"] == Int(3)
        assert env["B"] == Int(1)

    def test_implication_b_0_allows_anything(self):
        """Test that B = 0 doesn't constrain C."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 0 allows X to be anything in 1..5
        result = engine.query("B #==> (X #= 3), X in 1..5, B = 0")
        solutions = list(result)

        assert len(solutions) == 1
        # X can be any value in 1..5

    def test_implication_constraint_false_forces_b_0(self):
        """Test that constraint being false forces B = 0."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 5 makes (X #= 3) false, so B must be 0 (contrapositive)
        result = engine.query("B #==> (X #= 3), X = 5")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["B"] == Int(0)
        assert env["X"] == Int(5)

    def test_implication_constraint_true_allows_b_any(self):
        """Test that constraint being true allows B to be 0 or 1."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 3 makes constraint true, B can be 0 or 1
        result = engine.query("B #==> (X #= 3), X = 3")
        solutions = list(result)

        assert len(solutions) == 1
        # B is still {0,1} (unconstrained Boolean)

    def test_implication_contradiction_fails(self):
        """Test that B = 1 with false constraint fails."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 requires X = 3, but X = 5 contradicts
        result = engine.query("B #==> (X #= 3), B = 1, X = 5")
        solutions = list(result)

        assert len(solutions) == 0

    def test_implication_entailed_noop(self):
        """Test that already-entailed constraint doesn't force B."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Constraint (3 #= 3) is already true, B remains unconstrained
        result = engine.query("B #==> (3 #= 3)")
        solutions = list(result)

        assert len(solutions) == 1
        # B should still be {0,1} (unconstrained)


class TestReificationBackwardImplicationBuiltin:
    """Test the B #<== C builtin for backward implication."""

    def test_backward_constraint_true_forces_b_1(self):
        """Test that constraint being true forces B = 1."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 3 makes constraint true, so B must be 1
        result = engine.query("B #<== (X #= 3), X = 3")
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["B"] == Int(1)
        assert env["X"] == Int(3)

    def test_backward_constraint_false_allows_b_any(self):
        """Test that constraint being false allows B to be 0 or 1."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 5 makes constraint false, B can be 0 or 1
        result = engine.query("B #<== (X #= 3), X = 5")
        solutions = list(result)

        assert len(solutions) == 1
        # B can be 0 or 1

    def test_backward_b_0_allows_constraint_any(self):
        """Test that B = 0 doesn't force constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 0 allows X to be anything
        result = engine.query("B #<== (X #= 3), X in 1..5, B = 0")
        solutions = list(result)

        assert len(solutions) == 1
        # X can be any value in 1..5

    def test_backward_contradiction_fails(self):
        """Test that B = 0 with true constraint fails."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # X = 3 requires B = 1, but B = 0 contradicts
        result = engine.query("B #<== (X #= 3), B = 0, X = 3")
        solutions = list(result)

        assert len(solutions) == 0


class TestReificationWithDifferentConstraints:
    """Test reification with various constraint types."""

    def test_reification_with_less_equal(self):
        """Test reification with #=< constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 should force X =< 5
        result = engine.query("B #<=> (X #=< 5), X in 1..10, B = 1")
        solutions = list(result)

        assert len(solutions) == 1
        # X should be constrained to 1..5

    def test_reification_with_greater_than(self):
        """Test reification with #> constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 0 should force X =< 5 (negation of X > 5)
        result = engine.query("B #<=> (X #> 5), X in 1..10, B = 0")
        solutions = list(result)

        assert len(solutions) == 1
        # X should be constrained to 1..5

    def test_reification_with_greater_equal(self):
        """Test reification with #>= constraint."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B = 1 should force X >= 5
        result = engine.query("B #<=> (X #>= 5), X in 1..10, B = 1")
        solutions = list(result)

        assert len(solutions) == 1
        # X should be constrained to 5..10


class TestReificationErrorHandling:
    """Test error handling in reification builtins."""

    def test_invalid_boolean_term_fails(self):
        """Test that non-Boolean terms fail."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # atom(foo) is not a valid Boolean
        result = engine.query("foo #<=> (X #= 3)")
        solutions = list(result)

        assert len(solutions) == 0

    def test_invalid_constraint_structure_fails(self):
        """Test that invalid constraint structures fail."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # foo(X) is not a valid constraint
        result = engine.query("B #<=> foo(X)")
        solutions = list(result)

        assert len(solutions) == 0

    def test_unsupported_constraint_fails(self):
        """Test that unsupported constraints fail."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Currently we don't support arbitrary expressions in reification
        # (future enhancement)
        result = engine.query("B #<=> (X + Y #= 10)")
        solutions = list(result)

        # This should fail for now (not implemented)
        assert len(solutions) == 0


class TestReificationIntegration:
    """Test integration of reification with other CLP(FD) features."""

    def test_multiple_reified_constraints(self):
        """Test multiple reified constraints in same problem."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Both constraints with their Booleans
        result = engine.query(
            """
            B1 #<=> (X #= 3),
            B2 #<=> (X #< 5),
            X in 1..10,
            B1 = 1.
        """
        )
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["X"] == Int(3)
        assert env["B1"] == Int(1)
        assert env["B2"] == Int(1)  # 3 < 5 is true

    def test_reification_with_labeling(self):
        """Test that reified constraints work with labeling."""
        engine = Engine(program(), trace=False, occurs_check=False)

        result = engine.query(
            """
            B #<=> (X #= 3),
            X in 1..5,
            label([B, X]).
        """
        )
        solutions = list(result)

        # Should get all valid combinations
        # B=0 with X in {1,2,4,5} (4 solutions)
        # B=1 with X=3 (1 solution)
        assert len(solutions) == 5

        # Check that B=1 only when X=3
        for sol in solutions:
            if sol["X"] == Int(3):
                assert sol["B"] == Int(1)
            else:
                assert sol["B"] == Int(0)

    def test_reification_chains(self):
        """Test chained reification relationships."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B1 implies B2, B2 implies constraint
        result = engine.query(
            """
            B1 #==> B2,
            B2 #==> (X #= 3),
            X in 1..5,
            B1 = 1.
        """
        )
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["B1"] == Int(1)
        assert env["B2"] == Int(1)
        assert env["X"] == Int(3)


class TestReificationComplexCases:
    """Test complex reification scenarios."""

    def test_reification_with_aliased_variables(self):
        """Test reification with aliased variables."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Y = X, then reify on Y
        result = engine.query(
            """
            Y = X,
            B #<=> (Y #= 3),
            X in 1..5,
            B = 1.
        """
        )
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["X"] == Int(3)
        assert env["Y"] == Int(3)
        assert env["B"] == Int(1)

    def test_reification_boolean_as_constraint_arg(self):
        """Test using a Boolean variable in constraint argument."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # B1 as both reification var and constraint argument
        result = engine.query(
            """
            B1 #<=> (B2 #= 1),
            B2 in 0..1,
            B1 = 1.
        """
        )
        solutions = list(result)

        assert len(solutions) == 1
        env = solutions[0]
        assert env["B1"] == Int(1)
        assert env["B2"] == Int(1)

    def test_reification_immediate_propagation(self):
        """Test that reification propagates immediately."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Setting X should immediately affect B
        result = engine.query(
            """
            B #<=> (X #= 3),
            X = 3.
        """
        )
        solutions = list(result)

        assert len(solutions) == 1
        assert solutions[0]["B"] == Int(1)

        # And vice versa
        result = engine.query(
            """
            B #<=> (X #= 3),
            X in 1..5,
            B = 0,
            X = 4.
        """
        )
        solutions = list(result)

        assert len(solutions) == 1  # Should succeed

    def test_reification_backtracking(self):
        """Test that reification works correctly with backtracking."""
        engine = Engine(program(), trace=False, occurs_check=False)

        # Use disjunction to test backtracking
        result = engine.query(
            """
            B #<=> (X #= 3),
            X in 1..5,
            (B = 1 ; B = 0).
        """
        )
        solutions = list(result)

        assert len(solutions) == 2

        # One solution with B=1, X=3
        # One solution with B=0, X in {1,2,4,5}
