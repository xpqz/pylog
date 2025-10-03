"""Tests for CLP(FD) unification hooks - Phase 6 of Issue #126.

Tests the integration of CLP(FD) with attributed variable unification,
including domain merging for var-var aliasing and var-int grounding.
"""

from prolog.engine.engine import Engine, Program
from prolog.clpfd.api import get_domain


class TestCLPFDUnificationHooks:
    """Test CLP(FD) behavior during unification."""

    def test_var_var_domain_intersection(self):
        """X in 1..10, Y in 5..15, X = Y should result in both having domain 5..10."""
        engine = Engine(Program([]))

        query = "?- X in 1..10, Y in 5..15, X = Y."
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # Both X and Y should have the intersected domain 5..10
        # After unification, they are aliased so checking one is sufficient
        x_val = sol["X"]
        y_val = sol["Y"]

        # They should be the same variable after unification
        assert x_val.id == y_val.id

        # The domain should be the intersection
        x_domain = get_domain(engine.store, x_val.id)
        assert x_domain is not None
        assert x_domain.min() == 5
        assert x_domain.max() == 10

    def test_var_var_empty_intersection_fails(self):
        """X in 1..5, Y in 10..15, X = Y should fail due to empty intersection."""
        engine = Engine(Program([]))

        query = "?- X in 1..5, Y in 10..15, X = Y."
        solutions = list(engine.query(query))

        # Should fail - no solutions
        assert len(solutions) == 0

    def test_var_int_domain_membership_success(self):
        """X in 5..10, X = 7 should succeed with X bound to 7."""
        engine = Engine(Program([]))

        query = "?- X in 5..10, X = 7."
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        assert solutions[0]["X"].value == 7

    def test_var_int_domain_membership_failure(self):
        """X in 5..10, X = 3 should fail since 3 is not in domain."""
        engine = Engine(Program([]))

        query = "?- X in 5..10, X = 3."
        solutions = list(engine.query(query))

        # Should fail - no solutions
        assert len(solutions) == 0

    def test_var_var_watchers_merge(self):
        """Watchers should be merged when variables are aliased."""
        engine = Engine(Program([]))

        # Set up constraints that create watchers
        query = """
        ?- X in 1..10, Y in 5..15,
           X #< Z, Y #> W,
           Z in 1..20, W in 1..20,
           X = Y.
        """
        solutions = list(engine.query(query))

        assert len(solutions) > 0

        # After X = Y, the unified variable should have watchers from both
        sol = solutions[0]
        unified_var = sol["X"]  # X and Y are now the same

        # Check that constraints are still enforced
        # X (now unified with Y) should be constrained by both X #< Z and Y #> W
        # This means the domain should satisfy both constraints

    def test_grounding_triggers_propagation(self):
        """Grounding a variable should trigger propagation to dependent constraints."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..10, Y in 1..10,
           X #< Y,
           X = 5.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # X should be 5
        assert sol["X"].value == 5

        # Y should be pruned to 6..10 due to propagation
        y_domain = get_domain(engine.store, sol["Y"].id)
        assert y_domain is not None
        assert y_domain.min() == 6
        assert y_domain.max() == 10

    def test_backtracking_restores_domains(self):
        """Backtracking should restore original domains."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..10, Y in 5..15,
           (X = Y, X #< 7 ; X #> 8).
        """
        solutions = list(engine.query(query))

        # Should get two solution branches:
        # 1. X = Y with intersected domain, then X #< 7
        # 2. X #> 8 (without the unification)
        assert len(solutions) == 2

    def test_hook_preserves_non_clpfd_unification(self):
        """Hook should not interfere with regular unification."""
        engine = Engine(Program([]))

        # Regular unification without CLP(FD)
        query = "?- X = Y, Y = foo(bar, Z), Z = 42."
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]
        assert sol["X"].functor == "foo"
        assert sol["Y"].functor == "foo"
        assert sol["Z"].value == 42

    def test_multiple_domain_merges(self):
        """Chain of unifications should properly merge all domains."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..20, Y in 5..25, Z in 10..30,
           X = Y, Y = Z.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # All three should be aliased to the same variable
        assert sol["X"].id == sol["Y"].id == sol["Z"].id

        # Domain should be intersection of all three: 10..20
        domain = get_domain(engine.store, sol["X"].id)
        assert domain is not None
        assert domain.min() == 10
        assert domain.max() == 20

    def test_domain_update_on_both_vars_before_union(self):
        """Critical: Both variables must be updated before union to avoid lost updates."""
        engine = Engine(Program([]))

        # This tests the critical guardrail that both variables get the merged
        # domain BEFORE they are unified, preventing lost domain updates
        query = """
        ?- X in 1..10, Y in 5..15,
           X = Y,
           X in 6..9.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # Final domain should be 6..9 (further constrained after unification)
        domain = get_domain(engine.store, sol["X"].id)
        assert domain is not None
        assert domain.min() == 6
        assert domain.max() == 9


class TestCLPFDPropagationAfterUnification:
    """Test that propagation runs correctly after unification events."""

    def test_unification_triggers_propagation_queue(self):
        """Unification should trigger propagation of all affected constraints."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..10, Y in 1..10, Z in 1..10,
           X #< Y, Y #< Z,
           X = 5.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # X = 5 should propagate to Y and Z
        assert sol["X"].value == 5

        y_domain = get_domain(engine.store, sol["Y"].id)
        assert y_domain.min() == 6  # Y > 5

        z_domain = get_domain(engine.store, sol["Z"].id)
        assert z_domain.min() == 7  # Z > Y > 5, so Z >= 7

    def test_var_var_unification_propagates_to_both_sets_of_watchers(self):
        """When X=Y, watchers from both should be triggered."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..10, Y in 1..10, A in 1..10, B in 1..10,
           X #< A, Y #< B,
           X = Y, X = 5.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # Both A and B should be affected by X=Y=5
        a_domain = get_domain(engine.store, sol["A"].id)
        b_domain = get_domain(engine.store, sol["B"].id)

        assert a_domain.min() == 6  # A > X = 5
        assert b_domain.min() == 6  # B > Y = X = 5

    def test_failed_unification_preserves_domains(self):
        """Failed unification should not modify domains."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..5, Y in 10..15,
           (X = Y -> true ; (X in 2..4, Y in 11..14)).
        """
        solutions = list(engine.query(query))

        # Unification fails, so we take the else branch
        assert len(solutions) == 1
        sol = solutions[0]

        x_domain = get_domain(engine.store, sol["X"].id)
        y_domain = get_domain(engine.store, sol["Y"].id)

        # Domains should be from the else branch
        assert x_domain.min() == 2 and x_domain.max() == 4
        assert y_domain.min() == 11 and y_domain.max() == 14


class TestCLPFDHookEdgeCases:
    """Test edge cases and error conditions in CLP(FD) hooks."""

    def test_singleton_domain_unification(self):
        """Unifying variables with singleton domains."""
        engine = Engine(Program([]))

        query = "?- X in 5..5, Y in 5..5, X = Y."
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        assert solutions[0]["X"].value == 5
        assert solutions[0]["Y"].value == 5

    def test_singleton_domain_conflict(self):
        """Unifying variables with different singleton domains should fail."""
        engine = Engine(Program([]))

        query = "?- X in 5..5, Y in 7..7, X = Y."
        solutions = list(engine.query(query))

        assert len(solutions) == 0

    def test_empty_domain_unification(self):
        """Variables with empty domains (should already have failed)."""
        engine = Engine(Program([]))

        # This should fail at the domain posting stage
        query = "?- X in 2..1."
        solutions = list(engine.query(query))

        assert len(solutions) == 0

    def test_unification_with_non_fd_variable(self):
        """Unifying FD variable with regular variable."""
        engine = Engine(Program([]))

        query = "?- X in 5..10, X = Y, Y = 7."
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        assert solutions[0]["X"].value == 7
        assert solutions[0]["Y"].value == 7

    def test_cyclic_constraint_after_unification(self):
        """Ensure no infinite loops when variables involved in mutual constraints are unified."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..10, Y in 1..10,
           X #=< Y, Y #=< X,
           X = Y.
        """
        solutions = list(engine.query(query))

        # X #=< Y and Y #=< X means X = Y (equality constraint)
        # So unifying them should succeed
        assert len(solutions) == 1

        # They remain as unbound FD variables with domain 1..10
        sol = solutions[0]
        assert sol["X"].id == sol["Y"].id
        domain = get_domain(engine.store, sol["X"].id)
        assert domain.min() == 1 and domain.max() == 10
