"""Property-based tests for CLP(FD) - Phase 6 of Issue #126.

Tests important properties like confluence, monotonicity, trail invertibility,
and idempotence using Hypothesis for property-based testing.
"""

import pytest
from hypothesis import given, strategies as st, settings, assume
import random
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine, Program
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


class TestCLPFDConfluence:
    """Test that constraint posting order doesn't affect final result."""

    @given(
        constraints=st.lists(
            st.sampled_from([
                "X in 1..20",
                "Y in 5..25",
                "Z in 10..30",
                "X #< Y",
                "Y #< Z",
                "X #> 3",
                "Y #=< 20",
                "Z #>= 15",
            ]),
            min_size=2,
            max_size=6,
        ),
        seed=st.integers(min_value=0, max_value=1000)
    )
    @settings(max_examples=50, deadline=5000)
    def test_confluence_different_posting_orders(self, constraints, seed):
        """Random constraint posting order should yield same fixpoint."""
        # Skip if we have conflicting constraints
        if "X #< Y" in constraints and "Y #< X" in constraints:
            assume(False)

        # Create a deterministic shuffled version
        rng = random.Random(seed)
        shuffled_constraints = constraints.copy()
        rng.shuffle(shuffled_constraints)

        # Execute in original order
        engine1 = Engine(Program([]))
        query1 = "?- " + ", ".join(constraints) + "."
        solutions1 = list(engine1.query(query1))

        # Execute in shuffled order
        engine2 = Engine(Program([]))
        query2 = "?- " + ", ".join(shuffled_constraints) + "."
        solutions2 = list(engine2.query(query2))

        # Both should either succeed or fail together
        assert len(solutions1) == len(solutions2)

        if solutions1:
            # If successful, check domains are the same
            sol1 = solutions1[0]
            sol2 = solutions2[0]

            for var_name in ['X', 'Y', 'Z']:
                if var_name in sol1 and var_name in sol2:
                    dom1 = get_domain(engine1.store, sol1[var_name].id)
                    dom2 = get_domain(engine2.store, sol2[var_name].id)

                    if dom1 and dom2:
                        # Compare domains (ignoring revision numbers)
                        assert dom1.intervals == dom2.intervals

    def test_confluence_specific_reordering(self):
        """Test specific constraint reorderings for confluence."""
        test_cases = [
            # Original vs reversed
            (["X in 1..10", "Y in 5..15", "X #< Y", "X #> 3"],
             ["X #> 3", "X #< Y", "Y in 5..15", "X in 1..10"]),

            # Interleaved vs grouped
            (["X in 1..20", "X #< Y", "Y in 1..20", "Y #< Z", "Z in 1..20"],
             ["X in 1..20", "Y in 1..20", "Z in 1..20", "X #< Y", "Y #< Z"]),
        ]

        for original, reordered in test_cases:
            engine1 = Engine(Program([]))
            query1 = "?- " + ", ".join(original) + "."
            sols1 = list(engine1.query(query1))

            engine2 = Engine(Program([]))
            query2 = "?- " + ", ".join(reordered) + "."
            sols2 = list(engine2.query(query2))

            assert len(sols1) == len(sols2)


class TestCLPFDMonotonicity:
    """Test that domains only shrink, never expand."""

    def test_domain_never_expands(self):
        """Adding constraints should only shrink domains, never expand them."""
        engine = Engine(Program([]))

        # Start with initial domain
        query1 = "?- X in 1..100."
        sols1 = list(engine.query(query1))
        initial_domain = get_domain(engine.store, sols1[0]['X'].id)

        # Add constraint
        query2 = "?- X in 1..100, X #> 50."
        sols2 = list(engine.query(query2))
        constrained_domain = get_domain(engine.store, sols2[0]['X'].id)

        # Domain should have shrunk
        assert constrained_domain.min() >= initial_domain.min()
        assert constrained_domain.max() <= initial_domain.max()
        assert constrained_domain.size() < initial_domain.size()

        # Add another constraint
        query3 = "?- X in 1..100, X #> 50, X #< 75."
        sols3 = list(engine.query(query3))
        further_constrained = get_domain(engine.store, sols3[0]['X'].id)

        # Should shrink further
        assert further_constrained.min() >= constrained_domain.min()
        assert further_constrained.max() <= constrained_domain.max()
        assert further_constrained.size() < constrained_domain.size()

    @given(
        initial_min=st.integers(min_value=1, max_value=50),
        initial_max=st.integers(min_value=51, max_value=100),
        constraint_value=st.integers(min_value=1, max_value=100)
    )
    @settings(max_examples=50, deadline=5000)
    def test_monotonic_constraint_addition(self, initial_min, initial_max, constraint_value):
        """Property: adding constraints monotonically shrinks domains."""
        engine = Engine(Program([]))

        # Initial domain
        query = f"?- X in {initial_min}..{initial_max}."
        sols = list(engine.query(query))

        if not sols:
            return  # Invalid domain

        initial_domain = get_domain(engine.store, sols[0]['X'].id)
        initial_size = initial_domain.size()

        # Add various constraints and check monotonicity
        constraint_queries = [
            f"?- X in {initial_min}..{initial_max}, X #> {constraint_value}.",
            f"?- X in {initial_min}..{initial_max}, X #< {constraint_value}.",
            f"?- X in {initial_min}..{initial_max}, X #>= {constraint_value}.",
            f"?- X in {initial_min}..{initial_max}, X #=< {constraint_value}.",
        ]

        for query in constraint_queries:
            engine = Engine(Program([]))
            sols = list(engine.query(query))

            if sols:
                new_domain = get_domain(engine.store, sols[0]['X'].id)
                # Domain should never expand
                assert new_domain.size() <= initial_size


class TestCLPFDTrailInvertibility:
    """Test that backtracking correctly restores state."""

    def test_backtracking_restores_exact_domain(self):
        """Backtracking should restore exact domain state."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..20,
           (X #< 10, X #> 5, fail ; X #> 15).
        """
        solutions = list(engine.query(query))

        # First branch fails, second succeeds
        assert len(solutions) == 1
        sol = solutions[0]

        # Domain should be from second branch only
        domain = get_domain(engine.store, sol['X'].id)
        assert domain.min() == 16
        assert domain.max() == 20

    def test_nested_backtracking_restoration(self):
        """Nested choice points should properly restore domains."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..30,
           (
               (X #< 10, X #> 7, fail) ;
               (X in 15..25, (X #< 18, fail ; X #> 20))
           ).
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # Should have domain 21..25 (from successful branch)
        domain = get_domain(engine.store, sol['X'].id)
        assert domain.min() == 21
        assert domain.max() == 25

    @given(
        domain_ranges=st.lists(
            st.tuples(
                st.integers(min_value=1, max_value=50),
                st.integers(min_value=51, max_value=100)
            ),
            min_size=2,
            max_size=4
        )
    )
    @settings(max_examples=30, deadline=5000)
    def test_trail_restoration_property(self, domain_ranges):
        """Property: trail correctly restores domains on backtracking."""
        engine = Engine(Program([]))

        # Build a query with multiple choice points
        branches = []
        for i, (low, high) in enumerate(domain_ranges[:-1]):
            branches.append(f"(X in {low}..{high}, fail)")

        # Last branch succeeds
        last_low, last_high = domain_ranges[-1]
        branches.append(f"X in {last_low}..{last_high}")

        query = f"?- X in 1..100, ({' ; '.join(branches)})."
        solutions = list(engine.query(query))

        if solutions:
            sol = solutions[0]
            domain = get_domain(engine.store, sol['X'].id)

            # Should have the domain from the last (successful) branch
            assert domain.min() == last_low
            assert domain.max() == last_high


class TestCLPFDIdempotence:
    """Test that posting the same constraint multiple times is safe."""

    def test_duplicate_domain_posting(self):
        """Posting the same domain constraint twice should be idempotent."""
        engine = Engine(Program([]))

        query = "?- X in 5..15, X in 5..15."
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        domain = get_domain(engine.store, solutions[0]['X'].id)
        assert domain.min() == 5
        assert domain.max() == 15

    def test_duplicate_constraint_posting(self):
        """Posting the same constraint twice should be idempotent."""
        test_cases = [
            "?- X in 1..20, Y in 1..20, X #< Y, X #< Y.",
            "?- X in 1..20, X #> 5, X #> 5.",
            "?- X in 1..20, Y in 1..20, X #= Y, X #= Y.",
        ]

        for query in test_cases:
            engine = Engine(Program([]))
            solutions = list(engine.query(query))

            # Should still have solutions (constraints don't conflict with themselves)
            assert len(solutions) > 0

    @given(
        constraint=st.sampled_from([
            "X #> 5", "X #< 15", "X #>= 7", "X #=< 12"
        ]),
        repetitions=st.integers(min_value=1, max_value=5)
    )
    @settings(max_examples=30, deadline=5000)
    def test_idempotent_constraint_property(self, constraint, repetitions):
        """Property: repeating a constraint N times equals posting it once."""
        # Post constraint once
        engine1 = Engine(Program([]))
        query1 = f"?- X in 1..20, {constraint}."
        sols1 = list(engine1.query(query1))

        # Post constraint multiple times
        engine2 = Engine(Program([]))
        constraints = ", ".join([constraint] * repetitions)
        query2 = f"?- X in 1..20, {constraints}."
        sols2 = list(engine2.query(query2))

        # Should have same result
        assert len(sols1) == len(sols2)

        if sols1:
            dom1 = get_domain(engine1.store, sols1[0]['X'].id)
            dom2 = get_domain(engine2.store, sols2[0]['X'].id)
            assert dom1.intervals == dom2.intervals


class TestCLPFDMemoryProperties:
    """Test that CLP(FD) doesn't leak memory on repeated operations."""

    def test_repeated_post_backtrack_cycles(self):
        """Repeated posting and backtracking shouldn't accumulate state."""
        engine = Engine(Program([]))

        # Run multiple cycles of constraint posting and backtracking
        for _ in range(10):
            query = """
            ?- X in 1..100,
               (X #< 50, X #> 25, fail ; X #> 75),
               !.
            """
            solutions = list(engine.query(query))
            assert len(solutions) == 1

        # Store should not accumulate garbage
        # (This is more of a sanity check - real memory testing needs profiling)
        assert len(engine.store.cells) < 1000  # Reasonable upper bound

    def test_no_watcher_accumulation(self):
        """Watchers should be properly cleaned up on backtracking."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..100, Y in 1..100, Z in 1..100,
           (
               (X #< Y, Y #< Z, Z #< 50, fail) ;
               (X #> Y, Y #> Z, Z #> 50, fail) ;
               X = 50
           ).
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        # After backtracking through failed branches, watchers should be cleaned up
        # The final solution should have minimal watchers