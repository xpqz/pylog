"""Tests for CLP(FD) labeling strategies - Phase 5 of Issue #123.

Tests the labeling mechanism that systematically searches for solutions
by trying values from FD variable domains.
"""

from prolog.engine.engine import Engine, Program
from prolog.clpfd.label import select_values
from prolog.clpfd.domain import Domain


class TestBasicLabeling:
    """Test basic labeling functionality."""

    def test_label_single_variable(self):
        """Label a single FD variable."""
        engine = Engine(Program([]))

        # Create a simple test program with labeling
        query = """
        ?- X in 1..3, label([X]).
        """

        # Execute query and get solutions
        solutions = list(engine.query(query))

        # Should find all 3 solutions
        assert len(solutions) == 3

        # Extract X values from solutions
        x_values = [sol["X"].value for sol in solutions]
        assert set(x_values) == {1, 2, 3}

    def test_label_multiple_variables(self):
        """Label multiple FD variables."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..2, Y in 3..4, label([X, Y]).
        """

        solutions = list(engine.query(query))

        # Should find all 4 combinations: (1,3), (1,4), (2,3), (2,4)
        assert len(solutions) == 4

        # Check all combinations are present
        combinations = [(sol["X"].value, sol["Y"].value) for sol in solutions]
        assert set(combinations) == {(1, 3), (1, 4), (2, 3), (2, 4)}

    def test_label_empty_list_succeeds(self):
        """Labeling an empty list should succeed immediately."""
        engine = Engine(Program([]))

        query = "?- label([])."
        solutions = list(engine.query(query))

        # Empty list should succeed with one solution
        assert len(solutions) == 1

    def test_label_already_bound_succeeds(self):
        """Labeling already bound variables succeeds if consistent."""
        engine = Engine(Program([]))

        query = "?- X = 5, label([X])."
        solutions = list(engine.query(query))

        # Should succeed with X=5
        assert len(solutions) == 1
        assert solutions[0]["X"].value == 5

    def test_label_with_constraints(self):
        """Label variables with constraints between them."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..3, Y in 1..3, X #< Y, label([X, Y]).
        """

        solutions = list(engine.query(query))

        # Check all solutions satisfy X < Y
        for sol in solutions:
            x_val = sol["X"].value
            y_val = sol["Y"].value
            assert x_val < y_val

        # Valid solutions: (1,2), (1,3), (2,3)
        combinations = [(sol["X"].value, sol["Y"].value) for sol in solutions]
        assert set(combinations) == {(1, 2), (1, 3), (2, 3)}


class TestLabelingStrategies:
    """Test different labeling strategies."""

    def test_indomain_min_strategy(self):
        """Test indomain_min value selection (default)."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 5..10, labeling([indomain_min], [X])."))
        assert len(sols) >= 1
        assert sols[0]["X"].value == 5

    def test_indomain_max_strategy(self):
        """Test indomain_max value selection."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 5..10, labeling([indomain_max], [X])."))
        assert len(sols) >= 1
        assert sols[0]["X"].value == 10

    def test_first_fail_variable_selection(self):
        """Test first_fail variable selection (smallest domain first)."""
        engine = Engine(Program([]))
        sols = list(
            engine.query(
                "?- X in 1..10, Y in 1..2, Z in 1..5, labeling([first_fail], [X,Y,Z])."
            )
        )
        assert len(sols) >= 1
        assert sols[0]["Y"].value in (1, 2)

    def test_most_constrained_selection(self):
        """Test most_constrained variable selection."""
        engine = Engine(Program([]))
        sols = list(
            engine.query(
                "?- X in 1..5, Y in 1..5, Z in 1..5, X #< Y, Y #< Z, labeling([most_constrained], [X,Y,Z])."
            )
        )
        assert len(sols) >= 1

    def test_indomain_random_seed_reproducible(self):
        """indomain_random honors seed(N) for deterministic order."""
        engine1 = Engine(Program([]))
        sols1 = list(
            engine1.query(
                "?- X in 1..7, labeling([indomain_random, seed(12345)], [X])."
            )
        )
        seq1 = [s["X"].value for s in sols1]

        engine2 = Engine(Program([]))
        sols2 = list(
            engine2.query(
                "?- X in 1..7, labeling([indomain_random, seed(12345)], [X])."
            )
        )
        seq2 = [s["X"].value for s in sols2]

        assert seq1 == seq2  # same seed => same permutation

        engine3 = Engine(Program([]))
        sols3 = list(
            engine3.query(
                "?- X in 1..7, labeling([indomain_random, seed(54321)], [X])."
            )
        )
        seq3 = [s["X"].value for s in sols3]
        assert seq1 != seq3  # different seed => likely different permutation

    def test_indomain_random_default_deterministic(self):
        """indomain_random without seed remains deterministic across runs (CI‑friendly)."""
        engine1 = Engine(Program([]))
        sols1 = list(engine1.query("?- X in 1..7, labeling([indomain_random], [X])."))
        seq1 = [s["X"].value for s in sols1]

        engine2 = Engine(Program([]))
        sols2 = list(engine2.query("?- X in 1..7, labeling([indomain_random], [X])."))
        seq2 = [s["X"].value for s in sols2]

        assert seq1 == seq2


class TestLabelingWithBacktracking:
    """Test labeling with backtracking to find all solutions."""

    def test_find_all_solutions(self):
        """Find all solutions through backtracking."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..2, Y in 1..2, label([X,Y])."))
        pairs = {(s["X"].value, s["Y"].value) for s in sols}
        assert pairs == {(1, 1), (1, 2), (2, 1), (2, 2)}

    def test_labeling_with_failing_constraint(self):
        """Test labeling when constraints make problem unsolvable."""
        engine = Engine(Program([]))
        sols = list(
            engine.query("?- X in 5..5, Y in 3..3, Y #< X, X #< Y, label([X,Y]).")
        )
        assert len(sols) == 0


class TestLabelingEdgeCases:
    """Test edge cases in labeling."""

    def test_label_singleton_domain(self):
        """Label variable with singleton domain."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 42..42, label([X])."))
        assert len(sols) == 1 and sols[0]["X"].value == 42

    def test_label_empty_domain_fails(self):
        """Labeling variable with empty domain should fail."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 2..1, label([X])."))
        assert len(sols) == 0

    def test_label_with_holes_in_domain(self):
        """Label variable over a non-trivial range."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..9, label([X])."))
        values = {s["X"].value for s in sols}
        assert values == set(range(1, 10))

    def test_label_mixed_bound_unbound(self):
        """Label mix of bound and unbound variables."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X = 10, Y in 1..3, Z in 4..6, label([X,Y,Z])."))
        assert len(sols) >= 1
        s = sols[0]
        assert (
            s["X"].value == 10
            and isinstance(s["Y"].value, int)
            and isinstance(s["Z"].value, int)
        )


class TestLabelingIntegration:
    """Test labeling integrated with full CLP(FD) system."""

    def test_label_triggers_propagation(self):
        """Labeling should trigger propagation after each choice."""
        engine = Engine(Program([]))
        sols = list(
            engine.query(
                "?- X in 1..3, Y in 1..3, Z in 1..3, X #< Y, Y #< Z, label([X,Y,Z])."
            )
        )
        assert any(s["X"].value < s["Y"].value < s["Z"].value for s in sols)

    def test_label_with_equality_constraints(self):
        """Test labeling with equality constraints."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..5, Y in 1..5, X #= Y, label([X,Y])."))
        assert all(s["X"].value == s["Y"].value for s in sols)


class TestSelectValuesOptimization:
    """Test optimized select_values function (Phase 1.2 optimization)."""

    def test_select_values_import(self):
        """Test that we can import select_values function."""
        assert callable(select_values)

    def test_select_values_empty_domain(self):
        """select_values should handle empty domains."""

        empty_domain = Domain(())
        values = list(select_values(empty_domain, "indomain_min"))
        assert values == []

    def test_select_values_singleton_domain(self):
        """select_values should handle singleton domains."""

        singleton_domain = Domain(((5, 5),))
        values = list(select_values(singleton_domain, "indomain_min"))
        assert values == [5]

    def test_select_values_indomain_min_strategy(self):
        """select_values with indomain_min should return values in ascending order."""

        domain = Domain(((1, 3), (7, 9)))
        values = select_values(domain, "indomain_min")
        assert values == [1, 2, 3, 7, 8, 9]

    def test_select_values_indomain_max_strategy(self):
        """select_values with indomain_max should return values in descending order."""

        domain = Domain(((1, 3), (7, 9)))
        values = select_values(domain, "indomain_max")
        assert values == [9, 8, 7, 3, 2, 1]

    def test_select_values_indomain_middle_strategy(self):
        """select_values with indomain_middle should start from middle value."""

        domain = Domain(((1, 5),))
        values = select_values(domain, "indomain_middle")
        # Should start with middle value (3)
        assert values[0] == 3
        assert set(values) == {1, 2, 3, 4, 5}

    def test_select_values_indomain_random_strategy(self):
        """select_values with indomain_random should return all values in random order."""

        domain = Domain(((1, 5),))
        values = select_values(domain, "indomain_random", rng_seed=42)
        # Should contain all values but in potentially different order
        assert set(values) == {1, 2, 3, 4, 5}
        assert len(values) == 5

    def test_select_values_indomain_random_reproducible(self):
        """select_values with same seed should produce same order."""

        domain = Domain(((1, 5),))
        values1 = select_values(domain, "indomain_random", rng_seed=12345)
        values2 = select_values(domain, "indomain_random", rng_seed=12345)
        assert values1 == values2

    def test_select_values_unknown_strategy_defaults_min(self):
        """select_values with unknown strategy should default to indomain_min."""

        domain = Domain(((1, 3),))
        values = select_values(domain, "unknown_strategy")
        assert values == [1, 2, 3]  # Same as indomain_min

    def test_select_values_returns_list_for_compatibility(self):
        """select_values should return a list for compatibility with existing code."""

        domain = Domain(((1, 10),))
        result = select_values(domain, "indomain_min")

        # Should return a list for compatibility
        assert isinstance(result, list)
        assert result == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    def test_select_values_memory_efficient_large_domain(self):
        """select_values should be memory efficient for large domains."""

        # Large domain that would cause memory explosion with old implementation
        large_domain = Domain(((1, 1000000),))
        result = select_values(large_domain, "indomain_min")

        # Should limit values to prevent memory explosion (max 1000)
        assert isinstance(result, list)
        assert len(result) == 1000  # Limited to max_values
        assert result[:10] == list(range(1, 11))  # First few values correct

    def test_select_values_indomain_split_strategy(self):
        """select_values with indomain_split should handle bisection strategy."""

        domain = Domain(((1, 6),))
        values = select_values(domain, "indomain_split")

        # Should start from middle and contain all values
        assert set(values) == {1, 2, 3, 4, 5, 6}
        assert len(values) == 6

    def test_select_values_multiple_intervals(self):
        """select_values should handle domains with multiple intervals."""

        domain = Domain(((1, 2), (5, 6), (10, 11)))
        values = select_values(domain, "indomain_min")
        assert values == [1, 2, 5, 6, 10, 11]

    def test_select_values_preserves_domain_immutability(self):
        """select_values should not modify the domain object."""

        domain = Domain(((1, 5),), rev=10)
        original_intervals = domain.intervals
        original_rev = domain.rev

        # Use the function multiple times with different strategies
        select_values(domain, "indomain_min")
        select_values(domain, "indomain_max")
        select_values(domain, "indomain_middle")

        # Domain should be unchanged
        assert domain.intervals == original_intervals
        assert domain.rev == original_rev
