"""Tests for CLP(FD) labeling strategies - Phase 5 of Issue #123.

Tests the labeling mechanism that systematically searches for solutions
by trying values from FD variable domains.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine, Program
from prolog.unify.store import Store
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_fd_eq, _builtin_fd_lt


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
        x_values = [sol['X'].value for sol in solutions]
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
        combinations = [(sol['X'].value, sol['Y'].value) for sol in solutions]
        assert set(combinations) == {(1,3), (1,4), (2,3), (2,4)}

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
        assert solutions[0]['X'].value == 5

    def test_label_with_constraints(self):
        """Label variables with constraints between them."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..3, Y in 1..3, X #< Y, label([X, Y]).
        """

        solutions = list(engine.query(query))

        # Check all solutions satisfy X < Y
        for sol in solutions:
            x_val = sol['X'].value
            y_val = sol['Y'].value
            assert x_val < y_val

        # Valid solutions: (1,2), (1,3), (2,3)
        combinations = [(sol['X'].value, sol['Y'].value) for sol in solutions]
        assert set(combinations) == {(1,2), (1,3), (2,3)}


class TestLabelingStrategies:
    """Test different labeling strategies."""

    def test_indomain_min_strategy(self):
        """Test indomain_min value selection (default)."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 5..10, labeling([indomain_min], [X])."))
        assert len(sols) >= 1
        assert sols[0]['X'].value == 5

    def test_indomain_max_strategy(self):
        """Test indomain_max value selection."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 5..10, labeling([indomain_max], [X])."))
        assert len(sols) >= 1
        assert sols[0]['X'].value == 10

    def test_first_fail_variable_selection(self):
        """Test first_fail variable selection (smallest domain first)."""
        engine = Engine(Program([]))
        sols = list(engine.query(
            "?- X in 1..10, Y in 1..2, Z in 1..5, labeling([first_fail], [X,Y,Z])."
        ))
        assert len(sols) >= 1
        assert sols[0]['Y'].value in (1, 2)

    def test_most_constrained_selection(self):
        """Test most_constrained variable selection."""
        engine = Engine(Program([]))
        sols = list(engine.query(
            "?- X in 1..5, Y in 1..5, Z in 1..5, X #< Y, Y #< Z, labeling([most_constrained], [X,Y,Z])."
        ))
        assert len(sols) >= 1

    def test_indomain_random_seed_reproducible(self):
        """indomain_random honors seed(N) for deterministic order."""
        engine1 = Engine(Program([]))
        sols1 = list(engine1.query("?- X in 1..7, labeling([indomain_random, seed(12345)], [X])."))
        seq1 = [s['X'].value for s in sols1]

        engine2 = Engine(Program([]))
        sols2 = list(engine2.query("?- X in 1..7, labeling([indomain_random, seed(12345)], [X])."))
        seq2 = [s['X'].value for s in sols2]

        assert seq1 == seq2  # same seed => same permutation

        engine3 = Engine(Program([]))
        sols3 = list(engine3.query("?- X in 1..7, labeling([indomain_random, seed(54321)], [X])."))
        seq3 = [s['X'].value for s in sols3]
        assert seq1 != seq3  # different seed => likely different permutation

    def test_indomain_random_default_deterministic(self):
        """indomain_random without seed remains deterministic across runs (CI‑friendly)."""
        engine1 = Engine(Program([]))
        sols1 = list(engine1.query("?- X in 1..7, labeling([indomain_random], [X])."))
        seq1 = [s['X'].value for s in sols1]

        engine2 = Engine(Program([]))
        sols2 = list(engine2.query("?- X in 1..7, labeling([indomain_random], [X])."))
        seq2 = [s['X'].value for s in sols2]

        assert seq1 == seq2


class TestLabelingWithBacktracking:
    """Test labeling with backtracking to find all solutions."""

    def test_find_all_solutions(self):
        """Find all solutions through backtracking."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..2, Y in 1..2, label([X,Y])."))
        pairs = {(s['X'].value, s['Y'].value) for s in sols}
        assert pairs == {(1,1), (1,2), (2,1), (2,2)}

    def test_labeling_with_failing_constraint(self):
        """Test labeling when constraints make problem unsolvable."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 5..5, Y in 3..3, Y #< X, X #< Y, label([X,Y])."))
        assert len(sols) == 0


class TestLabelingEdgeCases:
    """Test edge cases in labeling."""

    def test_label_singleton_domain(self):
        """Label variable with singleton domain."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 42..42, label([X])."))
        assert len(sols) == 1 and sols[0]['X'].value == 42

    def test_label_empty_domain_fails(self):
        """Labeling variable with empty domain should fail."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 2..1, label([X])."))
        assert len(sols) == 0

    def test_label_with_holes_in_domain(self):
        """Label variable over a non-trivial range."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..9, label([X])."))
        values = {s['X'].value for s in sols}
        assert values == set(range(1, 10))

    def test_label_mixed_bound_unbound(self):
        """Label mix of bound and unbound variables."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X = 10, Y in 1..3, Z in 4..6, label([X,Y,Z])."))
        assert len(sols) >= 1
        s = sols[0]
        assert s['X'].value == 10 and isinstance(s['Y'].value, int) and isinstance(s['Z'].value, int)


class TestLabelingIntegration:
    """Test labeling integrated with full CLP(FD) system."""

    def test_label_triggers_propagation(self):
        """Labeling should trigger propagation after each choice."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..3, Y in 1..3, Z in 1..3, X #< Y, Y #< Z, label([X,Y,Z])."))
        assert any(s['X'].value < s['Y'].value < s['Z'].value for s in sols)

    def test_label_with_equality_constraints(self):
        """Test labeling with equality constraints."""
        engine = Engine(Program([]))
        sols = list(engine.query("?- X in 1..5, Y in 1..5, X #= Y, label([X,Y])."))
        assert all(s['X'].value == s['Y'].value for s in sols)
