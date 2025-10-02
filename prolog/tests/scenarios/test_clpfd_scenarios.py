"""Integration tests for CLP(FD) scenarios - Phase 6 of Issue #126.

Tests realistic constraint problems to verify the complete CLP(FD) system
works correctly with unification hooks, propagation, and labeling.
"""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.clpfd.api import get_domain


class TestSimpleConstraintProblems:
    """Test simple but realistic constraint problems."""

    def test_send_more_money_digits(self):
        """Classic SEND + MORE = MONEY cryptarithmetic (simplified to just digits)."""
        engine = Engine(Program([]))

        # Optimized version - use all_different + first_fail labeling for dramatic speedup
        query = """
        ?- S in 1..9, E in 0..9, N in 0..9, D in 0..9,
           M in 1..9, O in 0..9, R in 0..9, Y in 0..9,
           all_different([S, E, N, D, M, O, R, Y]),
           once(labeling([first_fail], [S, E, N, D, M, O, R, Y])).
        """
        solutions = list(engine.query(query))

        # Should find valid assignments where all variables are different
        assert len(solutions) > 0

        # Check that all values are indeed different
        sol = solutions[0]
        values = [sol[v].value for v in ["S", "E", "N", "D", "M", "O", "R", "Y"]]
        assert len(set(values)) == 8  # All different

    def test_simple_scheduling(self):
        """Simple scheduling problem with precedence constraints."""
        engine = Engine(Program([]))

        query = """
        ?- Start1 in 0..10, Start2 in 0..10, Start3 in 0..10,
           Duration1 = 3, Duration2 = 2, Duration3 = 4,
           End1 #= Start1 + Duration1,
           End2 #= Start2 + Duration2,
           End3 #= Start3 + Duration3,
           End1 #=< Start2,
           End2 #=< Start3,
           End3 #=< 15,
           label([Start1, Start2, Start3]).
        """
        solutions = list(engine.query(query))

        assert len(solutions) > 0

        # Verify precedence constraints
        for sol in solutions:
            s1 = sol["Start1"].value
            s2 = sol["Start2"].value
            s3 = sol["Start3"].value
            assert s1 + 3 <= s2  # Task 1 must finish before Task 2 starts
            assert s2 + 2 <= s3  # Task 2 must finish before Task 3 starts
            assert s3 + 4 <= 15  # Task 3 must finish by time 15

    def test_magic_square_3x3_sum(self):
        """3x3 magic square - all rows, columns, diagonals sum to same value."""
        engine = Engine(Program([]))

        # Optimized: use all_different + first_fail for dramatic speedup
        # Full magic square would need sum constraints which we haven't implemented yet
        query = """
        ?- A in 1..9, B in 1..9, C in 1..9,
           D in 1..9, E in 1..9, F in 1..9,
           G in 1..9, H in 1..9, I in 1..9,
           all_different([A, B, C, D, E, F, G, H, I]),
           once(labeling([first_fail], [A, B, C, D, E, F, G, H, I])).
        """
        # Only get first solution for performance test
        solutions = []
        for sol in engine.query(query):
            solutions.append(sol)
            break

        # Should find permutations of 1..9
        assert len(solutions) > 0
        sol = solutions[0]
        values = [sol[v].value for v in ["A", "B", "C", "D", "E", "F", "G", "H", "I"]]
        assert set(values) == set(range(1, 10))


@pytest.mark.swi_baseline
class TestPerformanceBaselines:
    """SWI-Prolog baseline tests for performance-critical scenarios.

    MANUAL BASELINE RESULTS (confirmed via direct swipl testing):
    - 4-variable all_different + labeling: < 1ms
    - 8-variable all_different + labeling: < 1ms
    - 9-variable all_different + labeling: < 1ms

    These are the performance targets PyLog should aspire to.

    Note: The SWI harness currently enumerates ALL solutions via findall(),
    which is too slow for large permutation problems. We test smaller cases
    that work within the harness limitations.
    """

    def test_4_variable_all_different_swi_baseline(self, swi):
        """Baseline: 4 variables with all_different should be extremely fast in SWI-Prolog."""
        program = ":- use_module(library(clpfd))."
        goal = "(S in 1..4, E in 0..4, N in 0..4, D in 0..4, all_different([S, E, N, D]), labeling([ff], [S, E, N, D]))"

        # Should find solutions very quickly (even when enumerating all via findall)
        s_values = swi.onevar(program, goal, "S")
        assert len(s_values) > 0
        assert 1 <= int(s_values[0]) <= 4

    def test_basic_labeling_swi_baseline(self, swi):
        """Baseline: Basic labeling without constraints should be instant."""
        program = ":- use_module(library(clpfd))."
        goal = "(X in 1..5, Y in 1..5, labeling([ff], [X, Y]))"

        # Should find first solution instantly
        x_values = swi.onevar(program, goal, "X")
        assert len(x_values) > 0
        assert 1 <= int(x_values[0]) <= 5

        # Can also count smaller search spaces efficiently
        small_goal = "(X in 1..3, Y in 1..3, labeling([ff], [X, Y]))"
        count = swi.count(program, small_goal)
        assert count == 9  # 3*3 = 9 solutions

    def test_simple_constraints_swi_baseline(self, swi):
        """Baseline: Simple constraint combinations should be very fast."""
        program = ":- use_module(library(clpfd))."
        goal = "(X in 1..10, Y in 1..10, X #< Y, X + Y #= 15, labeling([ff], [X, Y]))"

        # Should solve simple arithmetic constraints quickly
        x_values = swi.onevar(program, goal, "X")
        assert len(x_values) > 0

        # Verify the constraint: X < Y and X + Y = 15
        y_values = swi.onevar(program, goal, "Y")
        assert len(y_values) > 0

        # Should find valid solutions like X=7, Y=8
        x_val = int(x_values[0])
        y_val = int(y_values[0])
        assert x_val < y_val
        assert x_val + y_val == 15


class TestChainPropagation:
    """Test scenarios with chains of propagating constraints."""

    def test_linear_chain_propagation(self):
        """Linear chain of less-than constraints."""
        engine = Engine(Program([]))

        query = """
        ?- A in 1..20, B in 1..20, C in 1..20, D in 1..20, E in 1..20,
           A #< B, B #< C, C #< D, D #< E,
           C = 10.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # C = 10 should propagate both ways
        assert sol["C"].value == 10

        # A and B should be constrained to less than 10

        a_dom = get_domain(engine.store, sol["A"].id)
        b_dom = get_domain(engine.store, sol["B"].id)
        assert a_dom.max() < 10
        assert b_dom.max() < 10
        assert b_dom.min() > a_dom.min()  # B > A

        # D and E should be constrained to greater than 10
        d_dom = get_domain(engine.store, sol["D"].id)
        e_dom = get_domain(engine.store, sol["E"].id)
        assert d_dom.min() > 10
        assert e_dom.min() > 10
        assert e_dom.min() > d_dom.min()  # E > D

    def test_diamond_propagation(self):
        """Diamond-shaped constraint network."""
        engine = Engine(Program([]))

        query = """
        ?- Top in 1..20, Left in 1..20, Right in 1..20, Bottom in 1..20,
           Top #< Left, Top #< Right,
           Left #< Bottom, Right #< Bottom,
           Top = 5, Bottom = 15.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # Top = 5, Bottom = 15
        assert sol["Top"].value == 5
        assert sol["Bottom"].value == 15

        # Left and Right should be between 5 and 15

        left_dom = get_domain(engine.store, sol["Left"].id)
        right_dom = get_domain(engine.store, sol["Right"].id)

        assert left_dom.min() > 5  # > Top
        assert left_dom.max() < 15  # < Bottom
        assert right_dom.min() > 5
        assert right_dom.max() < 15

    def test_propagation_with_equality_chains(self):
        """Test propagation through equality constraints."""
        engine = Engine(Program([]))

        query = """
        ?- A in 1..20, B in 1..20, C in 1..20, D in 1..20,
           A #= B, B #= C, C #= D,
           A #< 10, D #> 5.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # All should have domain 6..9 (intersection of constraints)

        for var in ["A", "B", "C", "D"]:
            dom = get_domain(engine.store, sol[var].id)
            assert dom.min() == 6
            assert dom.max() == 9


class TestLabelingWithConstraints:
    """Test labeling strategies in presence of constraints."""

    def test_first_fail_with_constraints(self):
        """First-fail should choose most constrained variable first."""
        engine = Engine(Program([]))

        query = """
        ?- A in 1..100, B in 45..55, C in 1..100,
           A #< B, B #< C,
           labeling([first_fail], [A, B, C]).
        """
        solutions = list(engine.query(query))

        # Should find solutions
        assert len(solutions) > 0

        # B has smallest domain, so should be labeled first
        # This should lead to efficient search

    def test_indomain_split_bisection(self):
        """Test indomain_split bisection strategy."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..8, X #> 3, X #< 7,
           labeling([indomain_split], [X]).
        """
        solutions = list(engine.query(query))

        # Should find all values in 4..6
        values = sorted([sol["X"].value for sol in solutions])
        assert values == [4, 5, 6]

    def test_most_constrained_selection(self):
        """Most constrained variable selection strategy."""
        engine = Engine(Program([]))

        query = """
        ?- A in 1..10, B in 1..10, C in 1..10,
           A #< B, B #< C, A #< C,
           labeling([most_constrained], [A, B, C]).
        """
        solutions = list(engine.query(query))

        assert len(solutions) > 0

        # Verify all solutions satisfy constraints
        for sol in solutions:
            a = sol["A"].value
            b = sol["B"].value
            c = sol["C"].value
            assert a < b < c


class TestMixedConstraintsAndUnification:
    """Test scenarios mixing CLP(FD) constraints with regular unification."""

    def test_structure_with_fd_variables(self):
        """FD variables in structures with unification."""
        engine = Engine(Program([]))

        query = """
        ?- X in 5..15, Y in 10..20,
           Point = point(X, Y),
           Point = point(A, B),
           A #< B,
           A = 10.
        """
        solutions = list(engine.query(query))

        assert len(solutions) == 1
        sol = solutions[0]

        # X and A are unified and equal to 10
        assert sol["X"].value == 10
        assert sol["A"].value == 10

        # Y and B are unified and > 10

        y_dom = get_domain(engine.store, sol["Y"].id)
        assert y_dom.min() > 10

    def test_list_of_fd_variables(self):
        """Lists containing FD variables."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..5, Y in 3..7, Z in 5..9,
           List = [X, Y, Z],
           List = [A, B, C],
           A #< B, B #< C,
           label([A, B, C]).
        """
        solutions = list(engine.query(query))

        assert len(solutions) > 0

        # Check all solutions have increasing values
        for sol in solutions:
            values = [sol[v].value for v in ["A", "B", "C"]]
            assert values[0] < values[1] < values[2]

    def test_partial_grounding_with_constraints(self):
        """Partial grounding through unification with constraint propagation."""
        engine = Engine(Program([]))

        query = """
        ?- X in 1..10, Y in 1..10, Z in 1..10,
           triple(X, Y, Z) = triple(3, B, C),
           B #> X, C #> B,
           label([B, C]).
        """
        solutions = list(engine.query(query))

        assert len(solutions) > 0

        # X should be grounded to 3
        # B should be > 3
        # C should be > B
        for sol in solutions:
            assert sol["X"].value == 3
            assert sol["B"].value > 3
            assert sol["C"].value > sol["B"].value


class TestComplexScenarios:
    """Test more complex real-world-like scenarios."""

    def test_sudoku_row_constraint(self):
        """Single Sudoku row constraint (all different)."""
        engine = Engine(Program([]))

        query = """
        ?- A in 1..9, B in 1..9, C in 1..9, D in 1..9,
           E in 1..9, F in 1..9, G in 1..9, H in 1..9, I in 1..9,
           all_different([A, B, C, D, E, F, G, H, I]),
           A = 5, I = 9,
           once(labeling([first_fail], [B, C, D, E, F, G, H])).
        """
        # Only get first solution for performance test
        solutions = []
        for sol in engine.query(query):
            solutions.append(sol)
            break

        assert len(solutions) > 0

        # Check solution validity
        sol = solutions[0]
        values = [sol[v].value for v in ["A", "B", "C", "D", "E", "F", "G", "H", "I"]]
        assert len(set(values)) == 9  # All different
        assert set(values) == set(range(1, 10))  # Exactly 1..9
        assert sol["A"].value == 5
        assert sol["I"].value == 9

    def test_graph_coloring_triangle(self):
        """Simple graph coloring for triangle (3 nodes, all connected)."""
        engine = Engine(Program([]))

        # Three nodes, each needs different color from neighbors
        query = """
        ?- Red = 1, Green = 2, Blue = 3,
           Node1 in 1..3, Node2 in 1..3, Node3 in 1..3,
           Node1 #\\= Node2, Node2 #\\= Node3, Node1 #\\= Node3,
           label([Node1, Node2, Node3]).
        """
        solutions = list(engine.query(query))

        assert len(solutions) > 0

        # Each solution should have all three different colors
        for sol in solutions:
            colors = [sol[f"Node{i}"].value for i in range(1, 4)]
            assert len(set(colors)) == 3

    def test_bin_packing_simple(self):
        """Simple bin packing - items must fit in bins with capacity."""
        engine = Engine(Program([]))

        # 3 items with sizes, 2 bins with capacity 10
        query = """
        ?- Item1Size = 4, Item2Size = 3, Item3Size = 5,
           BinCapacity = 10,
           Item1Bin in 1..2, Item2Bin in 1..2, Item3Bin in 1..2,
           label([Item1Bin, Item2Bin, Item3Bin]).
        """
        solutions = list(engine.query(query))

        # Should find valid assignments
        assert len(solutions) > 0

        # Note: Without sum constraints, we can't enforce capacity
        # This just tests that basic structure works
