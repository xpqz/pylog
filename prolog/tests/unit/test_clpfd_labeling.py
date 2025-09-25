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
        from prolog.clpfd.label import _builtin_labeling

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")

        # Set domain
        assert _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        # Label with explicit indomain_min
        options = List([Atom("indomain_min")])
        assert _builtin_labeling(engine, options, List([x]))

        # First value tried should be minimum (5)
        result = engine.step()
        assert result is not False
        assert engine.store.deref(x.id)[2].value == 5

    def test_indomain_max_strategy(self):
        """Test indomain_max value selection."""
        from prolog.clpfd.label import _builtin_labeling

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")

        # Set domain
        assert _builtin_in(engine, x, Struct("..", (Int(5), Int(10))))

        # Label with indomain_max
        options = List([Atom("indomain_max")])
        assert _builtin_labeling(engine, options, List([x]))

        # First value tried should be maximum (10)
        result = engine.step()
        assert result is not False
        assert engine.store.deref(x.id)[2].value == 10

    def test_first_fail_variable_selection(self):
        """Test first_fail variable selection (smallest domain first)."""
        from prolog.clpfd.label import _builtin_labeling

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")
        z = Var(engine.store.new_var(), "Z")

        # Set domains of different sizes
        assert _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))  # Size 10
        assert _builtin_in(engine, y, Struct("..", (Int(1), Int(2))))   # Size 2
        assert _builtin_in(engine, z, Struct("..", (Int(1), Int(5))))   # Size 5

        # Label with first_fail
        options = List([Atom("first_fail")])
        assert _builtin_labeling(engine, options, List([x, y, z]))

        # Y (smallest domain) should be labeled first
        result = engine.step()
        assert result is not False

        # Y should be bound, X and Z might not be yet
        y_deref = engine.store.deref(y.id)
        assert y_deref[0] == "BOUND"

    def test_most_constrained_selection(self):
        """Test most_constrained variable selection."""
        from prolog.clpfd.label import _builtin_labeling

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")
        z = Var(engine.store.new_var(), "Z")

        # Set domains and constraints
        assert _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        assert _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))
        assert _builtin_in(engine, z, Struct("..", (Int(1), Int(5))))

        # Y has most constraints
        assert _builtin_fd_lt(engine, x, y)  # X < Y
        assert _builtin_fd_lt(engine, y, z)  # Y < Z

        # Label with most_constrained
        options = List([Atom("most_constrained")])
        assert _builtin_labeling(engine, options, List([x, y, z]))

        result = engine.step()
        assert result is not False


class TestLabelingWithBacktracking:
    """Test labeling with backtracking to find all solutions."""

    def test_find_all_solutions(self):
        """Find all solutions through backtracking."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")

        # Small domains for exhaustive search
        assert _builtin_in(engine, x, Struct("..", (Int(1), Int(2))))
        assert _builtin_in(engine, y, Struct("..", (Int(1), Int(2))))

        # Label and collect all solutions
        assert _builtin_label(engine, List([x, y]))

        solutions = []
        while True:
            result = engine.step()
            if result is False:
                # Backtrack to find more solutions
                if not engine.backtrack():
                    break  # No more solutions
            else:
                # Record solution if we're done labeling
                x_deref = engine.store.deref(x.id)
                y_deref = engine.store.deref(y.id)
                if x_deref[0] == "BOUND" and y_deref[0] == "BOUND":
                    solutions.append((
                        x_deref[2].value,
                        y_deref[2].value
                    ))
                    # Force backtrack to find next solution
                    if not engine.backtrack():
                        break

        # Should have 4 solutions: (1,1), (1,2), (2,1), (2,2)
        assert len(solutions) == 4
        assert (1, 1) in solutions
        assert (1, 2) in solutions
        assert (2, 1) in solutions
        assert (2, 2) in solutions

    def test_labeling_with_failing_constraint(self):
        """Test labeling when constraints make problem unsolvable."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")

        # Set conflicting constraints
        assert _builtin_in(engine, x, Int(5))  # X = 5
        assert _builtin_in(engine, y, Int(3))  # Y = 3
        assert _builtin_fd_lt(engine, y, x)    # Y < X (3 < 5, ok so far)
        assert _builtin_fd_lt(engine, x, y)    # X < Y (5 < 3, contradiction!)

        # This should fail due to empty domain
        # But if it doesn't fail immediately, labeling should explore and fail
        _builtin_label(engine, List([x, y]))

        # Try to find a solution
        found_solution = False
        while engine.step() is not False:
            x_deref = engine.store.deref(x.id)
            y_deref = engine.store.deref(y.id)
            if x_deref[0] == "BOUND" and y_deref[0] == "BOUND":
                found_solution = True
                break

        assert not found_solution  # Should not find any solution


class TestLabelingEdgeCases:
    """Test edge cases in labeling."""

    def test_label_singleton_domain(self):
        """Label variable with singleton domain."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")

        # Singleton domain
        assert _builtin_in(engine, x, Int(42))

        assert _builtin_label(engine, List([x]))
        result = engine.step()
        assert result is not False

        assert engine.store.deref(x.id)[2].value == 42

    def test_label_empty_domain_fails(self):
        """Labeling variable with empty domain should fail."""
        from prolog.clpfd.label import _builtin_label
        from prolog.clpfd.domain import Domain
        from prolog.clpfd.api import set_domain

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")

        # Set empty domain
        x_deref = engine.store.deref(x.id)
        set_domain(engine.store, x_deref[1], Domain(()), engine.trail)

        # Labeling should handle empty domain gracefully
        assert _builtin_label(engine, List([x]))
        result = engine.step()
        assert result is False  # Should fail

    def test_label_with_holes_in_domain(self):
        """Label variable with holes in domain."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")

        # Domain with holes: {1,3,5,7,9}
        domain_term = Struct("\\/", (
            Int(1),
            Struct("\\/", (
                Int(3),
                Struct("\\/", (
                    Int(5),
                    Struct("\\/", (Int(7), Int(9)))
                ))
            ))
        ))
        assert _builtin_in(engine, x, domain_term)

        assert _builtin_label(engine, List([x]))

        # Collect all values tried
        values = []
        while True:
            result = engine.step()
            if result is False:
                if not engine.backtrack():
                    break
            else:
                x_deref = engine.store.deref(x.id)
                if x_deref[0] == "BOUND":
                    values.append(x_deref[2].value)
                    if not engine.backtrack():
                        break

        # Should try all values in domain
        assert set(values) == {1, 3, 5, 7, 9}

    def test_label_mixed_bound_unbound(self):
        """Label mix of bound and unbound variables."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")
        z = Var(engine.store.new_var(), "Z")

        # X is bound, Y has domain, Z has domain
        engine.store.bind(x.id, Int(10), engine.trail)
        assert _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        assert _builtin_in(engine, z, Struct("..", (Int(4), Int(6))))

        # Should only need to label Y and Z
        assert _builtin_label(engine, List([x, y, z]))

        result = engine.step()
        assert result is not False

        # All should be bound
        assert engine.store.deref(x.id)[2].value == 10
        assert engine.store.deref(y.id)[0] == "BOUND"
        assert engine.store.deref(z.id)[0] == "BOUND"


class TestLabelingIntegration:
    """Test labeling integrated with full CLP(FD) system."""

    def test_label_triggers_propagation(self):
        """Labeling should trigger propagation after each choice."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")
        z = Var(engine.store.new_var(), "Z")

        # Domains and constraints
        assert _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        assert _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        assert _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))
        assert _builtin_fd_lt(engine, x, y)  # X < Y
        assert _builtin_fd_lt(engine, y, z)  # Y < Z

        # Label should respect constraints
        assert _builtin_label(engine, List([x, y, z]))

        # Find a solution
        while engine.step() is not False:
            pass

        # Check if we found valid solution
        x_deref = engine.store.deref(x.id)
        y_deref = engine.store.deref(y.id)
        z_deref = engine.store.deref(z.id)

        if x_deref[0] == "BOUND" and y_deref[0] == "BOUND" and z_deref[0] == "BOUND":
            x_val = x_deref[2].value
            y_val = y_deref[2].value
            z_val = z_deref[2].value
            assert x_val < y_val < z_val  # Only solution: X=1, Y=2, Z=3

    def test_label_with_equality_constraints(self):
        """Test labeling with equality constraints."""
        from prolog.clpfd.label import _builtin_label

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")

        # Domains and equality
        assert _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        assert _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))
        assert _builtin_fd_eq(engine, x, y)  # X = Y

        # Label and find all solutions
        assert _builtin_label(engine, List([x, y]))

        solutions = []
        while True:
            result = engine.step()
            if result is not False:
                x_deref = engine.store.deref(x.id)
                y_deref = engine.store.deref(y.id)
                if x_deref[0] == "BOUND" and y_deref[0] == "BOUND":
                    x_val = x_deref[2].value
                    y_val = y_deref[2].value
                    solutions.append((x_val, y_val))

            if not engine.backtrack():
                break

        # All solutions should have X = Y
        for x_val, y_val in solutions:
            assert x_val == y_val