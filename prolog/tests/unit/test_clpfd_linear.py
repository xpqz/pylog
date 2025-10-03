"""Unit tests for CLP(FD) linear constraint propagator."""

from prolog.ast.terms import Var, Int
from prolog.engine.engine import Engine, Program
from prolog.clpfd.api import set_domain, get_domain
from prolog.clpfd.domain import Domain
from prolog.clpfd.props.linear import create_linear_propagator


class TestLinearPropagator:
    """Test linear constraint propagator."""

    def setup_method(self):
        """Set up test environment."""
        self.engine = Engine(Program(()))

    def test_simple_equality_x_eq_5(self):
        """Test X = 5 constraint."""

        # Create variable X in 1..10
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X = 5 (coeffs={x: 1}, const=-5, op='=')
        prop = create_linear_propagator({x_id: 1}, -5, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed == [x_id]

        # X should now be exactly 5
        dom = get_domain(self.engine.store, x_id)
        assert dom.min() == 5 and dom.max() == 5

    def test_simple_inequality_x_leq_5(self):
        """Test X =< 5 constraint."""

        # Create variable X in 1..10
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X =< 5 (coeffs={x: 1}, const=-5, op='=<')
        prop = create_linear_propagator({x_id: 1}, -5, "=<")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed == [x_id]

        # X should be pruned to 1..5
        dom = get_domain(self.engine.store, x_id)
        assert dom.min() == 1 and dom.max() == 5

    def test_two_variable_equality(self):
        """Test X + Y = 10 constraint."""

        # Create variables X, Y in 1..10
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y = 10
        prop = create_linear_propagator({x_id: 1, y_id: 1}, -10, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        # Both variables should be updated
        assert set(changed) == {x_id, y_id}

        # X should be pruned to 1..9 (since Y is at least 1)
        x_dom = get_domain(self.engine.store, x_id)
        assert x_dom.min() == 1 and x_dom.max() == 9

        # Y should be pruned to 1..9 (since X is at least 1)
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.min() == 1 and y_dom.max() == 9

    def test_weighted_constraint(self):
        """Test 2*X + 3*Y =< 20 constraint."""

        # Create variables X, Y in 1..10
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for 2*X + 3*Y =< 20
        prop = create_linear_propagator({x_id: 2, y_id: 3}, -20, "=<")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed  # At least one variable should be updated

        # X should be pruned (when Y=10, 2*X =< 20-30 = -10, impossible)
        # Maximum X when Y=1 is: 2*X =< 20-3 = 17, so X =< 8
        x_dom = get_domain(self.engine.store, x_id)
        assert x_dom.max() <= 8

        # Y should be pruned (when X=10, 3*Y =< 20-20 = 0, impossible)
        # Maximum Y when X=1 is: 3*Y =< 20-2 = 18, so Y =< 6
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.max() <= 6

    def test_negative_coefficients(self):
        """Test X - Y >= 3 constraint."""

        # Create variables X, Y in 1..10
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # X - Y >= 3 is equivalent to X - Y - 3 >= 0
        prop = create_linear_propagator({x_id: 1, y_id: -1}, -3, ">=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed

        # X must be at least 4 (when Y=1, X >= 3+1 = 4)
        x_dom = get_domain(self.engine.store, x_id)
        assert x_dom.min() >= 4

        # Y must be at most 7 (when X=10, Y <= 10-3 = 7)
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.max() <= 7

    def test_failure_case(self):
        """Test constraint that leads to failure."""

        # Create variable X in 1..5
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 5),)), self.engine.trail)

        # Create propagator for X >= 10 (impossible)
        prop = create_linear_propagator({x_id: 1}, -10, ">=")

        # Run propagator
        result, _ = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "fail"

    def test_already_satisfied(self):
        """Test constraint that is already satisfied."""

        # Create variable X in 5..5 (singleton)
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((5, 5),)), self.engine.trail)

        # Create propagator for X = 5 (already satisfied)
        prop = create_linear_propagator({x_id: 1}, -5, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed is None  # No changes needed

    def test_bound_variable_in_constraint(self):
        """Test constraint with a bound variable."""

        # Create variables X, Y
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()

        # Bind X to 5
        self.engine.unify(Var(x_id, "X"), Int(5))

        # Set domain for Y
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y = 8
        # Since X is bound to 5, this becomes 5 + Y = 8, so Y = 3
        prop = create_linear_propagator({x_id: 1, y_id: 1}, -8, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"

        # Y should be exactly 3
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.min() == 3 and y_dom.max() == 3

    def test_disequality_constraint(self):
        """Test X != 5 constraint."""

        # Create variable X in 4..6
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((4, 6),)), self.engine.trail)

        # Create propagator for X != 5
        prop = create_linear_propagator({x_id: 1}, -5, "!=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed == [x_id]

        # X should have 5 removed, leaving {4, 6}
        dom = get_domain(self.engine.store, x_id)
        assert not dom.contains(5)
        assert dom.contains(4)
        assert dom.contains(6)

    def test_multiple_variables_sum(self):
        """Test A + B + C + D = 10 constraint."""

        # Create variables A, B, C, D in 1..5
        a_id = self.engine.store.new_var()
        b_id = self.engine.store.new_var()
        c_id = self.engine.store.new_var()
        d_id = self.engine.store.new_var()

        for vid in [a_id, b_id, c_id, d_id]:
            set_domain(self.engine.store, vid, Domain(((1, 5),)), self.engine.trail)

        # Create propagator for A + B + C + D = 10
        prop = create_linear_propagator({a_id: 1, b_id: 1, c_id: 1, d_id: 1}, -10, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        # With domains 1..5 and target sum 10, no immediate pruning occurs
        # because each variable can still take any value in its domain
        # (other three could sum to make it work)
        # The constraint is set up but no domains change yet
        for vid in [a_id, b_id, c_id, d_id]:
            dom = get_domain(self.engine.store, vid)
            assert dom.min() >= 1 and dom.max() <= 5

    def test_zero_coefficient_ignored(self):
        """Test that zero coefficients are ignored."""

        # Create variable X in 1..10
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator with zero coefficient (0*X = 5 is just 0 = 5, which fails)
        prop = create_linear_propagator({x_id: 0}, -5, "=")

        # Run propagator
        result, _ = prop(self.engine.store, self.engine.trail, self.engine, None)

        # 0 = 5 is false, should fail
        assert result == "fail"

    def test_empty_constraint(self):
        """Test constraint with no variables."""

        # Create propagator for 0 = 0 (trivially true)
        prop = create_linear_propagator({}, 0, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed is None  # No variables to change

        # Create propagator for 0 = 5 (trivially false)
        prop = create_linear_propagator({}, -5, "=")

        # Run propagator
        result, _ = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "fail"

    def test_large_coefficients(self):
        """Test constraint with large coefficients."""

        # Create variables X, Y in appropriate ranges
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((0, 100),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((0, 100),)), self.engine.trail)

        # Create propagator for 1000*X + 100*Y = 5000
        prop = create_linear_propagator({x_id: 1000, y_id: 100}, -5000, "=")

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed

        # X must be at most 5 (since 1000*6 = 6000 > 5000)
        x_dom = get_domain(self.engine.store, x_id)
        assert x_dom.max() <= 5

        # When X=5, Y must be 0. When X=0, Y must be 50
        # So Y should be in 0..50
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.min() >= 0 and y_dom.max() <= 50

    def test_propagator_idempotence(self):
        """Test that running propagator twice doesn't change domains."""

        # Create variables
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y = 10
        prop = create_linear_propagator({x_id: 1, y_id: 1}, -10, "=")

        # Run propagator first time
        result1, changed1 = prop(
            self.engine.store, self.engine.trail, self.engine, None
        )
        assert result1 == "ok"
        assert changed1

        # Get domains after first run
        x_dom1 = get_domain(self.engine.store, x_id)
        y_dom1 = get_domain(self.engine.store, y_id)

        # Run propagator second time
        result2, changed2 = prop(
            self.engine.store, self.engine.trail, self.engine, None
        )
        assert result2 == "ok"
        assert changed2 is None  # No changes on second run

        # Domains should be unchanged
        x_dom2 = get_domain(self.engine.store, x_id)
        y_dom2 = get_domain(self.engine.store, y_id)
        assert x_dom1.min() == x_dom2.min() and x_dom1.max() == x_dom2.max()
        assert y_dom1.min() == y_dom2.min() and y_dom1.max() == y_dom2.max()
