"""Unit tests for CLP(FD) sum constraint propagator."""

from prolog.ast.terms import Var, Int
from prolog.engine.engine import Engine, Program
from prolog.clpfd.api import set_domain, get_domain
from prolog.clpfd.domain import Domain
from prolog.clpfd.props.sum import create_sum_propagator


class TestSumPropagator:
    """Test sum constraint propagator."""

    def setup_method(self):
        """Set up test environment."""
        self.engine = Engine(Program(()))

    def test_simple_sum_two_vars(self):
        """Test X + Y = 10."""

        # Create variables X, Y in 1..10
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y = 10
        prop = create_sum_propagator([x_id, y_id], 10)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert set(changed) == {x_id, y_id}

        # X should be 1..9 (since Y is at least 1)
        x_dom = get_domain(self.engine.store, x_id)
        assert x_dom.min() == 1 and x_dom.max() == 9

        # Y should be 1..9 (since X is at least 1)
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.min() == 1 and y_dom.max() == 9

    def test_sum_with_singleton(self):
        """Test sum with one variable fixed."""

        # Create variables
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(
            self.engine.store, x_id, Domain(((5, 5),)), self.engine.trail
        )  # X = 5
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y = 8
        prop = create_sum_propagator([x_id, y_id], 8)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"

        # Y should be exactly 3
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.min() == 3 and y_dom.max() == 3

    def test_sum_three_variables(self):
        """Test X + Y + Z = 15."""

        # Create variables in 1..10
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        z_id = self.engine.store.new_var()

        for vid in [x_id, y_id, z_id]:
            set_domain(self.engine.store, vid, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y + Z = 15
        prop = create_sum_propagator([x_id, y_id, z_id], 15)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        # With these domains, pruning might not occur immediately

        # Each variable should still be valid
        # Min for X: others at max sum to 20, so X >= 15-20 = -5, clamped to 1
        # Max for X: others at min sum to 2, so X <= 15-2 = 13, clamped to 10
        # So domains don't actually change much here
        for vid in [x_id, y_id, z_id]:
            dom = get_domain(self.engine.store, vid)
            # Min stays 1 (can't go lower)
            assert dom.min() >= 1
            # Max stays 10 (sufficient for sum=15 with others>=1)
            assert dom.max() <= 10

    def test_sum_failure(self):
        """Test impossible sum constraint."""

        # Create variables in 1..3
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 3),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 3),)), self.engine.trail)

        # Create propagator for X + Y = 10 (impossible)
        prop = create_sum_propagator([x_id, y_id], 10)

        # Run propagator
        result, _ = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "fail"

    def test_sum_single_variable(self):
        """Test sum with single variable."""

        # Create variable X in 1..10
        x_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X = 5
        prop = create_sum_propagator([x_id], 5)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed == [x_id]

        # X should be exactly 5
        x_dom = get_domain(self.engine.store, x_id)
        assert x_dom.min() == 5 and x_dom.max() == 5

    def test_sum_empty_list(self):
        """Test sum with no variables."""

        # Create propagator for 0 = 0 (trivially true)
        prop = create_sum_propagator([], 0)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed is None

        # Create propagator for 0 = 5 (trivially false)
        prop = create_sum_propagator([], 5)

        # Run propagator
        result, _ = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "fail"

    def test_sum_with_bound_variables(self):
        """Test sum with some variables already bound."""

        # Create variables
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        z_id = self.engine.store.new_var()

        # Bind X to 3
        self.engine.unify(Var(x_id, "X"), Int(3))

        # Set domains for Y and Z
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, z_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y + Z = 12
        # Since X=3, this becomes 3 + Y + Z = 12, so Y + Z = 9
        prop = create_sum_propagator([x_id, y_id, z_id], 12)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"

        # Y should be 1..8 (since Z is at least 1, Y + Z = 9)
        y_dom = get_domain(self.engine.store, y_id)
        assert y_dom.max() == 8

        # Z should be 1..8 (since Y is at least 1)
        z_dom = get_domain(self.engine.store, z_id)
        assert z_dom.max() == 8

    def test_sum_large_number_of_variables(self):
        """Test sum with many variables."""

        # Create 10 variables in 1..5
        var_ids = []
        for _ in range(10):
            vid = self.engine.store.new_var()
            set_domain(self.engine.store, vid, Domain(((1, 5),)), self.engine.trail)
            var_ids.append(vid)

        # Create propagator for sum = 30
        # Min sum = 10 (all at 1), Max sum = 50 (all at 5)
        # 30 is achievable
        prop = create_sum_propagator(var_ids, 30)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        # Domains should be pruned

    def test_sum_idempotence(self):
        """Test that running sum propagator twice doesn't change domains."""

        # Create variables
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((1, 10),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((1, 10),)), self.engine.trail)

        # Create propagator for X + Y = 10
        prop = create_sum_propagator([x_id, y_id], 10)

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

    def test_sum_with_negative_target(self):
        """Test sum with negative target value."""

        # Create variables in -5..5
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        set_domain(self.engine.store, x_id, Domain(((-5, 5),)), self.engine.trail)
        set_domain(self.engine.store, y_id, Domain(((-5, 5),)), self.engine.trail)

        # Create propagator for X + Y = -3
        prop = create_sum_propagator([x_id, y_id], -3)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        # Both should be pruned since sum must be negative

    def test_sum_tight_constraint(self):
        """Test sum where constraint forces exact values."""

        # Create 3 variables in 1..2
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        z_id = self.engine.store.new_var()

        for vid in [x_id, y_id, z_id]:
            set_domain(self.engine.store, vid, Domain(((1, 2),)), self.engine.trail)

        # Create propagator for X + Y + Z = 6
        # This forces all variables to be 2
        prop = create_sum_propagator([x_id, y_id, z_id], 6)

        # Run propagator
        result, changed = prop(self.engine.store, self.engine.trail, self.engine, None)

        assert result == "ok"
        assert changed

        # All variables should be exactly 2
        for vid in [x_id, y_id, z_id]:
            dom = get_domain(self.engine.store, vid)
            assert dom.min() == 2 and dom.max() == 2
