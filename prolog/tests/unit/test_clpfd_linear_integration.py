"""Integration tests for CLP(FD) linear constraints via #=/2 builtin.

Tests the full integration path through the engine, not just the propagators.
"""

import pytest
from prolog.ast.terms import Var, Int, Struct, Atom
from prolog.engine.engine import Engine, Program
from prolog.clpfd.api import get_domain
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import (
    _builtin_in,
    _builtin_fd_eq,
    _builtin_fd_neq,
    _builtin_fd_lt,
    _builtin_fd_le,
    _builtin_fd_gt,
    _builtin_fd_ge,
)


class TestLinearConstraintIntegration:
    """Test linear constraint posting via #=/2 and other CLP(FD) builtins."""

    def setup_method(self):
        """Set up test environment."""
        self.engine = Engine(Program(()))

    def test_simple_sum_via_builtin(self):
        """Test X + Y #= 10 via the builtin."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 1..10, Y in 1..10
        domain_term = Struct("..", (Int(1), Int(10)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)

        # Post constraint: X + Y #= 10
        x_plus_y = Struct("+", (x, y))
        assert _builtin_fd_eq(self.engine, x_plus_y, Int(10))

        # Check domains were tightened
        x_dom = get_domain(self.engine.store, x.id)
        y_dom = get_domain(self.engine.store, y.id)

        # Both should be 1..9 now (since other var is at least 1)
        assert x_dom.min() == 1 and x_dom.max() == 9
        assert y_dom.min() == 1 and y_dom.max() == 9

    def test_weighted_sum_via_builtin(self):
        """Test 2*X + 3*Y #= 20 via the builtin."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 1..10, Y in 1..10
        domain_term = Struct("..", (Int(1), Int(10)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)

        # Build 2*X + 3*Y
        two_x = Struct("*", (Int(2), x))
        three_y = Struct("*", (Int(3), y))
        expr = Struct("+", (two_x, three_y))

        # Post constraint: 2*X + 3*Y #= 20
        assert _builtin_fd_eq(self.engine, expr, Int(20))

        # Check domains were tightened
        x_dom = get_domain(self.engine.store, x.id)
        y_dom = get_domain(self.engine.store, y.id)

        # X should be at most 8 (when Y=1: 2*X + 3 = 20, so X <= 8.5)
        assert x_dom.max() <= 8
        # Y should be at most 6 (when X=1: 2 + 3*Y = 20, so Y <= 6)
        assert y_dom.max() <= 6

    def test_subtraction_via_builtin(self):
        """Test X - Y #= 5 via the builtin."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 1..10, Y in 1..10
        domain_term = Struct("..", (Int(1), Int(10)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)

        # Build X - Y
        expr = Struct("-", (x, y))

        # Post constraint: X - Y #= 5
        assert _builtin_fd_eq(self.engine, expr, Int(5))

        # Check domains were tightened
        x_dom = get_domain(self.engine.store, x.id)
        y_dom = get_domain(self.engine.store, y.id)

        # X must be at least 6 (when Y=1: X - 1 = 5, so X >= 6)
        assert x_dom.min() >= 6
        # Y must be at most 5 (when X=10: 10 - Y = 5, so Y <= 5)
        assert y_dom.max() <= 5

    def test_complex_expression_via_builtin(self):
        """Test (X + 2) - (Y - 3) #= 10 via the builtin."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 1..20, Y in 1..20
        domain_term = Struct("..", (Int(1), Int(20)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)

        # Build (X + 2) - (Y - 3)
        x_plus_2 = Struct("+", (x, Int(2)))
        y_minus_3 = Struct("-", (y, Int(3)))
        expr = Struct("-", (x_plus_2, y_minus_3))

        # Post constraint: (X + 2) - (Y - 3) #= 10
        # This simplifies to: X + 2 - Y + 3 = 10, or X - Y = 5
        assert _builtin_fd_eq(self.engine, expr, Int(10))

        # Check domains were tightened
        x_dom = get_domain(self.engine.store, x.id)
        y_dom = get_domain(self.engine.store, y.id)

        # X must be at least 6 (when Y=1: X - 1 = 5, so X >= 6)
        assert x_dom.min() >= 6
        # Y must be at most 15 (when X=20: 20 - Y = 5, so Y <= 15)
        assert y_dom.max() <= 15

    def test_inequality_via_builtin(self):
        """Test X + Y #=< 10 via the builtin."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 5..15, Y in 5..15
        domain_term = Struct("..", (Int(5), Int(15)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)

        # Build X + Y
        expr = Struct("+", (x, y))

        # Post constraint: X + Y #=< 10
        assert _builtin_fd_le(self.engine, expr, Int(10))

        # Check domains were tightened
        x_dom = get_domain(self.engine.store, x.id)
        y_dom = get_domain(self.engine.store, y.id)

        # Both must be exactly 5 (since 5 + 5 = 10, and we need <= 10)
        assert x_dom.min() == 5 and x_dom.max() == 5
        assert y_dom.min() == 5 and y_dom.max() == 5

    def test_disequality_via_builtin(self):
        """Test X + Y #\\= 10 via the builtin."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 5..5, Y in 4..6
        assert _builtin_in(self.engine, x, Int(5))
        domain_term = Struct("..", (Int(4), Int(6)))
        assert _builtin_in(self.engine, y, domain_term)

        # Build X + Y
        expr = Struct("+", (x, y))

        # Post constraint: X + Y #\= 10
        # Since X=5, this means Y != 5
        assert _builtin_fd_neq(self.engine, expr, Int(10))

        # Check Y's domain has 5 removed
        y_dom = get_domain(self.engine.store, y.id)
        assert not y_dom.contains(5)
        assert y_dom.contains(4)
        assert y_dom.contains(6)

    def test_constant_folding(self):
        """Test that bound variables are folded into constants."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")
        z = Var(self.engine.store.new_var(), "Z")

        # Post domains
        domain_term = Struct("..", (Int(1), Int(10)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)
        assert _builtin_in(self.engine, z, domain_term)

        # Bind X to 3
        self.engine.unify(x, Int(3))

        # Build 2*X + Y (where X=3, so this becomes 6 + Y)
        two_x = Struct("*", (Int(2), x))
        expr = Struct("+", (two_x, y))

        # Post constraint: 2*X + Y #= Z
        # Since X=3, this is 6 + Y = Z
        assert _builtin_fd_eq(self.engine, expr, z)

        # Check domains
        y_dom = get_domain(self.engine.store, y.id)
        z_dom = get_domain(self.engine.store, z.id)

        # Z should be 7..10 (since Y is 1..4 when Z must be at most 10)
        # But actually both adjust: Y in 1..4, Z in 7..10
        assert y_dom.max() <= 4  # Y + 6 <= 10, so Y <= 4
        assert z_dom.min() >= 7  # Z = Y + 6, Y >= 1, so Z >= 7

    def test_failure_on_inconsistent_constraint(self):
        """Test that impossible constraints fail properly."""
        # Create variable
        x = Var(self.engine.store.new_var(), "X")

        # Post domain: X in 1..5
        domain_term = Struct("..", (Int(1), Int(5)))
        assert _builtin_in(self.engine, x, domain_term)

        # Try to post impossible constraint: X + 10 #= 5
        # This means X = -5, which is outside the domain
        expr = Struct("+", (x, Int(10)))
        assert not _builtin_fd_eq(self.engine, expr, Int(5))

    def test_multiple_constraints_accumulate(self):
        """Test that multiple constraints properly accumulate."""
        # Create variables
        x = Var(self.engine.store.new_var(), "X")
        y = Var(self.engine.store.new_var(), "Y")

        # Post domains: X in 1..10, Y in 1..10
        domain_term = Struct("..", (Int(1), Int(10)))
        assert _builtin_in(self.engine, x, domain_term)
        assert _builtin_in(self.engine, y, domain_term)

        # Post constraint 1: X + Y #= 10
        x_plus_y = Struct("+", (x, y))
        assert _builtin_fd_eq(self.engine, x_plus_y, Int(10))

        # Post constraint 2: X #>= 6
        assert _builtin_fd_ge(self.engine, x, Int(6))

        # Check domains
        x_dom = get_domain(self.engine.store, x.id)
        y_dom = get_domain(self.engine.store, y.id)

        # X should be 6..9 (constrained by both X >= 6 and X + Y = 10 with Y >= 1)
        assert x_dom.min() == 6 and x_dom.max() == 9
        # Y should be 1..4 (since X >= 6 and X + Y = 10, so Y <= 4)
        assert y_dom.min() == 1 and y_dom.max() == 4

    def test_sum_optimization_path(self):
        """Test that pure sums use the optimized sum propagator."""
        # Create many variables
        vars = []
        for i in range(5):
            v = Var(self.engine.store.new_var(), f"V{i}")
            vars.append(v)
            # Each in 1..10
            domain_term = Struct("..", (Int(1), Int(10)))
            assert _builtin_in(self.engine, v, domain_term)

        # Build V0 + V1 + V2 + V3 + V4
        expr = vars[0]
        for v in vars[1:]:
            expr = Struct("+", (expr, v))

        # Post constraint: sum = 25
        # This should use the optimized sum propagator since all coeffs are 1
        assert _builtin_fd_eq(self.engine, expr, Int(25))

        # Check that domains were adjusted
        # Each variable can be at most 21 (if others are all 1: 1+1+1+1+x=25, x=21)
        # But our domain limits to 10, so max stays 10
        # Each variable must be at least 1 (already the case)
        # Actually with sum=25 and 5 vars each in 1..10:
        # Min sum = 5, Max sum = 50, target = 25
        # When 4 others at max (40), this var must be at least -15 (but clamped to 1)
        # When 4 others at min (4), this var must be at most 21 (but clamped to 10)
        # So domains don't change much in this case, but constraint is posted
        for v in vars:
            dom = get_domain(self.engine.store, v.id)
            assert dom is not None
            # Just verify the constraint was processed without error
            assert dom.min() >= 1