"""Unit tests for CLP(FD) arithmetic expression parser."""

import pytest
from prolog.ast.terms import Var, Int, Struct, Atom
from prolog.engine.engine import Engine, Program


class TestExpressionParser:
    """Test expression parsing for linear constraints."""

    def setup_method(self):
        """Set up test environment."""
        self.engine = Engine(Program(()))

    def test_parse_single_variable(self):
        """Test parsing a single variable."""
        from prolog.clpfd.expr import parse_linear_expression

        # Create variable X
        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # Parse X => {x_id: 1}, 0
        coeffs, const = parse_linear_expression(x, self.engine)
        assert coeffs == {x_id: 1}
        assert const == 0

    def test_parse_integer_constant(self):
        """Test parsing an integer constant."""
        from prolog.clpfd.expr import parse_linear_expression

        # Parse 42 => {}, 42
        coeffs, const = parse_linear_expression(Int(42), self.engine)
        assert coeffs == {}
        assert const == 42

    def test_parse_simple_addition(self):
        """Test parsing X + Y."""
        from prolog.clpfd.expr import parse_linear_expression

        # Create variables
        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # X + Y
        expr = Struct("+", (x, y))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 1, y_id: 1}
        assert const == 0

    def test_parse_variable_plus_constant(self):
        """Test parsing X + 5."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # X + 5
        expr = Struct("+", (x, Int(5)))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 1}
        assert const == 5

    def test_parse_subtraction(self):
        """Test parsing X - Y."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # X - Y
        expr = Struct("-", (x, y))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 1, y_id: -1}
        assert const == 0

    def test_parse_multiplication_constant_var(self):
        """Test parsing 3 * X."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # 3 * X
        expr = Struct("*", (Int(3), x))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 3}
        assert const == 0

    def test_parse_multiplication_var_constant(self):
        """Test parsing X * 3."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # X * 3
        expr = Struct("*", (x, Int(3)))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 3}
        assert const == 0

    def test_parse_complex_expression(self):
        """Test parsing 2*X + 3*Y - 5."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # 2*X + 3*Y - 5
        # Structure: (2*X + 3*Y) - 5
        two_x = Struct("*", (Int(2), x))
        three_y = Struct("*", (Int(3), y))
        sum_xy = Struct("+", (two_x, three_y))
        expr = Struct("-", (sum_xy, Int(5)))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 2, y_id: 3}
        assert const == -5

    def test_parse_sendmore_style(self):
        """Test parsing SEND+MORE style expression: 1000*S + 100*E + 10*N + D."""
        from prolog.clpfd.expr import parse_linear_expression

        # Create variables
        s_id = self.engine.store.new_var()
        e_id = self.engine.store.new_var()
        n_id = self.engine.store.new_var()
        d_id = self.engine.store.new_var()

        s = Var(s_id, "S")
        e = Var(e_id, "E")
        n = Var(n_id, "N")
        d = Var(d_id, "D")

        # Build 1000*S + 100*E + 10*N + D
        thousand_s = Struct("*", (Int(1000), s))
        hundred_e = Struct("*", (Int(100), e))
        ten_n = Struct("*", (Int(10), n))

        # ((1000*S + 100*E) + 10*N) + D
        sum1 = Struct("+", (thousand_s, hundred_e))
        sum2 = Struct("+", (sum1, ten_n))
        expr = Struct("+", (sum2, d))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {s_id: 1000, e_id: 100, n_id: 10, d_id: 1}
        assert const == 0

    def test_parse_unary_minus(self):
        """Test parsing -X."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # -X
        expr = Struct("-", (x,))  # Unary minus has single argument
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: -1}
        assert const == 0

    def test_parse_bound_variable(self):
        """Test parsing expressions with bound variables."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # Bind X to 10
        self.engine.unify(x, Int(10))

        # Parse X + 5 (should become constant 15)
        expr = Struct("+", (x, Int(5)))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {}  # No variables
        assert const == 15

    def test_parse_mixed_bound_unbound(self):
        """Test parsing with mix of bound and unbound variables."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # Bind X to 3
        self.engine.unify(x, Int(3))

        # Parse 2*X + Y (should become Y + 6)
        two_x = Struct("*", (Int(2), x))
        expr = Struct("+", (two_x, y))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {y_id: 1}
        assert const == 6

    def test_parse_constant_multiplication(self):
        """Test parsing 3 * 4."""
        from prolog.clpfd.expr import parse_linear_expression

        # 3 * 4
        expr = Struct("*", (Int(3), Int(4)))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {}
        assert const == 12

    def test_parse_nested_parentheses(self):
        """Test parsing (X + 2) - (Y - 3)."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # (X + 2) - (Y - 3) = X + 2 - Y + 3 = X - Y + 5
        x_plus_2 = Struct("+", (x, Int(2)))
        y_minus_3 = Struct("-", (y, Int(3)))
        expr = Struct("-", (x_plus_2, y_minus_3))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 1, y_id: -1}
        assert const == 5

    def test_parse_accumulate_same_variable(self):
        """Test parsing X + X + X."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # X + X + X = 3*X
        sum1 = Struct("+", (x, x))
        expr = Struct("+", (sum1, x))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 3}
        assert const == 0

    def test_parse_error_nonlinear(self):
        """Test that non-linear terms raise ValueError."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # X * Y is non-linear
        expr = Struct("*", (x, y))

        with pytest.raises(ValueError, match="Non-linear term"):
            parse_linear_expression(expr, self.engine)

    def test_parse_error_division(self):
        """Test that division raises ValueError."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # X / 2 is not supported
        expr = Struct("/", (x, Int(2)))

        with pytest.raises(ValueError, match="Unsupported arithmetic operator"):
            parse_linear_expression(expr, self.engine)

    def test_parse_error_non_integer(self):
        """Test that non-integer terms raise ValueError."""
        from prolog.clpfd.expr import parse_linear_expression

        # Atom is not a valid arithmetic term
        with pytest.raises(ValueError, match="Invalid arithmetic term"):
            parse_linear_expression(Atom("foo"), self.engine)

    def test_parse_negative_coefficients(self):
        """Test parsing -3*X + 2*Y."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        y_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        y = Var(y_id, "Y")

        # -3*X + 2*Y
        neg_three_x = Struct("-", (Struct("*", (Int(3), x)),))  # Unary minus
        two_y = Struct("*", (Int(2), y))
        expr = Struct("+", (neg_three_x, two_y))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: -3, y_id: 2}
        assert const == 0

    def test_parse_constant_minus_variable(self):
        """Test parsing 5 - X."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # 5 - X
        expr = Struct("-", (Int(5), x))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: -1}
        assert const == 5

    def test_parse_zero_coefficient(self):
        """Test parsing 0 * X."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # 0 * X => should eliminate variable
        expr = Struct("*", (Int(0), x))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {}  # No variables (coefficient is 0)
        assert const == 0

    def test_parse_negative_literal_times_var(self):
        """Test parsing (-3) * X with negative integer literal."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # (-3) * X where -3 is an Int(-3) literal
        expr = Struct("*", (Int(-3), x))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: -3}
        assert const == 0

    def test_parse_canceling_terms(self):
        """Test parsing X + 2*X - 3*X."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")

        # X + 2*X - 3*X = 0
        # Build: (X + 2*X) - 3*X
        two_x = Struct("*", (Int(2), x))
        three_x = Struct("*", (Int(3), x))
        x_plus_2x = Struct("+", (x, two_x))
        expr = Struct("-", (x_plus_2x, three_x))

        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {}  # Coefficients cancel to 0
        assert const == 0

    def test_parse_same_root_variables(self):
        """Test parsing with multiple variables referring to same root."""
        from prolog.clpfd.expr import parse_linear_expression

        x_id = self.engine.store.new_var()
        x = Var(x_id, "X")
        z = Var(x_id, "Z")  # Same ID, different hint

        # X + Z where both refer to same variable
        expr = Struct("+", (x, z))
        coeffs, const = parse_linear_expression(expr, self.engine)
        assert coeffs == {x_id: 2}
        assert const == 0