"""
Tests for arithmetic operators in is/2 evaluation.
Covers //, mod, and unary minus operators, plus error cases.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestIntegerDivision:
    """Tests for // (integer division) operator."""

    def test_integer_division_positive(self, empty_engine):
        """Test positive integer division."""
        X = Var(0, "X")
        # X is 7 // 3 (should be 2 - ISO/SWI semantics)
        query = Struct("is", (X, Struct("//", (Int(7), Int(3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(2)}]

    def test_integer_division_negative_dividend(self, empty_engine):
        """Test integer division with negative dividend."""
        X = Var(0, "X")
        # X is -7 // 3 (should be -3 - ISO/SWI floored division)
        query = Struct("is", (X, Struct("//", (Int(-7), Int(3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-3)}]

    def test_integer_division_negative_divisor(self, empty_engine):
        """Test integer division with negative divisor."""
        X = Var(0, "X")
        # X is 7 // -3 (should be -3 - ISO/SWI floored division)
        query = Struct("is", (X, Struct("//", (Int(7), Int(-3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-3)}]

    def test_integer_division_both_negative(self, empty_engine):
        """Test integer division with both negative."""
        X = Var(0, "X")
        # X is -7 // -3 (should be 2)
        query = Struct("is", (X, Struct("//", (Int(-7), Int(-3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(2)}]

    def test_integer_division_exact(self, empty_engine):
        """Test integer division that divides exactly."""
        X = Var(0, "X")
        # X is 12 // 4 (should be 3)
        query = Struct("is", (X, Struct("//", (Int(12), Int(4)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(3)}]

    def test_integer_division_by_one(self, empty_engine):
        """Test integer division by 1."""
        X = Var(0, "X")
        # X is 42 // 1 (should be 42)
        query = Struct("is", (X, Struct("//", (Int(42), Int(1)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(42)}]

    def test_integer_division_by_zero_fails(self, empty_engine):
        """Test integer division by zero fails."""
        X = Var(0, "X")
        # X is 7 // 0 (should fail with error)
        query = Struct("is", (X, Struct("//", (Int(7), Int(0)))))
        solutions = empty_engine.run([query])
        assert solutions == []

    def test_integer_division_zero_dividend(self, empty_engine):
        """Test 0 // n = 0."""
        X = Var(0, "X")
        # X is 0 // 5 (should be 0)
        query = Struct("is", (X, Struct("//", (Int(0), Int(5)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(0)}]

    @pytest.mark.parametrize("n,d,expected", [
        (7, 3, 2),
        (-7, 3, -3),
        (7, -3, -3),
        (-7, -3, 2),
        (0, 5, 0),
        (12, 4, 3),
    ])
    def test_integer_division_parametrized(self, empty_engine, n, d, expected):
        """Test integer division with various inputs."""
        X = Var(0, "X")
        query = Struct("is", (X, Struct("//", (Int(n), Int(d)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(expected)}]


class TestModuloOperator:
    """Tests for mod operator."""

    def test_modulo_positive(self, empty_engine):
        """Test positive modulo."""
        X = Var(0, "X")
        # X is 7 mod 3 (should be 1)
        query = Struct("is", (X, Struct("mod", (Int(7), Int(3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(1)}]

    def test_modulo_exact_division(self, empty_engine):
        """Test modulo when division is exact."""
        X = Var(0, "X")
        # X is 12 mod 4 (should be 0)
        query = Struct("is", (X, Struct("mod", (Int(12), Int(4)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(0)}]

    def test_modulo_negative_dividend(self, empty_engine):
        """Test modulo with negative dividend."""
        X = Var(0, "X")
        # X is -7 mod 3 (ISO/SWI gives 2)
        query = Struct("is", (X, Struct("mod", (Int(-7), Int(3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(2)}]

    def test_modulo_negative_divisor(self, empty_engine):
        """Test modulo with negative divisor."""
        X = Var(0, "X")
        # X is 7 mod -3 (ISO/SWI gives -2)
        query = Struct("is", (X, Struct("mod", (Int(7), Int(-3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-2)}]

    def test_modulo_both_negative(self, empty_engine):
        """Test modulo with both negative."""
        X = Var(0, "X")
        # X is -7 mod -3 (ISO/SWI gives -1)
        query = Struct("is", (X, Struct("mod", (Int(-7), Int(-3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-1)}]

    def test_modulo_by_one(self, empty_engine):
        """Test modulo by 1 is always 0."""
        X = Var(0, "X")
        # X is 42 mod 1 (should be 0)
        query = Struct("is", (X, Struct("mod", (Int(42), Int(1)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(0)}]

    def test_modulo_by_zero_fails(self, empty_engine):
        """Test modulo by zero fails."""
        X = Var(0, "X")
        # X is 7 mod 0 (should fail with error)
        query = Struct("is", (X, Struct("mod", (Int(7), Int(0)))))
        solutions = empty_engine.run([query])
        assert solutions == []

    def test_modulo_zero_dividend(self, empty_engine):
        """Test 0 mod n = 0."""
        X = Var(0, "X")
        # X is 0 mod 5 (should be 0)
        query = Struct("is", (X, Struct("mod", (Int(0), Int(5)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(0)}]

    @pytest.mark.parametrize("n,d", [(7, -3), (-7, 3), (-7, -3)])
    def test_mod_sign_and_identity(self, empty_engine, n, d):
        """Test mod sign convention and identity N = Q*D + R."""
        N, D = Int(n), Int(d)
        Q, R = Var(0, "Q"), Var(1, "R")
        query = Struct(",", (
            Struct("is", (Q, Struct("//", (N, D)))),
            Struct("is", (R, Struct("mod", (N, D))))
        ))
        sol = empty_engine.run([query])[0]
        # Verify N == Q*D + R
        check = Struct("is", (Int(n), Struct("+", (Struct("*", (sol["Q"], D)), sol["R"]))))
        assert len(empty_engine.run([check])) == 1


class TestUnaryMinus:
    """Tests for unary minus operator."""

    def test_unary_minus_positive(self, empty_engine):
        """Test unary minus on positive number."""
        X = Var(0, "X")
        # X is -5 (unary minus)
        query = Struct("is", (X, Struct("-", (Int(5),))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-5)}]

    def test_unary_minus_negative(self, empty_engine):
        """Test unary minus on negative number."""
        X = Var(0, "X")
        # X is -(-5) (should be 5)
        query = Struct("is", (X, Struct("-", (Int(-5),))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(5)}]

    def test_unary_minus_zero(self, empty_engine):
        """Test unary minus on zero."""
        X = Var(0, "X")
        # X is -0 (should be 0)
        query = Struct("is", (X, Struct("-", (Int(0),))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(0)}]

    def test_unary_minus_in_expression(self, empty_engine):
        """Test unary minus in compound expression."""
        X = Var(0, "X")
        # X is 10 + (-3) (should be 7)
        query = Struct("is", (X, Struct("+", (Int(10), Struct("-", (Int(3),))))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(7)}]

    def test_unary_minus_double_negation(self, empty_engine):
        """Test double unary minus."""
        X = Var(0, "X")
        # X is -(-42) (should be 42)
        query = Struct("is", (X, Struct("-", (Struct("-", (Int(42),)),))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(42)}]

    def test_unary_minus_rhs_var(self, empty_engine):
        """Test unary minus with RHS variable."""
        X, Y = Var(0, "X"), Var(1, "Y")
        query = Struct(",", (Struct("=", (Y, Int(5))), Struct("is", (X, Struct("-", (Y,))))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-5), "Y": Int(5)}]


class TestIsChecksAndLiterals:
    """Tests for is/2 checking behavior (not just assignment)."""

    def test_is_checks_when_lhs_bound_success(self, empty_engine):
        """Test is/2 can check as well as assign - success case."""
        X = Var(0, "X")
        # X = 2, and 7 // 3 evaluates to 2 -> success
        query = Struct(",", (Struct("=", (X, Int(2))), Struct("is", (X, Struct("//", (Int(7), Int(3)))))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(2)

    def test_is_checks_when_lhs_bound_failure(self, empty_engine):
        """Test is/2 can check as well as assign - failure case."""
        X = Var(0, "X")
        # X = 3, but 7 // 3 evaluates to 2 -> fail
        query = Struct(",", (Struct("=", (X, Int(3))), Struct("is", (X, Struct("//", (Int(7), Int(3)))))))
        solutions = empty_engine.run([query])
        assert solutions == []

    def test_is_with_numeric_lhs_literal(self, empty_engine):
        """Test is/2 with numeric literal on LHS."""
        query_ok = Struct("is", (Int(3), Struct("+", (Int(1), Int(2)))))
        query_fail = Struct("is", (Int(4), Struct("+", (Int(1), Int(2)))))
        assert len(empty_engine.run([query_ok])) == 1
        assert len(empty_engine.run([query_fail])) == 0


class TestOperandBinding:
    """Test variable binding on RHS of is/2."""

    def test_rhs_var_bound_then_evals(self, empty_engine):
        """Test RHS variable bound before evaluation."""
        X, Y = Var(0, "X"), Var(1, "Y")
        query = Struct(",", (
            Struct("=", (Y, Int(7))),
            Struct("is", (X, Struct("//", (Y, Int(3)))))
        ))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(2), "Y": Int(7)}]


class TestMixedArithmeticOperators:
    """Tests for combinations of arithmetic operators."""

    @pytest.mark.parametrize("n,d", [(-17, 5), (17, -5), (-17, -5)])
    def test_div_mod_identity_with_negatives(self, empty_engine, n, d):
        """Test that n = (n // d) * d + (n mod d) for all sign combinations."""
        N, D = Int(n), Int(d)
        Q, R, Check = Var(0, "Q"), Var(1, "R"), Var(2, "Check")
        query = Struct(",", (
            Struct("is", (Q, Struct("//", (N, D)))),
            Struct(",", (
                Struct("is", (R, Struct("mod", (N, D)))),
                Struct("is", (Check, Struct("+", (Struct("*", (Q, D)), R))))
            ))
        ))
        sols = empty_engine.run([query])
        assert len(sols) == 1
        assert sols[0]["Check"] == N

    def test_unary_minus_with_division(self, empty_engine):
        """Test unary minus with division."""
        X = Var(0, "X")
        # X is -(12 // 5) (should be -2)
        query = Struct("is", (X, Struct("-", (Struct("//", (Int(12), Int(5))),))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(-2)}]

    def test_modulo_of_negative_expression(self, empty_engine):
        """Test modulo of a negative expression."""
        X = Var(0, "X")
        # X is (-10) mod 3 (should be 2)
        query = Struct("is", (X, Struct("mod", (Struct("-", (Int(10),)), Int(3)))))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(2)}]

    def test_complex_expression_with_all_operators(self, empty_engine):
        """Test complex expression using //, mod, and unary minus."""
        X = Var(0, "X")
        # X is (20 // 3) * 3 + (20 mod 3) + (-(5))
        # (6 * 3) + 2 + (-5) = 18 + 2 - 5 = 15
        query = Struct("is", (X, 
            Struct("+", (
                Struct("+", (
                    Struct("*", (
                        Struct("//", (Int(20), Int(3))),
                        Int(3)
                    )),
                    Struct("mod", (Int(20), Int(3)))
                )),
                Struct("-", (Int(5),))
            ))
        ))
        solutions = empty_engine.run([query])
        assert solutions == [{"X": Int(15)}]


class TestArithmeticErrorHandling:
    """Tests for arithmetic error conditions."""

    def test_division_by_zero_in_complex_expression(self, empty_engine):
        """Test division by zero in middle of expression fails."""
        X = Var(0, "X")
        # X is 5 + (10 // 0) - should fail
        query = Struct("is", (X, Struct("+", (Int(5), Struct("//", (Int(10), Int(0)))))))
        solutions = empty_engine.run([query])
        assert solutions == []

    def test_modulo_by_zero_in_complex_expression(self, empty_engine):
        """Test modulo by zero in middle of expression fails."""
        X = Var(0, "X")
        # X is 5 + (10 mod 0) - should fail
        query = Struct("is", (X, Struct("+", (Int(5), Struct("mod", (Int(10), Int(0)))))))
        solutions = empty_engine.run([query])
        assert solutions == []

    def test_unbound_variable_in_arithmetic_fails(self, empty_engine):
        """Test arithmetic with unbound variable fails."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X is Y + 5 where Y is unbound - should fail
        query = Struct("is", (X, Struct("+", (Y, Int(5)))))
        solutions = empty_engine.run([query])
        assert solutions == []

    def test_non_number_in_arithmetic_fails(self, empty_engine):
        """Test arithmetic with non-number fails."""
        X = Var(0, "X")
        # X is abc + 5 - should fail
        query = Struct("is", (X, Struct("+", (Atom("abc"), Int(5)))))
        solutions = empty_engine.run([query])
        assert solutions == []