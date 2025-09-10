"""
Advanced tests for is/2 arithmetic evaluation builtin.
Tests edge cases and complex expressions.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestIsWithComplexExpressions:
    """Tests for is/2 with complex arithmetic expressions."""

    def test_is_deeply_nested_expression(self, empty_engine):
        """Test is/2 with deeply nested arithmetic expression."""
        X = Var(0, "X")
        # X is ((1 + 2) * 3) - (4 // 2)
        expr = Struct("-", (
            Struct("*", (
                Struct("+", (Int(1), Int(2))),
                Int(3)
            )),
            Struct("//", (Int(4), Int(2)))
        ))
        query = Struct("is", (X, expr))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(7)  # (3 * 3) - 2 = 7

    def test_is_with_multiple_unary_minus(self, empty_engine):
        """Test is/2 with multiple unary minus operations."""
        X = Var(0, "X")
        # X is -(-(5))
        expr = Struct("-", (Struct("-", (Int(5),)),))
        query = Struct("is", (X, expr))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(5)

    def test_is_with_all_operators(self, empty_engine):
        """Test is/2 with expression using all supported operators."""
        X = Var(0, "X")
        # X is (10 + 5) * 2 - 8 // 3 + 10 mod 3
        expr = Struct("+", (
            Struct("-", (
                Struct("*", (
                    Struct("+", (Int(10), Int(5))),
                    Int(2)
                )),
                Struct("//", (Int(8), Int(3)))
            )),
            Struct("mod", (Int(10), Int(3)))
        ))
        query = Struct("is", (X, expr))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # (15 * 2) - 2 + 1 = 30 - 2 + 1 = 29
        assert solutions[0]["X"] == Int(29)


class TestIsWithVariableExpressions:
    """Tests for is/2 with variables in expressions."""

    def test_is_rhs_variable_chain(self, empty_engine):
        """Test is/2 with chained variable references on RHS."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        W = Var(3, "W")
        # W = Z, Z = Y, Y = 10, X is W + 5
        query = Struct(",", (
            Struct("=", (W, Z)),
            Struct(",", (
                Struct("=", (Z, Y)),
                Struct(",", (
                    Struct("=", (Y, Int(10))),
                    Struct("is", (X, Struct("+", (W, Int(5)))))
                ))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(15)

    def test_is_with_partially_bound_expression(self, empty_engine):
        """Test is/2 with expression containing bound and unbound vars."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        # Y = 5, X is Y + Z (Z unbound - should fail)
        query = Struct(",", (
            Struct("=", (Y, Int(5))),
            Struct("is", (X, Struct("+", (Y, Z))))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0  # Fails due to unbound Z

    def test_is_rhs_variable_to_expression(self, empty_engine):
        """Test is/2 where RHS variable is bound to expression."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # Y = 3 + 4, X is Y (should evaluate Y)
        # Note: This binds Y to the struct +(3,4), not to 7
        query = Struct(",", (
            Struct("=", (Y, Struct("+", (Int(3), Int(4))))),
            Struct("is", (X, Y))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(7)


class TestIsAsChecker:
    """Tests for is/2 used as a checker (LHS already bound)."""

    def test_is_check_success(self, empty_engine):
        """Test is/2 succeeds when LHS matches evaluated RHS."""
        # 7 is 3 + 4
        query = Struct("is", (Int(7), Struct("+", (Int(3), Int(4)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_is_check_failure(self, empty_engine):
        """Test is/2 fails when LHS doesn't match evaluated RHS."""
        # 8 is 3 + 4 (should fail)
        query = Struct("is", (Int(8), Struct("+", (Int(3), Int(4)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_is_check_with_bound_variable(self, empty_engine):
        """Test is/2 check mode with bound variable on LHS."""
        X = Var(0, "X")
        # X = 10, X is 5 * 2
        query = Struct(",", (
            Struct("=", (X, Int(10))),
            Struct("is", (X, Struct("*", (Int(5), Int(2)))))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_is_check_bound_var_mismatch(self, empty_engine):
        """Test is/2 check fails with bound variable mismatch."""
        X = Var(0, "X")
        # X = 10, X is 5 + 2 (should fail: 10 != 7)
        query = Struct(",", (
            Struct("=", (X, Int(10))),
            Struct("is", (X, Struct("+", (Int(5), Int(2)))))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestIsErrorCases:
    """Tests for is/2 error conditions."""

    def test_is_with_non_numeric_in_expression(self, empty_engine):
        """Test is/2 fails with non-numeric terms in expression."""
        X = Var(0, "X")
        # X is abc + 5 (atom in arithmetic)
        query = Struct("is", (X, Struct("+", (Atom("abc"), Int(5)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_is_with_structure_in_expression(self, empty_engine):
        """Test is/2 fails with structure in expression."""
        X = Var(0, "X")
        # X is f(1) + 2
        query = Struct("is", (X, Struct("+", (Struct("f", (Int(1),)), Int(2)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_is_with_list_in_expression(self, empty_engine):
        """Test is/2 fails with list in expression."""
        X = Var(0, "X")
        # X is [1,2] + 3
        query = Struct("is", (X, Struct("+", (List((Int(1), Int(2))), Int(3)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_is_wrong_arity(self, empty_engine):
        """Test is/2 with wrong number of arguments."""
        # is(X) - missing second argument
        query1 = Struct("is", (Var(0, "X"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # is(X, Y, Z) - too many arguments
        query2 = Struct("is", (Var(0, "X"), Int(5), Int(3)))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0

    def test_is_unknown_operator(self, empty_engine):
        """Test is/2 with unknown operator in expression."""
        X = Var(0, "X")
        # X is 5 ** 2 (** not supported)
        query = Struct("is", (X, Struct("**", (Int(5), Int(2)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestIsWithDivisionByZero:
    """Tests for division by zero handling in is/2."""

    def test_is_integer_division_by_zero(self, empty_engine):
        """Test is/2 with integer division by zero."""
        X = Var(0, "X")
        # X is 10 // 0
        query = Struct("is", (X, Struct("//", (Int(10), Int(0)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_is_modulo_by_zero(self, empty_engine):
        """Test is/2 with modulo by zero."""
        X = Var(0, "X")
        # X is 10 mod 0
        query = Struct("is", (X, Struct("mod", (Int(10), Int(0)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_is_complex_expr_with_div_by_zero(self, empty_engine):
        """Test is/2 fails entire expression on division by zero."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # Y = 0, X is 5 + (10 // Y)
        query = Struct(",", (
            Struct("=", (Y, Int(0))),
            Struct("is", (X, Struct("+", (
                Int(5),
                Struct("//", (Int(10), Y))
            ))))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestIsInComplexQueries:
    """Tests for is/2 in complex query contexts."""

    def test_is_in_disjunction(self, empty_engine):
        """Test is/2 in disjunction branches."""
        X = Var(0, "X")
        # (X is 3 + 4) ; (X is 5 * 2)
        query = Struct(";", (
            Struct("is", (X, Struct("+", (Int(3), Int(4))))),
            Struct("is", (X, Struct("*", (Int(5), Int(2)))))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(7)
        assert solutions[1]["X"] == Int(10)

    def test_is_with_backtracking(self, empty_engine):
        """Test is/2 with backtracking through multiple solutions."""
        # Create a program with multiple facts
        from prolog.tests.helpers import mk_fact
        p = program(
            mk_fact("num", Int(1)),
            mk_fact("num", Int(2)),
            mk_fact("num", Int(3)),
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        Y = Var(1, "Y")
        # num(X), Y is X * 10
        query = Struct(",", (
            Struct("num", (X,)),
            Struct("is", (Y, Struct("*", (X, Int(10)))))
        ))
        solutions = engine.run([query])
        assert len(solutions) == 3
        assert solutions[0]["Y"] == Int(10)
        assert solutions[1]["Y"] == Int(20)
        assert solutions[2]["Y"] == Int(30)