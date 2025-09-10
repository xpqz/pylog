"""
Tests for arithmetic comparison operators.
Covers > (greater than) and =:= (arithmetic equality).
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.engine.engine import Engine
from prolog.tests.helpers import program


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestGreaterThan:
    """Tests for >/2 (greater than) predicate."""

    def test_gt_integers_true(self, empty_engine):
        """Test > succeeds when left > right."""
        query = Struct(">", (Int(5), Int(3)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_gt_integers_false(self, empty_engine):
        """Test > fails when left <= right."""
        query = Struct(">", (Int(3), Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_gt_equal_integers_false(self, empty_engine):
        """Test > fails when left == right."""
        query = Struct(">", (Int(5), Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_gt_negative_numbers(self, empty_engine):
        """Test > with negative numbers."""
        query = Struct(">", (Int(-3), Int(-5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_gt_zero_comparisons(self, empty_engine):
        """Test > with zero."""
        # 5 > 0
        query1 = Struct(">", (Int(5), Int(0)))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 1
        
        # 0 > -5
        query2 = Struct(">", (Int(0), Int(-5)))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 1
        
        # 0 > 0
        query3 = Struct(">", (Int(0), Int(0)))
        solutions3 = empty_engine.run([query3])
        assert len(solutions3) == 0

    def test_gt_with_bound_variables(self, empty_engine):
        """Test > with bound variables."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = 10, Y = 5, X > Y
        query = Struct(",", (
            Struct("=", (X, Int(10))),
            Struct(",", (
                Struct("=", (Y, Int(5))),
                Struct(">", (X, Y))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(10)
        assert solutions[0]["Y"] == Int(5)

    def test_gt_with_unbound_variable_fails(self, empty_engine):
        """Test > fails with unbound variables."""
        X = Var(0, "X")
        # X > 5 with X unbound
        query = Struct(">", (X, Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_gt_with_non_integer_fails(self, empty_engine):
        """Test > fails with non-integer arguments."""
        # atom > 5
        query1 = Struct(">", (Atom("abc"), Int(5)))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # 5 > atom
        query2 = Struct(">", (Int(5), Atom("abc")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0

    def test_gt_with_arithmetic_expressions(self, empty_engine):
        """Test > with arithmetic expressions via is/2."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X is 3 + 4, Y is 2 * 3, X > Y
        query = Struct(",", (
            Struct("is", (X, Struct("+", (Int(3), Int(4))))),
            Struct(",", (
                Struct("is", (Y, Struct("*", (Int(2), Int(3))))),
                Struct(">", (X, Y))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(7)
        assert solutions[0]["Y"] == Int(6)

    def test_gt_wrong_arity(self, empty_engine):
        """Test > with wrong number of arguments."""
        # > with 1 argument
        query1 = Struct(">", (Int(5),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # > with 3 arguments
        query2 = Struct(">", (Int(5), Int(3), Int(1)))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0


class TestArithmeticEquality:
    """Tests for =:=/2 (arithmetic equality) predicate."""

    def test_arith_eq_integers_true(self, empty_engine):
        """Test =:= succeeds when values are equal."""
        query = Struct("=:=", (Int(5), Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_arith_eq_integers_false(self, empty_engine):
        """Test =:= fails when values are different."""
        query = Struct("=:=", (Int(5), Int(3)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_arith_eq_negative_numbers(self, empty_engine):
        """Test =:= with negative numbers."""
        query = Struct("=:=", (Int(-5), Int(-5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_arith_eq_zero(self, empty_engine):
        """Test =:= with zero."""
        query = Struct("=:=", (Int(0), Int(0)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_arith_eq_with_bound_variables(self, empty_engine):
        """Test =:= with bound variables."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = 42, Y = 42, X =:= Y
        query = Struct(",", (
            Struct("=", (X, Int(42))),
            Struct(",", (
                Struct("=", (Y, Int(42))),
                Struct("=:=", (X, Y))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_arith_eq_with_expressions(self, empty_engine):
        """Test =:= with arithmetic expressions."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X is 3 + 4, Y is 2 + 5, X =:= Y
        query = Struct(",", (
            Struct("is", (X, Struct("+", (Int(3), Int(4))))),
            Struct(",", (
                Struct("is", (Y, Struct("+", (Int(2), Int(5))))),
                Struct("=:=", (X, Y))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_arith_eq_vs_unification(self, empty_engine):
        """Test difference between =:= and =."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        
        # X = 5, Y = 5, X = Y (unification succeeds)
        query1 = Struct(",", (
            Struct("=", (X, Int(5))),
            Struct(",", (
                Struct("=", (Y, Int(5))),
                Struct("=", (X, Y))
            ))
        ))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 1
        
        # Same with =:=
        X2 = Var(2, "X2")
        Y2 = Var(3, "Y2")
        query2 = Struct(",", (
            Struct("=", (X2, Int(5))),
            Struct(",", (
                Struct("=", (Y2, Int(5))),
                Struct("=:=", (X2, Y2))
            ))
        ))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 1

    def test_arith_eq_with_unbound_fails(self, empty_engine):
        """Test =:= fails with unbound variables."""
        X = Var(0, "X")
        # X =:= 5 with X unbound
        query = Struct("=:=", (X, Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_arith_eq_with_non_integer_fails(self, empty_engine):
        """Test =:= fails with non-integer arguments."""
        query = Struct("=:=", (Atom("abc"), Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_arith_eq_wrong_arity(self, empty_engine):
        """Test =:= with wrong number of arguments."""
        query = Struct("=:=", (Int(5),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestComparisonChains:
    """Tests for chained comparisons."""

    def test_comparison_chain(self, empty_engine):
        """Test chaining multiple comparisons."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        # X = 10, Y = 5, Z = 5, X > Y, Y =:= Z
        query = Struct(",", (
            Struct("=", (X, Int(10))),
            Struct(",", (
                Struct("=", (Y, Int(5))),
                Struct(",", (
                    Struct("=", (Z, Int(5))),
                    Struct(",", (
                        Struct(">", (X, Y)),
                        Struct("=:=", (Y, Z))
                    ))
                ))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_comparison_in_disjunction(self, empty_engine):
        """Test comparisons in disjunction."""
        X = Var(0, "X")
        # X = 5, (X > 10 ; X =:= 5)
        query = Struct(",", (
            Struct("=", (X, Int(5))),
            Struct(";", (
                Struct(">", (X, Int(10))),
                Struct("=:=", (X, Int(5)))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1  # Second branch succeeds

    def test_comparison_with_fail(self, empty_engine):
        """Test comparisons that fail."""
        # 3 > 5, should fail immediately
        query = Struct(">", (Int(3), Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestComparisonEdgeCases:
    """Edge cases for comparison operators."""

    def test_large_numbers(self, empty_engine):
        """Test comparisons with large numbers."""
        large1 = Int(1000000)
        large2 = Int(999999)
        
        query1 = Struct(">", (large1, large2))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 1
        
        query2 = Struct("=:=", (large1, large1))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 1

    def test_comparison_with_deeply_bound_vars(self, empty_engine):
        """Test comparisons with variables bound through chains."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        # X = Y, Y = Z, Z = 10, X > 5
        query = Struct(",", (
            Struct("=", (X, Y)),
            Struct(",", (
                Struct("=", (Y, Z)),
                Struct(",", (
                    Struct("=", (Z, Int(10))),
                    Struct(">", (X, Int(5)))
                ))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1