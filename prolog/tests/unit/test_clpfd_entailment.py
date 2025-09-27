"""Comprehensive tests for CLP(FD) entailment detection.

Tests entailment detection for all constraint types with various
domain configurations including var-var, var-int, and int-int cases.
"""

import pytest
from enum import Enum

from prolog.unify.store import Store
from prolog.unify.trail import Trail
from prolog.ast.terms import Var, Int
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import get_domain, set_domain


class TestEntailmentEnum:
    """Test the Entailment enum."""

    def test_entailment_enum_exists(self):
        """Test that Entailment enum is defined with correct values."""
        from prolog.clpfd.entailment import Entailment

        assert Entailment.TRUE.value == 1
        assert Entailment.FALSE.value == 0
        assert Entailment.UNKNOWN.value == -1

    def test_entailment_enum_comparison(self):
        """Test that Entailment values can be compared."""
        from prolog.clpfd.entailment import Entailment

        assert Entailment.TRUE != Entailment.FALSE
        assert Entailment.TRUE != Entailment.UNKNOWN
        assert Entailment.FALSE != Entailment.UNKNOWN


class TestEqualityEntailment:
    """Test check_equality_entailment for X #= Y."""

    def test_int_int_equality(self):
        """Test equality entailment with two integers."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()

        # 3 #= 3 should be TRUE
        result = check_equality_entailment(store, (None, 3), (None, 3))
        assert result == Entailment.TRUE

        # 3 #= 5 should be FALSE
        result = check_equality_entailment(store, (None, 3), (None, 5))
        assert result == Entailment.FALSE

    def test_var_int_equality(self):
        """Test equality entailment with variable and integer."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")

        # X in 1..5, X #= 3
        set_domain(store, x_id, Domain(((1, 5),)), trail)
        result = check_equality_entailment(store, x_id, (None, 3))
        assert result == Entailment.UNKNOWN  # 3 is possible but not certain

        # X in {3}, X #= 3
        set_domain(store, x_id, Domain(((3, 3),)), trail)
        result = check_equality_entailment(store, x_id, (None, 3))
        assert result == Entailment.TRUE  # X must be 3

        # X in 1..2, X #= 3
        set_domain(store, x_id, Domain(((1, 2),)), trail)
        result = check_equality_entailment(store, x_id, (None, 3))
        assert result == Entailment.FALSE  # 3 not in domain

    def test_int_var_equality(self):
        """Test equality entailment with integer and variable (symmetric)."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        y_id = store.new_var("Y")

        # 3 #= Y, Y in 1..5
        set_domain(store, y_id, Domain(((1, 5),)), trail)
        result = check_equality_entailment(store, (None, 3), y_id)
        assert result == Entailment.UNKNOWN

        # 3 #= Y, Y in {3}
        set_domain(store, y_id, Domain(((3, 3),)), trail)
        result = check_equality_entailment(store, (None, 3), y_id)
        assert result == Entailment.TRUE

        # 3 #= Y, Y in 4..6
        set_domain(store, y_id, Domain(((4, 6),)), trail)
        result = check_equality_entailment(store, (None, 3), y_id)
        assert result == Entailment.FALSE

    def test_var_var_equality_disjoint(self):
        """Test equality entailment with disjoint domains."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 1..3, Y in 5..7 (disjoint)
        set_domain(store, x_id, Domain(((1, 3),)), trail)
        set_domain(store, y_id, Domain(((5, 7),)), trail)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE

    def test_var_var_equality_overlapping(self):
        """Test equality entailment with overlapping domains."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 1..5, Y in 3..7 (overlapping)
        set_domain(store, x_id, Domain(((1, 5),)), trail)
        set_domain(store, y_id, Domain(((3, 7),)), trail)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN  # Could be equal in 3..5

    def test_var_var_equality_singleton_same(self):
        """Test equality entailment with same singleton domains."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in {5}, Y in {5}
        set_domain(store, x_id, Domain(((5, 5),)), trail)
        set_domain(store, y_id, Domain(((5, 5),)), trail)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.TRUE

    def test_var_var_equality_singleton_different(self):
        """Test equality entailment with different singleton domains."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in {3}, Y in {5}
        set_domain(store, x_id, Domain(((3, 3),)), trail)
        set_domain(store, y_id, Domain(((5, 5),)), trail)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE

    def test_var_var_equality_one_singleton(self):
        """Test equality entailment with one singleton domain."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in {5}, Y in 1..10
        set_domain(store, x_id, Domain(((5, 5),)), trail)
        set_domain(store, y_id, Domain(((1, 10),)), trail)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN  # Y could be 5

        # X in {5}, Y in 1..4
        set_domain(store, y_id, Domain(((1, 4),)), trail)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE  # Y can't be 5

    def test_var_var_equality_no_domains(self):
        """Test equality entailment with variables having no domains."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # No domains set - should be UNKNOWN
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN


class TestLessThanEntailment:
    """Test check_less_than_entailment for X #< Y."""

    def test_int_int_less_than(self):
        """Test less-than entailment with two integers."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()

        # 3 #< 5 should be TRUE
        result = check_less_than_entailment(store, (None, 3), (None, 5))
        assert result == Entailment.TRUE

        # 5 #< 3 should be FALSE
        result = check_less_than_entailment(store, (None, 5), (None, 3))
        assert result == Entailment.FALSE

        # 3 #< 3 should be FALSE
        result = check_less_than_entailment(store, (None, 3), (None, 3))
        assert result == Entailment.FALSE

    def test_var_int_less_than(self):
        """Test less-than entailment with variable and integer."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")

        # X in 1..3, X #< 5
        set_domain(store, x_id, Domain(((1, 3),)), trail)
        result = check_less_than_entailment(store, x_id, (None, 5))
        assert result == Entailment.TRUE  # Max of X is 3 < 5

        # X in 5..7, X #< 5
        set_domain(store, x_id, Domain(((5, 7),)), trail)
        result = check_less_than_entailment(store, x_id, (None, 5))
        assert result == Entailment.FALSE  # Min of X is 5 >= 5

        # X in 3..7, X #< 5
        set_domain(store, x_id, Domain(((3, 7),)), trail)
        result = check_less_than_entailment(store, x_id, (None, 5))
        assert result == Entailment.UNKNOWN  # X could be 3,4 (true) or 5,6,7 (false)

    def test_int_var_less_than(self):
        """Test less-than entailment with integer and variable."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        trail = Trail()
        y_id = store.new_var("Y")

        # 3 #< Y, Y in 5..7
        set_domain(store, y_id, Domain(((5, 7),)), trail)
        result = check_less_than_entailment(store, (None, 3), y_id)
        assert result == Entailment.TRUE  # 3 < min(Y)=5

        # 7 #< Y, Y in 5..7
        set_domain(store, y_id, Domain(((5, 7),)), trail)
        result = check_less_than_entailment(store, (None, 7), y_id)
        assert result == Entailment.FALSE  # 7 >= max(Y)=7

        # 5 #< Y, Y in 5..7
        set_domain(store, y_id, Domain(((5, 7),)), trail)
        result = check_less_than_entailment(store, (None, 5), y_id)
        assert result == Entailment.UNKNOWN  # Y could be 6,7 (true) or 5 (false)

    def test_var_var_less_than_disjoint(self):
        """Test less-than entailment with disjoint domains."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 1..3, Y in 5..7
        set_domain(store, x_id, Domain(((1, 3),)), trail)
        set_domain(store, y_id, Domain(((5, 7),)), trail)
        result = check_less_than_entailment(store, x_id, y_id)
        assert result == Entailment.TRUE  # max(X)=3 < min(Y)=5

        # X in 5..7, Y in 1..3
        set_domain(store, x_id, Domain(((5, 7),)), trail)
        set_domain(store, y_id, Domain(((1, 3),)), trail)
        result = check_less_than_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE  # min(X)=5 >= max(Y)=3

    def test_var_var_less_than_overlapping(self):
        """Test less-than entailment with overlapping domains."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 1..5, Y in 3..7
        set_domain(store, x_id, Domain(((1, 5),)), trail)
        set_domain(store, y_id, Domain(((3, 7),)), trail)
        result = check_less_than_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN  # Could be true or false

    def test_var_var_less_than_touching(self):
        """Test less-than entailment with touching domains."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 1..3, Y in 3..5
        set_domain(store, x_id, Domain(((1, 3),)), trail)
        set_domain(store, y_id, Domain(((3, 5),)), trail)
        result = check_less_than_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN  # X=3, Y=3 would be false


class TestLessEqualEntailment:
    """Test check_less_equal_entailment for X #=< Y."""

    def test_int_int_less_equal(self):
        """Test less-equal entailment with two integers."""
        from prolog.clpfd.entailment import check_less_equal_entailment, Entailment

        store = Store()

        # 3 #=< 5 should be TRUE
        result = check_less_equal_entailment(store, (None, 3), (None, 5))
        assert result == Entailment.TRUE

        # 5 #=< 3 should be FALSE
        result = check_less_equal_entailment(store, (None, 5), (None, 3))
        assert result == Entailment.FALSE

        # 3 #=< 3 should be TRUE
        result = check_less_equal_entailment(store, (None, 3), (None, 3))
        assert result == Entailment.TRUE

    def test_var_var_less_equal(self):
        """Test less-equal entailment with variables."""
        from prolog.clpfd.entailment import check_less_equal_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 1..3, Y in 3..5
        set_domain(store, x_id, Domain(((1, 3),)), trail)
        set_domain(store, y_id, Domain(((3, 5),)), trail)
        result = check_less_equal_entailment(store, x_id, y_id)
        assert result == Entailment.TRUE  # max(X)=3 <= min(Y)=3

        # X in 5..7, Y in 1..4
        set_domain(store, x_id, Domain(((5, 7),)), trail)
        set_domain(store, y_id, Domain(((1, 4),)), trail)
        result = check_less_equal_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE  # min(X)=5 > max(Y)=4


class TestGreaterThanEntailment:
    """Test check_greater_than_entailment for X #> Y."""

    def test_greater_than_via_less_than(self):
        """Test that X #> Y uses Y #< X internally."""
        from prolog.clpfd.entailment import check_greater_than_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 5..7, Y in 1..3
        set_domain(store, x_id, Domain(((5, 7),)), trail)
        set_domain(store, y_id, Domain(((1, 3),)), trail)
        result = check_greater_than_entailment(store, x_id, y_id)
        assert result == Entailment.TRUE  # min(X)=5 > max(Y)=3

        # X in 1..3, Y in 5..7
        set_domain(store, x_id, Domain(((1, 3),)), trail)
        set_domain(store, y_id, Domain(((5, 7),)), trail)
        result = check_greater_than_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE  # max(X)=3 < min(Y)=5


class TestGreaterEqualEntailment:
    """Test check_greater_equal_entailment for X #>= Y."""

    def test_greater_equal_via_less_equal(self):
        """Test that X #>= Y uses Y #=< X internally."""
        from prolog.clpfd.entailment import check_greater_equal_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X in 3..7, Y in 1..3
        set_domain(store, x_id, Domain(((3, 7),)), trail)
        set_domain(store, y_id, Domain(((1, 3),)), trail)
        result = check_greater_equal_entailment(store, x_id, y_id)
        assert result == Entailment.TRUE  # min(X)=3 >= max(Y)=3


class TestNotEqualEntailment:
    """Test check_not_equal_entailment for X #\\= Y."""

    def test_not_equal_inverse_of_equal(self):
        """Test that not-equal is the inverse of equality."""
        from prolog.clpfd.entailment import check_not_equal_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # When equality is TRUE, not-equal is FALSE
        set_domain(store, x_id, Domain(((3, 3),)), trail)
        set_domain(store, y_id, Domain(((3, 3),)), trail)
        result = check_not_equal_entailment(store, x_id, y_id)
        assert result == Entailment.FALSE

        # When equality is FALSE, not-equal is TRUE
        set_domain(store, x_id, Domain(((1, 2),)), trail)
        set_domain(store, y_id, Domain(((5, 6),)), trail)
        result = check_not_equal_entailment(store, x_id, y_id)
        assert result == Entailment.TRUE

        # When equality is UNKNOWN, not-equal is UNKNOWN
        set_domain(store, x_id, Domain(((1, 5),)), trail)
        set_domain(store, y_id, Domain(((3, 7),)), trail)
        result = check_not_equal_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN


class TestConstraintNegations:
    """Test the CONSTRAINT_NEGATIONS mapping."""

    def test_negation_mapping_complete(self):
        """Test that all constraint types have negations defined."""
        from prolog.clpfd.entailment import CONSTRAINT_NEGATIONS

        # Check all required mappings exist
        assert "#=" in CONSTRAINT_NEGATIONS
        assert "#\\=" in CONSTRAINT_NEGATIONS
        assert "#<" in CONSTRAINT_NEGATIONS
        assert "#=<" in CONSTRAINT_NEGATIONS
        assert "#>" in CONSTRAINT_NEGATIONS
        assert "#>=" in CONSTRAINT_NEGATIONS

    def test_negation_mapping_correct(self):
        """Test that negation mappings are correct."""
        from prolog.clpfd.entailment import CONSTRAINT_NEGATIONS

        # ¬(X #= Y) = X #\= Y
        assert CONSTRAINT_NEGATIONS["#="] == "#\\="

        # ¬(X #\= Y) = X #= Y
        assert CONSTRAINT_NEGATIONS["#\\="] == "#="

        # ¬(X #< Y) = X #>= Y
        assert CONSTRAINT_NEGATIONS["#<"] == "#>="

        # ¬(X #=< Y) = X #> Y
        assert CONSTRAINT_NEGATIONS["#=<"] == "#>"

        # ¬(X #> Y) = X #=< Y
        assert CONSTRAINT_NEGATIONS["#>"] == "#=<"

        # ¬(X #>= Y) = X #< Y
        assert CONSTRAINT_NEGATIONS["#>="] == "#<"

    def test_negation_mapping_invertible(self):
        """Test that negations are invertible (double negation)."""
        from prolog.clpfd.entailment import CONSTRAINT_NEGATIONS

        for constraint, negation in CONSTRAINT_NEGATIONS.items():
            # Double negation should give back the original
            double_neg = CONSTRAINT_NEGATIONS[negation]
            assert double_neg == constraint, f"¬¬{constraint} should be {constraint}, got {double_neg}"


class TestEdgeCases:
    """Test edge cases in entailment detection."""

    def test_empty_domain_handling(self):
        """Test handling of empty domains."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # X has empty domain
        set_domain(store, x_id, Domain(()), trail)
        set_domain(store, y_id, Domain(((1, 5),)), trail)

        # Should handle gracefully (implementation choice: could be FALSE or UNKNOWN)
        result = check_equality_entailment(store, x_id, y_id)
        assert result in (Entailment.FALSE, Entailment.UNKNOWN)

    def test_none_domain_handling(self):
        """Test handling when get_domain returns None."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        x_id = store.new_var("X")
        y_id = store.new_var("Y")

        # No domains set (get_domain returns None)
        result = check_equality_entailment(store, x_id, y_id)
        assert result == Entailment.UNKNOWN  # Can't determine without domains

    def test_mixed_var_int_none_cases(self):
        """Test mixed cases with variables, integers, and None values."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        x_id = store.new_var("X")

        # Variable with no domain vs integer
        result = check_less_than_entailment(store, x_id, (None, 5))
        assert result == Entailment.UNKNOWN  # Can't determine without domain


class TestPerformanceCharacteristics:
    """Test that entailment checks have expected performance."""

    def test_singleton_check_fast(self):
        """Test that singleton domain checks are efficient."""
        from prolog.clpfd.entailment import check_equality_entailment, Entailment

        store = Store()
        trail = Trail()

        # Create many variables with singleton domains
        for i in range(100):
            x_id = store.new_var(f"X{i}")
            y_id = store.new_var(f"Y{i}")
            set_domain(store, x_id, Domain(((i, i),)), trail)
            set_domain(store, y_id, Domain(((i, i),)), trail)

            # Should be fast (O(1) domain operations)
            result = check_equality_entailment(store, x_id, y_id)
            assert result == Entailment.TRUE

    def test_bounds_check_fast(self):
        """Test that bounds-based checks are efficient."""
        from prolog.clpfd.entailment import check_less_than_entailment, Entailment

        store = Store()
        trail = Trail()

        # Create many variables with disjoint domains
        for i in range(100):
            x_id = store.new_var(f"X{i}")
            y_id = store.new_var(f"Y{i}")
            set_domain(store, x_id, Domain(((i*10, i*10+4),)), trail)
            set_domain(store, y_id, Domain(((i*10+5, i*10+9),)), trail)

            # Should be fast (just compare bounds)
            result = check_less_than_entailment(store, x_id, y_id)
            assert result == Entailment.TRUE