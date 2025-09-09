"""Tests for SWI-Prolog baseline functionality."""

import pytest
from prolog.tests.swi_baseline import is_swipl_available


pytestmark = pytest.mark.swi_baseline


class TestSWIBaseline:
    """Test that the SWI baseline harness works correctly."""
    
    def test_swi_available(self):
        """Test that we can detect SWI-Prolog availability."""
        # This will be True in CI and on dev machines with SWI installed
        # The test will be skipped if SWI is not available
        assert is_swipl_available()
    
    def test_count_simple(self, swi):
        """Test counting solutions for simple predicates."""
        prog = "p(1). p(2). p(3)."
        assert swi.count(prog, "p(X)") == 3
        assert swi.count(prog, "p(1)") == 1
        assert swi.count(prog, "p(4)") == 0
    
    def test_count_conjunction(self, swi):
        """Test counting solutions for conjunctions."""
        prog = """
        p(1). p(2).
        q(a). q(b).
        """
        # All combinations
        assert swi.count(prog, "p(X), q(Y)") == 4
        # Specific binding
        assert swi.count(prog, "p(1), q(Y)") == 2
    
    def test_count_disjunction(self, swi):
        """Test counting solutions for disjunctions."""
        prog = "p(1). q(2)."
        assert swi.count(prog, "(p(X) ; q(X))") == 2
    
    def test_count_cut(self, swi):
        """Test that cut affects solution count."""
        prog = "p(1). p(2). p(3)."
        # Without cut: all solutions
        assert swi.count(prog, "p(X)") == 3
        # With cut: only first
        assert swi.count(prog, "p(X), !") == 1
    
    def test_onevar_atoms(self, swi):
        """Test extracting atom values."""
        prog = "color(red). color(green). color(blue)."
        values = swi.onevar(prog, "color(X)", "X")
        assert values == ["red", "green", "blue"]
    
    def test_onevar_numbers(self, swi):
        """Test extracting number values."""
        prog = "num(1). num(2). num(3)."
        values = swi.onevar(prog, "num(X)", "X")
        assert values == ["1", "2", "3"]
    
    def test_onevar_empty(self, swi):
        """Test extracting from failed goals."""
        prog = "p(1)."
        values = swi.onevar(prog, "p(2)", "X")
        assert values == []
    
    def test_onevar_builtin_member(self, swi):
        """Test member/2 builtin."""
        # No program needed for builtins
        values = swi.onevar("", "member(X, [a, b, c])", "X")
        assert values == ["a", "b", "c"]
    
    def test_catch_throw_baseline(self, swi):
        """Test catch/throw behavior matches SWI."""
        prog = ""
        # Caught exception succeeds
        assert swi.count(prog, "catch(throw(ball), ball, true)") == 1
        # Caught with unification
        assert swi.count(prog, "catch(throw(error(type, context)), error(X, Y), true)") == 1
    
    def test_once_baseline(self, swi):
        """Test once/1 behavior."""
        prog = "p(1). p(2). p(3)."
        # once/1 commits to first solution
        assert swi.count(prog, "once(p(X))") == 1
        values = swi.onevar(prog, "once(p(X))", "X")
        assert values == ["1"]