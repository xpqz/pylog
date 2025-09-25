"""Simple tests for CLP(FD) labeling - Phase 5 of Issue #123.

Basic tests that don't rely on complex strategies.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine, Program
from prolog.clpfd.label import _builtin_label, _builtin_labeling


class TestBasicLabelingBuiltin:
    """Test the labeling builtin functions directly."""

    def test_label_empty_list(self):
        """Label empty list should succeed."""
        engine = Engine(Program([]))
        result = _builtin_label(engine, List([]))
        assert result is True

    def test_label_with_already_bound_var(self):
        """Label with already bound variable should succeed."""
        from prolog.unify.unify import bind

        engine = Engine(Program([]))
        x = Var(engine.store.new_var(), "X")

        # Bind X
        bind(engine.store, x.id, Int(5), engine.trail)

        # Should succeed since X is already bound
        result = _builtin_label(engine, List([x]))
        assert result is True

    def test_labeling_with_options(self):
        """Labeling with options should parse correctly."""
        engine = Engine(Program([]))

        # Test with some options
        options = List([Atom("first"), Atom("indomain_min")])
        vars = List([])

        result = _builtin_labeling(engine, options, vars)
        assert result is True


class TestLabelingHelpers:
    """Test helper functions for labeling."""

    def test_parse_var_selection(self):
        """Test variable selection strategy parsing."""
        from prolog.clpfd.label import parse_var_selection

        # Default strategy
        assert parse_var_selection(List([])) == "first"

        # Explicit first
        assert parse_var_selection(List([Atom("first")])) == "first"

        # First fail
        assert parse_var_selection(List([Atom("first_fail")])) == "first_fail"

        # Most constrained
        assert parse_var_selection(List([Atom("most_constrained")])) == "most_constrained"

    def test_parse_value_selection(self):
        """Test value selection strategy parsing."""
        from prolog.clpfd.label import parse_value_selection

        # Default strategy
        assert parse_value_selection(List([])) == "indomain_min"

        # Explicit min
        assert parse_value_selection(List([Atom("indomain_min")])) == "indomain_min"

        # Max strategy
        assert parse_value_selection(List([Atom("indomain_max")])) == "indomain_max"

        # Middle strategy
        assert parse_value_selection(List([Atom("indomain_middle")])) == "indomain_middle"

    def test_extract_var_list(self):
        """Test variable list extraction."""
        from prolog.clpfd.label import extract_var_list

        engine = Engine(Program([]))

        # Empty list
        assert extract_var_list(List([])) == []

        # List with variables
        x = Var(engine.store.new_var(), "X")
        y = Var(engine.store.new_var(), "Y")

        var_ids = extract_var_list(List([x, y]))
        assert var_ids == [x.id, y.id]

        # Mixed list (only variables extracted)
        var_ids = extract_var_list(List([x, Int(5), y, Atom("foo")]))
        assert var_ids == [x.id, y.id]

    def test_select_values(self):
        """Test value selection from domain."""
        from prolog.clpfd.label import select_values
        from prolog.clpfd.domain import Domain

        # Create domain 1..5
        domain = Domain(((1, 5),))

        # Min strategy
        values = select_values(domain, "indomain_min")
        assert values == [1, 2, 3, 4, 5]

        # Max strategy
        values = select_values(domain, "indomain_max")
        assert values == [5, 4, 3, 2, 1]

        # Middle strategy
        values = select_values(domain, "indomain_middle")
        assert values[0] == 3  # Middle value first

        # Empty domain
        empty_domain = Domain(())
        values = select_values(empty_domain, "indomain_min")
        assert values == []

        # Domain with holes: {1, 3, 5}
        holey_domain = Domain(((1, 1), (3, 3), (5, 5)))
        values = select_values(holey_domain, "indomain_min")
        assert values == [1, 3, 5]