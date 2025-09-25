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

    def test_single_value_domain_order(self):
        """Test that single-value domain unifies before continuation.

        Regression test for push order issue where LABEL_CONTINUE
        was pushed after unify goal, causing it to run first.
        """
        from prolog.clpfd.label import push_labeling_choices
        from prolog.clpfd.domain import Domain
        from prolog.clpfd.api import set_domain

        engine = Engine(Program([]))

        # Create two variables with singleton domains
        x = engine.store.new_var("X")
        y = engine.store.new_var("Y")

        # Set singleton domains
        x_deref = engine.store.deref(x)
        y_deref = engine.store.deref(y)

        set_domain(engine.store, x_deref[1], Domain(((5, 5),)), engine.trail)
        set_domain(engine.store, y_deref[1], Domain(((7, 7),)), engine.trail)

        # Push labeling choices
        push_labeling_choices(engine, [x, y], "first", "indomain_min")

        # Check that unify goal is on top (will execute first)
        # and continuation is below it
        goal_stack = engine.goal_stack._stack
        assert len(goal_stack) >= 1

        # The top goal should be the unification (X = 5)
        from prolog.engine.runtime import GoalType

        top_goal = goal_stack[-1]
        assert top_goal.type == GoalType.BUILTIN
        assert top_goal.term.functor == "="

        # If there's a continuation, it should be below the unify
        if len(goal_stack) > 1:
            next_goal = goal_stack[-2]
            if next_goal.type == GoalType.CONTROL:
                assert next_goal.payload.get('op') == 'LABEL_CONTINUE'

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

        # Test indomain_split ordering
        split_values = select_values(Domain(((1, 7),)), "indomain_split")
        assert split_values[0] == 4  # Middle value first
        # Then alternates: 5, 3, 6, 2, 7, 1
        assert split_values == [4, 5, 3, 6, 2, 7, 1]

    def test_mixed_strategies(self):
        """Test combination of first_fail and indomain_max strategies."""
        from prolog.clpfd.label import parse_var_selection, parse_value_selection
        from prolog.clpfd.label import select_variable, select_values
        from prolog.clpfd.domain import Domain
        from prolog.clpfd.api import set_domain

        engine = Engine(Program([]))

        # Parse mixed strategies
        options = List([Atom("first_fail"), Atom("indomain_max")])
        var_select = parse_var_selection(options)
        val_select = parse_value_selection(options)

        assert var_select == "first_fail"
        assert val_select == "indomain_max"

        # Create variables with different domain sizes
        v1 = engine.store.new_var("V1")
        v2 = engine.store.new_var("V2")
        v3 = engine.store.new_var("V3")

        v1_deref = engine.store.deref(v1)
        v2_deref = engine.store.deref(v2)
        v3_deref = engine.store.deref(v3)

        # V1 has domain 1..10 (size 10)
        # V2 has domain 1..2 (size 2) - should be selected first
        # V3 has domain 1..5 (size 5)
        set_domain(engine.store, v1_deref[1], Domain(((1, 10),)), engine.trail)
        set_domain(engine.store, v2_deref[1], Domain(((1, 2),)), engine.trail)
        set_domain(engine.store, v3_deref[1], Domain(((1, 5),)), engine.trail)

        # Test first_fail selection
        unbound = [
            (v1_deref[1], Domain(((1, 10),))),
            (v2_deref[1], Domain(((1, 2),))),
            (v3_deref[1], Domain(((1, 5),)))
        ]

        selected_var, selected_dom = select_variable(unbound, "first_fail", engine)
        assert selected_var == v2_deref[1]  # V2 has smallest domain

        # Test indomain_max on selected domain
        values = select_values(selected_dom, "indomain_max")
        assert values == [2, 1]  # Max value first