"""Unit tests for element/3 constraint."""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, List, Struct, Atom
from prolog.clpfd.api import get_domain, set_domain, iter_watchers
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import (
    _builtin_in,
    _builtin_element_3,
    _builtin_all_different,
)


class TestElementBasic:
    """Test basic element/3 functionality."""

    def test_element_ground_index_ground_value(self):
        """element(2, [1,2,3], 2) should succeed."""
        engine = Engine(Program([]))

        # element(2, [1,2,3], 2)
        list_term = List([Int(1), Int(2), Int(3)])
        result = _builtin_element_3(engine, Int(2), list_term, Int(2))
        assert result is True

    def test_element_ground_index_wrong_value(self):
        """element(2, [1,2,3], 5) should fail."""
        engine = Engine(Program([]))

        # element(2, [1,2,3], 5)
        list_term = List([Int(1), Int(2), Int(3)])
        result = _builtin_element_3(engine, Int(2), list_term, Int(5))
        assert result is False

    def test_element_ground_index_variable_value(self):
        """element(2, [1,2,3], Z) should constrain Z to 2."""
        engine = Engine(Program([]))
        store = engine.store

        z = Var(store.new_var(), "Z")
        _builtin_in(engine, z, Struct("..", (Int(1), Int(10))))

        # element(2, [1,2,3], Z)
        list_term = List([Int(1), Int(2), Int(3)])
        result = _builtin_element_3(engine, Int(2), list_term, z)
        assert result is True

        # Z should be constrained to singleton domain {2}
        z_dom = get_domain(store, z.id)
        assert z_dom.is_singleton()
        assert z_dom.min() == 2

    def test_element_variable_index_ground_value(self):
        """element(I, [1,2,3,2], 2) should constrain I to {2,4}."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        _builtin_in(engine, i, Struct("..", (Int(1), Int(5))))

        # element(I, [1,2,3,2], 2)
        list_term = List([Int(1), Int(2), Int(3), Int(2)])
        result = _builtin_element_3(engine, i, list_term, Int(2))
        assert result is True

        # I should be constrained to indices where the value is 2: {2,4}
        i_dom = get_domain(store, i.id)
        assert i_dom.contains(2)
        assert i_dom.contains(4)
        assert not i_dom.contains(1)
        assert not i_dom.contains(3)
        assert not i_dom.contains(5)

    def test_element_with_variable_list_elements(self):
        """element(2, [X,Y,Z], 5) should constrain Y to 5."""
        engine = Engine(Program([]))
        store = engine.store

        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")

        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(10))))

        # element(2, [X,Y,Z], 5)
        list_term = List([x, y, z])
        result = _builtin_element_3(engine, Int(2), list_term, Int(5))
        assert result is True

        # Y (index 2) should be constrained to 5
        y_dom = get_domain(store, y.id)
        assert y_dom.is_singleton()
        assert y_dom.min() == 5

        # X and Z should be unchanged
        x_dom = get_domain(store, x.id)
        z_dom = get_domain(store, z.id)
        assert x_dom.intervals == ((1, 10),)
        assert z_dom.intervals == ((1, 10),)

    def test_element_all_variables_basic_propagation(self):
        """element(I, [X,Y,Z], V) with constrained domains."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")
        v = Var(store.new_var(), "V")

        # Set domains
        _builtin_in(engine, i, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))  # List[1]
        _builtin_in(engine, y, Struct("..", (Int(3), Int(7))))  # List[2]
        _builtin_in(engine, z, Struct("..", (Int(6), Int(10))))  # List[3]
        _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))

        # element(I, [X,Y,Z], V)
        list_term = List([x, y, z])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # V should be constrained to union of possible values: {1..5} ∪ {3..7} ∪ {6..10} = {1..10}
        # But this should be refined to the actual intersection based on I's domain
        # The exact result depends on the propagation algorithm implementation

    def test_element_index_out_of_bounds(self):
        """element(5, [1,2,3], V) should fail (index too large)."""
        engine = Engine(Program([]))

        v = Var(engine.store.new_var(), "V")
        list_term = List([Int(1), Int(2), Int(3)])
        result = _builtin_element_3(engine, Int(5), list_term, v)
        assert result is False

    def test_element_index_zero(self):
        """element(0, [1,2,3], V) should fail (index too small, 1-indexed)."""
        engine = Engine(Program([]))

        v = Var(engine.store.new_var(), "V")
        list_term = List([Int(1), Int(2), Int(3)])
        result = _builtin_element_3(engine, Int(0), list_term, v)
        assert result is False

    def test_element_empty_list(self):
        """element(I, [], V) should always fail."""
        engine = Engine(Program([]))

        i = Var(engine.store.new_var(), "I")
        v = Var(engine.store.new_var(), "V")
        result = _builtin_element_3(engine, i, List([]), v)
        assert result is False

    def test_element_singleton_index_domain(self):
        """element(I, [1,2,3], V) where I in {2} should constrain V to 2."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        v = Var(store.new_var(), "V")

        # I is constrained to singleton domain {2}
        _builtin_in(engine, i, Int(2))
        _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))

        # element(I, [1,2,3], V)
        list_term = List([Int(1), Int(2), Int(3)])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # V should be constrained to the value at index 2, which is 2
        v_dom = get_domain(store, v.id)
        assert v_dom.is_singleton()
        assert v_dom.min() == 2

    def test_element_singleton_value_domain(self):
        """element(I, [1,2,3,2], V) where V in {2} should constrain I to {2,4}."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        v = Var(store.new_var(), "V")

        _builtin_in(engine, i, Struct("..", (Int(1), Int(5))))
        # V is constrained to singleton domain {2}
        _builtin_in(engine, v, Int(2))

        # element(I, [1,2,3,2], V)
        list_term = List([Int(1), Int(2), Int(3), Int(2)])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # I should be constrained to indices where the value is 2: {2,4}
        i_dom = get_domain(store, i.id)
        assert i_dom.contains(2)
        assert i_dom.contains(4)
        assert not i_dom.contains(1)
        assert not i_dom.contains(3)


class TestElementPropagation:
    """Test element/3 domain propagation."""

    def test_index_domain_pruning_on_value_change(self):
        """When value domain changes, index domain should be pruned."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")
        v = Var(store.new_var(), "V")
        trail = engine.trail

        # Set initial domains
        _builtin_in(engine, i, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))  # List[1]
        _builtin_in(engine, y, Struct("..", (Int(5), Int(7))))  # List[2]
        _builtin_in(engine, z, Struct("..", (Int(8), Int(10))))  # List[3]
        _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))

        # Post element(I, [X,Y,Z], V)
        list_term = List([x, y, z])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # Now narrow V's domain to exclude Y and Z's ranges
        # This should prune I to only include index 1
        set_domain(store, v.id, Domain(((1, 3),)), trail)

        # Trigger propagation manually (in real system this would be automatic)
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, v.id):
            queue.schedule(pid, priority, cause=("domain_changed", v.id))
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # I should be pruned to only {1} since only X's domain intersects with V's
        i_dom = get_domain(store, i.id)
        assert i_dom.is_singleton()
        assert i_dom.min() == 1

    def test_value_domain_pruning_on_index_change(self):
        """When index domain changes, value domain should be pruned."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")
        v = Var(store.new_var(), "V")
        trail = engine.trail

        # Set initial domains
        _builtin_in(engine, i, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))  # List[1]
        _builtin_in(engine, y, Struct("..", (Int(5), Int(7))))  # List[2]
        _builtin_in(engine, z, Struct("..", (Int(8), Int(10))))  # List[3]
        _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))

        # Post element(I, [X,Y,Z], V)
        list_term = List([x, y, z])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # Now narrow I's domain to only index 2
        set_domain(store, i.id, Domain(((2, 2),)), trail)

        # Trigger propagation manually
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, i.id):
            queue.schedule(pid, priority, cause=("domain_changed", i.id))
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # V should be pruned to intersect with Y's domain: {5,6,7}
        v_dom = get_domain(store, v.id)
        assert v_dom.intervals == ((5, 7),)

    def test_list_element_domain_change_propagation(self):
        """When a list element's domain changes, it should trigger propagation."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        v = Var(store.new_var(), "V")
        trail = engine.trail

        # Set initial domains
        _builtin_in(engine, i, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))  # List[1]
        _builtin_in(engine, y, Struct("..", (Int(3), Int(8))))  # List[2]
        _builtin_in(engine, v, Struct("..", (Int(1), Int(8))))

        # Post element(I, [X,Y], V)
        list_term = List([x, y])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # Now narrow X's domain
        set_domain(store, x.id, Domain(((1, 2),)), trail)

        # Trigger propagation manually
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, x.id):
            queue.schedule(pid, priority, cause=("domain_changed", x.id))
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

        # V should be pruned to union of X and Y domains: {1,2} ∪ {3..8} = {1,2} ∪ {3..8}
        # The exact intervals depend on implementation, but should include both ranges

    def test_failure_on_empty_intersection(self):
        """element/3 should fail when domains have no valid intersection."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        v = Var(store.new_var(), "V")

        # Set domains that will create conflict
        _builtin_in(engine, i, Int(1))  # Index must be 1
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))  # List[1] in 1..5
        _builtin_in(
            engine, v, Struct("..", (Int(10), Int(15)))
        )  # V in 10..15 (no overlap)

        # element(I, [X], V) should fail due to empty intersection
        list_term = List([x])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is False

    def test_backtracking_restoration(self):
        """Domains should be restored on backtracking."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        v = Var(store.new_var(), "V")
        trail = engine.trail

        # Set domains - use 2-element list so index 2 is valid
        _builtin_in(engine, i, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(10))))
        _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))

        # Post element(I, [X,Y], V)
        list_term = List([x, y])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # Mark trail position
        mark = trail.position()

        # Make constraining change - narrow I to index 1 only
        set_domain(store, i.id, Domain(((1, 1),)), trail)

        # Trigger propagation
        queue = engine.clpfd_queue
        for pid, priority in iter_watchers(store, i.id):
            queue.schedule(pid, priority, cause=("domain_changed", i.id))
        queue.run_to_fixpoint(store, trail, engine)

        # Backtrack
        trail.unwind_to(mark, store)

        # Domains should be restored
        i_dom = get_domain(store, i.id)
        v_dom = get_domain(store, v.id)
        assert i_dom.intervals == ((1, 2),)
        assert v_dom.intervals == ((1, 10),)

    def test_no_infinite_propagation_loop(self):
        """element/3 should not create infinite propagation loops."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create circular dependency potential
        i = Var(store.new_var(), "I")
        j = Var(store.new_var(), "J")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        _builtin_in(engine, i, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, j, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))

        # Post element(I, [X,Y], J) and element(J, [X,Y], I)
        # This creates potential for circular propagation
        list_term = List([x, y])
        result1 = _builtin_element_3(engine, i, list_term, j)
        result2 = _builtin_element_3(engine, j, list_term, i)
        assert result1 is True
        assert result2 is True

        # Trigger propagation - should reach fixpoint without infinite loop
        queue = engine.clpfd_queue
        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True  # Should terminate


class TestElementEdgeCases:
    """Test edge cases for element/3."""

    def test_element_non_list_second_argument(self):
        """element/3 with non-list second argument should fail."""
        engine = Engine(Program([]))

        i = Var(engine.store.new_var(), "I")
        v = Var(engine.store.new_var(), "V")

        # Second argument is not a list
        result = _builtin_element_3(engine, i, Int(5), v)
        assert result is False

        # Second argument is atom
        result = _builtin_element_3(engine, i, Atom("not_a_list"), v)
        assert result is False

    def test_element_non_integer_list_elements(self):
        """element/3 with non-integer list elements should fail."""
        engine = Engine(Program([]))

        i = Var(engine.store.new_var(), "I")
        v = Var(engine.store.new_var(), "V")

        # List contains non-integer element
        list_term = List([Int(1), Atom("atom"), Int(3)])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is False

    def test_element_large_list(self):
        """element/3 should work with reasonably large lists."""
        engine = Engine(Program([]))
        store = engine.store

        # Create list of 100 elements
        large_list = List([Int(i) for i in range(1, 101)])

        i = Var(store.new_var(), "I")
        v = Var(store.new_var(), "V")

        _builtin_in(engine, i, Struct("..", (Int(1), Int(100))))
        _builtin_in(engine, v, Struct("..", (Int(1), Int(100))))

        # Should handle large list efficiently
        result = _builtin_element_3(engine, i, large_list, v)
        assert result is True

    def test_element_with_duplicate_values_in_list(self):
        """element/3 should handle lists with duplicate values correctly."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        v = Var(store.new_var(), "V")

        _builtin_in(engine, i, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, v, Int(3))  # V constrained to 3

        # List has 3 at positions 2 and 4
        list_term = List([Int(1), Int(3), Int(2), Int(3), Int(5)])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # I should be constrained to {2, 4}
        i_dom = get_domain(store, i.id)
        assert i_dom.contains(2)
        assert i_dom.contains(4)
        assert not i_dom.contains(1)
        assert not i_dom.contains(3)
        assert not i_dom.contains(5)


class TestElementIntegration:
    """Integration tests for element/3 with other constraints."""

    def test_element_with_arithmetic_constraints(self):
        """element/3 combined with arithmetic constraints."""
        engine = Engine(Program([]))

        # Test through query interface when available
        # For now, test basic interaction
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        v = Var(store.new_var(), "V")

        _builtin_in(engine, i, Struct("..", (Int(1), Int(2))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(5))))
        _builtin_in(engine, v, Struct("..", (Int(1), Int(10))))

        # Post element(I, [X,Y], V)
        list_term = List([x, y])
        result = _builtin_element_3(engine, i, list_term, v)
        assert result is True

        # Additional constraints can be added here when other builtins are available
        # For example: X + Y #= 6, V #> 3, etc.

    def test_element_with_all_different(self):
        """element/3 combined with all_different constraint."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")
        z = Var(store.new_var(), "Z")
        v = Var(store.new_var(), "V")

        # Set domains
        _builtin_in(engine, i, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, x, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, z, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, v, Struct("..", (Int(1), Int(3))))

        # Post element(I, [X,Y,Z], V)
        list_term = List([x, y, z])
        result1 = _builtin_element_3(engine, i, list_term, v)
        assert result1 is True

        # Post all_different([X,Y,Z])
        result2 = _builtin_all_different(engine, List([x, y, z]))
        assert result2 is True

        # Both constraints should be satisfiable together


class TestElementReification:
    """Test element/3 with reification (when available)."""

    @pytest.mark.xfail(reason="Reification support not yet implemented")
    def test_element_reification_basic(self):
        """Test B #<=> element(I, List, V) reification."""
        engine = Engine(Program([]))
        store = engine.store

        i = Var(store.new_var(), "I")
        v = Var(store.new_var(), "V")

        _builtin_in(engine, i, Struct("..", (Int(1), Int(3))))
        _builtin_in(engine, v, Struct("..", (Int(1), Int(5))))

        # This would test reification when implemented
        # B #<=> element(I, [1,2,3], V)
        # result = _builtin_reify(engine, b, element_constraint)
        # assert result is True

    @pytest.mark.xfail(reason="Reification support not yet implemented")
    def test_element_reification_failure_case(self):
        """Test reification when element constraint fails."""
        # Test reified element constraint that should fail
        # B should be bound to 0 when element constraint is false
        pass


@pytest.mark.swi_baseline
class TestElementSWIBaseline:
    """Test element/3 against SWI-Prolog baseline."""

    def test_element_basic_swi_comparison(self, swi):
        """Compare basic element/3 behavior with SWI-Prolog."""
        program = ":- use_module(library(clpfd))."

        # Test basic element access
        count = swi.count(program, "element(2, [1,2,3], X)")
        assert count == 1

        values = swi.onevar(program, "element(2, [1,2,3], X)", "X")
        assert values == ["2"]

        # Test with bound index and value - should succeed
        count = swi.count(program, "element(2, [1,2,3], 2)")
        assert count == 1

        # Test failure case
        count = swi.count(program, "element(2, [1,2,3], 5)")
        assert count == 0

    def test_element_domain_propagation_swi_comparison(self, swi):
        """Compare domain propagation with SWI-Prolog CLP(FD)."""
        program = ":- use_module(library(clpfd))."
        goal = "I in 1..4, element(I, [1,2,3,2], 2), indomain(I)"

        # Should get I = 2 and I = 4 (positions where value is 2)
        count = swi.count(program, goal)
        assert count == 2

        i_values = swi.onevar(program, goal, "I")
        assert sorted(i_values) == ["2", "4"]
