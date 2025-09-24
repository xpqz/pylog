"""Property tests for attributed variables - Issue #114.

Tests confluence, associativity, and correctness properties of the
attributed variables system using Hypothesis.
"""

import pytest
from hypothesis import given, strategies as st, assume, settings
from hypothesis.stateful import RuleBasedStateMachine, rule, invariant, precondition

from prolog.ast.terms import Atom, Int, Var
from prolog.unify.store import Store
from prolog.unify.unify import unify
from prolog.unify.trail import undo_to
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program


class TestAttributeConfluence:
    """Test that attribute operations are confluent."""

    @given(
        module1=st.text(min_size=1, max_size=10, alphabet='abcdefghijklmnopqrstuvwxyz'),
        module2=st.text(min_size=1, max_size=10, alphabet='abcdefghijklmnopqrstuvwxyz'),
        value1=st.integers(min_value=0, max_value=100),
        value2=st.integers(min_value=0, max_value=100),
    )
    def test_put_attr_order_independent(self, module1, module2, value1, value2):
        """Order of put_attr on different modules shouldn't affect final state."""
        assume(module1 != module2)  # Different modules

        program = Program(())

        # Scenario 1: module1 then module2
        engine1 = Engine(program)
        query1 = f"?- put_attr(X, {module1}, {value1}), put_attr(X, {module2}, {value2}), " \
                f"get_attr(X, {module1}, V1), get_attr(X, {module2}, V2)."
        solutions1 = list(engine1.query(query1))

        # Scenario 2: module2 then module1
        engine2 = Engine(program)
        query2 = f"?- put_attr(X, {module2}, {value2}), put_attr(X, {module1}, {value1}), " \
                f"get_attr(X, {module1}, V1), get_attr(X, {module2}, V2)."
        solutions2 = list(engine2.query(query2))

        # Both should give the same result
        assert len(solutions1) == len(solutions2) == 1
        assert solutions1[0]["V1"] == solutions2[0]["V1"] == Int(value1)
        assert solutions1[0]["V2"] == solutions2[0]["V2"] == Int(value2)

    @given(
        module=st.text(min_size=1, max_size=10, alphabet='abcdefghijklmnopqrstuvwxyz'),
        values=st.lists(st.integers(min_value=0, max_value=100), min_size=2, max_size=5)
    )
    def test_last_put_attr_wins(self, module, values):
        """Multiple put_attr on same module should have last value win."""
        program = Program(())
        engine = Engine(program)

        # Build query with multiple put_attr calls
        put_calls = " ".join(f"put_attr(X, {module}, {v})," for v in values)
        query = f"?- {put_calls} get_attr(X, {module}, V)."

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["V"] == Int(values[-1])  # Last value should win


class TestAliasingAssociativity:
    """Test that aliasing with attributes is associative."""

    def test_aliasing_chain_associative(self):
        """(X=Y)=Z should be equivalent to X=(Y=Z) with attributes."""
        store = Store()

        # Create three variables
        x_id = store.new_var("X")
        y_id = store.new_var("Y")
        z_id = store.new_var("Z")

        # Add attributes to each
        if not hasattr(store, 'attrs'):
            store.attrs = {}
        store.attrs[x_id] = {"a": Int(1)}
        store.attrs[y_id] = {"b": Int(2)}
        store.attrs[z_id] = {"c": Int(3)}

        # Scenario 1: (X=Y) then (result=Z)
        store1 = Store()
        x1 = store1.new_var("X")
        y1 = store1.new_var("Y")
        z1 = store1.new_var("Z")
        store1.attrs = {}
        store1.attrs[x1] = {"a": Int(1)}
        store1.attrs[y1] = {"b": Int(2)}
        store1.attrs[z1] = {"c": Int(3)}
        trail1 = []

        assert unify(Var(x1, "X"), Var(y1, "Y"), store1, trail1)
        assert unify(Var(x1, "X"), Var(z1, "Z"), store1, trail1)

        root1 = store1.deref(x1)[1]
        attrs1 = store1.attrs.get(root1, {})

        # Scenario 2: (Y=Z) then (X=result)
        store2 = Store()
        x2 = store2.new_var("X")
        y2 = store2.new_var("Y")
        z2 = store2.new_var("Z")
        store2.attrs = {}
        store2.attrs[x2] = {"a": Int(1)}
        store2.attrs[y2] = {"b": Int(2)}
        store2.attrs[z2] = {"c": Int(3)}
        trail2 = []

        assert unify(Var(y2, "Y"), Var(z2, "Z"), store2, trail2)
        assert unify(Var(x2, "X"), Var(y2, "Y"), store2, trail2)

        root2 = store2.deref(x2)[1]
        attrs2 = store2.attrs.get(root2, {})

        # Both should have all three attributes
        assert set(attrs1.keys()) == set(attrs2.keys()) == {"a", "b", "c"}
        assert attrs1["a"] == attrs2["a"] == Int(1)
        assert attrs1["b"] == attrs2["b"] == Int(2)
        assert attrs1["c"] == attrs2["c"] == Int(3)


class TestBacktrackingCompleteness:
    """Test that backtracking always restores exact state."""

    @given(
        modules=st.lists(
            st.text(min_size=1, max_size=5, alphabet='abcdefghijklmnopqrstuvwxyz'),
            min_size=1, max_size=3, unique=True
        ),
        values=st.lists(st.integers(min_value=0, max_value=100), min_size=1, max_size=3)
    )
    def test_backtrack_restores_attributes(self, modules, values):
        """Backtracking should restore exact attribute state."""
        # Ensure we have same number of modules and values
        min_len = min(len(modules), len(values))
        modules = modules[:min_len]
        values = values[:min_len]

        store = Store()
        trail = []

        # Create a variable
        var_id = store.new_var("X")

        # Add attributes
        if not hasattr(store, 'attrs'):
            store.attrs = {}

        # Save original state (no attributes)
        mark = len(trail)

        # Add attributes with trailing
        for module, value in zip(modules, values):
            if var_id not in store.attrs:
                store.attrs[var_id] = {}
            # Simulate proper trailing
            old_value = store.attrs[var_id].get(module)
            trail.append(('attr', var_id, module, old_value))
            store.attrs[var_id][module] = Int(value)

        # Verify attributes are set
        assert var_id in store.attrs
        assert len(store.attrs[var_id]) == len(modules)

        # Undo to mark
        undo_to(mark, trail, store)

        # Should have no attributes
        assert var_id not in store.attrs or not store.attrs.get(var_id)

    def test_complex_backtrack_scenario(self):
        """Test backtracking through complex attribute operations."""
        program = Program(())
        engine = Engine(program)

        # Complex query with backtracking
        query = """?- put_attr(X, test, 1),
                     put_attr(Y, test, 2),
                     ((X = Y, fail) ;
                      (get_attr(X, test, V1), get_attr(Y, test, V2)))."""

        # Register hook that allows merging
        engine.register_attr_hook("test", lambda e, v, o: True)

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["V1"] == Int(1)
        assert solutions[0]["V2"] == Int(2)


@settings(max_examples=50)
class AttributeStateMachine(RuleBasedStateMachine):
    """Stateful testing of attributed variables."""

    def __init__(self):
        super().__init__()
        self.store = Store()
        self.trail = []
        self.vars = {}
        self.expected_attrs = {}
        if not hasattr(self.store, 'attrs'):
            self.store.attrs = {}

    @rule(var_name=st.text(min_size=1, max_size=3, alphabet='XYZ'))
    def create_var(self, var_name):
        """Create a new variable."""
        if var_name not in self.vars:
            var_id = self.store.new_var(var_name)
            self.vars[var_name] = var_id
            self.expected_attrs[var_id] = {}

    @rule(
        var_name=st.text(min_size=1, max_size=3, alphabet='XYZ'),
        module=st.text(min_size=1, max_size=5, alphabet='abc'),
        value=st.integers(min_value=0, max_value=10)
    )
    def put_attribute(self, var_name, module, value):
        """Put an attribute on a variable."""
        if var_name not in self.vars:
            self.create_var(var_name)

        var_id = self.vars[var_name]
        root = self.store.deref(var_id)[1]

        # Update store
        if root not in self.store.attrs:
            self.store.attrs[root] = {}

        # Trail the change
        old_value = self.store.attrs[root].get(module)
        self.trail.append(('attr', root, module, old_value))

        # Set new value
        self.store.attrs[root][module] = Int(value)

        # Update expected attributes at the root
        if root not in self.expected_attrs:
            self.expected_attrs[root] = {}
        self.expected_attrs[root][module] = Int(value)

    @rule(
        var1=st.text(min_size=1, max_size=3, alphabet='XYZ'),
        var2=st.text(min_size=1, max_size=3, alphabet='XYZ')
    )
    def unify_vars(self, var1, var2):
        """Unify two variables."""
        if var1 not in self.vars:
            self.create_var(var1)
        if var2 not in self.vars:
            self.create_var(var2)

        if var1 != var2:
            v1 = Var(self.vars[var1], var1)
            v2 = Var(self.vars[var2], var2)

            # Get current roots before unification
            root1_before = self.store.deref(self.vars[var1])[1]
            root2_before = self.store.deref(self.vars[var2])[1]

            # Attempt unification
            mark = len(self.trail)
            if unify(v1, v2, self.store, self.trail):
                # After unification, find the new root
                root_after = self.store.deref(self.vars[var1])[1]

                # Merge expected attributes - take the union, but for overlapping
                # modules, the root's value should win (as per implementation)
                attrs1 = self.expected_attrs.get(root1_before, {})
                attrs2 = self.expected_attrs.get(root2_before, {})

                # Determine which was the root (winner)
                if root_after == root1_before:
                    # root1 won, so its values take precedence
                    merged = dict(attrs2)  # Start with attrs2
                    merged.update(attrs1)  # Overwrite with attrs1
                else:
                    # root2 won, so its values take precedence
                    merged = dict(attrs1)  # Start with attrs1
                    merged.update(attrs2)  # Overwrite with attrs2

                # Update expected for the new root
                self.expected_attrs[root_after] = merged
                # Clear expected for non-roots
                for vid in [root1_before, root2_before]:
                    if vid != root_after:
                        self.expected_attrs[vid] = {}

    @invariant()
    def attributes_consistent(self):
        """Check that actual attributes match expected."""
        for var_name, var_id in self.vars.items():
            root = self.store.deref(var_id)[1]
            actual = self.store.attrs.get(root, {})

            # Find expected through root
            expected = {}
            for vid, attrs in self.expected_attrs.items():
                if self.store.deref(vid)[1] == root:
                    expected.update(attrs)
                    break

            # Non-overlapping modules should all be present
            for module in set(actual.keys()) | set(expected.keys()):
                if module in actual and module in expected:
                    assert actual[module] == expected[module], \
                        f"Mismatch for {var_name} module {module}"


# Create the test case from the state machine
TestAttributeStateMachine = AttributeStateMachine.TestCase