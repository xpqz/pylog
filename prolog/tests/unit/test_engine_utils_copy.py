"""Unit tests for prolog.engine.utils.copy module."""

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.engine.utils.copy import (
    copy_term_with_fresh_vars,
    copy_term_recursive,
    build_prolog_list,
)
from prolog.unify.store import Store


class TestCopyTermWithFreshVars:
    """Test copy_term_with_fresh_vars function."""

    def test_copy_atom_unchanged(self):
        """Test copying atom returns identical atom."""
        store = Store()
        atom = Atom("test")

        result = copy_term_with_fresh_vars(store, atom)

        assert result is atom  # Atoms are immutable, can be same object

    def test_copy_integer_unchanged(self):
        """Test copying integer returns identical integer."""
        store = Store()
        integer = Int(42)

        result = copy_term_with_fresh_vars(store, integer)

        assert result is integer  # Integers are immutable

    def test_copy_variable_gets_fresh_id(self):
        """Test copying variable creates new variable with fresh ID."""
        store = Store()
        original_var = Var(1, "X")

        result = copy_term_with_fresh_vars(store, original_var)

        assert isinstance(result, Var)
        assert result.id != original_var.id  # Should have fresh ID
        assert result.hint == original_var.hint  # Should preserve hint

    def test_copy_structure_with_fresh_vars(self):
        """Test copying structure with variables creates fresh variables."""
        store = Store()
        struct = Struct("foo", (Var(1, "X"), Atom("bar"), Var(2, "Y")))

        result = copy_term_with_fresh_vars(store, struct)

        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 3

        # First argument should be fresh variable
        assert isinstance(result.args[0], Var)
        assert result.args[0].id != 1
        assert result.args[0].hint == "X"

        # Second argument should be unchanged atom
        assert result.args[1] is struct.args[1]

        # Third argument should be fresh variable
        assert isinstance(result.args[2], Var)
        assert result.args[2].id != 2
        assert result.args[2].hint == "Y"

    def test_copy_list_with_fresh_vars(self):
        """Test copying list with variables creates fresh variables."""
        store = Store()
        original_list = PrologList((Var(1, "X"), Atom("a")), Var(2, "Tail"))

        result = copy_term_with_fresh_vars(store, original_list)

        assert isinstance(result, PrologList)
        assert len(result.items) == 2

        # Items should have fresh variables
        assert isinstance(result.items[0], Var)
        assert result.items[0].id != 1
        assert result.items[0].hint == "X"

        # Tail should have fresh variable
        assert isinstance(result.tail, Var)
        assert result.tail.id != 2
        assert result.tail.hint == "Tail"


class TestCopyTermRecursive:
    """Test copy_term_recursive function."""

    def test_copy_with_existing_mapping(self):
        """Test copying with pre-existing variable mapping."""
        store = Store()
        var_mapping = {1: 10, 2: 20}  # Map old var IDs to new ones
        struct = Struct("test", (Var(1, "X"), Var(2, "Y")))

        result = copy_term_recursive(struct, var_mapping, store)

        assert isinstance(result, Struct)
        assert result.functor == "test"
        assert isinstance(result.args[0], Var)
        assert result.args[0].id == 10  # Should use mapped ID
        assert isinstance(result.args[1], Var)
        assert result.args[1].id == 20  # Should use mapped ID

    def test_copy_creates_new_mapping_entries(self):
        """Test that copying creates new mapping entries for unmapped variables."""
        store = Store()
        var_mapping = {}
        var = Var(5, "Z")

        result = copy_term_recursive(var, var_mapping, store)

        assert isinstance(result, Var)
        assert result.id != 5  # Should have new ID
        assert result.hint == "Z"
        assert 5 in var_mapping  # Should have created mapping entry
        assert var_mapping[5] == result.id

    def test_copy_consistent_mapping(self):
        """Test that same variable gets same mapping across multiple references."""
        store = Store()
        var_mapping = {}
        # Structure with same variable referenced twice
        struct = Struct("foo", (Var(3, "X"), Var(3, "X")))

        result = copy_term_recursive(struct, var_mapping, store)

        assert isinstance(result, Struct)
        assert result.args[0].id == result.args[1].id  # Should map to same ID
        assert len(var_mapping) == 1  # Should only create one mapping


class TestBuildPrologList:
    """Test build_prolog_list function."""

    def test_build_empty_list(self):
        """Test building empty list returns empty atom."""
        result = build_prolog_list([])

        assert isinstance(result, Atom)
        assert result.name == "[]"

    def test_build_single_item_list(self):
        """Test building single-item list."""
        items = [Atom("hello")]

        result = build_prolog_list(items)

        assert isinstance(result, PrologList)
        assert len(result.items) == 1
        assert result.items[0].name == "hello"
        assert isinstance(result.tail, Atom)
        assert result.tail.name == "[]"

    def test_build_multi_item_list(self):
        """Test building multi-item list."""
        items = [Atom("a"), Int(1), Atom("b")]

        result = build_prolog_list(items)

        assert isinstance(result, PrologList)
        assert len(result.items) == 1  # Only first item in items tuple
        assert result.items[0].name == "a"

        # Rest should be nested in tail
        assert isinstance(result.tail, PrologList)
        assert result.tail.items[0].value == 1

        # Final tail should be proper list
        current = result
        count = 0
        while isinstance(current.tail, PrologList) and count < 10:
            current = current.tail
            count += 1
        assert isinstance(current.tail, Atom)
        assert current.tail.name == "[]"

    def test_build_list_preserves_term_types(self):
        """Test that building list preserves all term types."""
        var = Var(1, "X")
        struct = Struct("foo", (Int(42),))
        items = [var, struct, Atom("end")]

        result = build_prolog_list(items)

        # Verify first item is the variable
        assert isinstance(result, PrologList)
        assert isinstance(result.items[0], Var)
        assert result.items[0].id == var.id

        # Navigate to structure
        tail1 = result.tail
        assert isinstance(tail1, PrologList)
        assert isinstance(tail1.items[0], Struct)
        assert tail1.items[0].functor == "foo"
