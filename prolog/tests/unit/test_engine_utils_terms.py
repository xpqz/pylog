"""Unit tests for prolog.engine.utils.terms module."""

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.engine.utils.terms import reify_var, reify_term
from prolog.unify.store import Store
from prolog.unify.unify_helpers import bind_root_to_term
from prolog.engine.runtime import Trail


class TestReifyVar:
    """Test reify_var function."""

    def test_reify_unbound_variable(self):
        """Test reifying an unbound variable returns Var representation."""
        store = Store()
        qname_by_id = {}
        varid = store.new_var("X")

        result = reify_var(store, qname_by_id, varid)

        assert isinstance(result, Var)
        assert result.id == varid
        assert result.hint == f"_{varid}"

    def test_reify_unbound_variable_with_query_name(self):
        """Test reifying unbound variable uses query name when available."""
        store = Store()
        varid = store.new_var("X")
        qname_by_id = {varid: "X"}

        result = reify_var(store, qname_by_id, varid)

        assert isinstance(result, Var)
        assert result.hint == "X"

    def test_reify_bound_variable_to_atom(self):
        """Test reifying variable bound to atom."""
        store = Store()
        trail = Trail()
        qname_by_id = {}
        varid = store.new_var("X")
        atom = Atom("hello")

        bind_root_to_term(varid, atom, trail, store)
        result = reify_var(store, qname_by_id, varid)

        assert isinstance(result, Atom)
        assert result.name == "hello"

    def test_reify_bound_variable_to_structure(self):
        """Test reifying variable bound to structure."""
        store = Store()
        trail = Trail()
        qname_by_id = {}
        varid = store.new_var("X")
        struct = Struct("foo", (Atom("a"), Int(42)))

        bind_root_to_term(varid, struct, trail, store)
        result = reify_var(store, qname_by_id, varid)

        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert len(result.args) == 2


class TestReifyTerm:
    """Test reify_term function."""

    def test_reify_atom(self):
        """Test reifying an atom returns the atom unchanged."""
        store = Store()
        qname_by_id = {}
        atom = Atom("test")

        result = reify_term(store, qname_by_id, atom)

        assert result is atom

    def test_reify_integer(self):
        """Test reifying an integer returns the integer unchanged."""
        store = Store()
        qname_by_id = {}
        integer = Int(42)

        result = reify_term(store, qname_by_id, integer)

        assert result is integer

    def test_reify_unbound_variable(self):
        """Test reifying unbound variable in term context."""
        store = Store()
        qname_by_id = {}
        var = Var(store.new_var("X"), "X")

        result = reify_term(store, qname_by_id, var)

        assert isinstance(result, Var)
        assert result.id == var.id

    def test_reify_structure_with_variables(self):
        """Test reifying structure containing variables."""
        store = Store()
        trail = Trail()
        qname_by_id = {}
        varid = store.new_var("X")
        struct = Struct("foo", (Var(varid, "X"), Atom("bar")))

        # Bind the variable
        bind_root_to_term(varid, Int(123), trail, store)

        result = reify_term(store, qname_by_id, struct)

        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert isinstance(result.args[0], Int)
        assert result.args[0].value == 123
        assert isinstance(result.args[1], Atom)
        assert result.args[1].name == "bar"

    def test_reify_list_flattening(self):
        """Test that nested PrologList structures get flattened."""
        store = Store()
        qname_by_id = {}

        # Create nested list structure: [a | [b | []]]
        inner_list = PrologList((Atom("b"),), Atom("[]"))
        outer_list = PrologList((Atom("a"),), inner_list)

        result = reify_term(store, qname_by_id, outer_list)

        assert isinstance(result, PrologList)
        assert len(result.items) == 2  # Should be flattened to [a, b]
        assert result.items[0].name == "a"
        assert result.items[1].name == "b"
        assert isinstance(result.tail, Atom)
        assert result.tail.name == "[]"

    def test_reify_improper_list_preserves_tail(self):
        """Test that improper lists preserve their non-list tail."""
        store = Store()
        qname_by_id = {}

        # Create improper list: [a | b]
        improper_list = PrologList((Atom("a"),), Atom("b"))

        result = reify_term(store, qname_by_id, improper_list)

        assert isinstance(result, PrologList)
        assert len(result.items) == 1
        assert result.items[0].name == "a"
        assert isinstance(result.tail, Atom)
        assert result.tail.name == "b"

    def test_reify_complex_nested_structure(self):
        """Test reifying complex nested structure with variables and lists."""
        store = Store()
        trail = Trail()
        qname_by_id = {}

        # Create variables with proper store
        var_x_id = store.new_var("X")
        var_y_id = store.new_var("Y")
        qname_by_id[var_x_id] = "X"
        qname_by_id[var_y_id] = "Y"

        # Create structure: foo(X, [Y, bar])
        var_x = Var(var_x_id, "X")
        var_y = Var(var_y_id, "Y")
        list_term = PrologList((var_y, Atom("bar")), Atom("[]"))
        struct = Struct("foo", (var_x, list_term))

        # Bind variables
        bind_root_to_term(var_x_id, Atom("hello"), trail, store)
        bind_root_to_term(var_y_id, Int(42), trail, store)

        result = reify_term(store, qname_by_id, struct)

        assert isinstance(result, Struct)
        assert result.functor == "foo"
        assert isinstance(result.args[0], Atom)
        assert result.args[0].name == "hello"
        assert isinstance(result.args[1], PrologList)
        assert isinstance(result.args[1].items[0], Int)
        assert result.args[1].items[0].value == 42
