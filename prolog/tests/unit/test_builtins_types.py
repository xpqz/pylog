"""Unit tests for builtins.types module.

These tests target the builtin type predicates directly, ensuring correct behavior
including variable dereferencing, type checking, and ground term analysis.
"""

from prolog.ast.clauses import Program
from prolog.ast.terms import Atom, Int, Float, Var, Struct, List as PrologList
from prolog.engine.engine import Engine
from prolog.engine.builtins.types import (
    builtin_var,
    builtin_nonvar,
    builtin_atom,
    builtin_integer,
    builtin_float,
    builtin_number,
    builtin_atomic,
    builtin_compound,
    builtin_callable,
    builtin_ground,
    is_ground,
    register,
)
from prolog.unify.unify import unify


class TestBuiltinTypePredicates:
    """Test type-checking builtin predicates directly."""

    def setup_method(self):
        """Set up test environment."""
        self.program = Program([])
        self.engine = Engine(self.program)

    def test_builtin_var_with_unbound_variable(self):
        """Test var/1 with unbound variable."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")
        assert builtin_var(self.engine, (var,)) is True

    def test_builtin_var_with_bound_variable(self):
        """Test var/1 with bound variable."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")
        # Bind variable to atom

        unify(var, Atom("hello"), self.engine.store, self.engine.trail)
        assert builtin_var(self.engine, (var,)) is False

    def test_builtin_var_with_non_variable(self):
        """Test var/1 with non-variable terms."""
        assert builtin_var(self.engine, (Atom("hello"),)) is False
        assert builtin_var(self.engine, (Int(42),)) is False
        assert builtin_var(self.engine, (Struct("foo", (Int(1),)),)) is False

    def test_builtin_nonvar_complement_of_var(self):
        """Test nonvar/1 is complement of var/1."""
        var_id = self.engine.store.new_var("X")
        unbound_var = Var(var_id, "X")

        var_id2 = self.engine.store.new_var("Y")
        bound_var = Var(var_id2, "Y")

        unify(bound_var, Int(123), self.engine.store, self.engine.trail)

        # var/1 and nonvar/1 should be complements
        assert builtin_var(self.engine, (unbound_var,)) is True
        assert builtin_nonvar(self.engine, (unbound_var,)) is False

        assert builtin_var(self.engine, (bound_var,)) is False
        assert builtin_nonvar(self.engine, (bound_var,)) is True

    def test_builtin_atom_with_atoms(self):
        """Test atom/1 with various atom types."""
        assert builtin_atom(self.engine, (Atom("hello"),)) is True
        assert builtin_atom(self.engine, (Atom("[]"),)) is True  # Empty list atom
        assert builtin_atom(self.engine, (Atom(""),)) is True  # Empty atom

    def test_builtin_atom_with_non_atoms(self):
        """Test atom/1 with non-atom terms."""
        assert builtin_atom(self.engine, (Int(42),)) is False
        assert builtin_atom(self.engine, (Float(3.14),)) is False
        assert builtin_atom(self.engine, (Struct("foo", (Int(1),)),)) is False
        assert builtin_atom(self.engine, (PrologList((Int(1), Int(2))),)) is False

    def test_builtin_atom_with_bound_variable(self):
        """Test atom/1 with variable bound to atom."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")

        unify(var, Atom("test"), self.engine.store, self.engine.trail)
        assert builtin_atom(self.engine, (var,)) is True

    def test_builtin_integer_with_integers(self):
        """Test integer/1 with integer values."""
        assert builtin_integer(self.engine, (Int(42),)) is True
        assert builtin_integer(self.engine, (Int(-17),)) is True
        assert builtin_integer(self.engine, (Int(0),)) is True

    def test_builtin_integer_with_non_integers(self):
        """Test integer/1 with non-integer terms."""
        assert builtin_integer(self.engine, (Float(3.14),)) is False
        assert builtin_integer(self.engine, (Atom("42"),)) is False
        assert builtin_integer(self.engine, (Struct("num", (Int(42),)),)) is False

    def test_builtin_float_with_floats(self):
        """Test float/1 with float values."""
        assert builtin_float(self.engine, (Float(3.14),)) is True
        assert builtin_float(self.engine, (Float(-2.5),)) is True
        assert builtin_float(self.engine, (Float(0.0),)) is True

    def test_builtin_float_with_non_floats(self):
        """Test float/1 with non-float terms."""
        assert builtin_float(self.engine, (Int(42),)) is False
        assert builtin_float(self.engine, (Atom("3.14"),)) is False

    def test_builtin_number_with_numbers(self):
        """Test number/1 with both integers and floats."""
        assert builtin_number(self.engine, (Int(42),)) is True
        assert builtin_number(self.engine, (Float(3.14),)) is True
        assert builtin_number(self.engine, (Int(-17),)) is True
        assert builtin_number(self.engine, (Float(0.0),)) is True

    def test_builtin_number_with_non_numbers(self):
        """Test number/1 with non-numeric terms."""
        assert builtin_number(self.engine, (Atom("42"),)) is False
        assert builtin_number(self.engine, (Struct("num", (Int(42),)),)) is False

    def test_builtin_atomic_with_atomic_terms(self):
        """Test atomic/1 with atomic terms (atoms and numbers)."""
        assert builtin_atomic(self.engine, (Atom("hello"),)) is True
        assert builtin_atomic(self.engine, (Int(42),)) is True
        assert builtin_atomic(self.engine, (Float(3.14),)) is True

    def test_builtin_atomic_with_compound_terms(self):
        """Test atomic/1 with compound terms."""
        assert builtin_atomic(self.engine, (Struct("foo", (Int(1),)),)) is False
        assert builtin_atomic(self.engine, (PrologList((Int(1), Int(2))),)) is False

    def test_builtin_compound_with_structures(self):
        """Test compound/1 with structure terms."""
        assert builtin_compound(self.engine, (Struct("foo", (Int(1),)),)) is True
        assert builtin_compound(self.engine, (Struct("bar", ()),)) is True

    def test_builtin_compound_with_lists(self):
        """Test compound/1 with list terms."""
        assert builtin_compound(self.engine, (PrologList((Int(1), Int(2))),)) is True
        assert builtin_compound(self.engine, (PrologList(()),)) is True

    def test_builtin_compound_with_atomic_terms(self):
        """Test compound/1 with atomic terms."""
        assert builtin_compound(self.engine, (Atom("hello"),)) is False
        assert builtin_compound(self.engine, (Int(42),)) is False
        assert builtin_compound(self.engine, (Float(3.14),)) is False

    def test_builtin_callable_with_atoms(self):
        """Test callable/1 with atoms."""
        assert builtin_callable(self.engine, (Atom("foo"),)) is True
        assert builtin_callable(self.engine, (Atom("[]"),)) is True

    def test_builtin_callable_with_compound_terms(self):
        """Test callable/1 with compound terms."""
        assert builtin_callable(self.engine, (Struct("foo", (Int(1),)),)) is True
        assert builtin_callable(self.engine, (PrologList((Int(1), Int(2))),)) is True

    def test_builtin_callable_with_numbers(self):
        """Test callable/1 with numbers (not callable)."""
        assert builtin_callable(self.engine, (Int(42),)) is False
        assert builtin_callable(self.engine, (Float(3.14),)) is False

    def test_builtin_ground_with_ground_terms(self):
        """Test ground/1 with completely ground terms."""
        assert builtin_ground(self.engine, (Atom("hello"),)) is True
        assert builtin_ground(self.engine, (Int(42),)) is True
        assert builtin_ground(self.engine, (Float(3.14),)) is True

    def test_builtin_ground_with_unbound_variables(self):
        """Test ground/1 with unbound variables."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")
        assert builtin_ground(self.engine, (var,)) is False

    def test_builtin_ground_with_bound_variables(self):
        """Test ground/1 with bound variables."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")

        unify(var, Int(42), self.engine.store, self.engine.trail)
        assert builtin_ground(self.engine, (var,)) is True


class TestIsGroundHelper:
    """Test the is_ground helper function."""

    def setup_method(self):
        """Set up test environment."""
        self.program = Program([])
        self.engine = Engine(self.program)

    def test_is_ground_with_nested_structures(self):
        """Test is_ground with nested structures containing variables."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")

        # Ground structure
        ground_struct = Struct("complex", (Int(1), Atom("test"), Float(2.5)))
        assert is_ground(self.engine, ground_struct) is True

        # Structure with unbound variable
        struct_with_var = Struct("complex", (Int(1), var, Float(2.5)))
        assert is_ground(self.engine, struct_with_var) is False

    def test_is_ground_with_lists(self):
        """Test is_ground with lists containing variables."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")

        # Ground list
        ground_list = PrologList((Int(1), Int(2), Int(3)))
        assert is_ground(self.engine, ground_list) is True

        # List with variable in items
        list_with_var_item = PrologList((Int(1), var, Int(3)))
        assert is_ground(self.engine, list_with_var_item) is False

        # List with variable in tail
        tail_var_id = self.engine.store.new_var("T")
        tail_var = Var(tail_var_id, "T")
        list_with_var_tail = PrologList((Int(1), Int(2)), tail_var)
        assert is_ground(self.engine, list_with_var_tail) is False

    def test_is_ground_with_bound_variables_in_structures(self):
        """Test is_ground with bound variables in complex structures."""
        var_id = self.engine.store.new_var("X")
        var = Var(var_id, "X")

        unify(var, Int(42), self.engine.store, self.engine.trail)

        # Structure with bound variable should be ground
        struct_with_bound_var = Struct("test", (Int(1), var, Atom("end")))
        assert is_ground(self.engine, struct_with_bound_var) is True


class TestBuiltinRegistration:
    """Test the builtin registration system."""

    def test_register_populates_registry(self):
        """Test that register() populates the registry correctly."""
        registry = {}
        register(registry)

        # Check that all expected predicates are registered
        expected_predicates = [
            ("var", 1),
            ("nonvar", 1),
            ("atom", 1),
            ("integer", 1),
            ("float", 1),
            ("number", 1),
            ("atomic", 1),
            ("compound", 1),
            ("callable", 1),
            ("ground", 1),
        ]

        for pred_name, arity in expected_predicates:
            assert (pred_name, arity) in registry
            assert callable(registry[(pred_name, arity)])

    def test_registry_function_identity(self):
        """Test that registry contains actual function references."""
        registry = {}
        register(registry)

        # Check that the registered functions are the actual builtin functions
        assert registry[("var", 1)] is builtin_var
        assert registry[("nonvar", 1)] is builtin_nonvar
        assert registry[("atom", 1)] is builtin_atom
        assert registry[("integer", 1)] is builtin_integer
        assert registry[("float", 1)] is builtin_float
        assert registry[("number", 1)] is builtin_number
        assert registry[("atomic", 1)] is builtin_atomic
        assert registry[("compound", 1)] is builtin_compound
        assert registry[("callable", 1)] is builtin_callable
        assert registry[("ground", 1)] is builtin_ground

    def test_register_is_idempotent(self):
        """Test that calling register multiple times is safe."""
        registry = {}
        register(registry)
        initial_size = len(registry)

        # Register again
        register(registry)
        assert len(registry) == initial_size

        # Functions should still be correct
        assert registry[("var", 1)] is builtin_var
        assert registry[("ground", 1)] is builtin_ground


class TestArityChecking:
    """Test that all builtin predicates properly check arity."""

    def setup_method(self):
        """Set up test environment."""
        self.program = Program([])
        self.engine = Engine(self.program)

    def test_all_predicates_reject_wrong_arity(self):
        """Test that all type predicates reject wrong arity."""
        predicates = [
            builtin_var,
            builtin_nonvar,
            builtin_atom,
            builtin_integer,
            builtin_float,
            builtin_number,
            builtin_atomic,
            builtin_compound,
            builtin_callable,
            builtin_ground,
        ]

        for predicate in predicates:
            # Wrong arity should return False
            assert predicate(self.engine, ()) is False  # 0 args
            assert predicate(self.engine, (Atom("a"), Atom("b"))) is False  # 2 args

            # Correct arity should not fail due to arity (may fail for other reasons)
            # We use an atom which should succeed for most predicates or fail gracefully
            result = predicate(self.engine, (Atom("test"),))
            assert isinstance(result, bool)  # Should return a boolean
