"""
Tests for type-checking builtins in engine.
Tests var/1, nonvar/1, atom/1, and \\=/2 predicates.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestNotUnifiable:
    """Tests for \\=/2 (not unifiable) predicate."""

    def test_not_unifiable_different_atoms(self, empty_engine):
        """Test \\= succeeds with different atoms."""
        query = Struct("\\=", (Atom("a"), Atom("b")))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_same_atoms_fails(self, empty_engine):
        """Test \\= fails with same atoms."""
        query = Struct("\\=", (Atom("a"), Atom("a")))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_different_numbers(self, empty_engine):
        """Test \\= succeeds with different numbers."""
        query = Struct("\\=", (Int(1), Int(2)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_same_numbers_fails(self, empty_engine):
        """Test \\= fails with same numbers."""
        query = Struct("\\=", (Int(42), Int(42)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_different_structures(self, empty_engine):
        """Test \\= succeeds with different structures."""
        s1 = Struct("f", (Int(1), Int(2)))
        s2 = Struct("f", (Int(1), Int(3)))
        query = Struct("\\=", (s1, s2))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_different_functors(self, empty_engine):
        """Test \\= succeeds with structures having different functors."""
        s1 = Struct("f", (Int(1),))
        s2 = Struct("g", (Int(1),))
        query = Struct("\\=", (s1, s2))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_var_and_ground_fails(self, empty_engine):
        """Test \\= fails with variable and ground term (they can unify)."""
        X = Var(0, "X")
        query = Struct("\\=", (X, Int(5)))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_variables_always_fails(self, empty_engine):
        """Test \\= always fails with two unbound variables."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        query = Struct("\\=", (X, Y))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_lists(self, empty_engine):
        """Test \\= with lists."""
        list1 = List((Int(1), Int(2)))
        list2 = List((Int(1), Int(3)))
        query = Struct("\\=", (list1, list2))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_partial_can_unify_fails(self, empty_engine):
        """Test \\= fails when structures can be unified through variables."""
        X = Var(0, "X")
        s1 = Struct("f", (X, Int(2)))
        s2 = Struct("f", (Int(1), Int(2)))
        query = Struct("\\=", (s1, s2))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0  # They can unify if X = 1

    def test_not_unifiable_wrong_arity_fails(self, empty_engine):
        """Test \\= with wrong arity (should fail as builtin check)."""
        # Try with 1 argument (wrong arity)
        query = Struct("\\=", (Int(1),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_with_bound_var(self, empty_engine):
        """Test \\= with a bound variable."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = 1, Y = 2, X \\= Y
        query = Struct(",", (
            Struct("=", (X, Int(1))),
            Struct(",", (
                Struct("=", (Y, Int(2))),
                Struct("\\=", (X, Y))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)

    def test_not_unifiable_cyclic_structure(self, empty_engine):
        """Test \\= with potentially cyclic structures."""
        X = Var(0, "X")
        # X \\= f(X) - should succeed as they can't unify (occurs check)
        query = Struct("\\=", (X, Struct("f", (X,))))
        solutions = empty_engine.run([query])
        # With occurs check on, X cannot unify with f(X)
        if empty_engine.occurs_check:
            assert len(solutions) == 1
        else:
            # Without occurs check, they could unify
            assert len(solutions) == 0

    def test_not_unifiable_empty_list_vs_nonempty(self, empty_engine):
        """Test \\= between empty list and non-empty list."""
        query = Struct("\\=", (Atom("[]"), List((Int(1),))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_nested_structures(self, empty_engine):
        """Test \\= with deeply nested structures."""
        s1 = Struct("f", (Struct("g", (Int(1),)),))
        s2 = Struct("f", (Struct("g", (Int(2),)),))
        query = Struct("\\=", (s1, s2))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1


class TestVarPredicate:
    """Tests for var/1 predicate."""

    def test_var_with_unbound_variable(self, empty_engine):
        """Test var/1 succeeds with unbound variable."""
        X = Var(0, "X")
        query = Struct("var", (X,))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # X remains unbound in the solution
        assert isinstance(solutions[0]["X"], Var)

    def test_var_with_bound_variable_fails(self, empty_engine):
        """Test var/1 fails with bound variable."""
        X = Var(0, "X")
        # X = 5, var(X)
        query = Struct(",", (Struct("=", (X, Int(5))), Struct("var", (X,))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_var_with_atom_fails(self, empty_engine):
        """Test var/1 fails with atom."""
        query = Struct("var", (Atom("abc"),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_var_with_number_fails(self, empty_engine):
        """Test var/1 fails with number."""
        query = Struct("var", (Int(42),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_var_with_structure_fails(self, empty_engine):
        """Test var/1 fails with structure."""
        query = Struct("var", (Struct("f", (Int(1), Int(2))),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestNonvarPredicate:
    """Tests for nonvar/1 predicate."""

    def test_nonvar_with_atom(self, empty_engine):
        """Test nonvar/1 succeeds with atom."""
        query = Struct("nonvar", (Atom("abc"),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_nonvar_with_number(self, empty_engine):
        """Test nonvar/1 succeeds with number."""
        query = Struct("nonvar", (Int(42),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_nonvar_with_structure(self, empty_engine):
        """Test nonvar/1 succeeds with structure."""
        query = Struct("nonvar", (Struct("f", (Int(1), Int(2))),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_nonvar_with_unbound_variable_fails(self, empty_engine):
        """Test nonvar/1 fails with unbound variable."""
        X = Var(0, "X")
        query = Struct("nonvar", (X,))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_nonvar_with_bound_variable(self, empty_engine):
        """Test nonvar/1 succeeds with bound variable."""
        X = Var(0, "X")
        # X = 5, nonvar(X)
        query = Struct(",", (Struct("=", (X, Int(5))), Struct("nonvar", (X,))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(5)


class TestAtomPredicate:
    """Tests for atom/1 predicate."""

    def test_atom_with_atom(self, empty_engine):
        """Test atom/1 succeeds with atom."""
        query = Struct("atom", (Atom("abc"),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_atom_with_empty_list(self, empty_engine):
        """Test atom/1 succeeds with empty list []."""
        query = Struct("atom", (Atom("[]"),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_atom_with_number_fails(self, empty_engine):
        """Test atom/1 fails with number."""
        query = Struct("atom", (Int(42),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_structure_fails(self, empty_engine):
        """Test atom/1 fails with structure."""
        query = Struct("atom", (Struct("f", (Int(1),)),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_list_fails(self, empty_engine):
        """Test atom/1 fails with non-empty list."""
        query = Struct("atom", (List((Int(1), Int(2))),))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_unbound_variable_fails(self, empty_engine):
        """Test atom/1 fails with unbound variable."""
        X = Var(0, "X")
        query = Struct("atom", (X,))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_var_bound_to_empty_list(self, empty_engine):
        """Test atom/1 with variable bound to []."""
        X = Var(0, "X")
        # X = [], atom(X)
        query = Struct(",", (Struct("=", (X, Atom("[]"))), Struct("atom", (X,))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("[]")

    def test_atom_with_variable_bound_to_atom(self, empty_engine):
        """Test atom/1 with variable bound to atom."""
        X = Var(0, "X")
        # X = abc, atom(X)
        query = Struct(",", (Struct("=", (X, Atom("abc"))), Struct("atom", (X,))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("abc")

    def test_atom_with_variable_bound_to_number_fails(self, empty_engine):
        """Test atom/1 fails with variable bound to number."""
        X = Var(0, "X")
        # X = 42, atom(X)
        query = Struct(",", (Struct("=", (X, Int(42))), Struct("atom", (X,))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0


class TestMixedTypeChecking:
    """Tests for combinations of type-checking predicates."""

    def test_var_nonvar_exclusive(self, empty_engine):
        """Test that var/1 and nonvar/1 are mutually exclusive."""
        X = Var(0, "X")
        # var(X), nonvar(X) - should fail
        query = Struct(",", (Struct("var", (X,)), Struct("nonvar", (X,))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_bound_var_type_checking(self, empty_engine):
        """Test type checking with bound variables."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = abc, Y = 42, atom(X), nonvar(Y)
        query = Struct(",", (
            Struct("=", (X, Atom("abc"))),
            Struct(",", (
                Struct("=", (Y, Int(42))),
                Struct(",", (
                    Struct("atom", (X,)),
                    Struct("nonvar", (Y,))
                ))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("abc")
        assert solutions[0]["Y"] == Int(42)

    def test_type_check_in_compound_goal(self, empty_engine):
        """Test type checking predicates in compound goals."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # (var(X) ; atom(X)), X = abc, atom(X)
        query = Struct(",", (
            Struct(";", (
                Struct("var", (X,)),
                Struct("atom", (X,))
            )),
            Struct(",", (
                Struct("=", (X, Atom("abc"))),
                Struct("atom", (X,))
            ))
        ))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("abc")