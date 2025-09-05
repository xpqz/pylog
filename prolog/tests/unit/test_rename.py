"""Tests for Variable Renaming (Stage 0)."""

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.engine.rename import VarRenamer
from prolog.unify.store import Store


class TestVarRenamer:
    """Tests for VarRenamer class."""

    def test_rename_term_creates_fresh_var_for_var(self):
        """Test rename_term creates fresh variable for Var."""
        store = Store()
        renamer = VarRenamer(store)

        # Original var
        original = Var(99, "X")

        # Rename should create fresh var
        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        assert isinstance(renamed, Var)
        assert renamed.id != original.id  # Different ID
        assert renamed.hint == "X"  # Hint preserved

    def test_rename_term_preserves_atoms_unchanged(self):
        """Test rename_term preserves atoms and ints unchanged."""
        store = Store()
        renamer = VarRenamer(store)

        # Atoms should be unchanged
        atom = Atom("test")
        mapping = {}
        assert renamer.rename_term(atom, mapping) is atom

        # Ints should be unchanged
        integer = Int(42)
        assert renamer.rename_term(integer, mapping) is integer

    def test_rename_term_handles_nested_structures(self):
        """Test rename_term handles nested structures recursively."""
        store = Store()
        renamer = VarRenamer(store)

        # Original: f(X, g(Y, a), X)
        original = Struct(
            "f", (Var(10, "X"), Struct("g", (Var(20, "Y"), Atom("a"))), Var(10, "X"))
        )

        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        # Structure preserved
        assert isinstance(renamed, Struct)
        assert renamed.functor == "f"
        assert len(renamed.args) == 3

        # First X renamed to fresh var
        assert isinstance(renamed.args[0], Var)
        x_id = renamed.args[0].id

        # Nested structure
        assert isinstance(renamed.args[1], Struct)
        assert renamed.args[1].functor == "g"
        assert isinstance(renamed.args[1].args[0], Var)
        y_id = renamed.args[1].args[0].id
        assert x_id != y_id  # X and Y get different IDs
        assert renamed.args[1].args[1] == Atom("a")

        # Second X should map to same fresh var as first X
        assert isinstance(renamed.args[2], Var)
        assert renamed.args[2].id == x_id  # Same as first X

    def test_rename_term_handles_lists_with_tails(self):
        """Test rename_term handles lists with tails (default Atom('[]'))."""
        store = Store()
        renamer = VarRenamer(store)

        # [X, Y | Z]
        original = PrologList((Var(1, "X"), Var(2, "Y")), tail=Var(3, "Z"))

        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        assert isinstance(renamed, PrologList)
        assert len(renamed.items) == 2

        # Items renamed
        assert isinstance(renamed.items[0], Var)
        x_id = renamed.items[0].id
        assert isinstance(renamed.items[1], Var)
        y_id = renamed.items[1].id
        assert x_id != y_id  # X and Y get different IDs

        # Tail renamed
        assert isinstance(renamed.tail, Var)
        # Check all three variables have different IDs
        assert len({renamed.items[0].id, renamed.items[1].id, renamed.tail.id}) == 3

    def test_rename_term_handles_empty_list_tail(self):
        """Test rename_term handles empty list tail Atom('[]')."""
        store = Store()
        renamer = VarRenamer(store)

        # [X, Y] (with default [] tail)
        original = PrologList((Var(1, "X"), Var(2, "Y")), tail=Atom("[]"))

        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        assert isinstance(renamed, PrologList)
        # Items renamed - check they have different IDs
        assert isinstance(renamed.items[0], Var)
        assert isinstance(renamed.items[1], Var)
        assert renamed.items[0].id != renamed.items[1].id
        # Empty list tail unchanged
        assert renamed.tail == Atom("[]")

    def test_rename_clause_renames_both_head_and_body(self):
        """Test rename_clause renames both head and body."""
        store = Store()
        renamer = VarRenamer(store)

        # Original clause: p(X, Y) :- q(X), r(Y, X).
        original = Clause(
            head=Struct("p", (Var(10, "X"), Var(20, "Y"))),
            body=(
                Struct("q", (Var(10, "X"),)),
                Struct("r", (Var(20, "Y"), Var(10, "X"))),
            ),
        )

        renamed = renamer.rename_clause(original)

        # Check it's a new Clause
        assert isinstance(renamed, Clause)
        assert renamed is not original

        # Head renamed
        assert renamed.head.functor == "p"
        x_id = renamed.head.args[0].id
        y_id = renamed.head.args[1].id
        assert x_id != y_id  # X and Y get different IDs

        # Body renamed with consistent mapping
        assert renamed.body[0].functor == "q"
        assert renamed.body[0].args[0].id == x_id  # Same X

        assert renamed.body[1].functor == "r"
        assert renamed.body[1].args[0].id == y_id  # Same Y
        assert renamed.body[1].args[1].id == x_id  # Same X

    def test_consistent_mapping_within_single_clause(self):
        """Test consistent variable mapping within single clause."""
        store = Store()
        renamer = VarRenamer(store)

        # Clause with repeated variables
        original = Clause(
            head=Struct("test", (Var(5, "A"), Var(5, "A"), Var(7, "B"))),
            body=(Struct("use", (Var(7, "B"), Var(5, "A"))),),
        )

        renamed = renamer.rename_clause(original)

        # All occurrences of A map to same fresh var
        a_id = renamed.head.args[0].id
        assert renamed.head.args[1].id == a_id  # Second A
        assert renamed.body[0].args[1].id == a_id  # A in body

        # All occurrences of B map to same fresh var
        b_id = renamed.head.args[2].id
        assert renamed.body[0].args[0].id == b_id  # B in body

        # A and B have different IDs
        assert a_id != b_id

    def test_fresh_renamer_for_each_clause_use(self):
        """Test fresh renamer for each clause use (no var sharing)."""
        store = Store()

        # Same clause used twice
        clause = Clause(head=Struct("p", (Var(1, "X"),)), body=())

        # First renaming
        renamer1 = VarRenamer(store)
        renamed1 = renamer1.rename_clause(clause)
        x_id_1 = renamed1.head.args[0].id

        # Second renaming with fresh renamer
        renamer2 = VarRenamer(store)
        renamed2 = renamer2.rename_clause(clause)
        x_id_2 = renamed2.head.args[0].id

        # Different fresh variables allocated
        assert x_id_1 != x_id_2

    def test_deterministic_var_ids(self):
        """Test deterministic var IDs (same input -> same output)."""
        # Two identical stores
        store1 = Store()
        store2 = Store()

        # Same term
        term = Struct("f", (Var(10, "X"), Var(20, "Y"), Var(10, "X")))

        # Rename with two renamers
        renamer1 = VarRenamer(store1)
        mapping1 = {}
        renamed1 = renamer1.rename_term(term, mapping1)

        renamer2 = VarRenamer(store2)
        mapping2 = {}
        renamed2 = renamer2.rename_term(term, mapping2)

        # Should get same fresh var IDs
        assert renamed1.args[0].id == renamed2.args[0].id  # Both X -> 0
        assert renamed1.args[1].id == renamed2.args[1].id  # Both Y -> 1
        assert renamed1.args[2].id == renamed2.args[2].id  # Both X -> 0

    def test_rename_complex_nested_list_structure(self):
        """Test renaming complex nested list structure."""
        store = Store()
        renamer = VarRenamer(store)

        # [[X, Y], [Y, Z] | W]
        original = PrologList(
            (
                PrologList((Var(1, "X"), Var(2, "Y")), tail=Atom("[]")),
                PrologList((Var(2, "Y"), Var(3, "Z")), tail=Atom("[]")),
            ),
            tail=Var(4, "W"),
        )

        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        # Check structure preserved
        assert isinstance(renamed, PrologList)
        assert len(renamed.items) == 2

        # First sublist [X, Y]
        first = renamed.items[0]
        assert isinstance(first, PrologList)
        x_id = first.items[0].id
        y_id = first.items[1].id

        # Second sublist [Y, Z] - Y should be consistent
        second = renamed.items[1]
        assert isinstance(second, PrologList)
        assert second.items[0].id == y_id  # Same Y
        z_id = second.items[1].id

        # Tail W
        w_id = renamed.tail.id

        # All different variables
        assert len({x_id, y_id, z_id, w_id}) == 4

    def test_rename_preserves_immutability(self):
        """Test renaming produces new terms, preserving immutability."""
        store = Store()
        renamer = VarRenamer(store)

        # Original structure
        original = Struct("f", (Var(1, "X"), Atom("a")))

        # Rename
        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        # Different objects
        assert renamed is not original
        assert renamed.args is not original.args
        # But atom is shared (unchanged)
        assert renamed.args[1] is original.args[1]

    def test_rename_handles_deeply_nested_structures(self):
        """Test renaming handles deeply nested structures."""
        store = Store()
        renamer = VarRenamer(store)

        # Build deeply nested structure: f(g(h(i(X))))
        original = Struct(
            "f", (Struct("g", (Struct("h", (Struct("i", (Var(99, "X"),)),)),)),)
        )

        mapping = {}
        renamed = renamer.rename_term(original, mapping)

        # Navigate to the variable
        assert renamed.functor == "f"
        inner_g = renamed.args[0]
        assert inner_g.functor == "g"
        inner_h = inner_g.args[0]
        assert inner_h.functor == "h"
        inner_i = inner_h.args[0]
        assert inner_i.functor == "i"

        # Variable renamed
        assert isinstance(inner_i.args[0], Var)
        assert inner_i.args[0].id != 99  # Different from original
