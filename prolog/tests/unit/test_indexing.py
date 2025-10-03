"""
Unit tests for Stage 2: Indexing infrastructure.

Tests the core indexing data structures and algorithms for first-argument
indexing with type switching.

Note on list representations:
- Empty lists can be represented as List((), Atom("[]")) or Atom('[]')
- Non-empty lists can be List((head, ...), tail) or canonical Struct('.', (head, tail))
- Stage 2 indexing must handle both representations correctly

Selection is first-argument only; tests ensure second and later arguments
do not affect candidate filtering.
"""

import types
import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.unify.store import Store
from prolog.ast.clauses import Clause
from prolog.engine.indexing import (
    PredIndex,
    ClauseIndex,
    build_from_clauses,
    analyze_first_arg,
)

# Note: These will be needed when implementation is complete
# from prolog.ast.terms import Float  # Future: Float support
try:
    from prolog.unify.unify import bind
except ImportError:
    # Mock bind for tests until implementation is complete
    def bind(store, var_id, term, trail):
        """Mock bind function for testing."""
        pass


class TestPredIndex:
    """Tests for per-predicate index structure."""

    def test_predindex_initializes_empty(self):
        """PredIndex should initialize with empty buckets."""
        idx = PredIndex()
        assert idx.order == []
        assert idx.var_ids == set()
        assert idx.empty_list_ids == set()
        assert idx.int_ids == set()
        assert idx.list_nonempty_ids == set()
        assert idx.struct_functor == {}

    def test_predindex_has_slots_for_memory_efficiency(self):
        """PredIndex should use __slots__ to reduce memory overhead."""
        idx = PredIndex()
        assert hasattr(idx.__class__, "__slots__")
        # Should not have __dict__ due to __slots__
        with pytest.raises(AttributeError):
            _ = idx.__dict__

    def test_predindex_stores_clause_ids_per_bucket(self):
        """PredIndex should correctly categorize clause IDs into buckets."""
        idx = PredIndex()

        # Add some clause IDs to different buckets
        idx.order = [0, 1, 2, 3, 4]
        idx.var_ids.add(0)
        idx.empty_list_ids.add(1)
        idx.int_ids.add(2)
        idx.list_nonempty_ids.add(3)
        idx.struct_functor[("foo", 2)] = {4}

        assert 0 in idx.var_ids
        assert 1 in idx.empty_list_ids
        assert 2 in idx.int_ids
        assert 3 in idx.list_nonempty_ids
        assert 4 in idx.struct_functor[("foo", 2)]


class TestClauseIndex:
    """Tests for the global clause index structure."""

    def test_clauseindex_initializes_empty(self):
        """ClauseIndex should initialize with empty dictionaries."""
        idx = ClauseIndex()
        assert idx.preds == {}
        assert idx.clauses == {}

    def test_clauseindex_stores_clauses_per_predicate(self):
        """ClauseIndex should maintain separate PredIndex per predicate."""
        idx = ClauseIndex()

        # Create PredIndex for different predicates
        idx.preds[("foo", 1)] = PredIndex()
        idx.preds[("bar", 2)] = PredIndex()

        assert ("foo", 1) in idx.preds
        assert ("bar", 2) in idx.preds
        assert idx.preds[("foo", 1)] is not idx.preds[("bar", 2)]

    def test_clauseindex_maps_predicate_and_id_to_clause(self):
        """ClauseIndex should map (pred_key, clause_id) to actual Clause."""
        idx = ClauseIndex()

        # Create a test clause
        clause = Clause(head=Struct("foo", (Atom("a"),)), body=())

        pred_key = ("foo", 1)
        clause_id = 0
        idx.clauses[(pred_key, clause_id)] = clause

        assert idx.clauses[(pred_key, clause_id)] is clause


class TestFirstArgumentAnalysis:
    """Tests for analyzing first argument of clause heads."""

    def test_analyze_variable_first_arg(self):
        """analyze_first_arg should detect variable first argument."""
        store = Store()
        head = Struct("p", (Var(0, "X"),))
        arg_type = analyze_first_arg(head, store)
        assert arg_type == "var"

    def test_analyze_atom_first_arg(self):
        """analyze_first_arg should detect atom first argument."""
        # Atoms are keyed in struct_functor with arity 0
        store = Store()
        head = Struct("p", (Atom("foo"),))
        arg_type = analyze_first_arg(head, store)
        assert arg_type == ("atom", "foo", 0)

    def test_analyze_empty_list_first_arg(self):
        """analyze_first_arg should detect empty list [] as special."""
        store = Store()
        head = Struct("p", (List((), Atom("[]")),))  # Empty list
        arg_type = analyze_first_arg(head, store)
        assert arg_type == "empty_list"

    def test_analyze_nonempty_list_first_arg(self):
        """analyze_first_arg should detect non-empty list [H|T]."""
        store = Store()
        head = Struct("p", (List((Atom("a"),), Var(0, "T")),))  # [a|T]
        arg_type = analyze_first_arg(head, store)
        assert arg_type == "list_nonempty"

    def test_analyze_integer_first_arg(self):
        """analyze_first_arg should detect integer first argument."""
        store = Store()
        head = Struct("p", (Int(42),))
        arg_type = analyze_first_arg(head, store)
        assert arg_type == "int"

    def test_analyze_negative_integer_first_arg(self):
        """analyze_first_arg should handle negative integers."""
        store = Store()
        head = Struct("p", (Int(-3),))
        arg_type = analyze_first_arg(head, store)
        assert arg_type == "int"

    def test_analyze_struct_first_arg(self):
        """analyze_first_arg should detect struct with functor/arity."""
        store = Store()
        head = Struct("p", (Struct("foo", (Atom("a"), Atom("b"))),))
        arg_type = analyze_first_arg(head, store)
        assert arg_type == ("struct", "foo", 2)

    def test_analyze_zero_arity_predicate(self):
        """analyze_first_arg should handle predicates with no arguments."""
        store = Store()
        head = Atom("p")  # p/0
        arg_type = analyze_first_arg(head, store)
        assert arg_type == "no_args"


class TestIndexBuilding:
    """Tests for building index from clauses."""

    def test_build_preserves_source_order(self):
        """Index building must preserve original clause order."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
        ]

        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]

        # Order should be [0, 1, 2] - exact source order
        assert pred_idx.order == [0, 1, 2]

    def test_clauses_indexed_per_predicate(self):
        """Each predicate should have its own separate index."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("q", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
        ]

        idx = build_from_clauses(clauses)

        assert ("p", 1) in idx.preds
        assert ("q", 1) in idx.preds
        assert len(idx.preds[("p", 1)].order) == 2  # p has 2 clauses
        assert len(idx.preds[("q", 1)].order) == 1  # q has 1 clause

    def test_each_predicate_has_separate_predindex(self):
        """Each predicate must have its own PredIndex instance."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("q", (Atom("a"),)), body=()),
        ]

        idx = build_from_clauses(clauses)

        p_idx = idx.preds[("p", 1)]
        q_idx = idx.preds[("q", 1)]
        assert p_idx is not q_idx

    def test_clause_ids_unique_per_predicate(self):
        """Clause IDs should be unique within each predicate."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
            Clause(head=Struct("q", (Atom("c"),)), body=()),
            Clause(head=Struct("p", (Atom("d"),)), body=()),
        ]

        idx = build_from_clauses(clauses)

        p_ids = idx.preds[("p", 1)].order
        q_ids = idx.preds[("q", 1)].order

        # p should have IDs [0, 1, 2]
        assert p_ids == [0, 1, 2]
        # q should have ID [0] (its own numbering)
        assert q_ids == [0]

    def test_clause_ids_monotonic_in_source_order(self):
        """Clause IDs should be monotonic increasing in source order."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),
            Clause(head=Struct("p", (Int(3),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
        ]

        idx = build_from_clauses(clauses)
        p_idx = idx.preds[("p", 1)]

        # IDs should be [0, 1, 2, 3] and monotonic
        assert p_idx.order == [0, 1, 2, 3]
        for i in range(len(p_idx.order) - 1):
            assert p_idx.order[i] < p_idx.order[i + 1]

    def test_build_from_clauses_deterministic(self):
        """build_from_clauses should be deterministic given same input."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),
            Clause(head=Struct("p", (Int(3),)), body=()),
        ]

        idx1 = build_from_clauses(clauses)
        idx2 = build_from_clauses(clauses)

        # Both indexes should have identical structure
        assert idx1.preds.keys() == idx2.preds.keys()
        p_idx1 = idx1.preds[("p", 1)]
        p_idx2 = idx2.preds[("p", 1)]

        assert p_idx1.order == p_idx2.order
        assert p_idx1.var_ids == p_idx2.var_ids
        assert p_idx1.int_ids == p_idx2.int_ids
        assert p_idx1.struct_functor == p_idx2.struct_functor

    def test_bucket_assignment_for_different_types(self):
        """Clauses should be assigned to correct buckets by first arg type."""
        clauses = [
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # var
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # atom
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # []
            Clause(
                head=Struct("p", (List((Atom("h"),), Var(1, "T")),)), body=()
            ),  # [h|T]
            Clause(head=Struct("p", (Int(42),)), body=()),  # int
            Clause(head=Struct("p", (Struct("f", (Atom("x"),)),)), body=()),  # struct
        ]

        idx = build_from_clauses(clauses)
        p_idx = idx.preds[("p", 1)]

        assert 0 in p_idx.var_ids
        assert 1 in p_idx.struct_functor[("a", 0)]
        assert 2 in p_idx.empty_list_ids
        assert 3 in p_idx.list_nonempty_ids
        assert 4 in p_idx.int_ids
        assert 5 in p_idx.struct_functor[("f", 1)]


class TestStaticProgramAssumption:
    """Tests for static program assumption and guards."""

    def test_index_assumes_static_program(self):
        """Index should document that it assumes a static program."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
        ]

        idx = build_from_clauses(clauses)
        # Index should be marked as finalized/static
        assert hasattr(idx, "finalized")
        assert idx.finalized is True

    def test_error_if_clauses_added_after_build(self):
        """Should error/assert if trying to add clauses after index built."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
        ]

        idx = build_from_clauses(clauses)

        # Attempting to add a clause after build should raise
        new_clause = Clause(head=Struct("p", (Atom("b"),)), body=())
        with pytest.raises(AssertionError, match="static program"):
            idx.add_clause(new_clause)

    def test_rebuild_policy_documented(self):
        """Index should document rebuild vs assertion policy."""
        # Stage 2: indexing assumes static programs; dynamic updates must rebuild or assert in dev mode
        idx = ClauseIndex()
        # Should have a policy attribute or method
        assert hasattr(idx, "supports_rebuild") or hasattr(idx, "can_rebuild")

    def test_float_bucket_placeholder(self):
        """Float bucket placeholder exists for future float support."""
        idx = PredIndex()
        assert hasattr(idx, "float_ids")
        # The float_ids bucket exists as a placeholder but floats
        # themselves are not yet implemented in the AST.
        # When Float type is added to AST, it should be handled
        # in analyze_first_arg to populate this bucket.


class TestPredicateIsolation:
    """Tests ensuring predicates never share buckets."""

    def test_predicates_never_share_buckets(self):
        """Different predicates must never share index buckets."""
        clauses = [
            Clause(head=Struct("p", (Atom("shared"),)), body=()),
            Clause(head=Struct("q", (Atom("shared"),)), body=()),
        ]

        idx = build_from_clauses(clauses)

        p_idx = idx.preds[("p", 1)]
        q_idx = idx.preds[("q", 1)]

        # Even with same first arg, buckets are separate
        assert p_idx.struct_functor[("shared", 0)] == {0}
        assert q_idx.struct_functor[("shared", 0)] == {0}
        # But they're different sets
        assert p_idx.struct_functor is not q_idx.struct_functor

    def test_empty_list_goes_to_special_bucket(self):
        """Empty list [] must go to empty_list_ids, not struct_functor."""
        clauses = [
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # []
            Clause(head=Struct("p", (Atom("[]"),)), body=()),  # The atom '[]'
        ]

        idx = build_from_clauses(clauses)
        p_idx = idx.preds[("p", 1)]

        # First clause (actual empty list) goes to empty_list_ids
        assert 0 in p_idx.empty_list_ids
        # Second clause (atom named '[]') also goes to empty_list_ids
        # because Atom('[]') represents the empty list
        assert 1 in p_idx.empty_list_ids
        # Neither should be in struct_functor
        assert ("[]", 0) not in p_idx.struct_functor

    def test_negative_integers_use_same_bucket(self):
        """Both positive and negative integers should use int_ids bucket."""
        clauses = [
            Clause(head=Struct("p", (Int(3),)), body=()),
            Clause(head=Struct("p", (Int(-3),)), body=()),
            Clause(head=Struct("p", (Int(0),)), body=()),
        ]

        idx = build_from_clauses(clauses)
        p_idx = idx.preds[("p", 1)]

        # All three should be in int_ids
        assert 0 in p_idx.int_ids
        assert 1 in p_idx.int_ids
        assert 2 in p_idx.int_ids


class TestAdditionalInvariants:
    """Additional tests for common regression scenarios."""

    def test_float_ids_slot_exists(self):
        """Ensure float_ids placeholder slot is not removed prematurely."""
        idx = PredIndex()
        assert hasattr(idx, "float_ids")
        assert idx.float_ids == set()

    def test_predicate_key_includes_arity(self):
        """Predicate keys must distinguish by arity (p/0 vs p/1)."""
        clauses = [
            Clause(head=Atom("p"), body=()),  # p/0
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # p/1
        ]
        idx = build_from_clauses(clauses)
        assert ("p", 0) in idx.preds
        assert ("p", 1) in idx.preds
        assert idx.preds[("p", 0)].order == [0]
        assert idx.preds[("p", 1)].order == [0]

    def test_struct_bucket_accumulates_multiple_ids(self):
        """Struct buckets should hold all IDs with identical principal functor."""
        clauses = [
            Clause(head=Struct("p", (Struct("f", (Atom("x"),)),)), body=()),  # id 0
            Clause(head=Struct("p", (Struct("f", (Atom("y"),)),)), body=()),  # id 1
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # id 2 (var)
        ]
        idx = build_from_clauses(clauses)
        p = idx.preds[("p", 1)]
        assert p.struct_functor[("f", 1)] == {0, 1}
        assert p.order == [0, 1, 2]  # still in source order

    def test_canonical_dot_struct_counts_as_list_nonempty(self):
        """Canonical '.'/2 structure should be treated as non-empty list."""
        clauses = [
            Clause(
                head=Struct("p", (Struct(".", (Atom("h"), Atom("[]"))),)), body=()
            ),  # [h]
        ]
        idx = build_from_clauses(clauses)
        p = idx.preds[("p", 1)]
        assert 0 in p.list_nonempty_ids
        assert (".", 2) not in p.struct_functor  # not a generic struct bucket

    def test_clause_mapping_populated_for_all_ids(self):
        """idx.clauses should contain entries for all (pred_key, id) pairs."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),
            Clause(head=Struct("q", (Int(1),)), body=()),
        ]
        idx = build_from_clauses(clauses)
        for pred_key, pred_idx in idx.preds.items():
            for cid in pred_idx.order:
                assert (pred_key, cid) in idx.clauses
                assert isinstance(idx.clauses[(pred_key, cid)], Clause)

    def test_interleaved_predicates_have_independent_id_sequences(self):
        """Interleaved predicates must have independent clause ID sequences."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # p id 0
            Clause(head=Struct("q", (Atom("a"),)), body=()),  # q id 0
            Clause(head=Struct("p", (Atom("b"),)), body=()),  # p id 1
            Clause(head=Struct("q", (Atom("b"),)), body=()),  # q id 1
        ]
        idx = build_from_clauses(clauses)
        assert idx.preds[("p", 1)].order == [0, 1]
        assert idx.preds[("q", 1)].order == [0, 1]

    def test_clauseindex_has_slots_for_memory_efficiency(self):
        """ClauseIndex should use __slots__ to reduce memory overhead."""
        idx = ClauseIndex()
        assert hasattr(idx.__class__, "__slots__")
        with pytest.raises(AttributeError):
            _ = idx.__dict__


class TestClauseSelection:
    """Tests for clause selection algorithm."""

    def test_select_clauses_for_atom_goal(self):
        """Select should return matching clauses for atom goal."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Atom("b"),)), body=()),  # 1
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2
            Clause(head=Struct("p", (Atom("c"),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)

        # Query: p(a)
        goal = Struct("p", (Atom("a"),))
        store = Store()
        selected = list(idx.select(("p", 1), goal, store))

        # Should return clauses 0 and 2 (exact match and variable)
        assert len(selected) == 2
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p(a)
        assert selected[1] is idx.clauses[(("p", 1), 2)]  # p(X)

    def test_select_clauses_for_struct_goal(self):
        """Select should return matching clauses for struct goal."""
        clauses = [
            Clause(head=Struct("p", (Struct("f", (Atom("x"),)),)), body=()),  # 0
            Clause(head=Struct("p", (Struct("g", (Atom("y"),)),)), body=()),  # 1
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2
            Clause(head=Struct("p", (Struct("f", (Atom("z"),)),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)

        # Query: p(f(w))
        goal = Struct("p", (Struct("f", (Atom("w"),)),))
        store = Store()
        selected = list(idx.select(("p", 1), goal, store))

        # Should return clauses 0, 2, 3 (f/1 functors and variable)
        assert len(selected) == 3
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p(f(x))
        assert selected[1] is idx.clauses[(("p", 1), 2)]  # p(X)
        assert selected[2] is idx.clauses[(("p", 1), 3)]  # p(f(z))

    def test_select_clauses_for_variable_goal(self):
        """Select should return all clauses for variable goal."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Int(42),)), body=()),  # 1
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)

        # Query: p(Y) where Y is unbound
        store = Store()
        var_id = store.new_var("Y")
        goal = Struct("p", (Var(var_id, "Y"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should return all 4 clauses in source order
        assert len(selected) == 4
        for i in range(4):
            assert selected[i] is idx.clauses[(("p", 1), i)]

    def test_select_clauses_for_integer_goal(self):
        """Select should return matching clauses for integer goal."""
        clauses = [
            Clause(head=Struct("p", (Int(1),)), body=()),  # 0
            Clause(head=Struct("p", (Int(2),)), body=()),  # 1
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 3
            Clause(head=Struct("p", (Int(-5),)), body=()),  # 4
        ]
        idx = build_from_clauses(clauses)

        # Query: p(3)
        goal = Struct("p", (Int(3),))
        store = Store()
        selected = list(idx.select(("p", 1), goal, store))

        # Should return clauses 0, 1, 2, 4 (all integers and variable)
        assert len(selected) == 4
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p(1)
        assert selected[1] is idx.clauses[(("p", 1), 1)]  # p(2)
        assert selected[2] is idx.clauses[(("p", 1), 2)]  # p(X)
        assert selected[3] is idx.clauses[(("p", 1), 4)]  # p(-5)

    def test_select_clauses_for_empty_list_goal(self):
        """Select should return matching clauses for empty list goal."""
        clauses = [
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # 0: []
            Clause(
                head=Struct("p", (List((Atom("a"),), Atom("[]")),)), body=()
            ),  # 1: [a]
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2: X
            Clause(head=Struct("p", (Atom("[]"),)), body=()),  # 3: atom '[]'
        ]
        idx = build_from_clauses(clauses)

        # Query: p([])
        goal = Struct("p", (List((), Atom("[]")),))
        store = Store()
        selected = list(idx.select(("p", 1), goal, store))

        # Should return clauses 0, 2, 3 (empty lists and variable)
        assert len(selected) == 3
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p([])
        assert selected[1] is idx.clauses[(("p", 1), 2)]  # p(X)
        assert selected[2] is idx.clauses[(("p", 1), 3)]  # p('[]')

    def test_select_clauses_for_nonempty_list_goal(self):
        """Select should return matching clauses for non-empty list goal."""
        clauses = [
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # 0: []
            Clause(
                head=Struct("p", (List((Atom("a"),), Atom("[]")),)), body=()
            ),  # 1: [a]
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2: X
            Clause(
                head=Struct("p", (List((Atom("b"),), Var(1, "T")),)), body=()
            ),  # 3: [b|T]
        ]
        idx = build_from_clauses(clauses)

        # Query: p([c,d])
        goal = Struct("p", (List((Atom("c"), Atom("d")), Atom("[]")),))
        store = Store()
        selected = list(idx.select(("p", 1), goal, store))

        # Should return clauses 1, 2, 3 (non-empty lists and variable)
        assert len(selected) == 3
        assert selected[0] is idx.clauses[(("p", 1), 1)]  # p([a])
        assert selected[1] is idx.clauses[(("p", 1), 2)]  # p(X)
        assert selected[2] is idx.clauses[(("p", 1), 3)]  # p([b|T])

    def test_select_for_zero_arity_predicate(self):
        """Select should handle predicates with no arguments."""
        clauses = [
            Clause(head=Atom("p"), body=()),  # p/0
            Clause(
                head=Struct("p", (Atom("a"),)), body=()
            ),  # p/1 (different predicate)
        ]
        idx = build_from_clauses(clauses)

        # Query: p (zero-arity)
        goal = Atom("p")
        store = Store()
        selected = list(idx.select(("p", 0), goal, store))

        # Should return only the p/0 clause
        assert len(selected) == 1
        assert selected[0] is idx.clauses[(("p", 0), 0)]

    def test_select_returns_empty_for_missing_predicate(self):
        """Select should return empty for non-existent predicate."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
        ]
        idx = build_from_clauses(clauses)

        # Query: q(a) - predicate q doesn't exist
        goal = Struct("q", (Atom("a"),))
        store = Store()
        selected = list(idx.select(("q", 1), goal, store))

        assert len(selected) == 0


class TestOrderPreservation:
    """Tests for critical order preservation invariant."""

    def test_order_preservation_with_interleaved_var_heads(self):
        """Variable heads must not disrupt source order."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 1
            Clause(head=Struct("p", (Atom("b"),)), body=()),  # 2
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Query p(a) must try clauses 0 then 1 (not skip to 2)
        goal_a = Struct("p", (Atom("a"),))
        selected_a = list(idx.select(("p", 1), goal_a, store))
        assert len(selected_a) == 2
        assert selected_a[0] is idx.clauses[(("p", 1), 0)]  # p(a) first
        assert selected_a[1] is idx.clauses[(("p", 1), 1)]  # p(X) second

        # Query p(b) must try clauses 1 then 2 (not 0)
        goal_b = Struct("p", (Atom("b"),))
        selected_b = list(idx.select(("p", 1), goal_b, store))
        assert len(selected_b) == 2
        assert selected_b[0] is idx.clauses[(("p", 1), 1)]  # p(X) first
        assert selected_b[1] is idx.clauses[(("p", 1), 2)]  # p(b) second

    def test_source_order_maintained_within_candidates(self):
        """Source order must be strictly maintained within candidates."""
        clauses = [
            Clause(head=Struct("p", (Struct("f", (Atom("1"),)),)), body=()),  # 0
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 1
            Clause(head=Struct("p", (Struct("f", (Atom("2"),)),)), body=()),  # 2
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 3
            Clause(head=Struct("p", (Struct("f", (Atom("3"),)),)), body=()),  # 4
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Query: p(f(_))
        goal = Struct("p", (Struct("f", (Var(1, "_"),)),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should return clauses 0, 2, 3, 4 in that exact order
        assert len(selected) == 4
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # f(1)
        assert selected[1] is idx.clauses[(("p", 1), 2)]  # f(2)
        assert selected[2] is idx.clauses[(("p", 1), 3)]  # X
        assert selected[3] is idx.clauses[(("p", 1), 4)]  # f(3)

    def test_never_concatenate_buckets(self):
        """Selection must filter through order, never concatenate buckets."""
        clauses = [
            Clause(head=Struct("p", (Int(1),)), body=()),  # 0
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 1
            Clause(head=Struct("p", (Int(2),)), body=()),  # 2
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 3
            Clause(head=Struct("p", (Int(3),)), body=()),  # 4
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Query: p(5) - integer goal
        goal = Struct("p", (Int(5),))
        selected = list(idx.select(("p", 1), goal, store))

        # Must be [0, 2, 3, 4] in that order (not [0,2,4] then [3])
        assert len(selected) == 4
        assert selected[0] is idx.clauses[(("p", 1), 0)]
        assert selected[1] is idx.clauses[(("p", 1), 2)]
        assert selected[2] is idx.clauses[(("p", 1), 3)]
        assert selected[3] is idx.clauses[(("p", 1), 4)]

    def test_variable_clauses_always_included(self):
        """Variable clauses must always be included when matching."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 1
            Clause(head=Struct("p", (Int(42),)), body=()),  # 2
            Clause(head=Struct("p", (Var(1, "Y"),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Any goal should include both variable clauses
        goals = [
            Struct("p", (Atom("a"),)),
            Struct("p", (Int(99),)),
            Struct("p", (List((), Atom("[]")),)),
            Struct("p", (Struct("f", (Atom("x"),)),)),
        ]

        for goal in goals:
            selected = list(idx.select(("p", 1), goal, store))
            p_key = ("p", 1)
            pred = idx.preds[p_key]
            selected_ids = [
                cid for cid in pred.order if idx.clauses[(p_key, cid)] in selected
            ]
            # Both variable clauses (1 and 3) must be included
            assert 1 in selected_ids
            assert 3 in selected_ids

    def test_order_intersect_candidates_pattern(self):
        """Verify the Order ∩ Candidates pattern is correctly implemented."""
        clauses = [
            Clause(head=Struct("p", (Atom("x"),)), body=()),  # 0
            Clause(head=Struct("p", (Var(0, "V"),)), body=()),  # 1
            Clause(head=Struct("p", (Atom("y"),)), body=()),  # 2
            Clause(head=Struct("p", (Atom("x"),)), body=()),  # 3
            Clause(head=Struct("p", (Var(1, "W"),)), body=()),  # 4
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Query: p(x)
        goal = Struct("p", (Atom("x"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Candidates are {0, 1, 3, 4} (x atoms and vars)
        # Order is [0, 1, 2, 3, 4]
        # Result should be [0, 1, 3, 4] - order filtered by candidates
        assert len(selected) == 4
        assert selected[0] is idx.clauses[(("p", 1), 0)]
        assert selected[1] is idx.clauses[(("p", 1), 1)]
        assert selected[2] is idx.clauses[(("p", 1), 3)]
        assert selected[3] is idx.clauses[(("p", 1), 4)]


class TestDereferencingBeforeSelection:
    """Tests for dereferencing first argument before selection."""

    def test_selection_uses_dereferenced_first_argument(self):
        """Selection must use dereferenced value of first argument."""
        clauses = [
            Clause(head=Struct("s", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("s", (Atom("b"),)), body=()),  # 1
            Clause(head=Struct("s", (Var(0, "X"),)), body=()),  # 2
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Create a variable and bind it to 'a'
        var_id = store.new_var("Y")
        bind(store, var_id, Atom("a"), [])

        # Query: s(Y) where Y is bound to 'a'
        goal = Struct("s", (Var(var_id, "Y"),))
        selected = list(idx.select(("s", 1), goal, store))

        # Should select s(a) and s(X) clauses, not all clauses
        assert len(selected) == 2
        assert selected[0] is idx.clauses[(("s", 1), 0)]  # s(a)
        assert selected[1] is idx.clauses[(("s", 1), 2)]  # s(X)

    def test_bound_variables_select_correct_bucket(self):
        """Bound variables should select based on their bound value."""
        clauses = [
            Clause(head=Struct("p", (Int(1),)), body=()),  # 0
            Clause(head=Struct("p", (Int(2),)), body=()),  # 1
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 2
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Bind variable to integer
        var_id = store.new_var("Z")
        bind(store, var_id, Int(5), [])

        # Query: p(Z) where Z is bound to 5
        goal = Struct("p", (Var(var_id, "Z"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select integer clauses and variable clause
        assert len(selected) == 3
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p(1)
        assert selected[1] is idx.clauses[(("p", 1), 1)]  # p(2)
        assert selected[2] is idx.clauses[(("p", 1), 3)]  # p(X)

    def test_unbound_variables_select_all_clauses(self):
        """Unbound variables should select all clauses."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Int(1),)), body=()),  # 1
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # 2
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Unbound variable
        var_id = store.new_var("U")

        # Query: p(U) where U is unbound
        goal = Struct("p", (Var(var_id, "U"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select all clauses
        assert len(selected) == 4
        for i in range(4):
            assert selected[i] is idx.clauses[(("p", 1), i)]

    def test_deref_of_attributed_variable_treated_as_variable(self):
        """Attributed variables should be treated as unbound for selection."""
        # This test is a placeholder for when attributed variables are implemented
        # For now, we test that regular variables work correctly
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 1
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Create an unbound variable (simulating attributed var)
        var_id = store.new_var("Attr")

        # Query: p(Attr) - should select all clauses
        goal = Struct("p", (Var(var_id, "Attr"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select all clauses (treated as unbound)
        assert len(selected) == 2
        assert selected[0] is idx.clauses[(("p", 1), 0)]
        assert selected[1] is idx.clauses[(("p", 1), 1)]

    def test_deref_chains_followed_correctly(self):
        """Dereferencing should follow union-find chains correctly."""
        clauses = [
            Clause(head=Struct("p", (Atom("final"),)), body=()),  # 0
            Clause(head=Struct("p", (Atom("other"),)), body=()),  # 1
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 2
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Create a chain of variables: V1 -> V2 -> V3 -> 'final'
        v1 = store.new_var("V1")
        v2 = store.new_var("V2")
        v3 = store.new_var("V3")

        bind(store, v3, Atom("final"), [])
        bind(store, v2, Var(v3, "V3"), [])
        bind(store, v1, Var(v2, "V2"), [])

        # Query: p(V1) - should deref to 'final'
        goal = Struct("p", (Var(v1, "V1"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select p(final) and p(X)
        assert len(selected) == 2
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p(final)
        assert selected[1] is idx.clauses[(("p", 1), 2)]  # p(X)

    def test_runtime_binding_respected(self):
        """Runtime bindings must be respected in selection."""
        clauses = [
            Clause(head=Struct("test", (Var(0, "Y"),)), body=()),  # 0
            Clause(head=Struct("test", (Atom("a"),)), body=()),  # 1
            Clause(head=Struct("test", (Atom("b"),)), body=()),  # 2
        ]
        idx = build_from_clauses(clauses)

        # First query with Y bound to 'a'
        store1 = Store()
        var_y = store1.new_var("Y")
        bind(store1, var_y, Atom("a"), [])

        goal1 = Struct("test", (Var(var_y, "Y"),))
        selected1 = list(idx.select(("test", 1), goal1, store1))

        # Should select test(Y) and test(a)
        assert len(selected1) == 2
        assert selected1[0] is idx.clauses[(("test", 1), 0)]
        assert selected1[1] is idx.clauses[(("test", 1), 1)]

        # Second query with Y bound to 'b'
        store2 = Store()
        var_y2 = store2.new_var("Y")
        bind(store2, var_y2, Atom("b"), [])

        goal2 = Struct("test", (Var(var_y2, "Y"),))
        selected2 = list(idx.select(("test", 1), goal2, store2))

        # Should select test(Y) and test(b)
        assert len(selected2) == 2
        assert selected2[0] is idx.clauses[(("test", 1), 0)]
        assert selected2[1] is idx.clauses[(("test", 1), 2)]


class TestGoalVariableHandling:
    """Tests for handling goal variables in various edge cases."""

    def test_goal_var_not_in_store(self):
        """Goal variable not in store is treated as unbound."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Int(1),)), body=()),
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),
        ]
        idx = build_from_clauses(clauses)
        store = Store()  # Empty store

        # Query with variable ID that doesn't exist in store
        goal = Struct("p", (Var(999, "NonExistent"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select all clauses (treated as unbound)
        assert len(selected) == 3
        for i in range(3):
            assert selected[i] is idx.clauses[(("p", 1), i)]

    def test_canonical_list_goal_both_representations(self):
        """Both List and canonical '.'/2 goal representations work equivalently."""
        clauses = [
            Clause(head=Struct("p", (Atom("[]"),)), body=()),  # 0: empty
            Clause(head=Struct("p", (List((Atom("a"),), None),)), body=()),  # 1: [a]
            Clause(
                head=Struct("p", (Struct(".", (Atom("b"), Atom("[]"))),)), body=()
            ),  # 2: .(b,[])
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 3: var
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Query with List representation
        goal1 = Struct("p", (List((Atom("c"),), None),))
        selected1 = list(idx.select(("p", 1), goal1, store))

        # Query with canonical '.'/2 representation
        goal2 = Struct("p", (Struct(".", (Atom("c"), Atom("[]"))),))
        selected2 = list(idx.select(("p", 1), goal2, store))

        # Both should select the same clauses: 1, 2, and 3 (non-empty lists and var)
        assert len(selected1) == len(selected2) == 3
        assert selected1[0] is selected2[0] is idx.clauses[(("p", 1), 1)]
        assert selected1[1] is selected2[1] is idx.clauses[(("p", 1), 2)]
        assert selected1[2] is selected2[2] is idx.clauses[(("p", 1), 3)]


class TestStreamingSemantics:
    """Tests for streaming/generator behavior of select()."""

    def test_select_returns_iterator_not_list(self):
        """select() should return an iterator/generator, not a list."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        goal = Struct("p", (Atom("a"),))
        result = idx.select(("p", 1), goal, store)

        # Should be an iterator/generator, not a list
        assert hasattr(result, "__iter__")
        assert hasattr(result, "__next__") or isinstance(result, types.GeneratorType)
        # Should not be a list
        assert not isinstance(result, list)

    def test_lazy_evaluation_with_early_termination(self):
        """select() should support lazy evaluation for early termination."""
        # This test verifies that we can stop iteration early
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 0
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # 1
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 2
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        goal = Struct("p", (Atom("a"),))
        selected_iter = idx.select(("p", 1), goal, store)

        # Take only first two elements
        first = next(selected_iter)
        second = next(selected_iter)

        assert first is idx.clauses[(("p", 1), 0)]
        assert second is idx.clauses[(("p", 1), 1)]

        # Iterator should still have more elements, but we don't consume them
        # This simulates a cut operation that stops after finding first solution

    def test_select_for_nonempty_list_goal_canonical_dot(self):
        """Select should handle canonical '.'/2 form for list goals."""
        clauses = [
            Clause(head=Struct("p", (List((Atom("a"),), Atom("[]")),)), body=()),  # [a]
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # X
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # []
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Goal using canonical '.'/2 structure for [c]
        goal = Struct("p", (Struct(".", (Atom("c"), Atom("[]"))),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select non-empty list and variable clauses
        assert len(selected) == 2
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p([a])
        assert selected[1] is idx.clauses[(("p", 1), 1)]  # p(X)

    def test_select_for_empty_list_goal_atom_form(self):
        """Select should handle Atom('[]') form for empty list goals."""
        clauses = [
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()),  # []
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),  # X
            Clause(head=Struct("p", (List((Atom("a"),), Atom("[]")),)), body=()),  # [a]
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Goal using Atom('[]') form
        goal = Struct("p", (Atom("[]"),))
        selected = list(idx.select(("p", 1), goal, store))

        # Should select empty list and variable clauses
        assert len(selected) == 2
        assert selected[0] is idx.clauses[(("p", 1), 0)]  # p([])
        assert selected[1] is idx.clauses[(("p", 1), 1)]  # p(X)

    def test_variable_clauses_preserve_relative_order(self):
        """Multiple variable clauses must preserve their source order."""
        clauses = [
            Clause(head=Struct("p", (Var(0, "A"),)), body=()),  # 0
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 1
            Clause(head=Struct("p", (Var(1, "B"),)), body=()),  # 2
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # 3
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Query: p(a) - should match specific atoms and all variables
        goal = Struct("p", (Atom("a"),))
        selected = list(idx.select(("p", 1), goal, store))

        p_key = ("p", 1)
        pred = idx.preds[p_key]
        selected_ids = [
            cid for cid in pred.order if idx.clauses[(p_key, cid)] in selected
        ]

        # Order should be [0, 1, 2, 3] - all clauses match
        # Variables 0 and 2 should maintain their relative order
        assert selected_ids == [0, 1, 2, 3]
        assert selected[0] is idx.clauses[(p_key, 0)]  # p(A) - first var
        assert selected[1] is idx.clauses[(p_key, 1)]  # p(a) - first match
        assert selected[2] is idx.clauses[(p_key, 2)]  # p(B) - second var
        assert selected[3] is idx.clauses[(p_key, 3)]  # p(a) - second match

    def test_selection_respects_arity(self):
        """Selection must respect predicate arity boundaries."""
        clauses = [
            Clause(head=Atom("p"), body=()),  # p/0
            Clause(head=Struct("p", (Atom("a"),)), body=()),  # p/1
            Clause(head=Struct("p", (Atom("a"), Atom("b"))), body=()),  # p/2
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        # Select for p/0
        sel0 = list(idx.select(("p", 0), Atom("p"), store))
        assert len(sel0) == 1
        assert sel0[0] is idx.clauses[(("p", 0), 0)]

        # Select for p/1
        sel1 = list(idx.select(("p", 1), Struct("p", (Atom("a"),)), store))
        assert len(sel1) == 1
        assert sel1[0] is idx.clauses[(("p", 1), 0)]

        # Select for p/2
        sel2 = list(idx.select(("p", 2), Struct("p", (Atom("a"), Atom("b"))), store))
        assert len(sel2) == 1
        assert sel2[0] is idx.clauses[(("p", 2), 0)]

    def test_select_returns_fresh_iterator_each_time(self):
        """Each call to select should return a fresh iterator."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Var(0, "X"),)), body=()),
            Clause(head=Struct("p", (Atom("a"),)), body=()),
        ]
        idx = build_from_clauses(clauses)
        store = Store()

        goal = Struct("p", (Atom("a"),))

        # First call
        sel1 = list(idx.select(("p", 1), goal, store))

        # Second call - should get fresh iterator
        sel2 = list(idx.select(("p", 1), goal, store))

        # Both should yield the same results
        assert len(sel1) == len(sel2) == 3
        assert sel1 == sel2

        # But calling select again should give a new iterator
        iter1 = idx.select(("p", 1), goal, store)
        iter2 = idx.select(("p", 1), goal, store)
        assert iter1 is not iter2


class TestComprehensiveTypeDetection:
    """Comprehensive type detection tests for all term types."""

    def test_detect_atom_type_comprehensive(self):
        """Comprehensive atom type detection."""
        store = Store()

        # Regular atoms
        head = Struct("p", (Atom("hello"),))
        assert analyze_first_arg(head, store) == ("atom", "hello", 0)

        head = Struct("q", (Atom("world"),))
        assert analyze_first_arg(head, store) == ("atom", "world", 0)

        # Atom with special characters
        head = Struct("r", (Atom("foo_bar"),))
        assert analyze_first_arg(head, store) == ("atom", "foo_bar", 0)

    def test_detect_struct_type_comprehensive(self):
        """Comprehensive struct type detection."""
        store = Store()

        # Simple struct
        head = Struct("p", (Struct("f", (Int(1),)),))
        assert analyze_first_arg(head, store) == ("struct", "f", 1)

        # Struct with multiple arguments
        head = Struct("p", (Struct("g", (Int(1), Atom("a"), Int(2))),))
        assert analyze_first_arg(head, store) == ("struct", "g", 3)

        # Zero-arity struct (different from atom)
        head = Struct("p", (Struct("h", ()),))
        assert analyze_first_arg(head, store) == ("struct", "h", 0)

    def test_detect_all_integer_variants(self):
        """Test detection of positive, negative, and zero integers."""
        store = Store()

        # Positive
        head = Struct("p", (Int(42),))
        assert analyze_first_arg(head, store) == "int"

        # Negative
        head = Struct("p", (Int(-17),))
        assert analyze_first_arg(head, store) == "int"

        # Zero
        head = Struct("p", (Int(0),))
        assert analyze_first_arg(head, store) == "int"

        # Large positive
        head = Struct("p", (Int(1000000),))
        assert analyze_first_arg(head, store) == "int"

        # Large negative
        head = Struct("p", (Int(-1000000),))
        assert analyze_first_arg(head, store) == "int"


class TestListTypeBucketSeparation:
    """Tests for empty vs non-empty list bucket separation."""

    def test_empty_list_forms_all_go_to_same_bucket(self):
        """All empty list representations go to the same bucket."""
        clauses = [
            Clause(Struct("p", (Atom("[]"),)), Atom("empty1")),  # Atom form
            Clause(Struct("p", (List((), None),)), Atom("empty2")),  # PrologList form
            Clause(
                Struct("p", (List((), Atom("[]")),)), Atom("empty3")
            ),  # PrologList with [] tail
        ]

        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]

        # All should be in empty_list_ids
        assert 0 in pred_idx.empty_list_ids
        assert 1 in pred_idx.empty_list_ids
        assert 2 in pred_idx.empty_list_ids

        # None should be in other buckets
        assert len(pred_idx.list_nonempty_ids) == 0
        assert len(pred_idx.struct_functor) == 0

    def test_canonical_dot_form_is_nonempty_list(self):
        """Canonical '.'/2 structure is recognized as non-empty list."""
        clauses = [
            Clause(Struct("p", (Struct(".", (Int(1), Atom("[]"))),)), Atom("dot1")),
            Clause(
                Struct(
                    "p",
                    (Struct(".", (Atom("a"), Struct(".", (Atom("b"), Atom("[]"))))),),
                ),
                Atom("dot2"),
            ),
            Clause(Struct("p", (List((Int(1),), None),)), Atom("list1")),
        ]

        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]

        # All should be in list_nonempty_ids
        assert 0 in pred_idx.list_nonempty_ids
        assert 1 in pred_idx.list_nonempty_ids
        assert 2 in pred_idx.list_nonempty_ids

        # '.'/2 should NOT be in struct_functor
        assert (".", 2) not in pred_idx.struct_functor

    def test_list_separation_with_variables(self):
        """Variable clauses match both empty and non-empty lists."""
        clauses = [
            Clause(Struct("p", (Var(0, "X"),)), Atom("var1")),
            Clause(Struct("p", (Atom("[]"),)), Atom("empty")),
            Clause(Struct("p", (List((Int(1),), None),)), Atom("nonempty")),
            Clause(Struct("p", (Var(1, "Y"),)), Atom("var2")),
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Query with empty list
        goal = Struct("p", (Atom("[]"),))
        results = list(idx.select(("p", 1), goal, store))
        bodies = [r.body.name for r in results]
        assert bodies == ["var1", "empty", "var2"]  # Both vars and empty

        # Query with non-empty list
        goal = Struct("p", (List((Int(1),), None),))
        results = list(idx.select(("p", 1), goal, store))
        bodies = [r.body.name for r in results]
        assert bodies == ["var1", "nonempty", "var2"]  # Both vars and nonempty

    def test_improper_list_head_treated_as_nonempty(self):
        """Improper lists (cons with non-list tail) are treated as non-empty lists."""
        clauses = [
            # [1|42] - tail is not a list
            Clause(Struct("p", (Struct(".", (Int(1), Int(42))),)), Atom("improper")),
            Clause(Struct("p", (Atom("[]"),)), Atom("empty")),
            Clause(Struct("p", (List((Int(2),), None),)), Atom("proper")),
        ]

        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]

        # Improper list should be in list_nonempty_ids
        assert 0 in pred_idx.list_nonempty_ids

        # Should NOT be in struct_functor (even though it's '.'/2)
        assert (".", 2) not in pred_idx.struct_functor

        # Verify selection works correctly
        store = Store()
        goal = Struct("p", (Struct(".", (Int(3), Atom("x"))),))  # Another improper list
        results = list(idx.select(("p", 1), goal, store))
        bodies = [r.body.name for r in results]
        # Should match improper and proper (both non-empty lists)
        assert bodies == ["improper", "proper"]


class TestFunctorArityBuckets:
    """Tests for functor/arity discrimination in bucket assignment."""

    def test_functor_arity_combinations(self):
        """Test all combinations of functor/arity discrimination."""
        clauses = [
            Clause(Struct("p", (Struct("f", ()),)), Atom("f0")),
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("f1")),
            Clause(Struct("p", (Struct("f", (Int(1), Int(2))),)), Atom("f2")),
            Clause(Struct("p", (Struct("g", ()),)), Atom("g0")),
            Clause(Struct("p", (Struct("g", (Int(1),)),)), Atom("g1")),
        ]

        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]

        # Check correct bucket assignment
        assert 0 in pred_idx.struct_functor[("f", 0)]
        assert 1 in pred_idx.struct_functor[("f", 1)]
        assert 2 in pred_idx.struct_functor[("f", 2)]
        assert 3 in pred_idx.struct_functor[("g", 0)]
        assert 4 in pred_idx.struct_functor[("g", 1)]

        # Verify each functor/arity has exactly its clauses
        assert pred_idx.struct_functor[("f", 0)] == {0}
        assert pred_idx.struct_functor[("f", 1)] == {1}
        assert pred_idx.struct_functor[("f", 2)] == {2}
        assert pred_idx.struct_functor[("g", 0)] == {3}
        assert pred_idx.struct_functor[("g", 1)] == {4}

    def test_functors_never_cross_match(self):
        """Different functors never match even with same arity."""
        clauses = [
            Clause(Struct("p", (Struct("foo", (Int(1),)),)), Atom("foo1")),
            Clause(Struct("p", (Struct("bar", (Int(1),)),)), Atom("bar1")),
            Clause(Struct("p", (Struct("baz", (Int(1),)),)), Atom("baz1")),
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Query for foo/1 should only match foo/1
        goal = Struct("p", (Struct("foo", (Int(2),)),))
        results = list(idx.select(("p", 1), goal, store))
        assert len(results) == 1
        assert results[0].body.name == "foo1"

        # Query for bar/1 should only match bar/1
        goal = Struct("p", (Struct("bar", (Int(2),)),))
        results = list(idx.select(("p", 1), goal, store))
        assert len(results) == 1
        assert results[0].body.name == "bar1"

    def test_atom_vs_zero_arity_struct_disambiguation(self):
        """Atom f and zero-arity struct f() should ideally be distinct but currently share bucket."""
        clauses = [
            Clause(Struct("p", (Atom("f"),)), Atom("atom_f")),
            Clause(Struct("p", (Struct("f", ()),)), Atom("struct_f0")),
            Clause(Struct("p", (Atom("g"),)), Atom("atom_g")),
        ]

        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]

        # Both atom 'f' and struct f() currently share the same bucket key ("f", 0)
        # This is a known limitation - they should ideally be distinct
        assert 0 in pred_idx.struct_functor[("f", 0)]
        assert 1 in pred_idx.struct_functor[("f", 0)]
        assert pred_idx.struct_functor[("f", 0)] == {0, 1}

        store = Store()

        # CURRENT BEHAVIOR: Both match each other (not ideal)
        # Query with zero-arity struct matches both
        goal = Struct("p", (Struct("f", ()),))
        results = list(idx.select(("p", 1), goal, store))
        assert len(results) == 2
        assert [r.body.name for r in results] == ["atom_f", "struct_f0"]

        # Query with atom also matches both
        goal = Struct("p", (Atom("f"),))
        results = list(idx.select(("p", 1), goal, store))
        assert len(results) == 2
        assert [r.body.name for r in results] == ["atom_f", "struct_f0"]

        # TODO: In proper Prolog, f and f() should be distinct:
        # - Atom("f") should only match Atom("f")
        # - Struct("f", ()) should only match Struct("f", ())
        # This test documents current behavior for future fix


class TestEnhancedPredicateIsolation:
    """Enhanced tests for complete predicate isolation guarantees."""

    def test_interleaved_predicates_maintain_isolation(self):
        """Interleaved predicate definitions maintain complete isolation."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("p1")),
            Clause(Struct("q", (Int(1),)), Atom("q1")),
            Clause(Struct("r", (Int(1),)), Atom("r1")),
            Clause(Struct("p", (Int(2),)), Atom("p2")),
            Clause(Struct("q", (Int(2),)), Atom("q2")),
            Clause(Struct("r", (Int(2),)), Atom("r2")),
        ]

        idx = build_from_clauses(clauses)

        # Each predicate should have exactly 2 clauses
        assert len(idx.preds[("p", 1)].order) == 2
        assert len(idx.preds[("q", 1)].order) == 2
        assert len(idx.preds[("r", 1)].order) == 2

        # Clause IDs should be independent per predicate
        assert idx.preds[("p", 1)].order == [0, 1]
        assert idx.preds[("q", 1)].order == [0, 1]
        assert idx.preds[("r", 1)].order == [0, 1]

    def test_same_name_different_arity_complete_isolation(self):
        """Same predicate name with different arities are completely isolated."""
        clauses = [
            Clause(Atom("test"), Atom("test0")),  # test/0
            Clause(Struct("test", (Int(1),)), Atom("test1")),  # test/1
            Clause(Struct("test", (Int(1), Int(2))), Atom("test2")),  # test/2
            Clause(Struct("test", (Int(1), Int(2), Int(3))), Atom("test3")),  # test/3
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Query test/1 should only get test/1 clause
        goal = Struct("test", (Int(5),))
        results = list(idx.select(("test", 1), goal, store))
        assert len(results) == 1
        assert results[0].body.name == "test1"

        # Query test/2 should only get test/2 clause
        goal = Struct("test", (Int(5), Int(6)))
        results = list(idx.select(("test", 2), goal, store))
        assert len(results) == 1
        assert results[0].body.name == "test2"


class TestTypeSwitchingFiltering:
    """Tests for type-based clause filtering and selection."""

    def test_type_switching_completeness(self):
        """Test that type switching correctly filters all type combinations."""
        clauses = [
            Clause(Struct("p", (Var(0, "X"),)), Atom("var")),
            Clause(Struct("p", (Int(42),)), Atom("int")),
            Clause(Struct("p", (Atom("hello"),)), Atom("atom")),
            Clause(Struct("p", (Atom("[]"),)), Atom("empty_list")),
            Clause(Struct("p", (List((Int(1),), None),)), Atom("nonempty_list")),
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("struct")),
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Type-specific queries and their expected matches
        test_cases = [
            (Int(100), ["var", "int"]),  # Integer query
            (Atom("hello"), ["var", "atom"]),  # Exact atom match
            (Atom("world"), ["var"]),  # Non-matching atom
            (Atom("[]"), ["var", "empty_list"]),  # Empty list
            (List((Int(2),), None), ["var", "nonempty_list"]),  # Non-empty list
            (Struct("f", (Int(2),)), ["var", "struct"]),  # Matching functor
            (Struct("g", (Int(1),)), ["var"]),  # Non-matching functor
        ]

        for query_arg, expected_bodies in test_cases:
            goal = Struct("p", (query_arg,))
            results = list(idx.select(("p", 1), goal, store))
            bodies = [r.body.name for r in results]
            assert (
                bodies == expected_bodies
            ), f"For {query_arg}, expected {expected_bodies}, got {bodies}"

    def test_mixed_type_source_order(self):
        """Mixed types preserve exact source order."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("int1")),
            Clause(Struct("p", (Atom("a"),)), Atom("atom1")),
            Clause(Struct("p", (Var(0, "X"),)), Atom("var1")),
            Clause(Struct("p", (List((), Atom("[]")),)), Atom("empty1")),
            Clause(Struct("p", (Int(2),)), Atom("int2")),
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("struct1")),
            Clause(Struct("p", (Var(1, "Y"),)), Atom("var2")),
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Query with integer - should match integers and variables in source order
        goal = Struct("p", (Int(3),))
        results = list(idx.select(("p", 1), goal, store))
        bodies = [r.body.name for r in results]
        assert bodies == ["int1", "var1", "int2", "var2"]

        # Query with unbound variable - should match all in source order
        vid = store.new_var("Z")
        goal = Struct("p", (Var(vid, "Z"),))
        results = list(idx.select(("p", 1), goal, store))
        bodies = [r.body.name for r in results]
        assert bodies == ["int1", "atom1", "var1", "empty1", "int2", "struct1", "var2"]
