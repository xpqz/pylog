"""
Unit tests for Stage 2: Indexing infrastructure.

Tests the core indexing data structures and algorithms for first-argument
indexing with type switching.

Note on list representations:
- Empty lists can be represented as List((), Atom("[]")) or Atom('[]')
- Non-empty lists can be List((head, ...), tail) or canonical Struct('.', (head, tail))
- Stage 2 indexing must handle both representations correctly
"""

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
        clause = Clause(
            head=Struct("foo", (Atom("a"),)),
            body=()
        )
        
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
            Clause(head=Struct("p", (Atom("a"),)), body=()),     # atom
            Clause(head=Struct("p", (List((), Atom("[]")),)), body=()), # []
            Clause(head=Struct("p", (List((Atom("h"),), Var(1, "T")),)), body=()), # [h|T]
            Clause(head=Struct("p", (Int(42),)), body=()),       # int
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

    @pytest.mark.xfail(reason="Float support deferred to future stage")
    def test_float_bucket_placeholder(self):
        """Float bucket should exist but be marked as future work."""
        idx = PredIndex()
        assert hasattr(idx, "float_ids")
        # For now, floats would raise NotImplementedError
        store = Store()
        from prolog.ast.terms import Float  # May not exist yet
        head = Struct("p", (Float(3.14),))
        with pytest.raises(NotImplementedError):
            analyze_first_arg(head, store)


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
            Clause(head=Struct("p", (Atom("[]"),)), body=()),      # The atom '[]'
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

    def test_predicate_key_includes_arity(self):
        """Predicate keys must distinguish by arity (p/0 vs p/1)."""
        clauses = [
            Clause(head=Atom("p"), body=()),                                 # p/0
            Clause(head=Struct("p", (Atom("a"),)), body=()),                 # p/1
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
            Clause(head=Struct("p", (Var(0,"X"),)), body=()),                 # id 2 (var)
        ]
        idx = build_from_clauses(clauses)
        p = idx.preds[("p", 1)]
        assert p.struct_functor[("f", 1)] == {0, 1}
        assert p.order == [0, 1, 2]  # still in source order

    def test_canonical_dot_struct_counts_as_list_nonempty(self):
        """Canonical '.'/2 structure should be treated as non-empty list."""
        clauses = [
            Clause(head=Struct("p", (Struct(".", (Atom("h"), Atom("[]"))),)), body=()),  # [h]
        ]
        idx = build_from_clauses(clauses)
        p = idx.preds[("p", 1)]
        assert 0 in p.list_nonempty_ids
        assert ("." , 2) not in p.struct_functor  # not a generic struct bucket

    def test_clause_mapping_populated_for_all_ids(self):
        """idx.clauses should contain entries for all (pred_key, id) pairs."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Var(0,"X"),)), body=()),
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