"""
Phase 3 tests for Stage 2 indexing: Type Switching and List Separation.

This test suite validates:
1. Type detection for all term types
2. List type separation (empty vs non-empty)
3. Struct functor/arity discrimination
4. Predicate isolation guarantees
5. Type switching correctness
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.unify.store import Store
from prolog.engine.indexing import (
    ClauseIndex, PredIndex, analyze_first_arg, build_from_clauses
)


class TestTypeDetection:
    """Test type detection for first arguments."""
    
    def test_detect_atom_type(self):
        """Detect atom type (non-empty-list)."""
        store = Store()
        
        # Regular atom
        head = Struct("p", (Atom("hello"),))
        result = analyze_first_arg(head, store)
        assert result == ("atom", "hello", 0)
        
        # Another atom
        head = Struct("q", (Atom("world"),))
        result = analyze_first_arg(head, store)
        assert result == ("atom", "world", 0)
    
    def test_detect_empty_list_special_atom(self):
        """Detect empty list as special atom."""
        store = Store()
        
        # Empty list as Atom('[]')
        head = Struct("p", (Atom("[]"),))
        result = analyze_first_arg(head, store)
        assert result == "empty_list"
        
        # Empty list as PrologList with no items
        head = Struct("p", (PrologList((), None),))
        result = analyze_first_arg(head, store)
        assert result == "empty_list"
    
    def test_detect_non_empty_list_type(self):
        """Detect non-empty list type."""
        store = Store()
        
        # Non-empty list as PrologList
        head = Struct("p", (PrologList((Int(1),), None),))
        result = analyze_first_arg(head, store)
        assert result == "list_nonempty"
        
        # Non-empty list in canonical form '.'/2
        head = Struct("p", (Struct(".", (Int(1), Atom("[]"))),))
        result = analyze_first_arg(head, store)
        assert result == "list_nonempty"
    
    def test_detect_integer_type(self):
        """Detect integer type."""
        store = Store()
        
        # Positive integer
        head = Struct("p", (Int(42),))
        result = analyze_first_arg(head, store)
        assert result == "int"
        
        # Negative integer
        head = Struct("p", (Int(-17),))
        result = analyze_first_arg(head, store)
        assert result == "int"
        
        # Zero
        head = Struct("p", (Int(0),))
        result = analyze_first_arg(head, store)
        assert result == "int"
    
    def test_detect_struct_type(self):
        """Detect struct type."""
        store = Store()
        
        # Simple struct
        head = Struct("p", (Struct("f", (Int(1),)),))
        result = analyze_first_arg(head, store)
        assert result == ("struct", "f", 1)
        
        # Struct with multiple arguments
        head = Struct("p", (Struct("g", (Int(1), Atom("a"), Int(2))),))
        result = analyze_first_arg(head, store)
        assert result == ("struct", "g", 3)
        
        # Zero-arity struct (should not be confused with atom)
        head = Struct("p", (Struct("h", ()),))
        result = analyze_first_arg(head, store)
        assert result == ("struct", "h", 0)
    
    def test_detect_variable_type(self):
        """Detect variable type."""
        store = Store()
        
        # Variable
        head = Struct("p", (Var(0, "X"),))
        result = analyze_first_arg(head, store)
        assert result == "var"
        
        # Different variable
        head = Struct("p", (Var(1, "Y"),))
        result = analyze_first_arg(head, store)
        assert result == "var"
    
    def test_zero_arity_predicates(self):
        """Test type detection for zero-arity predicates."""
        store = Store()
        
        # Atom head (zero-arity)
        head = Atom("p")
        result = analyze_first_arg(head, store)
        assert result == "no_args"
        
        # Struct with no arguments
        head = Struct("q", ())
        result = analyze_first_arg(head, store)
        assert result == "no_args"


class TestListTypeSeparation:
    """Test that empty and non-empty lists are properly separated."""
    
    def test_empty_list_separate_bucket(self):
        """[] and [H|T] require separate buckets."""
        clauses = [
            Clause(Struct("p", (Atom("[]"),)), Atom("empty")),
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("nonempty")),
            Clause(Struct("p", (Struct(".", (Int(2), Atom("[]"))),)), Atom("nonempty2")),
        ]
        
        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]
        
        # Check bucket separation
        assert 0 in pred_idx.empty_list_ids
        assert 1 in pred_idx.list_nonempty_ids
        assert 2 in pred_idx.list_nonempty_ids
        
        # Empty list should not be in non-empty bucket
        assert 0 not in pred_idx.list_nonempty_ids
        
        # Non-empty lists should not be in empty bucket
        assert 1 not in pred_idx.empty_list_ids
        assert 2 not in pred_idx.empty_list_ids
    
    def test_empty_list_never_matches_nonempty_clauses(self):
        """[] never matches [H|T] clauses."""
        clauses = [
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("one")),
            Clause(Struct("p", (PrologList((Int(2), Int(3)), None),)), Atom("two_three")),
            Clause(Struct("p", (Struct(".", (Atom("a"), Atom("[]"))),)), Atom("a")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with empty list
        goal = Struct("p", (Atom("[]"),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should get no matches (empty list doesn't match non-empty list clauses)
        assert len(results) == 0
    
    def test_nonempty_list_never_matches_empty_clauses(self):
        """[H|T] never matches [] clauses."""
        clauses = [
            Clause(Struct("p", (Atom("[]"),)), Atom("empty1")),
            Clause(Struct("p", (PrologList((), None),)), Atom("empty2")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with non-empty list
        goal = Struct("p", (PrologList((Int(1),), None),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should get no matches (non-empty list doesn't match empty list clauses)
        assert len(results) == 0
        
        # Query with canonical list form
        goal = Struct("p", (Struct(".", (Int(1), Atom("[]"))),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should still get no matches
        assert len(results) == 0
    
    def test_variable_clauses_match_both_list_types(self):
        """Variable clauses match both list types."""
        clauses = [
            Clause(Struct("p", (Var(0, "X"),)), Atom("var_clause")),
            Clause(Struct("p", (Atom("[]"),)), Atom("empty")),
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("nonempty")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with empty list
        goal = Struct("p", (Atom("[]"),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match variable clause and empty clause
        assert len(results) == 2
        assert results[0].body.name == "var_clause"  # Source order
        assert results[1].body.name == "empty"
        
        # Query with non-empty list
        goal = Struct("p", (PrologList((Int(1),), None),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match variable clause and non-empty clause
        assert len(results) == 2
        assert results[0].body.name == "var_clause"  # Source order
        assert results[1].body.name == "nonempty"


class TestStructFunctorDiscrimination:
    """Test functor/arity discrimination for structures."""
    
    def test_different_functors_different_buckets(self):
        """Different functors use different buckets."""
        clauses = [
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("f_clause")),
            Clause(Struct("p", (Struct("g", (Int(2),)),)), Atom("g_clause")),
            Clause(Struct("p", (Struct("h", (Int(3),)),)), Atom("h_clause")),
        ]
        
        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]
        
        # Check that each functor has its own bucket
        assert ("f", 1) in pred_idx.struct_functor
        assert ("g", 1) in pred_idx.struct_functor
        assert ("h", 1) in pred_idx.struct_functor
        
        # Check bucket contents
        assert 0 in pred_idx.struct_functor[("f", 1)]
        assert 1 in pred_idx.struct_functor[("g", 1)]
        assert 2 in pred_idx.struct_functor[("h", 1)]
        
        # Check no cross-contamination
        assert 0 not in pred_idx.struct_functor[("g", 1)]
        assert 0 not in pred_idx.struct_functor[("h", 1)]
        assert 1 not in pred_idx.struct_functor[("f", 1)]
        assert 1 not in pred_idx.struct_functor[("h", 1)]
    
    def test_same_functor_different_arity_separate_buckets(self):
        """Same functor different arity separate buckets."""
        clauses = [
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("f1")),
            Clause(Struct("p", (Struct("f", (Int(1), Int(2))),)), Atom("f2")),
            Clause(Struct("p", (Struct("f", (Int(1), Int(2), Int(3))),)), Atom("f3")),
        ]
        
        idx = build_from_clauses(clauses)
        pred_idx = idx.preds[("p", 1)]
        
        # Check that each arity has its own bucket
        assert ("f", 1) in pred_idx.struct_functor
        assert ("f", 2) in pred_idx.struct_functor
        assert ("f", 3) in pred_idx.struct_functor
        
        # Check bucket contents
        assert 0 in pred_idx.struct_functor[("f", 1)]
        assert 1 in pred_idx.struct_functor[("f", 2)]
        assert 2 in pred_idx.struct_functor[("f", 3)]
        
        # Check no cross-contamination between arities
        assert 0 not in pred_idx.struct_functor[("f", 2)]
        assert 0 not in pred_idx.struct_functor[("f", 3)]
        assert 1 not in pred_idx.struct_functor[("f", 1)]
        assert 1 not in pred_idx.struct_functor[("f", 3)]
    
    def test_f1_doesnt_match_g1(self):
        """f/1 doesn't match g/1."""
        clauses = [
            Clause(Struct("p", (Struct("g", (Int(1),)),)), Atom("g_clause")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with f/1
        goal = Struct("p", (Struct("f", (Int(1),)),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should get no matches
        assert len(results) == 0
    
    def test_f1_doesnt_match_f2(self):
        """f/1 doesn't match f/2."""
        clauses = [
            Clause(Struct("p", (Struct("f", (Int(1), Int(2))),)), Atom("f2_clause")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with f/1
        goal = Struct("p", (Struct("f", (Int(1),)),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should get no matches
        assert len(results) == 0


class TestPredicateIsolation:
    """Test complete isolation between predicates."""
    
    def test_different_predicates_never_share_buckets(self):
        """Different predicates never share buckets."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("p1")),
            Clause(Struct("q", (Int(1),)), Atom("q1")),
            Clause(Struct("r", (Int(1),)), Atom("r1")),
        ]
        
        idx = build_from_clauses(clauses)
        
        # Check that each predicate has its own index
        assert ("p", 1) in idx.preds
        assert ("q", 1) in idx.preds
        assert ("r", 1) in idx.preds
        
        # Check that indices are separate objects
        assert idx.preds[("p", 1)] is not idx.preds[("q", 1)]
        assert idx.preds[("p", 1)] is not idx.preds[("r", 1)]
        assert idx.preds[("q", 1)] is not idx.preds[("r", 1)]
        
        # Check that buckets are independent
        assert 0 in idx.preds[("p", 1)].int_ids
        assert 0 in idx.preds[("q", 1)].int_ids  # Different 0 for q
        assert 0 in idx.preds[("r", 1)].int_ids  # Different 0 for r
    
    def test_p1_clauses_never_returned_for_q1_query(self):
        """p/1 clauses never returned for q/1 query."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("p1")),
            Clause(Struct("p", (Int(2),)), Atom("p2")),
            Clause(Struct("q", (Int(1),)), Atom("q1")),
            Clause(Struct("q", (Int(2),)), Atom("q2")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query q/1 with integer - returns all q/1 integer clauses
        goal = Struct("q", (Int(1),))
        results = list(idx.select(("q", 1), goal, store))
        
        # Should only get q clauses (both match because they have integer first args)
        assert len(results) == 2
        assert results[0].body.name == "q1"
        assert results[1].body.name == "q2"
        
        # Query p/1 with integer - returns all p/1 integer clauses
        goal = Struct("p", (Int(1),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should only get p clauses (both match because they have integer first args)
        assert len(results) == 2
        assert results[0].body.name == "p1"
        assert results[1].body.name == "p2"
        
        # Verify no cross-contamination: p clauses never returned for q
        q_results = list(idx.select(("q", 1), goal, store))
        for r in q_results:
            assert r.head.functor == "q", "q/1 query returned non-q clause!"
    
    def test_predicate_key_includes_arity(self):
        """Predicate key includes arity."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("p1_clause")),
            Clause(Struct("p", (Int(1), Int(2))), Atom("p2_clause")),
            Clause(Struct("p", (Int(1), Int(2), Int(3))), Atom("p3_clause")),
        ]
        
        idx = build_from_clauses(clauses)
        
        # Check that different arities create different predicates
        assert ("p", 1) in idx.preds
        assert ("p", 2) in idx.preds
        assert ("p", 3) in idx.preds
        
        # Verify they're separate
        assert idx.preds[("p", 1)] is not idx.preds[("p", 2)]
        assert idx.preds[("p", 1)] is not idx.preds[("p", 3)]
        assert idx.preds[("p", 2)] is not idx.preds[("p", 3)]
        
        # Verify clause assignment
        assert len(idx.preds[("p", 1)].order) == 1
        assert len(idx.preds[("p", 2)].order) == 1
        assert len(idx.preds[("p", 3)].order) == 1


class TestTypeSwitching:
    """Test type switching provides correct clause filtering."""
    
    def test_integer_vs_atom_discrimination(self):
        """Integer vs atom discrimination."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("int1")),
            Clause(Struct("p", (Int(2),)), Atom("int2")),
            Clause(Struct("p", (Atom("a"),)), Atom("atom_a")),
            Clause(Struct("p", (Atom("b"),)), Atom("atom_b")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with integer - matches all integer clauses
        goal = Struct("p", (Int(1),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match all integer clauses (indexing returns candidates, not exact matches)
        assert len(results) == 2
        assert results[0].body.name == "int1"
        assert results[1].body.name == "int2"
        
        # Query with atom 'a' - matches clauses with atom 'a'
        goal = Struct("p", (Atom("a"),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match the clause with atom 'a' (exact functor match)
        assert len(results) == 1
        assert results[0].body.name == "atom_a"
        
        # Query with atom 'b' - matches clauses with atom 'b'
        goal = Struct("p", (Atom("b"),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match the clause with atom 'b' (exact functor match)
        assert len(results) == 1
        assert results[0].body.name == "atom_b"
    
    def test_list_vs_struct_discrimination(self):
        """List vs struct discrimination."""
        clauses = [
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("list1")),
            Clause(Struct("p", (PrologList((Int(2), Int(3)), None),)), Atom("list23")),
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("struct_f")),
            Clause(Struct("p", (Struct("g", (Int(2),)),)), Atom("struct_g")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with list
        goal = Struct("p", (PrologList((Int(1),), None),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should only match list clauses
        assert len(results) == 2
        assert results[0].body.name == "list1"
        assert results[1].body.name == "list23"
        
        # Query with struct
        goal = Struct("p", (Struct("f", (Int(1),)),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should only match struct clauses
        assert len(results) == 1
        assert results[0].body.name == "struct_f"
    
    def test_empty_vs_nonempty_list_discrimination(self):
        """Empty vs non-empty list discrimination."""
        clauses = [
            Clause(Struct("p", (Atom("[]"),)), Atom("empty1")),
            Clause(Struct("p", (PrologList((), None),)), Atom("empty2")),
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("nonempty1")),
            Clause(Struct("p", (Struct(".", (Int(2), Atom("[]"))),)), Atom("nonempty2")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with empty list
        goal = Struct("p", (Atom("[]"),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should only match empty list clauses
        assert len(results) == 2
        assert results[0].body.name == "empty1"
        assert results[1].body.name == "empty2"
        
        # Query with non-empty list
        goal = Struct("p", (PrologList((Int(1),), None),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should only match non-empty list clauses
        assert len(results) == 2
        assert results[0].body.name == "nonempty1"
        assert results[1].body.name == "nonempty2"
    
    def test_all_type_combinations(self):
        """All type combinations."""
        clauses = [
            # Variables
            Clause(Struct("p", (Var(0, "X"),)), Atom("var")),
            # Integers
            Clause(Struct("p", (Int(42),)), Atom("int")),
            # Atoms
            Clause(Struct("p", (Atom("hello"),)), Atom("atom")),
            # Empty lists
            Clause(Struct("p", (Atom("[]"),)), Atom("empty_list")),
            # Non-empty lists
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("nonempty_list")),
            # Structures
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("struct")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Test each type matches correctly
        # Note: For atoms, indexing uses exact functor matching in struct_functor bucket
        test_cases = [
            (Int(100), ["var", "int"]),
            (Atom("hello"), ["var", "atom"]),  # Match exact atom 'hello'
            (Atom("[]"), ["var", "empty_list"]),
            (PrologList((Int(2),), None), ["var", "nonempty_list"]),
            (Struct("f", (Int(2),)), ["var", "struct"]),
            (Struct("g", (Int(1),)), ["var"]),  # Different functor
        ]
        
        for arg, expected_bodies in test_cases:
            goal = Struct("p", (arg,))
            results = list(idx.select(("p", 1), goal, store))
            bodies = [r.body.name for r in results]
            assert bodies == expected_bodies, f"For {arg}, expected {expected_bodies}, got {bodies}"
    
    def test_unbound_variable_matches_all(self):
        """Unbound variable in goal matches all clauses."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("int")),
            Clause(Struct("p", (Atom("a"),)), Atom("atom")),
            Clause(Struct("p", (Atom("[]"),)), Atom("empty")),
            Clause(Struct("p", (PrologList((Int(1),), None),)), Atom("list")),
            Clause(Struct("p", (Struct("f", (Int(1),)),)), Atom("struct")),
            Clause(Struct("p", (Var(0, "X"),)), Atom("var")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Create an unbound variable in store
        vid = store.new_var("Y")
        
        # Query with unbound variable
        goal = Struct("p", (Var(vid, "Y"),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match all clauses in source order
        assert len(results) == 6
        expected = ["int", "atom", "empty", "list", "struct", "var"]
        actual = [r.body.name for r in results]
        assert actual == expected


class TestSourceOrderPreservation:
    """Verify that source order is always preserved."""
    
    def test_order_preserved_within_type(self):
        """Source order preserved for clauses of same type."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("first")),
            Clause(Struct("p", (Int(2),)), Atom("second")),
            Clause(Struct("p", (Int(3),)), Atom("third")),
            Clause(Struct("p", (Int(4),)), Atom("fourth")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query that matches all integer clauses
        goal = Struct("p", (Int(5),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should match all in source order
        assert len(results) == 4
        assert [r.body.name for r in results] == ["first", "second", "third", "fourth"]
    
    def test_order_preserved_mixed_types(self):
        """Source order preserved with mixed types."""
        clauses = [
            Clause(Struct("p", (Int(1),)), Atom("int1")),
            Clause(Struct("p", (Var(0, "X"),)), Atom("var1")),
            Clause(Struct("p", (Int(2),)), Atom("int2")),
            Clause(Struct("p", (Var(1, "Y"),)), Atom("var2")),
            Clause(Struct("p", (Int(3),)), Atom("int3")),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with integer (matches int and var clauses)
        goal = Struct("p", (Int(4),))
        results = list(idx.select(("p", 1), goal, store))
        
        # Should preserve source order
        expected = ["int1", "var1", "int2", "var2", "int3"]
        assert [r.body.name for r in results] == expected