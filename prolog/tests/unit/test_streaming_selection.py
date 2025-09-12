"""
Tests for streaming selection behavior.

These tests verify generator semantics at the API boundary; they don't attempt 
to prove internal memory monotonicity.

Tests ensure that:
1. select() returns an iterator/generator, not a list
2. Streaming behavior with cut - only first candidate visited
3. Memory profile stays constant with large candidate sets
"""

import pytest
from collections.abc import Iterator
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.engine.indexing import ClauseIndex, build_from_clauses
from prolog.unify.store import Store


class TestStreamingSelection:
    """Test suite for streaming selection semantics."""
    
    def test_select_returns_iterator_not_list(self):
        """select() should return an iterator/generator, not a list."""
        clauses = [
            Clause(Struct("fact", (Int(i),)), ()) for i in range(10)
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Get the result of select()
        result = idx.select(("fact", 1), Struct("fact", (Var(0, "X"),)), store)
        
        # Verify it's an iterator/generator, not a list
        assert isinstance(result, Iterator)
        assert not isinstance(result, (list, tuple))
        assert iter(result) is result  # Iterator identity
        
        # Verify we can iterate over it
        count = 0
        for clause in result:
            assert isinstance(clause, Clause)
            count += 1
        assert count == 10
        
        # Verify iterator is exhausted after consumption
        result2 = idx.select(("fact", 1), Struct("fact", (Var(1, "Y"),)), store)
        items = list(result2)
        assert len(items) == 10
        
        # Try to iterate again - should get nothing (iterator exhausted)
        more_items = list(result2)
        assert len(more_items) == 0
    
    def test_streaming_with_cut_early_termination(self):
        """Streaming behavior with cut - only first candidate should be visited."""
        # Create many clauses
        clauses = [
            Clause(Struct("cuttest", (Int(i),)), ())
            for i in range(100)  # Many clauses
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Get iterator for all clauses
        iterator = idx.select(("cuttest", 1), Struct("cuttest", (Var(0, "X"),)), store)
        
        # Simulate cut behavior: only take first result
        first_clause = next(iterator)
        assert first_clause is not None
        
        # In real cut, iterator would be discarded here
        # Verify that remaining clauses were not materialized
        # (This demonstrates lazy evaluation)
        
        # If select() were returning a list, all 100 clauses would be in memory
        # With streaming, only accessed clauses are processed
        
        # Get a fresh iterator and consume only first 3
        iterator2 = idx.select(("cuttest", 1), Struct("cuttest", (Var(1, "Y"),)), store)
        first_three = []
        for i, clause in enumerate(iterator2):
            first_three.append(clause)
            if i == 2:
                break  # Simulate cut after 3
        
        assert len(first_three) == 3
        # Remaining 97 clauses never materialized
    
    @pytest.mark.slow
    def test_streaming_large_candidate_set_smoke(self):
        """Streaming smoke test for large candidate sets."""
        # Create a large predicate
        num_clauses = 100000
        
        # Build in chunks to avoid memory spike during construction
        clauses = []
        for i in range(0, num_clauses, 1000):
            chunk = [
                Clause(Struct("large", (Int(j),)), ())
                for j in range(i, min(i + 1000, num_clauses))
            ]
            clauses.extend(chunk)
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Test that we can iterate without loading all into memory
        goal = Struct("large", (Var(0, "X"),))
        
        # Process in streaming fashion
        count = 0
        max_count = 10  # Only process first 10 despite 100k available
        
        for clause in idx.select(("large", 1), goal, store):
            count += 1
            if count >= max_count:
                break  # Early termination
        
        assert count == max_count
        
        # Verify we can do multiple selections without memory accumulation
        for trial in range(5):
            trial_count = 0
            for clause in idx.select(("large", 1), goal, store):
                trial_count += 1
                if trial_count >= max_count:
                    break
            assert trial_count == max_count
    
    def test_generator_preserves_order(self):
        """Generator should yield clauses in source order."""
        clauses = [
            Clause(Struct("ordered", (Atom("first"),)), Int(1)),
            Clause(Struct("ordered", (Var(0, "X"),)), Int(2)),  # Matches everything
            Clause(Struct("ordered", (Atom("third"),)), Int(3)),
            Clause(Struct("ordered", (Int(4),)), Int(4)),
            Clause(Struct("ordered", (Var(1, "Y"),)), Int(5)),  # Another var clause
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query with unbound variable - should get all clauses in order
        results = list(idx.select(("ordered", 1), Struct("ordered", (Var(2, "Z"),)), store))
        assert len(results) == 5
        assert results[0].body.value == 1
        assert results[1].body.value == 2
        assert results[2].body.value == 3
        assert results[3].body.value == 4
        assert results[4].body.value == 5
        
        # Query with specific atom - should get matching clauses in order
        results = list(idx.select(("ordered", 1), Struct("ordered", (Atom("first"),)), store))
        assert len(results) == 3  # first, and two var clauses
        assert results[0].body.value == 1  # Exact match
        assert results[1].body.value == 2  # First var clause
        assert results[2].body.value == 5  # Second var clause
    
    def test_empty_iterator_for_no_matches(self):
        """Should return empty iterator when no clauses match."""
        clauses = [
            Clause(Struct("sparse", (Int(1),)), ()),
            Clause(Struct("sparse", (Int(2),)), ()),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Query for non-existent predicate
        result = idx.select(("nonexistent", 1), Struct("nonexistent", (Int(1),)), store)
        items = list(result)
        assert len(items) == 0
        
        # Query for integer value - gets all integer clauses (type-based)
        result = idx.select(("sparse", 1), Struct("sparse", (Int(99),)), store)
        items = list(result)
        assert len(items) == 2  # Both sparse(1) and sparse(2)
        
        # Query with wrong arity
        result = idx.select(("sparse", 2), Struct("sparse", (Int(1), Int(2),)), store)
        items = list(result)
        assert len(items) == 0
    
    def test_exhaustion_and_fresh_iterator(self):
        """Test iterator exhaustion and fresh iterator creation."""
        clauses = [Clause(Struct("fact", (Int(i),)), ()) for i in range(5)]
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Test exhaustion
        it = idx.select(("fact", 1), Struct("fact", (Var(0, "X"),)), store)
        assert iter(it) is it
        list(it)  # exhaust
        with pytest.raises(StopIteration):
            next(it)
        
        # Each call returns a fresh iterator
        it1 = idx.select(("fact", 1), Struct("fact", (Var(1, "Y"),)), store)
        it2 = idx.select(("fact", 1), Struct("fact", (Var(2, "Z"),)), store)
        assert list(it1) == list(it2)
    
    def test_zero_arity_yields_all_in_order(self):
        """Test zero-arity predicate streaming."""
        clauses = [Clause(Atom("p"), ()), Clause(Atom("p"), ()), Clause(Atom("p"), ())]
        idx = build_from_clauses(clauses)
        store = Store()
        it = idx.select(("p", 0), Atom("p"), store)
        assert isinstance(it, Iterator)
        assert len(list(it)) == 3
    
    def test_malformed_goal_returns_empty(self):
        """Test malformed goal shape returns empty iterator."""
        clauses = [Clause(Struct("sparse", (Int(1),)), ())]
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Wrong shape: Atom instead of Struct for non-zero arity
        it = idx.select(("sparse", 1), Atom("sparse"), store)
        assert list(it) == []
    
    def test_streaming_for_nonempty_list_goal_canonical_dot(self):
        """Test streaming for canonical list goal '.'/2."""
        clauses = [
            Clause(Struct("p", (PrologList((Int(1),), Atom("[]")),)), ()),
            Clause(Struct("p", (Var(0, "X"),)), ()),
        ]
        idx = build_from_clauses(clauses)
        store = Store()
        goal = Struct("p", (Struct(".", (Int(9), Atom("[]"))),))  # [9]
        it = idx.select(("p", 1), goal, store)
        assert isinstance(it, Iterator)
        first = next(it)
        assert isinstance(first, Clause)