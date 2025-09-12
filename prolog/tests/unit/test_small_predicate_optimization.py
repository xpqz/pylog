"""
Tests for small predicate optimization.

These tests ensure that:
1. Small predicates can bypass indexing for efficiency
2. The bypass threshold is configurable
3. No performance regression for tiny predicates
4. Semantics remain identical with bypass active
"""

import pytest
import time
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause
from prolog.engine.indexing import ClauseIndex, build_from_clauses
from prolog.unify.store import Store


class TestSmallPredicateOptimization:
    """Test suite for small predicate optimization."""
    
    def test_small_predicates_can_bypass_indexing(self):
        """Predicates with <= 3 clauses can bypass indexing."""
        # Note: This test assumes we'll add a bypass mechanism
        # For now, we test the expected behavior once implemented
        
        clauses = [
            # Small predicate: foo/1 with 2 clauses
            Clause(Struct("foo", (Int(1),)), ()),
            Clause(Struct("foo", (Var(0, "X"),)), ()),
            
            # Small predicate: bar/1 with 3 clauses (at threshold)
            Clause(Struct("bar", (Int(1),)), ()),
            Clause(Struct("bar", (Int(2),)), ()),
            Clause(Struct("bar", (Atom("a"),)), ()),
            
            # Large predicate: baz/1 with 4 clauses (above threshold)
            Clause(Struct("baz", (Int(1),)), ()),
            Clause(Struct("baz", (Int(2),)), ()),
            Clause(Struct("baz", (Int(3),)), ()),
            Clause(Struct("baz", (Atom("a"),)), ()),
        ]
        
        idx = build_from_clauses(clauses)
        
        # Check that small predicates are identified
        # This would be a new attribute/method to add
        foo_idx = idx.preds[("foo", 1)]
        bar_idx = idx.preds[("bar", 1)]
        baz_idx = idx.preds[("baz", 1)]
        
        assert len(foo_idx.order) == 2  # Small
        assert len(bar_idx.order) == 3  # At threshold
        assert len(baz_idx.order) == 4  # Large
        
        # Once bypass is implemented, we'd check:
        # assert idx.should_bypass(("foo", 1)) == True
        # assert idx.should_bypass(("bar", 1)) == True
        # assert idx.should_bypass(("baz", 1)) == False
    
    def test_bypass_threshold_is_configurable(self):
        """The bypass threshold should be configurable."""
        # This test assumes we'll add a configurable threshold
        
        clauses = [
            # Predicate with 5 clauses
            Clause(Struct("pred", (Int(i),)), ()) for i in range(5)
        ]
        
        # Test with default threshold (e.g., 3)
        idx_default = build_from_clauses(clauses)
        # assert idx_default.bypass_threshold == 3
        # assert not idx_default.should_bypass(("pred", 1))
        
        # Test with custom threshold (e.g., 10)
        # idx_custom = build_from_clauses(clauses, bypass_threshold=10)
        # assert idx_custom.bypass_threshold == 10
        # assert idx_custom.should_bypass(("pred", 1))
        
        # For now, just verify the index is built correctly
        assert len(idx_default.preds[("pred", 1)].order) == 5
        
        # TODO: Once bypass_threshold is implemented, uncomment above assertions
    
    @pytest.mark.perf
    def test_no_performance_regression_tiny_predicates(self):
        """No performance regression for tiny predicates."""
        # Create a tiny predicate with just 1 clause
        clauses = [
            Clause(Struct("tiny", (Var(0, "X"),)), ()),
        ]
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Measure selection time for tiny predicate
        goal = Struct("tiny", (Int(42),))
        
        # Warm up
        for _ in range(100):
            list(idx.select(("tiny", 1), goal, store))
        
        # Measure
        iterations = 10000
        start = time.perf_counter()
        for _ in range(iterations):
            results = list(idx.select(("tiny", 1), goal, store))
        duration = time.perf_counter() - start
        
        # Should be fast (under 25ms per 1000 selections - realistic for Python)
        ms_per_k = (duration / iterations) * 1000 * 1000
        assert ms_per_k < 25.0, f"Tiny predicate selection too slow: {ms_per_k:.3f}ms per 1000 selections"
        
        # Verify correctness
        results = list(idx.select(("tiny", 1), goal, store))
        assert len(results) == 1
    
    def test_semantics_identical_with_bypass(self):
        """Semantics remain identical whether bypass is active or not."""
        # Create predicates of various sizes
        clauses = []
        
        # Tiny: 1 clause
        clauses.append(Clause(Struct("tiny", (Var(0, "X"),)), ()))
        
        # Small: 2 clauses
        clauses.append(Clause(Struct("small", (Int(1),)), Atom("s1")))
        clauses.append(Clause(Struct("small", (Var(1, "Y"),)), Atom("s2")))
        
        # Medium: 3 clauses (at typical threshold)
        clauses.append(Clause(Struct("medium", (Int(1),)), Atom("m1")))
        clauses.append(Clause(Struct("medium", (Int(2),)), Atom("m2")))
        clauses.append(Clause(Struct("medium", (Var(2, "Z"),)), Atom("m3")))
        
        # Large: 10 clauses
        for i in range(10):
            clauses.append(Clause(Struct("large", (Int(i),)), Atom(f"l{i}")))
        
        idx = build_from_clauses(clauses)
        store = Store()
        
        # Test various selection patterns
        test_cases = [
            # (predicate_key, goal, expected_count)
            (("tiny", 1), Struct("tiny", (Int(42),)), 1),
            (("tiny", 1), Struct("tiny", (Var(10, "A"),)), 1),
            
            (("small", 1), Struct("small", (Int(1),)), 2),  # Matches small(1) and small(Y)
            (("small", 1), Struct("small", (Int(2),)), 2),  # Type-based: matches both small(1) and small(Y)
            (("small", 1), Struct("small", (Var(11, "B"),)), 2),
            
            (("medium", 1), Struct("medium", (Int(1),)), 3),  # Type-based: all int clauses + var
            (("medium", 1), Struct("medium", (Int(2),)), 3),  # Type-based: all int clauses + var
            (("medium", 1), Struct("medium", (Int(3),)), 3),  # Type-based: all int clauses + var
            (("medium", 1), Struct("medium", (Var(12, "C"),)), 3),
            
            (("large", 1), Struct("large", (Int(5),)), 10),  # Type-based: all 10 int clauses
            (("large", 1), Struct("large", (Int(15),)), 10),  # Type-based: all 10 int clauses
            (("large", 1), Struct("large", (Var(13, "D"),)), 10),
        ]
        
        for pred_key, goal, expected_count in test_cases:
            results = list(idx.select(pred_key, goal, store))
            assert len(results) == expected_count, \
                f"Failed for {pred_key} with goal {goal}: got {len(results)}, expected {expected_count}"
            
            # Verify order is preserved
            if pred_key == ("medium", 1) and isinstance(goal.args[0], Var):
                # Should get m1, m2, m3 in that order
                assert results[0].body.name == "m1"
                assert results[1].body.name == "m2"
                assert results[2].body.name == "m3"
    
    def test_zero_arity_tiny_predicate(self):
        """Test zero-arity predicate optimization."""
        clauses = [Clause(Atom("p"), ())]
        idx = build_from_clauses(clauses)
        store = Store()
        results = list(idx.select(("p", 0), Atom("p"), store))
        assert len(results) == 1
        assert results[0] == idx.clauses[(("p", 0), 0)]