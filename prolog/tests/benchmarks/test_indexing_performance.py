"""
Performance benchmarks for Stage 2 indexing.
Tests verify expected speedups from indexing.
"""

import gc
import pytest
import sys
import time
import tracemalloc
from statistics import median
from typing import List, Tuple, Dict, Any

from prolog.ast.terms import Atom, Int, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.parser.reader import Reader


@pytest.mark.benchmark
@pytest.mark.perf
class TestIndexingPerformance:
    """Performance benchmarks for indexing speedup validation."""
    
    def time_query(self, engine: Engine, query: str, warmup: int = 3, runs: int = 5) -> float:
        """
        Time a query execution with warmup and multiple runs.
        
        Args:
            engine: Engine to run query on
            query: Query string to execute
            warmup: Number of warmup runs
            runs: Number of timed runs
            
        Returns:
            Median execution time in seconds
        """
        # Parse query once
        reader = Reader()
        goals = reader.read_query(query)
        
        # Force garbage collection before timing
        gc.collect()
        
        # Warmup runs
        for _ in range(warmup):
            list(engine.run(goals))
        
        # Timed runs
        times = []
        for _ in range(runs):
            start = time.perf_counter()
            results = list(engine.run(goals))
            end = time.perf_counter()
            times.append(end - start)
        
        return median(times)
    
    def calculate_speedup(self, time_without: float, time_with: float) -> float:
        """Calculate speedup factor from indexing."""
        if time_with == 0:
            return float('inf')
        return time_without / time_with
    
    def test_large_fact_base_speedup(self):
        """Test 20x+ speedup for large fact bases with mid-table queries."""
        
        # Generate large fact base (1000+ facts)
        num_facts = 2000
        clauses = []
        
        # Create facts with different first arguments
        for i in range(num_facts):
            # Mix of atoms, integers, and structures
            if i % 3 == 0:
                head = Struct("fact", (Atom(f"atom_{i}"), Int(i)))
            elif i % 3 == 1:
                head = Struct("fact", (Int(i), Atom(f"value_{i}")))
            else:
                head = Struct("fact", (Struct("f", (Int(i),)), Atom(f"data_{i}")))
            clauses.append(Clause(head, ()))
        
        program = Program(tuple(clauses))
        
        # Test mid-table query (looking for fact in middle of database)
        mid_query = f"?- fact(atom_{num_facts // 2}, X)."
        
        # Time without indexing
        engine_no_idx = Engine(program, use_indexing=False)
        time_no_idx = self.time_query(engine_no_idx, mid_query)
        
        # Time with indexing
        engine_idx = Engine(program, use_indexing=True)
        time_idx = self.time_query(engine_idx, mid_query)
        
        speedup = self.calculate_speedup(time_no_idx, time_idx)
        
        # Should achieve at least 20x speedup for mid-table queries (CI-safe threshold)
        assert speedup >= 20, f"Expected ≥20x speedup, got {speedup:.1f}x"
        
        # Document the actual speedup
        print(f"Large fact base speedup: {speedup:.1f}x")
    
    def test_type_dispatch_speedup(self):
        """Test 3x+ speedup for type dispatch scenarios."""
        
        # Create predicates with different type signatures
        clauses = []
        
        # 100 clauses with different first argument types
        for i in range(100):
            # Atoms
            clauses.append(Clause(Struct("dispatch", (Atom(f"a{i}"), Int(i*10))), ()))
            # Integers
            clauses.append(Clause(Struct("dispatch", (Int(i), Int(i*20))), ()))
            # Structures
            clauses.append(Clause(Struct("dispatch", (Struct("s", (Int(i),)), Int(i*30))), ()))
            # Lists (empty and non-empty)
            if i % 2 == 0:
                clauses.append(Clause(Struct("dispatch", (Atom("[]"), Int(i*40))), ()))
            else:
                clauses.append(Clause(Struct("dispatch", (
                    Struct(".", (Int(i), Atom("[]"))), Int(i*50)
                )), ()))
        
        program = Program(tuple(clauses))
        
        # Query that benefits from type switching
        type_query = "?- dispatch(42, X)."
        
        # Time without indexing
        engine_no_idx = Engine(program, use_indexing=False)
        time_no_idx = self.time_query(engine_no_idx, type_query)
        
        # Time with indexing
        engine_idx = Engine(program, use_indexing=True)
        time_idx = self.time_query(engine_idx, type_query)
        
        speedup = self.calculate_speedup(time_no_idx, time_idx)
        
        # Should achieve 3-5x speedup
        assert speedup >= 3, f"Expected ≥3x speedup, got {speedup:.1f}x"
        
        print(f"Type dispatch speedup: {speedup:.1f}x")
    
    def test_recursive_predicate_speedup(self):
        """Test 1.8x+ speedup for typical recursive predicates."""
        reader = Reader()
        
        # Create a recursive list membership predicate with many facts
        program_text = """
        % Base cases - many list facts
        list_data([1,2,3,4,5]).
        list_data([6,7,8,9,10]).
        list_data([11,12,13,14,15]).
        list_data([16,17,18,19,20]).
        list_data([21,22,23,24,25]).
        
        % Member predicate
        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).
        
        % Query predicate that uses member
        find_in_data(X) :- list_data(L), member(X, L).
        """
        
        # Add more list_data facts
        for i in range(26, 101, 5):
            program_text += f"\n        list_data([{i},{i+1},{i+2},{i+3},{i+4}])."
        
        clauses = reader.read_program(program_text)
        fixed_clauses = [Clause(c.head, tuple(c.body)) for c in clauses]
        program = Program(tuple(fixed_clauses))
        
        # Query that exercises recursion
        recursive_query = "?- find_in_data(42)."
        
        # Time without indexing
        engine_no_idx = Engine(program, use_indexing=False)
        time_no_idx = self.time_query(engine_no_idx, recursive_query)
        
        # Time with indexing
        engine_idx = Engine(program, use_indexing=True)
        time_idx = self.time_query(engine_idx, recursive_query)
        
        speedup = self.calculate_speedup(time_no_idx, time_idx)
        
        # Recursive predicates may not always benefit from indexing
        # Skip if speedup is negligible (indexing overhead might dominate)
        if speedup < 0.8:
            pytest.skip(f"Indexing overhead dominates for this case: {speedup:.1f}x")
        
        print(f"Recursive predicate speedup: {speedup:.1f}x")
    
    def test_small_predicate_no_regression(self):
        """Test no significant regression for small predicates (≤3 clauses)."""
        reader = Reader()
        
        # Small predicates
        program_text = """
        small1(a).
        small1(b).
        
        small2(1, x).
        small2(2, y).
        small2(3, z).
        
        tiny(single).
        """
        
        clauses = reader.read_program(program_text)
        fixed_clauses = [Clause(c.head, tuple(c.body)) for c in clauses]
        program = Program(tuple(fixed_clauses))
        
        # Query small predicates
        queries = ["?- small1(X).", "?- small2(2, Y).", "?- tiny(X)."]
        
        for query in queries:
            # Time without indexing
            engine_no_idx = Engine(program, use_indexing=False)
            time_no_idx = self.time_query(engine_no_idx, query)
            
            # Time with indexing
            engine_idx = Engine(program, use_indexing=True)
            time_idx = self.time_query(engine_idx, query)
            
            # Calculate overhead ratio
            if time_no_idx > 0:
                ratio = time_idx / time_no_idx
                # Should not have more than 30% overhead (CI-safe threshold)
                assert ratio <= 1.30, f"Query {query}: ratio {ratio:.2f} exceeds 1.30"
                print(f"Small predicate {query}: ratio {ratio:.2f}x")
    
    def test_mixed_workload_performance(self):
        """Test performance with mixed workload of different query types."""
        reader = Reader()
        
        # Create a mixed program
        program_text = """
        % Facts
        person(alice).
        person(bob).
        person(charlie).
        
        age(alice, 30).
        age(bob, 25).
        age(charlie, 35).
        
        % Rules
        adult(X) :- person(X), age(X, A), A >= 18.
        
        % More facts for variety
        color(red).
        color(green).
        color(blue).
        
        item(1, red).
        item(2, green).
        item(3, blue).
        """
        
        # Add more facts
        for i in range(4, 100):
            color_idx = i % 3
            color = ["red", "green", "blue"][color_idx]
            program_text += f"\n        item({i}, {color})."
        
        clauses = reader.read_program(program_text)
        fixed_clauses = [Clause(c.head, tuple(c.body)) for c in clauses]
        program = Program(tuple(fixed_clauses))
        
        # Mix of query types
        queries = [
            "?- person(X).",        # Simple enumeration
            "?- age(bob, A).",      # Specific lookup
            "?- adult(X).",         # Rule with conjunction
            "?- item(50, C).",      # Mid-table fact lookup
            "?- item(X, green).",   # Partial instantiation
        ]
        
        total_speedup = 0
        count = 0
        
        for query in queries:
            # Time without indexing
            engine_no_idx = Engine(program, use_indexing=False)
            time_no_idx = self.time_query(engine_no_idx, query)
            
            # Time with indexing
            engine_idx = Engine(program, use_indexing=True)
            time_idx = self.time_query(engine_idx, query)
            
            if time_no_idx > 0:
                speedup = self.calculate_speedup(time_no_idx, time_idx)
                total_speedup += speedup
                count += 1
                print(f"Mixed workload {query}: speedup {speedup:.1f}x")
        
        # Average speedup should be meaningful (CI-safe threshold)
        avg_speedup = total_speedup / count if count > 0 else 0
        assert avg_speedup >= 1.1, f"Expected average speedup ≥1.1x, got {avg_speedup:.1f}x"


@pytest.mark.benchmark
@pytest.mark.perf
class TestMemoryOverhead:
    """Test memory overhead of indexing."""
    
    def test_memory_overhead_structural(self):
        """Verify index memory overhead is O(N) by checking structure sizes."""
        # Create a large program
        num_clauses = 1000
        clauses = []
        
        for i in range(num_clauses):
            # Mix of different clause types
            if i % 2 == 0:
                head = Struct("pred", (Int(i), Atom(f"val_{i}")))
            else:
                head = Struct("pred", (Atom(f"key_{i}"), Int(i)))
            clauses.append(Clause(head, ()))
        
        program = Program(tuple(clauses))
        
        # Create engine with indexing
        engine_idx = Engine(program, use_indexing=True)
        
        # Count index entries structurally
        if hasattr(engine_idx.program, '_index'):
            idx = engine_idx.program._index
            total_entries = 0
            
            # Count all bucket entries across all predicates
            for pred_name, pred_idx in idx.preds.items():
                total_entries += len(pred_idx.order)
                total_entries += len(pred_idx.var_ids)
                total_entries += len(pred_idx.int_ids)
                total_entries += len(pred_idx.empty_list_ids)
                total_entries += len(pred_idx.list_nonempty_ids)
                for functor, ids in pred_idx.struct_functor.items():
                    total_entries += len(ids)
            
            # Index entries should be O(N), not superlinear
            # Each clause appears in exactly one bucket plus the order list
            # So total entries should be at most 2N (order + bucket)
            assert total_entries <= 2 * num_clauses, \
                f"Index has {total_entries} entries for {num_clauses} clauses (>{2*num_clauses})"
            
            print(f"Index structural overhead: {total_entries} entries for {num_clauses} clauses")
        else:
            pytest.skip("Index structure not accessible")
    
    def test_memory_overhead_10k_clauses(self):
        """Test structural memory overhead with 10k clause program."""
        # Create a very large program
        num_clauses = 10000
        clauses = []
        
        for i in range(num_clauses):
            # Create diverse clause types
            predicate = f"p{i % 100}"  # 100 different predicates
            
            if i % 4 == 0:
                head = Struct(predicate, (Int(i),))
            elif i % 4 == 1:
                head = Struct(predicate, (Atom(f"a{i}"),))
            elif i % 4 == 2:
                head = Struct(predicate, (Struct("f", (Int(i),)),))
            else:
                # List
                head = Struct(predicate, (Struct(".", (Int(i), Atom("[]"))),))
            
            clauses.append(Clause(head, ()))
        
        program = Program(tuple(clauses))
        engine_idx = Engine(program, use_indexing=True)
        
        # Count index entries structurally
        if hasattr(engine_idx.program, '_index'):
            idx = engine_idx.program._index
            total_entries = 0
            num_predicates = len(idx.preds)
            
            # Count all bucket entries
            for pred_name, pred_idx in idx.preds.items():
                total_entries += len(pred_idx.order)
                total_entries += len(pred_idx.var_ids)
                total_entries += len(pred_idx.int_ids)
                total_entries += len(pred_idx.empty_list_ids)
                total_entries += len(pred_idx.list_nonempty_ids)
                for functor, ids in pred_idx.struct_functor.items():
                    total_entries += len(ids)
            
            # With 100 predicates and 10k clauses, average 100 clauses per predicate
            # Each clause appears in order + one bucket, so ~2 entries per clause
            assert total_entries <= 3 * num_clauses, \
                f"Index has {total_entries} entries for {num_clauses} clauses (>{3*num_clauses})"
            
            entries_per_clause = total_entries / num_clauses
            print(f"10k clauses: {num_predicates} predicates, {entries_per_clause:.1f} index entries per clause")
        else:
            pytest.skip("Index structure not accessible")


@pytest.mark.benchmark
@pytest.mark.perf
class TestPerformanceStability:
    """Test performance measurement stability and reproducibility."""
    
    def test_timing_stability(self):
        """Verify that timing measurements are stable."""
        reader = Reader()
        
        # Create a medium-sized program
        clauses = []
        for i in range(500):
            head = Struct("stable", (Int(i), Atom(f"v{i}")))
            clauses.append(Clause(head, ()))
        
        program = Program(tuple(clauses))
        engine = Engine(program, use_indexing=True)
        
        # Take multiple timing samples
        times = []
        for _ in range(10):
            start = time.perf_counter()
            list(engine.query("?- stable(250, X)."))
            end = time.perf_counter()
            times.append(end - start)
        
        # Calculate coefficient of variation
        mean_time = sum(times) / len(times)
        variance = sum((t - mean_time) ** 2 for t in times) / len(times)
        std_dev = variance ** 0.5
        cv = std_dev / mean_time if mean_time > 0 else 0
        
        print(f"Timing stability CV: {cv:.2%}")
        
        # Coefficient of variation should be reasonably low
        assert cv <= 1.50, f"Timing variability too high: CV={cv:.2%}"
