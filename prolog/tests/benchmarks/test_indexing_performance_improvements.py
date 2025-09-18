"""
Benchmarks for indexing performance improvements (Issue #95).

These benchmarks measure:
1. Memory allocation in ClauseIndex.select
2. List materialization overhead in Engine
3. Performance with large predicates
"""

import pytest
import time
import tracemalloc
from typing import List, Tuple

from prolog.engine.engine import Engine
from prolog.engine.indexed_program import IndexedProgram
from prolog.ast.clauses import Clause, Program
from prolog.ast.terms import Atom, Struct, Int, Var
from prolog.unify.store import Store


def generate_large_predicate(name: str, num_clauses: int) -> List[Clause]:
    """Generate a predicate with many clauses for performance testing."""
    clauses = []

    # Mix of different first argument types for realistic indexing
    for i in range(num_clauses):
        if i % 4 == 0:
            # Integer first argument
            head = Struct(name, (Int(i), Var(0, "X")))
        elif i % 4 == 1:
            # Atom first argument
            head = Struct(name, (Atom(f"atom_{i}"), Var(0, "X")))
        elif i % 4 == 2:
            # Structure first argument
            head = Struct(name, (Struct("f", (Int(i),)), Var(0, "X")))
        else:
            # Variable first argument
            head = Struct(name, (Var(1, "Y"), Var(0, "X")))

        clauses.append(Clause(head=head, body=()))

    return clauses


class TestIndexingMemoryPerformance:
    """Benchmark memory allocation in indexing operations."""

    def test_select_memory_allocation(self):
        """Measure memory allocated by select() for large predicates."""
        # Create a large predicate
        clauses = generate_large_predicate("test", 10000)
        program = IndexedProgram(tuple(clauses))
        store = Store()

        # Query with integer first arg (matches 25% of clauses)
        goal = Struct("test", (Int(100), Var(0, "Result")))

        # Measure memory before and during select
        tracemalloc.start()

        # Force materialization to measure peak memory
        results = list(program.select(("test", 2), goal, store))

        current, peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()

        # Record baseline memory usage
        print(f"\nBaseline select() memory:")
        print(f"  Current: {current / 1024:.2f} KB")
        print(f"  Peak: {peak / 1024:.2f} KB")
        print(f"  Results: {len(results)} clauses")

        # We expect this to improve after optimization
        # Currently builds union set of ~2500 candidates
        assert peak < 5_000_000  # Less than 5MB for this test

    def test_select_time_first_result(self):
        """Measure time to get first result from select()."""
        # Create a large predicate
        clauses = generate_large_predicate("test", 10000)
        program = IndexedProgram(tuple(clauses))
        store = Store()

        # Query that matches early clauses
        goal = Struct("test", (Int(0), Var(0, "Result")))

        # Measure time to first result
        start = time.perf_counter()

        iterator = program.select(("test", 2), goal, store)
        first = next(iterator, None)

        elapsed = time.perf_counter() - start

        print(f"\nBaseline time to first result: {elapsed * 1000:.3f} ms")
        assert first is not None

        # Should be very fast even with large predicate
        assert elapsed < 0.01  # Less than 10ms


class TestEngineListMaterialization:
    """Benchmark list materialization in Engine."""

    def test_engine_materialization_overhead(self):
        """Measure overhead of materializing full clause list."""
        # Create program with large predicate
        clauses = generate_large_predicate("test", 5000)
        clauses.extend([
            # Add a simple goal to query
            Clause(head=Struct("go", ()), body=(
                Struct("test", (Int(100), Var(0, "X"))),
            ))
        ])

        program = Program(tuple(clauses))

        # Measure with indexing (currently materializes list)
        engine_indexed = Engine(program, use_indexing=True)

        start = time.perf_counter()
        results_indexed = list(engine_indexed.query("go"))
        time_indexed = time.perf_counter() - start

        # Measure without indexing for comparison
        engine_no_index = Engine(program, use_indexing=False)

        start = time.perf_counter()
        results_no_index = list(engine_no_index.query("go"))
        time_no_index = time.perf_counter() - start

        print(f"\nQuery execution time:")
        print(f"  With indexing: {time_indexed * 1000:.3f} ms")
        print(f"  Without indexing: {time_no_index * 1000:.3f} ms")
        print(f"  Speedup: {time_no_index / time_indexed:.2f}x")

        # Results should be the same
        assert len(results_indexed) == len(results_no_index)


class TestLargeIntegerPredicates:
    """Benchmark performance with predicates having many integer clauses."""

    def test_integer_heavy_predicate(self):
        """Test performance when most clauses have integer first args."""
        clauses = []

        # Create 10000 clauses, 90% with integer first args
        for i in range(9000):
            clauses.append(Clause(
                head=Struct("data", (Int(i), Atom(f"value_{i}"))),
                body=()
            ))

        # Add some variable clauses
        for i in range(1000):
            clauses.append(Clause(
                head=Struct("data", (Var(0, "X"), Atom(f"var_{i}"))),
                body=()
            ))

        program = IndexedProgram(tuple(clauses))
        store = Store()

        # Query with specific integer
        goal = Struct("data", (Int(5000), Var(1, "Value")))

        # Measure memory for this query
        tracemalloc.start()
        results = list(program.select(("data", 2), goal, store))
        current, peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()

        print(f"\nInteger-heavy predicate:")
        print(f"  Clauses: 10000 (90% integers)")
        print(f"  Memory peak: {peak / 1024:.2f} KB")
        print(f"  Matches: {len(results)}")

        # Should match all integer clauses (type-based indexing) + variable clauses
        assert len(results) == 10000  # 9000 integers + 1000 variables


def test_negative_integer_indexing():
    """Test that negative integers are indexed same as positive."""
    clauses = [
        Clause(head=Struct("test", (Int(-5), Atom("negative"))), body=()),
        Clause(head=Struct("test", (Int(5), Atom("positive"))), body=()),
        Clause(head=Struct("test", (Int(0), Atom("zero"))), body=()),
        Clause(head=Struct("test", (Var(0, "X"), Atom("var"))), body=()),
    ]

    program = IndexedProgram(tuple(clauses))
    store = Store()

    # Query with negative integer
    goal_neg = Struct("test", (Int(-5), Var(1, "Result")))
    results_neg = list(program.select(("test", 2), goal_neg, store))

    # Query with positive integer
    goal_pos = Struct("test", (Int(5), Var(1, "Result")))
    results_pos = list(program.select(("test", 2), goal_pos, store))

    print(f"\nNegative integer indexing:")
    print(f"  Negative query matches: {len(results_neg)}")
    print(f"  Positive query matches: {len(results_pos)}")

    # All should match all integer clauses (type-based) + variable clause
    assert len(results_neg) == 4  # 3 integers + 1 variable
    assert len(results_pos) == 4  # 3 integers + 1 variable


if __name__ == "__main__":
    # Run benchmarks
    print("=" * 60)
    print("INDEXING PERFORMANCE BENCHMARKS - BASELINE")
    print("=" * 60)

    # Memory tests
    mem_test = TestIndexingMemoryPerformance()
    mem_test.test_select_memory_allocation()
    mem_test.test_select_time_first_result()

    # Engine tests
    engine_test = TestEngineListMaterialization()
    engine_test.test_engine_materialization_overhead()

    # Integer tests
    int_test = TestLargeIntegerPredicates()
    int_test.test_integer_heavy_predicate()

    # Negative integer test
    test_negative_integer_indexing()

    print("\n" + "=" * 60)
    print("BASELINE BENCHMARKS COMPLETE")
    print("=" * 60)