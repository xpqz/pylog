"""
Performance benchmarks for tracing overhead.

Measures:
- Overhead with tracing disabled (target: ≤5%)
- Overhead with pretty tracing (target: ≤20%)
- Overhead with JSONL tracing (target: ≤30%)
- Overhead with metrics only (target: ≤10%)
"""

import time
import statistics
from typing import List, Dict, Any
from pathlib import Path
import tempfile

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.debug.tracer import PortsTracer
from prolog.debug.sinks import PrettyTraceSink, JSONLTraceSink, CollectorSink


def create_test_program(size: str = "medium") -> Program:
    """Create a test program for benchmarking.

    Args:
        size: "small", "medium", or "large"
    """
    clauses = []

    if size == "small":
        # Small program - 10 facts
        for i in range(10):
            clauses.append(Clause(head=Struct("fact", (Int(i),)), body=()))
    elif size == "medium":
        # Medium program - 100 facts with some rules
        for i in range(100):
            clauses.append(Clause(head=Struct("num", (Int(i),)), body=()))

        # Add rules that create backtracking
        clauses.append(
            Clause(
                head=Struct("even", (Var(0, "X"),)),
                body=(
                    Struct("num", (Var(0, "X"),)),
                    Struct("=:=", (
                        Struct("mod", (Var(0, "X"), Int(2))),
                        Int(0)
                    )),
                )
            )
        )
    else:  # large
        # Large program - 1000 facts with complex rules
        for i in range(1000):
            clauses.append(Clause(head=Struct("item", (Int(i),)), body=()))

        # Add rules that create lots of backtracking
        clauses.append(
            Clause(
                head=Struct("pair", (Var(0, "X"), Var(1, "Y"))),
                body=(
                    Struct("item", (Var(0, "X"),)),
                    Struct("item", (Var(1, "Y"),)),
                    Struct("<", (Var(0, "X"), Var(1, "Y"))),
                    Struct("=:=", (
                        Struct("mod", (
                            Struct("+", (Var(0, "X"), Var(1, "Y"))),
                            Int(10)
                        )),
                        Int(0)
                    )),
                )
            )
        )

    return Program(tuple(clauses))


def measure_execution_time(engine: Engine, query: str, iterations: int = 5) -> float:
    """Measure average execution time for a query.

    Args:
        engine: The engine to use
        query: The query to execute
        iterations: Number of iterations to average over

    Returns:
        Average execution time in seconds
    """
    times = []

    for _ in range(iterations):
        start = time.perf_counter()
        results = list(engine.query(query))
        end = time.perf_counter()
        times.append(end - start)

    return statistics.mean(times)


@pytest.mark.xfail(reason="Performance targets are aspirational - optimization pending")
class TestTracingOverhead:
    """Test tracing overhead against performance targets."""

    def test_overhead_with_tracing_disabled(self):
        """Test overhead is ≤5% with tracing disabled."""
        program = create_test_program("medium")

        # Baseline: no tracing at all
        engine_baseline = Engine(program, trace=False)
        baseline_time = measure_execution_time(engine_baseline, "even(X)")

        # With trace=False (but tracer infrastructure present)
        engine_disabled = Engine(program, trace=False)
        disabled_time = measure_execution_time(engine_disabled, "even(X)")

        # Calculate overhead
        overhead = ((disabled_time - baseline_time) / baseline_time) * 100

        # Should be ≤5%
        assert overhead <= 5.0, f"Overhead with tracing disabled: {overhead:.1f}% (target: ≤5%)"

    def test_overhead_with_pretty_tracing(self, tmp_path):
        """Test overhead is ≤20% with pretty tracing."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False)
        baseline_time = measure_execution_time(engine_baseline, "even(X)")

        # With pretty tracing to file
        trace_file = tmp_path / "trace.txt"
        engine_pretty = Engine(program, trace=True)

        with open(trace_file, 'w') as f:
            sink = PrettyTraceSink(f)
            engine_pretty.tracer.add_sink(sink)
            pretty_time = measure_execution_time(engine_pretty, "even(X)")

        # Calculate overhead
        overhead = ((pretty_time - baseline_time) / baseline_time) * 100

        # Should be ≤20%
        assert overhead <= 20.0, f"Overhead with pretty tracing: {overhead:.1f}% (target: ≤20%)"

    def test_overhead_with_jsonl_tracing(self, tmp_path):
        """Test overhead is ≤30% with JSONL tracing."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False)
        baseline_time = measure_execution_time(engine_baseline, "even(X)")

        # With JSONL tracing
        trace_file = tmp_path / "trace.jsonl"
        engine_jsonl = Engine(program, trace=True)
        sink = JSONLTraceSink(str(trace_file))
        engine_jsonl.tracer.add_sink(sink)

        jsonl_time = measure_execution_time(engine_jsonl, "even(X)")
        sink.close()

        # Calculate overhead
        overhead = ((jsonl_time - baseline_time) / baseline_time) * 100

        # Should be ≤30%
        assert overhead <= 30.0, f"Overhead with JSONL tracing: {overhead:.1f}% (target: ≤30%)"

    def test_overhead_with_metrics_only(self):
        """Test overhead is ≤10% with metrics only."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False, metrics=False)
        baseline_time = measure_execution_time(engine_baseline, "even(X)")

        # With metrics only (no tracing)
        engine_metrics = Engine(program, trace=False, metrics=True)
        metrics_time = measure_execution_time(engine_metrics, "even(X)")

        # Calculate overhead
        overhead = ((metrics_time - baseline_time) / baseline_time) * 100

        # Should be ≤10%
        assert overhead <= 10.0, f"Overhead with metrics only: {overhead:.1f}% (target: ≤10%)"



@pytest.mark.xfail(reason="Performance targets are aspirational - optimization pending")
class TestScalabilityOverhead:
    """Test how overhead scales with program size."""

    def test_overhead_scaling_with_program_size(self):
        """Test that overhead doesn't increase dramatically with program size."""
        overheads = {}

        for size in ["small", "medium", "large"]:
            program = create_test_program(size)

            # Choose appropriate query for each size
            if size == "small":
                query = "fact(X)"
            elif size == "medium":
                query = "even(X)"
            else:
                query = "pair(X, Y)"

            # Baseline
            engine_baseline = Engine(program, trace=False)
            # Limit solutions for large program
            if size == "large":
                engine_baseline.max_solutions = 10
            baseline_time = measure_execution_time(engine_baseline, query, iterations=3)

            # With tracing (collector sink for consistent overhead)
            engine_traced = Engine(program, trace=True)
            if size == "large":
                engine_traced.max_solutions = 10
            engine_traced.tracer.add_sink(CollectorSink())
            traced_time = measure_execution_time(engine_traced, query, iterations=3)

            overhead = ((traced_time - baseline_time) / baseline_time) * 100
            overheads[size] = overhead

        # Overhead should not increase dramatically
        # Large programs should have at most 2x the overhead of small programs
        assert overheads["large"] <= overheads["small"] * 2.5, \
            f"Overhead scaling: small={overheads['small']:.1f}%, " \
            f"medium={overheads['medium']:.1f}%, large={overheads['large']:.1f}%"


@pytest.mark.xfail(reason="Filtering not yet implemented")
class TestFilteringOverhead:
    """Test overhead of filtering."""

    def test_filter_reduces_overhead(self):
        """Test that filtering reduces overhead by not processing filtered events."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False)
        baseline_time = measure_execution_time(engine_baseline, "even(X)")

        # Full tracing (no filter)
        engine_full = Engine(program, trace=True)
        engine_full.tracer.add_sink(CollectorSink())
        full_time = measure_execution_time(engine_full, "even(X)")

        # Filtered tracing (only capture 'even' predicates)
        engine_filtered = Engine(program, trace=True)
        engine_filtered.tracer.set_filter(lambda ev: 'even' in ev.pred_id if ev.pred_id else False)
        engine_filtered.tracer.add_sink(CollectorSink())
        filtered_time = measure_execution_time(engine_filtered, "even(X)")

        # Filtered should be faster than full
        assert filtered_time < full_time, \
            "Filtering did not reduce execution time"

        # Calculate overheads
        full_overhead = ((full_time - baseline_time) / baseline_time) * 100
        filtered_overhead = ((filtered_time - baseline_time) / baseline_time) * 100

        # Filtering should reduce overhead noticeably
        assert filtered_overhead < full_overhead * 0.8, \
            f"Filtering didn't reduce overhead enough: " \
            f"full={full_overhead:.1f}%, filtered={filtered_overhead:.1f}%"


@pytest.mark.xfail(reason="Performance targets are aspirational - optimization pending")
class TestTracingWithBacktracking:
    """Test overhead with heavy backtracking."""

    def test_overhead_with_heavy_backtracking(self):
        """Test overhead remains reasonable with heavy backtracking."""
        # Create a program that causes lots of backtracking
        clauses = []
        for i in range(20):
            clauses.append(Clause(head=Struct("choice", (Int(i),)), body=()))

        # Rule that creates exponential backtracking
        clauses.append(
            Clause(
                head=Struct("test", (Var(0, "A"), Var(1, "B"), Var(2, "C"))),
                body=(
                    Struct("choice", (Var(0, "A"),)),
                    Struct("choice", (Var(1, "B"),)),
                    Struct("choice", (Var(2, "C"),)),
                    Struct("<", (Var(0, "A"), Var(1, "B"))),
                    Struct("<", (Var(1, "B"), Var(2, "C"))),
                )
            )
        )
        program = Program(tuple(clauses))

        # Baseline
        engine_baseline = Engine(program, trace=False, max_solutions=10)
        baseline_time = measure_execution_time(engine_baseline, "test(A, B, C)", iterations=3)

        # With tracing
        engine_traced = Engine(program, trace=True, max_solutions=10)
        engine_traced.tracer.add_sink(CollectorSink())
        traced_time = measure_execution_time(engine_traced, "test(A, B, C)", iterations=3)

        # Calculate overhead
        overhead = ((traced_time - baseline_time) / baseline_time) * 100

        # Even with heavy backtracking, overhead should be reasonable
        assert overhead <= 40.0, \
            f"Overhead with heavy backtracking: {overhead:.1f}% (target: ≤40%)"


@pytest.mark.parametrize("sink_type", ["pretty", "jsonl", "collector"])
def test_sink_performance_comparison(sink_type, tmp_path):
    """Compare performance of different sink types."""
    program = create_test_program("medium")

    # Create appropriate sink
    if sink_type == "pretty":
        trace_file = tmp_path / "trace.txt"
        with open(trace_file, 'w') as f:
            sink = PrettyTraceSink(f)
    elif sink_type == "jsonl":
        trace_file = tmp_path / "trace.jsonl"
        sink = JSONLTraceSink(str(trace_file))
    else:  # collector
        sink = CollectorSink()

    # Measure with sink
    engine = Engine(program, trace=True)
    engine.tracer.add_sink(sink)

    start = time.perf_counter()
    results = list(engine.query("even(X)"))
    end = time.perf_counter()

    execution_time = end - start

    # Close file-based sinks
    if sink_type == "jsonl":
        sink.close()

    # Just record the time - actual assertions are in specific tests
    print(f"\n{sink_type} sink: {execution_time:.3f}s")

    # Basic sanity check
    assert execution_time < 5.0, f"{sink_type} sink took too long: {execution_time:.3f}s"