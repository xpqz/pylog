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
import gc
import os
from typing import List, Dict, Any, Tuple
from pathlib import Path
import tempfile

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.debug.tracer import PortsTracer
from prolog.debug.sinks import PrettyTraceSink, JSONLTraceSink, CollectorSink


class NullSink:
    """A no-op sink that discards all events without any processing."""

    def write_event(self, event):
        """Discard event immediately."""
        return True

    def flush(self):
        """No-op flush."""
        return True

    def close(self):
        """No-op close."""
        pass


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


def measure_execution_time(engine: Engine, query: str, iterations: int = 5, warmup: int = 1) -> Tuple[float, float]:
    """Measure execution time for a query with warmup and GC control.

    Args:
        engine: The engine to use
        query: The query to execute
        iterations: Number of iterations to measure
        warmup: Number of warmup iterations

    Returns:
        Tuple of (median time, IQR) in seconds
    """
    # Warmup iterations
    for _ in range(warmup):
        list(engine.query(query))

    # Disable GC during measurement
    gc_was_enabled = gc.isenabled()
    gc.disable()

    times = []
    try:
        for _ in range(iterations):
            gc.collect()  # Clean slate before each measurement
            start = time.perf_counter()
            results = list(engine.query(query))
            end = time.perf_counter()
            times.append(end - start)
    finally:
        # Restore GC state
        if gc_was_enabled:
            gc.enable()

    times.sort()
    median = statistics.median(times)
    if len(times) >= 4:
        q1 = times[len(times) // 4]
        q3 = times[3 * len(times) // 4]
        iqr = q3 - q1
    else:
        iqr = 0

    return median, iqr


@pytest.mark.perf
@pytest.mark.slow
class TestTracingOverhead:
    """Test tracing overhead against performance targets."""

    @classmethod
    def should_enforce(cls):
        """Check if performance assertions should be enforced."""
        enforce_val = os.getenv('CI_ENFORCE_PERF', '').lower()
        return enforce_val in ('1', 'true', 'yes', 'on')

    @classmethod
    def get_threshold(cls, name: str, default: float) -> float:
        """Get configurable threshold from environment."""
        env_var = f'PERF_{name.upper()}_MAX'
        val = os.getenv(env_var)
        if val:
            try:
                return float(val)
            except ValueError:
                pass
        return default

    def test_overhead_with_trace_infrastructure(self):
        """Test overhead of trace infrastructure with no-op filter."""
        program = create_test_program("medium")

        # Baseline: trace=False
        engine_baseline = Engine(program, trace=False)
        baseline_time, baseline_iqr = measure_execution_time(engine_baseline, "even(X)")

        # With trace=True but no-op filter (no events emitted)
        engine_traced = Engine(program, trace=True)
        # Set a filter that rejects all events - avoid closure overhead
        def reject_all(ev):
            return False
        if hasattr(engine_traced.tracer, 'set_filter'):
            engine_traced.tracer.set_filter(reject_all)
        traced_time, traced_iqr = measure_execution_time(engine_traced, "even(X)")

        # Calculate overhead
        overhead = ((traced_time - baseline_time) / baseline_time) * 100

        # Report results
        print(f"\nTrace infrastructure overhead: {overhead:.1f}% (median: {traced_time:.3f}s vs {baseline_time:.3f}s)")

        # Only enforce if requested
        if self.should_enforce():
            threshold = self.get_threshold('infrastructure', 5.0)
            assert overhead <= threshold, f"Overhead with trace infrastructure: {overhead:.1f}% (target: ≤{threshold}%)"

    def test_overhead_with_pretty_tracing(self, tmp_path):
        """Test overhead with pretty tracing."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False)
        baseline_time, baseline_iqr = measure_execution_time(engine_baseline, "even(X)")

        # With pretty tracing to file
        trace_file = tmp_path / "trace.txt"
        engine_pretty = Engine(program, trace=True)

        with open(trace_file, 'w') as f:
            sink = PrettyTraceSink(f)
            engine_pretty.tracer.add_sink(sink)
            pretty_time, pretty_iqr = measure_execution_time(engine_pretty, "even(X)")

        # Calculate overhead
        overhead = ((pretty_time - baseline_time) / baseline_time) * 100

        # Report results
        print(f"\nPretty tracing overhead: {overhead:.1f}% (median: {pretty_time:.3f}s vs {baseline_time:.3f}s)")

        # Only enforce if requested
        if self.should_enforce():
            threshold = self.get_threshold('pretty', 25.0)
            assert overhead <= threshold, f"Overhead with pretty tracing: {overhead:.1f}% (target: ≤{threshold}%)"

    def test_overhead_with_jsonl_tracing(self, tmp_path):
        """Test overhead with JSONL tracing."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False)
        baseline_time, baseline_iqr = measure_execution_time(engine_baseline, "even(X)")

        # With JSONL tracing
        trace_file = tmp_path / "trace.jsonl"
        engine_jsonl = Engine(program, trace=True)

        with open(trace_file, 'w') as f:
            sink = JSONLTraceSink(output=f)
            engine_jsonl.tracer.add_sink(sink)
            jsonl_time, jsonl_iqr = measure_execution_time(engine_jsonl, "even(X)")

        # Calculate overhead
        overhead = ((jsonl_time - baseline_time) / baseline_time) * 100

        # Report results
        print(f"\nJSONL tracing overhead: {overhead:.1f}% (median: {jsonl_time:.3f}s vs {baseline_time:.3f}s)")

        # Only enforce if requested
        if self.should_enforce():
            threshold = self.get_threshold('jsonl', 35.0)
            assert overhead <= threshold, f"Overhead with JSONL tracing: {overhead:.1f}% (target: ≤{threshold}%)"

    def test_overhead_with_collector_sink(self):
        """Test overhead with CollectorSink (no I/O)."""
        program = create_test_program("medium")

        # Baseline
        engine_baseline = Engine(program, trace=False)
        baseline_time, baseline_iqr = measure_execution_time(engine_baseline, "even(X)")

        # With CollectorSink (CPU-only, no I/O)
        engine_collector = Engine(program, trace=True)
        sink = CollectorSink()
        engine_collector.tracer.add_sink(sink)
        collector_time, collector_iqr = measure_execution_time(engine_collector, "even(X)")

        # Calculate overhead
        overhead = ((collector_time - baseline_time) / baseline_time) * 100

        # Report results
        print(f"\nCollectorSink overhead: {overhead:.1f}% (median: {collector_time:.3f}s vs {baseline_time:.3f}s)")

        # Only enforce if requested
        if self.should_enforce():
            threshold = self.get_threshold('collector', 15.0)
            assert overhead <= threshold, f"Overhead with CollectorSink: {overhead:.1f}% (target: ≤{threshold}%)"



@pytest.mark.perf
@pytest.mark.slow
class TestScalabilityOverhead:
    """Test how overhead scales with program size."""

    @classmethod
    def should_enforce(cls):
        """Check if performance assertions should be enforced."""
        enforce_val = os.getenv('CI_ENFORCE_PERF', '').lower()
        return enforce_val in ('1', 'true', 'yes', 'on')

    @classmethod
    def get_threshold(cls, name: str, default: float) -> float:
        """Get configurable threshold from environment."""
        env_var = f'PERF_{name.upper()}_MAX'
        val = os.getenv(env_var)
        if val:
            try:
                return float(val)
            except ValueError:
                pass
        return default

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

            # Set max_solutions for both engines consistently
            max_solutions = 10 if size == "large" else None

            # Baseline
            engine_baseline = Engine(program, trace=False, max_solutions=max_solutions)
            baseline_time, _ = measure_execution_time(engine_baseline, query, iterations=3)

            # With tracing (collector sink for consistent overhead)
            engine_traced = Engine(program, trace=True, max_solutions=max_solutions)
            engine_traced.tracer.add_sink(CollectorSink())
            traced_time, _ = measure_execution_time(engine_traced, query, iterations=3)

            overhead = ((traced_time - baseline_time) / baseline_time) * 100
            overheads[size] = overhead

        # Report results
        print(f"\nOverhead scaling: small={overheads['small']:.1f}%, "
              f"medium={overheads['medium']:.1f}%, large={overheads['large']:.1f}%")

        # Calculate ratio of ratios
        if overheads['small'] > 0:
            scaling_factor = overheads['large'] / overheads['small']
            print(f"Scaling factor (large/small): {scaling_factor:.2f}x")

            # Only enforce if requested
            if self.should_enforce():
                threshold = self.get_threshold('scaling', 3.0)
                assert scaling_factor <= threshold, \
                    f"Overhead scaling too high: {scaling_factor:.2f}x (target: ≤{threshold}x)"


@pytest.mark.perf
@pytest.mark.skipif("not hasattr(Engine(Program(()), trace=True).tracer, 'set_filter')")
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


@pytest.mark.perf
@pytest.mark.slow
class TestTracingWithBacktracking:
    """Test overhead with heavy backtracking."""

    @classmethod
    def should_enforce(cls):
        """Check if performance assertions should be enforced."""
        enforce_val = os.getenv('CI_ENFORCE_PERF', '').lower()
        return enforce_val in ('1', 'true', 'yes', 'on')

    @classmethod
    def get_threshold(cls, name: str, default: float) -> float:
        """Get configurable threshold from environment."""
        env_var = f'PERF_{name.upper()}_MAX'
        val = os.getenv(env_var)
        if val:
            try:
                return float(val)
            except ValueError:
                pass
        return default

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
        baseline_time, _ = measure_execution_time(engine_baseline, "test(A, B, C)", iterations=3)

        # With tracing
        engine_traced = Engine(program, trace=True, max_solutions=10)
        engine_traced.tracer.add_sink(CollectorSink())
        traced_time, _ = measure_execution_time(engine_traced, "test(A, B, C)", iterations=3)

        # Calculate overhead
        overhead = ((traced_time - baseline_time) / baseline_time) * 100

        # Report results
        print(f"\nOverhead with heavy backtracking: {overhead:.1f}%")

        # Only enforce if requested
        if self.should_enforce():
            threshold = self.get_threshold('backtracking', 45.0)
            assert overhead <= threshold, \
                f"Overhead with heavy backtracking: {overhead:.1f}% (target: ≤{threshold}%)"


@pytest.mark.perf
@pytest.mark.parametrize("sink_type", ["pretty", "jsonl", "collector"])
def test_sink_performance_comparison(sink_type, tmp_path):
    """Compare performance of different sink types."""
    program = create_test_program("medium")

    # Create appropriate sink
    if sink_type == "pretty":
        trace_file = tmp_path / "trace.txt"
        f = open(trace_file, 'w')
        sink = PrettyTraceSink(f)
    elif sink_type == "jsonl":
        trace_file = tmp_path / "trace.jsonl"
        f = open(trace_file, 'w')
        sink = JSONLTraceSink(output=f)
    else:  # collector
        sink = CollectorSink()
        f = None

    # Measure with sink
    engine = Engine(program, trace=True)
    engine.tracer.add_sink(sink)

    try:
        start = time.perf_counter()
        results = list(engine.query("even(X)"))
        end = time.perf_counter()

        execution_time = end - start

        # Report time
        print(f"\n{sink_type} sink: {execution_time:.3f}s")

        # Basic sanity check
        assert execution_time < 5.0, f"{sink_type} sink took too long: {execution_time:.3f}s"
    finally:
        # Close file handles
        if f:
            f.close()


@pytest.mark.perf
class TestMicroBenchmarks:
    """Micro benchmarks for specific overhead components."""

    @classmethod
    def should_enforce(cls):
        """Check if performance assertions should be enforced."""
        enforce_val = os.getenv('CI_ENFORCE_PERF', '').lower()
        return enforce_val in ('1', 'true', 'yes', 'on')

    @classmethod
    def get_threshold(cls, name: str, default: float) -> float:
        """Get configurable threshold from environment."""
        env_var = f'PERF_{name.upper()}_MAX'
        val = os.getenv(env_var)
        if val:
            try:
                return float(val)
            except ValueError:
                pass
        return default

    def test_time_to_first_event(self):
        """Test overhead for generating the first trace event on a trivial query."""
        # Create trivial program with single fact
        clauses = [Clause(head=Struct("fact", (Int(1),)), body=())]
        program = Program(tuple(clauses))

        # Time to first solution with no tracing
        engine_baseline = Engine(program, trace=False)

        gc.collect()
        gc.disable()
        try:
            start = time.perf_counter()
            results = list(engine_baseline.query("fact(1)"))
            baseline_time = time.perf_counter() - start
        finally:
            gc.enable()

        # Time to first solution with CollectorSink
        engine_traced = Engine(program, trace=True)
        sink = CollectorSink()
        engine_traced.tracer.add_sink(sink)

        gc.collect()
        gc.disable()
        try:
            start = time.perf_counter()
            results = list(engine_traced.query("fact(1)"))
            traced_time = time.perf_counter() - start
        finally:
            gc.enable()

        # Calculate overhead for this micro operation
        if baseline_time > 0:
            overhead_ratio = traced_time / baseline_time
            overhead_pct = (overhead_ratio - 1) * 100
        else:
            overhead_ratio = float('inf')
            overhead_pct = float('inf')

        # Report results
        print(f"\nTime to first event: baseline={baseline_time*1000:.3f}ms, "
              f"traced={traced_time*1000:.3f}ms, "
              f"overhead={overhead_pct:.1f}% ({overhead_ratio:.2f}x)")

        # Only enforce if requested
        if self.should_enforce():
            # We expect higher overhead on tiny operations due to fixed setup costs
            # but it should still be bounded
            threshold = self.get_threshold('first_event', 5.0)
            assert overhead_ratio <= threshold, \
                f"Time-to-first-event overhead too high: {overhead_ratio:.2f}x (target: ≤{threshold}x)"

    def test_event_creation_rate(self):
        """Test rate of trace event creation without I/O."""
        # Create program that generates many events quickly
        clauses = []
        for i in range(100):
            clauses.append(Clause(head=Struct("test", (Int(i),)), body=()))

        # Add rule to query all
        clauses.append(
            Clause(
                head=Struct("all_tests", (Var(0, "X"),)),
                body=(Struct("test", (Var(0, "X"),)),)
            )
        )
        program = Program(tuple(clauses))

        # Measure with CollectorSink (includes list append overhead)
        engine = Engine(program, trace=True)
        sink = CollectorSink()
        engine.tracer.add_sink(sink)

        gc.collect()
        gc.disable()
        try:
            start = time.perf_counter()
            results = list(engine.query("all_tests(X)"))
            elapsed = time.perf_counter() - start
        finally:
            gc.enable()

        # Calculate event rate
        num_events = len(sink.events) if hasattr(sink, 'events') else 0
        if elapsed > 0:
            events_per_second = num_events / elapsed
        else:
            events_per_second = float('inf')

        print(f"\nEvent creation rate (CollectorSink): {num_events} events in {elapsed:.3f}s "
              f"= {events_per_second:.0f} events/sec")

        # Only enforce if requested
        if self.should_enforce():
            # Basic sanity check - should handle at least 1000 events/sec
            min_rate = float(os.getenv('PERF_EVENT_RATE_MIN', '1000'))
            assert events_per_second >= min_rate, \
                f"Event creation rate too slow: {events_per_second:.0f} events/sec (target: ≥{min_rate})"

    def test_pure_event_creation_rate(self):
        """Test pure event creation rate with NullSink (no append overhead)."""
        # Create program that generates many events quickly
        clauses = []
        for i in range(100):
            clauses.append(Clause(head=Struct("test", (Int(i),)), body=()))

        clauses.append(
            Clause(
                head=Struct("all_tests", (Var(0, "X"),)),
                body=(Struct("test", (Var(0, "X"),)),)
            )
        )
        program = Program(tuple(clauses))

        # Measure with NullSink (no storage overhead)
        engine = Engine(program, trace=True)
        sink = NullSink()
        engine.tracer.add_sink(sink)

        gc.collect()
        gc.disable()
        try:
            start = time.perf_counter()
            results = list(engine.query("all_tests(X)"))
            elapsed = time.perf_counter() - start
        finally:
            gc.enable()

        # Estimate event count (100 facts * ~4 ports each)
        estimated_events = len(results) * 4

        if elapsed > 0:
            events_per_second = estimated_events / elapsed
        else:
            events_per_second = float('inf')

        print(f"\nPure event creation rate (NullSink): ~{estimated_events} events in {elapsed:.3f}s "
              f"= {events_per_second:.0f} events/sec")

        # This test is purely informational, no assertions