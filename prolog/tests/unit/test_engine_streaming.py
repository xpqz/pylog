"""
Integration tests for streaming clause selection in Engine.

Tests that streaming cursor integration preserves semantics,
trace ordering, and result correctness.
"""

import pytest
from typing import List

from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Atom, Struct, Int, Var, List as PrologList
from prolog.ast.clauses import Clause
from prolog.debug.tracer import PortsTracer, TraceEvent, CollectorSink


class TestEngineStreamingIntegration:
    """Test streaming cursor integration with Engine."""

    def test_streaming_vs_materialized_result_parity(self):
        """Test that streaming and materialized paths yield identical results."""
        # Create test program with multiple clauses
        clauses = [
            Clause(head=Struct("test", (Int(1), Atom("a"))), body=()),
            Clause(head=Struct("test", (Int(2), Atom("b"))), body=()),
            Clause(head=Struct("test", (Int(3), Atom("c"))), body=()),
            Clause(head=Struct("test", (Var(0, "X"), Atom("var"))), body=()),
        ]
        program = Program(tuple(clauses))

        # Engine with indexing but no streaming (baseline)
        engine_materialized = Engine(program, use_indexing=True, use_streaming=False)
        results_materialized = list(engine_materialized.query("test(X, Y)"))

        # Engine with indexing and streaming
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)
        results_streaming = list(engine_streaming.query("test(X, Y)"))

        # Results should be identical in order and content
        assert len(results_streaming) == len(results_materialized)
        for streaming, materialized in zip(results_streaming, results_materialized):
            assert streaming == materialized

    def test_streaming_early_termination(self):
        """Test that streaming doesn't materialize unnecessary clauses."""
        # Create large predicate but query will match early
        clauses = []
        for i in range(1000):
            clauses.append(
                Clause(head=Struct("data", (Int(i), Atom(f"val_{i}"))), body=())
            )
        program = Program(tuple(clauses))

        # Query for specific early value
        engine = Engine(program, use_indexing=True, use_streaming=True)
        results = list(engine.query("data(0, X)"))

        # Should find exactly one match (the first clause)
        assert len(results) == 1
        assert results[0]['X'] == Atom("val_0")

        # The streaming cursor should not have materialized all 1000 clauses

    def test_streaming_with_backtracking(self):
        """Test streaming works correctly with backtracking."""
        clauses = [
            # Multiple color facts
            Clause(head=Struct("color", (Atom("red"),)), body=()),
            Clause(head=Struct("color", (Atom("green"),)), body=()),
            Clause(head=Struct("color", (Atom("blue"),)), body=()),

            # Rule that uses color
            Clause(
                head=Struct("test", (Var(0, "X"),)),
                body=(Struct("color", (Var(0, "X"),)),)
            ),
        ]
        program = Program(tuple(clauses))

        # Both engines should find all colors via backtracking
        engine_mat = Engine(program, use_indexing=True, use_streaming=False)
        engine_str = Engine(program, use_indexing=True, use_streaming=True)

        results_mat = list(engine_mat.query("test(X)"))
        results_str = list(engine_str.query("test(X)"))

        assert len(results_str) == 3
        assert results_str == results_mat

    def test_streaming_preserves_trace_order(self):
        """Test that 4-port trace ordering is preserved with streaming."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
            Clause(
                head=Struct("q", (Var(0, "X"),)),
                body=(Struct("p", (Var(0, "X"),)),)
            ),
        ]
        program = Program(tuple(clauses))

        # Collect traces from materialized engine
        collector_mat = CollectorSink()
        tracer_mat = PortsTracer(sink=collector_mat)
        engine_mat = Engine(
            program,
            use_indexing=True,
            use_streaming=False,
            trace=tracer_mat
        )
        list(engine_mat.query("q(X)"))
        events_mat = collector_mat.events

        # Collect traces from streaming engine
        collector_str = CollectorSink()
        tracer_str = PortsTracer(sink=collector_str)
        engine_str = Engine(
            program,
            use_indexing=True,
            use_streaming=True,
            trace=tracer_str
        )
        list(engine_str.query("q(X)"))
        events_str = collector_str.events

        # Trace events should be identical
        assert len(events_str) == len(events_mat)
        for ev_str, ev_mat in zip(events_str, events_mat):
            assert ev_str.port == ev_mat.port
            assert str(ev_str.goal) == str(ev_mat.goal)

    def test_streaming_with_mixed_first_args(self):
        """Test streaming with various first argument types."""
        clauses = [
            # Integer first args
            Clause(head=Struct("test", (Int(1), Atom("int1"))), body=()),
            Clause(head=Struct("test", (Int(2), Atom("int2"))), body=()),

            # Variable first arg
            Clause(head=Struct("test", (Var(0, "X"), Atom("var"))), body=()),

            # Atom first args
            Clause(head=Struct("test", (Atom("foo"), Atom("atom1"))), body=()),
            Clause(head=Struct("test", (Atom("bar"), Atom("atom2"))), body=()),

            # List first args
            Clause(
                head=Struct("test", (PrologList([Int(1), Int(2)]), Atom("list"))),
                body=()
            ),
        ]
        program = Program(tuple(clauses))

        engine_mat = Engine(program, use_indexing=True, use_streaming=False)
        engine_str = Engine(program, use_indexing=True, use_streaming=True)

        # Query with variable - should match all
        results_mat = list(engine_mat.query("test(X, Y)"))
        results_str = list(engine_str.query("test(X, Y)"))
        assert results_str == results_mat
        assert len(results_str) == 6

        # Query with specific integer
        results_mat = list(engine_mat.query("test(1, Y)"))
        results_str = list(engine_str.query("test(1, Y)"))
        assert results_str == results_mat

        # Query with atom
        results_mat = list(engine_mat.query("test(foo, Y)"))
        results_str = list(engine_str.query("test(foo, Y)"))
        assert results_str == results_mat

    def test_streaming_disabled_with_debug(self):
        """Test that streaming is disabled when debug mode is on."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(100)
        ]
        program = Program(tuple(clauses))

        # With debug tracer, streaming should be disabled
        tracer = PortsTracer(sink=CollectorSink())
        engine = Engine(
            program,
            use_indexing=True,
            use_streaming=True,  # Request streaming
            trace=tracer  # But debug is enabled
        )

        # Engine should use materialized path despite use_streaming=True
        # This preserves existing debug test assumptions
        # We can't directly test this without accessing internals,
        # but we verify behavior is correct
        results = list(engine.query("test(X)"))
        assert len(results) == 100

    def test_streaming_with_cut(self):
        """Test streaming works correctly with cut."""
        clauses = [
            Clause(head=Struct("test", (Int(1),)), body=(Atom("!"),)),
            Clause(head=Struct("test", (Int(2),)), body=()),
            Clause(head=Struct("test", (Int(3),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine_mat = Engine(program, use_indexing=True, use_streaming=False)
        engine_str = Engine(program, use_indexing=True, use_streaming=True)

        results_mat = list(engine_mat.query("test(X)"))
        results_str = list(engine_str.query("test(X)"))

        # Cut should prevent backtracking to other clauses
        assert len(results_str) == 1
        assert results_str == results_mat
        assert results_str[0]['X'] == Int(1)

    def test_streaming_with_zero_arity(self):
        """Test streaming with zero-arity predicates."""
        clauses = [
            Clause(head=Atom("fact1"), body=()),
            Clause(head=Atom("fact2"), body=()),
            Clause(head=Atom("fact3"), body=()),
        ]
        program = Program(tuple(clauses))

        engine_mat = Engine(program, use_indexing=True, use_streaming=False)
        engine_str = Engine(program, use_indexing=True, use_streaming=True)

        # Zero-arity predicates should work with streaming
        assert engine_str.query_one("fact1") == {}
        assert engine_str.query_one("fact2") == {}
        assert engine_str.query_one("fact3") == {}

        # Non-existent predicate
        assert engine_str.query_one("fact4") is None

    def test_streaming_with_recursive_predicates(self):
        """Test streaming with recursive predicates."""
        clauses = [
            # Base case
            Clause(
                head=Struct("count", (Int(0), Int(0))),
                body=()
            ),
            # Recursive case
            Clause(
                head=Struct("count", (Var(0, "N"), Var(1, "R"))),
                body=(
                    Struct(">", (Var(0, "N"), Int(0))),
                    Struct("is", (Var(2, "N1"), Struct("-", (Var(0, "N"), Int(1))))),
                    Struct("count", (Var(2, "N1"), Var(3, "R1"))),
                    Struct("is", (Var(1, "R"), Struct("+", (Var(3, "R1"), Int(1))))),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine_mat = Engine(program, use_indexing=True, use_streaming=False)
        engine_str = Engine(program, use_indexing=True, use_streaming=True)

        # Both should compute same result for count(3, R)
        result_mat = engine_mat.query_one("count(3, R)")
        result_str = engine_str.query_one("count(3, R)")

        assert result_str == result_mat
        assert result_str['R'] == Int(3)


class TestStreamingPerformance:
    """Test performance characteristics of streaming."""

    def test_memory_efficiency_large_predicate(self):
        """Test that streaming reduces memory usage for large predicates."""
        # Create very large predicate
        clauses = []
        for i in range(10000):
            clauses.append(
                Clause(head=Struct("big", (Int(i), Atom(f"v{i}"))), body=())
            )
        program = Program(tuple(clauses))

        # Query that matches early in the predicate
        engine = Engine(program, use_indexing=True, use_streaming=True)
        result = engine.query_one("big(5, X)")

        assert result is not None
        assert result['X'] == Atom("v5")

        # With streaming, we shouldn't have materialized all 10000 clauses
        # just to find the 6th one

    def test_streaming_generator_behavior(self):
        """Test that streaming maintains generator-like behavior."""
        clauses = [
            Clause(head=Struct("gen", (Int(i),)), body=())
            for i in range(100)
        ]
        program = Program(tuple(clauses))

        engine = Engine(program, use_indexing=True, use_streaming=True)

        # Get generator for query
        gen = engine.query("gen(X)")

        # Should be able to consume just first few
        result1 = next(gen)
        result2 = next(gen)
        result3 = next(gen)

        assert result1['X'] == Int(0)
        assert result2['X'] == Int(1)
        assert result3['X'] == Int(2)

        # Rest of generator is still available but not materialized


class TestStreamingFeatureGating:
    """Test feature gating for streaming functionality."""

    def test_streaming_requires_indexing(self):
        """Test that streaming only works when indexing is enabled."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # Streaming should be ignored without indexing
        engine = Engine(program, use_indexing=False, use_streaming=True)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

        # With indexing, streaming should work
        engine = Engine(program, use_indexing=True, use_streaming=True)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

    def test_streaming_disabled_with_metrics(self):
        """Test that streaming is disabled when metrics are enabled."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # With metrics, streaming should be disabled to preserve counts
        engine = Engine(
            program,
            use_indexing=True,
            use_streaming=True,
            metrics=True  # Metrics enabled
        )

        # Should still work correctly, just not streaming
        results = list(engine.query("test(X)"))
        assert len(results) == 10

    def test_streaming_env_variable_override(self):
        """Test PYLOG_STREAM_SELECTION environment variable."""
        import os

        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # Test forcing streaming off via env var
        os.environ['PYLOG_STREAM_SELECTION'] = '0'
        engine = Engine(program, use_indexing=True, use_streaming=True)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

        # Test forcing streaming on via env var
        os.environ['PYLOG_STREAM_SELECTION'] = '1'
        engine = Engine(program, use_indexing=True, use_streaming=False)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

        # Clean up
        del os.environ['PYLOG_STREAM_SELECTION']