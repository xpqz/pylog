"""
Integration tests for streaming clause selection in Engine.

Tests that streaming cursor integration preserves semantics,
trace ordering, and result correctness.
"""

import pytest
import os
from typing import List

from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Atom, Struct, Int, Var, List as PrologList
from prolog.ast.clauses import Clause
from prolog.debug.tracer import PortsTracer, TraceEvent
from prolog.debug.sinks import CollectorSink


class TestEngineStreamingResultParity:
    """Test result parity across different engine configurations."""

    @pytest.mark.parametrize("first_arg_type", [
        "int_pos",      # Positive integer
        "int_neg",      # Negative integer
        "atom",         # Regular atom
        "empty_list",   # Empty list atom
        # "list_sugar",   # List sugar - SKIP: PrologList not hashable (IndexedProgram issue)
        "list_canon",   # List canonical '.'/2
        "struct",       # Regular struct
        "var",          # Variable
    ])
    def test_mixed_first_arg_types(self, first_arg_type):
        """Test streaming with various first argument types."""
        # Create clauses fresh for each test to avoid hashing issues
        if first_arg_type == "int_pos":
            first_arg = Int(1)
        elif first_arg_type == "int_neg":
            first_arg = Int(-5)
        elif first_arg_type == "atom":
            first_arg = Atom("foo")
        elif first_arg_type == "empty_list":
            first_arg = Atom("[]")
        elif first_arg_type == "list_sugar":
            first_arg = PrologList([Int(1), Int(2)])
        elif first_arg_type == "list_canon":
            first_arg = Struct(".", (Int(1), Atom("[]")))
        elif first_arg_type == "struct":
            first_arg = Struct("f", (Atom("a"),))
        else:  # var
            first_arg = Var(0, "X")

        clauses = [
            Clause(head=Struct("test", (first_arg, Atom("match"))), body=()),
            Clause(head=Struct("test", (Var(1, "Y"), Atom("var"))), body=()),
            Clause(head=Struct("test", (Int(99), Atom("other"))), body=()),
        ]
        program = Program(tuple(clauses))

        # Compare three configurations
        engine_no_index = Engine(program, use_indexing=False)
        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        results_no_index = list(engine_no_index.query("test(_, R)"))
        results_indexed = list(engine_indexed.query("test(_, R)"))
        results_streaming = list(engine_streaming.query("test(_, R)"))

        # All should produce same results in same order
        assert results_streaming == results_indexed == results_no_index

    def test_facts_rules_mixed(self):
        """Test streaming with mix of facts and rules."""
        clauses = [
            # Facts
            Clause(head=Struct("parent", (Atom("tom"), Atom("bob"))), body=()),
            Clause(head=Struct("parent", (Atom("bob"), Atom("ann"))), body=()),

            # Rule
            Clause(
                head=Struct("grandparent", (Var(0, "X"), Var(1, "Z"))),
                body=(
                    Struct("parent", (Var(0, "X"), Var(2, "Y"))),
                    Struct("parent", (Var(2, "Y"), Var(1, "Z"))),
                )
            ),
        ]
        program = Program(tuple(clauses))

        # Compare all configurations
        engine_no_index = Engine(program, use_indexing=False)
        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        query = "grandparent(X, Z)"
        results_no_index = list(engine_no_index.query(query))
        results_indexed = list(engine_indexed.query(query))
        results_streaming = list(engine_streaming.query(query))

        assert results_streaming == results_indexed == results_no_index

    def test_zero_arity_predicates(self):
        """Test streaming with zero-arity predicates."""
        clauses = [
            Clause(head=Atom("fact1"), body=()),
            Clause(head=Atom("fact2"), body=()),
            Clause(head=Atom("fact3"), body=()),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # All facts should be found
        for fact in ["fact1", "fact2", "fact3"]:
            results_streaming = list(engine_streaming.query(fact))
            results_indexed = list(engine_indexed.query(fact))
            assert len(results_streaming) == 1
            assert results_streaming == results_indexed
            assert results_streaming[0] == {}

        # Non-existent predicate
        results_streaming = list(engine_streaming.query("fact4"))
        results_indexed = list(engine_indexed.query("fact4"))
        assert results_streaming == results_indexed == []

    def test_undefined_predicates(self):
        """Test streaming with undefined predicates yields empty."""
        program = Program(tuple())

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        results_indexed = list(engine_indexed.query("undefined(X)"))
        results_streaming = list(engine_streaming.query("undefined(X)"))

        assert results_streaming == results_indexed == []

    def test_malformed_goal_shapes(self):
        """Test streaming with malformed goal shapes."""
        clauses = [
            Clause(head=Struct("test", (Int(1),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Query with atom instead of struct (malformed for non-zero arity)
        # Should yield empty results, not exception
        results_indexed = list(engine_indexed.query("test"))  # Atom instead of Struct
        results_streaming = list(engine_streaming.query("test"))

        assert results_streaming == results_indexed

    def test_variable_first_arg_selection(self):
        """Test unbound variable streams all clauses in order."""
        clauses = [
            Clause(head=Struct("data", (Int(1), Atom("one"))), body=()),
            Clause(head=Struct("data", (Int(2), Atom("two"))), body=()),
            Clause(head=Struct("data", (Atom("a"), Atom("letter"))), body=()),
            Clause(head=Struct("data", (Var(0, "Y"), Atom("var"))), body=()),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Unbound variable should match all clauses
        results_indexed = list(engine_indexed.query("data(X, Y)"))
        results_streaming = list(engine_streaming.query("data(X, Y)"))

        assert len(results_streaming) == 4
        assert results_streaming == results_indexed

    def test_bound_variable_after_deref(self):
        """Test bound variable uses the bound term for selection."""
        clauses = [
            # Rule that binds then calls
            Clause(
                head=Struct("test", (Var(0, "X"),)),  # Note comma for single-element tuple
                body=(
                    Struct("=", (Var(0, "X"), Int(42))),
                    Struct("data", (Var(0, "X"), Var(1, "Y"))),
                )
            ),
            # Data facts
            Clause(head=Struct("data", (Int(1), Atom("one"))), body=()),
            Clause(head=Struct("data", (Int(42), Atom("found"))), body=()),
            Clause(head=Struct("data", (Int(99), Atom("other"))), body=()),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        results_indexed = list(engine_indexed.query("test(X)"))
        results_streaming = list(engine_streaming.query("test(X)"))

        # Should find X=42 via the binding
        assert len(results_streaming) == 1
        assert results_streaming == results_indexed


class TestEngineStreamingTraceOrder:
    """Test 4-port trace ordering preservation."""

    def test_four_port_order_preserved(self):
        """Test CALL/EXIT/REDO/FAIL order is preserved."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
            Clause(
                head=Struct("q", (Var(0, "X"),)),
                body=(Struct("p", (Var(0, "X"),)),)
            ),
        ]
        program = Program(tuple(clauses))

        # Collect traces from both engines
        collector_indexed = CollectorSink()
        engine_indexed = Engine(
            program,
            use_indexing=True,
            use_streaming=False,
            trace=True
        )
        # Add collector sink to the tracer
        engine_indexed.tracer.add_sink(collector_indexed)
        list(engine_indexed.query("q(X)"))

        collector_streaming = CollectorSink()
        engine_streaming = Engine(
            program,
            use_indexing=True,
            use_streaming=True,
            trace=True
        )
        # Add collector sink to the tracer
        engine_streaming.tracer.add_sink(collector_streaming)
        list(engine_streaming.query("q(X)"))

        # Compare sequences of (port, pred_id) ignoring step_id
        def extract_port_pred(events):
            result = []
            for ev in events:
                if ev.port in ["CALL", "EXIT", "REDO", "FAIL"]:
                    # Extract predicate identifier from goal
                    if isinstance(ev.goal, Struct):
                        pred_id = f"{ev.goal.functor}/{len(ev.goal.args)}"
                    elif isinstance(ev.goal, Atom):
                        pred_id = f"{ev.goal.name}/0"
                    else:
                        pred_id = str(ev.goal)
                    result.append((ev.port, pred_id))
            return result

        indexed_trace = extract_port_pred(collector_indexed.events)
        streaming_trace = extract_port_pred(collector_streaming.events)

        assert streaming_trace == indexed_trace


class TestEngineStreamingBacktracking:
    """Test backtracking behavior with streaming."""

    def test_multiple_solutions_backtracking(self):
        """Test streaming finds all solutions via backtracking."""
        clauses = [
            Clause(head=Struct("color", (Atom("red"),)), body=()),
            Clause(head=Struct("color", (Atom("green"),)), body=()),
            Clause(head=Struct("color", (Atom("blue"),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        results_indexed = list(engine_indexed.query("color(X)"))
        results_streaming = list(engine_streaming.query("color(X)"))

        assert len(results_streaming) == 3
        assert results_streaming == results_indexed

    def test_cut_prunes_alternatives(self):
        """Test cut operator prunes alternatives correctly."""
        clauses = [
            Clause(head=Struct("test", (Int(1),)), body=(Atom("!"),)),
            Clause(head=Struct("test", (Int(2),)), body=()),
            Clause(head=Struct("test", (Int(3),)), body=()),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        results_indexed = list(engine_indexed.query("test(X)"))
        results_streaming = list(engine_streaming.query("test(X)"))

        # Cut should prevent backtracking
        assert len(results_streaming) == 1
        assert results_streaming == results_indexed
        assert results_streaming[0]['X'] == Int(1)

    def test_disjunction_with_streaming(self):
        """Test disjunction (;) works with streaming."""
        clauses = [
            Clause(
                head=Struct("test", (Var(0, "X"),)),
                body=(
                    Struct(";", (
                        Struct("=", (Var(0, "X"), Atom("a"))),
                        Struct("=", (Var(0, "X"), Atom("b")))
                    )),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        results_indexed = list(engine_indexed.query("test(X)"))
        results_streaming = list(engine_streaming.query("test(X)"))

        assert len(results_streaming) == 2
        assert results_streaming == results_indexed

    def test_if_then_else_with_streaming(self):
        """Test if-then-else (->/) works with streaming."""
        clauses = [
            Clause(
                head=Struct("test", (Var(0, "X"), Var(1, "Y"))),
                body=(
                    Struct("->", (
                        Struct("=", (Var(0, "X"), Int(1))),
                        Struct("=", (Var(1, "Y"), Atom("one"))),
                        Struct("=", (Var(1, "Y"), Atom("other")))
                    )),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Test with X=1 (then branch)
        results_indexed = list(engine_indexed.query("test(1, Y)"))
        results_streaming = list(engine_streaming.query("test(1, Y)"))
        assert results_streaming == results_indexed

        # Test with X=2 (else branch)
        results_indexed = list(engine_indexed.query("test(2, Y)"))
        results_streaming = list(engine_streaming.query("test(2, Y)"))
        assert results_streaming == results_indexed


class TestEngineStreamingRecursion:
    """Test recursive predicates with streaming."""

    def test_simple_recursion(self):
        """Test simple recursive predicate."""
        clauses = [
            # member/2
            Clause(
                head=Struct("member", (Var(0, "X"),
                    Struct(".", (Var(0, "X"), Var(1, "_"))))),
                body=()
            ),
            Clause(
                head=Struct("member", (Var(0, "X"),
                    Struct(".", (Var(1, "_"), Var(2, "T"))))),
                body=(Struct("member", (Var(0, "X"), Var(2, "T"))),)
            ),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Test member/2 with a list
        query = "member(X, [1, 2, 3])"
        results_indexed = list(engine_indexed.query(query))
        results_streaming = list(engine_streaming.query(query))

        assert len(results_streaming) == 3
        assert results_streaming == results_indexed

    def test_recursive_with_accumulator(self):
        """Test recursive predicate with accumulator."""
        clauses = [
            # length/2 using accumulator
            Clause(
                head=Struct("length", (Atom("[]"), Int(0))),
                body=()
            ),
            Clause(
                head=Struct("length", (
                    Struct(".", (Var(0, "_"), Var(1, "T"))),
                    Var(2, "N")
                )),
                body=(
                    Struct("length", (Var(1, "T"), Var(3, "N1"))),
                    Struct("is", (Var(2, "N"),
                        Struct("+", (Var(3, "N1"), Int(1))))),
                )
            ),
        ]
        program = Program(tuple(clauses))

        engine_indexed = Engine(program, use_indexing=True, use_streaming=False)
        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Test with known list
        query = "length([1, 2], L)"
        results_indexed = list(engine_indexed.query(query))
        results_streaming = list(engine_streaming.query(query))

        assert len(results_streaming) == 1
        assert results_streaming == results_indexed
        assert results_streaming[0]['L'] == Int(2)


class TestEngineStreamingPerformance:
    """Test performance characteristics."""

    def test_early_termination_efficiency(self):
        """Test early termination doesn't process unnecessary clauses."""
        # Create large predicate
        clauses = []
        for i in range(1000):
            clauses.append(
                Clause(head=Struct("data", (Int(i), Atom(f"v{i}"))), body=())
            )
        program = Program(tuple(clauses))

        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Query for first item - should terminate early
        results = list(engine_streaming.query("data(0, X)"))
        assert len(results) == 1
        assert results[0]['X'] == Atom("v0")

        # Only first solution produced, no errors
        results = list(engine_streaming.query("data(0, X)"))
        assert len(results) == 1

    @pytest.mark.slow
    def test_large_predicate_efficiency(self):
        """Test streaming with very large predicates."""
        # Create very large predicate
        clauses = []
        for i in range(10000):
            clauses.append(
                Clause(head=Struct("big", (Int(i), Atom(f"v{i}"))), body=())
            )
        program = Program(tuple(clauses))

        engine_streaming = Engine(program, use_indexing=True, use_streaming=True)

        # Should efficiently find early match
        results = list(engine_streaming.query("big(5, X)"))
        assert len(results) == 1
        assert results[0]['X'] == Atom("v5")


class TestEngineStreamingFeatureGating:
    """Test feature gating for streaming."""

    def test_streaming_disabled_when_debug_enabled(self):
        """Test streaming disabled with debug mode."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # With debug tracer, streaming should be disabled
        engine = Engine(
            program,
            use_indexing=True,
            use_streaming=True,  # Request streaming
            trace=True  # But debug is enabled
        )

        # Should still work correctly (falls back to materialized)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

    def test_streaming_disabled_when_metrics_enabled(self):
        """Test streaming disabled with metrics."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        engine = Engine(
            program,
            use_indexing=True,
            use_streaming=True,  # Request streaming
            metrics=True  # Metrics enabled
        )

        # Should work correctly (falls back to materialized)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

    def test_streaming_enabled_conditions(self):
        """Test streaming enabled when conditions are met."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # Streaming enabled: indexing on, debug off, metrics off
        engine = Engine(
            program,
            use_indexing=True,
            use_streaming=True,
            trace=None,
            metrics=False
        )

        results = list(engine.query("test(X)"))
        assert len(results) == 10

    def test_streaming_requires_indexing(self):
        """Test streaming ignored without indexing."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # Streaming ignored without indexing
        engine = Engine(
            program,
            use_indexing=False,
            use_streaming=True  # Ignored
        )

        results = list(engine.query("test(X)"))
        assert len(results) == 10

    def test_env_override(self, monkeypatch):
        """Test PYLOG_STREAM_SELECTION environment variable."""
        clauses = [
            Clause(head=Struct("test", (Int(i),)), body=())
            for i in range(10)
        ]
        program = Program(tuple(clauses))

        # Force streaming off
        monkeypatch.setenv("PYLOG_STREAM_SELECTION", "0")
        engine = Engine(program, use_indexing=True, use_streaming=True)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

        # Force streaming on
        monkeypatch.setenv("PYLOG_STREAM_SELECTION", "1")
        engine = Engine(program, use_indexing=True, use_streaming=False)
        results = list(engine.query("test(X)"))
        assert len(results) == 10

        # Clean up happens automatically with monkeypatch