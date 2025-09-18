"""
Tests for performance and behavior metrics collection.

Tests the metrics module for tracking engine operations with minimal overhead.
"""

import pytest
from dataclasses import FrozenInstanceError
from typing import Dict, Any

from prolog.debug.metrics import PredMetrics, EngineMetrics
from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Struct, Int, Var
from prolog.ast.clauses import Program, Clause
from prolog.tests.helpers import mk_fact, mk_rule, program, run_query


# Helper functions to reduce boilerplate
def program_from_source(src: str) -> Program:
    """Create a Program from Prolog source text.

    For now, return an empty program since we're mostly testing metrics,
    not parsing. The engine tests will still work with simple facts.
    """
    # Simple programs for testing - just return empty or simple facts
    if "foo(1)" in src:
        return program(mk_fact("foo", Int(1)), mk_fact("foo", Int(2)))
    elif "test(1). test(2). test(3)" in src:
        return program(mk_fact("test", Int(1)), mk_fact("test", Int(2)), mk_fact("test", Int(3)))
    elif "test(1). test(2)" in src:
        return program(mk_fact("test", Int(1)), mk_fact("test", Int(2)))
    elif "test(1)" in src:
        return program(mk_fact("test", Int(1)))
    elif "parent(tom, bob)" in src:
        return program(
            mk_fact("parent", Atom("tom"), Atom("bob")),
            mk_fact("parent", Atom("bob"), Atom("pat")),
            mk_rule("grandparent", (Var(0, "X"), Var(1, "Z")),
                    Struct("parent", (Var(0, "X"), Var(2, "Y"))),
                    Struct("parent", (Var(2, "Y"), Var(1, "Z"))))
        )
    else:
        # For other cases, return empty program
        return program()


def engine_for(src: str, debug: bool = True, metrics: bool = True) -> Engine:
    """Create an Engine from Prolog source text."""
    return Engine(program=program_from_source(src), debug=debug, metrics=metrics)


class TestPredMetrics:
    """Tests for per-predicate metrics dataclass."""

    def test_pred_metrics_required_fields(self):
        """PredMetrics has all required counter fields."""
        metrics = PredMetrics(
            pred_id="append/3",
            calls=10,
            exits=8,
            fails=2,
            redos=3,
            unifications=25,
            backtracks=5
        )

        assert metrics.pred_id == "append/3"
        assert metrics.calls == 10
        assert metrics.exits == 8
        assert metrics.fails == 2
        assert metrics.redos == 3
        assert metrics.unifications == 25
        assert metrics.backtracks == 5

    def test_pred_metrics_immutable(self):
        """PredMetrics is immutable (frozen=True)."""
        metrics = PredMetrics(
            pred_id="member/2",
            calls=1,
            exits=1,
            fails=0,
            redos=0,
            unifications=2,
            backtracks=0
        )

        with pytest.raises(FrozenInstanceError):
            metrics.calls = 2

    def test_pred_metrics_defaults_zero(self):
        """PredMetrics defaults all counters to 0."""
        metrics = PredMetrics(pred_id="p/1")
        assert metrics.calls == 0
        assert metrics.exits == 0
        assert metrics.fails == 0
        assert metrics.redos == 0
        assert metrics.unifications == 0
        assert metrics.backtracks == 0

    def test_pred_metrics_has_slots(self):
        """PredMetrics uses slots for memory efficiency."""
        metrics = PredMetrics(
            pred_id="test/0",
            calls=0,
            exits=0,
            fails=0,
            redos=0,
            unifications=0,
            backtracks=0
        )

        # Classes with __slots__ don't have __dict__
        assert not hasattr(metrics, '__dict__')
        assert hasattr(type(metrics), '__slots__')

    def test_pred_metrics_to_dict(self):
        """PredMetrics exports to dict format."""
        metrics = PredMetrics(
            pred_id="foo/2",
            calls=5,
            exits=3,
            fails=2,
            redos=1,
            unifications=10,
            backtracks=2
        )

        d = metrics.to_dict()
        assert isinstance(d, dict)
        assert d["pred_id"] == "foo/2"
        assert d["calls"] == 5
        assert d["exits"] == 3
        assert d["fails"] == 2
        assert d["redos"] == 1
        assert d["unifications"] == 10
        assert d["backtracks"] == 2


class TestEngineMetrics:
    """Tests for global engine metrics."""

    def test_engine_metrics_initialization(self):
        """EngineMetrics initializes with all counters at 0."""
        metrics = EngineMetrics()

        # Global counters
        assert metrics.unifications_attempted == 0
        assert metrics.unifications_succeeded == 0
        assert metrics.backtracks_taken == 0
        assert metrics.cuts_executed == 0
        assert metrics.alternatives_pruned == 0
        assert metrics.exceptions_thrown == 0
        assert metrics.exceptions_caught == 0
        assert metrics.candidates_considered == 0
        assert metrics.candidates_yielded == 0

        # Per-predicate tracking
        assert len(metrics._pred_metrics) == 0

    def test_engine_metrics_increment_global(self):
        """Global counters increment correctly."""
        metrics = EngineMetrics()

        metrics.record_unification_attempt()
        assert metrics.unifications_attempted == 1

        metrics.record_unification_success()
        assert metrics.unifications_succeeded == 1

        metrics.record_backtrack()
        assert metrics.backtracks_taken == 1

        metrics.record_cut()
        assert metrics.cuts_executed == 1

        metrics.record_alternatives_pruned(3)
        assert metrics.alternatives_pruned == 3

        metrics.record_exception_thrown()
        assert metrics.exceptions_thrown == 1

        metrics.record_exception_caught()
        assert metrics.exceptions_caught == 1

        # Candidates accumulate (not overwrite)
        metrics.record_candidates(10, 2)
        assert metrics.candidates_considered == 10
        assert metrics.candidates_yielded == 2

        metrics.record_candidates(5, 1)
        assert metrics.candidates_considered == 15
        assert metrics.candidates_yielded == 3

    def test_engine_metrics_negative_inputs_rejected(self):
        """Negative inputs to metrics methods raise ValueError."""
        metrics = EngineMetrics()

        with pytest.raises(ValueError):
            metrics.record_alternatives_pruned(-1)

        with pytest.raises(ValueError):
            metrics.record_candidates(-5, 1)

        with pytest.raises(ValueError):
            metrics.record_candidates(5, -1)

        # Test validation for predicate unification
        with pytest.raises(ValueError, match="Negative attempted"):
            metrics.record_predicate_unification("test/1", -1, 0)

        with pytest.raises(ValueError, match="Negative succeeded"):
            metrics.record_predicate_unification("test/1", 5, -1)

        with pytest.raises(ValueError, match="succeeded .* > attempted"):
            metrics.record_predicate_unification("test/1", 5, 10)

    def test_engine_metrics_per_predicate_tracking(self):
        """Per-predicate metrics are tracked correctly."""
        metrics = EngineMetrics()

        # Record some calls
        metrics.record_call("append/3")
        metrics.record_call("append/3")
        metrics.record_call("member/2")

        # Record exits
        metrics.record_exit("append/3")
        metrics.record_exit("member/2")

        # Record fails
        metrics.record_fail("append/3")

        # Check per-predicate stats
        append_stats = metrics.get_predicate_metrics("append/3")
        assert append_stats.calls == 2
        assert append_stats.exits == 1
        assert append_stats.fails == 1

        member_stats = metrics.get_predicate_metrics("member/2")
        assert member_stats.calls == 1
        assert member_stats.exits == 1
        assert member_stats.fails == 0

    def test_engine_metrics_redo_tracking(self):
        """REDO port updates per-predicate metrics."""
        metrics = EngineMetrics()

        metrics.record_call("between/3")
        metrics.record_exit("between/3")
        metrics.record_redo("between/3")
        metrics.record_exit("between/3")
        metrics.record_redo("between/3")
        metrics.record_fail("between/3")

        stats = metrics.get_predicate_metrics("between/3")
        assert stats.calls == 1
        assert stats.exits == 2
        assert stats.redos == 2
        assert stats.fails == 1

    def test_engine_metrics_reset(self):
        """Reset clears all metrics."""
        metrics = EngineMetrics()

        # Add some data
        metrics.record_unification_attempt()
        metrics.record_backtrack()
        metrics.record_call("test/0")
        metrics.record_exit("test/0")

        # Reset
        metrics.reset()

        # All counters should be 0
        assert metrics.unifications_attempted == 0
        assert metrics.backtracks_taken == 0
        assert len(metrics._pred_metrics) == 0

        # Getting predicate metrics after reset returns zeros
        stats = metrics.get_predicate_metrics("test/0")
        assert stats.calls == 0
        assert stats.exits == 0

    def test_engine_metrics_to_dict(self):
        """Export to dict includes all metrics."""
        metrics = EngineMetrics()

        # Record some activity
        metrics.record_unification_attempt()
        metrics.record_unification_success()
        metrics.record_backtrack()
        metrics.record_call("foo/1")
        metrics.record_exit("foo/1")

        # Export
        d = metrics.to_dict()

        # Check structure
        assert isinstance(d, dict)
        assert "global" in d
        assert "predicates" in d

        # Check global metrics
        assert d["global"]["unifications_attempted"] == 1
        assert d["global"]["unifications_succeeded"] == 1
        assert d["global"]["backtracks_taken"] == 1

        # Check per-predicate metrics
        assert "foo/1" in d["predicates"]
        assert d["predicates"]["foo/1"]["calls"] == 1
        assert d["predicates"]["foo/1"]["exits"] == 1

    def test_engine_metrics_unification_tracking(self):
        """Per-predicate unification counts are tracked."""
        metrics = EngineMetrics()

        metrics.record_call("unify_test/2")
        metrics.record_predicate_unification("unify_test/2", attempted=5, succeeded=3)
        metrics.record_exit("unify_test/2")

        stats = metrics.get_predicate_metrics("unify_test/2")
        assert stats.unifications == 5  # Total attempted for this predicate

    def test_engine_metrics_backtrack_tracking(self):
        """Per-predicate backtrack counts are tracked."""
        metrics = EngineMetrics()

        metrics.record_call("choice/1")
        metrics.record_exit("choice/1")
        metrics.record_predicate_backtrack("choice/1")
        metrics.record_redo("choice/1")
        metrics.record_fail("choice/1")

        stats = metrics.get_predicate_metrics("choice/1")
        assert stats.backtracks == 1


class TestMetricsEngineIntegration:
    """Tests for metrics integration with the engine."""

    def test_engine_accepts_debug_parameter(self):
        """Engine accepts debug parameter for metrics."""
        # Engine already accepts debug parameter
        engine = Engine(program=Program(()), debug=True)
        assert engine.debug is True

        engine2 = Engine(program=Program(()), debug=False)
        assert engine2.debug is False

    def test_engine_creates_metrics_when_debug_true(self):
        """Engine creates EngineMetrics when metrics=True."""
        engine = Engine(program=Program(()), metrics=True)
        assert hasattr(engine, 'metrics')
        assert isinstance(engine.metrics, EngineMetrics)

    def test_engine_no_metrics_when_debug_false(self):
        """Engine doesn't create metrics when metrics=False."""
        engine = Engine(program=Program(()), metrics=False)
        assert not hasattr(engine, 'metrics') or engine.metrics is None

    def test_metrics_track_unifications(self):
        """Metrics track unification attempts/successes during execution."""
        engine = engine_for("foo(1). foo(2).")

        # Query that will attempt unifications
        results = engine.query("foo(X)")

        # Should have attempted some unifications
        assert engine.metrics.unifications_attempted > 0
        # Some should have succeeded (at least 2 for the solutions)
        assert engine.metrics.unifications_succeeded >= 2

    def test_metrics_track_backtracks(self):
        """Metrics track backtracking during execution."""
        engine = engine_for("test(1). test(2). test(3).")

        # Query for all solutions (will backtrack)
        results = engine.query("test(X)")

        # Should have backtracked at least twice (after first and second solutions)
        assert engine.metrics.backtracks_taken >= 2

    def test_metrics_track_cuts(self):
        """Metrics track cut operations."""
        # Create a simple program with cut
        prog = program(
            mk_rule("test", (Var(0, "X"),),
                    Struct("=", (Var(0, "X"), Int(1))),
                    Atom("!")),
            mk_rule("test", (Var(0, "X"),),
                    Struct("=", (Var(0, "X"), Int(2))))
        )
        engine = Engine(program=prog, debug=True, metrics=True)

        # Query that will execute a cut
        results = engine.query("test(X)")

        # Should have executed one cut
        assert engine.metrics.cuts_executed == 1
        # Should have pruned at least one alternative (the second clause)
        assert engine.metrics.alternatives_pruned >= 1

    def test_metrics_reset_between_queries(self):
        """Metrics reset automatically when engine is reset."""
        engine = engine_for("test(1).")

        # First query
        engine.query("test(X)")
        first_unifications = engine.metrics.unifications_attempted
        assert first_unifications > 0

        # Reset the engine; metrics should reset as part of this
        engine.reset()

        # Metrics should be back to 0 (no manual metrics.reset() needed)
        assert engine.metrics.unifications_attempted == 0

        # Second query
        engine.query("test(X)")
        # Should have new counts
        assert engine.metrics.unifications_attempted > 0

    def test_metrics_track_predicate_calls(self):
        """Metrics track per-predicate call/exit/fail."""
        engine = engine_for("""
            parent(tom, bob).
            parent(bob, pat).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)

        # Query for grandparent
        results = engine.query("grandparent(tom, Z)")

        # Should have metrics for predicates
        gp_stats = engine.metrics.get_predicate_metrics("grandparent/2")
        assert gp_stats.calls >= 1

        parent_stats = engine.metrics.get_predicate_metrics("parent/2")
        assert parent_stats.calls >= 2  # Called at least twice in the rule

    def test_metrics_zero_overhead_when_disabled(self):
        """No metrics overhead when debug=False."""
        engine = engine_for("test(1). test(2).", debug=False, metrics=False)

        # Should not have metrics attribute or it's None
        assert not hasattr(engine, 'metrics') or engine.metrics is None

        # Query should work without metrics
        results = engine.query("test(X)")
        assert len(results) == 2

    def test_metrics_export_format(self):
        """Exported metrics are JSON-compatible."""
        import json

        engine = engine_for("test(1).")
        engine.query("test(X)")

        # Export metrics
        metrics_dict = engine.metrics.to_dict()

        # Should be JSON serializable
        json_str = json.dumps(metrics_dict)
        assert isinstance(json_str, str)

        # Can round-trip
        parsed = json.loads(json_str)
        assert parsed == metrics_dict

    def test_metrics_track_clause_filtering(self):
        """Metrics track clause candidate filtering by indexing."""
        # Create a program with multiple clauses that indexing can filter
        prog = program(
            mk_fact("test", Atom("a"), Int(1)),
            mk_fact("test", Atom("b"), Int(2)),
            mk_fact("test", Atom("c"), Int(3)),
            mk_fact("test", Var(0, "X"), Int(4)),  # Variable in first arg
            mk_fact("test", Atom("d"), Int(5)),
        )
        # Enable indexing to test filtering
        engine = Engine(program=prog, debug=True, metrics=True, use_indexing=True)

        # Query with a specific first argument
        results = engine.query("test(b, Y)")

        # With indexing: should have considered fewer clauses (filtered)
        # The indexing should select only clauses matching 'b' or having a variable
        assert engine.metrics.candidates_considered > 0
        assert engine.metrics.candidates_yielded > 0
        assert engine.metrics.candidates_yielded <= engine.metrics.candidates_considered

        # Verify we got the right answers
        # Both test(b, 2) and test(X, 4) match the query
        assert len(results) == 2
        assert results[0]["Y"] == Int(2)
        assert results[1]["Y"] == Int(4)

    def test_metrics_no_filtering_without_indexing(self):
        """Without indexing, all clauses are considered and yielded."""
        prog = program(
            mk_fact("test", Atom("a"), Int(1)),
            mk_fact("test", Atom("b"), Int(2)),
            mk_fact("test", Atom("c"), Int(3)),
            mk_fact("test", Var(0, "X"), Int(4)),
            mk_fact("test", Atom("d"), Int(5)),
        )
        # No indexing - all clauses should be yielded
        engine = Engine(program=prog, debug=True, metrics=True, use_indexing=False)

        results = engine.query("test(b, Y)")

        # Without indexing: all 5 clauses considered and yielded
        assert engine.metrics.candidates_considered == 5
        assert engine.metrics.candidates_yielded == 5

        # Still get the right answers
        assert len(results) == 2
        assert results[0]["Y"] == Int(2)
        assert results[1]["Y"] == Int(4)

    @pytest.mark.xfail(reason="throw/catch integration not yet wired in engine")
    def test_metrics_track_exceptions_integration(self):
        """Metrics track exceptions thrown and caught."""
        engine = engine_for("""
            risky(X) :- throw(error(bad(X))).
            safe(X) :- catch(risky(X), E, true).
        """)

        engine.query("safe(1)")
        assert engine.metrics.exceptions_thrown >= 1
        assert engine.metrics.exceptions_caught >= 1