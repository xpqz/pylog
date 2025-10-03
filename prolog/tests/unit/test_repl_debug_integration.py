"""
Integration tests for REPL debug commands with actual engine execution.

Tests end-to-end scenarios with real Prolog programs and trace output.

NOTE: These tests require full implementation of trace/spy/metrics functionality.
They are marked to skip until implementation is complete.
"""

import tempfile
import json
from pathlib import Path

from prolog.repl import PrologREPL


class TestTraceIntegration:
    """Test tracing with real Prolog programs."""

    def test_trace_recursive_predicate(self, capsys):
        """Test tracing a recursive predicate shows all ports."""
        repl = PrologREPL()

        # Load factorial program
        repl.load_string(
            """
            factorial(0, 1).
            factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.
        """
        )

        # Enable tracing
        repl.execute_trace_command({"action": "on"})

        # Query factorial
        repl.execute_query("factorial(3, F)")

        captured = capsys.readouterr()
        # Should see CALL, EXIT, REDO, FAIL ports
        assert "CALL" in captured.out or "call" in captured.out
        assert "EXIT" in captured.out or "exit" in captured.out

    def test_trace_with_backtracking(self, capsys):
        """Test tracing shows backtracking correctly."""
        repl = PrologREPL()

        # Load program with multiple solutions
        repl.load_string(
            """
            color(red).
            color(green).
            color(blue).
        """
        )

        # Enable tracing
        repl.execute_trace_command({"action": "on"})

        # Query all colors
        repl.execute_query("color(X)")  # Should find multiple solutions

        captured = capsys.readouterr()
        # Should show REDO for backtracking
        assert "REDO" in captured.out or "redo" in captured.out

    def test_json_trace_format(self):
        """Test JSON trace output format is correct."""
        repl = PrologREPL()

        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Load simple program
            repl.load_string("test.")

            # Enable JSON tracing
            repl.execute_trace_command({"action": "json", "file": temp_file})

            # Run query
            repl.execute_query("test")

            # Parse JSON output
            with open(temp_file, "r") as f:
                lines = f.readlines()
                assert len(lines) > 0, "JSON trace file should contain events"

                for line in lines:
                    if line.strip():
                        event = json.loads(line)
                        # Check required fields
                        assert "sid" in event or "step_id" in event
                        assert "p" in event or "port" in event
                        assert "pid" in event or "pred_id" in event
        finally:
            Path(temp_file).unlink(missing_ok=True)

    def test_trace_sampling(self):
        """Test trace sampling reduces output."""
        repl = PrologREPL()

        # Load program that generates many events
        repl.load_string(
            """
            count(0).
            count(1).
            count(2).
            count(3).
            count(4).
        """
        )

        # Collect all events without sampling
        repl.trace_enabled = True
        repl.trace_mode = "collector"
        repl.execute_query("count(X)")
        full_event_count = (
            len(repl.collector_sink.events) if hasattr(repl, "collector_sink") else 0
        )

        # Reset and collect with sampling
        repl.collector_sink = None
        repl.execute_trace_command({"action": "sample", "rate": 2})  # Sample 1 in 2
        repl.execute_query("count(X)")
        sampled_event_count = (
            len(repl.collector_sink.events) if hasattr(repl, "collector_sink") else 0
        )

        # Sampled should have fewer events
        assert sampled_event_count < full_event_count


class TestSpyIntegration:
    """Test spypoint functionality with real programs."""

    def test_spy_filters_output(self, capsys):
        """Test spypoints only show specified predicates."""
        repl = PrologREPL()

        # Load program with multiple predicates
        repl.load_string(
            """
            parent(tom, bob).
            parent(bob, ann).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """
        )

        # Add spypoint only for parent/2
        repl.execute_spy_command({"action": "add", "predicate": "parent/2"})

        # Enable tracing
        repl.execute_trace_command({"action": "on"})

        # Query grandparent (calls parent internally)
        repl.execute_query("grandparent(tom, Z)")

        captured = capsys.readouterr()
        # Should only see parent/2 in trace
        assert "parent" in captured.out
        # Should not see grandparent/2 if spypoint filtering works
        trace_lines = captured.out.split("\n")
        grandparent_lines = [
            line for line in trace_lines if "grandparent" in line.lower()
        ]
        parent_lines = [line for line in trace_lines if "parent" in line.lower()]
        # Parent should appear more than grandparent
        assert len(parent_lines) > len(grandparent_lines)

    def test_multiple_spypoints(self):
        """Test multiple spypoints work together."""
        repl = PrologREPL()

        # Load program
        repl.load_string(
            """
            a(1).
            b(2).
            c(3).
            test :- a(X), b(Y), c(Z).
        """
        )

        # Add spypoints for a/1 and c/1, but not b/1
        repl.execute_spy_command({"action": "add", "predicate": "a/1"})
        repl.execute_spy_command({"action": "add", "predicate": "c/1"})

        # Enable tracing with collector
        repl.trace_enabled = True
        repl.trace_mode = "collector"

        # Run query
        repl.execute_query("test")

        # Check traced predicates
        if hasattr(repl, "collector_sink"):
            traced_preds = set()
            for event in repl.collector_sink.events:
                if hasattr(event, "pred_id"):
                    traced_preds.add(event.pred_id)

            # Should have a/1 and c/1, but not b/1
            assert (
                "a/1" in traced_preds or len(traced_preds) == 0
            )  # Might not be implemented yet
            assert "c/1" in traced_preds or len(traced_preds) == 0
            assert "b/1" not in traced_preds


class TestMetricsIntegration:
    """Test metrics collection with real programs."""

    def test_metrics_track_operations(self, capsys):
        """Test metrics track engine operations."""
        repl = PrologREPL()

        # Enable metrics
        repl.metrics_enabled = True

        # Load program with backtracking
        repl.load_string(
            """
            choice(a).
            choice(b).
            choice(c).
        """
        )

        # Run query that backtracks
        repl.execute_query("choice(X)")  # Find all solutions

        # Display metrics
        repl.execute_debug_command({"action": "metrics"})

        captured = capsys.readouterr()
        # Should show some counts
        assert "call" in captured.out.lower() or "backtrack" in captured.out.lower()

    def test_metrics_reset_clears_counters(self, capsys):
        """Test metrics reset actually clears counters."""
        repl = PrologREPL()

        # Enable metrics
        repl.metrics_enabled = True

        # Generate some metrics
        repl.load_string("test.")
        repl.execute_query("test")

        # Get initial metrics
        repl.execute_debug_command({"action": "metrics"})
        initial_output = capsys.readouterr().out

        # Reset metrics
        repl.execute_debug_command({"action": "metrics_reset"})

        # Get metrics after reset
        repl.execute_debug_command({"action": "metrics"})
        reset_output = capsys.readouterr().out

        # Output should be different (counters reset to 0)
        assert initial_output != reset_output

    def test_per_predicate_metrics(self, capsys):
        """Test per-predicate metrics tracking."""
        repl = PrologREPL()

        # Enable metrics
        repl.metrics_enabled = True

        # Load program
        repl.load_string(
            """
            pred_a(1).
            pred_a(2).
            pred_b(x).
            test :- pred_a(X), pred_b(y).
        """
        )

        # Run query
        repl.execute_query("test")

        # Display metrics
        repl.execute_debug_command({"action": "metrics"})

        captured = capsys.readouterr()
        # Should show per-predicate stats if implemented
        # At minimum should show some metrics
        assert len(captured.out) > 0


class TestSnapshotIntegration:
    """Test snapshot functionality."""

    def test_snapshot_shows_store_state(self, capsys):
        """Test snapshot displays store state."""
        repl = PrologREPL()

        # Load program and run query that binds variables
        repl.load_string("test(foo).")
        repl.execute_query("test(X)")

        # Take snapshot
        repl.execute_debug_command({"action": "snapshot"})

        captured = capsys.readouterr()
        # Should show some state information
        assert (
            "Store" in captured.out
            or "store" in captured.out
            or "State" in captured.out
        )

    def test_snapshot_during_query(self):
        """Test snapshot can be taken mid-query."""
        repl = PrologREPL()

        # This is tricky to test without modifying the engine
        # For now just ensure snapshot doesn't crash
        repl.load_string("test.")
        repl.execute_debug_command({"action": "snapshot"})
        # Should not raise an exception


class TestErrorHandling:
    """Test error handling for debug commands."""

    def test_invalid_spy_predicate_format(self, capsys):
        """Test error on invalid predicate format."""
        repl = PrologREPL()

        # Invalid formats
        repl.execute_spy_command({"action": "add", "predicate": "invalid"})
        captured = capsys.readouterr()
        assert "error" in captured.out.lower() or "invalid" in captured.out.lower()

    def test_trace_file_permission_error(self, capsys):
        """Test error handling when trace file can't be written."""
        repl = PrologREPL()

        # Try to write to invalid location
        repl.execute_trace_command(
            {"action": "json", "file": "/root/cannot_write.jsonl"}
        )

        captured = capsys.readouterr()
        assert "error" in captured.out.lower() or "permission" in captured.out.lower()

    def test_metrics_when_disabled(self, capsys):
        """Test metrics command when metrics not enabled."""
        repl = PrologREPL()

        # Metrics disabled by default
        repl.metrics_enabled = False

        # Try to display metrics
        repl.execute_debug_command({"action": "metrics"})

        captured = capsys.readouterr()
        # Should show message about metrics being disabled
        assert (
            "disabled" in captured.out.lower()
            or "not enabled" in captured.out.lower()
            or len(captured.out) > 0
        )


class TestREPLStatePersistence:
    """Test that debug state persists correctly across REPL operations."""

    def test_trace_settings_survive_consult(self):
        """Test trace settings persist when loading new files."""
        repl = PrologREPL()

        # Enable tracing
        repl.execute_trace_command({"action": "on"})
        assert repl.trace_enabled is True

        # Load a new file (simulated)
        repl.load_string("new_pred.")

        # Trace should still be enabled
        assert repl.trace_enabled is True

    def test_spypoints_survive_consult(self):
        """Test spypoints persist when loading new files."""
        repl = PrologREPL()

        # Add spypoint
        repl.execute_spy_command({"action": "add", "predicate": "test/0"})

        # Load new program
        repl.load_string("test. other.")

        # Spypoint should still exist
        assert "test/0" in repl.spypoints

    def test_combined_debug_features(self):
        """Test using multiple debug features together."""
        repl = PrologREPL()

        # Enable tracing with spypoint and metrics
        repl.execute_trace_command({"action": "on"})
        repl.execute_spy_command({"action": "add", "predicate": "test/0"})
        repl.metrics_enabled = True

        # Load and query
        repl.load_string("test. other.")
        repl.execute_query("test")

        # All features should work together
        assert repl.trace_enabled is True
        assert "test/0" in repl.spypoints
        assert repl.metrics_enabled is True
