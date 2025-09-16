"""Tests for trace replay tool and analysis utilities."""

import json
import pytest
from unittest.mock import patch

from prolog.debug.replay import (
    ReplayTool,
    TraceAnalyzer,
    TraceStatistics,
    InvalidTraceError,
    InvariantViolationError,
)
from prolog.debug.invariant_checker import check_trace_invariants


class TestReplayTool:
    """Test trace replay functionality."""

    def test_replay_reads_jsonl_file(self, tmp_path):
        """Test that replay tool can read JSONL trace files."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0", "fd": 0, "cd": 0, "gh": 1, "ws": 1},
            {"sid": 2, "p": 1, "pid": "test/0", "fd": 0, "cd": 0, "gh": 1, "ws": 1},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        replay = ReplayTool(trace_file)
        loaded_events = replay.load_events()

        assert len(loaded_events) == 2
        assert loaded_events[0]["sid"] == 1
        assert loaded_events[1]["sid"] == 2

    def test_replay_reconstructs_last_n_steps(self, tmp_path):
        """Test reconstruction of last N steps from trace."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i}/1", "fd": 0}
            for i in range(1, 301)  # 300 events
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        replay = ReplayTool(trace_file)
        last_200 = replay.reconstruct_last_n(200)

        assert len(last_200) == 200
        assert last_200[0]["sid"] == 101  # First of last 200
        assert last_200[-1]["sid"] == 300  # Last event

    def test_replay_validates_trace_consistency(self, tmp_path):
        """Test that replay detects trace consistency issues."""
        trace_file = tmp_path / "trace.jsonl"
        # Invalid: step_id not monotonic
        events = [
            {"sid": 1, "p": 0, "pid": "test/0", "fd": 0},
            {"sid": 3, "p": 1, "pid": "test/0", "fd": 0},  # Skip sid=2
            {"sid": 2, "p": 2, "pid": "test/0", "fd": 0},  # Out of order
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        replay = ReplayTool(trace_file)
        with pytest.raises(InvalidTraceError) as exc_info:
            replay.validate_consistency()

        assert "monotonic" in str(exc_info.value).lower()

    def test_replay_detects_invariant_violations(self, tmp_path):
        """Test that replay detects invariant violations."""
        trace_file = tmp_path / "trace.jsonl"
        # Invalid port sequence: REDO without preceding EXIT
        events = [
            {"sid": 1, "p": 0, "pid": "test/0", "fd": 0},  # CALL
            {"sid": 2, "p": 3, "pid": "test/0", "fd": 0},  # FAIL
            {"sid": 3, "p": 2, "pid": "test/0", "fd": 0},  # REDO - invalid!
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        replay = ReplayTool(trace_file)
        violations = replay.check_invariants()

        assert len(violations) > 0
        assert any(("redo" in str(v).lower() and "exit" in str(v).lower()) for v in violations)

    def test_replay_handles_empty_trace(self, tmp_path):
        """Test replay handles empty trace files gracefully."""
        trace_file = tmp_path / "trace.jsonl"
        trace_file.touch()  # Empty file

        replay = ReplayTool(trace_file)
        events = replay.load_events()

        assert events == []
        assert replay.reconstruct_last_n(10) == []

    def test_replay_handles_malformed_json(self, tmp_path):
        """Test replay handles malformed JSON lines."""
        trace_file = tmp_path / "trace.jsonl"
        with trace_file.open("w") as f:
            f.write('{"sid": 1, "p": 0}\n')
            f.write('not valid json\n')
            f.write('{"sid": 2, "p": 1}\n')

        replay = ReplayTool(trace_file)
        with pytest.raises(InvalidTraceError) as exc_info:
            replay.load_events()

        assert "malformed" in str(exc_info.value).lower() or "invalid" in str(exc_info.value).lower()

    def test_replay_reports_step_sequence_violations(self, tmp_path):
        """Test detection of step sequence violations."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},
            {"sid": 2, "p": 0, "pid": "b/0", "fd": 1},
            {"sid": 4, "p": 1, "pid": "b/0", "fd": 1},  # Skip sid=3
            {"sid": 5, "p": 1, "pid": "a/0", "fd": 0},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        replay = ReplayTool(trace_file)
        violations = replay.check_step_sequence()

        assert len(violations) == 1
        assert violations[0]["missing_sid"] == 3
        assert violations[0]["after_sid"] == 2
        assert violations[0]["before_sid"] == 4


class TestTraceAnalyzer:
    """Test trace analysis utilities."""

    def test_analyzer_parses_jsonl_correctly(self, tmp_path):
        """Test that analyzer parses JSONL format correctly."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "foo/1", "fd": 0},
            {"sid": 2, "p": 1, "pid": "foo/1", "fd": 0},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        parsed = analyzer.parse()

        assert len(parsed) == 2
        assert parsed[0]["pid"] == "foo/1"

    def test_analyzer_computes_port_statistics(self, tmp_path):
        """Test computation of port count statistics."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0},  # CALL
            {"sid": 2, "p": 0},  # CALL
            {"sid": 3, "p": 1},  # EXIT
            {"sid": 4, "p": 2},  # REDO
            {"sid": 5, "p": 3},  # FAIL
            {"sid": 6, "p": 0},  # CALL
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        stats = analyzer.compute_port_counts()

        assert stats["call"] == 3
        assert stats["exit"] == 1
        assert stats["redo"] == 1
        assert stats["fail"] == 1

    def test_analyzer_computes_depth_distribution(self, tmp_path):
        """Test computation of frame depth distribution."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "fd": 0},
            {"sid": 2, "fd": 1},
            {"sid": 3, "fd": 2},
            {"sid": 4, "fd": 1},
            {"sid": 5, "fd": 0},
            {"sid": 6, "fd": 3},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        dist = analyzer.compute_depth_distribution()

        assert dist["max_depth"] == 3
        assert dist["avg_depth"] == pytest.approx(1.167, rel=0.01)
        assert dist["depth_counts"][0] == 2
        assert dist["depth_counts"][1] == 2
        assert dist["depth_counts"][2] == 1
        assert dist["depth_counts"][3] == 1

    def test_analyzer_identifies_hot_predicates(self, tmp_path):
        """Test identification of frequently called predicates."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "foo/1"},
            {"sid": 2, "p": 0, "pid": "bar/2"},
            {"sid": 3, "p": 0, "pid": "foo/1"},
            {"sid": 4, "p": 0, "pid": "baz/0"},
            {"sid": 5, "p": 0, "pid": "foo/1"},
            {"sid": 6, "p": 0, "pid": "bar/2"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        hot = analyzer.identify_hot_predicates(top_n=2)

        assert len(hot) == 2
        assert hot[0] == ("foo/1", 3)
        assert hot[1] == ("bar/2", 2)

    def test_analyzer_measures_backtrack_frequency(self, tmp_path):
        """Test measurement of backtracking frequency."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "a/0"},  # CALL
            {"sid": 2, "p": 1, "pid": "a/0"},  # EXIT
            {"sid": 3, "p": 0, "pid": "b/0"},  # CALL
            {"sid": 4, "p": 3, "pid": "b/0"},  # FAIL (backtrack)
            {"sid": 5, "p": 2, "pid": "a/0"},  # REDO (backtrack)
            {"sid": 6, "p": 3, "pid": "a/0"},  # FAIL (backtrack)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        freq = analyzer.measure_backtrack_frequency()

        assert freq["total_events"] == 6
        assert freq["backtrack_events"] == 3  # FAIL, REDO, FAIL
        assert freq["backtrack_rate"] == pytest.approx(0.5, rel=0.01)

    def test_analyzer_handles_missing_or_unknown_fields(self, tmp_path):
        """Test analyzer handles missing or unknown fields gracefully."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1},                 # missing p, fd, pid
            {"sid": 2, "p": 99},        # unknown port code
            {"sid": 3, "fd": "NaN"},    # bad depth type
        ]
        with trace_file.open("w") as f:
            for e in events:
                f.write(json.dumps(e) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        # Should not crash; treat missing as 0/unknown bucket
        counts = analyzer.compute_port_counts()
        assert isinstance(counts, dict)

        dist = analyzer.compute_depth_distribution()
        assert "max_depth" in dist and "depth_counts" in dist

    def test_analyzer_hot_predicates_count_calls_only(self, tmp_path):
        """Test that hot predicates count only CALL events."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "foo/1"},  # CALL
            {"sid": 2, "p": 1, "pid": "foo/1"},  # EXIT should not increment
        ]
        with trace_file.open("w") as f:
            for e in events:
                f.write(json.dumps(e) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        hot = dict(analyzer.identify_hot_predicates(top_n=10))
        assert hot.get("foo/1", 0) == 1

    def test_analyzer_generates_summary_report(self, tmp_path):
        """Test generation of comprehensive summary reports."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i % 3}/1", "fd": i % 5}
            for i in range(1, 101)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        analyzer = TraceAnalyzer(trace_file)
        report = analyzer.generate_summary_report()

        assert "total_events" in report
        assert report["total_events"] == 100
        assert "port_counts" in report
        assert "depth_distribution" in report
        assert "hot_predicates" in report
        assert "backtrack_frequency" in report
        assert "duration_ms" in report or "duration_estimate" in report


class TestTraceStatistics:
    """Test statistical computation utilities."""

    def test_statistics_computes_basic_metrics(self):
        """Test computation of basic statistical metrics."""
        events = [
            {"sid": 1, "fd": 0, "cd": 0},
            {"sid": 2, "fd": 1, "cd": 1},
            {"sid": 3, "fd": 2, "cd": 1},
            {"sid": 4, "fd": 1, "cd": 2},
        ]

        stats = TraceStatistics(events)
        metrics = stats.compute_basic_metrics()

        assert metrics["event_count"] == 4
        assert metrics["max_frame_depth"] == 2
        assert metrics["max_choice_depth"] == 2
        assert metrics["avg_frame_depth"] == pytest.approx(1.0, rel=0.01)

    def test_statistics_handles_empty_trace(self):
        """Test statistics handle empty traces gracefully."""
        stats = TraceStatistics([])
        metrics = stats.compute_basic_metrics()

        assert metrics["event_count"] == 0
        assert metrics["max_frame_depth"] == 0
        assert metrics["max_choice_depth"] == 0
        assert metrics["avg_frame_depth"] == 0

    def test_statistics_computes_percentiles(self):
        """Test computation of percentile statistics."""
        events = [
            {"sid": i, "fd": i % 10}
            for i in range(1, 101)
        ]

        stats = TraceStatistics(events)
        percentiles = stats.compute_depth_percentiles([50, 90, 99])

        assert percentiles["p50"] == 4  # Median
        assert percentiles["p90"] == 8
        assert percentiles["p99"] == 9


class TestCIIntegration:
    """Test CI integration functionality."""

    def test_ci_captures_trace_on_failure(self, tmp_path):
        """Test that CI captures traces when tests fail."""
        # This would typically test CI workflow configuration
        # For unit testing, we simulate the capture mechanism
        trace_file = tmp_path / "trace.jsonl"
        with trace_file.open("w") as f:
            f.write('{"sid": 1, "p": 0}\n')

        # Simulate CI artifact collection
        artifact_dir = tmp_path / "artifacts"
        artifact_dir.mkdir()

        from prolog.debug.ci_integration import capture_failure_artifacts

        capture_failure_artifacts(
            trace_file=trace_file,
            output_dir=artifact_dir,
            max_trace_size_mb=10
        )

        assert (artifact_dir / "trace.jsonl").exists()

    def test_ci_respects_trace_size_limit(self, tmp_path):
        """Test that CI respects trace size limits."""
        # Create a 2MB file quickly
        trace_file = tmp_path / "trace.jsonl"
        trace_file.write_bytes(b'X' * (2 * 1024 * 1024))  # 2MB

        artifact_dir = tmp_path / "artifacts"
        artifact_dir.mkdir()

        from prolog.debug.ci_integration import capture_failure_artifacts

        capture_failure_artifacts(
            trace_file=trace_file,
            output_dir=artifact_dir,
            max_trace_size_mb=1  # 1MB limit
        )

        captured = artifact_dir / "trace.jsonl"
        assert captured.exists()
        assert captured.stat().st_size <= 1024 * 1024  # <= 1MB

    def test_ci_includes_snapshot_in_artifacts(self, tmp_path):
        """Test that CI includes snapshots in artifacts."""
        trace_file = tmp_path / "trace.jsonl"
        snapshot_file = tmp_path / "snapshot.json"

        trace_file.write_text('{"sid": 1}\n')
        snapshot_file.write_text('{"store": {}, "trail": []}')

        artifact_dir = tmp_path / "artifacts"
        artifact_dir.mkdir()

        from prolog.debug.ci_integration import capture_failure_artifacts

        capture_failure_artifacts(
            trace_file=trace_file,
            snapshot_file=snapshot_file,
            output_dir=artifact_dir
        )

        assert (artifact_dir / "trace.jsonl").exists()
        assert (artifact_dir / "snapshot.json").exists()

    def test_ci_configurable_trace_enablement(self):
        """Test that trace collection can be enabled/disabled."""
        from prolog.debug.ci_integration import is_tracing_enabled

        # Test environment variable control
        with patch.dict("os.environ", {"PYLOG_TRACE": "1"}):
            assert is_tracing_enabled() is True

        with patch.dict("os.environ", {"PYLOG_TRACE": "0"}):
            assert is_tracing_enabled() is False

        with patch.dict("os.environ", {}, clear=True):
            # Default behavior when not set
            assert is_tracing_enabled() is False  # Default to disabled


class TestInvariantChecker:
    """Test trace invariant checking."""

    def test_invariant_checker_validates_step_id_monotonic(self):
        """Test that step_id must be strictly monotonic."""
        events = [
            {"sid": 1, "p": 0},
            {"sid": 2, "p": 1},
            {"sid": 2, "p": 2},  # Duplicate step_id
        ]

        violations = check_trace_invariants(events)
        assert any("step_id" in str(v) and "monotonic" in str(v) for v in violations)

    def test_invariant_checker_validates_port_sequences(self):
        """Test valid port sequence invariants."""
        # Invalid: REDO without EXIT
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},  # CALL
            {"sid": 2, "p": 3, "pid": "a/0", "fd": 0},  # FAIL
            {"sid": 3, "p": 2, "pid": "a/0", "fd": 0},  # REDO - invalid
        ]

        violations = check_trace_invariants(events)
        assert any("REDO" in str(v) and "EXIT" in str(v) for v in violations)

    def test_invariant_checker_validates_depth_changes(self):
        """Test frame depth change invariants."""
        # Invalid: depth jumps by more than 1
        events = [
            {"sid": 1, "p": 0, "fd": 0},
            {"sid": 2, "p": 0, "fd": 3},  # Jump from 0 to 3
        ]

        violations = check_trace_invariants(events)
        assert any("depth" in str(v).lower() for v in violations)

    def test_invariant_checker_returns_empty_for_valid_trace(self):
        """Test that valid traces produce no violations."""
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},  # CALL
            {"sid": 2, "p": 0, "pid": "b/0", "fd": 1},  # CALL (deeper)
            {"sid": 3, "p": 1, "pid": "b/0", "fd": 1},  # EXIT
            {"sid": 4, "p": 1, "pid": "a/0", "fd": 0},  # EXIT
        ]

        violations = check_trace_invariants(events)
        assert violations == []

    def test_replay_reconstructs_last_n_edges(self, tmp_path):
        """Test edge cases in tail reconstruction."""
        trace_file = tmp_path / "trace.jsonl"
        with trace_file.open("w") as f:
            for i in range(1, 4):
                f.write(json.dumps({"sid": i, "p": 0}) + "\n")

        replay = ReplayTool(trace_file)
        assert replay.reconstruct_last_n(0) == []
        last_10 = replay.reconstruct_last_n(10)
        assert [e["sid"] for e in last_10] == [1, 2, 3]  # N > len returns all

    def test_replay_rejects_duplicate_or_nonpositive_step_ids(self, tmp_path):
        """Test detection of duplicate and non-positive step IDs."""
        trace_file = tmp_path / "trace.jsonl"
        with trace_file.open("w") as f:
            f.write('{"sid": 1, "p": 0}\n')
            f.write('{"sid": 1, "p": 1}\n')  # duplicate sid

        replay = ReplayTool(trace_file)
        with pytest.raises(InvalidTraceError) as exc:
            replay.validate_consistency()
        assert "duplicate" in str(exc.value).lower() or "monotonic" in str(exc.value).lower()

    def test_replay_detects_cross_predicate_sequence_violations(self, tmp_path):
        """Test detection of cross-predicate port sequence violations."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},  # CALL a/0
            {"sid": 2, "p": 1, "pid": "b/0", "fd": 0},  # EXIT b/0 (invalid)
        ]
        with trace_file.open("w") as f:
            for e in events:
                f.write(json.dumps(e) + "\n")

        replay = ReplayTool(trace_file)
        violations = replay.check_invariants()
        assert any("predicate" in str(v).lower() for v in violations)

    def test_invariant_checker_exit_without_prior_call(self):
        """Test detection of EXIT without prior CALL."""
        events = [{"sid": 1, "p": 1, "pid": "a/0", "fd": 0}]  # EXIT first
        violations = check_trace_invariants(events)
        assert any(("exit" in str(v).lower() and "call" in str(v).lower()) for v in violations)

    def test_invariant_checker_depth_underflow(self):
        """Test detection of depth underflow."""
        events = [
            {"sid": 1, "p": 0, "fd": 0},
            {"sid": 2, "p": 1, "fd": -1},  # underflow
        ]
        violations = check_trace_invariants(events)
        assert any(("depth" in str(v).lower() and ("underflow" in str(v).lower() or "negative" in str(v).lower())) for v in violations)