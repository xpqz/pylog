"""Tests for trace statistics computation module."""

import json
import pytest

from prolog.debug.trace_stats import (
    compute_trace_statistics,
    generate_statistics_report,
    analyze_performance_metrics,
    detect_performance_anomalies,
)


class TestTraceStatisticsComputation:
    """Test trace statistics computation functions."""

    def test_compute_statistics_from_file(self, tmp_path):
        """Test computing statistics from a trace file."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i % 5}/1", "fd": i % 3, "cd": i % 2}
            for i in range(1, 51)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        stats = compute_trace_statistics(trace_file)

        assert stats["total_events"] == 50
        assert stats["unique_predicates"] == 5
        assert "port_distribution" in stats
        assert "depth_statistics" in stats
        assert "predicate_frequency" in stats

    def test_statistics_with_timestamps(self, tmp_path):
        """Test statistics computation with timestamp data."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "ts": 1000.0},
            {"sid": 2, "p": 1, "ts": 1000.5},
            {"sid": 3, "p": 0, "ts": 1001.0},
            {"sid": 4, "p": 1, "ts": 1002.0},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        stats = compute_trace_statistics(trace_file)

        assert stats["duration_ms"] == pytest.approx(2000.0, rel=0.01)
        assert stats["events_per_second"] == pytest.approx(2.0, rel=0.01)

    def test_statistics_without_timestamps(self, tmp_path):
        """Test statistics handle missing timestamps gracefully."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0"},
            {"sid": 2, "p": 1, "pid": "test/0"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        stats = compute_trace_statistics(trace_file)

        assert "duration_ms" not in stats
        assert "events_per_second" not in stats

    def test_predicate_call_graph_statistics(self, tmp_path):
        """Test computation of predicate call graph metrics."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},  # a calls
            {"sid": 2, "p": 0, "pid": "b/0", "fd": 1},  # b (from a)
            {"sid": 3, "p": 0, "pid": "c/0", "fd": 2},  # c (from b)
            {"sid": 4, "p": 1, "pid": "c/0", "fd": 2},  # c exits
            {"sid": 5, "p": 1, "pid": "b/0", "fd": 1},  # b exits
            {"sid": 6, "p": 0, "pid": "c/0", "fd": 1},  # c (from a)
            {"sid": 7, "p": 1, "pid": "c/0", "fd": 1},  # c exits
            {"sid": 8, "p": 1, "pid": "a/0", "fd": 0},  # a exits
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        stats = compute_trace_statistics(trace_file)

        assert "call_graph" in stats
        graph = stats["call_graph"]
        assert ("a/0", "b/0") in graph["edges"]
        assert ("a/0", "c/0") in graph["edges"]
        assert ("b/0", "c/0") in graph["edges"]

    def test_backtrack_point_statistics(self, tmp_path):
        """Test identification of backtrack points."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},
            {"sid": 2, "p": 1, "pid": "a/0", "fd": 0},  # EXIT
            {"sid": 3, "p": 0, "pid": "b/0", "fd": 0},
            {"sid": 4, "p": 3, "pid": "b/0", "fd": 0},  # FAIL
            {"sid": 5, "p": 2, "pid": "a/0", "fd": 0},  # REDO (backtrack point)
            {"sid": 6, "p": 1, "pid": "a/0", "fd": 0},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        stats = compute_trace_statistics(trace_file)

        assert "backtrack_points" in stats
        assert len(stats["backtrack_points"]) == 1
        assert stats["backtrack_points"][0]["sid"] == 5
        assert stats["backtrack_points"][0]["pid"] == "a/0"


class TestStatisticsReport:
    """Test statistics report generation."""

    def test_generate_human_readable_report(self, tmp_path):
        """Test generation of human-readable statistics report."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i % 3}/1", "fd": i % 5}
            for i in range(1, 101)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        report = generate_statistics_report(trace_file, format="text")

        assert isinstance(report, str)
        assert "Total Events: 100" in report
        assert "Unique Predicates: 3" in report
        assert "Port Distribution" in report
        assert "Hot Predicates" in report

    def test_generate_json_report(self, tmp_path):
        """Test generation of JSON statistics report."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0"},
            {"sid": 2, "p": 1, "pid": "test/0"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        report = generate_statistics_report(trace_file, format="json")

        data = json.loads(report)
        assert data["total_events"] == 2
        assert "port_distribution" in data

    def test_generate_csv_report(self, tmp_path):
        """Test generation of CSV statistics report."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": 0, "pid": f"pred{i}/0"}
            for i in range(1, 6)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        report = generate_statistics_report(trace_file, format="csv")

        lines = report.strip().split("\n")
        assert len(lines) > 1
        assert "metric,value" in lines[0] or "Metric,Value" in lines[0]
        assert "total_events,5" in report.lower().replace(" ", "")


class TestPerformanceAnalysis:
    """Test performance analysis functions."""

    def test_analyze_performance_metrics(self, tmp_path):
        """Test analysis of performance metrics from trace."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "slow/0", "ts": 1000.000},
            {"sid": 2, "p": 1, "pid": "slow/0", "ts": 1000.100},  # 100 ms
            {"sid": 3, "p": 0, "pid": "fast/0", "ts": 1000.200},
            {"sid": 4, "p": 1, "pid": "fast/0", "ts": 1000.201},  # 1 ms
            {"sid": 5, "p": 0, "pid": "slow/0", "ts": 1000.300},
            {"sid": 6, "p": 1, "pid": "slow/0", "ts": 1000.400},  # 100 ms
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        metrics = analyze_performance_metrics(trace_file)

        assert "predicate_timings" in metrics
        assert "slow/0" in metrics["predicate_timings"]
        assert "fast/0" in metrics["predicate_timings"]
        assert metrics["predicate_timings"]["slow/0"]["avg_ms"] == pytest.approx(100.0, rel=0.01)
        assert metrics["predicate_timings"]["fast/0"]["avg_ms"] == pytest.approx(1.0, rel=0.01)

    def test_detect_performance_anomalies(self, tmp_path):
        """Test detection of performance anomalies."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            # Normal ~10 ms
            {"sid": 1, "p": 0, "pid": "normal/0", "ts": 1000.000},
            {"sid": 2, "p": 1, "pid": "normal/0", "ts": 1000.010},  # 10 ms
            {"sid": 3, "p": 0, "pid": "normal/0", "ts": 1000.020},
            {"sid": 4, "p": 1, "pid": "normal/0", "ts": 1000.030},  # 10 ms
            # Anomalous ~500 ms
            {"sid": 5, "p": 0, "pid": "normal/0", "ts": 1000.040},
            {"sid": 6, "p": 1, "pid": "normal/0", "ts": 1000.540},  # 500 ms
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        anomalies = detect_performance_anomalies(trace_file, threshold_stddev=2.0)

        assert anomalies  # not empty
        assert any(a["pid"] == "normal/0" and a["duration_ms"] >= 500 for a in anomalies)

    def test_performance_metrics_without_timestamps(self, tmp_path):
        """Test performance analysis handles missing timestamps."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0"},
            {"sid": 2, "p": 1, "pid": "test/0"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        metrics = analyze_performance_metrics(trace_file)

        assert metrics is not None
        # Should handle gracefully without timestamps
        assert "predicate_timings" not in metrics or not metrics["predicate_timings"]

    def test_memory_usage_analysis(self, tmp_path):
        """Test analysis of memory usage patterns."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "store_size": 10, "trail_size": 5},
            {"sid": 2, "p": 0, "store_size": 20, "trail_size": 10},
            {"sid": 3, "p": 1, "store_size": 20, "trail_size": 10},
            {"sid": 4, "p": 3, "store_size": 15, "trail_size": 8},  # After backtrack
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        metrics = analyze_performance_metrics(trace_file)

        assert "memory_usage" in metrics
        assert metrics["memory_usage"]["max_store_size"] == 20
        assert metrics["memory_usage"]["max_trail_size"] == 10
        assert metrics["memory_usage"]["avg_store_size"] == pytest.approx(16.25, rel=0.01)


class TestAdditionalStatistics:
    """Additional test coverage for statistics computation."""

    def test_port_distribution_sums_to_total(self, tmp_path):
        """Test that port distribution sums to total events."""
        trace_file = tmp_path / "trace.jsonl"
        with trace_file.open("w") as f:
            for i in range(20):
                f.write(json.dumps({"sid": i+1, "p": i % 4}) + "\n")

        stats = compute_trace_statistics(trace_file)
        pdist = stats["port_distribution"]
        assert sum(pdist.values()) == stats["total_events"]

    def test_duration_uses_min_max_ts(self, tmp_path):
        """Test duration uses min/max timestamps, not event order."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 2, "p": 0, "ts": 1010.0},
            {"sid": 1, "p": 0, "ts": 1000.0},
            {"sid": 3, "p": 0, "ts": 1020.0},
        ]
        with trace_file.open("w") as f:
            for e in events:
                f.write(json.dumps(e) + "\n")

        stats = compute_trace_statistics(trace_file)
        assert stats["duration_ms"] == pytest.approx(20000.0, rel=0.01)

    def test_call_graph_edge_weights_and_parents(self, tmp_path):
        """Test call graph edge weights and parent tracking."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "a/0", "fd": 0},
            {"sid": 2, "p": 0, "pid": "b/0", "fd": 1},  # a->b
            {"sid": 3, "p": 1, "pid": "b/0", "fd": 1},
            {"sid": 4, "p": 0, "pid": "b/0", "fd": 1},  # a->b again
            {"sid": 5, "p": 1, "pid": "b/0", "fd": 1},
            {"sid": 6, "p": 1, "pid": "a/0", "fd": 0},
        ]
        with trace_file.open("w") as f:
            for e in events:
                f.write(json.dumps(e) + "\n")

        stats = compute_trace_statistics(trace_file)
        graph = stats["call_graph"]
        assert ("a/0", "b/0") in graph["edges"]
        if "weights" in graph:
            assert graph["weights"][("a/0", "b/0")] == 2

    def test_performance_metrics_ignore_partial_pairs(self, tmp_path):
        """Test that performance metrics ignore incomplete CALL/EXIT pairs."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "p/0", "ts": 1.0},
            {"sid": 2, "p": 0, "pid": "p/0", "ts": 2.0},  # another CALL without EXIT
        ]
        with trace_file.open("w") as f:
            for e in events:
                f.write(json.dumps(e) + "\n")

        metrics = analyze_performance_metrics(trace_file)
        assert "predicate_timings" not in metrics or "p/0" not in metrics.get("predicate_timings", {})

    def test_memory_usage_maxes_and_average(self, tmp_path):
        """Test memory usage tracking for max and average values."""
        trace_file = tmp_path / "trace.jsonl"
        vals = [(10, 5), (20, 10), (5, 4), (15, 8)]
        with trace_file.open("w") as f:
            for i, (s, t) in enumerate(vals, 1):
                f.write(json.dumps({"sid": i, "p": 0, "store_size": s, "trail_size": t}) + "\n")

        metrics = analyze_performance_metrics(trace_file)
        assert metrics["memory_usage"]["max_store_size"] == 20
        assert metrics["memory_usage"]["max_trail_size"] == 10
        assert metrics["memory_usage"]["avg_store_size"] == pytest.approx(
            sum(s for s, _ in vals) / len(vals), rel=0.01
        )


class TestReportFormatting:
    """Test different report output formats."""

    def test_markdown_report_generation(self, tmp_path):
        """Test generation of Markdown-formatted reports."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i}/0"}
            for i in range(1, 21)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        report = generate_statistics_report(trace_file, format="markdown")

        assert "# Trace Statistics Report" in report
        assert "## Summary" in report
        assert "| Metric | Value |" in report
        assert "Total Events" in report

    def test_html_report_generation(self, tmp_path):
        """Test generation of HTML-formatted reports."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0"},
            {"sid": 2, "p": 1, "pid": "test/0"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        report = generate_statistics_report(trace_file, format="html")

        assert "<html>" in report or "<!DOCTYPE" in report
        assert "Total Events" in report
        assert "<table>" in report or "<div>" in report