"""Tests for replay tool CLI interface."""

import json
import sys
import pytest
from unittest.mock import patch

from prolog.tools.replay_trace import main as replay_main
from prolog.tools.analyze_trace import main as analyze_main


def _write_events(path, events):
    """Helper to write events to a JSONL file."""
    with path.open("w") as f:
        for e in events:
            f.write(json.dumps(e) + "\n")


class TestReplayToolCLI:
    """Test replay tool command-line interface."""

    def test_replay_cli_basic_usage(self, tmp_path, capsys):
        """Test basic replay tool CLI usage."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i}/0", "fd": 0} for i in range(1, 11)
        ]
        _write_events(trace_file, events)

        with patch.object(sys, "argv", ["replay_trace", str(trace_file)]):
            replay_main()

        captured = capsys.readouterr()
        assert "Loaded 10 events" in captured.out
        assert "violation" not in captured.out.lower()
        assert captured.err == ""

    def test_replay_cli_with_last_n_option(self, tmp_path, capsys):
        """Test replay tool with --last N option."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": 0, "pid": f"pred{i}/0", "fd": 0} for i in range(1, 301)
        ]
        _write_events(trace_file, events)

        with patch.object(
            sys,
            "argv",
            ["replay_trace", str(trace_file), "--last", "50", "--format", "json"],
        ):
            replay_main()

        captured = capsys.readouterr()
        data = json.loads(captured.out)
        sids = [e["sid"] for e in data.get("events", [])]
        assert sids == list(range(251, 301))  # exactly the last 50

    def test_replay_cli_check_invariants(self, tmp_path, capsys):
        """Test replay tool invariant checking."""
        trace_file = tmp_path / "trace.jsonl"
        # Create trace with invariant violation
        events = [
            {"sid": 1, "p": 0, "pid": "test/0", "fd": 0},
            {"sid": 3, "p": 1, "pid": "test/0", "fd": 0},  # Skip sid=2
        ]
        _write_events(trace_file, events)

        with patch.object(
            sys, "argv", ["replay_trace", str(trace_file), "--check-invariants"]
        ):
            with pytest.raises(SystemExit) as exc:
                replay_main()
            assert exc.value.code != 0

        captured = capsys.readouterr()
        assert "violation" in captured.out.lower() or "error" in captured.out.lower()

    def test_replay_cli_output_format(self, tmp_path, capsys):
        """Test replay tool output format options."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0", "fd": 0},
            {"sid": 2, "p": 1, "pid": "test/0", "fd": 0},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        # Test JSON output
        with patch.object(
            sys, "argv", ["replay_trace", str(trace_file), "--format", "json"]
        ):
            replay_main()

        captured = capsys.readouterr()
        # Output should be valid JSON
        result = json.loads(captured.out)
        assert "events" in result
        assert result.get("total") == 2 or len(result["events"]) == 2

    def test_replay_cli_verbose_mode(self, tmp_path, capsys):
        """Test replay tool verbose mode."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0", "fd": 0},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        with patch.object(sys, "argv", ["replay_trace", str(trace_file), "-v"]):
            replay_main()

        captured = capsys.readouterr()
        # Verbose mode should show more details
        assert "test/0" in captured.out or "sid" in captured.out

    def test_replay_cli_handles_missing_file(self, capsys):
        """Test replay tool handles missing file gracefully."""
        with patch.object(sys, "argv", ["replay_trace", "nonexistent.jsonl"]):
            with pytest.raises(SystemExit) as exc:
                replay_main()
            assert exc.value.code != 0

        captured = capsys.readouterr()
        assert captured.err  # non-empty error channel
        assert "not found" in captured.err.lower() or "error" in captured.err.lower()

    def test_replay_cli_handles_corrupted_file(self, tmp_path, capsys):
        """Test replay tool handles corrupted files."""
        trace_file = tmp_path / "trace.jsonl"
        trace_file.write_text("not valid json\n")

        with patch.object(sys, "argv", ["replay_trace", str(trace_file)]):
            with pytest.raises(SystemExit) as exc:
                replay_main()
            assert exc.value.code != 0

        captured = capsys.readouterr()
        assert captured.err  # non-empty error channel
        assert "error" in captured.err.lower() or "invalid" in captured.err.lower()

    def test_replay_cli_help(self, capsys):
        """Test replay tool help/usage output."""
        with patch.object(sys, "argv", ["replay_trace", "--help"]):
            with pytest.raises(SystemExit) as exc:
                replay_main()
            assert exc.value.code == 0

        captured = capsys.readouterr()
        assert "usage" in captured.out.lower()


class TestAnalyzeToolCLI:
    """Test analyze tool command-line interface."""

    def test_analyze_cli_basic_usage(self, tmp_path, capsys):
        """Test basic analyze tool CLI usage."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": i, "p": i % 4, "pid": f"pred{i % 3}/0", "fd": i % 5}
            for i in range(1, 101)
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        with patch.object(sys, "argv", ["analyze_trace", str(trace_file)]):
            analyze_main()

        captured = capsys.readouterr()
        assert (
            "Total Events: 100" in captured.out or "100 events" in captured.out.lower()
        )
        assert "Hot Predicates" in captured.out or "predicates" in captured.out.lower()

    def test_analyze_cli_statistics_output(self, tmp_path, capsys):
        """Test analyze tool statistics output."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "foo/0", "fd": 0},
            {"sid": 2, "p": 1, "pid": "foo/0", "fd": 0},
            {"sid": 3, "p": 0, "pid": "bar/0", "fd": 1},
            {"sid": 4, "p": 3, "pid": "bar/0", "fd": 1},
        ]
        _write_events(trace_file, events)

        with patch.object(sys, "argv", ["analyze_trace", str(trace_file), "--stats"]):
            analyze_main()

        captured = capsys.readouterr()
        assert "Port Distribution" in captured.out or "ports" in captured.out.lower()
        assert "CALL" in captured.out or "call" in captured.out.lower()

    def test_analyze_cli_json_output(self, tmp_path, capsys):
        """Test analyze tool JSON output format."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0"},
            {"sid": 2, "p": 1, "pid": "test/0"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        with patch.object(
            sys, "argv", ["analyze_trace", str(trace_file), "--format", "json"]
        ):
            analyze_main()

        captured = capsys.readouterr()
        result = json.loads(captured.out)
        assert "total_events" in result
        assert result["total_events"] == 2
        assert "port_distribution" in result

    def test_analyze_cli_hot_predicates(self, tmp_path, capsys):
        """Test analyze tool hot predicate detection."""
        trace_file = tmp_path / "trace.jsonl"
        events = []
        for i in range(1, 51):
            pid = "hot/0" if i % 2 == 0 else f"cold{i % 10}/0"
            events.append({"sid": i, "p": 0, "pid": pid})

        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        with patch.object(
            sys, "argv", ["analyze_trace", str(trace_file), "--top", "3"]
        ):
            analyze_main()

        captured = capsys.readouterr()
        assert "hot/0" in captured.out
        assert "25" in captured.out or "50%" in captured.out

    def test_analyze_cli_output_file(self, tmp_path):
        """Test analyze tool output to file."""
        trace_file = tmp_path / "trace.jsonl"
        output_file = tmp_path / "report.txt"
        events = [
            {"sid": 1, "p": 0, "pid": "test/0"},
            {"sid": 2, "p": 1, "pid": "test/0"},
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        with patch.object(
            sys, "argv", ["analyze_trace", str(trace_file), "-o", str(output_file)]
        ):
            analyze_main()

        assert output_file.exists()
        content = output_file.read_text()
        assert "Total Events" in content or "events" in content.lower()

    def test_analyze_cli_performance_analysis(self, tmp_path, capsys):
        """Test analyze tool performance analysis."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0, "pid": "slow/0", "ts": 1000.000},
            {"sid": 2, "p": 1, "pid": "slow/0", "ts": 1000.100},  # 100 ms
            {"sid": 3, "p": 0, "pid": "fast/0", "ts": 1000.200},
            {"sid": 4, "p": 1, "pid": "fast/0", "ts": 1000.201},  # 1 ms
        ]
        _write_events(trace_file, events)

        with patch.object(
            sys, "argv", ["analyze_trace", str(trace_file), "--performance"]
        ):
            analyze_main()

        captured = capsys.readouterr()
        out = captured.out.lower()
        assert "slow/0" in out
        assert "100" in out and "ms" in out  # May be formatted as "100.0 ms"

    def test_analyze_cli_help(self, capsys):
        """Test analyze tool help/usage output."""
        with patch.object(sys, "argv", ["analyze_trace", "--help"]):
            with pytest.raises(SystemExit) as exc:
                analyze_main()
            assert exc.value.code == 0

        captured = capsys.readouterr()
        assert "usage" in captured.out.lower()

    def test_analyze_cli_invalid_format(self, tmp_path, capsys):
        """Test analyze tool with invalid format option."""
        trace_file = tmp_path / "trace.jsonl"
        _write_events(trace_file, [{"sid": 1, "p": 0}])

        with patch.object(
            sys, "argv", ["analyze_trace", str(trace_file), "--format", "yaml"]
        ):
            with pytest.raises(SystemExit) as exc:
                analyze_main()
            assert exc.value.code != 0

    def test_analyze_cli_json_output_to_file(self, tmp_path):
        """Test analyze tool JSON output to file."""
        trace_file = tmp_path / "trace.jsonl"
        output_file = tmp_path / "report.json"
        _write_events(trace_file, [{"sid": 1, "p": 0}, {"sid": 2, "p": 1}])

        with patch.object(
            sys,
            "argv",
            [
                "analyze_trace",
                str(trace_file),
                "--format",
                "json",
                "-o",
                str(output_file),
            ],
        ):
            analyze_main()

        assert output_file.exists()
        json.loads(output_file.read_text())  # Verify valid JSON

    def test_analyze_cli_backtrack_analysis(self, tmp_path, capsys):
        """Test analyze tool backtrack analysis."""
        trace_file = tmp_path / "trace.jsonl"
        events = [
            {"sid": 1, "p": 0},  # CALL
            {"sid": 2, "p": 1},  # EXIT
            {"sid": 3, "p": 2},  # REDO
            {"sid": 4, "p": 3},  # FAIL
        ]
        with trace_file.open("w") as f:
            for event in events:
                f.write(json.dumps(event) + "\n")

        with patch.object(
            sys, "argv", ["analyze_trace", str(trace_file), "--backtrack"]
        ):
            analyze_main()

        captured = capsys.readouterr()
        out = captured.out.lower()
        assert "backtrack" in out
        assert "50%" in out or "2/4" in out or "2 of 4" in out
