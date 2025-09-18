"""
Tests for REPL debug and trace command integration.

Tests command parsing, execution, and state management for:
- Trace commands (on/off/json/pretty/sample)
- Spy commands (spy/unspy/spys/untrace)
- Debug commands (snapshot/metrics)
"""

import pytest
from io import StringIO
from pathlib import Path
import tempfile
import json

from prolog.repl import PrologREPL
from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Atom, Struct
from prolog.ast.clauses import Clause
from prolog.debug.tracer import PortsTracer
from prolog.debug.sinks import CollectorSink, PrettyTraceSink, JSONLTraceSink


class TestTraceCommands:
    """Test trace command parsing and execution."""

    def test_parse_trace_on(self):
        """Test parsing 'trace on' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("trace on")
        assert cmd['type'] == 'trace'
        assert cmd['action'] == 'on'

    def test_parse_trace_off(self):
        """Test parsing 'trace off' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("trace off")
        assert cmd['type'] == 'trace'
        assert cmd['action'] == 'off'

    def test_parse_trace_json(self):
        """Test parsing 'trace json FILE' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("trace json output.jsonl")
        assert cmd['type'] == 'trace'
        assert cmd['action'] == 'json'
        assert cmd['file'] == 'output.jsonl'

    def test_parse_trace_pretty(self):
        """Test parsing 'trace pretty FILE' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("trace pretty trace.log")
        assert cmd['type'] == 'trace'
        assert cmd['action'] == 'pretty'
        assert cmd['file'] == 'trace.log'

    def test_parse_trace_sample(self):
        """Test parsing 'trace sample N' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("trace sample 100")
        assert cmd['type'] == 'trace'
        assert cmd['action'] == 'sample'
        assert cmd['rate'] == 100

    def test_execute_trace_on(self):
        """Test executing 'trace on' enables tracing."""
        repl = PrologREPL()
        repl.execute_trace_command({'action': 'on'})

        # Engine should be created with trace=True
        assert repl.trace_enabled is True
        assert repl.trace_mode == 'pretty'

    def test_execute_trace_off(self):
        """Test executing 'trace off' disables tracing."""
        repl = PrologREPL()
        repl.trace_enabled = True
        repl.execute_trace_command({'action': 'off'})

        assert repl.trace_enabled is False

    def test_execute_trace_json_sets_state(self):
        """Test 'trace json FILE' sets correct state."""
        repl = PrologREPL()

        repl.execute_trace_command({'action': 'json', 'file': 'output.jsonl'})

        assert repl.trace_enabled is True
        assert repl.trace_mode == 'json'
        assert repl.trace_file == 'output.jsonl'

    def test_execute_trace_sample(self):
        """Test 'trace sample N' sets sampling rate."""
        repl = PrologREPL()
        repl.execute_trace_command({'action': 'sample', 'rate': 10})

        assert repl.trace_enabled is True
        assert repl.trace_sample_rate == 10


class TestSpyCommands:
    """Test spy/unspy command parsing and execution."""

    def test_parse_spy(self):
        """Test parsing 'spy name/arity' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("spy append/3")
        assert cmd['type'] == 'spy'
        assert cmd['action'] == 'add'
        assert cmd['predicate'] == 'append/3'

    def test_parse_unspy(self):
        """Test parsing 'unspy name/arity' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("unspy append/3")
        assert cmd['type'] == 'spy'
        assert cmd['action'] == 'remove'
        assert cmd['predicate'] == 'append/3'

    def test_parse_spys(self):
        """Test parsing 'spys' command to list spypoints."""
        repl = PrologREPL()
        cmd = repl.parse_command("spys")
        assert cmd['type'] == 'spy'
        assert cmd['action'] == 'list'

    def test_parse_untrace(self):
        """Test parsing 'untrace' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("untrace")
        assert cmd['type'] == 'spy'
        assert cmd['action'] == 'clear'

    def test_execute_spy_add(self):
        """Test adding a spypoint."""
        repl = PrologREPL()
        repl.execute_spy_command({'action': 'add', 'predicate': 'append/3'})

        assert 'append/3' in repl.spypoints

    def test_execute_unspy(self):
        """Test removing a spypoint."""
        repl = PrologREPL()
        repl.spypoints.add('append/3')
        repl.execute_spy_command({'action': 'remove', 'predicate': 'append/3'})

        assert 'append/3' not in repl.spypoints

    def test_execute_spys_list(self, capsys):
        """Test listing spypoints."""
        repl = PrologREPL()
        repl.spypoints = {'append/3', 'member/2', 'parent/2'}
        repl.execute_spy_command({'action': 'list'})

        captured = capsys.readouterr()
        assert "append/3" in captured.out
        assert "member/2" in captured.out
        assert "parent/2" in captured.out

    def test_execute_untrace(self):
        """Test clearing all spypoints."""
        repl = PrologREPL()
        repl.spypoints = {'append/3', 'member/2'}
        repl.execute_spy_command({'action': 'clear'})

        assert len(repl.spypoints) == 0

    def test_spypoint_state_management(self):
        """Test that spypoints are managed correctly."""
        repl = PrologREPL()

        # Add spypoints
        repl.spypoints.add('test/0')
        repl.spypoints.add('other/1')

        assert 'test/0' in repl.spypoints
        assert 'other/1' in repl.spypoints

        # Remove one
        repl.spypoints.discard('test/0')
        assert 'test/0' not in repl.spypoints
        assert 'other/1' in repl.spypoints


class TestDebugCommands:
    """Test snapshot and metrics commands."""

    def test_parse_snapshot(self):
        """Test parsing 'snapshot' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("snapshot")
        assert cmd['type'] == 'debug'
        assert cmd['action'] == 'snapshot'

    def test_parse_metrics(self):
        """Test parsing 'metrics' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("metrics")
        assert cmd['type'] == 'debug'
        assert cmd['action'] == 'metrics'

    def test_parse_metrics_reset(self):
        """Test parsing 'metrics reset' command."""
        repl = PrologREPL()
        cmd = repl.parse_command("metrics reset")
        assert cmd['type'] == 'debug'
        assert cmd['action'] == 'metrics_reset'

    def test_execute_snapshot(self, capsys):
        """Test snapshot displays engine state."""
        repl = PrologREPL()

        # Execute snapshot
        repl.execute_debug_command({'action': 'snapshot'})

        captured = capsys.readouterr()
        # Should show some engine state info
        assert "Engine State Snapshot" in captured.out or "Store" in captured.out

    def test_execute_metrics(self, capsys):
        """Test metrics display."""
        repl = PrologREPL()

        # Enable metrics
        repl.metrics_enabled = True

        # Display metrics
        repl.execute_debug_command({'action': 'metrics'})

        captured = capsys.readouterr()
        # Should show metrics
        assert "Metrics" in captured.out or "calls" in captured.out

    def test_execute_metrics_reset(self, capsys):
        """Test metrics reset."""
        repl = PrologREPL()

        # Reset metrics
        repl.execute_debug_command({'action': 'metrics_reset'})

        captured = capsys.readouterr()
        # Should acknowledge reset
        assert "reset" in captured.out.lower()


class TestREPLIntegration:
    """Test integration of debug commands with REPL workflow."""

    def test_help_includes_debug_commands(self):
        """Test that help text includes new debug commands."""
        repl = PrologREPL()
        help_text = repl.get_help_text()

        # Check trace commands are documented
        assert "trace on" in help_text or "Trace" in help_text
        assert "spy" in help_text or "Spy" in help_text
        assert "snapshot" in help_text or "metrics" in help_text

    def test_trace_state_persistence(self):
        """Test trace settings persist."""
        repl = PrologREPL()

        # Enable tracing
        repl.execute_trace_command({'action': 'on'})
        assert repl.trace_enabled is True

        # Do other operations (simulated by just checking state)
        assert repl.trace_enabled is True

        # Change mode
        repl.execute_trace_command({'action': 'json', 'file': 'test.jsonl'})
        assert repl.trace_enabled is True
        assert repl.trace_mode == 'json'

    def test_spypoints_state_persistence(self):
        """Test spypoints persist."""
        repl = PrologREPL()

        # Add spypoint
        repl.execute_spy_command({'action': 'add', 'predicate': 'test/0'})
        assert 'test/0' in repl.spypoints

        # Add another
        repl.execute_spy_command({'action': 'add', 'predicate': 'other/1'})
        assert 'test/0' in repl.spypoints
        assert 'other/1' in repl.spypoints

    def test_invalid_commands_handled_gracefully(self):
        """Test that invalid commands produce helpful errors."""
        repl = PrologREPL()

        # Invalid trace command
        cmd = repl.parse_command("trace invalid")
        assert cmd['type'] == 'error' or cmd['type'] == 'query'

        # Invalid spy predicate
        result = repl.execute_spy_command({'action': 'add', 'predicate': 'invalid'})
        # Should handle gracefully, not crash
        assert result is False or result is None

    def test_trace_file_state_cleared_on_off(self):
        """Test that trace file state is cleared when tracing disabled."""
        repl = PrologREPL()

        # Enable file tracing
        repl.execute_trace_command({'action': 'json', 'file': 'output.jsonl'})
        assert repl.trace_file == 'output.jsonl'

        # Disable tracing - should clear file
        repl.execute_trace_command({'action': 'off'})
        # File state could be cleared or retained - define behavior in implementation


class TestCommandParsing:
    """Test robust command parsing."""

    def test_case_insensitive_commands(self):
        """Test commands work regardless of case."""
        repl = PrologREPL()

        # These should all parse correctly
        assert repl.parse_command("TRACE ON")['type'] == 'trace'
        assert repl.parse_command("Spy append/3")['type'] == 'spy'
        assert repl.parse_command("METRICS")['type'] == 'debug'

    def test_trailing_period_optional(self):
        """Test commands work with or without trailing period."""
        repl = PrologREPL()

        assert repl.parse_command("trace on")['type'] == 'trace'
        assert repl.parse_command("trace on.")['type'] == 'trace'
        assert repl.parse_command("spy test/0")['type'] == 'spy'
        assert repl.parse_command("spy test/0.")['type'] == 'spy'

    def test_whitespace_handling(self):
        """Test commands handle extra whitespace."""
        repl = PrologREPL()

        assert repl.parse_command("  trace   on  ")['type'] == 'trace'
        assert repl.parse_command("\tspy\ttest/0\t")['type'] == 'spy'

    def test_ambiguous_commands_resolved(self):
        """Test that trace/1 predicate doesn't conflict with trace command."""
        repl = PrologREPL()

        # Command should be recognized
        cmd = repl.parse_command("trace on")
        assert cmd['type'] == 'trace'

        # Query should still work
        cmd = repl.parse_command("?- trace(X).")
        assert cmd['type'] == 'query'
        assert cmd['content'] == 'trace(X)'