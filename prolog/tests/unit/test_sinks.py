"""
Tests for trace output sinks.

Tests the trace output system including buffering, rotation,
backpressure handling, and multiple output formats.
"""

import pytest
import json
import os
import tempfile
import re
from unittest.mock import patch, MagicMock
from collections import deque
from io import StringIO
from pytest import approx

from prolog.debug.tracer import TraceEvent
from prolog.debug.sinks import (
    TraceSink, PrettyTraceSink, JSONLTraceSink,
    FileTraceSink
)


class TestTraceSinkBase:
    """Tests for the TraceSink abstract base class."""

    def test_ring_buffer_capacity(self):
        """Ring buffer respects maxlen capacity."""
        sink = TestMemoryTraceSink(maxlen=3)

        # Add 5 events to a buffer with capacity 3
        for i in range(5):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Should only have last 3 events
        assert len(sink.buffer) == 3
        assert sink.buffer[0].step_id == 2
        assert sink.buffer[1].step_id == 3
        assert sink.buffer[2].step_id == 4

    def test_events_dropped_counter(self):
        """Dropped events are tracked correctly."""
        sink = TestMemoryTraceSink(maxlen=2)

        # Fill buffer
        for i in range(5):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Should have dropped 3 events (0, 1, 2)
        assert sink.events_dropped_total == 3

    def test_flush_called(self):
        """Flush is called when needed."""
        sink = TestMemoryTraceSink(batch_size=3)

        # Add events up to batch size
        for i in range(3):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Batch should be flushed
        assert sink.flush_count == 1

    def test_close_flushes_pending(self):
        """Close flushes any pending events."""
        sink = TestMemoryTraceSink(batch_size=10)

        # Add fewer events than batch size
        for i in range(3):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Close should flush
        sink.close()
        assert sink.flush_count == 1
        assert sink.closed

    def _make_event(self, step_id=1, port="call", **kwargs):
        """Create a test TraceEvent."""
        defaults = {
            'version': 1,
            'run_id': "test_run",
            'step_id': step_id,
            'port': port,
            'pred_id': "test/1",
            'goal': "test(X)",
            'frame_depth': 0,
            'cp_depth': 0,
            'goal_height': 1,
            'write_stamp': 0
        }
        defaults.update(kwargs)
        return TraceEvent(**defaults)


class TestPrettyTraceSink:
    """Tests for PrettyTraceSink human-readable output."""

    def test_pretty_format(self):
        """Pretty format is human-readable."""
        output = StringIO()
        sink = PrettyTraceSink(output=output)

        event = self._make_event(
            step_id=1,
            port="call",
            pred_id="member/2",
            goal_pretty="member(X, [1,2,3])"
        )
        sink.write_event(event)
        sink.flush()

        result = output.getvalue()
        assert "1" in result  # step_id
        assert "CALL" in result  # port
        assert "member/2" in result  # pred_id
        assert "member(X, [1,2,3])" in result  # goal

    @pytest.mark.parametrize("port,expected", [
        ("call", "CALL"),
        ("exit", "EXIT"),
        ("redo", "REDO"),
        ("fail", "FAIL")
    ])
    def test_pretty_port_formatting(self, port, expected):
        """Ports are formatted consistently."""
        output = StringIO()
        sink = PrettyTraceSink(output=output)

        event = self._make_event(port=port)
        sink.write_event(event)
        sink.flush()
        assert expected in output.getvalue()

    def test_pretty_depth_indicators(self):
        """Depth shown with indentation."""
        output = StringIO()
        sink = PrettyTraceSink(output=output)

        # Event at depth 3
        event = self._make_event(frame_depth=3)
        sink.write_event(event)
        sink.flush()

        line = output.getvalue().splitlines()[0]
        # Check exact indentation (assuming 2 spaces per depth level)
        m = re.match(r'^(\s*)\[\d+\]\s', line)
        assert m, f"line not in expected format: {line!r}"
        assert len(m.group(1)) == 6  # 3 depth * 2 spaces

    def test_pretty_caps_applied(self):
        """Term depth and list length caps are applied."""
        output = StringIO()
        sink = PrettyTraceSink(output=output, max_term_depth=2)

        # Deep nested term
        event = self._make_event(
            goal_pretty="f(g(h(i(j(k(l(m(n(o(p(q(r(s(t(u(v(w(x(y(z))))))))))))))))))"
        )
        sink.write_event(event)
        sink.flush()

        result = output.getvalue()
        # Should be truncated (accept both ASCII and Unicode ellipsis)
        assert ("..." in result) or ("…" in result)

    def test_file_output(self):
        """Can write to file instead of stdout."""
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as f:
            filepath = f.name

        try:
            sink = PrettyTraceSink(file_path=filepath)
            event = self._make_event()
            sink.write_event(event)
            sink.close()

            # Read back from file
            with open(filepath, 'r') as f:
                content = f.read()
                assert "test/1" in content
        finally:
            os.unlink(filepath)

    def _make_event(self, step_id=1, port="call", pred_id="test/1",
                    goal_pretty="test(X)", frame_depth=0, **kwargs):
        """Create a test TraceEvent."""
        defaults = {
            'version': 1,
            'run_id': "test_run",
            'step_id': step_id,
            'port': port,
            'pred_id': pred_id,
            'goal': goal_pretty,
            'frame_depth': frame_depth,
            'cp_depth': 0,
            'goal_height': 1,
            'write_stamp': 0
        }
        defaults.update(kwargs)
        return TraceEvent(**defaults)


class TestJSONLTraceSink:
    """Tests for JSONLTraceSink machine-readable output."""

    def test_jsonl_format(self):
        """JSONL output is valid JSON Lines."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        # Write multiple events
        for i in range(3):
            event = self._make_event(step_id=i)
            sink.write_event(event)
        sink.flush()

        # Each line should be valid JSON
        lines = output.getvalue().strip().split('\n')
        assert len(lines) == 3

        for line in lines:
            obj = json.loads(line)  # Should not raise
            assert 'sid' in obj  # step_id
            assert 'p' in obj    # port
            assert 'pid' in obj  # pred_id

    def test_compact_keys(self):
        """Uses compact keys per schema v1."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        event = self._make_event()
        sink.write_event(event)
        sink.flush()

        line = output.getvalue().strip()
        obj = json.loads(line)

        # Check compact key mappings
        assert 'sid' in obj  # step_id
        assert 'p' in obj    # port
        assert 'fd' in obj   # frame_depth
        assert 'cd' in obj   # cp_depth
        assert 'gh' in obj   # goal_height
        assert 'ws' in obj   # write_stamp
        assert 'pid' in obj  # pred_id
        assert 'g' in obj    # goal
        assert 'rid' in obj  # run_id

    @pytest.mark.parametrize("port_name,port_code", [
        ("call", 0),
        ("exit", 1),
        ("redo", 2),
        ("fail", 3)
    ])
    def test_port_encoding(self, port_name, port_code):
        """Ports encoded as integers 0-3."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        event = self._make_event(port=port_name)
        sink.write_event(event)
        sink.flush()

        obj = json.loads(output.getvalue().strip())
        assert obj['p'] == port_code

    def test_optional_fields_omitted(self):
        """None fields are omitted from output."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        event = self._make_event()
        # These should be None
        assert event.timestamp is None
        assert event.bindings is None

        sink.write_event(event)
        sink.flush()

        line = output.getvalue().strip()
        obj = json.loads(line)

        # Optional fields should not be present
        assert 'ts' not in obj  # timestamp
        assert 'b' not in obj   # bindings

    def test_timestamp_included_when_present(self):
        """Timestamp included when not None."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        event = self._make_event(timestamp=1234567890.123)
        sink.write_event(event)
        sink.flush()

        obj = json.loads(output.getvalue().strip())
        assert 'ts' in obj
        assert obj['ts'] == approx(1234567890.123, rel=0, abs=1e-6)

    def test_jsonl_includes_version(self):
        """JSONL includes schema version."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        sink.write_event(self._make_event())
        sink.flush()

        obj = json.loads(output.getvalue().strip())
        assert obj.get('v') == 1

    def test_jsonl_bindings_serialised_when_present(self):
        """Bindings included when not None."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        event = self._make_event(bindings={'X': 1, 'Y': 2})
        sink.write_event(event)
        sink.flush()

        obj = json.loads(output.getvalue().strip())
        assert 'b' in obj
        assert obj['b'] == {'X': 1, 'Y': 2}

    def _make_event(self, step_id=1, port="call", **kwargs):
        """Create a test TraceEvent."""
        defaults = {
            'version': 1,
            'run_id': "test_run",
            'step_id': step_id,
            'port': port,
            'pred_id': "test/1",
            'goal': "test(X)",
            'frame_depth': 0,
            'cp_depth': 0,
            'goal_height': 1,
            'write_stamp': 0
        }
        defaults.update(kwargs)
        return TraceEvent(**defaults)


class TestFileTraceSink:
    """Tests for FileTraceSink with rotation."""

    def test_file_creation(self):
        """Creates output file correctly."""
        with tempfile.TemporaryDirectory() as tmpdir:
            filepath = os.path.join(tmpdir, "trace.jsonl")
            sink = FileTraceSink(filepath, format="jsonl")

            event = self._make_event()
            sink.write_event(event)
            sink.close()

            assert os.path.exists(filepath)
            with open(filepath, 'r') as f:
                content = f.read()
                assert len(content) > 0

    def test_rotation_by_size(self):
        """Rotates file when max size reached."""
        with tempfile.TemporaryDirectory() as tmpdir:
            filepath = os.path.join(tmpdir, "trace.jsonl")
            # Very small max size to trigger rotation
            sink = FileTraceSink(
                filepath,
                format="jsonl",
                max_size_mb=0.0001  # ~100 bytes
            )

            # Write many events to trigger rotation
            for i in range(100):
                event = self._make_event(step_id=i)
                sink.write_event(event)
            sink.close()

            # Should have created rotated files
            files = os.listdir(tmpdir)
            assert len(files) > 1  # Original + rotated
            assert any('.1' in f for f in files)  # Rotated file

    def test_max_rotated_files(self):
        """Keeps only max_files rotated files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            filepath = os.path.join(tmpdir, "trace.jsonl")
            sink = FileTraceSink(
                filepath,
                format="jsonl",
                max_size_mb=0.0001,  # ~100 bytes
                max_files=2  # Keep only 2 rotated files
            )

            # Write many events to create multiple rotations
            for i in range(500):
                event = self._make_event(step_id=i)
                sink.write_event(event)
            sink.close()

            # Count rotated files
            files = os.listdir(tmpdir)
            rotated = [f for f in files if '.jsonl.' in f]
            assert len(rotated) <= 2  # Should not exceed max_files

    def test_batch_writing(self):
        """Batches writes for efficiency."""
        with tempfile.TemporaryDirectory() as tmpdir:
            filepath = os.path.join(tmpdir, "trace.jsonl")

            with patch('builtins.open', MagicMock()) as mock_open:
                mock_file = MagicMock()
                mock_open.return_value.__enter__.return_value = mock_file

                sink = FileTraceSink(
                    filepath,
                    format="jsonl",
                    batch_size=10
                )

                # Write less than batch size
                for i in range(5):
                    event = self._make_event(step_id=i)
                    sink.write_event(event)

                # Should not have written yet
                assert mock_file.write.call_count == 0

                # Write more to reach batch size
                for i in range(5, 10):
                    event = self._make_event(step_id=i)
                    sink.write_event(event)

                # Now should have written exactly once (batch of 10)
                assert mock_file.write.call_count == 1

    def test_close_flushes_pending(self):
        """Close flushes pending events."""
        with tempfile.TemporaryDirectory() as tmpdir:
            filepath = os.path.join(tmpdir, "trace.jsonl")
            sink = FileTraceSink(
                filepath,
                format="jsonl",
                batch_size=100  # Large batch
            )

            # Write fewer than batch size
            event = self._make_event()
            sink.write_event(event)

            # Close should flush
            sink.close()

            # File should contain the event
            with open(filepath, 'r') as f:
                content = f.read()
                assert "test/1" in content

    def test_format_selection(self):
        """Supports both pretty and jsonl formats."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Test JSONL format
            jsonl_path = os.path.join(tmpdir, "trace.jsonl")
            jsonl_sink = FileTraceSink(jsonl_path, format="jsonl")
            event = self._make_event()
            jsonl_sink.write_event(event)
            jsonl_sink.close()

            with open(jsonl_path, 'r') as f:
                json.loads(f.read().strip())  # Should be valid JSON

            # Test pretty format
            pretty_path = os.path.join(tmpdir, "trace.log")
            pretty_sink = FileTraceSink(pretty_path, format="pretty")
            pretty_sink.write_event(event)
            pretty_sink.close()

            with open(pretty_path, 'r') as f:
                content = f.read()
                assert "CALL" in content  # Pretty format indicator

    def test_file_close_idempotent_and_writes_after_close_fail(self):
        """Close is idempotent and writes after close fail."""
        with tempfile.TemporaryDirectory() as tmpdir:
            filepath = os.path.join(tmpdir, "trace.jsonl")
            sink = FileTraceSink(filepath, format="jsonl")

            sink.write_event(self._make_event())
            sink.close()

            # Idempotent close
            sink.close()

            # Writing after close should return False
            assert sink.write_event(self._make_event()) is False

    def _make_event(self, step_id=1, port="call", **kwargs):
        """Create a test TraceEvent."""
        defaults = {
            'version': 1,
            'run_id': "test_run",
            'step_id': step_id,
            'port': port,
            'pred_id': "test/1",
            'goal': "test(X)",
            'frame_depth': 0,
            'cp_depth': 0,
            'goal_height': 1,
            'write_stamp': 0
        }
        defaults.update(kwargs)
        return TraceEvent(**defaults)


class TestBackpressure:
    """Tests for backpressure handling."""

    def test_drops_when_buffer_full(self):
        """Events dropped when buffer is full."""
        sink = TestMemoryTraceSink(maxlen=2, drop_on_full=True)

        # Fill buffer completely
        for i in range(10):
            event = self._make_event(step_id=i)
            result = sink.write_event(event)

            if i < 2:
                assert result is True  # Accepted
            else:
                # Later events dropped when full
                pass  # Depends on implementation

        # Should track drops
        assert sink.events_dropped_total > 0

    def test_step_id_is_monotonic_under_drops(self):
        """step_id is monotonic despite drops."""
        sink = TestMemoryTraceSink(maxlen=3, drop_on_full=True)

        for i in range(10):
            sink.write_event(self._make_event(step_id=i))

        # Buffer should have last 3 events with monotonic IDs
        buf_ids = [e.step_id for e in sink.buffer]
        assert all(b >= a for a, b in zip(buf_ids, buf_ids[1:]))

    def test_step_id_is_contiguous_when_no_drops(self):
        """step_id is strictly contiguous when no drops occur."""
        output = StringIO()
        sink = JSONLTraceSink(output=output)

        for i in range(10):
            event = self._make_event(step_id=i)
            sink.write_event(event)
        sink.flush()

        step_ids = [json.loads(line)['sid']
                    for line in output.getvalue().splitlines() if line]
        # Strict contiguity (no drops expected)
        assert step_ids == list(range(10))

    def test_engine_unaffected_by_drops(self):
        """Engine results unaffected by sink drops."""
        # This would be tested at integration level
        # Here we just verify the interface
        sink = TestMemoryTraceSink(maxlen=1, drop_on_full=True)

        # Write returns success/failure but doesn't raise
        event = self._make_event()
        result1 = sink.write_event(event)
        result2 = sink.write_event(event)

        # Should not raise exceptions
        assert isinstance(result1, bool)
        assert isinstance(result2, bool)

    def test_drop_reason_is_buffer_full_when_capacity_reached(self):
        """Track reason for drops when buffer full."""
        sink = TestMemoryTraceSink(maxlen=2)

        # Fill buffer
        for i in range(5):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Should have specific drop reason
        assert hasattr(sink, 'drop_reason')
        assert sink.drop_reason == 'buffer_full'

    def _make_event(self, step_id=1, port="call", **kwargs):
        """Create a test TraceEvent."""
        defaults = {
            'version': 1,
            'run_id': "test_run",
            'step_id': step_id,
            'port': port,
            'pred_id': "test/1",
            'goal': "test(X)",
            'frame_depth': 0,
            'cp_depth': 0,
            'goal_height': 1,
            'write_stamp': 0
        }
        defaults.update(kwargs)
        return TraceEvent(**defaults)


class TestMemoryBounds:
    """Tests for memory-bounded operation."""

    def test_memory_bounded_with_large_traces(self):
        """Memory usage bounded even with large traces."""
        sink = TestMemoryTraceSink(maxlen=1000)

        # Write many more events than buffer size
        for i in range(10000):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Buffer should not exceed maxlen
        assert len(sink.buffer) <= 1000

        # Should have tracked drops
        assert sink.events_dropped_total == 9000

    def test_batch_size_limits_memory(self):
        """Batch size prevents unbounded memory growth."""
        output = StringIO()
        sink = JSONLTraceSink(output=output, batch_size=100)

        # Track flushes
        flush_count = 0
        original_flush = sink.flush

        def counting_flush():
            nonlocal flush_count
            flush_count += 1
            return original_flush()

        sink.flush = counting_flush

        # Write many events
        for i in range(1000):
            event = self._make_event(step_id=i)
            sink.write_event(event)

        # Should have flushed multiple times
        assert flush_count >= 9  # At least 900/100 flushes

    def _make_event(self, step_id=1, port="call", **kwargs):
        """Create a test TraceEvent."""
        defaults = {
            'version': 1,
            'run_id': "test_run",
            'step_id': step_id,
            'port': port,
            'pred_id': "test/1",
            'goal': "test(X)",
            'frame_depth': 0,
            'cp_depth': 0,
            'goal_height': 1,
            'write_stamp': 0
        }
        defaults.update(kwargs)
        return TraceEvent(**defaults)


# Helper implementation for testing - will be replaced by real implementation
class TestMemoryTraceSink(TraceSink):
    """In-memory sink for testing."""

    def __init__(self, maxlen=1000, batch_size=100, drop_on_full=False):
        super().__init__(maxlen=maxlen, batch_size=batch_size)
        self.buffer = deque(maxlen=maxlen)
        self.flush_count = 0
        self.closed = False
        self.drop_on_full = drop_on_full
        self.drop_reason = None

    def _write_batch(self, events):
        """Write a batch of events to memory."""
        for event in events:
            if self.buffer.maxlen and len(self.buffer) >= self.buffer.maxlen:
                self.events_dropped_total += 1
                self.drop_reason = 'buffer_full'
                if self.drop_on_full:
                    return False
            self.buffer.append(event)
        self.flush_count += 1
        return True

    def close(self):
        """Close the sink."""
        if self.batch:
            self.flush()
        self.closed = True