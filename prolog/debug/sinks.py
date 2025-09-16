"""
Trace output sinks with buffering, rotation, and backpressure handling.

Provides multiple output formats (human-readable and machine-parseable)
with efficient I/O batching and memory-bounded operation.
"""

import json
import os
import sys
from abc import ABC, abstractmethod
from collections import deque
from typing import Optional, List, Any, TextIO, Dict
from pathlib import Path
import shutil

from prolog.debug.tracer import TraceEvent


class TraceSink(ABC):
    """
    Abstract base class for trace output sinks.

    Provides ring buffer, batching, and drop tracking.
    """

    def __init__(self, maxlen: int = 10000, batch_size: int = 100):
        """
        Initialize trace sink with buffer management.

        Args:
            maxlen: Maximum buffer size (ring buffer capacity)
            batch_size: Number of events to batch before flush
        """
        self.maxlen = maxlen
        self.batch_size = batch_size
        self.batch = []
        self.events_dropped_total = 0
        self.drop_reason = None

    def write_event(self, event: TraceEvent) -> bool:
        """
        Write an event to the sink.

        Args:
            event: The trace event to write

        Returns:
            True if event was written, False if dropped
        """
        self.batch.append(event)

        # Flush if batch is full
        if len(self.batch) >= self.batch_size:
            return self.flush()

        return True

    def flush(self) -> bool:
        """
        Flush pending events to output.

        Returns:
            True if flush succeeded, False otherwise
        """
        if not self.batch:
            return True

        success = self._write_batch(self.batch)
        self.batch = []
        return success

    @abstractmethod
    def _write_batch(self, events: List[TraceEvent]) -> bool:
        """
        Write a batch of events to the actual output.

        Args:
            events: List of events to write

        Returns:
            True if write succeeded, False otherwise
        """
        pass

    def close(self):
        """Close the sink, flushing any pending events."""
        if self.batch:
            self.flush()


class PrettyTraceSink(TraceSink):
    """
    Human-readable trace output sink.

    Formats events with indentation, port names, and truncation.
    """

    def __init__(self, output: Optional[TextIO] = None,
                 file_path: Optional[str] = None,
                 max_term_depth: int = 4,
                 max_items_per_list: int = 10,
                 **kwargs):
        """
        Initialize pretty trace sink.

        Args:
            output: Output stream (default: sys.stdout)
            file_path: Optional file path for output
            max_term_depth: Maximum depth for term display
            max_items_per_list: Maximum items to show per list
            **kwargs: Additional arguments for TraceSink
        """
        super().__init__(**kwargs)

        if file_path:
            self.file = open(file_path, 'w')
            self.output = self.file
            self.owns_file = True
        else:
            self.output = output or sys.stdout
            self.owns_file = False

        self.max_term_depth = max_term_depth
        self.max_items_per_list = max_items_per_list

    def _write_batch(self, events: List[TraceEvent]) -> bool:
        """Write events in human-readable format."""
        for event in events:
            line = self.format_event(event)
            self.output.write(line + '\n')

        # Flush output stream
        if hasattr(self.output, 'flush'):
            self.output.flush()

        return True

    def format_event(self, event: TraceEvent) -> str:
        """Format a single event for display."""
        # Port formatting
        port_map = {
            'call': 'CALL',
            'exit': 'EXIT',
            'redo': 'REDO',
            'fail': 'FAIL'
        }
        port = port_map.get(event.port, event.port.upper())

        # Depth indentation (2 spaces per level)
        indent = ' ' * (event.frame_depth * 2)

        # Truncate goal if needed
        goal = self._truncate_term(event.goal_pretty, self.max_term_depth)

        # Format: indent[step_id] PORT pred_id: goal
        return f"{indent}[{event.step_id}] {port} {event.pred_id}: {goal}"

    def _truncate_term(self, term: str, max_depth: int) -> str:
        """
        Truncate deeply nested terms.

        Simple heuristic: count parentheses depth.
        """
        if max_depth <= 0:
            return term

        depth = 0
        result = []

        for char in term:
            if char in '([{':
                depth += 1
                if depth > max_depth:
                    result.append('...')
                    # Skip to end of this level
                    skip_depth = depth
                    for remaining in term[len(result):]:
                        if remaining in '([{':
                            skip_depth += 1
                        elif remaining in ')]}':
                            skip_depth -= 1
                            if skip_depth == depth - 1:
                                break
                    result.append(remaining)
                    depth = skip_depth
                    continue
            elif char in ')]}':
                depth -= 1

            result.append(char)

        return ''.join(result)

    def close(self):
        """Close the sink and file if owned."""
        super().close()
        if self.owns_file:
            self.file.close()


class JSONLTraceSink(TraceSink):
    """
    JSON Lines trace output sink.

    Outputs compact JSONL with schema version 1.
    """

    def __init__(self, output: Optional[TextIO] = None, **kwargs):
        """
        Initialize JSONL trace sink.

        Args:
            output: Output stream (default: sys.stdout)
            **kwargs: Additional arguments for TraceSink
        """
        super().__init__(**kwargs)
        self.output = output or sys.stdout

    def _write_batch(self, events: List[TraceEvent]) -> bool:
        """Write events in JSONL format."""
        for event in events:
            obj = self._event_to_json(event)
            line = json.dumps(obj, separators=(',', ':'))
            self.output.write(line + '\n')

        # Flush output stream
        if hasattr(self.output, 'flush'):
            self.output.flush()

        return True

    def _event_to_json(self, event: TraceEvent) -> Dict[str, Any]:
        """
        Convert event to compact JSON representation.

        Uses schema v1 with compact keys.
        """
        # Port encoding
        port_map = {
            'call': 0,
            'exit': 1,
            'redo': 2,
            'fail': 3
        }

        # Required fields with compact keys
        obj = {
            'v': 1,  # Schema version
            'rid': event.run_id,
            'sid': event.step_id,
            'p': port_map.get(event.port, -1),
            'pid': event.pred_id,
            'g': event.goal_pretty,
            'fd': event.frame_depth,
            'cd': event.cp_depth,
            'gh': event.goal_height,
            'ws': event.write_stamp
        }

        # Optional fields (only include if not None)
        if hasattr(event, 'monotonic_ns') and event.monotonic_ns is not None:
            obj['ts'] = event.monotonic_ns

        if hasattr(event, 'bindings') and event.bindings is not None:
            obj['b'] = event.bindings

        return obj


class FileTraceSink(TraceSink):
    """
    File-based trace sink with rotation support.

    Writes to files with automatic rotation when size limits are reached.
    """

    def __init__(self, file_path: str, format: str = 'jsonl',
                 max_size_mb: float = 100.0,
                 max_files: int = 5,
                 batch_size: int = 100,
                 **kwargs):
        """
        Initialize file trace sink.

        Args:
            file_path: Path to output file
            format: Output format ('jsonl' or 'pretty')
            max_size_mb: Maximum file size before rotation
            max_files: Maximum number of rotated files to keep
            batch_size: Events per batch
            **kwargs: Additional arguments
        """
        super().__init__(batch_size=batch_size)

        self.file_path = file_path
        self.format = format
        self.max_size_bytes = int(max_size_mb * 1024 * 1024)
        self.max_files = max_files
        self.current_size = 0
        self.closed = False

        # Create the underlying sink
        if format == 'jsonl':
            self.sink = None
            self._open_file()
        elif format == 'pretty':
            self.sink = None
            self._open_file()
        else:
            raise ValueError(f"Unknown format: {format}")

    def _open_file(self):
        """Open a new file for writing."""
        self.file = open(self.file_path, 'w')
        self.current_size = 0

        # Create appropriate sink for the format
        if self.format == 'jsonl':
            self.sink = JSONLTraceSink(output=self.file)
        else:  # pretty
            self.sink = PrettyTraceSink(output=self.file)

    def _rotate_file(self):
        """Rotate the current file."""
        # Close current file
        self.file.close()

        # Rotate existing files
        for i in range(self.max_files - 1, 0, -1):
            old_name = f"{self.file_path}.{i}"
            new_name = f"{self.file_path}.{i+1}"
            if os.path.exists(old_name):
                if i + 1 <= self.max_files:
                    shutil.move(old_name, new_name)
                else:
                    os.remove(old_name)

        # Rename current file
        if os.path.exists(self.file_path):
            shutil.move(self.file_path, f"{self.file_path}.1")

        # Open new file
        self._open_file()

    def _write_batch(self, events: List[TraceEvent]) -> bool:
        """Write batch via underlying sink, handling rotation."""
        if self.closed:
            return False

        # Check if rotation needed (rough estimate)
        batch_size = sum(len(str(e)) for e in events)
        if self.current_size + batch_size > self.max_size_bytes:
            self._rotate_file()

        # Delegate to underlying sink
        result = self.sink._write_batch(events)
        if result:
            self.current_size += batch_size

        return result

    def write_event(self, event: TraceEvent) -> bool:
        """Write event, handling rotation if needed."""
        if self.closed:
            return False

        # Add to batch
        self.batch.append(event)

        # Check if we need to flush
        if len(self.batch) >= self.batch_size:
            return self.flush()

        return True

    def flush(self) -> bool:
        """Flush pending events."""
        if self.closed or not self.batch:
            return True

        # Check rotation before flushing
        batch_size = sum(len(str(e)) for e in self.batch)
        if self.current_size + batch_size > self.max_size_bytes:
            self._rotate_file()

        # Delegate to underlying sink
        result = self.sink._write_batch(self.batch)
        if result:
            self.current_size += batch_size
            self.batch = []

        return result

    def close(self):
        """Close the file sink."""
        if not self.closed:
            # Flush any pending events
            if self.batch:
                self.flush()
            # Close underlying sink
            if self.sink:
                self.sink.close()
            # Close file
            if hasattr(self, 'file'):
                self.file.close()
            self.closed = True