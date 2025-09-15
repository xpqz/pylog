"""
Output sinks for trace events.

Provides various output formats including human-readable pretty format
and machine-parseable JSONL format.
"""

from abc import ABC, abstractmethod
from typing import Optional
import sys

from prolog.debug.tracer import TraceEvent


class TraceSink(ABC):
    """Abstract base for trace output destinations."""

    @abstractmethod
    def write_event(self, event: TraceEvent):
        """Write an event to this sink."""
        pass


class PrettyTraceSink(TraceSink):
    """
    Human-readable single-line trace output.

    Format: [step_id] port(goal_height): goal @ frame=N cp=M
    """

    def __init__(self, output=None):
        """
        Initialize pretty sink.

        Args:
            output: Output stream (default: sys.stdout)
        """
        self.output = output or sys.stdout
        self.max_goal_length = 50  # Default max goal length

    def format_event(self, event: TraceEvent) -> str:
        """
        Format event as human-readable single line.

        Format: [123] call(5): append([1,2], [3], X) @ frame=2 cp=1
        """
        # Truncate goal if needed
        goal_str = event.goal_pretty
        if len(goal_str) > self.max_goal_length:
            goal_str = goal_str[:self.max_goal_length - 3] + "..."

        # Format: [step_id] port(goal_height): goal @ frame=N cp=M
        return (
            f"[{event.step_id}] "
            f"{event.port}({event.goal_height}): "
            f"{goal_str} @ "
            f"frame={event.frame_depth} cp={event.cp_depth}"
        )

    def write_event(self, event: TraceEvent):
        """Write formatted event to output."""
        line = self.format_event(event)
        print(line, file=self.output)