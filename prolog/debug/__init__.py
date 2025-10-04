"""
Debug and observability infrastructure for PyLog.

This module provides comprehensive debugging, tracing, and observability
capabilities for understanding and reproducing Prolog execution behavior.
"""

from .tracer import TraceEvent, PortsTracer
from .sinks import PrettyTraceSink
from .metrics import PredMetrics, EngineMetrics
from .snapshot import (
    EngineSnapshot,
    CPSnapshot,
    FrameSnapshot,
    SnapshotManager,
    SnapshotDiff,
)

__all__ = [
    "TraceEvent",
    "PortsTracer",
    "PrettyTraceSink",
    "PredMetrics",
    "EngineMetrics",
    "EngineSnapshot",
    "CPSnapshot",
    "FrameSnapshot",
    "SnapshotManager",
    "SnapshotDiff",
]
