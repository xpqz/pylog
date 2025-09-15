"""
Debug and observability infrastructure for PyLog.

This module provides comprehensive debugging, tracing, and observability
capabilities for understanding and reproducing Prolog execution behavior.
"""

from .tracer import TraceEvent, PortsTracer
from .sinks import PrettyTraceSink

__all__ = [
    "TraceEvent",
    "PortsTracer",
    "PrettyTraceSink",
]