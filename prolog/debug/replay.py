"""Trace replay tool for debugging and analysis."""

import json
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass


class InvalidTraceError(Exception):
    """Raised when trace file is invalid or corrupted."""
    pass


class InvariantViolationError(Exception):
    """Raised when trace invariants are violated."""
    pass


@dataclass
class ReplayTool:
    """Tool for replaying and validating trace files."""

    trace_file: Path

    def load_events(self) -> List[Dict[str, Any]]:
        """Load events from JSONL trace file."""
        events = []
        if not self.trace_file.exists():
            return events

        try:
            with self.trace_file.open() as f:
                for line in f:
                    if line.strip():
                        try:
                            events.append(json.loads(line))
                        except json.JSONDecodeError as e:
                            raise InvalidTraceError(f"Malformed JSON in trace file: {e}")
        except Exception as e:
            if isinstance(e, InvalidTraceError):
                raise
            raise InvalidTraceError(f"Error reading trace file: {e}")

        return events

    def reconstruct_last_n(self, n: int) -> List[Dict[str, Any]]:
        """Reconstruct the last N steps from trace."""
        if n <= 0:
            return []

        events = self.load_events()
        if n >= len(events):
            return events
        return events[-n:]

    def validate_consistency(self) -> None:
        """Validate trace consistency (monotonic step_id, etc)."""
        events = self.load_events()
        if not events:
            return

        prev_sid = 0
        for event in events:
            sid = event.get("sid", 0)

            # Check monotonic step_id
            if sid <= prev_sid:
                raise InvalidTraceError(
                    f"Step_id not monotonic: {sid} after {prev_sid}"
                )
            prev_sid = sid

    def check_invariants(self) -> List[str]:
        """Check trace invariants and return violations."""
        from prolog.debug.invariant_checker import check_trace_invariants
        events = self.load_events()
        return check_trace_invariants(events)

    def check_step_sequence(self) -> List[Dict[str, Any]]:
        """Check for missing steps in sequence."""
        events = self.load_events()
        violations = []

        if not events:
            return violations

        prev_sid = 0
        for event in events:
            sid = event.get("sid", 0)
            if sid > prev_sid + 1:
                # Missing step(s)
                for missing in range(prev_sid + 1, sid):
                    violations.append({
                        "missing_sid": missing,
                        "after_sid": prev_sid,
                        "before_sid": sid
                    })
            prev_sid = sid

        return violations


@dataclass
class TraceAnalyzer:
    """Analyzer for trace statistics and patterns."""

    trace_file: Path

    def parse(self) -> List[Dict[str, Any]]:
        """Parse JSONL trace file."""
        events = []
        with self.trace_file.open() as f:
            for line in f:
                if line.strip():
                    events.append(json.loads(line))
        return events

    def compute_port_counts(self) -> Dict[str, int]:
        """Compute counts for each port type."""
        port_map = {0: "call", 1: "exit", 2: "redo", 3: "fail"}
        counts = {"call": 0, "exit": 0, "redo": 0, "fail": 0}

        events = self.parse()
        for event in events:
            port = event.get("p")
            if port is not None and port in port_map:
                counts[port_map[port]] += 1

        return counts

    def compute_depth_distribution(self) -> Dict[str, Any]:
        """Compute frame depth distribution statistics."""
        events = self.parse()
        if not events:
            return {
                "max_depth": 0,
                "avg_depth": 0,
                "depth_counts": {}
            }

        depths = []
        depth_counts = {}

        for event in events:
            fd = event.get("fd", 0)
            if isinstance(fd, (int, float)):
                fd = int(fd)
                depths.append(fd)
                depth_counts[fd] = depth_counts.get(fd, 0) + 1

        if not depths:
            return {
                "max_depth": 0,
                "avg_depth": 0,
                "depth_counts": {}
            }

        return {
            "max_depth": max(depths),
            "avg_depth": sum(depths) / len(depths),
            "depth_counts": depth_counts
        }

    def identify_hot_predicates(self, top_n: int = 10) -> List[tuple]:
        """Identify most frequently called predicates."""
        events = self.parse()
        pred_counts = {}

        for event in events:
            # Only count CALL events (p=0)
            if event.get("p") == 0:
                pid = event.get("pid")
                if pid:
                    pred_counts[pid] = pred_counts.get(pid, 0) + 1

        # Sort by count descending
        sorted_preds = sorted(pred_counts.items(), key=lambda x: x[1], reverse=True)
        return sorted_preds[:top_n]

    def measure_backtrack_frequency(self) -> Dict[str, Any]:
        """Measure frequency of backtracking (FAIL and REDO events)."""
        events = self.parse()
        total = len(events)

        if total == 0:
            return {
                "total_events": 0,
                "backtrack_events": 0,
                "backtrack_rate": 0.0
            }

        # REDO (p=2) and FAIL (p=3) are backtrack events
        backtrack_count = sum(1 for e in events if e.get("p") in [2, 3])

        return {
            "total_events": total,
            "backtrack_events": backtrack_count,
            "backtrack_rate": backtrack_count / total
        }

    def generate_summary_report(self) -> Dict[str, Any]:
        """Generate comprehensive summary report."""
        events = self.parse()

        report = {
            "total_events": len(events),
            "port_counts": self.compute_port_counts(),
            "depth_distribution": self.compute_depth_distribution(),
            "hot_predicates": self.identify_hot_predicates(top_n=5),
            "backtrack_frequency": self.measure_backtrack_frequency()
        }

        # Add duration if timestamps available
        timestamps = []
        for event in events:
            ts = event.get("ts")
            if ts is not None:
                timestamps.append(ts)

        if timestamps:
            duration_s = max(timestamps) - min(timestamps)
            report["duration_ms"] = duration_s * 1000
        else:
            report["duration_estimate"] = None

        return report


class TraceStatistics:
    """Statistical analysis of trace events."""

    def __init__(self, events: List[Dict[str, Any]]):
        self.events = events

    def compute_basic_metrics(self) -> Dict[str, Any]:
        """Compute basic statistical metrics."""
        if not self.events:
            return {
                "event_count": 0,
                "max_frame_depth": 0,
                "max_choice_depth": 0,
                "avg_frame_depth": 0
            }

        frame_depths = []
        choice_depths = []

        for event in self.events:
            fd = event.get("fd", 0)
            cd = event.get("cd", 0)

            if isinstance(fd, (int, float)):
                frame_depths.append(fd)
            if isinstance(cd, (int, float)):
                choice_depths.append(cd)

        return {
            "event_count": len(self.events),
            "max_frame_depth": max(frame_depths) if frame_depths else 0,
            "max_choice_depth": max(choice_depths) if choice_depths else 0,
            "avg_frame_depth": sum(frame_depths) / len(frame_depths) if frame_depths else 0
        }

    def compute_depth_percentiles(self, percentiles: List[int]) -> Dict[str, int]:
        """Compute depth percentiles."""
        if not self.events:
            return {f"p{p}": 0 for p in percentiles}

        depths = []
        for event in self.events:
            fd = event.get("fd", 0)
            if isinstance(fd, (int, float)):
                depths.append(int(fd))

        if not depths:
            return {f"p{p}": 0 for p in percentiles}

        depths.sort()
        result = {}

        for p in percentiles:
            # Nearest-rank method
            idx = int((p / 100.0) * len(depths))
            if idx > 0:
                idx -= 1  # Convert to 0-based index
            if idx >= len(depths):
                idx = len(depths) - 1
            result[f"p{p}"] = depths[idx]

        return result