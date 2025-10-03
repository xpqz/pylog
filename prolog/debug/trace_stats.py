"""Trace statistics computation and report generation."""

import json
import statistics
from pathlib import Path
from typing import Dict, Any, List


def compute_trace_statistics(trace_file: Path) -> Dict[str, Any]:
    """
    Compute comprehensive statistics from a trace file.

    Port codes: {0: CALL, 1: EXIT, 2: REDO, 3: FAIL}
    """
    events = []
    with trace_file.open() as f:
        for line in f:
            if line.strip():
                events.append(json.loads(line))

    if not events:
        return {
            "total_events": 0,
            "unique_predicates": 0,
            "port_distribution": {},
            "depth_statistics": {},
            "predicate_frequency": [],
        }

    # Basic counts
    unique_preds = set()
    pred_counts = {}
    port_counts = {0: 0, 1: 0, 2: 0, 3: 0}
    depths = []

    for event in events:
        # Predicates
        pid = event.get("pid")
        if pid:
            unique_preds.add(pid)
            if event.get("p") == 0:  # Only count CALLs
                pred_counts[pid] = pred_counts.get(pid, 0) + 1

        # Ports
        port = event.get("p")
        if port is not None and port in port_counts:
            port_counts[port] += 1

        # Depths
        fd = event.get("fd", 0)
        if isinstance(fd, (int, float)):
            depths.append(int(fd))

    # Port distribution
    port_map = {0: "call", 1: "exit", 2: "redo", 3: "fail"}
    port_distribution = {port_map[k]: v for k, v in port_counts.items()}

    # Depth statistics
    depth_statistics = {}
    if depths:
        depth_statistics = {
            "max": max(depths),
            "min": min(depths),
            "avg": sum(depths) / len(depths),
            "median": statistics.median(depths) if depths else 0,
        }

    # Timestamps and duration
    timestamps = []
    for event in events:
        ts = event.get("ts")
        if ts is not None:
            timestamps.append(ts)

    stats = {
        "total_events": len(events),
        "unique_predicates": len(unique_preds),
        "port_distribution": port_distribution,
        "depth_statistics": depth_statistics,
        "predicate_frequency": sorted(
            pred_counts.items(), key=lambda x: x[1], reverse=True
        )[:10],
    }

    if timestamps:
        duration_s = max(timestamps) - min(timestamps)
        stats["duration_ms"] = duration_s * 1000
        if duration_s > 0:
            stats["events_per_second"] = len(events) / duration_s

    # Call graph
    stats["call_graph"] = compute_call_graph(events)

    # Backtrack points
    stats["backtrack_points"] = identify_backtrack_points(events)

    return stats


def compute_call_graph(events: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Compute call graph from events."""
    edges = set()
    edge_counts = {}
    stack = []  # Stack of (pred_id, depth)

    for event in events:
        port = event.get("p")
        pid = event.get("pid")
        fd = event.get("fd", 0)

        if port == 0 and pid:  # CALL
            if stack and fd > stack[-1][1]:
                # This is a call from the parent
                parent = stack[-1][0]
                edge = (parent, pid)
                edges.add(edge)
                edge_counts[edge] = edge_counts.get(edge, 0) + 1
            stack.append((pid, fd))

        elif port == 1 and pid:  # EXIT
            # Pop matching call from stack
            while stack and stack[-1][0] != pid:
                stack.pop()
            if stack:
                stack.pop()

    return {"edges": list(edges), "weights": edge_counts}


def identify_backtrack_points(events: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Identify backtrack points in the trace."""
    backtrack_points = []

    for i, event in enumerate(events):
        if event.get("p") == 2:  # REDO
            backtrack_points.append(
                {"sid": event.get("sid"), "pid": event.get("pid"), "position": i}
            )

    return backtrack_points


def generate_statistics_report(trace_file: Path, format: str = "text") -> str:
    """Generate a statistics report in the specified format."""
    stats = compute_trace_statistics(trace_file)

    if format == "json":
        return json.dumps(stats, indent=2)

    elif format == "csv":
        lines = ["Metric,Value"]
        lines.append(f"total_events,{stats['total_events']}")
        lines.append(f"unique_predicates,{stats['unique_predicates']}")
        for port, count in stats["port_distribution"].items():
            lines.append(f"port_{port},{count}")
        return "\n".join(lines)

    elif format == "markdown":
        lines = [
            "# Trace Statistics Report",
            "",
            "## Summary",
            "",
            "| Metric | Value |",
            "|--------|-------|",
            f"| Total Events | {stats['total_events']} |",
            f"| Unique Predicates | {stats['unique_predicates']} |",
        ]
        if "duration_ms" in stats:
            lines.append(f"| Duration (ms) | {stats['duration_ms']:.2f} |")
        return "\n".join(lines)

    elif format == "html":
        html = [
            "<!DOCTYPE html>",
            "<html><head><title>Trace Statistics</title></head>",
            "<body>",
            "<h1>Trace Statistics Report</h1>",
            "<table border='1'>",
            f"<tr><td>Total Events</td><td>{stats['total_events']}</td></tr>",
            f"<tr><td>Unique Predicates</td><td>{stats['unique_predicates']}</td></tr>",
            "</table>",
            "</body></html>",
        ]
        return "\n".join(html)

    else:  # text format
        lines = [
            "Trace Statistics Report",
            "=" * 40,
            f"Total Events: {stats['total_events']}",
            f"Unique Predicates: {stats['unique_predicates']}",
            "",
            "Port Distribution:",
        ]
        for port, count in stats["port_distribution"].items():
            lines.append(f"  {port}: {count}")

        lines.append("")
        lines.append("Hot Predicates:")
        for pred, count in stats["predicate_frequency"][:5]:
            lines.append(f"  {pred}: {count}")

        return "\n".join(lines)


def analyze_performance_metrics(trace_file: Path) -> Dict[str, Any]:
    """Analyze performance metrics from trace."""
    events = []
    with trace_file.open() as f:
        for line in f:
            if line.strip():
                events.append(json.loads(line))

    metrics = {}

    # Track CALL/EXIT pairs for timing
    call_stack = {}  # pid -> [(sid, ts), ...]
    timings = {}  # pid -> [duration_ms, ...]

    for event in events:
        port = event.get("p")
        pid = event.get("pid")
        ts = event.get("ts")

        if port == 0 and pid and ts is not None:  # CALL
            if pid not in call_stack:
                call_stack[pid] = []
            call_stack[pid].append((event.get("sid"), ts))

        elif port == 1 and pid and ts is not None:  # EXIT
            if pid in call_stack and call_stack[pid]:
                call_sid, call_ts = call_stack[pid].pop()
                duration_s = ts - call_ts
                duration_ms = duration_s * 1000

                if pid not in timings:
                    timings[pid] = []
                timings[pid].append(duration_ms)

    # Compute statistics per predicate
    if timings:
        predicate_timings = {}
        for pid, durations in timings.items():
            predicate_timings[pid] = {
                "count": len(durations),
                "avg_ms": sum(durations) / len(durations),
                "min_ms": min(durations),
                "max_ms": max(durations),
            }
        metrics["predicate_timings"] = predicate_timings

    # Memory usage
    store_sizes = []
    trail_sizes = []
    for event in events:
        if "store_size" in event:
            store_sizes.append(event["store_size"])
        if "trail_size" in event:
            trail_sizes.append(event["trail_size"])

    if store_sizes or trail_sizes:
        metrics["memory_usage"] = {}
        if store_sizes:
            metrics["memory_usage"]["max_store_size"] = max(store_sizes)
            metrics["memory_usage"]["avg_store_size"] = sum(store_sizes) / len(
                store_sizes
            )
        if trail_sizes:
            metrics["memory_usage"]["max_trail_size"] = max(trail_sizes)
            metrics["memory_usage"]["avg_trail_size"] = sum(trail_sizes) / len(
                trail_sizes
            )

    return metrics


def detect_performance_anomalies(
    trace_file: Path, threshold_stddev: float = 2.0
) -> List[Dict[str, Any]]:
    """Detect performance anomalies using statistical methods."""
    events = []
    with trace_file.open() as f:
        for line in f:
            if line.strip():
                events.append(json.loads(line))

    anomalies = []

    # Track CALL/EXIT pairs for timing
    call_stack = {}  # pid -> [ts, ...]
    all_durations = []

    for event in events:
        port = event.get("p")
        pid = event.get("pid")
        ts = event.get("ts")

        if port == 0 and pid and ts is not None:  # CALL
            if pid not in call_stack:
                call_stack[pid] = []
            call_stack[pid].append(ts)

        elif port == 1 and pid and ts is not None:  # EXIT
            if pid in call_stack and call_stack[pid]:
                call_ts = call_stack[pid].pop()
                duration_ms = (ts - call_ts) * 1000
                all_durations.append((pid, duration_ms))

    # Group by predicate
    by_predicate = {}
    for pid, duration in all_durations:
        if pid not in by_predicate:
            by_predicate[pid] = []
        by_predicate[pid].append(duration)

    # Find anomalies per predicate
    for pid, durations in by_predicate.items():
        if len(durations) >= 2:  # Need at least 2 for meaningful comparison
            mean = statistics.mean(durations)

            # For small samples, use median-based approach
            if len(durations) <= 3:
                median = statistics.median(durations)
                # Consider anomaly if > 10x median or > 3x mean
                for d in durations:
                    if median > 0 and d / median > 10:
                        anomalies.append(
                            {
                                "pid": pid,
                                "duration_ms": d,
                                "mean_ms": mean,
                                "stddev_ms": 0,
                                "zscore": 0,
                            }
                        )
                    elif mean > 0 and d / mean > threshold_stddev:
                        anomalies.append(
                            {
                                "pid": pid,
                                "duration_ms": d,
                                "mean_ms": mean,
                                "stddev_ms": 0,
                                "zscore": d / mean,
                            }
                        )
            else:
                # For larger samples, use standard deviation
                stdev = statistics.stdev(durations)
                if stdev > 0:
                    for d in durations:
                        zscore = (d - mean) / stdev
                        if abs(zscore) > threshold_stddev:
                            anomalies.append(
                                {
                                    "pid": pid,
                                    "duration_ms": d,
                                    "mean_ms": mean,
                                    "stddev_ms": stdev,
                                    "zscore": zscore,
                                }
                            )

    return anomalies
