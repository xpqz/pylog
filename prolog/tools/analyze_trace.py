#!/usr/bin/env python3
"""Command-line tool for analyzing trace statistics."""

import sys
import json
import argparse
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from prolog.debug.trace_stats import (
    compute_trace_statistics,
    generate_statistics_report,
    analyze_performance_metrics,
    detect_performance_anomalies
)


def main():
    """Main entry point for analyze tool."""
    parser = argparse.ArgumentParser(
        description="Analyze Prolog trace files for statistics and patterns"
    )
    parser.add_argument("trace_file", help="Path to JSONL trace file")
    parser.add_argument("--stats", action="store_true", help="Show detailed statistics")
    parser.add_argument(
        "--format",
        choices=["text", "json", "csv", "markdown", "html"],
        default="text",
        help="Output format"
    )
    parser.add_argument(
        "--top",
        type=int,
        default=5,
        help="Number of top predicates to show"
    )
    parser.add_argument(
        "-o", "--output",
        help="Output file (default: stdout)"
    )
    parser.add_argument(
        "--performance",
        action="store_true",
        help="Analyze performance metrics"
    )
    parser.add_argument(
        "--backtrack",
        action="store_true",
        help="Analyze backtracking patterns"
    )

    args = parser.parse_args()

    trace_path = Path(args.trace_file)
    if not trace_path.exists():
        print(f"Error: File not found: {trace_path}", file=sys.stderr)
        sys.exit(1)

    try:
        # Generate report or statistics
        if args.format == "json":
            stats = compute_trace_statistics(trace_path)
            output = json.dumps(stats, indent=2)
        else:
            # Generate formatted report
            report = generate_statistics_report(trace_path, format=args.format)

            # Add performance analysis if requested
            if args.performance:
                metrics = analyze_performance_metrics(trace_path)
                if args.format == "text":
                    report += "\n\nPerformance Analysis:\n"
                    for pid, timing in metrics.get("predicate_timings", {}).items():
                        avg_ms = timing["avg_ms"]
                        if avg_ms >= 100:
                            report += f"  {pid}: {avg_ms:.1f} ms (slow)\n"
                        elif avg_ms >= 10:
                            report += f"  {pid}: {avg_ms:.1f} ms\n"
                        else:
                            report += f"  {pid}: {avg_ms:.1f} ms (fast)\n"

            # Add backtrack analysis if requested
            if args.backtrack:
                stats = compute_trace_statistics(trace_path)
                total = stats["total_events"]
                backtrack_count = sum(
                    stats["port_distribution"].get(p, 0)
                    for p in ["redo", "fail"]
                )
                if args.format == "text":
                    report += f"\n\nBacktrack Analysis:\n"
                    report += f"  Total events: {total}\n"
                    report += f"  Backtrack events: {backtrack_count}\n"
                    if total > 0:
                        rate = (backtrack_count / total) * 100
                        report += f"  Backtrack rate: {rate:.1f}% ({backtrack_count}/{total})\n"

            output = report

        # Output results
        if args.output:
            output_path = Path(args.output)
            output_path.write_text(output)
        else:
            print(output)

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()