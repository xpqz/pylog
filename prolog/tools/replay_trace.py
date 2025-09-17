#!/usr/bin/env python3
"""Command-line tool for replaying and validating trace files."""

import sys
import json
import argparse
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from prolog.debug.replay import ReplayTool, InvalidTraceError


def main():
    """Main entry point for replay tool."""
    parser = argparse.ArgumentParser(
        description="Replay and validate Prolog trace files"
    )
    parser.add_argument("trace_file", help="Path to JSONL trace file")
    parser.add_argument("--last", type=int, help="Show only last N events")
    parser.add_argument(
        "--check-invariants",
        action="store_true",
        help="Check trace invariants"
    )
    parser.add_argument(
        "--format",
        choices=["text", "json"],
        default="text",
        help="Output format"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Verbose output"
    )

    args = parser.parse_args()

    trace_path = Path(args.trace_file)
    if not trace_path.exists():
        print(f"Error: File not found: {trace_path}", file=sys.stderr)
        sys.exit(1)

    try:
        replay = ReplayTool(trace_path)

        # Load events
        events = replay.load_events()

        if args.last:
            events = replay.reconstruct_last_n(args.last)

        if args.format == "json":
            output = {
                "events": events,
                "total": len(events)
            }
            print(json.dumps(output))
        else:
            print(f"Loaded {len(events)} events")

            if args.verbose:
                for event in events[:10]:  # Show first 10 in verbose mode
                    print(f"  sid={event.get('sid')} p={event.get('p')} pid={event.get('pid')}")
                if len(events) > 10:
                    print(f"  ... and {len(events) - 10} more")

        # Check invariants if requested
        if args.check_invariants:
            violations = replay.check_invariants()
            if violations:
                print(f"\nFound {len(violations)} invariant violations:")
                for v in violations:
                    print(f"  - {v}")
                sys.exit(1)
            else:
                print("\nNo invariant violations detected")

        # Always validate consistency
        try:
            replay.validate_consistency()
        except InvalidTraceError as e:
            print(f"\nConsistency error: {e}")
            sys.exit(1)

    except InvalidTraceError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()