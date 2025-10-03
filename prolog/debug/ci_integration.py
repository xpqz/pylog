"""CI integration for trace capture and artifact collection."""

import os
import shutil
from pathlib import Path
from typing import Optional


def is_tracing_enabled() -> bool:
    """Check if tracing is enabled via environment variable."""
    val = os.environ.get("PYLOG_TRACE", "0").strip().lower()
    return val in {"1", "true", "yes", "on"}


def capture_failure_artifacts(
    trace_file: Path,
    output_dir: Path,
    max_trace_size_mb: float = 10,
    snapshot_file: Optional[Path] = None,
) -> None:
    """
    Capture trace and snapshot artifacts for CI failure analysis.

    Args:
        trace_file: Path to the trace file
        output_dir: Directory to write artifacts to
        max_trace_size_mb: Maximum size of trace to capture in MB
        snapshot_file: Optional snapshot file to include
    """
    if not output_dir.exists():
        output_dir.mkdir(parents=True)

    # Capture trace file (respecting size limit)
    if trace_file.exists():
        trace_size = trace_file.stat().st_size
        max_size_bytes = max_trace_size_mb * 1024 * 1024

        output_trace = output_dir / "trace.jsonl"

        if trace_size <= max_size_bytes:
            # Copy entire file
            shutil.copy2(trace_file, output_trace)
        else:
            # Copy only last max_size_bytes
            with trace_file.open("rb") as src:
                # Seek to position to read last N bytes
                src.seek(max(0, trace_size - int(max_size_bytes)))
                # Find next newline to avoid partial line
                src.readline()  # Skip partial line

                with output_trace.open("wb") as dst:
                    # Copy rest of file
                    shutil.copyfileobj(src, dst)
                    # Ensure trailing newline
                    dst.seek(0, os.SEEK_END)
                    if dst.tell() > 0:
                        dst.seek(-1, os.SEEK_END)
                        last_byte = dst.read(1)
                        if last_byte != b"\n":
                            dst.write(b"\n")

    # Capture snapshot if provided
    if snapshot_file and snapshot_file.exists():
        output_snapshot = output_dir / "snapshot.json"
        shutil.copy2(snapshot_file, output_snapshot)
