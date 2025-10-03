"""
Engine state snapshots and diffs for debugging and regression detection.

Provides complete engine state capture with comparison capabilities
for detecting changes and verifying the no-growth property during
pure backtracking.
"""

from dataclasses import dataclass
from typing import Optional, Tuple, Any, Dict
import sys


@dataclass(frozen=True, slots=True)
class CPSnapshot:
    """
    Snapshot of a single choicepoint.

    Immutable dataclass with slots for memory efficiency.
    Note: cp_id may be snapshot-relative if engine doesn't provide stable IDs.
    """

    cp_id: int
    kind: str  # "clause" or "call"
    pred_id: str
    trail_top: int
    goal_height: int
    frame_height: int

    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary."""
        return {
            "cp_id": self.cp_id,
            "kind": self.kind,
            "pred_id": self.pred_id,
            "trail_top": self.trail_top,
            "goal_height": self.goal_height,
            "frame_height": self.frame_height,
        }


@dataclass(frozen=True, slots=True)
class FrameSnapshot:
    """
    Snapshot of a single frame.

    Immutable dataclass with slots for memory efficiency.
    Note: frame_id may be snapshot-relative if engine doesn't provide stable IDs.
    """

    frame_id: int
    pred_id: str
    parent_frame: Optional[int]

    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary."""
        return {
            "frame_id": self.frame_id,
            "pred_id": self.pred_id,
            "parent_frame": self.parent_frame,
        }


@dataclass(frozen=True, slots=True)
class EngineSnapshot:
    """
    Complete snapshot of engine state.

    Immutable dataclass with slots for memory efficiency.
    Collections are stored as tuples for deep immutability.
    """

    store_size: int
    trail_length: int
    trail_top: int
    goal_height: int
    goal_top: int
    frame_height: int
    frame_top: int
    cp_height: int
    cp_top: int
    write_stamp: int
    choicepoints: Tuple[CPSnapshot, ...]
    frames: Tuple[FrameSnapshot, ...]
    memory_bytes: Optional[int]

    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary for JSON serialization."""
        return {
            "store_size": self.store_size,
            "trail_length": self.trail_length,
            "trail_top": self.trail_top,
            "goal_height": self.goal_height,
            "goal_top": self.goal_top,
            "frame_height": self.frame_height,
            "frame_top": self.frame_top,
            "cp_height": self.cp_height,
            "cp_top": self.cp_top,
            "write_stamp": self.write_stamp,
            "choicepoints": [cp.to_dict() for cp in self.choicepoints],
            "frames": [f.to_dict() for f in self.frames],
            "memory_bytes": self.memory_bytes,
        }


@dataclass(frozen=True, slots=True)
class SnapshotDiff:
    """
    Differences between two engine snapshots.

    Positive deltas indicate growth, negative indicate reduction.
    Note: add/remove counts are cardinality deltas (length differences),
    not identity-based tracking of individual items.
    """

    store_size_delta: int
    trail_length_delta: int
    trail_top_delta: int
    goal_height_delta: int
    goal_top_delta: int
    frame_height_delta: int
    frame_top_delta: int
    cp_height_delta: int
    cp_top_delta: int
    write_stamp_delta: int
    choicepoints_added: int
    choicepoints_removed: int
    frames_added: int
    frames_removed: int
    memory_delta: Optional[int]

    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary."""
        return {
            "store_size_delta": self.store_size_delta,
            "trail_length_delta": self.trail_length_delta,
            "trail_top_delta": self.trail_top_delta,
            "goal_height_delta": self.goal_height_delta,
            "goal_top_delta": self.goal_top_delta,
            "frame_height_delta": self.frame_height_delta,
            "frame_top_delta": self.frame_top_delta,
            "cp_height_delta": self.cp_height_delta,
            "cp_top_delta": self.cp_top_delta,
            "write_stamp_delta": self.write_stamp_delta,
            "choicepoints_added": self.choicepoints_added,
            "choicepoints_removed": self.choicepoints_removed,
            "frames_added": self.frames_added,
            "frames_removed": self.frames_removed,
            "memory_delta": self.memory_delta,
        }

    def __str__(self) -> str:
        """Human-readable diff summary."""
        lines = []

        # Format deltas with sign
        def fmt_delta(name: str, value: int) -> str:
            if value > 0:
                return f"{name}: +{value}"
            elif value < 0:
                return f"{name}: {value}"
            else:
                return f"{name}: 0"

        lines.append(fmt_delta("store_size", self.store_size_delta))
        lines.append(fmt_delta("trail_length", self.trail_length_delta))
        lines.append(fmt_delta("goal_height", self.goal_height_delta))
        lines.append(fmt_delta("frame_height", self.frame_height_delta))
        lines.append(fmt_delta("cp_height", self.cp_height_delta))
        lines.append(fmt_delta("write_stamp", self.write_stamp_delta))

        # Format add/remove counts
        lines.append(
            f"choicepoints: +{self.choicepoints_added}/-{self.choicepoints_removed}"
        )
        lines.append(f"frames: +{self.frames_added}/-{self.frames_removed}")

        if self.memory_delta is not None:
            lines.append(fmt_delta("memory", self.memory_delta))

        return "\n".join(lines)


class SnapshotManager:
    """
    Manages engine state snapshots and comparisons.

    Uses only public engine APIs to maintain encapsulation.
    """

    def __init__(self, track_memory: bool = False):
        """
        Initialize snapshot manager.

        Args:
            track_memory: Whether to include memory measurements
        """
        self.track_memory = track_memory

    def _measure_memory(self, engine: Any) -> Optional[int]:
        """
        Measure memory usage of the engine.

        This is a best-effort probe that may inspect private fields when present.
        Uses sys.getsizeof for a shallow size estimate. Note that this
        is a heuristic - actual memory usage varies by Python implementation
        and container types.

        Args:
            engine: The engine to measure

        Returns:
            Memory usage in bytes or None if not available
        """
        try:
            # Base size of engine object
            size = sys.getsizeof(engine)

            # Add sizes from internal structures if available
            # This is best-effort and may access internals
            if hasattr(engine, "store") and hasattr(engine.store, "cells"):
                size += sys.getsizeof(engine.store.cells)
            if hasattr(engine, "trail"):
                size += sys.getsizeof(engine.trail)
            if hasattr(engine, "goal_stack"):
                size += sys.getsizeof(engine.goal_stack)
            if hasattr(engine, "frame_stack"):
                size += sys.getsizeof(engine.frame_stack)
            if hasattr(engine, "cp_stack"):
                size += sys.getsizeof(engine.cp_stack)

            return size if size > 0 else None
        except Exception:
            return None

    def snapshot(self, engine: Any) -> EngineSnapshot:
        """
        Capture complete engine state.

        Uses only public APIs to access engine state.

        Args:
            engine: The engine to snapshot

        Returns:
            Complete engine state snapshot
        """
        # Access state through public APIs only
        store_size = engine.store_size() if hasattr(engine, "store_size") else 0
        trail_length = engine.trail_length() if hasattr(engine, "trail_length") else 0
        trail_top = (
            engine.trail_top_value() if hasattr(engine, "trail_top_value") else 0
        )
        goal_height = engine.goal_height() if hasattr(engine, "goal_height") else 0
        goal_top = engine.goal_top_value() if hasattr(engine, "goal_top_value") else 0
        frame_height = engine.frame_height() if hasattr(engine, "frame_height") else 0
        frame_top = (
            engine.frame_top_value() if hasattr(engine, "frame_top_value") else 0
        )
        cp_height = (
            engine.choicepoint_height() if hasattr(engine, "choicepoint_height") else 0
        )
        cp_top = engine.choicepoint_top() if hasattr(engine, "choicepoint_top") else 0
        write_stamp = (
            engine.write_stamp_value() if hasattr(engine, "write_stamp_value") else 0
        )

        # Capture choicepoints using public API
        choicepoints = []
        if hasattr(engine, "choicepoints"):
            for i, cp in enumerate(engine.choicepoints()):
                # Prefer stable ID if available, otherwise use enumeration
                cp_id = getattr(cp, "id", None)
                if cp_id is None:
                    cp_id = i  # Snapshot-relative ID

                cp_snapshot = CPSnapshot(
                    cp_id=cp_id,
                    kind=cp.kind if hasattr(cp, "kind") else "clause",
                    pred_id=cp.pred_id if hasattr(cp, "pred_id") else "unknown",
                    trail_top=cp.trail_top if hasattr(cp, "trail_top") else 0,
                    goal_height=cp.goal_top if hasattr(cp, "goal_top") else 0,
                    frame_height=cp.frame_top if hasattr(cp, "frame_top") else 0,
                )
                choicepoints.append(cp_snapshot)

        # Capture frames using public API
        frames = []
        if hasattr(engine, "frames"):
            for i, frame in enumerate(engine.frames()):
                # Prefer stable ID if available, otherwise use enumeration
                frame_id = getattr(frame, "id", None)
                if frame_id is None:
                    frame_id = i  # Snapshot-relative ID

                frame_snapshot = FrameSnapshot(
                    frame_id=frame_id,
                    pred_id=frame.pred_id if hasattr(frame, "pred_id") else "unknown",
                    parent_frame=(
                        frame.parent_frame if hasattr(frame, "parent_frame") else None
                    ),
                )
                frames.append(frame_snapshot)

        # Optional memory tracking
        memory_bytes = self._measure_memory(engine) if self.track_memory else None

        # Convert lists to tuples for deep immutability
        return EngineSnapshot(
            store_size=store_size,
            trail_length=trail_length,
            trail_top=trail_top,
            goal_height=goal_height,
            goal_top=goal_top,
            frame_height=frame_height,
            frame_top=frame_top,
            cp_height=cp_height,
            cp_top=cp_top,
            write_stamp=write_stamp,
            choicepoints=tuple(choicepoints),  # Ensure tuple for immutability
            frames=tuple(frames),  # Ensure tuple for immutability
            memory_bytes=memory_bytes,
        )

    def diff(
        self, snapshot1: EngineSnapshot, snapshot2: EngineSnapshot
    ) -> SnapshotDiff:
        """
        Compute differences between two snapshots.

        Args:
            snapshot1: First snapshot (baseline)
            snapshot2: Second snapshot (comparison)

        Returns:
            Structured diff showing all changes
        """
        # Compute numeric deltas
        store_size_delta = snapshot2.store_size - snapshot1.store_size
        trail_length_delta = snapshot2.trail_length - snapshot1.trail_length
        trail_top_delta = snapshot2.trail_top - snapshot1.trail_top
        goal_height_delta = snapshot2.goal_height - snapshot1.goal_height
        goal_top_delta = snapshot2.goal_top - snapshot1.goal_top
        frame_height_delta = snapshot2.frame_height - snapshot1.frame_height
        frame_top_delta = snapshot2.frame_top - snapshot1.frame_top
        cp_height_delta = snapshot2.cp_height - snapshot1.cp_height
        cp_top_delta = snapshot2.cp_top - snapshot1.cp_top
        write_stamp_delta = snapshot2.write_stamp - snapshot1.write_stamp

        # Count choicepoint changes
        # Simple counting - could be enhanced to track modifications
        choicepoints_added = max(
            0, len(snapshot2.choicepoints) - len(snapshot1.choicepoints)
        )
        choicepoints_removed = max(
            0, len(snapshot1.choicepoints) - len(snapshot2.choicepoints)
        )

        # Count frame changes
        frames_added = max(0, len(snapshot2.frames) - len(snapshot1.frames))
        frames_removed = max(0, len(snapshot1.frames) - len(snapshot2.frames))

        # Memory delta
        memory_delta = None
        if snapshot1.memory_bytes is not None and snapshot2.memory_bytes is not None:
            memory_delta = snapshot2.memory_bytes - snapshot1.memory_bytes

        return SnapshotDiff(
            store_size_delta=store_size_delta,
            trail_length_delta=trail_length_delta,
            trail_top_delta=trail_top_delta,
            goal_height_delta=goal_height_delta,
            goal_top_delta=goal_top_delta,
            frame_height_delta=frame_height_delta,
            frame_top_delta=frame_top_delta,
            cp_height_delta=cp_height_delta,
            cp_top_delta=cp_top_delta,
            write_stamp_delta=write_stamp_delta,
            choicepoints_added=choicepoints_added,
            choicepoints_removed=choicepoints_removed,
            frames_added=frames_added,
            frames_removed=frames_removed,
            memory_delta=memory_delta,
        )
