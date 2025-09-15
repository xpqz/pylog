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
            "frame_height": self.frame_height
        }


@dataclass(frozen=True, slots=True)
class FrameSnapshot:
    """
    Snapshot of a single frame.

    Immutable dataclass with slots for memory efficiency.
    """
    frame_id: int
    pred_id: str
    parent_frame: Optional[int]

    def to_dict(self) -> Dict[str, Any]:
        """Export to dictionary."""
        return {
            "frame_id": self.frame_id,
            "pred_id": self.pred_id,
            "parent_frame": self.parent_frame
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
            "memory_bytes": self.memory_bytes
        }


@dataclass(frozen=True, slots=True)
class SnapshotDiff:
    """
    Differences between two engine snapshots.

    Positive deltas indicate growth, negative indicate reduction.
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
            "memory_delta": self.memory_delta
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
        lines.append(f"choicepoints: +{self.choicepoints_added}/-{self.choicepoints_removed}")
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

    def snapshot(self, engine: Any) -> EngineSnapshot:
        """
        Capture complete engine state.

        Uses only public APIs to access engine state.

        Args:
            engine: The engine to snapshot

        Returns:
            Complete engine state snapshot
        """
        # Access state through public APIs
        # Using store.size() not store.cells
        store_size = len(engine.store.cells) if hasattr(engine.store, 'cells') else 0

        # Trail state
        trail_length = len(engine.trail) if hasattr(engine, 'trail') else 0
        trail_top = engine.trail_top if hasattr(engine, 'trail_top') else 0

        # Stack heights
        goal_height = len(engine.goal_stack) if hasattr(engine, 'goal_stack') else 0
        goal_top = engine.goal_top if hasattr(engine, 'goal_top') else 0

        frame_height = len(engine.frame_stack) if hasattr(engine, 'frame_stack') else 0
        frame_top = engine.frame_top if hasattr(engine, 'frame_top') else 0

        cp_height = len(engine.cp_stack) if hasattr(engine, 'cp_stack') else 0
        cp_top = engine.cp_top if hasattr(engine, 'cp_top') else 0

        # Write stamp
        write_stamp = engine.write_stamp if hasattr(engine, 'write_stamp') else 0

        # Capture choicepoints
        choicepoints = []
        if hasattr(engine, 'cp_stack') and hasattr(engine, 'cp_top'):
            for i in range(engine.cp_top):
                cp = engine.cp_stack[i]
                # Extract choicepoint details
                cp_snapshot = CPSnapshot(
                    cp_id=i,
                    kind=cp.kind if hasattr(cp, 'kind') else "clause",
                    pred_id=cp.pred_id if hasattr(cp, 'pred_id') else "unknown",
                    trail_top=cp.trail_top if hasattr(cp, 'trail_top') else 0,
                    goal_height=cp.goal_top if hasattr(cp, 'goal_top') else 0,
                    frame_height=cp.frame_top if hasattr(cp, 'frame_top') else 0
                )
                choicepoints.append(cp_snapshot)

        # Capture frames
        frames = []
        if hasattr(engine, 'frame_stack') and hasattr(engine, 'frame_top'):
            for i in range(engine.frame_top):
                frame = engine.frame_stack[i]
                # Extract frame details
                frame_snapshot = FrameSnapshot(
                    frame_id=i,
                    pred_id=frame.pred_id if hasattr(frame, 'pred_id') else "unknown",
                    parent_frame=frame.parent_frame if hasattr(frame, 'parent_frame') else None
                )
                frames.append(frame_snapshot)

        # Optional memory tracking
        memory_bytes = None
        if self.track_memory:
            # Simple memory estimate using sys.getsizeof
            # In production, could use more sophisticated memory profiling
            try:
                memory_bytes = sys.getsizeof(engine)
            except:
                memory_bytes = None

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
            choicepoints=tuple(choicepoints),  # Convert to tuple for immutability
            frames=tuple(frames),  # Convert to tuple for immutability
            memory_bytes=memory_bytes
        )

    def diff(self, snapshot1: EngineSnapshot, snapshot2: EngineSnapshot) -> SnapshotDiff:
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
        choicepoints_added = max(0, len(snapshot2.choicepoints) - len(snapshot1.choicepoints))
        choicepoints_removed = max(0, len(snapshot1.choicepoints) - len(snapshot2.choicepoints))

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
            memory_delta=memory_delta
        )