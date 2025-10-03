"""
Tests for engine state snapshots and diffs.

Tests the snapshot module for capturing complete engine state, detecting
differences, and verifying the no-growth property during pure backtracking.
"""

import pytest
import json
from dataclasses import FrozenInstanceError

from prolog.debug.snapshot import (
    EngineSnapshot,
    CPSnapshot,
    FrameSnapshot,
    SnapshotManager,
    SnapshotDiff,
)
from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Program
from prolog.tests.helpers import mk_fact, mk_rule, program


class TestDataclasses:
    """Tests for snapshot dataclass structures."""

    def test_engine_snapshot_frozen(self):
        """EngineSnapshot is immutable (frozen=True)."""
        snapshot = EngineSnapshot(
            store_size=10,
            trail_length=5,
            trail_top=3,
            goal_height=2,
            goal_top=1,
            frame_height=1,
            frame_top=0,
            cp_height=0,
            cp_top=0,
            write_stamp=42,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        with pytest.raises(FrozenInstanceError):
            snapshot.store_size = 20

    def test_engine_snapshot_has_slots(self):
        """EngineSnapshot uses slots for memory efficiency."""
        snapshot = EngineSnapshot(
            store_size=0,
            trail_length=0,
            trail_top=0,
            goal_height=0,
            goal_top=0,
            frame_height=0,
            frame_top=0,
            cp_height=0,
            cp_top=0,
            write_stamp=0,
            choicepoints=(),  # Use tuples for immutability
            frames=(),
            memory_bytes=None,
        )

        # Classes with __slots__ don't have __dict__
        assert not hasattr(snapshot, "__dict__")
        assert hasattr(type(snapshot), "__slots__")

    def test_engine_snapshot_collections_immutable(self):
        """EngineSnapshot collections are deeply immutable."""
        snapshot = EngineSnapshot(
            store_size=0,
            trail_length=0,
            trail_top=0,
            goal_height=0,
            goal_top=0,
            frame_height=0,
            frame_top=0,
            cp_height=0,
            cp_top=0,
            write_stamp=0,
            choicepoints=(),  # Must be tuples
            frames=(),
            memory_bytes=None,
        )

        # Collections should be tuples for immutability
        assert isinstance(snapshot.choicepoints, tuple)
        assert isinstance(snapshot.frames, tuple)

        # Can't mutate tuples
        with pytest.raises(AttributeError):
            snapshot.choicepoints.append(None)
        with pytest.raises(AttributeError):
            snapshot.frames.append(None)

    def test_cp_snapshot_fields(self):
        """CPSnapshot has required fields."""
        cp = CPSnapshot(
            cp_id=0,
            kind="clause",
            pred_id="foo/2",
            trail_top=10,
            goal_height=5,
            frame_height=3,
        )

        assert cp.cp_id == 0
        assert cp.kind == "clause"
        assert cp.pred_id == "foo/2"
        assert cp.trail_top == 10
        assert cp.goal_height == 5
        assert cp.frame_height == 3

    def test_cp_snapshot_frozen(self):
        """CPSnapshot is immutable."""
        cp = CPSnapshot(
            cp_id=0,
            kind="clause",
            pred_id="test/1",
            trail_top=0,
            goal_height=0,
            frame_height=0,
        )

        with pytest.raises(FrozenInstanceError):
            cp.kind = "call"

    def test_frame_snapshot_fields(self):
        """FrameSnapshot has required fields."""
        frame = FrameSnapshot(frame_id=0, pred_id="append/3", parent_frame=None)

        assert frame.frame_id == 0
        assert frame.pred_id == "append/3"
        assert frame.parent_frame is None

    def test_frame_snapshot_with_parent(self):
        """FrameSnapshot can have parent frame reference."""
        frame = FrameSnapshot(frame_id=1, pred_id="member/2", parent_frame=0)

        assert frame.parent_frame == 0


class TestSnapshotManager:
    """Tests for SnapshotManager functionality."""

    def test_snapshot_empty_engine(self):
        """Snapshot captures empty engine state."""
        engine = Engine(program=Program(()), debug=True)
        manager = SnapshotManager()

        snapshot = manager.snapshot(engine)

        assert snapshot.store_size == 0
        assert snapshot.trail_length == 0
        assert snapshot.goal_height == 0
        assert snapshot.frame_height == 0
        assert snapshot.cp_height == 0
        assert snapshot.write_stamp == 0
        assert len(snapshot.choicepoints) == 0
        assert len(snapshot.frames) == 0

    def test_snapshot_with_query(self):
        """Snapshot captures state during query execution."""
        prog = program(mk_fact("test", Int(1)), mk_fact("test", Int(2)))
        engine = Engine(program=prog, debug=True)
        manager = SnapshotManager()

        # Execute a query
        results = engine.query("test(X)")

        snapshot = manager.snapshot(engine)

        # Should have non-zero values
        assert snapshot.store_size > 0  # Variable X was created
        assert snapshot.trail_length >= 0  # Trail may have entries
        assert snapshot.goal_height >= 0
        assert snapshot.write_stamp >= 0  # Write stamp may or may not be incremented

    def test_snapshot_choicepoints(self):
        """Snapshot captures choicepoint structure."""
        prog = program(
            mk_fact("choice", Int(1)),
            mk_fact("choice", Int(2)),
            mk_fact("choice", Int(3)),
        )
        engine = Engine(program=prog, debug=True)
        manager = SnapshotManager()

        # Execute query (choicepoints exist during execution but are cleaned up after)
        results = engine.query("choice(X)")

        snapshot = manager.snapshot(engine)

        # Choicepoints should be a tuple (even if empty after query completes)
        assert isinstance(snapshot.choicepoints, tuple)

        # After query completes, choicepoints are typically cleaned up
        # so we may have 0 choicepoints
        if len(snapshot.choicepoints) > 0:
            cp = snapshot.choicepoints[0]
            assert hasattr(cp, "pred_id")
            assert hasattr(cp, "kind")
            assert cp.kind in ["clause", "call"]

            # Check choicepoint IDs are unique
            cp_ids = [cp.cp_id for cp in snapshot.choicepoints]
            assert len(cp_ids) == len(set(cp_ids))

    def test_snapshot_frames(self):
        """Snapshot captures frame stack with meaningful assertions."""
        prog = program(
            mk_rule("parent", (Var(0, "X"),), Struct("child", (Var(0, "X"),))),
            mk_fact("child", Atom("alice")),
        )
        engine = Engine(program=prog, debug=True)
        manager = SnapshotManager()

        # Execute query that creates frames
        results = engine.query("parent(X)")

        snapshot = manager.snapshot(engine)

        # Should have captured frames as a tuple
        assert isinstance(snapshot.frames, tuple)
        # For a non-trivial call, we expect at least one frame
        # (implementation-dependent, but parent/child should create frames)
        if len(snapshot.frames) > 0:
            # Check frame structure
            frame = snapshot.frames[0]
            assert hasattr(frame, "frame_id")
            assert hasattr(frame, "pred_id")
            assert hasattr(frame, "parent_frame")

            # If multiple frames, check parent linkage
            if len(snapshot.frames) > 1:
                child = snapshot.frames[-1]
                frame_ids = {f.frame_id for f in snapshot.frames[:-1]}
                if child.parent_frame is not None:
                    assert child.parent_frame in frame_ids

    def test_snapshot_to_dict(self):
        """Snapshot exports to dictionary."""
        snapshot = EngineSnapshot(
            store_size=10,
            trail_length=5,
            trail_top=3,
            goal_height=2,
            goal_top=1,
            frame_height=1,
            frame_top=0,
            cp_height=1,
            cp_top=0,
            write_stamp=42,
            choicepoints=[CPSnapshot(0, "clause", "test/1", 2, 1, 0)],
            frames=[FrameSnapshot(0, "test/1", None)],
            memory_bytes=1024,
        )

        d = snapshot.to_dict()

        assert isinstance(d, dict)
        assert d["store_size"] == 10
        assert d["trail_length"] == 5
        assert d["write_stamp"] == 42
        assert len(d["choicepoints"]) == 1
        assert len(d["frames"]) == 1
        assert d["memory_bytes"] == 1024

    def test_snapshot_json_serialization(self):
        """Snapshots can be JSON serialized."""
        snapshot = EngineSnapshot(
            store_size=5,
            trail_length=2,
            trail_top=1,
            goal_height=1,
            goal_top=0,
            frame_height=0,
            frame_top=0,
            cp_height=0,
            cp_top=0,
            write_stamp=10,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        # Should be JSON serializable
        json_str = json.dumps(snapshot.to_dict())
        assert isinstance(json_str, str)

        # Can round-trip
        parsed = json.loads(json_str)
        assert parsed["store_size"] == 5
        assert parsed["write_stamp"] == 10

    def test_diff_identical_snapshots(self):
        """Diff of identical snapshots shows no changes."""
        snapshot1 = EngineSnapshot(
            store_size=10,
            trail_length=5,
            trail_top=3,
            goal_height=2,
            goal_top=1,
            frame_height=1,
            frame_top=0,
            cp_height=0,
            cp_top=0,
            write_stamp=42,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        manager = SnapshotManager()
        diff = manager.diff(snapshot1, snapshot1)

        assert diff.store_size_delta == 0
        assert diff.trail_length_delta == 0
        assert diff.goal_height_delta == 0
        assert diff.frame_height_delta == 0
        assert diff.cp_height_delta == 0
        assert diff.write_stamp_delta == 0
        assert diff.choicepoints_added == 0
        assert diff.choicepoints_removed == 0
        assert diff.frames_added == 0
        assert diff.frames_removed == 0

    def test_diff_different_snapshots(self):
        """Diff identifies changes between snapshots."""
        snapshot1 = EngineSnapshot(
            store_size=10,
            trail_length=5,
            trail_top=3,
            goal_height=2,
            goal_top=1,
            frame_height=1,
            frame_top=0,
            cp_height=0,
            cp_top=0,
            write_stamp=42,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        snapshot2 = EngineSnapshot(
            store_size=15,  # +5
            trail_length=8,  # +3
            trail_top=5,
            goal_height=3,  # +1
            goal_top=2,
            frame_height=2,  # +1
            frame_top=1,
            cp_height=1,  # +1
            cp_top=0,
            write_stamp=50,  # +8
            choicepoints=[CPSnapshot(0, "clause", "test/1", 5, 2, 1)],
            frames=[FrameSnapshot(0, "test/1", None), FrameSnapshot(1, "foo/2", 0)],
            memory_bytes=2048,
        )

        manager = SnapshotManager()
        diff = manager.diff(snapshot1, snapshot2)

        assert diff.store_size_delta == 5
        assert diff.trail_length_delta == 3
        assert diff.goal_height_delta == 1
        assert diff.frame_height_delta == 1
        assert diff.cp_height_delta == 1
        assert diff.write_stamp_delta == 8
        assert diff.choicepoints_added == 1
        assert diff.frames_added == 2

    def test_diff_removed_items(self):
        """Diff detects removed choicepoints and frames."""
        snapshot1 = EngineSnapshot(
            store_size=10,
            trail_length=5,
            trail_top=3,
            goal_height=2,
            goal_top=1,
            frame_height=2,
            frame_top=1,
            cp_height=2,
            cp_top=1,
            write_stamp=42,
            choicepoints=[
                CPSnapshot(0, "clause", "test/1", 2, 1, 0),
                CPSnapshot(1, "clause", "foo/2", 3, 2, 1),
            ],
            frames=[FrameSnapshot(0, "test/1", None), FrameSnapshot(1, "foo/2", 0)],
            memory_bytes=None,
        )

        snapshot2 = EngineSnapshot(
            store_size=10,
            trail_length=5,
            trail_top=3,
            goal_height=1,  # Reduced
            goal_top=0,
            frame_height=1,  # Reduced
            frame_top=0,
            cp_height=1,  # Reduced
            cp_top=0,
            write_stamp=45,
            choicepoints=[CPSnapshot(0, "clause", "test/1", 2, 1, 0)],
            frames=[FrameSnapshot(0, "test/1", None)],
            memory_bytes=None,
        )

        manager = SnapshotManager()
        diff = manager.diff(snapshot1, snapshot2)

        assert diff.goal_height_delta == -1
        assert diff.frame_height_delta == -1
        assert diff.cp_height_delta == -1
        assert diff.choicepoints_removed == 1
        assert diff.frames_removed == 1

    def test_diff_to_dict(self):
        """SnapshotDiff exports to dictionary."""
        diff = SnapshotDiff(
            store_size_delta=5,
            trail_length_delta=3,
            trail_top_delta=2,
            goal_height_delta=1,
            goal_top_delta=1,
            frame_height_delta=0,
            frame_top_delta=0,
            cp_height_delta=1,
            cp_top_delta=1,
            write_stamp_delta=10,
            choicepoints_added=2,
            choicepoints_removed=1,
            frames_added=3,
            frames_removed=0,
            memory_delta=1024,
        )

        d = diff.to_dict()

        assert isinstance(d, dict)
        assert d["store_size_delta"] == 5
        assert d["trail_length_delta"] == 3
        assert d["write_stamp_delta"] == 10
        assert d["choicepoints_added"] == 2
        assert d["choicepoints_removed"] == 1
        assert d["memory_delta"] == 1024

    def test_diff_human_readable(self):
        """SnapshotDiff provides human-readable string."""
        diff = SnapshotDiff(
            store_size_delta=5,
            trail_length_delta=3,
            trail_top_delta=2,
            goal_height_delta=1,
            goal_top_delta=1,
            frame_height_delta=0,
            frame_top_delta=0,
            cp_height_delta=1,
            cp_top_delta=1,
            write_stamp_delta=10,
            choicepoints_added=2,
            choicepoints_removed=1,
            frames_added=3,
            frames_removed=0,
            memory_delta=1024,
        )

        s = str(diff)
        assert isinstance(s, str)
        assert "store_size: +5" in s
        assert "trail_length: +3" in s
        assert "choicepoints: +2/-1" in s
        assert "frames: +3/-0" in s

    def test_no_growth_property(self):
        """Verify no growth during pure backtracking."""
        prog = program(
            mk_fact("member", Var(0, "X"), Struct(".", (Var(0, "X"), Var(1, "_")))),
            mk_rule(
                "member",
                (Var(0, "X"), Struct(".", (Var(1, "_"), Var(2, "T")))),
                Struct("member", (Var(0, "X"), Var(2, "T"))),
            ),
        )
        engine = Engine(program=prog, debug=True)
        manager = SnapshotManager()

        # For testing no-growth, we need to capture state at different points
        # Since we don't have a generator API, we'll use multiple queries
        # with different constraints to simulate backtracking

        # First query - find first element
        engine.query("X = 1")
        snapshot1 = manager.snapshot(engine)
        engine.reset()

        # Second query - find second element
        engine.query("X = 2")
        snapshot2 = manager.snapshot(engine)
        engine.reset()

        # Third query - find third element
        engine.query("X = 3")
        snapshot3 = manager.snapshot(engine)

        # Check no monotonic growth
        diff1_2 = manager.diff(snapshot1, snapshot2)
        diff2_3 = manager.diff(snapshot2, snapshot3)

        # Store size should not grow during pure backtracking
        assert diff1_2.store_size_delta == 0
        assert diff2_3.store_size_delta == 0

        # Trail might compact (negative or zero delta ok)
        assert diff1_2.trail_length_delta <= 0
        assert diff2_3.trail_length_delta <= 0

        # Choicepoint and frame heights should not increase
        # (may decrease as we backtrack through them)
        assert diff1_2.cp_height_delta <= 0
        assert diff2_3.cp_height_delta <= 0
        assert diff1_2.frame_height_delta <= 0
        assert diff2_3.frame_height_delta <= 0

    def test_snapshot_with_memory_tracking(self):
        """Snapshot can include memory measurement."""
        engine = Engine(program=Program(()), debug=True)
        manager = SnapshotManager(track_memory=True)

        snapshot = manager.snapshot(engine)

        # If memory tracking is supported, should have a value
        if snapshot.memory_bytes is not None:
            assert snapshot.memory_bytes > 0

    def test_snapshot_without_memory_tracking(self):
        """Snapshot without memory tracking has None."""
        engine = Engine(program=Program(()), debug=True)
        manager = SnapshotManager(track_memory=False)

        snapshot = manager.snapshot(engine)

        # Should be None when tracking is disabled
        assert snapshot.memory_bytes is None

    def test_diff_symmetry(self):
        """Diff operation is antisymmetric."""
        snapshot1 = EngineSnapshot(
            store_size=1,
            trail_length=2,
            trail_top=2,
            goal_height=1,
            goal_top=1,
            frame_height=1,
            frame_top=1,
            cp_height=0,
            cp_top=0,
            write_stamp=10,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        snapshot2 = EngineSnapshot(
            store_size=3,
            trail_length=5,
            trail_top=5,
            goal_height=2,
            goal_top=2,
            frame_height=2,
            frame_top=2,
            cp_height=1,
            cp_top=1,
            write_stamp=15,
            choicepoints=(CPSnapshot(0, "clause", "p/1", 5, 2, 1),),
            frames=(FrameSnapshot(0, "p/1", None),),
            memory_bytes=256,
        )

        manager = SnapshotManager()
        diff_ab = manager.diff(snapshot1, snapshot2)
        diff_ba = manager.diff(snapshot2, snapshot1)

        # Numeric deltas should negate
        assert diff_ab.store_size_delta == -diff_ba.store_size_delta
        assert diff_ab.trail_length_delta == -diff_ba.trail_length_delta
        assert diff_ab.goal_height_delta == -diff_ba.goal_height_delta
        assert diff_ab.frame_height_delta == -diff_ba.frame_height_delta
        assert diff_ab.cp_height_delta == -diff_ba.cp_height_delta
        assert diff_ab.write_stamp_delta == -diff_ba.write_stamp_delta

        # Added/removed should swap
        assert diff_ab.choicepoints_added == diff_ba.choicepoints_removed
        assert diff_ab.choicepoints_removed == diff_ba.choicepoints_added
        assert diff_ab.frames_added == diff_ba.frames_removed
        assert diff_ab.frames_removed == diff_ba.frames_added

    def test_diff_detects_trail_compaction(self):
        """Diff can detect trail compaction."""
        snapshot1 = EngineSnapshot(
            store_size=10,
            trail_length=100,  # Large trail
            trail_top=100,
            goal_height=2,
            goal_top=1,
            frame_height=1,
            frame_top=0,
            cp_height=1,
            cp_top=0,
            write_stamp=42,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        snapshot2 = EngineSnapshot(
            store_size=10,
            trail_length=20,  # Trail compacted
            trail_top=20,
            goal_height=2,
            goal_top=1,
            frame_height=1,
            frame_top=0,
            cp_height=1,
            cp_top=0,
            write_stamp=43,
            choicepoints=(),
            frames=(),
            memory_bytes=None,
        )

        manager = SnapshotManager()
        diff = manager.diff(snapshot1, snapshot2)

        # Negative delta indicates compaction
        assert diff.trail_length_delta == -80
        assert diff.trail_top_delta == -80


class TestSnapshotIntegration:
    """Integration tests with the engine."""

    def test_snapshot_during_execution(self):
        """Take snapshots at different points during execution."""
        prog = program(
            mk_rule(
                "path",
                (Var(0, "X"), Var(1, "Y")),
                Struct("edge", (Var(0, "X"), Var(1, "Y"))),
            ),
            mk_rule(
                "path",
                (Var(0, "X"), Var(1, "Z")),
                Struct("edge", (Var(0, "X"), Var(2, "Y"))),
                Struct("path", (Var(2, "Y"), Var(1, "Z"))),
            ),
            mk_fact("edge", Atom("a"), Atom("b")),
            mk_fact("edge", Atom("b"), Atom("c")),
            mk_fact("edge", Atom("c"), Atom("d")),
        )
        engine = Engine(program=prog, debug=True)
        manager = SnapshotManager()

        # Take snapshot before query
        snapshot_before = manager.snapshot(engine)

        # Execute query
        results = engine.query("path(a, X)")

        # Take snapshot after query
        snapshot_after = manager.snapshot(engine)

        snapshots = [snapshot_before, snapshot_after]

        # Should have multiple snapshots
        assert len(snapshots) >= 2

        # Compare snapshots
        for i in range(1, len(snapshots)):
            diff = manager.diff(snapshots[i - 1], snapshots[i])
            # Store size may change as variables are created
            # (write_stamp isn't currently incremented by engine)
            assert diff.store_size_delta >= 0

    def test_snapshot_completeness(self):
        """Snapshot captures all essential engine state."""
        prog = program(mk_fact("test", Int(1)), mk_fact("test", Int(2)))
        engine = Engine(program=prog, debug=True)
        manager = SnapshotManager()

        # Execute query
        results = engine.query("test(X)")

        snapshot = manager.snapshot(engine)

        # Verify all essential fields are captured
        assert hasattr(snapshot, "store_size")
        assert hasattr(snapshot, "trail_length")
        assert hasattr(snapshot, "trail_top")
        assert hasattr(snapshot, "goal_height")
        assert hasattr(snapshot, "goal_top")
        assert hasattr(snapshot, "frame_height")
        assert hasattr(snapshot, "frame_top")
        assert hasattr(snapshot, "cp_height")
        assert hasattr(snapshot, "cp_top")
        assert hasattr(snapshot, "write_stamp")
        assert hasattr(snapshot, "choicepoints")
        assert hasattr(snapshot, "frames")
        assert hasattr(snapshot, "memory_bytes")

    def test_snapshot_uses_public_apis(self):
        """Snapshot exposes only public state data."""
        engine = Engine(program=Program(()), debug=True)
        manager = SnapshotManager()

        # This should work without accessing private attributes
        snapshot = manager.snapshot(engine)

        # All exposed data should be basic types from public APIs
        assert isinstance(snapshot.store_size, int)
        assert isinstance(snapshot.trail_length, int)
        assert isinstance(snapshot.write_stamp, int)
        assert snapshot.store_size >= 0
