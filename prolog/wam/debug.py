"""Debug support for WAM: snapshots, pretty-printing, and tracing.

Provides observability into WAM execution state for testing and debugging.
"""

from __future__ import annotations

from typing import Protocol

from prolog.wam.machine import Machine


class TraceSink(Protocol):
    """Protocol for trace event consumers.

    Implementations can log, record, or visualize WAM execution events.
    """

    def on_step(self, machine: "Machine", instruction: tuple) -> None:
        """Called before executing each instruction.

        Args:
            machine: Current machine state
            instruction: Instruction tuple being executed
        """
        ...

    def on_call(self, predicate: str) -> None:
        """Called when entering a predicate (future phases).

        Args:
            predicate: Predicate name/arity string
        """
        ...

    def on_exit(self, predicate: str) -> None:
        """Called when exiting a predicate successfully (future phases).

        Args:
            predicate: Predicate name/arity string
        """
        ...

    def on_fail(self, predicate: str) -> None:
        """Called when predicate fails (future phases).

        Args:
            predicate: Predicate name/arity string
        """
        ...


def snapshot(machine: "Machine") -> dict:
    """Capture current machine state as JSON-serializable dict.

    Returns:
        Dictionary with registers, heap, stacks, and trail state.
        All values are JSON-serializable (int, str, list, dict, None).
    """
    return {
        "regs": {
            "P": machine.P,
            "H": machine.H,
            "B": machine.B,
            "E": machine.E,
            "CP": machine.CP,
            "TR": machine.TR,
            "HB": machine.HB,
        },
        "X": list(machine.X),
        "heap": list(machine.heap),
        "frames": list(machine.env_stack),
        "choicepoints": list(machine.choice_stack),
        "trail": list(machine.trail),
    }


def pretty_snapshot(snap: dict, max_heap: int | None = None) -> str:
    """Format snapshot as human-readable string.

    Args:
        snap: Snapshot dictionary from snapshot()
        max_heap: Optional limit on heap cells to display

    Returns:
        Multi-line formatted string showing machine state
    """
    lines = []

    # Registers
    regs = snap["regs"]
    lines.append(
        f"P={regs['P']} H={regs['H']} B={regs['B']} "
        f"E={regs['E']} CP={regs['CP']} TR={regs['TR']} HB={regs['HB']}"
    )

    # X registers
    x_vals = snap["X"]
    if x_vals:
        x_str = ", ".join(str(v) for v in x_vals)
        lines.append(f"X: [{x_str}]")

    # Heap
    heap = snap["heap"]
    if heap:
        lines.append(f"Heap ({len(heap)} cells):")
        display_heap = heap[:max_heap] if max_heap else heap
        for i, cell in enumerate(display_heap):
            lines.append(f"  {i}: {cell}")
        if max_heap and len(heap) > max_heap:
            lines.append(f"  ... ({len(heap) - max_heap} more cells)")

    # Environment frames
    frames = snap["frames"]
    if frames:
        lines.append(f"Environment frames: {len(frames)}")

    # Choicepoints
    choice = snap["choicepoints"]
    if choice:
        lines.append(f"Choicepoints: {len(choice)}")

    # Trail
    trail = snap["trail"]
    if trail:
        lines.append(f"Trail ({len(trail)} entries):")
        for entry in trail:
            lines.append(f"  {entry}")

    return "\n".join(lines)
