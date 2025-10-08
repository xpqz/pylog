"""Debug Adapter Protocol (DAP) integration for PyLog.

This package provides the engine-side components needed for DAP debugging:

- **StepController**: Manages stepping modes (step in/over/out, continue, pause)
  and controls execution flow via a barrier mechanism.

- **BreakpointStore**: Stores and matches predicate breakpoints with optional
  port filtering (e.g., break only on CALL port).

- **snapshot**: Creates immutable snapshots of engine state (goal stack, frame
  stack, variable bindings) for inspection by the debugger.

These components integrate with the PyLog engine's tracer to provide debugging
capabilities following the Debug Adapter Protocol specification.

Example usage:
    from prolog.debug.dap import StepController, BreakpointStore, create_snapshot

    # Create debugging components
    controller = StepController(eligible_ports={"CALL", "EXIT", "FAIL"})
    bp_store = BreakpointStore()

    # Add breakpoint
    bp_id = bp_store.add_breakpoint("member", 2, ports=["CALL"])

    # In tracer hook:
    if controller.should_pause(event) or bp_store.matches(event):
        snapshot = create_snapshot(engine, event)
        controller.wait_for_resume()  # Block until debugger resumes

See docs/dap.md for the complete architecture and integration guide.
"""

from prolog.debug.dap.step_controller import StepController
from prolog.debug.dap.breakpoint_store import BreakpointStore
from prolog.debug.dap.snapshot import create_snapshot

__all__ = ["StepController", "BreakpointStore", "create_snapshot"]
