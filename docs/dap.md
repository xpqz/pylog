# VS Code Debug Adapter for PyLog — Plan

This document outlines a practical, incremental plan to add an MVP‑quality VS Code debugging experience for PyLog using the Debug Adapter Protocol (DAP).

## Goals (MVP)

- Single thread/session debugging (PyLog is single‑threaded).
- Stepping: continue, step in, step over, step out at Prolog “ports”.
- Breakpoints: predicate breakpoints (by functor/arity) with optional port filter.
- Call stack: show Prolog stack (goals/frames) with the current goal highlighted.
- Variables view: show current query variable bindings (names and terms).
- Evaluate: simple variable lookup by name.

## Non‑Goals (for MVP)

- Multi‑thread or multi‑process debugging.
- Remote attach and hot‑reloading.
- Full source mapping + file/line breakpoints (planned after MVP).
- Conditional breakpoints, watch expressions, data breakpoints.

## High‑Level Architecture

The architecture is designed to be standards-compliant and minimally invasive. The Python component implements the Debug Adapter Protocol directly, allowing any DAP-compliant editor to connect.

- **VS Code Extension (TypeScript)**: A minimal extension that contributes the `pylog` debug type and launches the Python DAP server.
- **PyLog DAP Server (Python)**: A standalone server that implements the Debug Adapter Protocol. It listens for DAP commands, controls the PyLog engine, and sends back DAP events.
- **Engine Integration (Python)**: A small `StepController` wired into the tracer to pause on ports and implement step semantics.

The official **SWI-Prolog `debug_adapter` pack** will serve as a reference for mapping DAP concepts to Prolog's execution model.

```
VS Code  <— DAP —>  PyLog DAP Server (Python)  <—>  Engine + Tracer
```

## Engine Integration

Minimal additions to the engine/tracer layer are required.

### StepController
- Holds current stepping mode: `running | paused | stepIn | stepOver | stepOut`.
- Records a baseline call depth when a stepOver/stepOut command is issued.
- Provides a gate function, `should_pause(event)`, that decides whether to pause based on the current mode, depth, ports, and breakpoints.
- Manages a barrier mechanism to block and unblock the engine thread.

### Barrier Mechanism
The interaction between the single-threaded PyLog engine and the multi-threaded DAP server requires a thread-safe barrier. `threading.Event` is a suitable choice.

```python
# Example barrier implementation:
import threading

class StepController:
    def __init__(self):
        self._barrier = threading.Event()
        self._mode = "running"

    def should_pause(self, event) -> bool:
        # ... logic to determine if a pause is needed ...
        return should_pause

    def wait_for_resume(self):
        """Block engine thread until the DAP server calls resume()."""
        # A timeout is critical to prevent deadlocks if the client disconnects.
        if not self._barrier.wait(timeout=60.0):
            # If timeout occurs, assume client is gone and force resume.
            self._mode = "running"
        self._barrier.clear()

    def resume(self):
        """Called by the DAP server to unblock the engine thread."""
        self._barrier.set()

    def disconnect(self):
        """Force-release the barrier on disconnect to prevent hangs."""
        self._mode = "running"
        self._barrier.set()
```

### Tracer Hook
- At every emitted port event (CALL/EXIT/FAIL/REDO), the tracer calls `StepController.should_pause(event)`.
- If it returns `True`, the tracer publishes a snapshot to the DAP server and then calls `StepController.wait_for_resume()`.

### Snapshot Provider
- Serializes the minimal state required by the client when paused: current goal, port, depth, frame summary, and query variable bindings.
- **Variable Reference Stability**: To handle complex terms, cycles, and attributed variables without deep copying, variable references will be generated using `id(term)`. A mapping from these IDs to the actual terms will be maintained, likely in a `WeakValueDictionary`, to ensure stability across DAP requests within a single paused state.

## Stepping Semantics

Stepping is defined by the tracer’s port events and the goal stack depth:
- **stepIn**: Pause on the very next eligible port event.
- **stepOver**: Record `baseline = depth_at_command`; pause on the next eligible event where `depth <= baseline`.
- **stepOut**: Record `baseline = depth_at_command`; pause at the first eligible event where `depth < baseline`.
- **continue**: Run without pausing except at breakpoints.

### Port Eligibility
- Eligible ports for MVP are `CALL`, `EXIT`, and `FAIL`.
- The `REDO` port can be noisy, as backtracking may generate many events. It will be disabled by default but can be enabled in the `launch.json` configuration for detailed debugging.

## Breakpoints

- **MVP**: Predicate breakpoints specified by functor/arity, with an optional filter for ports (e.g., pause on `CALL` only).
- Matching is performed at `CALL` time for efficiency.

## Variables and Scopes

- **Scopes**: A single "Locals" scope will show the query variables.
- **Variable Expansion**:
  - To prevent infinite recursion and excessive data transfer, expansion will be limited.
  - `MAX_EXPANSION_DEPTH = 3`
  - `MAX_LIST_PREVIEW = 100` (for large lists)
  - Circular references (e.g., `X = f(X)`) will be detected during serialization and represented with a special marker.

## PyLog DAP Server (Python)

This component is a standalone DAP server. It is recommended to implement the protocol manually, using the official [DAP Specification](https://microsoft.github.io/debug-adapter-protocol/) and the SWI-Prolog implementation as guides, rather than using a library like `debugpy` which is tailored for Python code debugging.

**Responsibilities**:
- Open a socket or use stdio to listen for a DAP client.
- Handle DAP lifecycle requests: `initialize`, `launch`, `disconnect`.
- Translate DAP stepping requests into commands for the `StepController`.
- Manage breakpoints.
- Respond to data requests (`stackTrace`, `scopes`, `variables`) by querying the engine's state.
- Send DAP events (`stopped`, `terminated`, `output`) to the client.

## VS Code Extension (TypeScript)

The extension is a thin launcher.
- It uses `vscode.DebugAdapterDescriptorFactory` to spawn the Python DAP Server process.
- It contributes the `pylog` debugger type and `launch.json` snippets.

## DAP Request Mapping

| DAP Request | PyLog Action |
|---|---|
| `initialize` | Return capabilities |
| `launch` | Load program, start engine |
| `setBreakpoints` | Update BreakpointStore |
| `continue` | `StepController.resume()` with mode "running" |
| `next` | `StepController.resume()` with mode "stepOver" |
| `stepIn` | `StepController.resume()` with mode "stepIn" |
| `stepOut` | `StepController.resume()` with mode "stepOut" |
| `stackTrace` | Serialize `engine.goal_stack` |
| `scopes` | Return "Locals" scope |
| `variables` | Expand `variablesReference` by ID |
| `evaluate` | Lookup variable by name in the current frame |
| `disconnect` | Cleanup, release barrier, terminate engine |

## Example: Step-Over Flow

1. User clicks "Step Over" in VS Code.
2. VS Code → DAP Server: `{"command": "next", "arguments": {"threadId": 1}}`
3. DAP Server calls `StepController.resume()` with mode "stepOver".
4. Engine runs until the next `CALL` event.
5. Tracer → `StepController.should_pause(event)`
6. StepController logic: `current_depth <= baseline_depth` is true.
7. A snapshot of the engine state is created. The engine thread blocks on `StepController.wait_for_resume()`.
8. DAP Server → VS Code: `{"event": "stopped", "reason": "step", ...}`
9. VS Code requests `stackTrace`, `scopes`, and `variables` to update the UI.
10. User examines state and clicks "Continue".
11. Cycle repeats.

## Packaging

### Launch Configuration
The `launch.json` will be expanded to include engine-specific settings.

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "pylog",
      "request": "launch",
      "name": "Debug PyLog",
      "program": "${workspaceFolder}/examples/demo.pl",
      "query": "?- main.",
      "stopOnEntry": true,
      "ports": ["CALL", "EXIT", "FAIL"],
      "predicateBreakpoints": [
        {"functor": "member", "arity": 2, "ports": ["CALL"]}
      ],
      "occursCheck": false,
      "useIndexing": true
    }
  ]
}
```

### Entry Points
- VS Code extension (`pylog-vscode`).
- Python DAP Server console script (`pylog-dap`).

## Test Strategy

- **Unit tests (Python)**: `StepController` semantics, breakpoint matching, and DAP message handlers.
- **Integration tests**: A Python script acting as a DAP client will connect to the server and verify event sequences and data, removing the need for a Node.js harness.
- **Manual tests (VS Code)**: Test with real programs involving facts, recursion, and CLP(FD).

## Task Breakdown (MVP)

- **Engine**
  - [ ] Add `StepController` with barrier (`threading.Event`)
  - [ ] Add depth calculation to tracer events
  - [ ] Implement breakpoint matching logic
  - [ ] Implement snapshot API for stack/vars

- **PyLog DAP Server (Python)**
  - [ ] Implement DAP socket/stdio server loop
  - [ ] Handle `initialize`, `launch`, `disconnect` requests
  - [ ] Map `continue`, `next`, `stepIn`, `stepOut` to `StepController`
  - [ ] Handle `setBreakpoints` request
  - [ ] Implement `stackTrace`, `scopes`, `variables` requests with stable IDs
  - [ ] Send `stopped`, `output`, `terminated` events

- **VS Code Extension (TypeScript)**
  - [ ] Implement `DebugAdapterDescriptorFactory`
  - [ ] Spawn the Python DAP server process
  - [ ] Contribute the `pylog` debugger type and `launch.json` snippets

- **Docs**
  - [ ] README for the extension
  - [ ] User guide page with screenshots

---

## WAM Integration Notes (experimental backend)

- Backend toggle: support both tree‑walker and WAM backends; add a `backend` field in launch config (e.g., `"wam"` or `"tree"`).
- Stepping granularity:
  - Goal‑level (ports): default, same as tree‑walker using CALL/EXIT/REDO/FAIL.
  - Instruction‑level: optional; pauses at WAM instruction boundaries with registers view.
- Variables view (WAM):
  - Expose `P`, `CP`, `B`, `E`, `HB`, `H`, `TR` and selected `X[i]`/`Y[i]` values as expandable nodes.
  - Reify only on demand and with depth/length caps to avoid heavy serialization.
- Catch/throw mapping: ensure thrown terms and error terms surface identically under both backends; stop reasons include `exception` where applicable.
- Performance: keep tracing/debugging costs bounded; gate heavy reification and heap snapshots behind explicit flags.
