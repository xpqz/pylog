"""DAP session management.

Manages the state of a single DAP debugging session, including:
- Initialization state
- Engine instance and thread
- StepController and BreakpointStore
- Session cleanup
- Event emission to DAP client
"""

import threading
import logging
from typing import Optional, Any

logger = logging.getLogger(__name__)


class SessionManager:
    """Manages the state of a DAP debugging session.

    This is a singleton that maintains session state across handler calls.
    """

    def __init__(self):
        """Initialize session manager."""
        self._initialized = False
        self._engine = None
        self._engine_thread: Optional[threading.Thread] = None
        self._step_controller = None
        self._breakpoint_store = None
        self._server = None  # DAPServer instance for event emission

    def is_initialized(self) -> bool:
        """Check if session has been initialized."""
        return self._initialized

    def initialize(self):
        """Mark session as initialized."""
        self._initialized = True

    def set_engine_components(self, engine, step_controller, breakpoint_store):
        """Set engine and debugging components.

        Args:
            engine: Engine instance
            step_controller: StepController instance
            breakpoint_store: BreakpointStore instance
        """
        self._engine = engine
        self._step_controller = step_controller
        self._breakpoint_store = breakpoint_store

    def set_engine_thread(self, thread: threading.Thread):
        """Set the engine execution thread.

        Args:
            thread: Thread running the engine
        """
        self._engine_thread = thread

    def set_server(self, server):
        """Set the DAP server for event emission.

        Args:
            server: DAPServer instance
        """
        self._server = server

    def send_event(self, event: str, body: Optional[dict[str, Any]] = None):
        """Send an event to the DAP client.

        Args:
            event: Event name (e.g., "stopped", "output", "terminated")
            body: Optional event data
        """
        if self._server:
            self._server.send_event(event, body)
        else:
            logger.warning(f"Cannot send event '{event}' - no server set")

    def send_stopped_event(
        self,
        reason: str,
        thread_id: int = 1,
        description: Optional[str] = None,
        hit_breakpoint_ids: Optional[list[int]] = None,
    ):
        """Send a stopped event to the client.

        Args:
            reason: Stop reason (step, breakpoint, entry, pause, etc.)
            thread_id: Thread that stopped (default 1, PyLog is single-threaded)
            description: Optional human-readable description
            hit_breakpoint_ids: Optional list of breakpoint IDs that were hit
        """
        body: dict[str, Any] = {
            "reason": reason,
            "threadId": thread_id,
            "allThreadsStopped": True,  # PyLog is single-threaded
        }

        if description:
            body["description"] = description

        if hit_breakpoint_ids:
            body["hitBreakpointIds"] = hit_breakpoint_ids

        self.send_event("stopped", body)
        logger.debug(f"Stopped event sent: reason={reason}")

    def send_output_event(
        self, output: str, category: str = "stdout", variables_reference: int = 0
    ):
        """Send an output event to the client.

        Args:
            output: Output text
            category: Output category (stdout, stderr, console, etc.)
            variables_reference: Optional variables reference for structured output
        """
        body: dict[str, Any] = {"output": output}

        if category:
            body["category"] = category

        if variables_reference:
            body["variablesReference"] = variables_reference

        self.send_event("output", body)

    def send_terminated_event(self, restart: Optional[Any] = None):
        """Send a terminated event to the client.

        Args:
            restart: Optional restart data
        """
        body = {"restart": restart} if restart is not None else None
        self.send_event("terminated", body)
        logger.info("Terminated event sent")

    def cleanup(self):
        """Clean up session resources."""
        # Release step controller barrier
        if self._step_controller:
            self._step_controller.disconnect()

        # Stop engine thread if running
        if self._engine_thread and self._engine_thread.is_alive():
            # Thread should stop on its own when barrier is released
            self._engine_thread.join(timeout=5.0)
            if self._engine_thread.is_alive():
                logger.warning(
                    "Engine thread did not stop within 5s timeout - potential resource leak"
                )

        # Clear state
        self._engine = None
        self._engine_thread = None
        self._step_controller = None
        self._breakpoint_store = None
        self._server = None
        self._initialized = False

    @property
    def engine(self):
        """Get the engine instance."""
        return self._engine

    @property
    def step_controller(self):
        """Get the step controller."""
        return self._step_controller

    @property
    def breakpoint_store(self):
        """Get the breakpoint store."""
        return self._breakpoint_store


# Global session instance
_session = SessionManager()
_session_lock = threading.Lock()


def get_session() -> SessionManager:
    """Get the global session manager instance.

    Note: This returns a reference to the singleton instance. The instance itself
    is not thread-safe for concurrent access to its methods. This design assumes
    a single DAP connection at a time.
    """
    return _session


def reset_session():
    """Reset the global session (useful for testing).

    This operation is thread-safe with respect to reassigning the global _session
    variable, but callers should ensure no concurrent access to session methods.
    """
    global _session
    with _session_lock:
        _session.cleanup()
        _session = SessionManager()
