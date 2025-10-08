"""DAP session management.

Manages the state of a single DAP debugging session, including:
- Initialization state
- Engine instance and thread
- StepController and BreakpointStore
- Session cleanup
"""

import threading
from typing import Optional
from pathlib import Path


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
        self._program_path: Optional[Path] = None
        self._query: Optional[str] = None

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

    def cleanup(self):
        """Clean up session resources."""
        # Release step controller barrier
        if self._step_controller:
            self._step_controller.disconnect()

        # Stop engine thread if running
        if self._engine_thread and self._engine_thread.is_alive():
            # Thread should stop on its own when barrier is released
            self._engine_thread.join(timeout=5.0)

        # Clear state
        self._engine = None
        self._engine_thread = None
        self._step_controller = None
        self._breakpoint_store = None
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


def get_session() -> SessionManager:
    """Get the global session manager instance."""
    return _session


def reset_session():
    """Reset the global session (useful for testing)."""
    global _session
    _session.cleanup()
    _session = SessionManager()
