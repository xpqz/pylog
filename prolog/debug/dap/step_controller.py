import threading

VALID_MODES = {"running", "paused", "step_in", "step_over", "step_out"}


class StepController:
    """Manages the stepping state of the debugger."""

    def __init__(self, eligible_ports=None):
        self._mode = "running"
        self._barrier = threading.Event()
        self._baseline_depth = -1
        # Normalize ports to uppercase for case-insensitive matching
        self.eligible_ports = (
            {p.upper() for p in eligible_ports} if eligible_ports else None
        )

    def set_mode(self, mode, depth=None):
        """Set the stepping mode.

        Args:
            mode: One of 'running', 'paused', 'step_in', 'step_over', 'step_out'
            depth: Required for step_over and step_out modes (current frame depth)

        Raises:
            ValueError: If mode is invalid or depth is missing for step_over/step_out
        """
        if mode not in VALID_MODES:
            raise ValueError(f"Invalid mode: {mode}. Must be one of {VALID_MODES}")

        if mode in ("step_over", "step_out") and depth is None:
            raise ValueError(f"{mode} requires depth parameter")

        self._mode = mode
        if mode in ("step_over", "step_out"):
            self._baseline_depth = depth
        else:
            self._baseline_depth = -1  # Reset to sentinel value

    def get_mode(self):
        return self._mode

    def should_pause(self, event):
        """Check if execution should pause at this event.

        Args:
            event: Dict with 'port', 'depth' keys (from DAP event format)

        Returns:
            True if execution should pause at this event, False otherwise
        """
        # Check port eligibility first (normalize to uppercase)
        if self.eligible_ports:
            event_port = event.get("port", "").upper()
            if event_port not in self.eligible_ports:
                return False

        if self._mode == "running":
            return False
        if self._mode == "paused" or self._mode == "step_in":
            return True

        event_depth = event.get("depth", 0)
        if self._mode == "step_over":
            return event_depth <= self._baseline_depth
        if self._mode == "step_out":
            return event_depth < self._baseline_depth

        return False

    def wait_for_resume(self, timeout=60.0):
        if not self._barrier.wait(timeout=timeout):
            self._mode = "running"  # Force resume on timeout
        self._barrier.clear()

    def resume(self):
        self._barrier.set()

    def disconnect(self):
        self._mode = "running"
        self._barrier.set()
