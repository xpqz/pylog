class BreakpointStore:
    """Stores and matches predicate breakpoints."""

    def __init__(self):
        self._breakpoints = {}
        self._next_id = 0

    def add_breakpoint(self, functor, arity, ports=None):
        """Add a breakpoint for a predicate.

        Args:
            functor: Predicate name (e.g., "member")
            arity: Number of arguments (e.g., 2 for member/2)
            ports: Optional list of ports to break on (e.g., ["CALL", "EXIT"])
                   If None, breaks on all ports

        Returns:
            Breakpoint ID for later removal
        """
        bp_id = self._next_id
        self._breakpoints[bp_id] = {
            "functor": functor,
            "arity": arity,
            "ports": set(ports) if ports else None,
        }
        self._next_id += 1
        return bp_id

    def remove_breakpoint(self, bp_id):
        """Remove a breakpoint by ID.

        Args:
            bp_id: Breakpoint ID returned from add_breakpoint()
        """
        if bp_id in self._breakpoints:
            del self._breakpoints[bp_id]

    def has_breakpoint(self, functor, arity):
        """Check if a breakpoint exists for a predicate.

        Args:
            functor: Predicate name
            arity: Number of arguments

        Returns:
            True if any breakpoint exists for this predicate
        """
        for bp in self._breakpoints.values():
            if bp["functor"] == functor and bp["arity"] == arity:
                return True
        return False

    def matches(self, event):
        """Check if any breakpoint matches this trace event.

        Args:
            event: Dict with 'functor', 'arity', 'port' keys

        Returns:
            True if event matches any breakpoint (considering port filters)
        """
        for bp in self._breakpoints.values():
            if bp["functor"] == event.get("functor") and bp["arity"] == event.get(
                "arity"
            ):
                if bp["ports"] is None or event.get("port") in bp["ports"]:
                    return True
        return False
