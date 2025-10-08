class BreakpointStore:
    """Stores and matches predicate breakpoints."""

    def __init__(self):
        self._breakpoints = {}
        self._next_id = 0

    def add_breakpoint(self, functor, arity, ports=None):
        bp_id = self._next_id
        self._breakpoints[bp_id] = {
            "functor": functor,
            "arity": arity,
            "ports": set(ports) if ports else None,
        }
        self._next_id += 1
        return bp_id

    def remove_breakpoint(self, bp_id):
        if bp_id in self._breakpoints:
            del self._breakpoints[bp_id]

    def has_breakpoint(self, functor, arity):
        for bp in self._breakpoints.values():
            if bp["functor"] == functor and bp["arity"] == arity:
                return True
        return False

    def matches(self, event):
        for bp in self._breakpoints.values():
            if bp["functor"] == event.get("functor") and bp["arity"] == event.get(
                "arity"
            ):
                if bp["ports"] is None or event.get("port") in bp["ports"]:
                    return True
        return False
