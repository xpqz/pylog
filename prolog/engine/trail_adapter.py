"""Adapter to make Trail class work with existing unify module."""

from typing import Any, Tuple
from prolog.engine.runtime import Trail


class TrailAdapter:
    """List-like adapter for Trail to work with existing unify module."""
    
    def __init__(self, trail: Trail, engine=None, store=None):
        """Initialize with a Trail instance."""
        self.trail = trail
        self.engine = engine  # Optional reference to engine for debug counters
        self.store = store  # Store reference for undo operations
        self._temp_entries = []  # Temporary storage for new entries
        self._start_pos = trail.position()  # Track if this is first write
        
    def append(self, entry: Tuple[str, ...]) -> None:
        """Append an entry to the trail (list-like interface)."""
        # Track position before push to check if write actually happened
        pos_before = self.trail.position()
        
        if entry[0] == 'bind':
            _, varid, old_cell = entry
            self.trail.push_bind(varid, old_cell)
        elif entry[0] == 'parent':
            _, varid, old_parent = entry
            self.trail.push_parent(varid, old_parent)
        elif entry[0] == 'rank':
            _, varid, old_rank = entry
            self.trail.push_rank(varid, old_rank)
        else:
            # Generic push for other entry types
            self.trail.push(entry)
        
        # Increment counter if a write actually happened
        # (push_bind/push_parent might skip if already trailed)
        if self.engine and self.trail.position() > pos_before:
            self.engine._debug_trail_writes += 1
        
        # Track for len() support
        self._temp_entries.append(entry)
    
    def push(self, entry: Tuple[str, ...]) -> None:
        """Push an entry to the trail (Trail-like interface)."""
        self.append(entry)  # Delegate to append method
    
    def __len__(self) -> int:
        """Return current position in trail."""
        return self.trail.position()
    
    @property
    def entries(self):
        """Provide access to trail entries for undo_to compatibility.
        
        This returns a wrapper that supports pop() for the undo_to function
        in prolog.unify.trail module.
        """
        return _TrailEntriesWrapper(self.trail, self.store)
    
    def pop(self) -> Tuple[str, ...]:
        """Pop an entry (for undo_trail in unify)."""
        # This is a bit of a hack - we rely on the fact that
        # undo_trail in unify will handle the actual unwinding
        if self._temp_entries:
            return self._temp_entries.pop()
        raise IndexError("pop from empty trail")


class _TrailEntriesWrapper:
    """Wrapper to make Trail.entries compatible with undo_to."""
    
    def __init__(self, trail: Trail, store):
        self.trail = trail
        self.store = store
    
    def __len__(self):
        """Return number of entries in trail."""
        return len(self.trail._entries)
    
    def pop(self):
        """Pop an entry and return it for undo_to."""
        if not self.trail._entries:
            raise IndexError("pop from empty trail")
        return self.trail._entries.pop()