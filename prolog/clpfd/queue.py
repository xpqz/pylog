"""Propagation queue for CLP(FD) constraint propagation.

Implements priority-based queue with deduplication, priority escalation,
and self-requeue handling to compute fixpoint efficiently.
"""

from enum import IntEnum
from typing import Dict, List, Set, Tuple, Optional, Callable, Any
from collections import deque


class Priority(IntEnum):
    """Propagator priorities (lower value = higher priority)."""
    HIGH = 0  # Equality propagators
    MED = 1   # Inequality propagators  
    LOW = 2   # Labeling/search


class PropagationQueue:
    """Priority-based propagation queue with deduplication.
    
    Key features:
    - Three priority levels (HIGH/MED/LOW)
    - Automatic deduplication of queued propagators
    - Priority escalation when re-scheduling
    - Self-requeue prevention during propagator execution
    - Fixpoint computation with failure detection
    """

    def __init__(self):
        """Initialize empty propagation queue."""
        # Priority queues (deques for FIFO within priority)
        self.queues: Dict[Priority, deque] = {
            Priority.HIGH: deque(),
            Priority.MED: deque(),
            Priority.LOW: deque()
        }
        
        # Track which propagators are queued and at what priority
        # Maps propagator_id -> Priority
        self.queued: Dict[int, Priority] = {}
        
        # Registered propagators
        # Maps propagator_id -> callable(store, trail, engine, cause)
        self.propagators: Dict[int, Callable] = {}
        
        # Propagator ID counter
        self._next_pid = 0
        
        # Currently running propagator (for self-requeue detection)
        self.running: Optional[int] = None
        
        # Reschedule set for self-requeued propagators
        # Set of (propagator_id, Priority) tuples
        self.reschedule: Set[Tuple[int, Priority]] = set()
        
        # Causes for each queued propagator
        # Maps propagator_id -> cause
        self.causes: Dict[int, Any] = {}

    def register(self, propagator: Callable) -> int:
        """Register a propagator and return its unique ID.
        
        Args:
            propagator: Callable(store, trail, engine, cause) -> (status, changed_vars)
                       where status is 'ok' or 'fail'
                       and changed_vars is None or list of variable IDs
        
        Returns:
            Unique propagator ID
        """
        pid = self._next_pid
        self._next_pid += 1
        self.propagators[pid] = propagator
        return pid

    def schedule(self, propagator_id: int, priority: Priority, cause: Any = None) -> None:
        """Schedule a propagator for execution.
        
        Features:
        - If already queued at same or higher priority, no-op
        - If already queued at lower priority, escalates to higher priority
        - If currently running (self-requeue), adds to reschedule set
        
        Args:
            propagator_id: ID of propagator to schedule
            priority: Priority level for execution
            cause: Optional cause information (e.g., ('domain_changed', varid))
        """
        # Handle self-requeue
        if self.running == propagator_id:
            self.reschedule.add((propagator_id, priority))
            return
        
        # Check if already queued
        if propagator_id in self.queued:
            current_priority = self.queued[propagator_id]
            
            # Priority escalation: move to higher priority if needed
            if priority < current_priority:
                # Remove from old queue
                self._remove_from_queue(propagator_id, current_priority)
                # Add to new higher priority queue
                self._add_to_queue(propagator_id, priority, cause)
            # else: already at same or higher priority, no-op
        else:
            # Not queued, add it
            self._add_to_queue(propagator_id, priority, cause)

    def _add_to_queue(self, propagator_id: int, priority: Priority, cause: Any) -> None:
        """Add propagator to specified priority queue."""
        self.queues[priority].append(propagator_id)
        self.queued[propagator_id] = priority
        if cause is not None:
            self.causes[propagator_id] = cause

    def _remove_from_queue(self, propagator_id: int, priority: Priority) -> None:
        """Remove propagator from specified priority queue."""
        # Note: This is O(n) but queues are typically small
        # Could optimize with additional data structures if needed
        queue = self.queues[priority]
        try:
            queue.remove(propagator_id)
        except ValueError:
            pass  # Not in queue (shouldn't happen but be defensive)

    def pop(self) -> Optional[Tuple[int, Any]]:
        """Pop highest priority propagator from queue.
        
        Returns:
            Tuple of (propagator_id, cause) or None if queue is empty
        """
        # Try priorities in order (HIGH -> MED -> LOW)
        for priority in Priority:
            queue = self.queues[priority]
            if queue:
                pid = queue.popleft()
                del self.queued[pid]
                cause = self.causes.pop(pid, None)
                return (pid, cause)
        
        return None

    def is_empty(self) -> bool:
        """Check if queue is empty."""
        return len(self.queued) == 0

    def run_to_fixpoint(self, store, trail, engine) -> bool:
        """Run propagators until fixpoint or failure.
        
        Args:
            store: Variable store
            trail: Trail for backtracking
            engine: Engine instance
        
        Returns:
            True if fixpoint reached, False if any propagator failed
        """
        while not self.is_empty():
            item = self.pop()
            if item is None:
                break
                
            pid, cause = item
            propagator = self.propagators.get(pid)
            if propagator is None:
                continue  # Propagator was unregistered?
            
            # Mark as running (for self-requeue detection)
            self.running = pid
            
            try:
                # Run propagator
                status, changed_vars = propagator(store, trail, engine, cause)
                
                if status == 'fail':
                    return False
                
                # Wake watchers for changed variables
                if changed_vars:
                    for varid in changed_vars:
                        self._wake_watchers(store, varid)
                
            finally:
                # Clear running status
                self.running = None
                
                # Process reschedule set
                for p, prio in self.reschedule:
                    self.schedule(p, prio)
                self.reschedule.clear()
        
        return True

    def _wake_watchers(self, store, varid: int) -> None:
        """Wake all watchers of a variable.
        
        Args:
            store: Variable store
            varid: Variable ID whose watchers to wake
        """
        # Import here to avoid circular dependency
        from prolog.clpfd.api import iter_watchers
        
        for watcher_id, priority in iter_watchers(store, varid):
            # Schedule with cause indicating which variable changed
            self.schedule(watcher_id, priority, cause=('domain_changed', varid))