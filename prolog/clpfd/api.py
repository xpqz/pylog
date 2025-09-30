"""CLP(FD) API for domain and watcher management.

Helper functions for accessing and updating CLP(FD) attributes
via the Stage 4 attributed variables mechanism.
"""

from typing import Optional, Iterator, Tuple
from prolog.clpfd.priority import Priority


def get_fd_attrs(store, varid: int) -> Optional[dict]:
    """Get CLP(FD) attributes for a variable.

    Args:
        store: Variable store
        varid: Variable ID

    Returns:
        Dictionary of CLP(FD) attributes or None if not present
    """
    attrs = store.get_attrs(varid)
    if attrs and "clpfd" in attrs:
        return attrs["clpfd"]
    return None


def get_domain(store, varid: int):
    """Get domain for a variable.

    Args:
        store: Variable store
        varid: Variable ID

    Returns:
        Domain object or None if variable has no domain
    """
    fd_attrs = get_fd_attrs(store, varid)
    return fd_attrs.get("domain") if fd_attrs else None


def set_domain(store, varid: int, domain, trail):
    """Set domain for a variable with trailing.

    Args:
        store: Variable store
        varid: Variable ID
        domain: Domain object to set
        trail: Trail for backtracking

    Returns:
        The domain that was set
    """
    # Get current attrs via store helper
    attrs = store.get_attrs(varid) or {}

    # Get or create clpfd attrs (immutable update)
    fd_attrs = attrs.get("clpfd", {}).copy() if "clpfd" in attrs else {}
    old_domain = fd_attrs.get("domain")

    # Only update if changed
    if old_domain == domain:
        return domain

    # Set new domain
    fd_attrs["domain"] = domain

    # Update attrs with trailing (put_attr handles trailing)
    store.put_attr(varid, "clpfd", fd_attrs, trail)
    return domain


def add_watcher(store, varid: int, pid: int, priority: Priority, trail):
    """Add propagator to variable's watcher set.

    Args:
        store: Variable store
        varid: Variable ID
        pid: Propagator ID
        priority: Priority level
        trail: Trail for backtracking
    """
    attrs = store.get_attrs(varid) or {}

    # Get or create clpfd attrs (immutable update)
    fd_attrs = attrs.get("clpfd", {}).copy() if "clpfd" in attrs else {}

    # Initialize or copy watchers for immutable update
    if "watchers" not in fd_attrs:
        fd_attrs["watchers"] = {
            Priority.HIGH: set(),
            Priority.MED: set(),
            Priority.LOW: set(),
        }
    else:
        # Copy watchers for immutable update, ensuring all priorities exist
        old_watchers = fd_attrs["watchers"]
        fd_attrs["watchers"] = {
            Priority.HIGH: old_watchers.get(Priority.HIGH, set()).copy(),
            Priority.MED: old_watchers.get(Priority.MED, set()).copy(),
            Priority.LOW: old_watchers.get(Priority.LOW, set()).copy(),
        }

    # Add to appropriate priority set
    fd_attrs["watchers"][priority].add(pid)

    # Update attrs
    store.put_attr(varid, "clpfd", fd_attrs, trail)


def iter_watchers(store, varid: int) -> Iterator[Tuple[int, Priority]]:
    """Iterate over all watchers for a variable.

    Yields watchers in priority order (HIGH, MED, LOW).

    Args:
        store: Variable store
        varid: Variable ID

    Yields:
        Tuples of (propagator_id, priority)
    """
    fd_attrs = get_fd_attrs(store, varid)
    if not fd_attrs or "watchers" not in fd_attrs:
        return

    watchers = fd_attrs["watchers"]
    # Yield in priority order
    for priority in Priority:
        for pid in watchers.get(priority, set()):
            yield (pid, priority)
