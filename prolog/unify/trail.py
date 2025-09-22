"""Trail implementation for backtracking in unification.

The trail records all mutations to enable backtracking.
Trail entries are tuples with a tag and relevant data:
- ('parent', varid, old_parent): union-find parent change
- ('bind', varid, old_cell): variable binding
- ('rank', varid, old_rank): rank change for union operations
"""

from contextlib import contextmanager
from typing import List, Tuple, Any


# Type alias for trail entries
TrailEntry = Tuple[str, ...]


class Trail:
    """Trail for recording and undoing mutations."""

    def __init__(self):
        """Initialize an empty trail."""
        self.entries: List[TrailEntry] = []

    def push(self, entry: TrailEntry) -> None:
        """Add an entry to the trail.

        Args:
            entry: A trail entry tuple (tag, data...)
        """
        self.entries.append(entry)

    def mark(self) -> int:
        """Return the current trail position.

        Returns:
            The current number of entries (can be used with undo_to)
        """
        return len(self.entries)

    def clear(self) -> None:
        """Empty the trail completely."""
        self.entries.clear()

    def __len__(self) -> int:
        """Return the number of entries in the trail."""
        return len(self.entries)

    def __getitem__(self, index):
        """Allow indexing into trail entries."""
        return self.entries[index]

    def append(self, entry: TrailEntry) -> None:
        """Alias for push to support list-like interface."""
        self.push(entry)


def undo_to(mark: int, trail: Any, store: Any) -> None:
    """Undo trail entries back to a marked position.

    Args:
        mark: The trail position to restore to
        trail: Trail object or list of trail entries
        store: Store object to restore

    This function pops entries from the trail in reverse order
    and applies the inverse operation for each entry type.
    """
    # Support both Trail objects and plain lists
    entries = trail.entries if hasattr(trail, "entries") else trail

    while len(entries) > mark:
        entry = entries.pop()
        tag = entry[0]

        if tag == "parent":
            # Restore parent link
            _, vid, old_parent = entry
            store.cells[vid].ref = old_parent

        elif tag == "bind":
            # Restore entire cell
            _, vid, old_cell = entry
            store.cells[vid] = old_cell

        elif tag == "rank":
            # Restore rank value
            _, vid, old_rank = entry
            store.cells[vid].rank = old_rank

        elif tag == "attr":
            # Restore attribute value
            _, vid, module, old_value = entry
            if old_value is None:
                # Attribute didn't exist before - remove it
                if hasattr(store, 'attrs') and vid in store.attrs and module in store.attrs[vid]:
                    del store.attrs[vid][module]
                    # Clean up empty dict
                    if not store.attrs[vid]:
                        del store.attrs[vid]
            else:
                # Restore old value
                if not hasattr(store, 'attrs'):
                    store.attrs = {}
                if vid not in store.attrs:
                    store.attrs[vid] = {}
                store.attrs[vid][module] = old_value

        else:
            raise ValueError(f"Unknown trail entry tag: {tag}")


@contextmanager
def trail_guard(trail: Any, store: Any):
    """Context manager for automatic trail restoration on exception.

    Args:
        trail: Trail object or list
        store: Store object

    Yields:
        mark: The trail mark at entry

    If an exception occurs, automatically undoes to the mark.
    """
    # Support both Trail objects and plain lists
    if hasattr(trail, "mark"):
        mark = trail.mark()
    else:
        mark = len(trail)

    try:
        yield mark
    except Exception:
        undo_to(mark, trail, store)
        raise
