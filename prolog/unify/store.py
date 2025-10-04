"""Store implementation for variable cells and unification."""

from dataclasses import dataclass
from typing import Literal, Optional, Any, List, Tuple


# Minimum path length for compression
COMPRESS_MIN_PATH = 4


@dataclass
class Cell:
    """A cell in the store representing a variable's state."""

    tag: Literal["unbound", "bound"]
    ref: int  # parent varid for unbound (union-find); self if root
    term: Optional[Any] = None  # bound Term when tag="bound"
    rank: int = 0  # union-by-rank optimization (only meaningful for unbound roots)


class Store:
    """Variable store with union-find structure."""

    def __init__(self):
        """Initialize an empty store."""
        self.cells: List[Cell] = []
        self.attrs: dict[int, dict[str, Any]] = {}  # Sparse attribute storage

    def new_var(self, hint: Optional[str] = None) -> int:
        """Create a new unbound variable and return its ID.

        Args:
            hint: Optional hint for debugging (not stored in cell)

        Returns:
            The variable ID (index in cells list)
        """
        vid = len(self.cells)
        self.cells.append(Cell(tag="unbound", ref=vid, term=None, rank=0))
        return vid

    def size(self) -> int:
        """Get current store size (number of cells).

        Returns:
            The number of cells in the store.
        """
        return len(self.cells)

    def shrink_to(self, n: int) -> None:
        """Shrink the store to the given size.

        Only shrinks pure allocations (not bindings).
        Used for backtracking past allocation points.

        Args:
            n: Target size to shrink to.
        """
        if n < 0 or n > len(self.cells):
            return
        # Pop cells back to size n
        self.cells = self.cells[:n]

    def deref(
        self, varid: int, compress: bool = False, trail: Optional[List] = None
    ) -> Tuple:
        """Follow union-find chains to find the root.

        Args:
            varid: Variable ID to dereference
            compress: Whether to perform path compression
            trail: Trail to record compressions (required if compress=True)

        Returns:
            One of:
            - ("BOUND", root_vid, bound_term) for bound variables
            - ("UNBOUND", root_vid) for unbound variables
        """
        path = []
        v = varid

        # Follow parent links to find root
        while True:
            c = self.cells[v]
            if c.tag == "bound":
                # Found bound root
                root = ("BOUND", v, c.term)
                break
            parent = c.ref
            if parent == v:
                # Found unbound root
                root = ("UNBOUND", v)
                break
            path.append(v)
            v = parent

        # Path compression if requested
        # Path length check: we have len(path) nodes that need compression
        # For a chain v0->v1->v2->v3->v4, path = [v0, v1, v2, v3]
        if compress and trail is not None and len(path) >= COMPRESS_MIN_PATH:
            # Compress all nodes in path to point directly to root
            root_vid = root[1]
            for p in path:
                old_ref = self.cells[p].ref
                # Trail the change
                trail.append(("parent", p, old_ref))
                # Update parent
                self.cells[p].ref = root_vid

        return root

    def get_attrs(self, varid: int) -> Optional[dict[str, Any]]:
        """Get all attributes for a variable (after deref).

        Args:
            varid: Variable ID

        Returns:
            Dictionary of module->value or None if no attributes
        """
        # Deref to find root
        result = self.deref(varid)
        if result[0] == "BOUND":
            # Bound variables don't have attributes
            return None
        root_vid = result[1]
        return self.attrs.get(root_vid)

    def get_attr(self, varid: int, module: str) -> Optional[Any]:
        """Get specific attribute for a variable.

        Args:
            varid: Variable ID
            module: Module name

        Returns:
            Attribute value or None if not present
        """
        attrs = self.get_attrs(varid)
        return attrs.get(module) if attrs else None

    def put_attr(self, varid: int, module: str, value: Any, trail: Any) -> None:
        """Set attribute with trailing.

        Args:
            varid: Variable ID
            module: Module name
            value: Attribute value
            trail: Trail for backtracking
        """
        # Deref to find root
        result = self.deref(varid)
        if result[0] == "BOUND":
            # Cannot put attributes on bound variables
            return
        root_vid = result[1]

        # Trail the old value (None if new)
        old_value = None
        if root_vid in self.attrs and module in self.attrs[root_vid]:
            old_value = self.attrs[root_vid][module]
        trail.push(("attr", root_vid, module, old_value))

        # Set new value
        if root_vid not in self.attrs:
            self.attrs[root_vid] = {}
        self.attrs[root_vid][module] = value

    def del_attr(self, varid: int, module: str, trail: Any) -> None:
        """Delete attribute with trailing.

        Args:
            varid: Variable ID
            module: Module name
            trail: Trail for backtracking
        """
        # Deref to find root
        result = self.deref(varid)
        if result[0] == "BOUND":
            # Cannot delete attributes from bound variables
            return
        root_vid = result[1]

        # Only act if attribute exists
        if root_vid in self.attrs and module in self.attrs[root_vid]:
            old_value = self.attrs[root_vid][module]
            trail.push(("attr", root_vid, module, old_value))
            del self.attrs[root_vid][module]

            # Clean up empty dict
            if not self.attrs[root_vid]:
                del self.attrs[root_vid]
