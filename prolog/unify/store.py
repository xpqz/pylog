"""Store implementation for variable cells and unification."""

from dataclasses import dataclass
from typing import Literal, Optional, Any, List, Tuple


@dataclass
class Cell:
    """A cell in the store representing a variable's state."""
    tag: Literal["unbound", "bound"]
    ref: int                    # parent varid for unbound (union-find); self if root
    term: Optional[Any] = None  # bound Term when tag="bound"
    rank: int = 0               # union-by-rank optimization (only meaningful for unbound roots)


class Store:
    """Variable store with union-find structure."""
    
    def __init__(self):
        """Initialize an empty store."""
        self.cells: List[Cell] = []
    
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
    
    def deref(self, varid: int, compress: bool = False, trail: Optional[List] = None) -> Tuple:
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
        if compress and trail is not None and len(path) >= 4:
            # Compress all nodes in path to point directly to root
            root_vid = root[1]
            for p in path:
                old_ref = self.cells[p].ref
                # Trail the change
                trail.append(("parent", p, old_ref))
                # Update parent
                self.cells[p].ref = root_vid
        
        return root