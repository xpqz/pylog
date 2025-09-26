"""All-different global constraint propagator."""

from typing import Tuple, Optional, List, Set
from prolog.clpfd.api import get_domain, set_domain


def create_all_different_propagator(var_ids: List[int], fixed_values: Tuple[int, ...]):
    """Create all_different propagator with value elimination.

    Args:
        var_ids: List of variable IDs participating in the constraint
        fixed_values: Tuple of fixed integer values that must be avoided

    Returns:
        A propagator function that enforces all_different constraint
    """
    # Convert to set for O(1) membership testing
    var_id_set = set(var_ids)

    def all_different_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Enforce all_different constraint via value elimination.

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        # Gather current state
        singletons = set(fixed_values)
        domains = {}

        # Check for duplicated variables in constraint (X appears twice)
        seen_roots = set()
        for vid in var_ids:
            # Deref to get root variable
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                if root in seen_roots:
                    # Same variable appears twice - immediate failure
                    return ("fail", None)
                seen_roots.add(root)
            elif deref[0] == "BOUND":
                # Variable is bound to a value
                from prolog.ast.terms import Int

                val = deref[2]  # Bound term is at index 2
                if isinstance(val, Int):
                    if val.value in singletons:
                        return ("fail", None)  # Duplicate value
                    singletons.add(val.value)
                # If bound to non-int (shouldn't happen with FD vars), it's handled elsewhere

        # Get domains for unbound variables
        for vid in var_ids:
            deref = store.deref(vid)
            if deref[0] == "UNBOUND":
                root = deref[1]
                dom = get_domain(store, root)
                if dom is None:
                    continue  # No domain yet
                domains[root] = dom

                if dom.is_singleton():
                    val = dom.min()
                    if val in singletons:
                        return ("fail", None)  # Duplicate singleton
                    singletons.add(val)

        # Value elimination: remove singletons from other domains
        changed = []
        for vid, dom in domains.items():
            if dom.is_singleton():
                continue

            new_dom = dom
            for val in singletons:
                if new_dom.contains(val):
                    new_dom = new_dom.remove_value(val)

            if new_dom.is_empty():
                return ("fail", None)

            if new_dom.rev != dom.rev:
                set_domain(store, vid, new_dom, trail)
                changed.append(vid)

        # Check if two variables were aliased (unified) - they must be different
        # This is detected above when checking for duplicate roots

        return ("ok", changed if changed else None)

    return all_different_propagator
