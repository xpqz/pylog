"""All-different global constraint propagator."""

from typing import Tuple, Optional, List, Set, Dict
from prolog.clpfd.api import get_domain, set_domain


def create_all_different_propagator(var_ids: List[int], fixed_values: Tuple[int, ...]):
    """Create all_different propagator with value elimination and Hall-interval pruning.

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
        """Enforce all_different constraint via value elimination and Hall-interval pruning.

        Returns:
            ('ok', changed_vars) if propagation succeeds
            ('fail', None) if constraint is violated
        """
        # Gather current state
        singletons = set(fixed_values)
        domains: Dict[int, 'Domain'] = {}

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
                domains[vid] = new_dom  # Update for Hall-interval phase

        # Hall-interval pruning - OPTIMIZED
        # Only do expensive Hall pruning if we have a reasonable number of variables
        # and haven't already done significant pruning via value elimination
        if domains and len(domains) <= 20:  # Limit complexity for larger problems
            # Compute candidate intervals from unique bounds
            bounds = set()
            for dom in domains.values():
                if not dom.is_empty():
                    bounds.add(dom.min())
                    bounds.add(dom.max())

            sorted_bounds = sorted(bounds)

            # Limit the number of intervals we check for performance
            # Only check intervals up to a reasonable size
            max_interval_size = min(len(domains), 10)  # Don't check huge intervals

            # Check each interval [a,b]
            for i, a in enumerate(sorted_bounds):
                for b in sorted_bounds[i:]:
                    if a > b:
                        continue

                    interval_size = b - a + 1

                    # Skip intervals that are too large
                    if interval_size > max_interval_size:
                        continue

                    # Count tight variables (domain ⊆ [a,b])
                    tight_vars = []
                    for vid, dom in domains.items():
                        if dom.min() >= a and dom.max() <= b:
                            tight_vars.append(vid)

                    tight_count = len(tight_vars)

                    # Pigeonhole check
                    if tight_count > interval_size:
                        return ("fail", None)

                    # Hall set found: remove [a,b] from non-tight vars
                    if tight_count == interval_size and tight_count > 0:
                        for vid, dom in domains.items():
                            if vid not in tight_vars:
                                new_dom = dom.remove_interval(a, b)
                                if new_dom.is_empty():
                                    return ("fail", None)
                                if new_dom.rev != dom.rev:
                                    set_domain(store, vid, new_dom, trail)
                                    if vid not in changed:
                                        changed.append(vid)
                                    domains[vid] = new_dom  # Update for next iterations

        return ("ok", changed if changed else None)

    return all_different_propagator
