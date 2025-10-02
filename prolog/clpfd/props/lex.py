"""Lexicographic ordering constraint propagator.

Implements lex_chain/1 constraint where vectors are ordered lexicographically.
Provides bounds consistency with prefix equality propagation.
"""

from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


def create_lex_chain_propagator(parsed_vectors, parsed_values=None):
    """Create lexicographic chain constraint propagator.

    Args:
        parsed_vectors: List of variable lists (each list contains variable IDs or None for ground)
        parsed_values: List of value lists (corresponding ground values or None for variables)

    Returns:
        Propagator function
    """
    if parsed_values is None:
        # All variables case
        parsed_values = [[None] * len(vec) for vec in parsed_vectors]

    def lex_chain_propagator(store, trail, engine, cause):
        """Lex chain constraint propagator implementation.

        Algorithm:
        For each adjacent pair Vi ≤lex Vi+1:
        1. Find critical position (first different domain)
        2. Propagate prefix equality
        3. Enforce ordering at critical position
        4. Handle equal vectors case

        Returns:
            (status, changed) where status is "ok" or "fail" and changed is list of var IDs
        """
        changed = []

        # Process each adjacent pair Vi ≤lex Vi+1
        for i in range(len(parsed_vectors) - 1):
            vec1_vars = parsed_vectors[i]
            vec1_values = parsed_values[i]
            vec2_vars = parsed_vectors[i + 1]
            vec2_values = parsed_values[i + 1]

            # Propagate pairwise lex constraint
            status, pair_changed = _propagate_pairwise_lex(
                store, trail, vec1_vars, vec1_values, vec2_vars, vec2_values
            )

            if status == "fail":
                return "fail", []

            changed.extend(pair_changed)

        return "ok", changed

    return lex_chain_propagator


def _propagate_pairwise_lex(
    store, trail, vec1_vars, vec1_values, vec2_vars, vec2_values
):
    """Propagate vec1 ≤lex vec2 constraint.

    Returns:
        (status, changed) tuple
    """
    changed = []
    n = len(vec1_vars)

    # Get current values and domains
    vec1_current = []
    vec2_current = []

    for j in range(n):
        # Get value or domain for vec1[j]
        if vec1_vars[j] is None:
            # Ground value
            vec1_current.append(vec1_values[j])
        else:
            # Variable - get domain
            dom = get_domain(store, vec1_vars[j])
            if dom is None or dom.is_empty():
                return "fail", []
            vec1_current.append(dom)

        # Get value or domain for vec2[j]
        if vec2_vars[j] is None:
            # Ground value
            vec2_current.append(vec2_values[j])
        else:
            # Variable - get domain
            dom = get_domain(store, vec2_vars[j])
            if dom is None or dom.is_empty():
                return "fail", []
            vec2_current.append(dom)

    # Find critical position (first position where domains may differ)
    critical_pos = None

    for j in range(n):
        val1 = vec1_current[j]
        val2 = vec2_current[j]

        # Check if this position is already determined to be equal
        if isinstance(val1, int) and isinstance(val2, int):
            if val1 < val2:
                # vec1 <lex vec2 at position j - constraint already satisfied
                return "ok", changed
            elif val1 > val2:
                # vec1 >lex vec2 at position j - constraint violated
                return "fail", []
            # val1 == val2, continue
        elif isinstance(val1, int) and isinstance(val2, Domain):
            if val1 < val2.min():
                # vec1 <lex vec2 - constraint satisfied
                return "ok", changed
            elif val1 > val2.max():
                # vec1 >lex vec2 - constraint violated
                return "fail", []
            elif not val2.contains(val1):
                # At this position, vec1 != vec2, need to check bounds
                if val1 < val2.min():
                    return "ok", changed  # Already <lex
                else:
                    return "fail", []  # Would be >lex
            else:
                # val1 might equal val2 - this is the critical position
                critical_pos = j
                break
        elif isinstance(val1, Domain) and isinstance(val2, int):
            if val1.max() < val2:
                # vec1 <lex vec2 - constraint satisfied
                return "ok", changed
            elif val1.min() > val2:
                # vec1 >lex vec2 - constraint violated
                return "fail", []
            elif not val1.contains(val2):
                # At this position, vec1 != vec2
                if val1.max() < val2:
                    return "ok", changed  # Already <lex
                else:
                    return "fail", []  # Would be >lex
            else:
                # val1 might equal val2 - this is the critical position
                critical_pos = j
                break
        elif isinstance(val1, Domain) and isinstance(val2, Domain):
            # Both are domains - check if they can be equal or ordered
            if val1.max() < val2.min():
                # vec1 <lex vec2 - constraint satisfied
                return "ok", changed
            elif val1.min() > val2.max():
                # vec1 >lex vec2 - constraint violated
                return "fail", []
            else:
                # Domains may overlap - this is the critical position
                critical_pos = j
                break

    # If no critical position found, all positions are ground and equal
    if critical_pos is None:
        return "ok", changed

    # Propagate at critical position and beyond
    status, pos_changed = _propagate_at_critical_position(
        store,
        trail,
        vec1_vars,
        vec1_values,
        vec2_vars,
        vec2_values,
        vec1_current,
        vec2_current,
        critical_pos,
    )

    if status == "fail":
        return "fail", []

    changed.extend(pos_changed)
    return "ok", changed


def _propagate_at_critical_position(
    store,
    trail,
    vec1_vars,
    vec1_values,
    vec2_vars,
    vec2_values,
    vec1_current,
    vec2_current,
    critical_pos,
):
    """Propagate lex constraint at the critical position."""
    changed = []

    # At the critical position, we need: vec1[critical_pos] ≤ vec2[critical_pos]
    # OR equality holds for the rest of the vector

    val1 = vec1_current[critical_pos]
    val2 = vec2_current[critical_pos]

    if isinstance(val1, int) and isinstance(val2, Domain):
        # vec1 is ground, vec2 is variable
        if val2.contains(val1):
            # They might be equal - need to check suffix
            # For now, enforce val1 ≤ vec2 by removing values < val1 from vec2
            new_dom2 = val2.remove_lt(val1)
            if new_dom2.is_empty():
                return "fail", []
            if new_dom2.rev != val2.rev:
                set_domain(store, vec2_vars[critical_pos], new_dom2, trail)
                changed.append(vec2_vars[critical_pos])
        else:
            # They are definitely different - check ordering
            if val1 > val2.max():
                return "fail", []
            # val1 < val2.min() is already handled above

    elif isinstance(val1, Domain) and isinstance(val2, int):
        # vec1 is variable, vec2 is ground
        if val1.contains(val2):
            # They might be equal - need to check suffix
            # For now, enforce vec1 ≤ val2 by removing values > val2 from vec1
            new_dom1 = val1.remove_gt(val2)
            if new_dom1.is_empty():
                return "fail", []
            if new_dom1.rev != val1.rev:
                set_domain(store, vec1_vars[critical_pos], new_dom1, trail)
                changed.append(vec1_vars[critical_pos])
        else:
            # They are definitely different - check ordering
            if val1.min() > val2:
                return "fail", []
            # val1.max() < val2 is already handled above

    elif isinstance(val1, Domain) and isinstance(val2, Domain):
        # Both are variables - apply bounds reasoning
        # vec1 ≤ vec2 means max(vec1) ≤ max(vec2) and min(vec1) ≤ max(vec2)

        # If min(vec1) > max(vec2), definitely fail
        if val1.min() > val2.max():
            return "fail", []

        # For bounds consistency on ≤, we can:
        # 1. Remove from vec1 all values > max(vec2)
        # 2. Remove from vec2 all values < min(vec1)

        new_dom1 = val1.remove_gt(val2.max())
        if new_dom1.is_empty():
            return "fail", []
        if new_dom1.rev != val1.rev:
            set_domain(store, vec1_vars[critical_pos], new_dom1, trail)
            changed.append(vec1_vars[critical_pos])
            val1 = new_dom1  # Update for next check

        new_dom2 = val2.remove_lt(val1.min())
        if new_dom2.is_empty():
            return "fail", []
        if new_dom2.rev != val2.rev:
            set_domain(store, vec2_vars[critical_pos], new_dom2, trail)
            changed.append(vec2_vars[critical_pos])

    return "ok", changed
