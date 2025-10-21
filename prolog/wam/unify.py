"""WAM unification operations: deref, bind, and trail.

This module provides core unification support:
- Dereferencing: Follow REF chains to find root cells
- Binding: Bind variables with proper trailing
- Trailing: Record bindings for backtracking

Phase 1 Note:
In Phase 1, HB is always 0, so trailing criterion (addr < HB) never fires.
However, the API and TR increments must be correct for reuse in Phase 2.
"""

from prolog.wam.heap import (
    TAG_REF,
    is_con,
    is_list,
    is_ref,
    is_str,
    make_ref,
)

__all__ = [
    "deref",
    "bind",
    "trail_if_needed",
    "untrail",
    "unify",
    "occurs",
]


def deref(machine, addr: int) -> int:
    """Follow REF chains to root cell.

    Iteratively follows REF cells until reaching either:
    - A non-REF cell (STR, CON, LIST)
    - A self-referential REF (unbound variable)

    Args:
        machine: Machine instance with heap
        addr: Starting heap address

    Returns:
        Address of root cell (non-REF or unbound REF)

    Example:
        # heap[0] = (REF, 1)
        # heap[1] = (REF, 2)
        # heap[2] = (CON, 42)
        deref(m, 0) -> 2  # Follows chain to constant

    Note:
        Future optimization: Path compression during deref could materially
        improve performance for unify-heavy workloads with long-lived chains.
        However, compression requires trailing each shortened REF, so defer
        until profiling shows deref dominates execution time.
    """
    while True:
        cell = machine.heap[addr]
        tag = cell[0]

        # Non-REF cell: stop
        if tag != TAG_REF:
            return addr

        target = cell[1]

        # Self-referential REF (unbound): stop
        if target == addr:
            return addr

        # Follow chain
        addr = target


def trail_if_needed(machine, addr: int) -> None:
    """Trail address if below heap backtrack boundary.

    Records address in trail if it's below HB. In Phase 1, HB is always 0,
    so this criterion never fires. In later phases, this ensures old bindings
    are recorded for backtracking.

    Args:
        machine: Machine instance with trail, TR, and HB
        addr: Heap address to potentially trail

    Criterion:
        Trail if addr < machine.HB
    """
    if addr < machine.HB:
        machine.trail.append(addr)
        machine.TR += 1


def bind(machine, ref_addr: int, value_addr: int) -> bool:
    """Bind REF cell to another cell with trailing and optional occurs-check.

    Binds a reference variable to a value. Both addresses are dereferenced
    first. If they resolve to the same cell, binding is a no-op.

    The binding direction is chosen to ensure a REF cell points to the value.
    If both are REFs (unbound variables), prefer binding newer to older
    (optional heuristic).

    Trailing occurs if the bound address is below HB (addr < HB).

    If machine.occurs_check_enabled is True, performs occurs-check before
    binding to detect cycles. Returns False if cycle detected.

    Args:
        machine: Machine instance with heap, trail, TR, HB, occurs_check_enabled
        ref_addr: Address of REF cell to bind (will be dereferenced)
        value_addr: Address of value to bind to (will be dereferenced)

    Returns:
        True if binding succeeds, False if occurs-check fails

    Example:
        # Bind unbound variable to constant
        # heap[0] = (REF, 0)  # Unbound
        # heap[1] = (CON, 42)
        bind(m, 0, 1)  # True
        # Result: heap[0] = (REF, 1)

        # Occurs-check failure
        # heap[0] = (REF, 0)  # X
        # heap[1] = (STR, 2)  # f(X)
        # heap[2] = (CON, ("f", 1))
        # heap[3] = (REF, 0)  # X argument
        m.occurs_check_enabled = True
        bind(m, 0, 1)  # False - cycle detected
    """
    # Deref both addresses
    ref_addr = deref(machine, ref_addr)
    value_addr = deref(machine, value_addr)

    # Already bound to same cell: no-op (success)
    if ref_addr == value_addr:
        return True

    # Ensure ref_addr points to a REF cell
    # If both are REFs, prefer binding newer to older
    ref_cell = machine.heap[ref_addr]
    value_cell = machine.heap[value_addr]

    if ref_cell[0] != TAG_REF:
        # ref_addr is not REF, swap if value_addr is REF
        if value_cell[0] == TAG_REF:
            ref_addr, value_addr = value_addr, ref_addr
        else:
            # Neither is REF after deref: binding two non-variables is a no-op.
            # This case should not occur in valid usage where bind() is called
            # with at least one unbound variable. We treat it as a silent no-op
            # to avoid crashing on degenerate inputs, but proper usage should
            # check types before calling bind().
            return True
    elif value_cell[0] == TAG_REF:
        # Both are REFs: prefer binding newer to older (ref_addr > value_addr)
        if ref_addr < value_addr:
            ref_addr, value_addr = value_addr, ref_addr

    # Occurs-check if enabled
    if machine.occurs_check_enabled:
        if occurs(machine, ref_addr, value_addr):
            # Cycle detected: fail without binding
            return False

    # Trail if binding old variable (below HB)
    trail_if_needed(machine, ref_addr)

    # Perform binding: make ref_addr point to value_addr
    machine.heap[ref_addr] = make_ref(value_addr)
    return True


def untrail(machine, target_TR: int) -> None:
    """Restore trailed bindings to unbound state.

    Iterates through trail from target_TR to current TR, restoring each
    trailed address to a self-referential REF (unbound state).

    Used during backtracking to undo bindings made after a choicepoint.

    Args:
        machine: Machine instance with heap, trail, TR
        target_TR: Trail pointer to restore to (typically from choicepoint)

    Example:
        # Before: trail = [5, 7], TR = 2
        # heap[5] = (REF, 10), heap[7] = (REF, 12)
        untrail(m, 0)
        # After: heap[5] = (REF, 5), heap[7] = (REF, 7), TR = 0
    """
    # Iterate from current TR down to target_TR
    while machine.TR > target_TR:
        machine.TR -= 1
        addr = machine.trail[machine.TR]

        # Restore to self-referential REF (unbound)
        machine.heap[addr] = make_ref(addr)


def occurs(machine, var_addr: int, term_addr: int) -> bool:
    """Check if variable occurs in term (occurs-check for cycle detection).

    Performs iterative occurs-check using explicit stack and visited set.
    Detects cycles where var_addr would appear inside term_addr after binding.

    Args:
        machine: Machine instance with heap
        var_addr: Heap address of variable to check (dereferenced REF root)
        term_addr: Heap address of term to search (will be dereferenced)

    Returns:
        True if var_addr occurs in term_addr, False otherwise

    Examples:
        # X occurs in f(X)
        x_addr = new_ref(m)
        f_addr = new_str(m, "f", 1)
        note_struct_args(m, x_addr)
        occurs(m, x_addr, f_addr)  # True - cycle detected

        # X does not occur in f(a)
        x_addr = new_ref(m)
        f_addr = new_str(m, "f", 1)
        a_addr = new_con(m, "a")
        note_struct_args(m, a_addr)
        occurs(m, x_addr, f_addr)  # False - no cycle

    Algorithm:
        1. Deref both addresses
        2. If var_addr == term_addr: return True (immediate cycle)
        3. Use explicit stack to traverse term structure
        4. Track visited addresses to prevent infinite loops
        5. For each cell type:
           - REF: Compare root addresses
           - STR: Read arity from functor, push argument cells
           - LIST: Push head and tail cells
           - CON: Return False (constants don't contain variables)
    """
    # Deref both addresses to roots
    var_root = deref(machine, var_addr)
    term_root = deref(machine, term_addr)

    # Immediate cycle: variable occurs directly
    if var_root == term_root:
        return True

    # Explicit stack for iterative traversal
    stack = [term_root]
    visited = set()

    while stack:
        addr = stack.pop()

        # Skip addresses beyond current heap (uninitialized structure arguments)
        if addr >= len(machine.heap):
            continue

        # Deref current address
        addr = deref(machine, addr)

        # Skip already-visited addresses (prevent loops)
        if addr in visited:
            continue
        visited.add(addr)

        # Check if we've reached the variable
        if addr == var_root:
            return True

        # Examine cell type and push subterms
        cell = machine.heap[addr]
        tag = cell[0]

        if tag == TAG_REF:
            # REF: Already dereferenced, check root
            # (Covered by deref above, but be explicit)
            if addr == var_root:
                return True

        elif tag == 1:  # TAG_STR
            # Structure: read arity from functor cell and push args
            functor_addr = cell[1]

            # Guard against functor beyond heap bounds
            if functor_addr >= len(machine.heap):
                continue

            functor_cell = machine.heap[functor_addr]
            # functor_cell format: (TAG_CON, (name, arity))
            name, arity = functor_cell[1]

            # Push argument cells (functor_addr + 1 through functor_addr + arity)
            # Note: arguments may not exist yet if structure is being built
            for i in range(arity):
                arg_addr = functor_addr + 1 + i
                # Will be filtered by heap bounds check at top of loop
                stack.append(arg_addr)

        elif tag == 3:  # TAG_LIST
            # List: push head and tail
            head_addr = cell[1]
            tail_addr = cell[2]
            stack.append(head_addr)
            stack.append(tail_addr)

        elif tag == 2:  # TAG_CON
            # Constant: no subterms, cannot contain variable
            continue

    # Variable not found in term
    return False


def unify(machine, addr_a: int, addr_b: int) -> bool:
    """Unify two heap addresses with explicit stack.

    Implements the classic WAM unification algorithm using an explicit
    work stack to avoid Python recursion. Handles all cell type combinations:
    REF, CON, STR, and LIST.

    Algorithm:
    1. Deref both addresses and push to stack
    2. While stack is not empty:
       - Pop (u, v) pair
       - If same address: skip (already unified)
       - If either is REF: bind them
       - If both CON: compare values
       - If both STR: check functor, push argument pairs
       - If both LIST: push head and tail pairs
       - Type mismatch: fail

    Args:
        machine: Machine instance with heap
        addr_a: First heap address
        addr_b: Second heap address

    Returns:
        True if unification succeeds, False otherwise

    Examples:
        # Unify variable with constant
        var = new_ref(m)
        const = new_con(m, 42)
        unify(m, var, const)  # True, var now bound to const

        # Unify structures f(a, X) with f(a, b)
        str1 = new_str(m, "f", 2)
        new_con(m, "a")
        new_ref(m)
        str2 = new_str(m, "f", 2)
        new_con(m, "a")
        new_con(m, "b")
        unify(m, str1, str2)  # True, X bound to b

        # Fail on type mismatch
        atom = new_con(m, "foo")
        num = new_con(m, 42)
        unify(m, atom, num)  # False
    """
    # Deref both addresses
    a = deref(machine, addr_a)
    b = deref(machine, addr_b)

    # Work stack for iterative unification
    stack = [(a, b)]

    while stack:
        u, v = stack.pop()

        # Same cell: already unified
        if u == v:
            continue

        # Deref in case bindings occurred during processing
        u = deref(machine, u)
        v = deref(machine, v)

        if u == v:
            continue

        cell_u = machine.heap[u]
        cell_v = machine.heap[v]

        # At least one is REF: bind them
        if is_ref(cell_u) or is_ref(cell_v):
            if not bind(machine, u, v):
                # Binding failed (occurs-check)
                return False
            continue

        # Both CON: must be equal
        if is_con(cell_u) and is_con(cell_v):
            if cell_u[1] != cell_v[1]:  # Compare values
                return False
            continue

        # Both STR: functors must match, unify args
        if is_str(cell_u) and is_str(cell_v):
            functor_addr_u = cell_u[1]
            functor_addr_v = cell_v[1]
            functor_u = machine.heap[functor_addr_u]
            functor_v = machine.heap[functor_addr_v]

            # Functors must be identical (name and arity)
            if functor_u != functor_v:
                return False

            # Push argument pairs to stack
            # Arguments are at functor_addr + 1 through functor_addr + arity
            arity = functor_u[1][1]
            for i in range(arity):
                arg_addr_u = functor_addr_u + 1 + i
                arg_addr_v = functor_addr_v + 1 + i
                stack.append((arg_addr_u, arg_addr_v))
            continue

        # Both LIST: unify head and tail
        if is_list(cell_u) and is_list(cell_v):
            head_addr_u = cell_u[1]
            tail_addr_u = cell_u[2]
            head_addr_v = cell_v[1]
            tail_addr_v = cell_v[2]

            # Push head and tail pairs
            stack.append((head_addr_u, head_addr_v))
            stack.append((tail_addr_u, tail_addr_v))
            continue

        # Type mismatch
        return False

    return True
