"""Register allocation for WAM compilation.

Assigns X (temporary) and Y (permanent) register indices to variables
based on liveness analysis results.

Register allocation strategy:
- Temporary variables → X registers (not preserved across calls)
- Permanent variables → Y registers (preserved in environment frame)
- Y registers assigned in stable, deterministic order (sorted by var ID)
- X registers can be reused across goals

Key insight: Y register ordering must be stable for:
1. Deterministic code generation
2. Debuggability (same code for same source)
3. Future optimizations (environment trimming based on index)
"""

from typing import Dict, Set, Tuple
from prolog.ast.clauses import Clause


def allocate_registers(
    clause: Clause, temp_vars: Set[int], perm_vars: Set[int]
) -> Dict[int, Tuple[str, int]]:
    """Assign register indices to variables.

    Y registers are assigned to permanent variables in sorted order by var ID.
    This ensures stable, deterministic allocation across runs.

    X registers are assigned to temporary variables. For now, we use a simple
    scheme where each temp var gets its own X register. Future optimization:
    X register reuse across goals.

    Args:
        clause: AST Clause node (for future per-goal allocation)
        temp_vars: Set of temporary variable IDs
        perm_vars: Set of permanent variable IDs

    Returns:
        register_map: {var_id: ("X", index) | ("Y", index)}

    Examples:
        p(X) :- q(X), r(X).
        -> {1: ("Y", 0)}  # X is permanent, gets Y0

        p(X, Y) :- q(X), r(Y).
        -> {1: ("X", 0), 2: ("Y", 0)}  # X temporary, Y permanent
    """
    register_map = {}

    # Assign Y registers to permanent vars in sorted order
    # Sorted by var ID for stable, deterministic allocation
    for i, var_id in enumerate(sorted(perm_vars)):
        register_map[var_id] = ("Y", i)

    # Assign X registers to temporary vars in sorted order
    # For now, simple allocation: each temp var gets unique X register
    # Future: X register reuse per-goal for optimization
    for i, var_id in enumerate(sorted(temp_vars)):
        register_map[var_id] = ("X", i)

    return register_map


def debug_registers(clause: Clause, register_map: Dict[int, Tuple[str, int]]) -> None:
    """Pretty-print register allocation.

    Args:
        clause: The clause being allocated
        register_map: {var_id: ("X"|"Y", index)}
    """
    print(f"Clause: {clause.head}")
    if not register_map:
        print("  (no variables)")
        return

    # Group by register type
    x_regs = {vid: idx for vid, (rtype, idx) in register_map.items() if rtype == "X"}
    y_regs = {vid: idx for vid, (rtype, idx) in register_map.items() if rtype == "Y"}

    if y_regs:
        print("  Permanent (Y):")
        for var_id in sorted(y_regs.keys()):
            print(f"    var_{var_id} -> Y{y_regs[var_id]}")

    if x_regs:
        print("  Temporary (X):")
        for var_id in sorted(x_regs.keys()):
            print(f"    var_{var_id} -> X{x_regs[var_id]}")


__all__ = ["allocate_registers", "debug_registers"]
