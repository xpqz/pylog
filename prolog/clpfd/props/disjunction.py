"""Disjunction propagator for CLP(FD) constraints.

Implements A #\\/ B constraint propagation where A and B are constraints.
The disjunction is satisfied if at least one of A or B is satisfied.
"""

from typing import Optional, List, Tuple, Callable
from prolog.clpfd.entailment import Entailment


def create_disjunction_propagator(
    a_type: str,
    a_args: tuple,
    b_type: str,
    b_args: tuple,
    check_entailment_a: Callable[..., Entailment],
    check_entailment_b: Callable[..., Entailment],
    post_constraint_a: Callable[..., bool],
    post_constraint_b: Callable[..., bool],
):
    """Create a disjunction propagator for A #\\/ B.

    Args:
        a_type: Type of constraint A
        a_args: Arguments to constraint A
        b_type: Type of constraint B
        b_args: Arguments to constraint B
        check_entailment_a: Function to check entailment of A
        check_entailment_b: Function to check entailment of B
        post_constraint_a: Function to post constraint A
        post_constraint_b: Function to post constraint B

    Returns:
        Propagator function that enforces A #\/ B
    """

    def disjunction_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Propagate A #\\/ B.

        The disjunction A #\\/ B is:
        - Satisfied if either A or B (or both) are entailed
        - Failed if both A and B are dis-entailed
        - Unknown otherwise

        Propagation rules:
        1. If A is dis-entailed, post B
        2. If B is dis-entailed, post A
        3. If either A or B is entailed, the disjunction is satisfied
        """
        changed: List[int] = []

        # Check entailment status of both constraints
        entailment_a = check_entailment_a(store, *a_args)
        entailment_b = check_entailment_b(store, *b_args)

        # Rule 3: If either constraint is entailed, disjunction is satisfied
        if entailment_a == Entailment.TRUE or entailment_b == Entailment.TRUE:
            return ("ok", None)

        # Rule: If both constraints are dis-entailed, disjunction fails
        if entailment_a == Entailment.FALSE and entailment_b == Entailment.FALSE:
            return ("fail", None)

        # Rule 1: If A is dis-entailed but B is not, post B
        if entailment_a == Entailment.FALSE and entailment_b == Entailment.UNKNOWN:
            if not post_constraint_b(engine, store, trail, *b_args):
                return ("fail", None)
            # Constraint B was posted, so all its variables may have changed
            changed.extend(b_args)

        # Rule 2: If B is dis-entailed but A is not, post A
        elif entailment_b == Entailment.FALSE and entailment_a == Entailment.UNKNOWN:
            if not post_constraint_a(engine, store, trail, *a_args):
                return ("fail", None)
            # Constraint A was posted, so all its variables may have changed
            changed.extend(a_args)

        # Remove duplicates and filter to only variable IDs
        if changed:
            unique_changed = []
            seen = set()
            for var_id in changed:
                if isinstance(var_id, int) and var_id not in seen:
                    unique_changed.append(var_id)
                    seen.add(var_id)
            return ("ok", unique_changed if unique_changed else None)

        return ("ok", None)

    return disjunction_propagator
