"""Reification propagator for CLP(FD) constraints."""

from typing import Tuple, Optional, List, Callable

from prolog.clpfd.boolean import get_boolean_value, ensure_boolean_var
from prolog.clpfd.entailment import Entailment
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.domain import Domain


def create_reification_propagator(
    b_id: int,
    constraint_type: str,
    constraint_args: tuple,
    check_entailment: Callable,
    post_constraint: Callable,
    post_negation: Callable,
):
    """Create a reification propagator for B #<==> Constraint.

    Args:
        b_id: Boolean variable ID
        constraint_type: Type of constraint being reified
        constraint_args: Arguments to the constraint
        check_entailment: Function to check constraint entailment
        post_constraint: Function to post the constraint
        post_negation: Function to post constraint negation

    Returns:
        Propagator function
    """
    # Track if we've already posted to prevent loops
    posted_true = [False]
    posted_false = [False]

    def reification_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        """Propagate reification constraint B #<==> C.

        Three rules:
        1. If C is entailed, set B = 1
        2. If C is dis-entailed, set B = 0
        3. If B is determined:
           - B = 1: post C
           - B = 0: post ¬C
        """
        changed = []

        # Ensure B has Boolean domain
        if not ensure_boolean_var(store, b_id, trail):
            return ("fail", None)

        # Check current value of B
        b_value = get_boolean_value(store, b_id)

        # Check entailment of the constraint
        entailment = check_entailment(store, *constraint_args)

        # Rule 1 & 2: Update B based on constraint entailment
        if entailment == Entailment.TRUE:
            # Constraint is satisfied - B must be 1
            b_dom = get_domain(store, b_id)
            if b_dom and not b_dom.contains(1):
                return ("fail", None)

            if b_value != 1:
                new_dom = Domain(((1, 1),))
                set_domain(store, b_id, new_dom, trail)
                changed.append(b_id)
                b_value = 1

        elif entailment == Entailment.FALSE:
            # Constraint is violated - B must be 0
            b_dom = get_domain(store, b_id)
            if b_dom and not b_dom.contains(0):
                return ("fail", None)

            if b_value != 0:
                new_dom = Domain(((0, 0),))
                set_domain(store, b_id, new_dom, trail)
                changed.append(b_id)
                b_value = 0

        # Rule 3: Post constraint based on B's value
        if b_value == 1 and not posted_true[0]:
            # B = 1: Post the constraint
            posted_true[0] = True
            success = post_constraint(engine, store, trail, *constraint_args)
            if not success:
                return ("fail", None)

        elif b_value == 0 and not posted_false[0]:
            # B = 0: Post the constraint's negation
            posted_false[0] = True
            success = post_negation(engine, store, trail, *constraint_args)
            if not success:
                return ("fail", None)

        return ("ok", changed if changed else None)

    return reification_propagator


def create_implication_propagator(
    b_id: int,
    constraint_type: str,
    constraint_args: tuple,
    check_entailment: Callable,
    post_constraint: Callable,
    forward: bool = True,
):
    """Create propagator for B #==> C (forward) or B #<== C (backward).

    Forward (B #==> C): If B=1 then C must hold
    Backward (B #<== C): If C holds then B=1
    """
    posted = [False]

    def implication_propagator(
        store, trail, engine, cause
    ) -> Tuple[str, Optional[List[int]]]:
        changed = []

        # Ensure B has Boolean domain
        if not ensure_boolean_var(store, b_id, trail):
            return ("fail", None)

        b_value = get_boolean_value(store, b_id)
        entailment = check_entailment(store, *constraint_args)

        if forward:
            # B #==> C: B=1 implies C
            if b_value == 1 and not posted[0]:
                posted[0] = True
                success = post_constraint(engine, store, trail, *constraint_args)
                if not success:
                    return ("fail", None)

            # If C is false, B must be 0
            if entailment == Entailment.FALSE:
                if b_value != 0:
                    b_dom = get_domain(store, b_id)
                    if b_dom and not b_dom.contains(0):
                        return ("fail", None)
                    new_dom = Domain(((0, 0),))
                    set_domain(store, b_id, new_dom, trail)
                    changed.append(b_id)
        else:
            # B #<== C: C implies B=1
            if entailment == Entailment.TRUE:
                if b_value != 1:
                    b_dom = get_domain(store, b_id)
                    if b_dom and not b_dom.contains(1):
                        return ("fail", None)
                    new_dom = Domain(((1, 1),))
                    set_domain(store, b_id, new_dom, trail)
                    changed.append(b_id)

            # If B=0, C must be false (contrapositive)
            if b_value == 0 and not posted[0]:
                posted[0] = True
                # For backward implication with B=0, we can't directly
                # post ¬C in general. This is weaker than equivalence.
                # We just fail if C is already entailed
                if entailment == Entailment.TRUE:
                    return ("fail", None)

        return ("ok", changed if changed else None)

    return implication_propagator
