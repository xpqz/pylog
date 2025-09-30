"""Reification propagator for CLP(FD) constraints."""

from typing import Tuple, Optional, List, Callable, Protocol, Any

from prolog.ast.terms import Int
from prolog.clpfd.boolean import get_boolean_value, ensure_boolean_var
from prolog.clpfd.entailment import Entailment
from prolog.clpfd.api import get_domain, set_domain, get_fd_attrs
from prolog.clpfd.domain import Domain
from prolog.unify.unify_helpers import bind_root_to_term


class Store(Protocol):
    """Protocol for variable store operations."""

    def deref(self, varid: int) -> Tuple[str, int, Any]: ...


class Trail(Protocol):
    """Protocol for trail operations."""

    def push(self, entry: tuple) -> None: ...


class Engine(Protocol):
    """Protocol for engine operations."""

    pass  # Engine protocol methods if needed


# Type alias for propagator function signature
Propagator = Callable[
    [Store, Trail, Engine, Optional[int]], Tuple[str, Optional[List[int]]]
]


def create_reification_propagator(
    b_id: int,
    constraint_type: str,
    constraint_args: tuple,
    check_entailment: Callable[[Store, ...], Entailment],
    post_constraint: Callable[[Engine, Store, Trail, ...], bool],
    post_negation: Callable[[Engine, Store, Trail, ...], bool],
) -> Propagator:
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
    # No persistent state needed - idempotent posting is handled via
    # attributes on the Boolean variable that are trailed

    def reification_propagator(
        store: Store, trail: Trail, engine: Engine, cause: Optional[int]
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
            if b_dom is None:
                deref = store.deref(b_id)
                if deref[0] == "BOUND" and isinstance(deref[2], Int):
                    if deref[2].value != 1:
                        return ("fail", None)

            if b_value != 1:
                new_dom = Domain(((1, 1),))
                set_domain(store, b_id, new_dom, trail)
                queue = getattr(engine, "clpfd_queue", None)
                if queue is None or getattr(queue, "running", None) is None:
                    bind_root_to_term(b_id, Int(1), trail, store)
                changed.append(b_id)
                b_value = 1

        elif entailment == Entailment.FALSE:
            # Constraint is violated - B must be 0
            b_dom = get_domain(store, b_id)
            if b_dom and not b_dom.contains(0):
                return ("fail", None)
            if b_dom is None:
                deref = store.deref(b_id)
                if deref[0] == "BOUND" and isinstance(deref[2], Int):
                    if deref[2].value != 0:
                        return ("fail", None)

            if b_value != 0:
                new_dom = Domain(((0, 0),))
                set_domain(store, b_id, new_dom, trail)
                queue = getattr(engine, "clpfd_queue", None)
                if queue is None or getattr(queue, "running", None) is None:
                    bind_root_to_term(b_id, Int(0), trail, store)
                changed.append(b_id)
                b_value = 0

        # Rule 3: Post constraint based on B's value
        # Only post if entailment is UNKNOWN (not already determined)
        if entailment == Entailment.UNKNOWN:
            # Get or create reification posting tracking on the Boolean variable
            # IMPORTANT: Never mutate the stored attrs dict in-place.
            # Always copy before updating so backtracking can restore state.
            current_attrs = get_fd_attrs(store, b_id) or {}
            attrs = dict(current_attrs) if current_attrs else {}
            reif_posts = set(attrs.get("reif_posts", set()))

            # Create a stable key for what we're about to post
            # Args are already normalized from the builtin:
            # - (None, value) for integer constants
            # - var_id for variables
            # No further normalization needed
            normalized_args = constraint_args

            if b_value == 1:
                # B = 1: Post the constraint if not already posted
                post_key = ("reif", b_id, constraint_type, normalized_args, "C")
                if post_key not in reif_posts:
                    success = post_constraint(engine, store, trail, *constraint_args)
                    if not success:
                        return ("fail", None)
                    # Record that we posted this constraint
                    new_posts = reif_posts | {post_key}
                    attrs["reif_posts"] = new_posts
                    store.put_attr(b_id, "clpfd", attrs, trail)

            elif b_value == 0:
                # B = 0: Post the constraint's negation if not already posted
                post_key = ("reif", b_id, constraint_type, normalized_args, "¬C")
                if post_key not in reif_posts:
                    success = post_negation(engine, store, trail, *constraint_args)
                    if not success:
                        return ("fail", None)
                    # Record that we posted this negation
                    new_posts = reif_posts | {post_key}
                    attrs["reif_posts"] = new_posts
                    store.put_attr(b_id, "clpfd", attrs, trail)

        return ("ok", changed if changed else None)

    return reification_propagator


def create_implication_propagator(
    b_id: int,
    constraint_type: str,
    constraint_args: tuple,
    check_entailment: Callable[[Store, ...], Entailment],
    post_constraint: Callable[[Engine, Store, Trail, ...], bool],
    forward: bool = True,
) -> Propagator:
    """Create propagator for B #==> C (forward) or B #<== C (backward).

    Forward (B #==> C): If B=1 then C must hold
    Backward (B #<== C): If C holds then B=1
    """
    # No persistent state needed - idempotent posting is handled via
    # attributes on the Boolean variable that are trailed

    def implication_propagator(
        store: Store, trail: Trail, engine: Engine, cause: Optional[int]
    ) -> Tuple[str, Optional[List[int]]]:
        changed = []

        # Ensure B has Boolean domain
        if not ensure_boolean_var(store, b_id, trail):
            return ("fail", None)

        b_value = get_boolean_value(store, b_id)
        entailment = check_entailment(store, *constraint_args)

        if forward:
            # B #==> C: B=1 implies C
            # Only post constraint if entailment is unknown
            if b_value == 1 and entailment == Entailment.UNKNOWN:
                # Get or create implication posting tracking on the Boolean variable
                # Copy attrs before updating to ensure proper trailing
                current_attrs = get_fd_attrs(store, b_id) or {}
                attrs = dict(current_attrs) if current_attrs else {}
                impl_posts = set(attrs.get("impl_posts", set()))

                # Create a stable key for what we're about to post
                # Args already normalized - no change needed
                normalized_args = constraint_args
                post_key = ("impl_forward", b_id, constraint_type, normalized_args, "C")

                if post_key not in impl_posts:
                    success = post_constraint(engine, store, trail, *constraint_args)
                    if not success:
                        return ("fail", None)
                    # Record that we posted this implication
                    new_posts = impl_posts | {post_key}
                    attrs["impl_posts"] = new_posts
                    store.put_attr(b_id, "clpfd", attrs, trail)

            # If C is false, B must be 0
            if entailment == Entailment.FALSE:
                if b_value != 0:
                    b_dom = get_domain(store, b_id)
                    if b_dom and not b_dom.contains(0):
                        return ("fail", None)
                    if b_dom is None:
                        deref = store.deref(b_id)
                        if deref[0] == "BOUND" and isinstance(deref[2], Int):
                            if deref[2].value != 0:
                                return ("fail", None)
                    new_dom = Domain(((0, 0),))
                    set_domain(store, b_id, new_dom, trail)
                    queue = getattr(engine, "clpfd_queue", None)
                    if queue is None or getattr(queue, "running", None) is None:
                        bind_root_to_term(b_id, Int(0), trail, store)
                    changed.append(b_id)
        else:
            # B #<== C: C implies B=1
            if entailment == Entailment.TRUE:
                if b_value != 1:
                    b_dom = get_domain(store, b_id)
                    if b_dom and not b_dom.contains(1):
                        return ("fail", None)
                    if b_dom is None:
                        deref = store.deref(b_id)
                        if deref[0] == "BOUND" and isinstance(deref[2], Int):
                            if deref[2].value != 1:
                                return ("fail", None)
                    new_dom = Domain(((1, 1),))
                    set_domain(store, b_id, new_dom, trail)
                    queue = getattr(engine, "clpfd_queue", None)
                    if queue is None or getattr(queue, "running", None) is None:
                        bind_root_to_term(b_id, Int(1), trail, store)
                    changed.append(b_id)

            # If B=0, C must be false (contrapositive)
            if b_value == 0:
                # For backward implication with B=0, we can't directly
                # post ¬C in general. This is weaker than equivalence.
                # We just fail if C is already entailed
                if entailment == Entailment.TRUE:
                    return ("fail", None)

        return ("ok", changed if changed else None)

    return implication_propagator
