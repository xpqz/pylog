"""Exception handling builtin predicates.

This module contains builtin predicates for exception handling:
- throw/1 - throw an exception
- catch/3 - catch exceptions and execute recovery
- handle_throw - helper function for exception handling

These predicates are extracted from engine.py as part of Phase 6 of the
engine refactoring plan.
"""

from typing import Dict, Tuple, Callable
from prolog.ast.terms import Term, Atom, Var, Struct
from prolog.engine.errors import PrologThrow
from prolog.engine.runtime import ChoicepointKind, Choicepoint, Frame, Goal, GoalType

__all__ = [
    "register",
    "builtin_throw",
    "builtin_catch",
    "handle_throw",
]


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register exception handling builtin predicates.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    registry[("throw", 1)] = builtin_throw
    registry[("catch", 3)] = builtin_catch


def builtin_throw(engine, args: tuple) -> bool:
    """throw(Ball) - throw an exception.

    Raises an exception with the given ball term. The exception
    propagates up the execution stack until caught by catch/3.
    """
    if len(args) != 1:
        return False

    ball = args[0]

    # Dereference the ball
    if isinstance(ball, Var):
        ball_result = engine.store.deref(ball.id)
        if ball_result[0] == "BOUND":
            ball = ball_result[2]
        else:
            # Stage-1 policy: require instantiated ball
            # ISO would allow throwing unbound variables
            return False  # Dev-mode: fail on unbound

    # Reify the ball to capture current bindings
    # This ensures that variable bindings are preserved in the thrown term
    reified_ball = engine._reify_term(ball)

    # Record metrics for exception thrown
    if engine.metrics:
        engine.metrics.record_exception_thrown()

    # Raise PrologThrow directly
    raise PrologThrow(reified_ball)


def builtin_catch(engine, args: tuple) -> bool:
    """catch(Goal, Catcher, Recovery) - catch exceptions.

    Executes Goal. If Goal throws an exception that unifies with
    Catcher, executes Recovery. Otherwise the exception propagates.
    """
    if len(args) != 3:
        return False

    goal_arg, catcher_arg, recovery_arg = args

    # Dereference the goal
    if isinstance(goal_arg, Var):
        goal_result = engine.store.deref(goal_arg.id)
        if goal_result[0] == "BOUND":
            goal = goal_result[2]
        else:
            # Unbound goal - insufficient instantiation
            return False  # Dev-mode: fail
    else:
        goal = goal_arg

    # Goal must be callable
    if not isinstance(goal, (Atom, Struct)):
        return False  # Dev-mode: fail on non-callable

    # Capture baseline heights BEFORE pushing anything
    # These represent the state at the call site of catch/3
    base_trail_top = engine.trail.position()
    base_goal_height = engine.goal_stack.height()
    base_frame_height = len(engine.frame_stack)
    base_cp_height = len(engine.cp_stack)

    # Check if there's a POP_FRAME sentinel on top of the goal stack
    # If so, we need to track that it might be consumed before we backtrack
    top_goal = engine.goal_stack.peek()
    has_pop_frame_sentinel = (
        top_goal is not None and top_goal.type == GoalType.POP_FRAME
    )

    # Count frames for the same predicate for recursion tracking
    recursion_depth = 0
    if engine.frame_stack:
        current_pred = engine.frame_stack[-1].pred
        for frame in engine.frame_stack:
            if frame.pred == current_pred:
                recursion_depth += 1

    # Snapshot active streaming cursors for restoration after exception
    cursor_snapshots = {}
    for cp in engine.cp_stack:
        if cp.kind == ChoicepointKind.PREDICATE:
            cursor = cp.payload.get("cursor")
            if hasattr(cursor, "clone") and callable(cursor.clone):
                # Clone the cursor for restoration
                cursor_snapshots[id(cursor)] = cursor.clone()

    # Create CATCH choicepoint (phase=GOAL)
    stamp = engine.trail.next_stamp()
    catch_cp = Choicepoint(
        kind=ChoicepointKind.CATCH,
        trail_top=base_trail_top,
        goal_stack_height=base_goal_height,
        frame_stack_height=base_frame_height,
        payload={
            "phase": "GOAL",
            "catcher": catcher_arg,
            "recovery": recovery_arg,
            "cp_height": base_cp_height,  # Store CP height for restoration
            "has_pop_frame": has_pop_frame_sentinel,  # Track if we have a POP_FRAME that might be consumed
            "cursor_snapshots": cursor_snapshots,  # Snapshot for cursor restoration
            "recursion_depth": recursion_depth,  # Track recursion depth for frame validation
        },
        stamp=stamp,
    )
    engine.cp_stack.append(catch_cp)

    # Emit internal event for CP push
    if engine.tracer:
        engine.tracer.emit_internal_event(
            "cp_push", {"pred_id": "catch", "trail_top": base_trail_top}
        )

    # Create a frame to establish cut barrier for the catch scope
    # This prevents cuts within Goal from escaping the catch boundary
    engine._next_frame_id += 1
    catch_frame = Frame(
        frame_id=engine._next_frame_id,
        cut_barrier=len(engine.cp_stack),  # Cut stops at the CATCH CP
        goal_height=engine.goal_stack.height(),
        pred=("catch", 3),
    )
    engine.frame_stack.append(catch_frame)

    # Push POP_FRAME to clean up the catch frame after Goal completes
    engine._push_goal(
        Goal(GoalType.POP_FRAME, None, payload={"frame_id": engine._next_frame_id})
    )

    # Push the user's Goal to execute
    engine._push_goal(Goal.from_term(goal))

    return True


def handle_throw(engine, ball: Term) -> bool:
    """Handle a thrown exception by searching for a matching catch.

    Args:
        engine: The Prolog engine instance
        ball: The thrown exception term

    Returns:
        True if exception was caught and handled, False otherwise
    """

    # Search from inner to outer
    i = len(engine.cp_stack) - 1
    while i >= 0:
        cp = engine.cp_stack[i]
        i -= 1

        if cp.kind != ChoicepointKind.CATCH:
            continue
        if cp.payload.get("phase") != "GOAL":
            continue

        # Scope guard: only catch if cp window encloses current point
        cp_height = cp.payload.get("cp_height", 0)
        if cp_height > len(engine.cp_stack) - 1:
            continue

        # --- Two-phase unification: check match without side effects ---
        if not engine._match_only(ball, cp.payload.get("catcher")):
            continue

        # --- We have a matching catcher: restore to catch baselines ---
        if engine.trace:
            engine._trace_log.append(
                f"[CATCH] Restoring: cp_height={cp_height}, current cp_stack len={len(engine.cp_stack)}"
            )

        engine.trail.unwind_to(cp.trail_top, engine.store)
        engine._shrink_goal_stack_to(cp.goal_stack_height)
        while len(engine.frame_stack) > cp.frame_stack_height:
            engine.frame_stack.pop()
        while len(engine.cp_stack) > cp_height:
            removed = engine.cp_stack.pop()
            if engine.trace:
                engine._trace_log.append(f"[CATCH] Removing CP: {removed.kind.name}")

        # Switch to the catch window stamp
        engine.trail.set_current_stamp(cp.stamp)

        # Restore streaming cursors (conservative approach)
        cursor_snapshots = cp.payload.get("cursor_snapshots", {})
        for other_cp in engine.cp_stack[:cp_height]:
            if other_cp.kind == ChoicepointKind.PREDICATE:
                cursor = other_cp.payload.get("cursor")
                if hasattr(cursor, "clone") and callable(cursor.clone):
                    snapshot = cursor_snapshots.get(id(cursor))
                    if snapshot:
                        # Replace with snapshot directly (already cloned at catch time)
                        other_cp.payload["cursor"] = snapshot

        # Re-unify to make catcher bindings visible to Recovery
        ok2 = engine._unify(ball, cp.payload.get("catcher"))
        assert ok2, "ball unified in trial but failed after restore (bug)"

        # Validate frame stack consistency for recursive predicates
        expected_depth = cp.payload.get("recursion_depth", 0)
        if expected_depth > 0 and len(engine.frame_stack) > 0:
            current_pred = engine.frame_stack[-1].pred
            actual_depth = sum(1 for f in engine.frame_stack if f.pred == current_pred)

            if actual_depth != expected_depth:
                # Log the inconsistency but don't fail - this is defensive
                if engine.trace:
                    engine._trace_log.append(
                        f"[CATCH] Frame depth mismatch: expected {expected_depth}, actual {actual_depth}"
                    )

        # Debug assertions
        assert engine.goal_stack.height() == cp.goal_stack_height
        assert len(engine.cp_stack) == cp_height
        assert len(engine.frame_stack) >= cp.frame_stack_height

        # Record metrics for exception caught
        if engine.metrics:
            engine.metrics.record_exception_caught()

        # Emit catch_switch event when we catch an exception
        if engine.tracer:
            engine.tracer.emit_internal_event(
                "catch_switch",
                {
                    "exception": str(ball),
                    "handler": str(cp.payload.get("recovery")),
                },
            )

        # Push only the recovery goal - DO NOT re-install the CATCH choicepoint
        # If recovery fails, we backtrack transparently to outer choicepoints
        engine._push_goal(Goal.from_term(cp.payload.get("recovery")))
        return True

    return False  # No catcher matched
