"""
Dispatch functions extracted from Engine.

This module contains the bodies of Engine._dispatch_* methods as free functions
that take the engine as their first parameter. This keeps Engine methods as
thin delegates while isolating the complex dispatch logic.

Functions:
- dispatch_predicate(engine, goal, call_depth, call_emitted)
- dispatch_builtin(engine, goal, call_depth, call_emitted)
- dispatch_conjunction(engine, goal, call_depth, call_emitted)
- dispatch_disjunction(engine, goal, call_depth, call_emitted)
- dispatch_if_then_else(engine, goal, call_depth, call_emitted)
- dispatch_cut(engine, goal, call_depth, call_emitted)
"""

from typing import Protocol, Any

from prolog.ast.terms import Atom, Struct
from prolog.engine.runtime import (
    Goal,
    GoalType,
    ChoicepointKind,
    Frame,
    Choicepoint,
)
from prolog.engine.utils.selection import (
    select_clauses,
    SelectionContext,
    extract_predicate_key,
)
from prolog.engine.errors import UndefinedPredicateError


class EngineProtocol(Protocol):
    """Protocol defining the Engine interface needed by dispatch functions."""

    # Core attributes
    program: Any
    store: Any
    trail: Any
    cp_stack: list
    frame_stack: list
    goal_stack: Any
    mode: str
    metrics: Any
    tracer: Any
    trace: bool
    debug: bool
    use_indexing: bool
    use_streaming: bool

    # Internal state
    _renamer: Any
    _builtins: dict
    _next_frame_id: int
    _candidates_considered: int
    _trace_log: list
    _goal_call_depths: dict
    _goal_call_flags: dict

    # Core methods
    def _port(self, port: str, pred_id: str) -> None: ...
    def _trace_port(self, port: str, term: Any, depth_override: int = None) -> None: ...
    def _emit_fail_port(
        self, pred_id: str, term: Any, depth_override: int = None
    ) -> None: ...
    def _unify(self, term1: Any, term2: Any) -> bool: ...
    def _push_goal(self, goal: Goal) -> None: ...


def dispatch_predicate(
    engine: EngineProtocol, goal: Goal, call_depth: int, call_emitted: bool
) -> bool:
    """Dispatch a predicate goal.

    Args:
        engine: The Prolog engine.
        goal: The predicate goal to dispatch.
        call_depth: Current call depth.
        call_emitted: Whether CALL port was already emitted.

    Returns:
        True if successful, False if failed.
    """
    # Extract predicate key and emit ports
    pred_key = extract_predicate_key(goal.term)
    if pred_key is None:
        # Can't match a variable or other term
        return False

    functor, arity = pred_key
    pred_id = f"{functor}/{arity}"

    # Emit CALL port before processing (only once per logical call)
    if not call_emitted:
        engine._port("CALL", pred_id)
    # Also emit to tracer if enabled (always emit for tracer consumers)
    engine._trace_port("call", goal.term, depth_override=call_depth)

    # Record metrics for CALL
    if engine.metrics:
        engine.metrics.record_call(pred_id)

    # Select clauses using utilities
    context = SelectionContext(
        use_indexing=engine.use_indexing,
        use_streaming=engine.use_streaming,
        debug=engine.debug,
        metrics=engine.metrics,
        tracer=engine.tracer,
        trace=engine.trace,
    )
    selection = select_clauses(
        program=engine.program, goal_term=goal.term, store=engine.store, context=context
    )
    cursor = selection.cursor

    # Update debug counters and metrics
    if engine.debug:
        engine._candidates_considered += selection.candidates_considered
        if engine.metrics and selection.candidates_yielded > 0:
            engine.metrics.record_candidates(
                selection.candidates_considered, selection.candidates_yielded
            )

    # Log trace information if available
    if engine.trace and selection.total_clauses > 0:
        engine._trace_log.append(
            f"pred {pred_id}: considered {selection.candidates_considered} of {selection.total_clauses} clauses"
        )

    if not cursor.has_more():
        # No matching clauses
        if engine.mode == "iso":
            # In ISO mode, throw an error for undefined predicates
            raise UndefinedPredicateError(predicate=functor, arity=arity)
        else:
            # In dev mode, just fail - emit FAIL port
            engine._port("FAIL", f"{functor}/{arity}")
            if engine.metrics:
                engine.metrics.record_fail(pred_id)
            return False

    # Take first clause
    clause_idx = cursor.take()
    if clause_idx is None:
        return False
    clause = engine.program.clauses[clause_idx]

    # Capture cut barrier BEFORE creating the alternatives choicepoint
    # This ensures cut will prune alternative clauses (ISO semantics)
    cut_barrier = len(engine.cp_stack)

    # If there are more clauses, create a choicepoint
    if cursor.has_more():
        # Normal choicepoint with more alternatives
        engine.trail.next_stamp()

        # Save the continuation (goals below the call) as an immutable snapshot
        continuation = engine.goal_stack.snapshot()
        continuation_depths = tuple(
            engine._goal_call_depths.get(id(g), len(engine.frame_stack))
            for g in continuation
        )
        continuation_calls = tuple(
            engine._goal_call_flags.get(id(g), False) for g in continuation
        )

        # Save store size for allocation cleanup
        store_top = engine.store.size()

        # Debug: log what we're saving
        if engine.trace:
            engine._trace_log.append(
                f"Creating PREDICATE CP for {functor}/{arity}, saving {len(continuation)} continuation goals"
            )
            for i, g in enumerate(continuation):
                engine._trace_log.append(f"  Continuation[{i}]: {g}")

        cp = Choicepoint(
            kind=ChoicepointKind.PREDICATE,
            trail_top=engine.trail.position(),
            goal_stack_height=len(
                continuation
            ),  # Height = number of continuation goals
            frame_stack_height=0,  # PREDICATE CPs don't manage frames (frame lifecycle is independent)
            payload={
                "goal": goal,
                "cursor": cursor,
                "pred_ref": f"{functor}/{arity}",
                "continuation": continuation,  # Frozen snapshot of continuation goals
                "continuation_depths": continuation_depths,
                "continuation_calls": continuation_calls,
                "store_top": store_top,  # Store size for allocation cleanup
                "call_depth": call_depth,
                "has_succeeded": False,
            },
        )
        engine.cp_stack.append(cp)

        # Emit internal event for CP push
        if engine.tracer:
            # Compute alternatives count based on cursor type
            alternatives = 0
            if cursor:
                if hasattr(cursor, "matches") and hasattr(cursor, "pos"):
                    alternatives = len(cursor.matches) - cursor.pos
                elif hasattr(cursor, "has_more"):
                    alternatives = 1 if cursor.has_more() else 0
            engine.tracer.emit_internal_event(
                "cp_push",
                {
                    "pred_id": f"{functor}/{arity}",
                    "alternatives": alternatives,
                    "trail_top": cp.trail_top,
                },
            )

    # Rename clause with fresh variables
    renamed_clause = engine._renamer.rename_clause(clause)

    # Try to unify with clause head
    if engine._unify(renamed_clause.head, goal.term):
        # Unification succeeded - now push frame for body execution
        # Use the cut_barrier saved before creating choicepoint
        frame_id = engine._next_frame_id
        engine._next_frame_id += 1
        frame = Frame(
            frame_id=frame_id,
            cut_barrier=cut_barrier,
            goal_height=engine.goal_stack.height(),
            pred=f"{functor}/{arity}",
            goal_term=goal.term,  # Store for EXIT port emission
            call_depth=call_depth,
        )
        engine.frame_stack.append(frame)

        # Emit internal event for frame push
        if engine.tracer:
            engine.tracer.emit_internal_event(
                "frame_push",
                {"pred_id": f"{functor}/{arity}", "frame_id": frame_id},
            )

        # Don't call next_stamp() here - only when creating CPs

        # Push POP_FRAME sentinel first, then body goals
        engine._push_goal(
            Goal(
                GoalType.POP_FRAME,
                None,  # Internal goals don't need terms
                payload={"op": "POP_FRAME", "frame_id": frame_id},
            )
        )

        # Push body goals in reverse order
        for body_term in reversed(renamed_clause.body):
            body_goal = Goal.from_term(body_term)
            engine._push_goal(body_goal)
        return True
    else:
        # Unification failed - no frame was created
        return False


def dispatch_builtin(
    engine: EngineProtocol, goal: Goal, call_depth: int, call_emitted: bool
) -> bool:
    """Dispatch a builtin goal.

    Args:
        engine: The Prolog engine.
        goal: The builtin goal to dispatch.
        call_depth: Current call depth.
        call_emitted: Whether CALL port was already emitted.

    Returns:
        True if successful, False if failed.
    """
    if isinstance(goal.term, Atom):
        key = (goal.term.name, 0)
        args = ()
        pred_id = f"{goal.term.name}/0"
    elif isinstance(goal.term, Struct):
        key = (goal.term.functor, len(goal.term.args))
        args = goal.term.args
        pred_id = f"{goal.term.functor}/{len(goal.term.args)}"
    else:
        return False

    builtin_fn = engine._builtins.get(key)
    if builtin_fn is None:
        # Not a recognized builtin
        return False

    # Emit CALL port before executing builtin (only once per logical call)
    if not call_emitted:
        engine._port("CALL", pred_id)

    # Also emit to tracer if enabled
    term = goal.term if goal.term else None
    engine._trace_port("call", term, depth_override=call_depth)

    # Execute the builtin with uniform signature
    try:
        result = builtin_fn(engine, args)

        # Emit EXIT or FAIL port based on result
        if result:
            engine._port("EXIT", pred_id)
            engine._trace_port("exit", term, depth_override=call_depth)
            return True

        engine._emit_fail_port(pred_id, term, depth_override=call_depth)
        return False
    except (ValueError, TypeError):
        # Expected failures from arithmetic or type errors
        engine._emit_fail_port(pred_id, term, depth_override=call_depth)
        return False


def dispatch_conjunction(
    engine: EngineProtocol, goal: Goal, call_depth: int, call_emitted: bool
) -> bool:
    """Dispatch a conjunction goal.

    Args:
        engine: The Prolog engine.
        goal: The conjunction goal to dispatch.
        call_depth: Current call depth.
        call_emitted: Whether CALL port was already emitted.

    Returns:
        True (conjunction dispatch always succeeds in terms of goal pushing).
    """
    # (A, B) - push B then A for left-to-right execution
    conj = goal.term
    if isinstance(conj, Struct) and conj.functor == "," and len(conj.args) == 2:
        left, right = conj.args
        # Push in reverse order
        right_goal = Goal.from_term(right)
        left_goal = Goal.from_term(left)
        engine._push_goal(right_goal)
        engine._push_goal(left_goal)

    return True


def dispatch_disjunction(
    engine: EngineProtocol, goal: Goal, call_depth: int, call_emitted: bool
) -> bool:
    """Dispatch a disjunction goal.

    Args:
        engine: The Prolog engine.
        goal: The disjunction goal to dispatch.
        call_depth: Current call depth.
        call_emitted: Whether CALL port was already emitted.

    Returns:
        True (disjunction dispatch always succeeds in terms of goal pushing).
    """
    # (A ; B) - try A first, create choicepoint for B
    disj = goal.term
    if isinstance(disj, Struct) and disj.functor == ";" and len(disj.args) == 2:
        left, right = disj.args

        # Save the continuation (goals after disjunction) as an immutable snapshot
        continuation = engine.goal_stack.snapshot()
        continuation_depths = tuple(
            engine._goal_call_depths.get(id(g), len(engine.frame_stack))
            for g in continuation
        )
        continuation_calls = tuple(
            engine._goal_call_flags.get(id(g), False) for g in continuation
        )

        # Create choicepoint for right alternative
        stamp = engine.trail.next_stamp()
        cp = Choicepoint(
            kind=ChoicepointKind.DISJUNCTION,
            trail_top=engine.trail.position(),
            goal_stack_height=len(
                continuation
            ),  # Height = number of continuation goals
            frame_stack_height=len(engine.frame_stack),
            payload={
                "alternative": Goal.from_term(right),
                "alternative_depth": len(engine.frame_stack),
                "continuation": continuation,  # Frozen snapshot of continuation goals
                "continuation_depths": continuation_depths,
                "continuation_calls": continuation_calls,
            },
            stamp=stamp,
        )
        engine.cp_stack.append(cp)

        # Emit internal event for CP push
        if engine.tracer:
            engine.tracer.emit_internal_event(
                "cp_push", {"pred_id": "disjunction", "trail_top": cp.trail_top}
            )

        # Try left first (continuation remains on stack)
        left_goal = Goal.from_term(left)
        engine._push_goal(left_goal)

    return True


def dispatch_if_then_else(
    engine: EngineProtocol, goal: Goal, call_depth: int, call_emitted: bool
) -> bool:
    """Dispatch an if-then-else goal with proper commit semantics.

    Args:
        engine: The Prolog engine.
        goal: The if-then-else goal.
        call_depth: Current call depth.
        call_emitted: Whether CALL port was already emitted.

    Returns:
        True if dispatched successfully, False otherwise.
    """
    # (A -> B) ; C - if A succeeds commit to B, else try C
    ite = goal.term
    if not (isinstance(ite, Struct) and ite.functor == ";" and len(ite.args) == 2):
        return False

    left, else_term = ite.args
    if not (isinstance(left, Struct) and left.functor == "->" and len(left.args) == 2):
        return False

    cond, then_term = left.args

    # Capture current choicepoint stack height
    tmp_barrier = len(engine.cp_stack)

    # Create choicepoint that runs Else if Cond fails exhaustively
    engine.trail.next_stamp()
    cp = Choicepoint(
        kind=ChoicepointKind.IF_THEN_ELSE,
        trail_top=engine.trail.position(),
        goal_stack_height=engine.goal_stack.height(),
        frame_stack_height=len(engine.frame_stack),
        payload={
            "else_goal": Goal.from_term(else_term),
            "else_goal_depth": len(engine.frame_stack),
            "tmp_barrier": tmp_barrier,
        },
    )
    engine.cp_stack.append(cp)

    # Emit internal event for CP push
    if engine.tracer:
        engine.tracer.emit_internal_event(
            "cp_push", {"pred_id": "if_then_else", "trail_top": cp.trail_top}
        )

    # Push control goal that will commit and run Then on first success
    then_goal = Goal.from_term(then_term)
    engine._push_goal(
        Goal(
            GoalType.CONTROL,
            None,  # Internal control goal
            payload={
                "op": "ITE_THEN",
                "tmp_barrier": tmp_barrier,
                "then_goal": then_goal,
                "then_goal_depth": len(engine.frame_stack),
            },
        )
    )

    # Push condition to evaluate
    engine._push_goal(Goal.from_term(cond))

    return True


def dispatch_cut(
    engine: EngineProtocol, goal: Goal, call_depth: int, call_emitted: bool
) -> bool:
    """Execute cut (!) - remove choicepoints up to cut barrier.

    Args:
        engine: The Prolog engine.
        goal: The cut goal.
        call_depth: Current call depth.
        call_emitted: Whether CALL port was already emitted.

    Returns:
        True (cut always succeeds).
    """
    if engine.frame_stack:
        # Normal cut within a predicate
        current_frame = engine.frame_stack[-1]
        cut_barrier = current_frame.cut_barrier

        # Find the highest CATCH CP below our target to act as a barrier
        catch_barrier = None
        for i in range(len(engine.cp_stack) - 1, cut_barrier - 1, -1):
            if engine.cp_stack[i].kind == ChoicepointKind.CATCH:
                catch_barrier = i + 1  # Don't prune the CATCH itself
                break

        # Apply the more restrictive barrier
        if catch_barrier is not None:
            cut_barrier = max(cut_barrier, catch_barrier)

        # Count alternatives being pruned
        alternatives_pruned = 0

        # Remove all choicepoints above the barrier
        while len(engine.cp_stack) > cut_barrier:
            removed_cp = engine.cp_stack.pop()

            # Count actual alternatives pruned
            if removed_cp.kind == ChoicepointKind.PREDICATE:
                # For predicate CPs, count remaining clauses
                cursor = removed_cp.payload.get("cursor")
                if cursor and hasattr(cursor, "matches") and hasattr(cursor, "pos"):
                    # Count remaining alternatives in cursor (materialized)
                    alternatives_pruned += len(cursor.matches) - cursor.pos
                elif cursor and hasattr(cursor, "has_more"):
                    # For streaming cursors, just count as 1 (can't count ahead)
                    alternatives_pruned += 1 if cursor.has_more() else 0
                else:
                    alternatives_pruned += 1
            else:
                # For other CP types, count as 1 alternative
                alternatives_pruned += 1

            # Emit internal event for CP pop due to cut
            if engine.tracer:
                pred_id = (
                    removed_cp.payload.get("pred_ref", removed_cp.kind.name.lower())
                    if removed_cp.kind == ChoicepointKind.PREDICATE
                    else removed_cp.kind.name.lower()
                )
                engine.tracer.emit_internal_event("cp_pop", {"pred_id": pred_id})

        # Emit cut_commit event after all CP pops (even if no alternatives pruned)
        if engine.tracer:
            engine.tracer.emit_internal_event(
                "cut_commit",
                {
                    "alternatives_pruned": alternatives_pruned,
                    "barrier": cut_barrier,
                },
            )

        # Record metrics
        if engine.metrics and alternatives_pruned > 0:
            engine.metrics.record_cut()
            engine.metrics.record_alternatives_pruned(alternatives_pruned)
    else:
        # Top-level cut: prune everything (commit to current solution path)
        # This commits to the current solution and prevents backtracking.
        # Some Prolog systems treat top-level cut as a no-op, but we choose
        # to make it commit to prevent finding additional solutions.
        # Test case: ?- (a ; b), !. should return only first success
        engine.cp_stack.clear()

    # Cut always succeeds
    return True
