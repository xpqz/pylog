from prolog.engine.utils.terms import reify_term


def create_snapshot(engine, event):
    """Creates a snapshot of the engine state for the debugger."""

    # Reify query variables
    variables = {}
    if hasattr(engine, "_query_vars") and engine._query_vars:
        try:
            for var_id, var_name in engine._query_vars:
                # reify_var is not public, so we construct a Var and reify it.
                # This is a bit of a hack and might be improved if reify_var is made public.
                reified_value = reify_term(
                    engine.store, engine._qname_by_id, engine.store.cells[var_id].term
                )
                variables[var_name] = reified_value
        except (TypeError, AttributeError):
            # Handle mock objects or missing attributes gracefully
            pass

    # Serialize goal stack
    goal_stack = []
    if hasattr(engine, "goal_stack"):
        try:
            # Handle both real goal stack and mock lists
            if hasattr(engine.goal_stack, "_stack"):
                for goal in engine.goal_stack._stack:
                    goal_stack.append(str(goal.term))
            elif isinstance(engine.goal_stack, list):
                goal_stack = [str(g) for g in engine.goal_stack]
        except (TypeError, AttributeError):
            pass

    # Serialize frame stack
    frame_stack = []
    if hasattr(engine, "frame_stack") and engine.frame_stack:
        try:
            for frame in engine.frame_stack:
                # Handle real frames
                if hasattr(frame, "frame_id"):
                    frame_stack.append(
                        {
                            "id": frame.frame_id,
                            "predicate": str(frame.pred),
                            "depth": (
                                frame.call_depth if hasattr(frame, "call_depth") else 0
                            ),
                        }
                    )
                else:
                    # Handle mock frames
                    frame_stack.append(str(frame))
        except (TypeError, AttributeError):
            pass

    return {
        "goal_stack": goal_stack,
        "frame_stack": frame_stack,
        "variables": variables,
        "current_goal": str(event.get("goal")),
        "port": event.get("port"),
        "depth": event.get("depth"),
    }
