"""DAP lifecycle request handlers.

Implements handlers for DAP lifecycle requests: initialize, launch, disconnect.
These handlers integrate with the PyLog engine and StepController to provide
debugging capabilities.
"""

import threading
import logging
from typing import Any
from pathlib import Path

from prolog.dap.session import get_session
from prolog.engine.engine import Engine
from prolog.debug.dap.step_controller import StepController
from prolog.debug.dap.breakpoint_store import BreakpointStore
from prolog.parser.reader import Reader
from prolog.ast.clauses import Program
from prolog.ast.terms import Struct, List as PrologList

logger = logging.getLogger(__name__)


def handle_initialize(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the initialize request.

    Returns DAP capabilities indicating which features the adapter supports.

    Args:
        request: Initialize request from DAP client

    Returns:
        Capabilities dictionary
    """
    session = get_session()
    session.initialize()

    logger.info("DAP session initialized")

    # Return capabilities - MVP only supports basic features
    return {
        "supportsConfigurationDoneRequest": True,
        "supportsFunctionBreakpoints": False,
        "supportsConditionalBreakpoints": False,
        "supportsHitConditionalBreakpoints": False,
        "supportsEvaluateForHovers": False,
        "supportsStepBack": False,
        "supportsSetVariable": False,
        "supportsRestartFrame": False,
        "supportsGotoTargetsRequest": False,
        "supportsStepInTargetsRequest": False,
        "supportsCompletionsRequest": False,
        "supportsModulesRequest": False,
        "supportsRestartRequest": False,
        "supportsExceptionOptions": False,
        "supportsValueFormattingOptions": False,
        "supportsExceptionInfoRequest": False,
        "supportTerminateDebuggee": False,
        "supportSuspendDebuggee": False,
        "supportsDelayedStackTraceLoading": False,
        "supportsLoadedSourcesRequest": False,
        "supportsLogPoints": False,
        "supportsTerminateThreadsRequest": False,
        "supportsSetExpression": False,
        "supportsTerminateRequest": False,
        "supportsDataBreakpoints": False,
        "supportsReadMemoryRequest": False,
        "supportsWriteMemoryRequest": False,
        "supportsDisassembleRequest": False,
        "supportsCancelRequest": False,
        "supportsBreakpointLocationsRequest": False,
        "supportsClipboardContext": False,
        "supportsSteppingGranularity": False,
        "supportsInstructionBreakpoints": False,
        "supportsExceptionFilterOptions": False,
        "supportsSingleThreadExecutionRequests": False,
    }


def handle_launch(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the launch request.

    Loads the Prolog program and starts the engine with the specified query.

    Args:
        request: Launch request with program/query configuration

    Returns:
        Empty dict on success

    Raises:
        ValueError: If required arguments are missing
        FileNotFoundError: If program file doesn't exist
        RuntimeError: If called before initialize
    """
    session = get_session()

    # Verify session is initialized
    if not session.is_initialized():
        raise RuntimeError("launch called before initialize")

    # Extract arguments
    arguments = request.get("arguments", {})
    program_path_str = arguments.get("program")
    query = arguments.get("query")
    stop_on_entry = arguments.get("stopOnEntry", True)
    occurs_check = arguments.get("occursCheck", False)
    use_indexing = arguments.get("useIndexing", True)
    ports = arguments.get("ports")  # Optional port filter

    # Validate required arguments
    if not program_path_str:
        raise ValueError("launch requires 'program' argument")

    # Check program file exists
    program_path = Path(program_path_str)
    if not program_path.exists():
        raise FileNotFoundError(f"Program file not found: {program_path}")

    logger.info(f"Loading program: {program_path}")

    # Load and parse the program
    try:
        with open(program_path, "r") as f:
            program_text = f.read()
    except (OSError, IOError) as e:
        raise IOError(f"Failed to read program file '{program_path}': {e}")

    try:
        reader = Reader()
        clauses = reader.read_program(program_text)
        program = Program(clauses=tuple(clauses))
    except Exception as e:
        raise ValueError(f"Failed to parse program '{program_path}': {e}")

    # Create engine with debugging support
    step_controller = StepController(eligible_ports=ports)
    breakpoint_store = BreakpointStore()

    engine = Engine(
        program=program,
        trace=True,
        occurs_check=occurs_check,
        use_indexing=use_indexing,
    )

    # Wire up step controller to engine's tracer
    if engine.tracer:
        engine.tracer.step_controller = step_controller
        engine.tracer.breakpoint_store = breakpoint_store

    # Set initial stepping mode
    if stop_on_entry:
        step_controller.set_mode("paused")
    else:
        step_controller.set_mode("running")

    # Store engine components in session
    session.set_engine_components(engine, step_controller, breakpoint_store)

    logger.info(f"Engine created, stopOnEntry={stop_on_entry}")

    # Start engine in a separate thread if query is provided
    if query:
        logger.info(f"Query provided but execution not yet implemented: {query}")

        # TODO(#271): Implement query execution in run_engine()
        # Query execution requires:
        # 1. Parse query string to goals (using parse_query)
        # 2. Call engine.run(goals) or engine.query(query_text)
        # 3. Coordinate with StepController for stepping/pausing
        # 4. Send DAP events (stopped, continued, terminated)
        # 5. Handle exceptions and send proper error responses
        #
        # This is intentionally not implemented in this PR (#270) which focuses
        # on lifecycle handlers only. Query execution will be added in #271
        # (Stepping Control Integration).

        def run_engine():
            try:
                # Placeholder: Query execution not yet implemented
                # When implemented, this should call engine.query(query)
                raise NotImplementedError(
                    "Query execution not yet implemented (tracked in #271)"
                )
            except Exception as e:
                logger.error(f"Engine error: {e}", exc_info=True)

        engine_thread = threading.Thread(target=run_engine, daemon=True)
        session.set_engine_thread(engine_thread)
        engine_thread.start()

    return {}


def handle_disconnect(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the disconnect request.

    Cleanup and release resources:
    - Release StepController barrier
    - Stop engine thread
    - Clean up session state

    Args:
        request: Disconnect request

    Returns:
        Empty dict on success
    """
    session = get_session()
    session.cleanup()

    logger.info("DAP session disconnected and cleaned up")

    return {}


def handle_continue(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the continue request.

    Resumes execution in running mode (no pausing except at breakpoints).
    This operation is thread-safe and coordinates with the engine thread
    through the StepController's barrier mechanism.

    Args:
        request: Continue request from DAP client

    Returns:
        Empty dict on success

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.step_controller:
        raise RuntimeError("Cannot continue - no active debugging session")

    # Set mode to running and resume execution
    session.step_controller.set_mode("running")
    session.step_controller.resume()

    logger.debug("Continue: set mode to running")

    return {}


def handle_next(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the next (step over) request.

    Steps over the current statement, pausing at the next statement at
    the same or shallower depth. This operation is thread-safe and coordinates
    with the engine thread through the StepController's barrier mechanism.

    Args:
        request: Next request from DAP client

    Returns:
        Empty dict on success

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.step_controller:
        raise RuntimeError("Cannot step over - no active debugging session")

    # Get current depth from engine's goal stack
    # For now, use depth 0 as baseline (will be updated when query execution is implemented)
    # TODO(thread-safety): Access to goal_stack should be synchronized when query execution
    # is implemented, as the engine modifies it from a separate thread. Consider adding
    # a lock or having the engine report depth through a thread-safe mechanism.
    current_depth = 0
    if session.engine and hasattr(session.engine, "goal_stack"):
        current_depth = len(session.engine.goal_stack)

    # Set mode to step_over with current depth and resume
    session.step_controller.set_mode("step_over", depth=current_depth)
    session.step_controller.resume()

    logger.debug(f"Next: set mode to step_over at depth {current_depth}")

    return {}


def handle_step_in(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the stepIn request.

    Steps into the next statement, including entering called predicates.
    This operation is thread-safe and coordinates with the engine thread
    through the StepController's barrier mechanism.

    Args:
        request: StepIn request from DAP client

    Returns:
        Empty dict on success

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.step_controller:
        raise RuntimeError("Cannot step in - no active debugging session")

    # Set mode to step_in and resume execution
    session.step_controller.set_mode("step_in")
    session.step_controller.resume()

    logger.debug("StepIn: set mode to step_in")

    return {}


def handle_step_out(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the stepOut request.

    Steps out of the current predicate, pausing when returning to the caller.
    This operation is thread-safe and coordinates with the engine thread
    through the StepController's barrier mechanism.

    Args:
        request: StepOut request from DAP client

    Returns:
        Empty dict on success

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.step_controller:
        raise RuntimeError("Cannot step out - no active debugging session")

    # Get current depth from engine's goal stack
    # For now, use depth 0 as baseline (will be updated when query execution is implemented)
    # TODO(thread-safety): Access to goal_stack should be synchronized when query execution
    # is implemented, as the engine modifies it from a separate thread. Consider adding
    # a lock or having the engine report depth through a thread-safe mechanism.
    current_depth = 0
    if session.engine and hasattr(session.engine, "goal_stack"):
        current_depth = len(session.engine.goal_stack)

    # Set mode to step_out with current depth and resume
    session.step_controller.set_mode("step_out", depth=current_depth)
    session.step_controller.resume()

    logger.debug(f"StepOut: set mode to step_out at depth {current_depth}")

    return {}


def handle_set_breakpoints(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the setBreakpoints request.

    Sets or clears function breakpoints (predicate breakpoints in Prolog terms).
    This replaces all existing breakpoints with the new set.

    Args:
        request: SetBreakpoints request from DAP client with breakpoints list

    Returns:
        Dict with "breakpoints" key containing list of verified breakpoints

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.breakpoint_store:
        raise RuntimeError("No active debugging session - call launch first")

    # Extract breakpoints from request
    arguments = request.get("arguments", {})
    breakpoints = arguments.get("breakpoints", [])

    # Clear all existing breakpoints (setBreakpoints replaces, not adds)
    bp_store = session.breakpoint_store
    bp_store.clear_all()

    # Process each breakpoint
    verified_breakpoints = []

    for idx, bp in enumerate(breakpoints):
        name = bp.get("name", "")

        # Parse functor/arity from name (format: "functor/arity")
        if "/" not in name:
            # Invalid format
            verified_breakpoints.append(
                {
                    "id": idx,
                    "verified": False,
                    "message": f"Invalid breakpoint format: '{name}'. Expected 'functor/arity'",
                }
            )
            continue

        try:
            functor, arity_str = name.rsplit("/", 1)
            arity = int(arity_str)
        except (ValueError, TypeError):
            # Invalid arity
            verified_breakpoints.append(
                {
                    "id": idx,
                    "verified": False,
                    "message": f"Invalid arity in breakpoint: '{name}'",
                }
            )
            continue

        # Parse port filter from condition if present
        condition = bp.get("condition", "")
        ports = None
        if condition:
            # Parse "port=CALL" or "port=CALL,EXIT" format
            if condition.startswith("port="):
                port_str = condition[5:]  # Remove "port=" prefix
                ports = [p.strip().upper() for p in port_str.split(",")]

        # Add breakpoint to store
        bp_id = bp_store.add_breakpoint(functor, arity, ports=ports)

        # Return verified breakpoint
        verified_breakpoints.append(
            {
                "id": bp_id,
                "verified": True,
            }
        )

        logger.debug(
            f"Set breakpoint: {functor}/{arity}"
            + (f" with ports {ports}" if ports else "")
        )

    logger.info(f"Set {len(verified_breakpoints)} breakpoints")

    return {"breakpoints": verified_breakpoints}


# Variable reference tracking for data inspection
_var_references = {}
_next_var_ref = 1
_var_ref_lock = threading.Lock()


def _get_var_reference(obj):
    """Get or create a variable reference for an object.

    Args:
        obj: Object to get reference for (term, list, etc.)

    Returns:
        int: Variable reference ID (0 for non-expandable, >0 for expandable)
    """
    global _next_var_ref

    # Don't create references for simple atoms or unbound vars
    if not _is_expandable(obj):
        return 0

    with _var_ref_lock:
        # Check if we already have a reference
        obj_id = id(obj)
        if obj_id in _var_references:
            return _var_references[obj_id]

        # Create new reference
        ref = _next_var_ref
        _next_var_ref += 1
        _var_references[obj_id] = ref
        _var_references[ref] = obj  # Reverse mapping
        return ref


def _clear_var_references():
    """Clear all variable references.

    Should be called when starting a new pause/stopped event to prevent
    unbounded memory growth. Variable references are only valid within
    a single pause scope.
    """
    global _var_references, _next_var_ref
    with _var_ref_lock:
        _var_references.clear()
        _next_var_ref = 1


def _is_expandable(obj):
    """Check if an object is expandable (compound term, list, etc.).

    Args:
        obj: Object to check

    Returns:
        bool: True if expandable, False otherwise
    """
    # Compound terms and lists are expandable
    return isinstance(obj, (Struct, PrologList))


def handle_stack_trace(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the stackTrace request.

    Returns information about the call stack frames.

    Args:
        request: StackTrace request from DAP client

    Returns:
        Dict with "stackFrames" key containing list of frames

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.engine:
        raise RuntimeError("No active debugging session - call launch first")

    # Get stack frames from engine
    # For now, return empty list (will be populated when query execution is implemented)
    stack_frames = []

    # TODO: When query execution is implemented, iterate over engine.goal_stack
    # and create frame objects with id, name, line, column

    logger.debug(f"StackTrace: returning {len(stack_frames)} frames")

    return {"stackFrames": stack_frames}


def handle_scopes(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the scopes request.

    Returns the scopes for a stack frame (currently just "Locals").

    Args:
        request: Scopes request from DAP client with frameId

    Returns:
        Dict with "scopes" key containing list of scopes

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.engine:
        raise RuntimeError("No active debugging session - call launch first")

    # arguments = request.get("arguments", {})
    # frame_id = arguments.get("frameId", 0)

    # For MVP, we have a single "Locals" scope
    # The variablesReference points to the query variables
    scopes = [
        {
            "name": "Locals",
            "variablesReference": 1,  # Fixed reference for query variables
            "expensive": False,
        }
    ]

    logger.debug("Scopes: returning Locals scope")

    return {"scopes": scopes}


def handle_variables(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the variables request.

    Returns variables for a given variablesReference.

    Args:
        request: Variables request from DAP client with variablesReference

    Returns:
        Dict with "variables" key containing list of variables

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.engine:
        raise RuntimeError("No active debugging session - call launch first")

    arguments = request.get("arguments", {})
    var_ref = arguments.get("variablesReference", 0)

    # For now, return empty list (will be populated when query execution is implemented)
    variables = []

    # TODO: When query execution is implemented:
    # - If var_ref == 1, return query variables
    # - Otherwise, look up the term by reference and expand it

    logger.debug(f"Variables: returning {len(variables)} variables for ref {var_ref}")

    return {"variables": variables}


def handle_evaluate(request: dict[str, Any]) -> dict[str, Any]:
    """Handle the evaluate request.

    Evaluates an expression (variable lookup by name).

    Args:
        request: Evaluate request from DAP client with expression

    Returns:
        Dict with "result" and "variablesReference" keys

    Raises:
        RuntimeError: If no active engine session
    """
    session = get_session()

    if not session.engine:
        raise RuntimeError("No active debugging session - call launch first")

    arguments = request.get("arguments", {})
    expression = arguments.get("expression", "")

    # For now, return "Variable not found" (will be implemented with query execution)
    result = "Variable not found"
    var_ref = 0

    # TODO: When query execution is implemented:
    # - Look up the variable by name in the current frame
    # - Return its pretty-printed value
    # - If it's compound, create a variablesReference for expansion

    logger.debug(f"Evaluate: '{expression}' -> {result}")

    return {"result": result, "variablesReference": var_ref}
