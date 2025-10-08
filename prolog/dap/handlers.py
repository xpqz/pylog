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
