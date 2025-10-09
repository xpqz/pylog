"""Entry point for running the PyLog DAP server as a module.

Usage:
    python -m prolog.dap [OPTIONS]

The server listens on stdio by default, implementing the Debug Adapter Protocol
for debugging PyLog Prolog programs.

Options are passed through the launch configuration from VS Code and processed
by the launch handler. The server will read DAP protocol messages from stdin
and write responses to stdout.
"""

import sys
import logging
from prolog.dap.server import DAPServer
from prolog.dap.session import get_session
from prolog.dap.handlers import (
    handle_initialize,
    handle_launch,
    handle_disconnect,
    handle_continue,
    handle_next,
    handle_step_in,
    handle_step_out,
    handle_set_breakpoints,
    handle_stack_trace,
    handle_scopes,
    handle_variables,
    handle_evaluate,
)


def main():
    """Main entry point for the DAP server."""
    # Configure logging to stderr (stdout is for DAP protocol)
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        stream=sys.stderr,
    )

    logger = logging.getLogger(__name__)
    logger.info("Starting PyLog DAP server")

    # Create server instance
    server = DAPServer()

    # Register server with session for event emission
    session = get_session()
    session.set_server(server)

    # Register all DAP request handlers
    server.register_handler("initialize", handle_initialize)
    server.register_handler("launch", handle_launch)
    server.register_handler("disconnect", handle_disconnect)
    server.register_handler("continue", handle_continue)
    server.register_handler("next", handle_next)
    server.register_handler("stepIn", handle_step_in)
    server.register_handler("stepOut", handle_step_out)
    server.register_handler("setBreakpoints", handle_set_breakpoints)
    server.register_handler("stackTrace", handle_stack_trace)
    server.register_handler("scopes", handle_scopes)
    server.register_handler("variables", handle_variables)
    server.register_handler("evaluate", handle_evaluate)

    # Additional handlers that may be needed
    server.register_handler("configurationDone", lambda req: {})
    server.register_handler(
        "threads", lambda req: {"threads": [{"id": 1, "name": "main"}]}
    )

    logger.info("DAP server handlers registered")

    # Run server loop on stdio
    try:
        server.run_stdio()
    except KeyboardInterrupt:
        logger.info("Server interrupted by user")
    except Exception as e:
        logger.error(f"Server error: {e}", exc_info=True)
        sys.exit(1)

    logger.info("PyLog DAP server stopped")


if __name__ == "__main__":
    main()
