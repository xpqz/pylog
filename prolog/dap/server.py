"""DAP server loop and connection handling.

The server accepts DAP protocol messages via stdio or socket, dispatches them
to registered handlers, and sends responses/events back to the client.

Logging Configuration:
    The server uses Python's logging module. To see log output, configure logging
    before creating the server:

        import logging
        logging.basicConfig(level=logging.INFO)

    Or configure specifically for this module:

        import logging
        logging.getLogger('prolog.dap.server').setLevel(logging.DEBUG)
"""

import sys
import threading
import logging
from typing import Any, BinaryIO, Callable, Optional
from prolog.dap.protocol import DAPMessage, encode_message, decode_message

logger = logging.getLogger(__name__)


class DAPServer:
    """DAP server that handles protocol messages and dispatches to handlers.

    The server manages:
    - Message reading from input stream
    - Message dispatching to registered handlers
    - Response/event sending to output stream
    - Sequence number management
    - Thread-safe message sending
    """

    def __init__(
        self, stdin: Optional[BinaryIO] = None, stdout: Optional[BinaryIO] = None
    ):
        """Initialize DAP server.

        Args:
            stdin: Input stream for reading messages (default: sys.stdin.buffer)
            stdout: Output stream for writing messages (default: sys.stdout.buffer)
        """
        self._stdin = stdin if stdin is not None else sys.stdin.buffer
        self._stdout = stdout if stdout is not None else sys.stdout.buffer
        self._running = False
        self._seq = 0
        self._handlers: dict[str, Callable[[dict], dict]] = {}
        self._message_handler: Optional[Callable[[dict], None]] = None
        self._write_lock = threading.Lock()

    def is_running(self) -> bool:
        """Check if server is running."""
        return self._running

    def stop(self):
        """Stop the server."""
        self._running = False

    def set_message_handler(self, handler: Callable[[dict], None]):
        """Set a handler to be called for all received messages.

        Args:
            handler: Callable that receives the decoded message dict
        """
        self._message_handler = handler

    def register_handler(self, command: str, handler: Callable[[dict], dict]):
        """Register a handler for a specific command.

        Args:
            command: Command name (e.g., "initialize", "launch")
            handler: Callable that receives the request and returns response body
                     Should return dict with response data, or raise exception on error
        """
        self._handlers[command] = handler

    def _next_seq(self) -> int:
        """Get next sequence number for outgoing messages."""
        self._seq += 1
        return self._seq

    def send_message(self, message: dict[str, Any]):
        """Send a message to the client.

        Thread-safe method for sending messages.

        Args:
            message: Dictionary to encode and send
        """
        encoded = encode_message(message)
        with self._write_lock:
            self._stdout.write(encoded)
            self._stdout.flush()

    def send_response(
        self,
        request_seq: int,
        command: str,
        success: bool,
        body: Optional[dict[str, Any]] = None,
        message: Optional[str] = None,
    ):
        """Send a response message.

        Args:
            request_seq: Sequence number from the request
            command: Command name from the request
            success: Whether the request succeeded
            body: Optional response body
            message: Optional error message (if success is False)
        """
        msg = DAPMessage.response(
            seq=self._next_seq(),
            request_seq=request_seq,
            command=command,
            success=success,
            body=body,
            message=message,
        )
        self.send_message(msg.to_dict())

    def send_event(self, event: str, body: Optional[dict[str, Any]] = None):
        """Send an event message.

        Args:
            event: Event name (e.g., "stopped", "output", "terminated")
            body: Optional event data
        """
        msg = DAPMessage.create_event(seq=self._next_seq(), event_name=event, body=body)
        self.send_message(msg.to_dict())

    def process_message(self):
        """Read and process one message from the input stream.

        Raises:
            EOFError: If stream ends
            json.JSONDecodeError: If message has invalid JSON
        """
        message = decode_message(self._stdin)

        # Call message handler if registered
        if self._message_handler:
            self._message_handler(message)

        # Dispatch to command handler if this is a request
        if message.get("type") == "request":
            self.dispatch_message(message)

    def dispatch_message(self, request: dict[str, Any]) -> dict[str, Any]:
        """Dispatch a request message to its handler.

        Args:
            request: Request message dictionary

        Returns:
            Response dictionary (also sent to client)
        """
        command = request.get("command")
        request_seq = request.get("seq", 0)

        if command not in self._handlers:
            logger.warning(f"Unknown command: '{command}'")
            response = {
                "success": False,
                "message": f"Command '{command}' is not supported",
            }
            self.send_response(
                request_seq, command or "unknown", False, message=response["message"]
            )
            return response

        try:
            # Call handler
            handler = self._handlers[command]
            result = handler(request)

            # Send success response
            self.send_response(request_seq, command, True, body=result)
            return {"success": True, "body": result}

        except Exception as e:
            logger.error(f"Error handling '{command}': {e}", exc_info=True)
            error_msg = f"Error executing '{command}': {str(e)}"
            self.send_response(request_seq, command, False, message=error_msg)
            return {"success": False, "message": error_msg}

    def run_stdio(self):
        """Run the server loop, reading from stdin and writing to stdout.

        This blocks until the server is stopped or stdin closes.
        """
        self._running = True
        logger.info("DAP server started on stdio")

        try:
            while self._running:
                try:
                    self.process_message()
                except EOFError:
                    logger.info("Input stream closed, stopping server")
                    break
                except Exception as e:
                    logger.error(f"Error processing message: {e}", exc_info=True)
                    # Continue processing other messages

        finally:
            self._running = False
            logger.info("DAP server stopped")
