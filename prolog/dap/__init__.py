"""Debug Adapter Protocol (DAP) server implementation for PyLog.

This package implements a standalone DAP server that controls the PyLog engine
for debugging Prolog programs. It follows the Debug Adapter Protocol specification:
https://microsoft.github.io/debug-adapter-protocol/

Components:
- protocol: DAP message encoding/decoding (Content-Length header protocol)
- server: DAP server loop and connection handling
- handlers: Request handlers for DAP commands

See docs/dap.md for the complete architecture.
"""

from prolog.dap.protocol import DAPMessage, encode_message, decode_message
from prolog.dap.server import DAPServer

__all__ = ["DAPMessage", "encode_message", "decode_message", "DAPServer"]
