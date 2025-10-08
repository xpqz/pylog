"""DAP message protocol encoding and decoding.

Implements the Debug Adapter Protocol's two-part message format:
1. Header with Content-Length (and optional Content-Type)
2. JSON body

Example message:
    Content-Length: 119\r\n
    \r\n
    {"seq":1,"type":"request","command":"initialize","arguments":{"adapterID":"pylog"}}
"""

import json
from typing import Any, BinaryIO, Optional
from dataclasses import dataclass


def encode_message(message: dict[str, Any]) -> bytes:
    """Encode a DAP message with Content-Length header.

    Args:
        message: Dictionary to encode as JSON

    Returns:
        Encoded message bytes with header and body
    """
    body = json.dumps(message, ensure_ascii=False).encode("utf-8")
    content_length = len(body)
    header = f"Content-Length: {content_length}\r\n\r\n".encode("utf-8")
    return header + body


def decode_message(stream: BinaryIO) -> dict[str, Any]:
    """Decode a DAP message from a stream.

    Args:
        stream: Binary stream to read from (e.g., stdin or socket)

    Returns:
        Decoded message dictionary

    Raises:
        ValueError: If Content-Length header is missing
        EOFError: If stream ends before complete message is read
        json.JSONDecodeError: If body is not valid JSON
    """
    # Read headers until blank line
    headers = {}
    while True:
        line = stream.readline()
        if not line:
            raise EOFError("Stream ended while reading headers")

        line = line.decode("utf-8").strip()
        if not line:
            # Blank line marks end of headers
            break

        if ":" in line:
            key, value = line.split(":", 1)
            headers[key.strip()] = value.strip()

    # Extract Content-Length
    if "Content-Length" not in headers:
        raise ValueError("Missing Content-Length header in DAP message")

    content_length = int(headers["Content-Length"])

    # Read body
    body = stream.read(content_length)
    if len(body) < content_length:
        raise EOFError(f"Expected {content_length} bytes but got {len(body)}")

    # Decode JSON
    return json.loads(body.decode("utf-8"))


@dataclass
class DAPMessage:
    """Wrapper for DAP messages with type-safe construction.

    All DAP messages have:
    - seq: Sequence number
    - type: One of "request", "response", "event"

    Requests have:
    - command: Command name
    - arguments: Optional command arguments

    Responses have:
    - request_seq: Sequence number of the request
    - command: Command name from the request
    - success: Boolean indicating success
    - message: Optional error message (if success is false)
    - body: Optional response body

    Events have:
    - event: Event name
    - body: Optional event data
    """

    seq: int
    type: str
    command: Optional[str] = None
    arguments: Optional[dict[str, Any]] = None
    request_seq: Optional[int] = None
    success: Optional[bool] = None
    message: Optional[str] = None
    body: Optional[dict[str, Any]] = None
    event: Optional[str] = None

    @classmethod
    def request(
        cls, seq: int, command: str, arguments: Optional[dict[str, Any]] = None
    ) -> "DAPMessage":
        """Create a request message."""
        return cls(seq=seq, type="request", command=command, arguments=arguments)

    @classmethod
    def response(
        cls,
        seq: int,
        request_seq: int,
        command: str,
        success: bool,
        body: Optional[dict[str, Any]] = None,
        message: Optional[str] = None,
    ) -> "DAPMessage":
        """Create a response message."""
        return cls(
            seq=seq,
            type="response",
            request_seq=request_seq,
            command=command,
            success=success,
            body=body,
            message=message,
        )

    @classmethod
    def create_event(
        cls, seq: int, event_name: str, body: Optional[dict[str, Any]] = None
    ) -> "DAPMessage":
        """Create an event message."""
        return cls(seq=seq, type="event", event=event_name, body=body)

    def to_dict(self) -> dict[str, Any]:
        """Convert message to dictionary for encoding."""
        result = {"seq": self.seq, "type": self.type}

        if self.type == "request":
            result["command"] = self.command
            if self.arguments is not None:
                result["arguments"] = self.arguments

        elif self.type == "response":
            result["request_seq"] = self.request_seq
            result["command"] = self.command
            result["success"] = self.success
            if self.message is not None:
                result["message"] = self.message
            if self.body is not None:
                result["body"] = self.body

        elif self.type == "event":
            result["event"] = self.event
            if self.body is not None:
                result["body"] = self.body

        return result

    @classmethod
    def from_dict(cls, d: dict[str, Any]) -> "DAPMessage":
        """Create message from decoded dictionary."""
        return cls(
            seq=d.get("seq", 0),
            type=d.get("type", ""),
            command=d.get("command"),
            arguments=d.get("arguments"),
            request_seq=d.get("request_seq"),
            success=d.get("success"),
            message=d.get("message"),
            body=d.get("body"),
            event=d.get("event"),
        )
