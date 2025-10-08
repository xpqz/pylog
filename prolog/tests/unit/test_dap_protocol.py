"""Tests for DAP message protocol encoding/decoding."""

import pytest
import json
from io import BytesIO
from prolog.dap.protocol import DAPMessage, encode_message, decode_message


class TestDAPMessageFormat:
    """Test DAP message encoding follows the protocol specification."""

    def test_encode_simple_message(self):
        """Test encoding a simple DAP message with Content-Length header."""

        message = {"seq": 1, "type": "request", "command": "initialize"}
        encoded = encode_message(message)

        # Should have Content-Length header followed by \r\n\r\n and JSON body
        assert b"Content-Length: " in encoded
        assert b"\r\n\r\n" in encoded

        # Extract and verify content length
        header_end = encoded.index(b"\r\n\r\n")
        header = encoded[:header_end].decode("utf-8")
        body = encoded[header_end + 4 :]

        content_length = int(header.split(": ")[1])
        assert len(body) == content_length

        # Verify JSON body
        decoded_body = json.loads(body)
        assert decoded_body == message

    def test_encode_message_with_unicode(self):
        """Test encoding messages with unicode characters."""

        message = {"seq": 1, "type": "response", "body": {"text": "Hello 世界"}}
        encoded = encode_message(message)

        # Content-Length should match UTF-8 byte length, not character count
        header_end = encoded.index(b"\r\n\r\n")
        header = encoded[:header_end].decode("utf-8")
        body = encoded[header_end + 4 :]

        content_length = int(header.split(": ")[1])
        assert len(body) == content_length

        # Verify unicode is preserved
        decoded_body = json.loads(body)
        assert decoded_body["body"]["text"] == "Hello 世界"

    def test_decode_simple_message(self):
        """Test decoding a DAP message from bytes."""

        body = json.dumps({"seq": 1, "type": "request", "command": "initialize"})
        body_bytes = body.encode("utf-8")
        message = (
            f"Content-Length: {len(body_bytes)}\r\n\r\n".encode("utf-8") + body_bytes
        )

        stream = BytesIO(message)
        decoded = decode_message(stream)

        assert decoded["seq"] == 1
        assert decoded["type"] == "request"
        assert decoded["command"] == "initialize"

    def test_decode_message_with_extra_headers(self):
        """Test decoding with additional headers (should ignore unknown headers)."""

        body = json.dumps({"seq": 1, "type": "request"})
        body_bytes = body.encode("utf-8")
        message = (
            f"Content-Length: {len(body_bytes)}\r\n"
            f"Content-Type: application/json\r\n"
            f"\r\n"
        ).encode("utf-8") + body_bytes

        stream = BytesIO(message)
        decoded = decode_message(stream)

        assert decoded["seq"] == 1

    def test_decode_invalid_json(self):
        """Test decoding with invalid JSON body raises error."""

        body_bytes = b"{invalid json}"
        message = (
            f"Content-Length: {len(body_bytes)}\r\n\r\n".encode("utf-8") + body_bytes
        )

        stream = BytesIO(message)
        with pytest.raises(json.JSONDecodeError):
            decode_message(stream)

    def test_decode_incomplete_message(self):
        """Test decoding with incomplete message raises error."""

        # Message claims 100 bytes but only provides 10
        message = b'Content-Length: 100\r\n\r\n{"seq": 1}'

        stream = BytesIO(message)
        with pytest.raises(EOFError):
            decode_message(stream)

    def test_decode_missing_content_length(self):
        """Test decoding without Content-Length header raises error."""

        message = b'\r\n\r\n{"seq": 1}'

        stream = BytesIO(message)
        with pytest.raises(ValueError, match="Content-Length"):
            decode_message(stream)


class TestDAPMessageClass:
    """Test the DAPMessage wrapper class."""

    def test_create_request_message(self):
        """Test creating a request message."""

        msg = DAPMessage.request(
            seq=1, command="initialize", arguments={"adapterID": "pylog"}
        )

        assert msg.seq == 1
        assert msg.type == "request"
        assert msg.command == "initialize"
        assert msg.arguments == {"adapterID": "pylog"}

    def test_create_response_message(self):
        """Test creating a response message."""

        msg = DAPMessage.response(
            seq=2,
            request_seq=1,
            command="initialize",
            success=True,
            body={"supportsStepBack": False},
        )

        assert msg.seq == 2
        assert msg.type == "response"
        assert msg.request_seq == 1
        assert msg.success is True
        assert msg.body == {"supportsStepBack": False}

    def test_create_event_message(self):
        """Test creating an event message."""

        msg = DAPMessage.create_event(
            seq=3, event_name="stopped", body={"reason": "breakpoint"}
        )

        assert msg.seq == 3
        assert msg.type == "event"
        assert msg.event == "stopped"
        assert msg.body == {"reason": "breakpoint"}

    def test_message_to_dict(self):
        """Test converting message to dict for encoding."""

        msg = DAPMessage.request(seq=1, command="initialize")
        d = msg.to_dict()

        assert d["seq"] == 1
        assert d["type"] == "request"
        assert d["command"] == "initialize"

    def test_message_from_dict(self):
        """Test creating message from decoded dict."""

        d = {
            "seq": 1,
            "type": "request",
            "command": "initialize",
            "arguments": {"adapterID": "pylog"},
        }
        msg = DAPMessage.from_dict(d)

        assert msg.seq == 1
        assert msg.type == "request"
        assert msg.command == "initialize"
