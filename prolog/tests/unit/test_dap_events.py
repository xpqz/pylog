"""Tests for DAP event emission.

Tests for stopped, output, and terminated events.
These tests follow TDD principles and verify actual behavior.
"""

import pytest
import threading
import logging
from io import BytesIO
from prolog.dap.server import DAPServer
from prolog.dap.handlers import (
    handle_initialize,
    handle_launch,
    handle_continue,
    handle_disconnect,
)
from prolog.dap.session import reset_session, get_session
from prolog.dap.protocol import decode_message


@pytest.fixture(autouse=True)
def reset_dap_session():
    """Reset DAP session before each test."""
    reset_session()
    yield
    reset_session()


@pytest.fixture
def server_with_streams():
    """Create a DAP server with test streams."""
    stdin = BytesIO()
    stdout = BytesIO()
    server = DAPServer(stdin=stdin, stdout=stdout)
    return server, stdin, stdout


@pytest.fixture
def initialized_server(server_with_streams, tmp_path):
    """Create an initialized server with a launched program."""
    server, stdin, stdout = server_with_streams

    # Register handlers
    server.register_handler("initialize", handle_initialize)
    server.register_handler("launch", handle_launch)
    server.register_handler("continue", handle_continue)
    server.register_handler("disconnect", handle_disconnect)

    # Create a test program
    program_file = tmp_path / "test.pl"
    program_file.write_text("member(X, [X|_]).\n" "member(X, [_|T]) :- member(X, T).\n")

    # Initialize
    handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

    # Set server on session for event emission
    session = get_session()
    session.set_server(server)

    # Launch
    handle_launch(
        {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {"program": str(program_file), "stopOnEntry": True},
        }
    )

    return server, stdin, stdout


class TestStoppedEvent:
    """Test the stopped event emission."""

    def test_stopped_event_has_required_fields(self, initialized_server):
        """Test that stopped event includes all required fields."""
        server, stdin, stdout = initialized_server

        # Emit a stopped event
        server.send_event("stopped", {"reason": "breakpoint", "threadId": 1})

        # Read the message from stdout
        stdout.seek(0)
        message = decode_message(stdout)

        # Verify event structure
        assert message["type"] == "event"
        assert message["event"] == "stopped"
        assert "seq" in message
        assert "body" in message
        assert message["body"]["reason"] == "breakpoint"
        assert message["body"]["threadId"] == 1

    def test_stopped_event_reason_step(self, initialized_server):
        """Test stopped event with step reason."""
        server, stdin, stdout = initialized_server

        server.send_event("stopped", {"reason": "step", "threadId": 1})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["reason"] == "step"

    def test_stopped_event_reason_entry(self, initialized_server):
        """Test stopped event with entry reason (stopOnEntry)."""
        server, stdin, stdout = initialized_server

        server.send_event("stopped", {"reason": "entry", "threadId": 1})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["reason"] == "entry"

    def test_stopped_event_reason_pause(self, initialized_server):
        """Test stopped event with pause reason."""
        server, stdin, stdout = initialized_server

        server.send_event("stopped", {"reason": "pause", "threadId": 1})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["reason"] == "pause"

    def test_stopped_event_with_description(self, initialized_server):
        """Test stopped event with optional description field."""
        server, stdin, stdout = initialized_server

        server.send_event(
            "stopped",
            {
                "reason": "breakpoint",
                "description": "Paused on member/2 at CALL port",
                "threadId": 1,
            },
        )

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["description"] == "Paused on member/2 at CALL port"

    def test_stopped_event_with_hit_breakpoint_ids(self, initialized_server):
        """Test stopped event includes breakpoint IDs when hit."""
        server, stdin, stdout = initialized_server

        server.send_event(
            "stopped",
            {
                "reason": "breakpoint",
                "threadId": 1,
                "hitBreakpointIds": [1, 2],
            },
        )

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["hitBreakpointIds"] == [1, 2]

    def test_stopped_event_all_threads_stopped_flag(self, initialized_server):
        """Test stopped event with allThreadsStopped flag."""
        server, stdin, stdout = initialized_server

        server.send_event(
            "stopped",
            {
                "reason": "step",
                "threadId": 1,
                "allThreadsStopped": True,
            },
        )

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["allThreadsStopped"] is True


class TestOutputEvent:
    """Test the output event emission."""

    def test_output_event_has_required_fields(self, initialized_server):
        """Test that output event includes required output field."""
        server, stdin, stdout = initialized_server

        # Emit an output event
        server.send_event("output", {"output": "Hello from Prolog\n"})

        # Read the message from stdout
        stdout.seek(0)
        message = decode_message(stdout)

        # Verify event structure
        assert message["type"] == "event"
        assert message["event"] == "output"
        assert "seq" in message
        assert "body" in message
        assert message["body"]["output"] == "Hello from Prolog\n"

    def test_output_event_category_stdout(self, initialized_server):
        """Test output event with stdout category."""
        server, stdin, stdout = initialized_server

        server.send_event(
            "output", {"category": "stdout", "output": "Standard output\n"}
        )

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["category"] == "stdout"
        assert message["body"]["output"] == "Standard output\n"

    def test_output_event_category_stderr(self, initialized_server):
        """Test output event with stderr category."""
        server, stdin, stdout = initialized_server

        server.send_event("output", {"category": "stderr", "output": "Error output\n"})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["category"] == "stderr"

    def test_output_event_category_console(self, initialized_server):
        """Test output event with console category."""
        server, stdin, stdout = initialized_server

        server.send_event(
            "output", {"category": "console", "output": "Console message\n"}
        )

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["category"] == "console"

    def test_output_event_with_variables_reference(self, initialized_server):
        """Test output event can include variablesReference."""
        server, stdin, stdout = initialized_server

        server.send_event(
            "output",
            {
                "output": "Result: ",
                "variablesReference": 123,
            },
        )

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["variablesReference"] == 123

    def test_output_event_empty_output_string(self, initialized_server):
        """Test output event with empty string is valid."""
        server, stdin, stdout = initialized_server

        server.send_event("output", {"output": ""})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["output"] == ""

    def test_output_event_multiline_output(self, initialized_server):
        """Test output event with multi-line output."""
        server, stdin, stdout = initialized_server

        multiline_text = "Line 1\nLine 2\nLine 3\n"
        server.send_event("output", {"output": multiline_text})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["output"] == multiline_text


class TestTerminatedEvent:
    """Test the terminated event emission."""

    def test_terminated_event_basic(self, initialized_server):
        """Test that terminated event is properly formatted."""
        server, stdin, stdout = initialized_server

        # Emit a terminated event
        server.send_event("terminated")

        # Read the message from stdout
        stdout.seek(0)
        message = decode_message(stdout)

        # Verify event structure
        assert message["type"] == "event"
        assert message["event"] == "terminated"
        assert "seq" in message

    def test_terminated_event_with_no_body(self, initialized_server):
        """Test terminated event with no body (optional)."""
        server, stdin, stdout = initialized_server

        server.send_event("terminated", None)

        stdout.seek(0)
        message = decode_message(stdout)

        # Body should either be absent or empty
        assert message.get("body") is None or message.get("body") == {}

    def test_terminated_event_with_restart_hint(self, initialized_server):
        """Test terminated event with restart hint."""
        server, stdin, stdout = initialized_server

        server.send_event("terminated", {"restart": True})

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["restart"] is True

    def test_terminated_event_sequence_numbering(self, initialized_server):
        """Test that terminated event has unique sequence number."""
        server, stdin, stdout = initialized_server

        # Send multiple events
        server.send_event("output", {"output": "Done\n"})
        server.send_event("terminated")

        # Read messages
        stdout.seek(0)
        msg1 = decode_message(stdout)
        msg2 = decode_message(stdout)

        # Sequence numbers should be different and increasing
        assert msg1["seq"] < msg2["seq"]
        assert msg2["event"] == "terminated"


class TestEventThreadSafety:
    """Test thread-safe event emission."""

    def test_concurrent_event_emission(self, initialized_server):
        """Test that events can be sent from multiple threads safely."""
        server, stdin, stdout = initialized_server

        # Send events from multiple threads
        def send_events(event_type, count):
            for i in range(count):
                server.send_event(event_type, {"output": f"Event {i}\n"})

        threads = [
            threading.Thread(target=send_events, args=("output", 5)) for _ in range(3)
        ]

        for t in threads:
            t.start()

        for t in threads:
            t.join()

        # Read all messages
        stdout.seek(0)
        messages = []
        try:
            while True:
                messages.append(decode_message(stdout))
        except EOFError:
            pass

        # Should have 15 messages (3 threads * 5 events each)
        assert len(messages) == 15

        # All should be output events
        assert all(msg["event"] == "output" for msg in messages)

        # Sequence numbers should be unique
        seq_numbers = [msg["seq"] for msg in messages]
        assert len(seq_numbers) == len(set(seq_numbers))


class TestEventTiming:
    """Test event timing and ordering."""

    def test_events_sent_in_order(self, initialized_server):
        """Test that events are sent in the order they're called."""
        server, stdin, stdout = initialized_server

        # Send events in specific order
        server.send_event("output", {"output": "Starting\n"})
        server.send_event("stopped", {"reason": "step", "threadId": 1})
        server.send_event("output", {"output": "Stopped\n"})
        server.send_event("terminated")

        # Read messages
        stdout.seek(0)
        messages = []
        for _ in range(4):
            messages.append(decode_message(stdout))

        # Verify order
        assert messages[0]["event"] == "output"
        assert messages[0]["body"]["output"] == "Starting\n"
        assert messages[1]["event"] == "stopped"
        assert messages[2]["event"] == "output"
        assert messages[2]["body"]["output"] == "Stopped\n"
        assert messages[3]["event"] == "terminated"

    def test_sequence_numbers_monotonically_increasing(self, initialized_server):
        """Test that sequence numbers always increase."""
        server, stdin, stdout = initialized_server

        # Send mixed events
        for i in range(10):
            if i % 2 == 0:
                server.send_event("output", {"output": f"Message {i}\n"})
            else:
                server.send_event("stopped", {"reason": "step", "threadId": 1})

        # Read all messages
        stdout.seek(0)
        messages = []
        for _ in range(10):
            messages.append(decode_message(stdout))

        # Extract sequence numbers
        seq_numbers = [msg["seq"] for msg in messages]

        # Should be strictly increasing
        for i in range(1, len(seq_numbers)):
            assert seq_numbers[i] > seq_numbers[i - 1]


class TestSessionEventHelpers:
    """Test session manager event helper methods."""

    def test_session_send_stopped_event(self, initialized_server):
        """Test session manager's send_stopped_event helper."""
        server, stdin, stdout = initialized_server
        session = get_session()

        # Use session helper
        session.send_stopped_event("breakpoint", description="Hit member/2")

        # Read event
        stdout.seek(0)
        message = decode_message(stdout)

        assert message["event"] == "stopped"
        assert message["body"]["reason"] == "breakpoint"
        assert message["body"]["threadId"] == 1
        assert message["body"]["description"] == "Hit member/2"
        assert message["body"]["allThreadsStopped"] is True

    def test_session_send_stopped_event_with_breakpoints(self, initialized_server):
        """Test stopped event with breakpoint IDs."""
        server, stdin, stdout = initialized_server
        session = get_session()

        session.send_stopped_event("breakpoint", hit_breakpoint_ids=[5, 10])

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["hitBreakpointIds"] == [5, 10]

    def test_session_send_output_event(self, initialized_server):
        """Test session manager's send_output_event helper."""
        server, stdin, stdout = initialized_server
        session = get_session()

        # Use session helper
        session.send_output_event("Test output\n", category="console")

        # Read event
        stdout.seek(0)
        message = decode_message(stdout)

        assert message["event"] == "output"
        assert message["body"]["output"] == "Test output\n"
        assert message["body"]["category"] == "console"

    def test_session_send_output_event_default_category(self, initialized_server):
        """Test output event uses stdout as default category."""
        server, stdin, stdout = initialized_server
        session = get_session()

        session.send_output_event("Default\n")

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["category"] == "stdout"

    def test_session_send_terminated_event(self, initialized_server):
        """Test session manager's send_terminated_event helper."""
        server, stdin, stdout = initialized_server
        session = get_session()

        # Use session helper
        session.send_terminated_event()

        # Read event
        stdout.seek(0)
        message = decode_message(stdout)

        assert message["event"] == "terminated"

    def test_session_send_terminated_event_with_restart(self, initialized_server):
        """Test terminated event with restart hint."""
        server, stdin, stdout = initialized_server
        session = get_session()

        session.send_terminated_event(restart=True)

        stdout.seek(0)
        message = decode_message(stdout)

        assert message["body"]["restart"] is True

    def test_session_event_without_server_logs_warning(self, caplog):
        """Test that sending events without server logs warning."""
        session = get_session()
        session._server = None  # Ensure no server

        with caplog.at_level(logging.WARNING):
            session.send_event("stopped", {"reason": "test"})

        assert "Cannot send event 'stopped' - no server set" in caplog.text
