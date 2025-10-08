"""Tests for DAP lifecycle request handlers (initialize, launch, disconnect).

These tests focus on actual behavior rather than mocking internal implementation details.
"""

import pytest
from prolog.dap.handlers import handle_initialize, handle_launch, handle_disconnect
from prolog.dap.session import reset_session, get_session


@pytest.fixture(autouse=True)
def reset_dap_session():
    """Reset DAP session before each test."""
    reset_session()
    yield
    reset_session()


class TestInitializeHandler:
    """Test the initialize request handler."""

    def test_initialize_returns_capabilities(self):
        """Test that initialize returns correct DAP capabilities."""

        request = {
            "seq": 1,
            "type": "request",
            "command": "initialize",
            "arguments": {"clientID": "vscode", "adapterID": "pylog"},
        }

        response = handle_initialize(request)

        # Should return capabilities dict (DAP body format)
        assert isinstance(response, dict)

        # MVP capabilities we support
        assert response.get("supportsConfigurationDoneRequest") is True

        # Capabilities we don't support in MVP
        assert response.get("supportsFunctionBreakpoints") is False
        assert response.get("supportsConditionalBreakpoints") is False
        assert response.get("supportsStepBack") is False
        assert response.get("supportsRestartFrame") is False

    def test_initialize_without_arguments(self):
        """Test initialize without arguments still works."""

        request = {"seq": 1, "type": "request", "command": "initialize"}

        response = handle_initialize(request)

        # Should still return capabilities
        assert isinstance(response, dict)
        assert "supportsConfigurationDoneRequest" in response

    def test_initialize_idempotent(self):
        """Test that calling initialize multiple times is safe."""

        request = {"seq": 1, "type": "request", "command": "initialize"}

        # First initialize
        response1 = handle_initialize(request)
        assert isinstance(response1, dict)

        # Second initialize should return same capabilities
        response2 = handle_initialize(request)
        assert response1 == response2


class TestLaunchHandler:
    """Test the launch request handler."""

    def test_launch_with_program_and_query(self, tmp_path):
        """Test launching with a program file and query."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        # Create a test program file
        program_file = tmp_path / "test.pl"
        program_file.write_text(
            "append([], L, L).\nappend([H|T], L, [H|R]) :- append(T, L, R).\n"
        )

        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {
                "program": str(program_file),
                "query": "?- append([1,2], [3,4], X).",
                "stopOnEntry": True,
            },
        }

        response = handle_launch(request)

        # Should return empty dict on success
        assert response == {}

    def test_launch_requires_program(self):
        """Test that launch requires a program argument."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {"query": "?- test."},
        }

        with pytest.raises(ValueError, match="program"):
            handle_launch(request)

    def test_launch_with_invalid_program_path(self):
        """Test that launch fails gracefully with invalid program path."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {"program": "/nonexistent/file.pl", "query": "?- test."},
        }

        with pytest.raises(FileNotFoundError):
            handle_launch(request)

    def test_launch_with_stopOnEntry_false(self, tmp_path):
        """Test launching without stopping on entry."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        program_file = tmp_path / "facts.pl"
        program_file.write_text("parent(tom, bob).\nparent(tom, liz).\n")

        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {
                "program": str(program_file),
                "query": "?- parent(X, Y).",
                "stopOnEntry": False,
            },
        }

        response = handle_launch(request)

        assert response == {}

    def test_launch_with_engine_options(self, tmp_path):
        """Test launching with engine configuration options."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        program_file = tmp_path / "test.pl"
        program_file.write_text("test :- true.\n")

        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {
                "program": str(program_file),
                "query": "?- test.",
                "occursCheck": True,
                "useIndexing": True,
                "ports": ["CALL", "EXIT", "FAIL"],
            },
        }

        response = handle_launch(request)

        assert response == {}

    def test_launch_creates_engine_and_components(self, tmp_path):
        """Test that launch actually creates engine, step controller, and breakpoint store."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        program_file = tmp_path / "test.pl"
        program_file.write_text("test :- true.\n")

        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {
                "program": str(program_file),
                "stopOnEntry": True,
            },
        }

        response = handle_launch(request)
        assert response == {}

        # Verify actual state changes
        session = get_session()
        assert session.engine is not None, "Engine should be created"
        assert session.step_controller is not None, "StepController should be created"
        assert session.breakpoint_store is not None, "BreakpointStore should be created"

        # Verify tracer integration
        assert (
            session.engine.tracer is not None
        ), "Engine should have tracer when trace=True"
        assert (
            session.engine.tracer.step_controller is session.step_controller
        ), "Tracer should be wired to step controller"
        assert (
            session.engine.tracer.breakpoint_store is session.breakpoint_store
        ), "Tracer should be wired to breakpoint store"

    def test_launch_with_ports_configures_step_controller(self, tmp_path):
        """Test that ports argument is passed to StepController."""

        # Initialize session first
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        program_file = tmp_path / "test.pl"
        program_file.write_text("test :- true.\n")

        ports = ["CALL", "EXIT"]
        request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {
                "program": str(program_file),
                "ports": ports,
            },
        }

        response = handle_launch(request)
        assert response == {}

        # Verify ports were passed to StepController
        session = get_session()
        assert session.step_controller.eligible_ports == {
            "CALL",
            "EXIT",
        }, "StepController should have ports configured"


class TestDisconnectHandler:
    """Test the disconnect request handler."""

    def test_disconnect_returns_success(self):
        """Test that disconnect returns success response."""

        request = {"seq": 3, "type": "request", "command": "disconnect"}

        response = handle_disconnect(request)

        # Should return empty dict (success)
        assert response == {}

    def test_disconnect_idempotent(self):
        """Test that calling disconnect multiple times is safe."""

        request = {"seq": 3, "type": "request", "command": "disconnect"}

        # First disconnect
        response1 = handle_disconnect(request)
        assert response1 == {}

        # Second disconnect should also succeed (idempotent)
        response2 = handle_disconnect(request)
        assert response2 == {}


class TestLifecycleIntegration:
    """Test the full lifecycle flow."""

    def test_initialize_launch_disconnect_sequence(self, tmp_path):
        """Test the complete lifecycle sequence."""

        # Initialize
        init_request = {
            "seq": 1,
            "type": "request",
            "command": "initialize",
            "arguments": {"adapterID": "pylog"},
        }

        init_response = handle_initialize(init_request)
        assert "supportsConfigurationDoneRequest" in init_response

        # Launch
        program_file = tmp_path / "test.pl"
        program_file.write_text("test :- true.\n")

        launch_request = {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {"program": str(program_file), "query": "?- test."},
        }

        launch_response = handle_launch(launch_request)
        assert launch_response == {}

        # Disconnect
        disconnect_request = {"seq": 3, "type": "request", "command": "disconnect"}

        disconnect_response = handle_disconnect(disconnect_request)
        assert disconnect_response == {}

    def test_launch_before_initialize_fails(self, tmp_path):
        """Test that launching before initialize fails."""

        program_file = tmp_path / "test.pl"
        program_file.write_text("test :- true.\n")

        # Try to launch without initialize
        launch_request = {
            "seq": 1,
            "type": "request",
            "command": "launch",
            "arguments": {"program": str(program_file), "query": "?- test."},
        }

        with pytest.raises(RuntimeError, match="initialize"):
            handle_launch(launch_request)
