"""Tests for DAP stepping control request handlers.

Tests for continue, next (step over), stepIn, and stepOut requests.
These tests follow TDD principles and verify actual behavior.
"""

import pytest
from prolog.dap.handlers import (
    handle_initialize,
    handle_launch,
    handle_continue,
    handle_next,
    handle_step_in,
    handle_step_out,
)
from prolog.dap.session import reset_session, get_session


@pytest.fixture(autouse=True)
def reset_dap_session():
    """Reset DAP session before each test."""
    reset_session()
    yield
    reset_session()


@pytest.fixture
def initialized_session(tmp_path):
    """Create an initialized session with a launched program."""
    # Initialize
    handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

    # Create a test program
    program_file = tmp_path / "test.pl"
    program_file.write_text("test :- true.\n")

    # Launch
    handle_launch(
        {
            "seq": 2,
            "type": "request",
            "command": "launch",
            "arguments": {"program": str(program_file), "stopOnEntry": True},
        }
    )

    return get_session()


class TestContinueHandler:
    """Test the continue request handler."""

    def test_continue_sets_running_mode(self, initialized_session):
        """Test that continue sets the step controller to running mode."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "continue",
            "arguments": {"threadId": 1},
        }

        response = handle_continue(request)

        # Should return empty dict on success
        assert response == {}

        # Verify step controller mode is set to running
        assert initialized_session.step_controller.get_mode() == "running"

    def test_continue_without_arguments(self, initialized_session):
        """Test that continue works without arguments."""

        request = {"seq": 3, "type": "request", "command": "continue"}

        response = handle_continue(request)

        # Should still succeed
        assert response == {}
        assert initialized_session.step_controller.get_mode() == "running"

    def test_continue_before_launch_fails(self):
        """Test that continue fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "continue",
            "arguments": {"threadId": 1},
        }

        with pytest.raises(RuntimeError, match="Cannot continue"):
            handle_continue(request)


class TestNextHandler:
    """Test the next (step over) request handler."""

    def test_next_sets_step_over_mode(self, initialized_session):
        """Test that next sets the step controller to step_over mode."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "next",
            "arguments": {"threadId": 1},
        }

        response = handle_next(request)

        # Should return empty dict on success
        assert response == {}

        # Verify step controller mode is set to step_over
        assert initialized_session.step_controller.get_mode() == "step_over"

    def test_next_captures_current_depth(self, initialized_session):
        """Test that next captures the current call stack depth."""

        # This test verifies that the depth parameter is set
        # For now, we'll set depth to 0 as a baseline (no active query yet)
        # When query execution is implemented, this should capture actual depth

        request = {
            "seq": 3,
            "type": "request",
            "command": "next",
            "arguments": {"threadId": 1},
        }

        response = handle_next(request)

        assert response == {}
        # Step controller should have baseline depth set
        assert initialized_session.step_controller._baseline_depth == 0

    def test_next_before_launch_fails(self):
        """Test that next fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "next",
            "arguments": {"threadId": 1},
        }

        with pytest.raises(RuntimeError, match="Cannot step over"):
            handle_next(request)


class TestStepInHandler:
    """Test the stepIn request handler."""

    def test_step_in_sets_step_in_mode(self, initialized_session):
        """Test that stepIn sets the step controller to step_in mode."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "stepIn",
            "arguments": {"threadId": 1},
        }

        response = handle_step_in(request)

        # Should return empty dict on success
        assert response == {}

        # Verify step controller mode is set to step_in
        assert initialized_session.step_controller.get_mode() == "step_in"

    def test_step_in_without_arguments(self, initialized_session):
        """Test that stepIn works without arguments."""

        request = {"seq": 3, "type": "request", "command": "stepIn"}

        response = handle_step_in(request)

        # Should still succeed
        assert response == {}
        assert initialized_session.step_controller.get_mode() == "step_in"

    def test_step_in_before_launch_fails(self):
        """Test that stepIn fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "stepIn",
            "arguments": {"threadId": 1},
        }

        with pytest.raises(RuntimeError, match="Cannot step in"):
            handle_step_in(request)


class TestStepOutHandler:
    """Test the stepOut request handler."""

    def test_step_out_sets_step_out_mode(self, initialized_session):
        """Test that stepOut sets the step controller to step_out mode."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "stepOut",
            "arguments": {"threadId": 1},
        }

        response = handle_step_out(request)

        # Should return empty dict on success
        assert response == {}

        # Verify step controller mode is set to step_out
        assert initialized_session.step_controller.get_mode() == "step_out"

    def test_step_out_captures_current_depth(self, initialized_session):
        """Test that stepOut captures the current call stack depth."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "stepOut",
            "arguments": {"threadId": 1},
        }

        response = handle_step_out(request)

        assert response == {}
        # Step controller should have baseline depth set
        assert initialized_session.step_controller._baseline_depth == 0

    def test_step_out_before_launch_fails(self):
        """Test that stepOut fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "stepOut",
            "arguments": {"threadId": 1},
        }

        with pytest.raises(RuntimeError, match="Cannot step out"):
            handle_step_out(request)


class TestSteppingIntegration:
    """Test the interaction between different stepping commands."""

    def test_stepping_mode_transitions(self, initialized_session):
        """Test that stepping modes transition correctly."""

        # Start with paused (from stopOnEntry)
        assert initialized_session.step_controller.get_mode() == "paused"

        # Continue -> running
        handle_continue({"seq": 3, "type": "request", "command": "continue"})
        assert initialized_session.step_controller.get_mode() == "running"

        # Step in -> step_in
        handle_step_in({"seq": 4, "type": "request", "command": "stepIn"})
        assert initialized_session.step_controller.get_mode() == "step_in"

        # Next -> step_over
        handle_next({"seq": 5, "type": "request", "command": "next"})
        assert initialized_session.step_controller.get_mode() == "step_over"

        # Step out -> step_out
        handle_step_out({"seq": 6, "type": "request", "command": "stepOut"})
        assert initialized_session.step_controller.get_mode() == "step_out"

        # Back to continue -> running
        handle_continue({"seq": 7, "type": "request", "command": "continue"})
        assert initialized_session.step_controller.get_mode() == "running"
