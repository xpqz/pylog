"""Tests for DAP setBreakpoints request handler.

Tests for managing predicate breakpoints in the DAP server.
These tests follow TDD principles and verify actual behavior.
"""

import pytest
from prolog.dap.handlers import (
    handle_initialize,
    handle_launch,
    handle_set_breakpoints,
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
    program_file.write_text("member(X, [X|_]).\nmember(X, [_|T]) :- member(X, T).\n")

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


class TestSetBreakpointsHandler:
    """Test the setBreakpoints request handler."""

    def test_set_function_breakpoints(self, initialized_session):
        """Test setting function breakpoints (predicate breakpoints)."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2"},
                    {"name": "append/3"},
                ]
            },
        }

        response = handle_set_breakpoints(request)

        # Should return breakpoints with verified=true
        assert "breakpoints" in response
        assert len(response["breakpoints"]) == 2
        assert all(bp["verified"] for bp in response["breakpoints"])

        # Verify breakpoints are stored
        bp_store = initialized_session.breakpoint_store
        assert bp_store.has_breakpoint("member", 2)
        assert bp_store.has_breakpoint("append", 3)

    def test_set_breakpoints_with_port_filter(self, initialized_session):
        """Test setting breakpoints with port filters."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2", "condition": "port=CALL"},
                ]
            },
        }

        response = handle_set_breakpoints(request)

        assert "breakpoints" in response
        assert len(response["breakpoints"]) == 1
        assert response["breakpoints"][0]["verified"]

        # Verify breakpoint is stored with port filter
        bp_store = initialized_session.breakpoint_store
        assert bp_store.has_breakpoint("member", 2)

    def test_clear_all_breakpoints(self, initialized_session):
        """Test clearing all breakpoints by sending empty list."""

        # First, set some breakpoints
        request1 = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2"},
                    {"name": "append/3"},
                ]
            },
        }
        handle_set_breakpoints(request1)

        # Verify they're set
        bp_store = initialized_session.breakpoint_store
        assert bp_store.has_breakpoint("member", 2)
        assert bp_store.has_breakpoint("append", 3)

        # Clear all breakpoints
        request2 = {
            "seq": 4,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {"breakpoints": []},
        }
        response = handle_set_breakpoints(request2)

        assert response["breakpoints"] == []
        assert not bp_store.has_breakpoint("member", 2)
        assert not bp_store.has_breakpoint("append", 3)

    def test_replace_breakpoints(self, initialized_session):
        """Test that setBreakpoints replaces (not adds to) existing breakpoints."""

        # Set initial breakpoints
        request1 = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {"breakpoints": [{"name": "member/2"}]},
        }
        handle_set_breakpoints(request1)

        bp_store = initialized_session.breakpoint_store
        assert bp_store.has_breakpoint("member", 2)

        # Replace with different breakpoints
        request2 = {
            "seq": 4,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {"breakpoints": [{"name": "append/3"}]},
        }
        response = handle_set_breakpoints(request2)

        # Old breakpoint should be gone, new one should exist
        assert not bp_store.has_breakpoint("member", 2)
        assert bp_store.has_breakpoint("append", 3)
        assert len(response["breakpoints"]) == 1

    def test_set_breakpoints_with_invalid_format(self, initialized_session):
        """Test handling of invalid breakpoint name format."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "invalid"},  # Missing /arity
                ]
            },
        }

        response = handle_set_breakpoints(request)

        # Should return unverified breakpoint
        assert "breakpoints" in response
        assert len(response["breakpoints"]) == 1
        assert not response["breakpoints"][0]["verified"]
        assert "message" in response["breakpoints"][0]

    def test_set_breakpoints_with_invalid_arity(self, initialized_session):
        """Test handling of invalid arity."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/abc"},  # Non-numeric arity
                ]
            },
        }

        response = handle_set_breakpoints(request)

        # Should return unverified breakpoint
        assert "breakpoints" in response
        assert len(response["breakpoints"]) == 1
        assert not response["breakpoints"][0]["verified"]

    def test_set_breakpoints_before_launch_fails(self):
        """Test that setBreakpoints fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {"breakpoints": [{"name": "member/2"}]},
        }

        with pytest.raises(RuntimeError, match="No active debugging session"):
            handle_set_breakpoints(request)

    def test_set_breakpoints_without_arguments(self, initialized_session):
        """Test handling request without arguments (treat as clear all)."""

        # Set some breakpoints first
        request1 = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {"breakpoints": [{"name": "member/2"}]},
        }
        handle_set_breakpoints(request1)

        # Request without arguments should clear all
        request2 = {"seq": 4, "type": "request", "command": "setBreakpoints"}

        response = handle_set_breakpoints(request2)

        assert response["breakpoints"] == []
        assert not initialized_session.breakpoint_store.has_breakpoint("member", 2)

    def test_set_breakpoints_idempotent(self, initialized_session):
        """Test that setting same breakpoints multiple times is safe."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {"breakpoints": [{"name": "member/2"}]},
        }

        # Set breakpoints twice
        response1 = handle_set_breakpoints(request)
        response2 = handle_set_breakpoints(request)

        # Both should succeed (IDs may differ but breakpoints should be set)
        assert len(response1["breakpoints"]) == 1
        assert len(response2["breakpoints"]) == 1
        assert response1["breakpoints"][0]["verified"]
        assert response2["breakpoints"][0]["verified"]
        assert initialized_session.breakpoint_store.has_breakpoint("member", 2)

    def test_set_breakpoints_returns_ids(self, initialized_session):
        """Test that setBreakpoints returns IDs for each breakpoint."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2"},
                    {"name": "append/3"},
                ]
            },
        }

        response = handle_set_breakpoints(request)

        # Each breakpoint should have an ID
        assert len(response["breakpoints"]) == 2
        for bp in response["breakpoints"]:
            assert "id" in bp
            assert isinstance(bp["id"], int)

        # IDs should be unique
        ids = [bp["id"] for bp in response["breakpoints"]]
        assert len(ids) == len(set(ids))


class TestBreakpointPortFilters:
    """Test port filter parsing and handling."""

    def test_parse_port_filter_from_condition(self, initialized_session):
        """Test parsing port filter from condition string."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2", "condition": "port=CALL"},
                    {"name": "append/3", "condition": "port=EXIT"},
                ]
            },
        }

        response = handle_set_breakpoints(request)

        assert all(bp["verified"] for bp in response["breakpoints"])

    def test_parse_multiple_ports(self, initialized_session):
        """Test parsing multiple ports from condition."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2", "condition": "port=CALL,EXIT"},
                ]
            },
        }

        response = handle_set_breakpoints(request)

        assert response["breakpoints"][0]["verified"]

    def test_invalid_port_filter_still_verifies(self, initialized_session):
        """Test that invalid port filter doesn't prevent verification."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "setBreakpoints",
            "arguments": {
                "breakpoints": [
                    {"name": "member/2", "condition": "invalid_condition"},
                ]
            },
        }

        response = handle_set_breakpoints(request)

        # Should still verify (ignoring invalid condition)
        assert response["breakpoints"][0]["verified"]
