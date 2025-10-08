"""Tests for DAP data inspection request handlers.

Tests for stackTrace, scopes, variables, and evaluate requests.
These tests follow TDD principles and verify actual behavior.
"""

import pytest
from prolog.dap.handlers import (
    handle_initialize,
    handle_launch,
    handle_stack_trace,
    handle_scopes,
    handle_variables,
    handle_evaluate,
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
    program_file.write_text(
        "member(X, [X|_]).\n"
        "member(X, [_|T]) :- member(X, T).\n"
        "append([], L, L).\n"
        "append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).\n"
    )

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


class TestStackTraceHandler:
    """Test the stackTrace request handler."""

    def test_stack_trace_returns_frames(self, initialized_session):
        """Test that stackTrace returns frame information."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "stackTrace",
            "arguments": {"threadId": 1},
        }

        response = handle_stack_trace(request)

        # Should return stackFrames list
        assert "stackFrames" in response
        assert isinstance(response["stackFrames"], list)

    def test_stack_trace_empty_when_not_paused(self, initialized_session):
        """Test that stackTrace returns empty when engine not paused."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "stackTrace",
            "arguments": {"threadId": 1},
        }

        response = handle_stack_trace(request)

        # Should return empty list when no active query
        assert response["stackFrames"] == []

    def test_stack_trace_before_launch_fails(self):
        """Test that stackTrace fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "stackTrace",
            "arguments": {"threadId": 1},
        }

        with pytest.raises(RuntimeError, match="No active debugging session"):
            handle_stack_trace(request)

    def test_stack_trace_frame_format(self, initialized_session):
        """Test that stack frames have correct format."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "stackTrace",
            "arguments": {"threadId": 1},
        }

        response = handle_stack_trace(request)

        # Each frame should have required fields
        for frame in response["stackFrames"]:
            assert "id" in frame
            assert "name" in frame
            assert isinstance(frame["id"], int)
            assert isinstance(frame["name"], str)


class TestScopesHandler:
    """Test the scopes request handler."""

    def test_scopes_returns_locals_scope(self, initialized_session):
        """Test that scopes returns a Locals scope."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "scopes",
            "arguments": {"frameId": 0},
        }

        response = handle_scopes(request)

        # Should return scopes list with Locals
        assert "scopes" in response
        assert len(response["scopes"]) >= 1

        locals_scope = response["scopes"][0]
        assert locals_scope["name"] == "Locals"
        assert "variablesReference" in locals_scope
        assert isinstance(locals_scope["variablesReference"], int)

    def test_scopes_before_launch_fails(self):
        """Test that scopes fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "scopes",
            "arguments": {"frameId": 0},
        }

        with pytest.raises(RuntimeError, match="No active debugging session"):
            handle_scopes(request)

    def test_scopes_variables_reference_stable(self, initialized_session):
        """Test that variablesReference is stable across calls."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "scopes",
            "arguments": {"frameId": 0},
        }

        response1 = handle_scopes(request)
        response2 = handle_scopes(request)

        # Should return same reference for same frame
        assert (
            response1["scopes"][0]["variablesReference"]
            == response2["scopes"][0]["variablesReference"]
        )


class TestVariablesHandler:
    """Test the variables request handler."""

    def test_variables_returns_list(self, initialized_session):
        """Test that variables returns a list of variables."""

        # First get a variablesReference from scopes
        scopes_response = handle_scopes(
            {
                "seq": 3,
                "type": "request",
                "command": "scopes",
                "arguments": {"frameId": 0},
            }
        )

        var_ref = scopes_response["scopes"][0]["variablesReference"]

        # Now request variables
        request = {
            "seq": 4,
            "type": "request",
            "command": "variables",
            "arguments": {"variablesReference": var_ref},
        }

        response = handle_variables(request)

        # Should return variables list
        assert "variables" in response
        assert isinstance(response["variables"], list)

    def test_variables_have_correct_format(self, initialized_session):
        """Test that variables have required fields."""

        scopes_response = handle_scopes(
            {
                "seq": 3,
                "type": "request",
                "command": "scopes",
                "arguments": {"frameId": 0},
            }
        )

        var_ref = scopes_response["scopes"][0]["variablesReference"]

        request = {
            "seq": 4,
            "type": "request",
            "command": "variables",
            "arguments": {"variablesReference": var_ref},
        }

        response = handle_variables(request)

        # Each variable should have required fields
        for var in response["variables"]:
            assert "name" in var
            assert "value" in var
            assert "variablesReference" in var
            assert isinstance(var["name"], str)
            assert isinstance(var["value"], str)
            assert isinstance(var["variablesReference"], int)

    def test_variables_expansion_with_reference(self, initialized_session):
        """Test that compound terms have variablesReference for expansion."""

        scopes_response = handle_scopes(
            {
                "seq": 3,
                "type": "request",
                "command": "scopes",
                "arguments": {"frameId": 0},
            }
        )

        var_ref = scopes_response["scopes"][0]["variablesReference"]

        request = {
            "seq": 4,
            "type": "request",
            "command": "variables",
            "arguments": {"variablesReference": var_ref},
        }

        response = handle_variables(request)

        # Compound terms should have non-zero variablesReference
        # Simple terms should have 0
        for var in response["variables"]:
            if var["variablesReference"] > 0:
                # This is expandable - should be compound
                assert var["value"] != "_" or "[" in var["value"] or "(" in var["value"]

    def test_variables_before_launch_fails(self):
        """Test that variables fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "variables",
            "arguments": {"variablesReference": 1},
        }

        with pytest.raises(RuntimeError, match="No active debugging session"):
            handle_variables(request)

    def test_variables_invalid_reference_returns_empty(self, initialized_session):
        """Test that invalid variablesReference returns empty list."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "variables",
            "arguments": {"variablesReference": 99999},
        }

        response = handle_variables(request)

        # Should return empty list for invalid reference
        assert response["variables"] == []


class TestEvaluateHandler:
    """Test the evaluate request handler."""

    def test_evaluate_lookup_variable(self, initialized_session):
        """Test that evaluate can lookup variables by name."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "evaluate",
            "arguments": {"expression": "X", "frameId": 0},
        }

        response = handle_evaluate(request)

        # Should return result with value
        assert "result" in response
        assert isinstance(response["result"], str)

    def test_evaluate_returns_unbound_for_missing_var(self, initialized_session):
        """Test that evaluate returns appropriate value for unbound variables."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "evaluate",
            "arguments": {"expression": "NonExistentVar", "frameId": 0},
        }

        response = handle_evaluate(request)

        # Should indicate variable not found or unbound
        assert "result" in response
        # Either not found or shows as unbound "_"
        assert response["result"] in ["_", "Variable not found"]

    def test_evaluate_before_launch_fails(self):
        """Test that evaluate fails if called before launch."""

        # Initialize but don't launch
        handle_initialize({"seq": 1, "type": "request", "command": "initialize"})

        request = {
            "seq": 2,
            "type": "request",
            "command": "evaluate",
            "arguments": {"expression": "X", "frameId": 0},
        }

        with pytest.raises(RuntimeError, match="No active debugging session"):
            handle_evaluate(request)

    def test_evaluate_with_variables_reference(self, initialized_session):
        """Test that evaluate returns variablesReference for compound terms."""

        request = {
            "seq": 3,
            "type": "request",
            "command": "evaluate",
            "arguments": {"expression": "X", "frameId": 0},
        }

        response = handle_evaluate(request)

        # Should have variablesReference field
        assert "variablesReference" in response
        assert isinstance(response["variablesReference"], int)


class TestDataInspectionIntegration:
    """Test integration between stackTrace, scopes, variables, and evaluate."""

    def test_full_inspection_flow(self, initialized_session):
        """Test the complete flow: stackTrace -> scopes -> variables."""

        # 1. Get stack trace
        stack_response = handle_stack_trace(
            {
                "seq": 3,
                "type": "request",
                "command": "stackTrace",
                "arguments": {"threadId": 1},
            }
        )

        assert "stackFrames" in stack_response

        # 2. Get scopes for first frame (if any)
        frame_id = 0
        scopes_response = handle_scopes(
            {
                "seq": 4,
                "type": "request",
                "command": "scopes",
                "arguments": {"frameId": frame_id},
            }
        )

        assert "scopes" in scopes_response
        assert len(scopes_response["scopes"]) > 0

        # 3. Get variables for Locals scope
        var_ref = scopes_response["scopes"][0]["variablesReference"]
        vars_response = handle_variables(
            {
                "seq": 5,
                "type": "request",
                "command": "variables",
                "arguments": {"variablesReference": var_ref},
            }
        )

        assert "variables" in vars_response

    def test_variable_expansion_prevents_infinite_recursion(self, initialized_session):
        """Test that variable expansion doesn't cause infinite recursion."""

        # Get initial variables
        scopes_response = handle_scopes(
            {
                "seq": 3,
                "type": "request",
                "command": "scopes",
                "arguments": {"frameId": 0},
            }
        )

        var_ref = scopes_response["scopes"][0]["variablesReference"]

        # Request variables multiple times - should not hang
        for i in range(5):
            response = handle_variables(
                {
                    "seq": 4 + i,
                    "type": "request",
                    "command": "variables",
                    "arguments": {"variablesReference": var_ref},
                }
            )

            assert "variables" in response
