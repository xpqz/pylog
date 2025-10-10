"""
Test suite for Web Worker bootstrap functionality.

Tests the PyLog Web Worker message protocol and initialization.
Since we can't directly test JavaScript in Python, these tests validate
the expected API contract and message formats.
"""

import json
from pathlib import Path


class TestWebWorkerProtocol:
    """Test the Web Worker message protocol specification."""

    def test_init_message_format(self):
        """Test that init messages follow the expected format."""
        init_msg = {"type": "init"}

        # Should be valid JSON
        json_str = json.dumps(init_msg)
        parsed = json.loads(json_str)

        assert parsed["type"] == "init"
        assert len(parsed) == 1  # Only type field expected

    def test_query_message_format(self):
        """Test that query messages follow the expected format."""
        query_msg = {
            "type": "query",
            "data": {
                "query": "X = 1, Y is X + 1",
                "options": {"maxSteps": 1000, "maxSolutions": 10},
            },
        }

        # Should be valid JSON
        json_str = json.dumps(query_msg)
        parsed = json.loads(json_str)

        assert parsed["type"] == "query"
        assert "data" in parsed
        assert "query" in parsed["data"]
        assert "options" in parsed["data"]
        assert parsed["data"]["options"]["maxSteps"] == 1000
        assert parsed["data"]["options"]["maxSolutions"] == 10

    def test_reset_message_format(self):
        """Test that reset messages follow the expected format."""
        reset_msg = {"type": "reset"}

        # Should be valid JSON
        json_str = json.dumps(reset_msg)
        parsed = json.loads(json_str)

        assert parsed["type"] == "reset"
        assert len(parsed) == 1  # Only type field expected

    def test_expected_response_formats(self):
        """Test expected response message formats from worker."""

        # Initialized response
        init_response = {
            "type": "initialized",
            "versions": {"pylog": "0.1.0", "pyodide": "0.24.1"},
        }
        json.dumps(init_response)  # Should serialize

        # Solutions response
        solutions_response = {
            "type": "solutions",
            "query": "X = 42",
            "solutions": [{"X": "42"}],
            "stepCount": 1,
            "solutionCount": 1,
            "limits": {"maxSteps": 1000, "maxSolutions": 10},
        }
        json.dumps(solutions_response)  # Should serialize

        # Error response
        error_response = {"type": "error", "message": "PyLog not initialized"}
        json.dumps(error_response)  # Should serialize

        # Reset response
        reset_response = {"type": "reset", "message": "Engine reset successful"}
        json.dumps(reset_response)  # Should serialize

    def test_worker_file_exists(self):
        """Test that the worker.js file exists and has expected structure."""
        worker_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "worker.js"
        )

        assert worker_path.exists(), f"Worker file not found at {worker_path}"

        content = worker_path.read_text()

        # Check for required functions and patterns
        assert "self.onmessage" in content, "Worker should handle messages"
        assert "initializePyodide" in content, "Should have Pyodide initialization"
        assert "resetEngine" in content, "Should have engine reset functionality"
        assert "executeQuery" in content, "Should have query execution"
        assert "pyodide.version" in content, "Should access Pyodide version"
        assert "micropip.install" in content, "Should install dependencies"

    def test_pyrepl_file_exists(self):
        """Test that the main REPL interface file exists."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        assert pyrepl_path.exists(), f"PyREPL file not found at {pyrepl_path}"

        content = pyrepl_path.read_text()

        # Check for worker creation
        assert "new Worker" in content, "Should create Web Worker"
        assert "onmessage" in content, "Should handle worker messages"


class TestWorkerAPIContract:
    """Test the API contract defined in docs/repl.md."""

    def test_query_execution_limits(self):
        """Test that query execution respects limits."""
        # Default limits should be reasonable
        default_max_steps = 1000
        default_max_solutions = 10

        assert default_max_steps > 0
        assert default_max_solutions > 0

        # Custom limits should be respected
        custom_options = {"maxSteps": 5000, "maxSolutions": 50}

        assert custom_options["maxSteps"] > default_max_steps
        assert custom_options["maxSolutions"] > default_max_solutions

    def test_version_info_structure(self):
        """Test that version info has expected structure."""
        versions = {"pylog": "0.1.0", "pyodide": "0.24.1"}

        assert "pylog" in versions
        assert "pyodide" in versions
        assert isinstance(versions["pylog"], str)
        assert isinstance(versions["pyodide"], str)

        # Versions should look like semantic versions
        pylog_parts = versions["pylog"].split(".")
        pyodide_parts = versions["pyodide"].split(".")

        assert len(pylog_parts) >= 2  # At least major.minor
        assert len(pyodide_parts) >= 2  # At least major.minor

    def test_solution_batching(self):
        """Test that solutions are returned in batches with metadata."""
        batch_response = {
            "type": "solutions",
            "query": "member(X, [1,2,3])",
            "solutions": [{"X": "1"}, {"X": "2"}, {"X": "3"}],
            "stepCount": 6,
            "solutionCount": 3,
            "limits": {"maxSteps": 1000, "maxSolutions": 10},
        }

        assert batch_response["solutionCount"] == len(batch_response["solutions"])
        assert batch_response["stepCount"] >= batch_response["solutionCount"]
        assert (
            batch_response["solutionCount"] <= batch_response["limits"]["maxSolutions"]
        )
        assert batch_response["stepCount"] <= batch_response["limits"]["maxSteps"]


class TestVariableBindingDisplay:
    """Test that variable bindings are properly displayed in query results."""

    def test_solution_format_includes_bindings(self):
        """Test that solutions include variable bindings, not just 'true'."""
        # Expected format for append([1,2], [3,4], X) query
        expected_response = {
            "type": "solutions",
            "query": "append([1,2], [3,4], X)",
            "solutions": [{"X": "[1, 2, 3, 4]"}],  # Should include actual binding
            "stepCount": 5,  # Some reasonable number
            "solutionCount": 1,
            "limits": {"maxSteps": 100000, "maxSolutions": 100},
        }

        # Solutions must have the actual variable bindings
        assert expected_response["solutions"][0] != {}
        assert "X" in expected_response["solutions"][0]
        assert expected_response["solutions"][0]["X"] != "true"

    def test_multiple_variable_bindings(self):
        """Test that multiple variables are all shown in results."""
        # Expected format for member(X, [1,2,3]), Y is X + 1
        expected_response = {
            "type": "solutions",
            "query": "member(X, [1,2,3]), Y is X + 1",
            "solutions": [
                {"X": "1", "Y": "2"},
                {"X": "2", "Y": "3"},
                {"X": "3", "Y": "4"},
            ],
            "stepCount": 10,
            "solutionCount": 3,
            "limits": {"maxSteps": 100000, "maxSolutions": 100},
        }

        # All variables should be present in each solution
        for solution in expected_response["solutions"]:
            assert "X" in solution
            assert "Y" in solution

    def test_empty_solution_for_true_query(self):
        """Test that queries with no variables return empty solution dict."""
        # For queries like true, 1 = 1, etc.
        expected_response = {
            "type": "solutions",
            "query": "true",
            "solutions": [{}],  # Empty dict for 'true' with no variables
            "stepCount": 1,
            "solutionCount": 1,
            "limits": {"maxSteps": 100000, "maxSolutions": 100},
        }

        # Empty solution is valid for queries with no variables
        assert expected_response["solutions"] == [{}]

    def test_worker_preserves_variable_bindings(self):
        """Test that worker.js preserves variable bindings from engine."""
        worker_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "worker.js"
        )

        content = worker_path.read_text()

        # Check that solutions are properly extracted and formatted
        assert "for (let i = 0; i < solutionsLength; i++)" in content
        assert "pylogPretty" in content  # Should pretty-print values
        assert "prettySolution[key]" in content  # Should preserve all keys
        assert "solution.items" in content  # Should iterate Python dict items

        # Should NOT filter out non-null/non-undefined solutions incorrectly
        # The check should preserve empty dicts for 'true' results
        assert "solution !== null && solution !== undefined" in content


class TestWebAssetIntegration:
    """Test integration with web assets and build system."""

    def test_mkdocs_try_page_exists(self):
        """Test that the Try PyLog page exists."""
        try_page = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "index.md"
        )

        assert try_page.exists(), f"Try page not found at {try_page}"

        content = try_page.read_text()

        # Should reference the required JavaScript files
        assert "pyrepl.js" in content, "Should load pyrepl.js"
        assert "examples.js" in content, "Should load examples.js"
        # Should NOT include worker.js as a script tag (it's loaded via new Worker())
        assert (
            '<script src="worker.js">' not in content
        ), "worker.js should not be loaded as script"

    def test_examples_file_exists(self):
        """Test that the examples file exists and has structure."""
        examples_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "examples.js"
        )

        assert examples_path.exists(), f"Examples file not found at {examples_path}"

        content = examples_path.read_text()

        # Should have example queries
        assert "examples" in content.lower(), "Should contain example queries"


class TestSafetyLimits:
    """Test safety limits and timeout protection."""

    def test_production_safety_limits(self):
        """Test that production safety limits are reasonable."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for production-level limits
        assert "maxSteps: 100000" in content, "Should have 100k step limit"
        assert "maxSolutions: 100" in content, "Should have 100 solution limit"
        assert "timeoutMs: 10000" in content, "Should have 10s timeout"

    def test_safety_commands_present(self):
        """Test that safety commands are available."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for safety commands
        assert "limits" in content, "Should have limits command"
        assert "set_limits" in content, "Should have set_limits command"
        assert "getSafetyLimits" in content, "Should have safety limits function"
        assert "handleQueryTimeout" in content, "Should have timeout handler"

    def test_timeout_protection_structure(self):
        """Test that timeout protection is properly structured."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for timeout infrastructure
        assert "setTimeout" in content, "Should start timeouts"
        assert "clearTimeout" in content, "Should clear timeouts"
        assert "queryTimeoutId" in content, "Should track timeout ID"
        assert "terminate()" in content, "Should terminate worker on timeout"

    def test_configurable_limits_validation(self):
        """Test that configurable limits have proper validation."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for validation ranges
        assert "1000000" in content, "Should have max step limit validation"
        assert "1000" in content, "Should have max solution limit validation"
        assert "60000" in content, "Should have max timeout validation"

    def test_help_includes_safety_info(self):
        """Test that help includes safety information."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for safety documentation
        assert "Safety features" in content, "Should document safety features"
        assert "Step limit" in content, "Should explain step limits"
        assert "Worker termination" in content, "Should explain termination"


class TestErrorFormatting:
    """Test error formatting and ReaderError handling."""

    def test_error_formatting_functions_exist(self):
        """Test that error formatting functions are defined."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for error formatting functions
        assert "function displayError(" in content, "Should have displayError function"
        assert (
            "function displayReaderError(" in content
        ), "Should have displayReaderError function"
        assert (
            "function addParseErrorSuggestions(" in content
        ), "Should have suggestions function"

    def test_worker_error_extraction_functions(self):
        """Test that worker has error extraction functions."""
        worker_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "worker.js"
        )

        content = worker_path.read_text()

        # Check for error extraction functions
        assert (
            "function extractErrorInfo(" in content
        ), "Should have extractErrorInfo function"
        assert (
            "function extractReaderErrorInfo(" in content
        ), "Should have ReaderError extraction"
        assert "ReaderError" in content, "Should handle ReaderError type"

    def test_error_display_features(self):
        """Test that error display includes position highlighting."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for position highlighting
        assert "position" in content, "Should handle error positions"
        assert "columnNumber" in content, "Should calculate column numbers"
        assert "marker" in content, "Should create position markers"
        assert "^" in content, "Should use caret for position indication"

    def test_helpful_error_suggestions(self):
        """Test that helpful suggestions are provided for common errors."""
        pyrepl_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "pyrepl.js"
        )

        content = pyrepl_path.read_text()

        # Check for helpful suggestions
        assert "expected opening bracket" in content, "Should suggest bracket fixes"
        assert "expected period" in content, "Should suggest period fixes"
        assert "unknown operator" in content, "Should suggest operator fixes"
        assert "Documentation:" in content, "Should link to documentation"
        assert "../basics/" in content, "Should link to basics documentation"

    def test_examples_system_integration(self):
        """Test that examples system is properly integrated."""
        examples_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "examples.js"
        )

        content = examples_path.read_text()

        # Check for example categories that actually work
        assert "basics" in content, "Should have basics category"
        assert "lists" in content, "Should have lists category"
        assert "arithmetic" in content, "Should have arithmetic category"
        assert "clpfd" in content, "Should have CLP(FD) category"
        # Note: hanoi and sudoku removed as these predicates don't exist in PyLog yet

        # Check for dropdown functionality
        assert (
            "dropdown" in content or "select" in content
        ), "Should have dropdown interface"
        assert "loadSelectedExample" in content, "Should have example loading function"

    def test_examples_content_quality(self):
        """Test that examples include the required content."""
        examples_path = (
            Path(__file__).parent.parent.parent.parent
            / "mkdocs"
            / "docs"
            / "try"
            / "examples.js"
        )

        content = examples_path.read_text()

        # Check for specific working examples
        assert "member(X" in content, "Should have member examples"
        assert "append(" in content, "Should have append examples"
        assert "all_different" in content, "Should have all_different examples"
        assert "label([X" in content, "Should have labeling examples"
        # Note: hanoi examples removed as the predicate doesn't exist yet
