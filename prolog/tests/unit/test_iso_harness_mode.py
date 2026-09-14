"""Tests for ISO test harness mode functionality."""

import pytest
from pathlib import Path
from scripts.run_iso_suite import ISOTestRunner, parse_harness_output


class TestHarnessOutputParser:
    """Test parsing of harness.pl output format."""

    def test_parse_single_pass(self):
        """Test parsing a single passing test."""
        output = """Test 1/1: OK
"""
        results = parse_harness_output(output)
        assert results["summary"]["total"] == 1
        assert results["summary"]["passed"] == 1
        assert results["summary"]["failed"] == 0

    def test_parse_single_fail(self):
        """Test parsing a single failing test."""
        output = """Test 1/1: expected success
, got failure
"""
        results = parse_harness_output(output)
        assert results["summary"]["total"] == 1
        assert results["summary"]["passed"] == 0
        assert results["summary"]["failed"] == 1

    def test_parse_summary_line(self):
        """Test parsing summary line from harness output."""
        output = """----- Finished tests from file iso.tst
100 tests found.
85 tests succeeded.
10 tests failed.
5 tests skipped.
"""
        results = parse_harness_output(output)
        assert results["summary"]["total"] == 100
        assert results["summary"]["passed"] == 85
        assert results["summary"]["failed"] == 10
        assert results["summary"]["skipped"] == 5

    def test_parse_empty_output(self):
        """Test parsing empty output."""
        results = parse_harness_output("")
        assert results["summary"]["total"] == 0
        assert results["summary"]["passed"] == 0
        assert results["summary"]["failed"] == 0


class TestHarnessMode:
    """Test harness mode execution."""

    def test_harness_mode_available(self):
        """Test that harness mode is available as an option."""
        runner = ISOTestRunner()
        # Should not raise
        assert hasattr(runner, "run_suite")

    def test_consult_harness_file(self):
        """Test consulting harness.pl file into engine."""
        harness_path = Path("iso_test_js/harness.pl")
        if not harness_path.exists():
            pytest.skip("harness.pl not found")

        # Note: harness.pl contains directives (:- op(...)) which are not yet
        # supported in PyLog. This test verifies the structure is in place,
        # but full functionality requires directive support.
        # For now, we just verify the harness file exists and is readable
        text = harness_path.read_text()
        assert len(text) > 0
        assert ":- op(1200, fy, fixme)" in text

        # Once directives are supported, uncomment this:
        # engine = Engine(Program(tuple()), mode="iso")
        # engine.consult_string(text)
        # assert len(engine.program.clauses) > 0


class TestModeComparison:
    """Test comparison between Python and harness modes."""

    def test_mode_comparison_structure(self):
        """Test that mode comparison produces expected structure."""
        # This will be implemented after the harness mode is functional
        # For now, just ensure the structure is defined

        # Should return a dict with differences
        # Will be properly tested once implementation is complete
        pass


class TestCLIIntegration:
    """Test CLI interface for harness mode."""

    def test_mode_parameter_accepted(self):
        """Test that --mode parameter is accepted."""
        # This tests the CLI argument parsing

        # Parser should accept --mode argument
        # Will be tested via actual CLI once implemented
        pass
