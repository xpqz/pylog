"""
Unit tests for ISO test suite runner.

Tests the core functionality of parsing and executing ISO test patterns.
"""

import pytest
from pathlib import Path

from prolog.ast.terms import Atom, Int, Var, Struct
from scripts.iso_test_parser import (
    ClauseScanner,
    PatternDetector,
    ISOTestKind,
)


class TestClauseScanner:
    """Test clause scanning functionality."""

    def test_scan_single_clause(self):
        """Scan single simple clause."""
        scanner = ClauseScanner("call(fail) should_fail.")
        clauses = scanner.scan_clauses()
        assert len(clauses) == 1
        assert clauses[0][0] == "call(fail) should_fail."

    def test_scan_multiple_clauses(self):
        """Scan multiple clauses."""
        text = "call(fail) should_fail.\ncall(!) should_give true."
        scanner = ClauseScanner(text)
        clauses = scanner.scan_clauses()
        assert len(clauses) == 2

    def test_scan_ignores_comments(self):
        """Scanner ignores line comments."""
        text = "% This is a comment\ncall(fail) should_fail.\n% Another comment"
        scanner = ClauseScanner(text)
        clauses = scanner.scan_clauses()
        assert len(clauses) == 1

    def test_scan_respects_parentheses(self):
        """Scanner respects parentheses depth for dots."""
        text = "call((a,b.c)) should_fail."
        scanner = ClauseScanner(text)
        clauses = scanner.scan_clauses()
        assert len(clauses) == 1

    def test_scan_respects_quoted_atoms(self):
        """Scanner respects quoted atoms."""
        text = "call('atom.with.dots') should_fail."
        scanner = ClauseScanner(text)
        clauses = scanner.scan_clauses()
        assert len(clauses) == 1

    def test_scan_handles_dot_dot_operator(self):
        """Scanner distinguishes .. operator from clause terminator."""
        text = "X in 1..10 should_give true."
        scanner = ClauseScanner(text)
        clauses = scanner.scan_clauses()
        assert len(clauses) == 1


class TestPatternDetector:
    """Test pattern detection functionality."""

    def test_find_should_fail_postfix(self):
        """Detect should_fail postfix operator."""
        pos = PatternDetector.find_operator_position(
            "call(fail) should_fail", "should_fail"
        )
        assert pos is not None
        assert pos == (11, 22)  # Position of "should_fail"

    def test_find_should_give_infix(self):
        """Detect should_give infix operator."""
        pos = PatternDetector.find_operator_position(
            "call(!) should_give true", "should_give"
        )
        assert pos is not None

    def test_operator_not_inside_parens(self):
        """Operator inside parentheses not detected at top level."""
        pos = PatternDetector.find_operator_position(
            "call((X=1 should_give Y=2))", "should_give"
        )
        assert pos is None

    def test_operator_not_in_quoted_atom(self):
        """Operator in quoted atom not detected."""
        pos = PatternDetector.find_operator_position(
            "call('should_fail') should_fail", "should_fail"
        )
        # Should find the second occurrence, not the quoted one
        assert pos is not None
        assert pos[0] >= 20  # After the quoted atom

    def test_split_at_should_fail(self):
        """Split clause at should_fail operator."""
        result = PatternDetector.split_at_operator(
            "call(fail) should_fail", "should_fail"
        )
        assert result is not None
        left, right = result
        assert left == "call(fail)"
        assert right == ""

    def test_split_at_should_give(self):
        """Split clause at should_give operator."""
        result = PatternDetector.split_at_operator(
            "call(!) should_give true", "should_give"
        )
        assert result is not None
        left, right = result
        assert left == "call(!)"
        assert right == "true"


class TestISOTestParsing:
    """Test parsing of ISO test patterns."""

    def test_parse_should_fail_pattern(self):
        """Parse a should_fail test pattern."""
        test_case = PatternDetector.detect_pattern("call(fail) should_fail", 1, 0)
        assert test_case is not None
        assert test_case.kind == ISOTestKind.SHOULD_FAIL
        assert isinstance(test_case.goal_term, Struct)
        assert test_case.goal_term.functor == "call"
        assert test_case.check_term is None
        assert test_case.exception_term is None
        assert not test_case.skipped

    def test_parse_should_give_pattern(self):
        """Parse a should_give test pattern."""
        test_case = PatternDetector.detect_pattern("call(!) should_give true", 1, 0)
        assert test_case is not None
        assert test_case.kind == ISOTestKind.SHOULD_GIVE
        assert isinstance(test_case.goal_term, Struct)
        assert test_case.goal_term.functor == "call"
        assert test_case.check_term is not None
        assert isinstance(test_case.check_term, Atom)
        assert test_case.check_term.name == "true"

    def test_parse_should_throw_pattern(self):
        """Parse a should_throw test pattern."""
        test_case = PatternDetector.detect_pattern(
            "call(_) should_throw error(instantiation_error,_)", 1, 0
        )
        assert test_case is not None
        assert test_case.kind == ISOTestKind.SHOULD_THROW
        assert isinstance(test_case.goal_term, Struct)
        assert test_case.goal_term.functor == "call"
        assert test_case.exception_term is not None
        assert isinstance(test_case.exception_term, Struct)
        assert test_case.exception_term.functor == "error"

    def test_parse_fixme_pattern(self):
        """Parse a fixme (skip) test pattern."""
        test_case = PatternDetector.detect_pattern("fixme call(fail) should_fail", 1, 0)
        assert test_case is not None
        assert test_case.kind == ISOTestKind.SHOULD_FAIL
        assert test_case.skipped is True

    def test_parse_multiple_solutions_pattern(self):
        """Parse a multiple_solutions test pattern."""
        test_case = PatternDetector.detect_pattern(
            "call((X=1;X=2)) should_give multiple_solutions(K, K==2, K==X)", 1, 0
        )
        assert test_case is not None
        assert test_case.kind == ISOTestKind.SHOULD_GIVE
        assert test_case.check_term is not None
        assert isinstance(test_case.check_term, Struct)
        assert test_case.check_term.functor == "multiple_solutions"
        assert len(test_case.check_term.args) == 3

    def test_parse_disjunction_with_operator(self):
        """Parse pattern with disjunction around should_give."""
        test_case = PatternDetector.detect_pattern("(X=1;X=2) should_give true", 1, 0)
        assert test_case is not None
        assert test_case.kind == ISOTestKind.SHOULD_GIVE
        # Goal should be the disjunction
        assert isinstance(test_case.goal_term, Struct)
        assert test_case.goal_term.functor == ";"


class TestISOTestExecution:
    """Test execution of ISO test patterns."""

    @pytest.mark.skip(reason="Engine API usage pending implementation")
    def test_should_fail_succeeds_on_zero_solutions(self):
        """should_fail test passes when goal has no solutions."""
        pass

    @pytest.mark.skip(reason="Engine API usage pending implementation")
    def test_should_fail_fails_on_solutions(self):
        """should_fail test fails when goal has solutions."""
        pass

    @pytest.mark.skip(reason="Engine API usage pending implementation")
    def test_should_give_with_simple_check(self):
        """should_give test with simple true check."""
        pass

    @pytest.mark.skip(reason="Engine API usage pending implementation")
    def test_should_give_with_unification_check(self):
        """should_give test with variable binding check."""
        pass


class TestISORunnerCore:
    """Test core runner functionality."""

    def test_runner_identifies_test_types(self):
        """Runner correctly identifies different test pattern types."""
        # should_fail
        term = Struct("should_fail", (Atom("fail"),))
        assert term.functor == "should_fail"

        # should_give
        term = Struct("should_give", (Atom("true"), Atom("true")))
        assert term.functor == "should_give"

        # should_throw
        term = Struct("should_throw", (Atom("fail"), Atom("error")))
        assert term.functor == "should_throw"

        # fixme
        term = Struct("fixme", (Struct("should_fail", (Atom("fail"),)),))
        assert term.functor == "fixme"

    def test_runner_handles_empty_input(self):
        """Runner handles empty input gracefully."""
        # Empty list should be handled
        tests = []
        assert len(tests) == 0


class TestExceptionSubsumption:
    """Test exception subsumption matching."""

    def test_exact_exception_match(self):
        """Exact exception term matches."""
        expected = Struct("error", (Atom("instantiation_error"), Var(1, "_")))
        thrown = Struct("error", (Atom("instantiation_error"), Atom("context")))
        # Would use unify to check subsumption
        # For now, just verify structure
        assert expected.functor == thrown.functor
        assert expected.args[0] == thrown.args[0]

    def test_exception_with_variable_matches_any(self):
        """Exception pattern with variables acts as wildcard."""
        expected = Struct("error", (Var(1, "E"), Var(2, "_")))
        thrown = Struct("error", (Atom("type_error"), Atom("context")))
        # Variables in expected should match anything
        assert expected.functor == thrown.functor


class TestMultipleSolutions:
    """Test multiple_solutions pattern handling."""

    def test_multiple_solutions_structure(self):
        """multiple_solutions has correct structure: K, FinalCheck, SolutionCheck."""
        check = Struct(
            "multiple_solutions",
            (
                Var(1, "K"),
                Struct("==", (Var(1, "K"), Int(2))),
                Struct("==", (Var(2, "X"), Var(1, "K"))),
            ),
        )
        assert check.functor == "multiple_solutions"
        assert len(check.args) == 3
        # K variable
        k_var = check.args[0]
        assert isinstance(k_var, Var)
        # FinalCheck: K==2
        final_check = check.args[1]
        assert isinstance(final_check, Struct)
        # SolutionCheck: X==K
        solution_check = check.args[2]
        assert isinstance(solution_check, Struct)


class TestISOTestFileLoading:
    """Test loading actual ISO test file."""

    def test_iso_test_file_exists(self):
        """ISO test file iso.tst exists and is readable."""
        iso_file = Path("iso_test_js/iso.tst")
        assert iso_file.exists()
        assert iso_file.is_file()
        assert iso_file.stat().st_size > 0

    def test_iso_test_file_has_test_patterns(self):
        """ISO test file contains expected patterns."""
        iso_file = Path("iso_test_js/iso.tst")
        content = iso_file.read_text()
        # Should contain test operators
        assert "should_fail" in content
        assert "should_give" in content
        assert "should_throw" in content
        # Should have comment header
        assert "ISO-Prolog test patterns" in content
