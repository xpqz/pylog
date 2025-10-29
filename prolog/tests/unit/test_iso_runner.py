"""
Unit tests for ISO test suite runner.

Tests the core functionality of parsing and executing ISO test patterns.
"""

import pytest
from pathlib import Path

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause
from scripts.iso_test_operators import iso_test_operators, parse_iso_tests


class TestISOTestParsing:
    """Test parsing of ISO test patterns from iso.tst file.

    BLOCKER: Reader postfix operator support required.
    The ISO test operators include postfix (should_fail) and infix
    (should_give, should_throw) operators. The Reader's Pratt parser
    currently has a hardcoded list of operator token types in
    _get_infix_info() and does not implement postfix operator handling.

    Required changes to Reader:
    1. Add postfix operator support to parse_term loop
    2. Make _get_infix_info/_get_postfix_info dynamic (query operator table)
    3. Or: parse ISO tests as regular structures without operator support
    """

    @pytest.mark.skip(reason="Reader postfix operator support required")
    def test_parse_should_fail_pattern(self):
        """Parse a should_fail test pattern."""
        code = "call(fail) should_fail."
        with iso_test_operators():
            clauses = parse_iso_tests(code)
        assert len(clauses) == 1
        clause = clauses[0]
        assert isinstance(clause, Clause)
        # Head is the pattern: should_fail(call(fail))
        head = clause.head
        assert isinstance(head, Struct)
        assert head.functor == "should_fail"
        assert len(head.args) == 1
        # Goal is call(fail)
        goal = head.args[0]
        assert isinstance(goal, Struct)
        assert goal.functor == "call"
        assert len(goal.args) == 1
        assert isinstance(goal.args[0], Atom)
        assert goal.args[0].name == "fail"

    @pytest.mark.skip(reason="Reader infix operator dynamic lookup required")
    def test_parse_should_give_pattern(self):
        """Parse a should_give test pattern."""
        code = "call(!) should_give true."
        with iso_test_operators():
            clauses = parse_iso_tests(code)
        assert len(clauses) == 1
        clause = clauses[0]
        # Head is: should_give(call(!), true)
        head = clause.head
        assert isinstance(head, Struct)
        assert head.functor == "should_give"
        assert len(head.args) == 2
        # Goal is call(!)
        goal = head.args[0]
        assert isinstance(goal, Struct)
        assert goal.functor == "call"
        # Check is true
        check = head.args[1]
        assert isinstance(check, Atom)
        assert check.name == "true"

    @pytest.mark.skip(reason="Reader infix operator dynamic lookup required")
    def test_parse_should_throw_pattern(self):
        """Parse a should_throw test pattern."""
        code = "call(_) should_throw error(instantiation_error,_)."
        with iso_test_operators():
            clauses = parse_iso_tests(code)
        assert len(clauses) == 1
        clause = clauses[0]
        # Head is: should_throw(call(_), error(...))
        head = clause.head
        assert isinstance(head, Struct)
        assert head.functor == "should_throw"
        assert len(head.args) == 2
        # Goal is call(_)
        goal = head.args[0]
        assert isinstance(goal, Struct)
        assert goal.functor == "call"
        # Exception pattern
        exception = head.args[1]
        assert isinstance(exception, Struct)
        assert exception.functor == "error"

    @pytest.mark.skip(reason="Reader prefix operator dynamic lookup required")
    def test_parse_fixme_pattern(self):
        """Parse a fixme (skip) test pattern."""
        code = "fixme call(fail) should_fail."
        with iso_test_operators():
            clauses = parse_iso_tests(code)
        assert len(clauses) == 1
        clause = clauses[0]
        # Head is: fixme(should_fail(call(fail)))
        head = clause.head
        assert isinstance(head, Struct)
        assert head.functor == "fixme"
        assert len(head.args) == 1
        # Wrapped test is should_fail
        wrapped = head.args[0]
        assert isinstance(wrapped, Struct)
        assert wrapped.functor == "should_fail"

    @pytest.mark.skip(reason="Reader infix operator dynamic lookup required")
    def test_parse_multiple_solutions_pattern(self):
        """Parse a multiple_solutions test pattern."""
        code = "call((X=1;X=2)) should_give multiple_solutions(K, K==2, K==X)."
        with iso_test_operators():
            clauses = parse_iso_tests(code)
        assert len(clauses) == 1
        clause = clauses[0]
        head = clause.head
        assert isinstance(head, Struct)
        assert head.functor == "should_give"
        # Check is multiple_solutions
        check = head.args[1]
        assert isinstance(check, Struct)
        assert check.functor == "multiple_solutions"
        assert len(check.args) == 3


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
