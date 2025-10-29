"""
ISO test pattern parser for Phase A.

Pragmatic text-based scanner that:
- Tracks parentheses/brackets/quotes depth
- Skips comments
- Finds top-level clause terminators
- Detects ISO test operators at top level
- Parses goal and check terms using Reader

This avoids modifying Reader for postfix operators (tracked in #424).
"""

from dataclasses import dataclass
from typing import Optional, List
from enum import Enum

from prolog.ast.terms import Term
from prolog.parser.reader import Reader, ReaderError


class ISOTestKind(Enum):
    """ISO test pattern types."""

    SHOULD_FAIL = "should_fail"
    SHOULD_GIVE = "should_give"
    SHOULD_THROW = "should_throw"


@dataclass
class ISOTestCase:
    """Parsed ISO test case."""

    kind: ISOTestKind
    goal_term: Term
    check_term: Optional[Term] = None  # For should_give
    exception_term: Optional[Term] = None  # For should_throw
    skipped: bool = False  # For fixme prefix
    clause_index: int = 0
    line_number: Optional[int] = None
    source_text: str = ""


class ClauseScanner:
    """
    Scanner for ISO test clauses.

    Tracks depth and finds clause terminators while respecting:
    - Parentheses and brackets
    - Quoted atoms with escapes
    - Comments
    """

    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.line = 1

    def scan_clauses(self) -> List[tuple[str, int]]:
        """
        Scan text and return list of (clause_text, line_number) tuples.

        Returns:
            List of (clause_text, line_number) for each clause found
        """
        clauses = []
        current_clause = []
        clause_start_line = 1
        paren_depth = 0
        bracket_depth = 0
        in_quoted_atom = False
        escape_next = False

        i = 0
        line = 1

        while i < len(self.text):
            char = self.text[i]

            # Track line numbers
            if char == "\n":
                line += 1

            # Handle escape sequences in quoted atoms
            if escape_next:
                current_clause.append(char)
                escape_next = False
                i += 1
                continue

            # Track quoted atoms
            if char == "'" and not escape_next:
                in_quoted_atom = not in_quoted_atom
                current_clause.append(char)
                i += 1
                continue

            # Handle escapes in quoted atoms
            if in_quoted_atom and char == "\\":
                escape_next = True
                current_clause.append(char)
                i += 1
                continue

            # Skip tracking inside quoted atoms
            if in_quoted_atom:
                current_clause.append(char)
                i += 1
                continue

            # Handle line comments (% starts line comment when not in quoted atom)
            if char == "%":
                # Skip to end of line
                while i < len(self.text) and self.text[i] != "\n":
                    i += 1
                # Don't skip the newline itself, let normal processing handle it
                continue

            # Track parentheses and brackets
            if char == "(":
                paren_depth += 1
            elif char == ")":
                paren_depth -= 1
            elif char == "[":
                bracket_depth += 1
            elif char == "]":
                bracket_depth -= 1
            elif char == "." and paren_depth == 0 and bracket_depth == 0:
                # Check if this is part of .. operator
                if i + 1 < len(self.text) and self.text[i + 1] == ".":
                    # It's a .. operator, not clause terminator
                    current_clause.append(char)  # Add first .
                    current_clause.append(self.text[i + 1])  # Add second .
                    i += 2  # Skip both dots
                    continue

                # Found clause terminator at top level
                current_clause.append(char)
                clause_text = "".join(current_clause).strip()
                if clause_text and clause_text != ".":
                    clauses.append((clause_text, clause_start_line))
                current_clause = []
                clause_start_line = line + 1
                i += 1
                continue

            current_clause.append(char)
            i += 1

        # Handle any remaining text
        if current_clause:
            clause_text = "".join(current_clause).strip()
            if clause_text:
                clauses.append((clause_text, clause_start_line))

        return clauses


class PatternDetector:
    """
    Detects ISO test patterns in clause text.

    Finds top-level operators (should_fail, should_give, should_throw)
    and fixme prefix while respecting nesting.
    """

    @staticmethod
    def find_operator_position(text: str, operator: str) -> Optional[tuple[int, int]]:
        """
        Find top-level operator position in text.

        Args:
            text: Clause text to search
            operator: Operator to find (e.g., "should_fail")

        Returns:
            (start, end) position tuple if found at top level, None otherwise
        """
        paren_depth = 0
        bracket_depth = 0
        in_quoted_atom = False
        escape_next = False

        i = 0
        while i < len(text):
            char = text[i]

            # Handle escapes
            if escape_next:
                escape_next = False
                i += 1
                continue

            # Track quoted atoms
            if char == "'" and not escape_next:
                in_quoted_atom = not in_quoted_atom
                i += 1
                continue

            if in_quoted_atom:
                if char == "\\":
                    escape_next = True
                i += 1
                continue

            # Skip comments
            if char == "%":
                while i < len(text) and text[i] != "\n":
                    i += 1
                continue

            # Track depth
            if char == "(":
                paren_depth += 1
            elif char == ")":
                paren_depth -= 1
            elif char == "[":
                bracket_depth += 1
            elif char == "]":
                bracket_depth -= 1

            # Check for operator at top level
            if paren_depth == 0 and bracket_depth == 0:
                # Check if we're at the start of the operator
                if text[i : i + len(operator)] == operator:
                    # Verify word boundaries (for word operators like "should_fail")
                    before_ok = i == 0 or not text[i - 1].isalnum()
                    after_ok = (
                        i + len(operator) >= len(text)
                        or not text[i + len(operator)].isalnum()
                    )
                    if before_ok and after_ok:
                        return (i, i + len(operator))

            i += 1

        return None

    @staticmethod
    def split_at_operator(text: str, operator: str) -> Optional[tuple[str, str]]:
        """
        Split text at top-level operator.

        Args:
            text: Clause text
            operator: Operator to split on

        Returns:
            (left, right) tuple if operator found, None otherwise
        """
        pos = PatternDetector.find_operator_position(text, operator)
        if pos is None:
            return None

        start, end = pos
        left = text[:start].strip()
        right = text[end:].strip()
        return (left, right)

    @staticmethod
    def detect_pattern(
        clause_text: str, line_number: int, clause_index: int
    ) -> Optional[ISOTestCase]:
        """
        Detect and parse ISO test pattern from clause text.

        Args:
            clause_text: Complete clause text (without trailing dot)
            line_number: Source line number
            clause_index: Clause index in file

        Returns:
            ISOTestCase if valid pattern detected, None otherwise
        """
        # Remove trailing dot if present
        if clause_text.endswith("."):
            clause_text = clause_text[:-1].strip()

        # Check for fixme prefix
        skipped = False
        if clause_text.startswith("fixme "):
            skipped = True
            clause_text = clause_text[6:].strip()  # Remove "fixme "

        # Try each operator in precedence order
        # should_fail (postfix)
        split = PatternDetector.split_at_operator(clause_text, "should_fail")
        if split:
            goal_text, check_text = split
            if check_text.strip():  # should_fail is postfix, no right side
                return None
            return PatternDetector._parse_should_fail(
                goal_text, skipped, line_number, clause_index, clause_text
            )

        # should_give (infix)
        split = PatternDetector.split_at_operator(clause_text, "should_give")
        if split:
            goal_text, check_text = split
            return PatternDetector._parse_should_give(
                goal_text, check_text, skipped, line_number, clause_index, clause_text
            )

        # should_throw (infix)
        split = PatternDetector.split_at_operator(clause_text, "should_throw")
        if split:
            goal_text, check_text = split
            return PatternDetector._parse_should_throw(
                goal_text, check_text, skipped, line_number, clause_index, clause_text
            )

        return None

    @staticmethod
    def _parse_should_fail(
        goal_text: str,
        skipped: bool,
        line_number: int,
        clause_index: int,
        source_text: str,
    ) -> Optional[ISOTestCase]:
        """Parse should_fail pattern."""
        try:
            reader = Reader()
            goal_term = reader.read_term(goal_text)
            return ISOTestCase(
                kind=ISOTestKind.SHOULD_FAIL,
                goal_term=goal_term,
                skipped=skipped,
                clause_index=clause_index,
                line_number=line_number,
                source_text=source_text,
            )
        except ReaderError:
            return None

    @staticmethod
    def _parse_should_give(
        goal_text: str,
        check_text: str,
        skipped: bool,
        line_number: int,
        clause_index: int,
        source_text: str,
    ) -> Optional[ISOTestCase]:
        """Parse should_give pattern."""
        try:
            reader = Reader()
            goal_term = reader.read_term(goal_text)
            check_term = reader.read_term(check_text)
            return ISOTestCase(
                kind=ISOTestKind.SHOULD_GIVE,
                goal_term=goal_term,
                check_term=check_term,
                skipped=skipped,
                clause_index=clause_index,
                line_number=line_number,
                source_text=source_text,
            )
        except ReaderError:
            return None

    @staticmethod
    def _parse_should_throw(
        goal_text: str,
        exception_text: str,
        skipped: bool,
        line_number: int,
        clause_index: int,
        source_text: str,
    ) -> Optional[ISOTestCase]:
        """Parse should_throw pattern."""
        try:
            reader = Reader()
            goal_term = reader.read_term(goal_text)
            exception_term = reader.read_term(exception_text)
            return ISOTestCase(
                kind=ISOTestKind.SHOULD_THROW,
                goal_term=goal_term,
                exception_term=exception_term,
                skipped=skipped,
                clause_index=clause_index,
                line_number=line_number,
                source_text=source_text,
            )
        except ReaderError:
            return None


def parse_iso_test_file(file_path: str) -> List[ISOTestCase]:
    """
    Parse ISO test file and return list of test cases.

    Args:
        file_path: Path to iso.tst file

    Returns:
        List of ISOTestCase objects
    """
    with open(file_path, "r", encoding="utf-8") as f:
        content = f.read()

    scanner = ClauseScanner(content)
    clauses = scanner.scan_clauses()

    test_cases = []
    for clause_index, (clause_text, line_number) in enumerate(clauses):
        test_case = PatternDetector.detect_pattern(
            clause_text, line_number, clause_index
        )
        if test_case:
            test_cases.append(test_case)

    return test_cases
