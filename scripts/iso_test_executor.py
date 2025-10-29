"""
ISO test execution semantics for Phase A.

Implements execution of parsed ISO test patterns:
- should_fail: expect zero solutions
- should_give: run combined Goal, !, Check
- should_throw: verify exception subsumption
- multiple_solutions: enumerate and verify per-solution checks
"""

from dataclasses import dataclass
from enum import Enum
from typing import Optional, List
import time

from prolog.ast.terms import Term, Var, Struct, Int
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine, PrologThrow
from prolog.unify.store import Store
from prolog.unify.unify import unify
from prolog.parser.reader import Reader, ReaderError


class ExecutionStatus(Enum):
    """Test execution result status."""

    PASS = "pass"
    FAIL = "fail"
    SKIP = "skip"
    XFAIL = "xfail"
    ERROR = "error"


@dataclass
class ExecutionResult:
    """Result of executing a single test."""

    status: ExecutionStatus
    duration_ms: float
    expected: Optional[str] = None
    actual: Optional[str] = None
    error_message: Optional[str] = None


class SubsumptionChecker:
    """
    Check if one term subsumes another.

    Subsumption: expected term unifies with thrown term,
    treating variables in expected as wildcards.
    """

    @staticmethod
    def subsumes(expected: Term, thrown: Term) -> bool:
        """
        Check if expected subsumes thrown.

        Uses fresh Store to avoid mutating engine state.
        Variables in expected act as wildcards.

        Args:
            expected: Expected term pattern (may have variables)
            thrown: Actual thrown term (typically ground)

        Returns:
            True if expected subsumes thrown
        """
        # Create fresh store and trail for subsumption check
        store = Store()
        trail = []

        # Collect all variable IDs from expected term
        var_ids = set()

        def collect_vars(term):
            if isinstance(term, Var):
                var_ids.add(term.id)
            elif isinstance(term, Struct):
                for arg in term.args:
                    collect_vars(arg)

        collect_vars(expected)

        # Ensure all variables have cells in the store
        max_var_id = max(var_ids) if var_ids else -1
        for _ in range(max_var_id + 1):
            if len(store.cells) <= max_var_id:
                store.new_var()

        try:
            # Attempt unification
            success = unify(expected, thrown, store, trail, occurs_check=False)
            return success
        except Exception:
            return False


class ISOTestExecutor:
    """
    Executor for ISO test patterns.

    Provides execution semantics for each test kind with
    safety limits and proper error handling.
    """

    def __init__(
        self,
        max_solutions: int = 10000,
        max_steps: int = 1000000,
        timeout_ms: Optional[int] = None,
    ):
        """
        Initialize executor with safety limits.

        Args:
            max_solutions: Maximum solutions to enumerate
            max_steps: Maximum execution steps per query
            timeout_ms: Optional timeout in milliseconds
        """
        self.max_solutions = max_solutions
        self.max_steps = max_steps
        self.timeout_ms = timeout_ms

    @staticmethod
    def _make_program(program: Optional[List[Clause]]) -> Program:
        """Create Program object from optional clause list."""
        if program is None:
            return Program(tuple())
        return Program(tuple(program))

    def run_should_fail(
        self, goal_term: Term, program: Optional[List[Clause]] = None
    ) -> ExecutionResult:
        """
        Execute should_fail test: expect zero solutions.

        Args:
            goal_term: Goal to execute
            program: Optional program clauses

        Returns:
            ExecutionResult with pass/fail status
        """
        start = time.time()

        try:
            prog = self._make_program(program)
            engine = Engine(prog)
            solutions = []

            # Run query and get solutions
            solutions = engine.run([goal_term], max_solutions=self.max_solutions)

            duration_ms = (time.time() - start) * 1000

            if len(solutions) == 0:
                return ExecutionResult(
                    status=ExecutionStatus.PASS,
                    duration_ms=duration_ms,
                    expected="zero solutions",
                    actual="zero solutions",
                )
            else:
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected="zero solutions",
                    actual=f"{len(solutions)} solution(s)",
                )

        except Exception as e:
            duration_ms = (time.time() - start) * 1000
            return ExecutionResult(
                status=ExecutionStatus.ERROR,
                duration_ms=duration_ms,
                expected="zero solutions",
                actual="exception",
                error_message=str(e),
            )

    def run_should_give(
        self, goal_text: str, check_text: str, program: Optional[List[Clause]] = None
    ) -> ExecutionResult:
        """
        Execute should_give test: run Goal, !, Check in shared namespace.

        Uses parse-once approach to ensure variables are shared.

        Args:
            goal_text: Goal text (unparsed)
            check_text: Check text (unparsed)
            program: Optional program clauses

        Returns:
            ExecutionResult with pass/fail status
        """
        start = time.time()

        try:
            # Parse combined query to share variable namespace
            combined_text = f"{goal_text}, !, {check_text}"
            reader = Reader()
            combined_term = reader.read_term(combined_text)

            prog = self._make_program(program)
            engine = Engine(prog)
            solutions = engine.run([combined_term], max_solutions=1)

            duration_ms = (time.time() - start) * 1000

            if len(solutions) > 0:
                return ExecutionResult(
                    status=ExecutionStatus.PASS,
                    duration_ms=duration_ms,
                    expected="success with check satisfied",
                    actual="success",
                )
            else:
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected="success with check satisfied",
                    actual="failure (no solutions)",
                )

        except ReaderError as e:
            duration_ms = (time.time() - start) * 1000
            return ExecutionResult(
                status=ExecutionStatus.ERROR,
                duration_ms=duration_ms,
                expected="success with check satisfied",
                actual="parse error",
                error_message=str(e),
            )
        except Exception as e:
            duration_ms = (time.time() - start) * 1000
            return ExecutionResult(
                status=ExecutionStatus.ERROR,
                duration_ms=duration_ms,
                expected="success with check satisfied",
                actual="exception",
                error_message=str(e),
            )

    def run_should_throw(
        self,
        goal_term: Term,
        expected_exception: Term,
        program: Optional[List[Clause]] = None,
    ) -> ExecutionResult:
        """
        Execute should_throw test: verify exception subsumption.

        Args:
            goal_term: Goal that should throw
            expected_exception: Expected exception pattern
            program: Optional program clauses

        Returns:
            ExecutionResult with pass/fail status
        """
        start = time.time()

        try:
            prog = self._make_program(program)
            engine = Engine(prog)

            # Try to get first solution (should throw before yielding)
            thrown_exception = None
            try:
                solutions = engine.run([goal_term], max_solutions=1)
                # If we get solutions without throwing, that's a failure
                duration_ms = (time.time() - start) * 1000
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected=f"exception {expected_exception}",
                    actual=f"{len(solutions)} solution(s), no exception",
                )
            except PrologThrow as e:
                thrown_exception = e.ball

            duration_ms = (time.time() - start) * 1000

            if thrown_exception is None:
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected=f"exception {expected_exception}",
                    actual="no exception",
                )

            # Check subsumption
            if SubsumptionChecker.subsumes(expected_exception, thrown_exception):
                return ExecutionResult(
                    status=ExecutionStatus.PASS,
                    duration_ms=duration_ms,
                    expected=f"exception matching {expected_exception}",
                    actual=f"exception {thrown_exception}",
                )
            else:
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected=f"exception matching {expected_exception}",
                    actual=f"exception {thrown_exception} (no match)",
                )

        except Exception as e:
            duration_ms = (time.time() - start) * 1000
            return ExecutionResult(
                status=ExecutionStatus.ERROR,
                duration_ms=duration_ms,
                expected=f"exception {expected_exception}",
                actual="unexpected error",
                error_message=str(e),
            )

    def run_multiple_solutions(
        self,
        goal_term: Term,
        k_var_name: str,
        final_check_term: Term,
        solution_check_term: Term,
        program: Optional[List[Clause]] = None,
    ) -> ExecutionResult:
        """
        Execute multiple_solutions test pattern.

        Enumerates all solutions of Goal, runs SolutionCheck for each
        with K bound to solution index, then runs FinalCheck with K=N.

        Args:
            goal_term: Goal to enumerate
            k_var_name: Name of K variable (e.g., "K")
            final_check_term: Check to run after all solutions (K=N)
            solution_check_term: Check to run for each solution (K=i)
            program: Optional program clauses

        Returns:
            ExecutionResult with pass/fail status
        """
        start = time.time()

        try:
            prog = self._make_program(program)
            engine = Engine(prog)

            # Enumerate all solutions
            solutions = engine.run([goal_term], max_solutions=self.max_solutions)

            if len(solutions) == 0:
                duration_ms = (time.time() - start) * 1000
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected="at least one solution",
                    actual="zero solutions",
                )

            # Run SolutionCheck for each solution
            for i, sol in enumerate(solutions, start=1):
                # Build query: K=i, apply bindings, then SolutionCheck
                # For simplicity: just bind K and run check
                # TODO: properly apply solution bindings to check term
                k_binding = Struct("=", (Var(0, k_var_name), Int(i)))
                check_query = Struct(",", (k_binding, solution_check_term))

                engine_check = Engine(prog)
                check_solutions = engine_check.run([check_query], max_solutions=1)

                if len(check_solutions) == 0:
                    duration_ms = (time.time() - start) * 1000
                    return ExecutionResult(
                        status=ExecutionStatus.FAIL,
                        duration_ms=duration_ms,
                        expected=f"SolutionCheck success for K={i}",
                        actual=f"SolutionCheck failed for K={i}",
                    )

            # Run FinalCheck with K=N
            n = len(solutions)
            k_binding = Struct("=", (Var(0, k_var_name), Int(n)))
            final_query = Struct(",", (k_binding, final_check_term))

            engine_final = Engine(prog)
            final_solutions = engine_final.run([final_query], max_solutions=1)

            duration_ms = (time.time() - start) * 1000

            if len(final_solutions) > 0:
                return ExecutionResult(
                    status=ExecutionStatus.PASS,
                    duration_ms=duration_ms,
                    expected=f"FinalCheck success with K={n}",
                    actual="success",
                )
            else:
                return ExecutionResult(
                    status=ExecutionStatus.FAIL,
                    duration_ms=duration_ms,
                    expected=f"FinalCheck success with K={n}",
                    actual="FinalCheck failed",
                )

        except Exception as e:
            duration_ms = (time.time() - start) * 1000
            return ExecutionResult(
                status=ExecutionStatus.ERROR,
                duration_ms=duration_ms,
                expected="multiple_solutions check",
                actual="exception",
                error_message=str(e),
            )
