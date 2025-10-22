"""Tests for WAM disjunction (;/2) compilation.

Tests choicepoint instruction generation for disjunction compilation,
including try_me_else/retry_me_else/trust_me patterns, label generation,
branch jumps, variable tracking, and structured argument compilation.
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, List, Struct, Var
from prolog.wam.codegen import compile_clause, compile_disjunction, _flatten_disjunction
from prolog.wam.instructions import (
    OP_CALL,
    OP_EXECUTE,
    OP_JUMP,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_PUT_VALUE,
    OP_PUT_VARIABLE,
    OP_RETRY_ME_ELSE,
    OP_TRUST_ME,
    OP_TRY_ME_ELSE,
)


class TestSimpleDisjunction:
    """Test basic disjunction compilation patterns."""

    def test_two_branch_disjunction(self):
        """(a ; b) compiles with try_me_else, jump, trust_me pattern."""
        branches = [Atom("a"), Atom("b")]
        register_map = {}
        is_last_goal = True
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Expected pattern:
        # try_me_else L1
        # execute user:a/0
        # jump Lend
        # L1:
        # trust_me
        # execute user:b/0
        # Lend:

        assert instructions[0][0] == OP_TRY_ME_ELSE
        label1 = instructions[0][1]
        assert instructions[1] == (OP_EXECUTE, "user:a/0")
        # CRITICAL: Must jump past second branch after first succeeds
        assert instructions[2][0] == OP_JUMP
        label_end = instructions[2][1]
        assert instructions[3] == ("LABEL", label1)
        assert instructions[4] == (OP_TRUST_ME,)
        assert instructions[5] == (OP_EXECUTE, "user:b/0")
        assert instructions[6] == ("LABEL", label_end)

        # Labels must be distinct
        assert label1 != label_end

    def test_three_branch_disjunction(self):
        """(a ; b ; c) compiles with try/retry/trust pattern."""
        branches = [Atom("a"), Atom("b"), Atom("c")]
        register_map = {}
        is_last_goal = True
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Expected pattern:
        # try_me_else L1
        # execute user:a/0
        # jump Lend
        # L1:
        # retry_me_else L2
        # execute user:b/0
        # jump Lend
        # L2:
        # trust_me
        # execute user:c/0
        # Lend:

        assert instructions[0][0] == OP_TRY_ME_ELSE
        label1 = instructions[0][1]
        assert instructions[1] == (OP_EXECUTE, "user:a/0")
        assert instructions[2][0] == OP_JUMP
        label_end = instructions[2][1]

        assert instructions[3] == ("LABEL", label1)
        assert instructions[4][0] == OP_RETRY_ME_ELSE
        label2 = instructions[4][1]
        assert instructions[5] == (OP_EXECUTE, "user:b/0")
        assert instructions[6][0] == OP_JUMP
        assert instructions[6][1] == label_end  # Same end label

        assert instructions[7] == ("LABEL", label2)
        assert instructions[8] == (OP_TRUST_ME,)
        assert instructions[9] == (OP_EXECUTE, "user:c/0")
        assert instructions[10] == ("LABEL", label_end)

        # All labels must be distinct
        assert len({label1, label2, label_end}) == 3

    def test_disjunction_not_last_goal(self):
        """Disjunction not in tail position uses call instead of execute."""
        branches = [Atom("a"), Atom("b")]
        register_map = {}
        is_last_goal = False
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # First branch should use call, not execute
        assert instructions[1] == (OP_CALL, "user:a/0")
        # Second branch should also use call
        assert instructions[5] == (OP_CALL, "user:b/0")


class TestDisjunctionWithStructureGoals:
    """Test disjunction with structured goals."""

    def test_disjunction_with_args(self):
        """(p(a) ; q(b)) compiles arguments correctly."""
        branches = [
            Struct("p", (Atom("a"),)),
            Struct("q", (Atom("b"),)),
        ]
        register_map = {}
        is_last_goal = True
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Should have put_constant for arguments before calls
        # Check first branch has put_constant
        found_put_a = any(
            instr[0] == OP_PUT_CONSTANT and instr[1] == "a" for instr in instructions
        )
        found_put_b = any(
            instr[0] == OP_PUT_CONSTANT and instr[1] == "b" for instr in instructions
        )

        assert found_put_a, "First branch should emit put_constant a"
        assert found_put_b, "Second branch should emit put_constant b"

    def test_disjunction_with_structured_args(self):
        """(p(f(X)) ; q(g(Y))) compiles nested structures correctly.

        CRITICAL: This tests the fix for Bug #3 - structured arguments
        must be compiled, not just variables and constants.
        """
        x_var = Var(0, "X")
        y_var = Var(1, "Y")
        branches = [
            Struct("p", (Struct("f", (x_var,)),)),
            Struct("q", (Struct("g", (y_var,)),)),
        ]
        register_map = {0: ("X", 0), 1: ("X", 1)}
        is_last_goal = True
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Should have put_structure instructions for nested structures
        found_put_f = any(
            instr[0] == OP_PUT_STRUCTURE and instr[1] == ("f", 1)
            for instr in instructions
        )
        found_put_g = any(
            instr[0] == OP_PUT_STRUCTURE and instr[1] == ("g", 1)
            for instr in instructions
        )

        assert found_put_f, "First branch should emit put_structure f/1"
        assert found_put_g, "Second branch should emit put_structure g/1"

    def test_disjunction_with_list_args(self):
        """(p([X|Xs]) ; q([])) compiles list arguments correctly.

        CRITICAL: Tests Bug #3 fix - List arguments must be compiled.
        """
        x_var = Var(0, "X")
        xs_var = Var(1, "Xs")
        branches = [
            Struct("p", (List((x_var,), xs_var),)),
            Struct("q", (Atom("[]"),)),
        ]
        register_map = {0: ("X", 0), 1: ("X", 1)}
        is_last_goal = True
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Should have list construction for first branch
        # Check for put_constant [] in second branch
        found_empty_list = any(
            instr[0] == OP_PUT_CONSTANT and instr[1] == "[]" for instr in instructions
        )

        assert found_empty_list, "Second branch should emit put_constant []"


class TestNestedDisjunction:
    """Test nested disjunction flattening."""

    def test_nested_disjunction_flattening(self):
        """((a ; b) ; c) flattens to [a, b, c]."""
        # This would create nested choicepoints if not flattened
        inner_disj = Struct(";", (Atom("a"), Atom("b")))
        outer_disj = Struct(";", (inner_disj, Atom("c")))

        branches = _flatten_disjunction(outer_disj)

        assert len(branches) == 3
        assert branches[0] == Atom("a")
        assert branches[1] == Atom("b")
        assert branches[2] == Atom("c")


class TestDisjunctionWithVariables:
    """Test variable tracking across disjunction branches."""

    def test_disjunction_with_shared_variables(self):
        """(p(X) ; q(X)) preserves variable bindings across branches.

        CRITICAL: This tests the fix for Bug #2 - seen_vars must be passed
        through to prevent clobbering bindings.
        """
        x_var = Var(0, "X")
        branches = [
            Struct("p", (x_var,)),
            Struct("q", (x_var,)),
        ]
        register_map = {0: ("X", 0)}
        is_last_goal = True
        # CRITICAL: X already seen before disjunction
        seen_vars = {0}

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Should use put_value for X in both branches (not put_variable)
        # Count put_value vs put_variable for X
        put_values = [
            instr
            for instr in instructions
            if instr[0] == OP_PUT_VALUE and instr[1] == ("X", 0)
        ]
        put_variables = [
            instr
            for instr in instructions
            if instr[0] == OP_PUT_VARIABLE and instr[1] == ("X", 0)
        ]

        # Should have two put_value (one per branch), no put_variable
        assert (
            len(put_values) == 2
        ), "X should use put_value in both branches (already seen)"
        assert (
            len(put_variables) == 0
        ), "X should not use put_variable (would clobber binding)"

    def test_disjunction_introduces_variable(self):
        """(p(X) ; q(Y)) with fresh variables uses put_variable."""
        x_var = Var(0, "X")
        y_var = Var(1, "Y")
        branches = [
            Struct("p", (x_var,)),
            Struct("q", (y_var,)),
        ]
        register_map = {0: ("X", 0), 1: ("X", 1)}
        is_last_goal = True
        seen_vars = set()  # Neither variable seen yet

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # First occurrence of each variable should use put_variable
        found_put_var_x = any(
            instr[0] == OP_PUT_VARIABLE and instr[1] == ("X", 0)
            for instr in instructions
        )
        found_put_var_y = any(
            instr[0] == OP_PUT_VARIABLE and instr[1] == ("X", 1)
            for instr in instructions
        )

        assert found_put_var_x, "X first occurrence should use put_variable"
        assert found_put_var_y, "Y first occurrence should use put_variable"


class TestDisjunctionLabelGeneration:
    """Test unique label generation for disjunction branches."""

    def test_unique_labels_per_disjunction(self):
        """Multiple disjunctions generate unique labels."""
        branches1 = [Atom("a"), Atom("b")]
        branches2 = [Atom("c"), Atom("d")]
        register_map = {}
        is_last_goal = True
        seen_vars = set()

        instructions1 = compile_disjunction(
            branches1, register_map, is_last_goal, "user", seen_vars
        )
        instructions2 = compile_disjunction(
            branches2, register_map, is_last_goal, "user", seen_vars
        )

        # Extract labels from instructions1
        labels1 = set()
        for instr in instructions1:
            if instr[0] == "LABEL":
                labels1.add(instr[1])
            elif instr[0] in (OP_TRY_ME_ELSE, OP_RETRY_ME_ELSE, OP_JUMP):
                labels1.add(instr[1])

        # Extract labels from instructions2
        labels2 = set()
        for instr in instructions2:
            if instr[0] == "LABEL":
                labels2.add(instr[1])
            elif instr[0] in (OP_TRY_ME_ELSE, OP_RETRY_ME_ELSE, OP_JUMP):
                labels2.add(instr[1])

        # Labels should be globally unique
        assert labels1.isdisjoint(labels2), "Labels across disjunctions must be unique"


class TestEmptyBranches:
    """Test edge cases with single branches."""

    def test_single_branch_no_choicepoint(self):
        """Single branch optimizes to no choicepoint instructions."""
        branches = [Atom("a")]
        register_map = {}
        is_last_goal = True
        seen_vars = set()

        instructions = compile_disjunction(
            branches, register_map, is_last_goal, "user", seen_vars
        )

        # Should just be the goal, no try/trust/jump
        assert not any(instr[0] == OP_TRY_ME_ELSE for instr in instructions)
        assert not any(instr[0] == OP_TRUST_ME for instr in instructions)
        assert not any(instr[0] == OP_JUMP for instr in instructions)
        assert instructions == [(OP_EXECUTE, "user:a/0")]


class TestFlattenDisjunction:
    """Test _flatten_disjunction helper function."""

    def test_flatten_non_disjunction(self):
        """Non-disjunction goal returns single-element list."""
        goal = Atom("a")
        branches = _flatten_disjunction(goal)
        assert branches == [Atom("a")]

    def test_flatten_simple_disjunction(self):
        """(a ; b) flattens to [a, b]."""
        goal = Struct(";", (Atom("a"), Atom("b")))
        branches = _flatten_disjunction(goal)
        assert branches == [Atom("a"), Atom("b")]

    def test_flatten_nested_disjunction(self):
        """((a ; b) ; c) flattens to [a, b, c]."""
        inner = Struct(";", (Atom("a"), Atom("b")))
        goal = Struct(";", (inner, Atom("c")))
        branches = _flatten_disjunction(goal)
        assert branches == [Atom("a"), Atom("b"), Atom("c")]

    def test_flatten_deeply_nested(self):
        """(((a ; b) ; c) ; d) flattens to [a, b, c, d]."""
        d1 = Struct(";", (Atom("a"), Atom("b")))
        d2 = Struct(";", (d1, Atom("c")))
        d3 = Struct(";", (d2, Atom("d")))
        branches = _flatten_disjunction(d3)
        assert branches == [Atom("a"), Atom("b"), Atom("c"), Atom("d")]


class TestClauseLevelIntegration:
    """Test disjunction integrated into clause compilation."""

    def test_disjunction_as_only_goal(self):
        """p :- (a ; b). compiles correctly."""
        clause = Clause(head=Atom("p"), body=[Struct(";", (Atom("a"), Atom("b")))])

        instructions = compile_clause(clause, module="user")

        # Should contain choicepoint instructions
        found_try = any(instr[0] == OP_TRY_ME_ELSE for instr in instructions)
        found_trust = any(instr[0] == OP_TRUST_ME for instr in instructions)
        found_jump = any(instr[0] == OP_JUMP for instr in instructions)

        assert found_try, "Should have try_me_else"
        assert found_trust, "Should have trust_me"
        assert found_jump, "Should have jump to end"

    def test_disjunction_in_middle_of_body(self):
        """p :- q, (a ; b), r. compiles with goals before and after.

        CRITICAL: This tests Bug #1 fix - disjunction must jump to
        continuation (r), not fall through to next branch.
        """
        clause = Clause(
            head=Atom("p"),
            body=[Atom("q"), Struct(";", (Atom("a"), Atom("b"))), Atom("r")],
        )

        instructions = compile_clause(clause, module="user")

        # Should have:
        # - call q
        # - try_me_else
        # - call/execute a
        # - jump to r
        # - trust_me
        # - call/execute b
        # - call/execute r (last goal, so execute)

        found_call_q = any(
            instr[0] == OP_CALL and instr[1] == "user:q/0" for instr in instructions
        )
        found_call_r = any(
            instr[0] in (OP_CALL, OP_EXECUTE) and instr[1] == "user:r/0"
            for instr in instructions
        )
        found_jump = any(instr[0] == OP_JUMP for instr in instructions)

        assert found_call_q, "Should call q before disjunction"
        assert found_call_r, "Should call/execute r after disjunction"
        assert found_jump, "Should jump past second branch to continuation"

    def test_nested_disjunction_in_clause(self):
        """p :- ((a ; b) ; c). flattens and compiles correctly."""
        inner = Struct(";", (Atom("a"), Atom("b")))
        outer = Struct(";", (inner, Atom("c")))
        clause = Clause(head=Atom("p"), body=[outer])

        instructions = compile_clause(clause, module="user")

        # Should have 3 branches (flattened)
        # try_me_else, retry_me_else, trust_me
        found_try = any(instr[0] == OP_TRY_ME_ELSE for instr in instructions)
        found_retry = any(instr[0] == OP_RETRY_ME_ELSE for instr in instructions)
        found_trust = any(instr[0] == OP_TRUST_ME for instr in instructions)

        assert found_try, "Should have try_me_else for 3-branch disjunction"
        assert found_retry, "Should have retry_me_else for middle branch"
        assert found_trust, "Should have trust_me for last branch"

    def test_disjunction_at_start_of_body(self):
        """p :- (a ; b), r. compiles with continuation after disjunction."""
        clause = Clause(
            head=Atom("p"), body=[Struct(";", (Atom("a"), Atom("b"))), Atom("r")]
        )

        instructions = compile_clause(clause, module="user")

        # Both branches should jump to r, not fall through
        found_jump = any(instr[0] == OP_JUMP for instr in instructions)
        found_call_r = any(
            instr[0] in (OP_CALL, OP_EXECUTE) and instr[1] == "user:r/0"
            for instr in instructions
        )

        assert found_jump, "Branches should jump to continuation"
        assert found_call_r, "Should have r as continuation"


class TestDisjunctionFailurePath:
    """Test disjunction failure leads to clause failure."""

    def test_all_branches_fail_leads_to_clause_failure(self):
        """p :- (fail ; fail). compiles correctly.

        All branches should fail naturally, no stray choicepoint.
        """
        clause = Clause(
            head=Atom("p"), body=[Struct(";", (Atom("fail"), Atom("fail")))]
        )

        instructions = compile_clause(clause, module="user")

        # Should have try/trust pattern even though both fail
        found_try = any(instr[0] == OP_TRY_ME_ELSE for instr in instructions)
        found_trust = any(instr[0] == OP_TRUST_ME for instr in instructions)

        assert found_try, "Should create choicepoint"
        assert found_trust, "Should have final branch"
