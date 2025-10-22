"""Tests for WAM disjunction compilation.

Tests the compilation of disjunction (;/2) into choicepoint instruction sequences.
"""

from prolog.ast.terms import Atom, Int, Struct, Var
from prolog.wam.codegen import compile_disjunction
from prolog.wam.instructions import (
    OP_TRY_ME_ELSE,
    OP_RETRY_ME_ELSE,
    OP_TRUST_ME,
    OP_PUT_CONSTANT,
    OP_CALL,
    OP_EXECUTE,
)


class TestSimpleDisjunction:
    """Test compilation of simple disjunctions."""

    def test_two_branch_disjunction(self):
        """(a ; b) compiles with try_me_else and trust_me."""
        # Disjunction: (a ; b)
        branches = [Atom("a"), Atom("b")]
        register_map = {}
        is_last_goal = True

        instructions = compile_disjunction(branches, register_map, is_last_goal)

        # Expected pattern:
        # try_me_else L1
        # execute user:a/0
        # L1:
        # trust_me
        # execute user:b/0

        # Check for try_me_else at start
        assert instructions[0][0] == OP_TRY_ME_ELSE
        label1 = instructions[0][1]

        # Check for execute a/0 (last goal optimization)
        assert instructions[1] == (OP_EXECUTE, "user:a/0")

        # Check for label
        assert instructions[2] == ("LABEL", label1)

        # Check for trust_me
        assert instructions[3] == (OP_TRUST_ME,)

        # Check for execute b/0
        assert instructions[4] == (OP_EXECUTE, "user:b/0")

    def test_three_branch_disjunction(self):
        """(a ; b ; c) compiles with try, retry, trust pattern."""
        # Disjunction: (a ; b ; c)
        branches = [Atom("a"), Atom("b"), Atom("c")]
        register_map = {}
        is_last_goal = True

        instructions = compile_disjunction(branches, register_map, is_last_goal)

        # Expected pattern:
        # try_me_else L1
        # execute user:a/0
        # L1:
        # retry_me_else L2
        # execute user:b/0
        # L2:
        # trust_me
        # execute user:c/0

        # Check for try_me_else
        assert instructions[0][0] == OP_TRY_ME_ELSE
        label1 = instructions[0][1]

        # Check for execute a/0
        assert instructions[1] == (OP_EXECUTE, "user:a/0")

        # Check for label 1
        assert instructions[2] == ("LABEL", label1)

        # Check for retry_me_else
        assert instructions[3][0] == OP_RETRY_ME_ELSE
        label2 = instructions[3][1]

        # Check for execute b/0
        assert instructions[4] == (OP_EXECUTE, "user:b/0")

        # Check for label 2
        assert instructions[5] == ("LABEL", label2)

        # Check for trust_me
        assert instructions[6] == (OP_TRUST_ME,)

        # Check for execute c/0
        assert instructions[7] == (OP_EXECUTE, "user:c/0")

    def test_disjunction_not_last_goal(self):
        """(a ; b) as non-last goal uses call instead of execute."""
        branches = [Atom("a"), Atom("b")]
        register_map = {}
        is_last_goal = False

        instructions = compile_disjunction(branches, register_map, is_last_goal)

        # Check for try_me_else
        assert instructions[0][0] == OP_TRY_ME_ELSE

        # Check for call a/0 (not execute)
        assert instructions[1] == (OP_CALL, "user:a/0")

        # Check for label
        assert instructions[2][0] == "LABEL"

        # Check for trust_me
        assert instructions[3] == (OP_TRUST_ME,)

        # Check for call b/0
        assert instructions[4] == (OP_CALL, "user:b/0")


class TestDisjunctionWithStructureGoals:
    """Test disjunction with structured goals requiring argument setup."""

    def test_disjunction_with_args(self):
        """(p(1) ; q(2)) compiles with put instructions."""
        # Disjunction: (p(1) ; q(2))
        branches = [
            Struct("p", (Int(1),)),
            Struct("q", (Int(2),)),
        ]
        register_map = {}
        is_last_goal = True

        instructions = compile_disjunction(branches, register_map, is_last_goal)

        # Should have try_me_else, put+execute for first branch,
        # label+trust_me, put+execute for second branch
        assert instructions[0][0] == OP_TRY_ME_ELSE

        # First branch: put_constant 1, X0; execute p/1
        assert instructions[1] == (OP_PUT_CONSTANT, 1, 0)
        assert instructions[2] == (OP_EXECUTE, "user:p/1")

        # Second branch label
        assert instructions[3][0] == "LABEL"

        # trust_me
        assert instructions[4] == (OP_TRUST_ME,)

        # Second branch: put_constant 2, X0; execute q/1
        assert instructions[5] == (OP_PUT_CONSTANT, 2, 0)
        assert instructions[6] == (OP_EXECUTE, "user:q/1")


class TestNestedDisjunction:
    """Test compilation of nested disjunctions."""

    def test_nested_disjunction_flattening(self):
        """((a ; b) ; c) should flatten to (a ; b ; c)."""
        # Nested disjunction: ((a ; b) ; c)
        inner_disj = Struct(";", (Atom("a"), Atom("b")))
        _outer_disj = Struct(";", (inner_disj, Atom("c")))

        # The compile function should recognize nested disjunctions
        # and flatten them or handle them appropriately
        # For now, test that it compiles without error
        # (exact behavior depends on implementation)

        # This test documents expected behavior - implementation may vary
        pass  # Placeholder for nested disjunction test


class TestDisjunctionWithVariables:
    """Test disjunction with goals containing variables."""

    def test_disjunction_with_shared_variables(self):
        """(p(X) ; q(X)) compiles with proper register allocation."""
        # Disjunction: (p(X) ; q(X))
        x_var = Var(1, "X")
        branches = [
            Struct("p", (x_var,)),
            Struct("q", (x_var,)),
        ]
        register_map = {1: ("X", 0)}  # X is in X0
        is_last_goal = True

        instructions = compile_disjunction(branches, register_map, is_last_goal)

        # Should compile successfully with put_value for X in both branches
        assert instructions[0][0] == OP_TRY_ME_ELSE
        # First branch should use X0 for X
        # Second branch should use X0 for X
        # (exact instructions depend on first/subsequent occurrence tracking)


class TestDisjunctionLabelGeneration:
    """Test that labels are unique and properly generated."""

    def test_unique_labels_per_disjunction(self):
        """Multiple disjunctions should generate unique labels."""
        branches1 = [Atom("a"), Atom("b")]
        branches2 = [Atom("c"), Atom("d")]
        register_map = {}

        instrs1 = compile_disjunction(branches1, register_map, True)
        instrs2 = compile_disjunction(branches2, register_map, True)

        # Extract labels from first disjunction
        label1_1 = instrs1[0][1]  # try_me_else label

        # Extract labels from second disjunction
        label2_1 = instrs2[0][1]  # try_me_else label

        # Labels should be different
        assert label1_1 != label2_1


class TestEmptyBranches:
    """Test edge cases with empty or single branches."""

    def test_single_branch_no_choicepoint(self):
        """Single branch disjunction should not create choicepoint."""
        # (a) is not really a disjunction, but if passed with single branch
        # should just compile the goal without choicepoint overhead
        branches = [Atom("a")]
        register_map = {}
        is_last_goal = True

        instructions = compile_disjunction(branches, register_map, is_last_goal)

        # Should not have try_me_else or trust_me - just the goal
        op_codes = [instr[0] for instr in instructions]
        assert OP_TRY_ME_ELSE not in op_codes
        assert OP_TRUST_ME not in op_codes
        assert OP_EXECUTE in op_codes or OP_CALL in op_codes
