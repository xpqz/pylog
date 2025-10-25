"""Tests for WAM if-then-else compilation.

Tests compilation of (Cond -> Then ; Else) patterns into WAM bytecode using
get_level and cut instructions for proper commit semantics.
"""

from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.ast.clauses import Clause
from prolog.wam.codegen import compile_clause
from prolog.wam.instructions import (
    OP_GET_LEVEL,
    OP_CUT,
    OP_TRY_ME_ELSE,
    OP_TRUST_ME,
)


class TestIfThenElseCompilation:
    """Test compilation of if-then-else patterns."""

    def test_simple_if_then_else(self):
        """Test simple if-then-else compilation generates correct instructions."""
        # test(X) :- (1=1 -> X=yes ; X=no)
        clause = Clause(
            head=Struct("test", (Var(0, "X"),)),
            body=(
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Struct("=", (Int(1), Int(1))),
                                Struct("=", (Var(0, "X"), Atom("yes"))),
                            ),
                        ),
                        Struct("=", (Var(0, "X"), Atom("no"))),
                    ),
                ),
            ),
        )

        # Compile the clause
        instructions = compile_clause(clause)

        # Extract opcodes (filtering out labels)
        opcodes = [
            instr[0]
            for instr in instructions
            if isinstance(instr, tuple) and len(instr) > 0
        ]

        # Should contain GET_LEVEL to save choicepoint
        assert OP_GET_LEVEL in opcodes
        # Should contain CUT to commit on success
        assert OP_CUT in opcodes
        # Should contain TRY_ME_ELSE for branching
        assert OP_TRY_ME_ELSE in opcodes
        # Should contain TRUST_ME for else branch
        assert OP_TRUST_ME in opcodes

    def test_if_then_without_else(self):
        """Test if-then without else clause (-> without semicolon)."""
        # test(X) :- X=1 -> Y=yes.
        clause = Clause(
            head=Struct("test", (Var(0, "X"),)),
            body=(
                Struct(
                    "->",
                    (
                        Struct("=", (Var(0, "X"), Int(1))),
                        Struct("=", (Var(1, "Y"), Atom("yes"))),
                    ),
                ),
            ),
        )

        # Compile the clause
        instructions = compile_clause(clause)

        # Extract opcodes
        opcodes = [
            instr[0]
            for instr in instructions
            if isinstance(instr, tuple) and len(instr) > 0
        ]

        # Should contain GET_LEVEL and CUT for if-then pattern
        assert OP_GET_LEVEL in opcodes
        assert OP_CUT in opcodes
        assert OP_TRY_ME_ELSE in opcodes
        # Should have TRUST_ME for the implicit fail branch
        assert OP_TRUST_ME in opcodes

        # The else branch should call fail
        fail_calls = [
            instr
            for instr in instructions
            if isinstance(instr, tuple)
            and len(instr) >= 2
            and instr[1] == "user:fail/0"
        ]
        assert len(fail_calls) > 0

    def test_nested_if_then_else(self):
        """Test nested if-then-else constructs compile correctly."""
        # test(X, Y, Z) :- (X=1 -> (Y=a -> Z=aa ; Z=ab) ; (Y=b -> Z=ba ; Z=bb)).
        clause = Clause(
            head=Struct("test", (Var(0, "X"), Var(1, "Y"), Var(2, "Z"))),
            body=(
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Struct("=", (Var(0, "X"), Int(1))),
                                Struct(
                                    ";",
                                    (
                                        Struct(
                                            "->",
                                            (
                                                Struct("=", (Var(1, "Y"), Atom("a"))),
                                                Struct("=", (Var(2, "Z"), Atom("aa"))),
                                            ),
                                        ),
                                        Struct("=", (Var(2, "Z"), Atom("ab"))),
                                    ),
                                ),
                            ),
                        ),
                        Struct(
                            ";",
                            (
                                Struct(
                                    "->",
                                    (
                                        Struct("=", (Var(1, "Y"), Atom("b"))),
                                        Struct("=", (Var(2, "Z"), Atom("ba"))),
                                    ),
                                ),
                                Struct("=", (Var(2, "Z"), Atom("bb"))),
                            ),
                        ),
                    ),
                ),
            ),
        )

        # Compile the clause
        instructions = compile_clause(clause)

        # Extract opcodes
        opcodes = [
            instr[0]
            for instr in instructions
            if isinstance(instr, tuple) and len(instr) > 0
        ]

        # Should contain multiple GET_LEVEL and CUT for nested if-then-else
        get_level_count = opcodes.count(OP_GET_LEVEL)
        cut_count = opcodes.count(OP_CUT)

        # We expect at least 3 if-then-else structures (1 outer + 2 inner)
        assert get_level_count >= 3
        assert cut_count >= 3

    def test_if_then_else_with_conjunction_condition(self):
        """Test if-then-else with conjunction in condition."""
        # test(X, Y) :- (X=1, Y=2 -> Z=yes ; Z=no).
        clause = Clause(
            head=Struct("test", (Var(0, "X"), Var(1, "Y"))),
            body=(
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Struct(
                                    ",",
                                    (
                                        Struct("=", (Var(0, "X"), Int(1))),
                                        Struct("=", (Var(1, "Y"), Int(2))),
                                    ),
                                ),
                                Struct("=", (Var(2, "Z"), Atom("yes"))),
                            ),
                        ),
                        Struct("=", (Var(2, "Z"), Atom("no"))),
                    ),
                ),
            ),
        )

        # Compile the clause
        instructions = compile_clause(clause)

        # Extract opcodes
        opcodes = [
            instr[0]
            for instr in instructions
            if isinstance(instr, tuple) and len(instr) > 0
        ]

        # Should contain standard if-then-else instructions
        assert OP_GET_LEVEL in opcodes
        assert OP_CUT in opcodes
        assert OP_TRY_ME_ELSE in opcodes
        assert OP_TRUST_ME in opcodes

    def test_if_then_else_continuation(self):
        """Test if-then-else with continuation goals."""
        # test(X, Y) :- (X=1 -> Y=yes ; Y=no), Y=yes.
        clause = Clause(
            head=Struct("test", (Var(0, "X"), Var(1, "Y"))),
            body=(
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Struct("=", (Var(0, "X"), Int(1))),
                                Struct("=", (Var(1, "Y"), Atom("yes"))),
                            ),
                        ),
                        Struct("=", (Var(1, "Y"), Atom("no"))),
                    ),
                ),
                Struct("=", (Var(1, "Y"), Atom("yes"))),
            ),
        )

        # Compile the clause
        instructions = compile_clause(clause)

        # Extract opcodes
        opcodes = [
            instr[0]
            for instr in instructions
            if isinstance(instr, tuple) and len(instr) > 0
        ]

        # Should contain if-then-else instructions
        assert OP_GET_LEVEL in opcodes
        assert OP_CUT in opcodes
        assert OP_TRY_ME_ELSE in opcodes
        assert OP_TRUST_ME in opcodes

        # The continuation (Y=yes) should appear after both branches
        # Count calls to =/2
        eq_calls = [
            instr
            for instr in instructions
            if isinstance(instr, tuple)
            and len(instr) >= 2
            and isinstance(instr[1], str)
            and "=/2" in instr[1]
        ]
        # Should have at least 4 calls to =/2 (condition + 2 branches + continuation for each branch)
        assert len(eq_calls) >= 4

    def test_if_then_else_instruction_pattern(self):
        """Test that if-then-else generates the expected instruction pattern."""
        # Simple test to verify the exact pattern
        clause = Clause(
            head=Struct("test", (Var(0, "X"),)),
            body=(
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Atom("true"),  # Condition
                                Struct("=", (Var(0, "X"), Atom("yes"))),  # Then
                            ),
                        ),
                        Struct("=", (Var(0, "X"), Atom("no"))),  # Else
                    ),
                ),
            ),
        )

        instructions = compile_clause(clause)

        # Find the pattern: GET_LEVEL followed by TRY_ME_ELSE
        found_pattern = False
        for i in range(len(instructions) - 1):
            if (
                isinstance(instructions[i], tuple)
                and len(instructions[i]) >= 1
                and instructions[i][0] == OP_GET_LEVEL
            ):
                # Check if TRY_ME_ELSE follows soon after
                for j in range(i + 1, min(i + 3, len(instructions))):
                    if (
                        isinstance(instructions[j], tuple)
                        and len(instructions[j]) >= 1
                        and instructions[j][0] == OP_TRY_ME_ELSE
                    ):
                        found_pattern = True
                        break

        assert (
            found_pattern
        ), "Expected pattern GET_LEVEL followed by TRY_ME_ELSE not found"
