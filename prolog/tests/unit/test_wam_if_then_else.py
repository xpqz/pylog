"""Tests for WAM if-then-else compilation.

Tests compilation of (Cond -> Then ; Else) patterns into WAM bytecode using
get_level and cut_to instructions for proper commit semantics.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.ast.clauses import Clause, Program

# These imports will be uncommented once the implementation is done
# from prolog.wam.compiler import compile_program
# from prolog.wam.instructions import Opcode
# from prolog.wam.machine import Machine
# from prolog.wam.debugger import run_query

# Placeholders for testing - will be replaced with actual imports
Machine = None  # noqa: F811
compile_program = None  # noqa: F811
run_query = None  # noqa: F811
Opcode = type(
    "Opcode", (), {"GET_LEVEL": None, "CUT_TO": None, "TRY_ME_ELSE": None}
)()  # noqa: F811


@pytest.mark.skip(reason="Waiting for if-then-else compilation implementation")
class TestIfThenElseCompilation:
    """Test compilation of if-then-else patterns."""

    def test_simple_if_then_else_success(self):
        """Test simple if-then-else where condition succeeds."""
        # test((1=1 -> X=yes ; X=no), X) should unify X with yes
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"),)),
                    body=Struct(
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
        )

        machine = Machine()
        compile_program(program, machine)

        # Query: test(Result)
        results = list(run_query(machine, "test", [Var(0, "Result")]))
        assert len(results) == 1
        assert results[0]["Result"] == Atom("yes")

    def test_simple_if_then_else_failure(self):
        """Test simple if-then-else where condition fails."""
        # test((1=2 -> X=yes ; X=no), X) should unify X with no
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"),)),
                    body=Struct(
                        ";",
                        (
                            Struct(
                                "->",
                                (
                                    Struct("=", (Int(1), Int(2))),
                                    Struct("=", (Var(0, "X"), Atom("yes"))),
                                ),
                            ),
                            Struct("=", (Var(0, "X"), Atom("no"))),
                        ),
                    ),
                ),
            )
        )

        machine = Machine()
        compile_program(program, machine)

        # Query: test(Result)
        results = list(run_query(machine, "test", [Var(0, "Result")]))
        assert len(results) == 1
        assert results[0]["Result"] == Atom("no")

    def test_if_then_else_commit_behavior(self):
        """Test that if-then-else commits on condition success."""
        # test(X) :- (X=1 -> Y=yes ; Y=no), X=2.
        # This should fail because after X=1 succeeds, we commit to Then branch,
        # and X=2 will fail
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"),)),
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
                        Struct("=", (Var(0, "X"), Int(2))),
                    ),
                ),
            )
        )

        machine = Machine()
        compile_program(program, machine)

        # Query: test(X)
        results = list(run_query(machine, "test", [Var(0, "X")]))
        # Should fail completely - no solutions
        assert len(results) == 0

    def test_if_then_else_no_commit_on_failure(self):
        """Test that if-then-else doesn't commit when condition fails."""
        # test(X) :- (fail -> Y=yes ; Y=no), X=2.
        # Since condition fails, we take else branch and X=2 should succeed
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"),)),
                    body=(
                        Struct(
                            ";",
                            (
                                Struct(
                                    "->",
                                    (
                                        Atom("fail"),  # Always fails
                                        Struct("=", (Var(1, "Y"), Atom("yes"))),
                                    ),
                                ),
                                Struct("=", (Var(1, "Y"), Atom("no"))),
                            ),
                        ),
                        Struct("=", (Var(0, "X"), Int(2))),
                    ),
                ),
            )
        )

        machine = Machine()
        compile_program(program, machine)

        # Query: test(X)
        results = list(run_query(machine, "test", [Var(0, "X")]))
        assert len(results) == 1
        assert results[0]["X"] == Int(2)

    def test_nested_if_then_else(self):
        """Test nested if-then-else constructs."""
        # test(X, Y) :- (X=1 -> (Y=a -> Z=aa ; Z=ab) ; (Y=b -> Z=ba ; Z=bb)).
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"), Var(1, "Y"), Var(2, "Z"))),
                    body=Struct(
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
                                                    Struct(
                                                        "=", (Var(1, "Y"), Atom("a"))
                                                    ),
                                                    Struct(
                                                        "=", (Var(2, "Z"), Atom("aa"))
                                                    ),
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
        )

        machine = Machine()
        compile_program(program, machine)

        # Test different paths through the nested if-then-else
        # X=1, Y=a should give Z=aa
        results = list(run_query(machine, "test", [Int(1), Atom("a"), Var(0, "Z")]))
        assert len(results) == 1
        assert results[0]["Z"] == Atom("aa")

        # X=1, Y=c should give Z=ab (inner else)
        results = list(run_query(machine, "test", [Int(1), Atom("c"), Var(0, "Z")]))
        assert len(results) == 1
        assert results[0]["Z"] == Atom("ab")

        # X=2, Y=b should give Z=ba (outer else, inner then)
        results = list(run_query(machine, "test", [Int(2), Atom("b"), Var(0, "Z")]))
        assert len(results) == 1
        assert results[0]["Z"] == Atom("ba")

        # X=2, Y=c should give Z=bb (outer else, inner else)
        results = list(run_query(machine, "test", [Int(2), Atom("c"), Var(0, "Z")]))
        assert len(results) == 1
        assert results[0]["Z"] == Atom("bb")

    def test_if_then_else_with_choice_in_condition(self):
        """Test if-then-else with non-deterministic condition."""
        # member(X, [1, 2, 3]).
        # test(X, Y) :- (member(X, [1,2,3]), X > 1 -> Y=found ; Y=notfound).
        # Should only find one solution (first X > 1, which is X=2)
        program = Program(
            (
                Clause(
                    head=Struct("member", (Var(0, "X"), Var(1, "Xs"))),
                    body=(
                        Struct(
                            "=",
                            (Var(1, "Xs"), Struct(".", (Var(0, "X"), Var(2, "Tail")))),
                        ),
                    ),
                ),
                Clause(
                    head=Struct("member", (Var(0, "X"), Var(1, "Xs"))),
                    body=(
                        Struct(
                            "=",
                            (Var(1, "Xs"), Struct(".", (Var(2, "H"), Var(3, "Tail")))),
                        ),
                        Struct("member", (Var(0, "X"), Var(3, "Tail"))),
                    ),
                ),
                Clause(
                    head=Struct("test", (Var(0, "X"), Var(1, "Y"))),
                    body=Struct(
                        ";",
                        (
                            Struct(
                                "->",
                                (
                                    # Condition: member(X, [1,2,3]), X > 1 (wrapped in conjunction)
                                    Struct(
                                        ",",
                                        (
                                            Struct(
                                                "member",
                                                (
                                                    Var(0, "X"),
                                                    Struct(
                                                        ".",
                                                        (
                                                            Int(1),
                                                            Struct(
                                                                ".",
                                                                (
                                                                    Int(2),
                                                                    Struct(
                                                                        ".",
                                                                        (
                                                                            Int(3),
                                                                            Atom("[]"),
                                                                        ),
                                                                    ),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                            Struct(">", (Var(0, "X"), Int(1))),
                                        ),
                                    ),
                                    # Then: Y=found
                                    Struct("=", (Var(1, "Y"), Atom("found"))),
                                ),
                            ),
                            # Else: Y=notfound
                            Struct("=", (Var(1, "Y"), Atom("notfound"))),
                        ),
                    ),
                ),
            )
        )

        machine = Machine()
        compile_program(program, machine)

        # Query: test(X, Y)
        results = list(run_query(machine, "test", [Var(0, "X"), Var(1, "Y")]))
        # Should get exactly one solution: X=2, Y=found
        # The commit after finding X=2 > 1 prevents backtracking to X=3
        assert len(results) == 1
        assert results[0]["X"] == Int(2)
        assert results[0]["Y"] == Atom("found")

    def test_if_then_without_else(self):
        """Test if-then without else clause (-> without semicolon)."""
        # In standard Prolog, (Cond -> Then) is equivalent to (Cond -> Then ; fail)
        # test(X) :- X=1 -> Y=yes.
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"),)),
                    body=Struct(
                        "->",
                        (
                            Struct("=", (Var(0, "X"), Int(1))),
                            Struct("=", (Var(1, "Y"), Atom("yes"))),
                        ),
                    ),
                ),
            )
        )

        machine = Machine()
        compile_program(program, machine)

        # Query: test(1) - should succeed
        results = list(run_query(machine, "test", [Int(1)]))
        assert len(results) == 1

        # Query: test(2) - should fail (no else clause)
        results = list(run_query(machine, "test", [Int(2)]))
        assert len(results) == 0

    def test_if_then_else_instruction_pattern(self):
        """Test that if-then-else generates correct instruction pattern."""
        # Simple if-then-else to check instruction generation
        program = Program(
            (
                Clause(
                    head=Struct("test", (Var(0, "X"),)),
                    body=Struct(
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
        )

        machine = Machine()
        compile_program(program, machine)

        # Check that the generated code contains get_level and cut_to
        # Bytecode is stored as tuples like (opcode_int, ...)
        code = machine.code
        opcodes = [
            instr[0] for instr in code if isinstance(instr, tuple) and len(instr) > 0
        ]

        # Should contain GET_LEVEL to save choicepoint
        assert Opcode.GET_LEVEL in opcodes
        # Should contain CUT_TO to commit on success
        assert Opcode.CUT_TO in opcodes
        # Should contain TRY_ME_ELSE for the branching
        assert Opcode.TRY_ME_ELSE in opcodes
