"""Tests for WAM body compilation (put/call sequences).

Tests compile_body() which generates put/call instruction sequences from clause body goals.
Covers variable tracking, structure handling, multi-goal bodies, and generator outputs.

Note: Argument register indices (Aj) are 0-based in emitted instructions.
Test comments use human-friendly "A1, A2..." notation but assertions use 0-based ints.
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.wam.liveness import classify_vars
from prolog.wam.regalloc import allocate_registers
from prolog.wam.codegen import compile_body
from prolog.wam.instructions import (
    OP_PUT_VARIABLE,
    OP_PUT_VALUE,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_GET_STRUCTURE,
    OP_CALL,
    OP_UNIFY_VARIABLE,
    OP_UNIFY_CONSTANT,
)


class TestBasicPutSequences:
    """Basic put instruction emission for single goals."""

    def test_put_variable_first_occurrence(self):
        """Variable not in head, first occurrence in body goal."""
        # p :- q(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Atom("p"), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected: put_variable X0, A1; call user:q/1
        assert len(instructions) == 2
        assert instructions[0] == (OP_PUT_VARIABLE, ("X", 0), 0)  # A1 (0-based)
        assert instructions[1][0] == OP_CALL
        assert "q/1" in str(instructions[1][1])

    def test_put_value_from_head(self):
        """Variable from head reused in body."""
        # p(X) :- q(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected: put_value X0, A1; call user:q/1
        assert len(instructions) == 2
        assert instructions[0] == (OP_PUT_VALUE, ("X", 0), 0)  # A1 (0-based)
        assert instructions[1][0] == OP_CALL

    def test_put_constant_atom(self):
        """Atom constant in body goal."""
        # p :- q(foo).
        clause = Clause(head=Atom("p"), body=(Struct("q", (Atom("foo"),)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected: put_constant foo, A1; call user:q/1
        assert len(instructions) == 2
        assert instructions[0] == (OP_PUT_CONSTANT, "foo", 0)  # A1 (0-based)
        assert instructions[1][0] == OP_CALL

    def test_put_constant_integer(self):
        """Integer constant in body goal."""
        # p :- q(42).
        clause = Clause(head=Atom("p"), body=(Struct("q", (Int(42),)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected: put_constant 42, A1; call user:q/1
        assert len(instructions) == 2
        assert instructions[0] == (OP_PUT_CONSTANT, 42, 0)  # A1 (0-based)
        assert instructions[1][0] == OP_CALL


class TestStructureCompilation:
    """Structure (put_structure + unify sequences) in body."""

    def test_put_structure_simple(self):
        """Simple structure in body goal."""
        # p :- q(f(X)).
        x = Var(id=1, hint="X")
        clause = Clause(head=Atom("p"), body=(Struct("q", (Struct("f", (x,)),)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected:
        # put_structure f/1, X1 (temp for structure)
        # unify_variable X0 (the X variable)
        # put_value X1, A1 (pass structure to q)
        # call user:q/1
        assert instructions[0][0] == OP_PUT_STRUCTURE
        assert instructions[0][1] == ("f", 1)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[2][0] == OP_PUT_VALUE
        assert instructions[3][0] == OP_CALL

    def test_put_structure_multiple_args(self):
        """Structure with multiple arguments."""
        # p :- q(f(X, a)).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Atom("p"), body=(Struct("q", (Struct("f", (x, Atom("a"))),)),)
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected:
        # put_structure f/2, temp
        # unify_variable X0
        # unify_constant a
        # put_value temp, A1
        # call user:q/1
        assert instructions[0][0] == OP_PUT_STRUCTURE
        assert instructions[0][1] == ("f", 2)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[2] == (OP_UNIFY_CONSTANT, "a")
        assert instructions[3][0] == OP_PUT_VALUE
        assert instructions[4][0] == OP_CALL

    def test_put_structure_nested(self):
        """Nested structures in body."""
        # p :- q(f(g(X))).
        x = Var(id=1, hint="X")
        inner = Struct("g", (x,))
        outer = Struct("f", (inner,))
        clause = Clause(head=Atom("p"), body=(Struct("q", (outer,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Nested structures: outer put_structure, inner get_structure (like head)
        # Expected sequence:
        #   put_structure f/1, Xk    (k = temp register)
        #   unify_variable Xj        (j = another temp)
        #   get_structure g/1, Xj    (not put_structure - matches head semantics)
        #   unify_variable X0        (X allocated to X0)
        #   put_value Xk, A1         (built structure to arg register)
        #   call user:q/1

        put_structs = [i for i in instructions if i[0] == OP_PUT_STRUCTURE]
        get_structs = [i for i in instructions if i[0] == OP_GET_STRUCTURE]

        # Outermost structure uses put_structure
        assert len(put_structs) == 1
        assert put_structs[0][0] == OP_PUT_STRUCTURE
        assert put_structs[0][1] == ("f", 1)

        # Inner nested structure uses get_structure (after unify_variable)
        assert len(get_structs) == 1
        assert get_structs[0][0] == OP_GET_STRUCTURE
        assert get_structs[0][1] == ("g", 1)

        # Final instruction is call
        assert instructions[-1][0] == OP_CALL


class TestMultiGoalBodies:
    """Multiple goals in clause body."""

    def test_two_goals_independent_vars(self):
        """Two goals with independent variables."""
        # p :- q(X), r(Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(head=Atom("p"), body=(Struct("q", (x,)), Struct("r", (y,))))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected:
        # put_variable X0, A1; call user:q/1
        # put_variable X1, A1; call user:r/1
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 2
        assert "q/1" in str(calls[0][1])
        assert "r/1" in str(calls[1][1])

    def test_two_goals_shared_var(self):
        """Two goals sharing a variable."""
        # p :- q(X, Y), r(Y, Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Atom("p"),
            body=(Struct("q", (x, y)), Struct("r", (y, z))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Y is put_variable in q (first occurrence)
        # Y is put_value in r (subsequent occurrence)
        # Z is put_variable in r (first occurrence)
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 2

    def test_three_goal_chain(self):
        """Three goals with variable threading."""
        # p :- q(X), r(X, Y), s(Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Atom("p"),
            body=(Struct("q", (x,)), Struct("r", (x, y)), Struct("s", (y,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # X: first in q, reused in r
        # Y: first in r, reused in s
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 3
        assert "q/1" in str(calls[0][1])
        assert "r/2" in str(calls[1][1])
        assert "s/1" in str(calls[2][1])


class TestGeneratorOutputs:
    """Variables introduced by non-last goals (generators)."""

    def test_generator_output_variable(self):
        """Variable introduced in first goal, used in second."""
        # p :- q(X, Y), r(Y).
        # Y is a generator output from q
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(head=Atom("p"), body=(Struct("q", (x, y)), Struct("r", (y,))))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Y must be put_variable in q (first occurrence)
        # Y must be put_value in r (subsequent, even though q "generates" it)
        # This follows WAM semantics: put sequences are for term construction
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 2

    def test_multiple_generators(self):
        """Chain of generator outputs."""
        # p :- q(X), r(X, Y), s(Y, Z), t(Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Atom("p"),
            body=(
                Struct("q", (x,)),
                Struct("r", (x, y)),
                Struct("s", (y, z)),
                Struct("t", (z,)),
            ),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # All variables should be tracked correctly
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 4


class TestCallTargets:
    """Call instruction target formatting."""

    def test_call_with_default_module(self):
        """Call defaults to user module."""
        # p :- q(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Atom("p"), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        call_instr = [i for i in instructions if i[0] == OP_CALL][0]
        # Should be "user:q/1" or similar module-qualified format
        assert "q/1" in str(call_instr[1])

    def test_call_different_arities(self):
        """Calls with different arities."""
        # p :- q(X), r(X, Y), s(X, Y, Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Atom("p"),
            body=(Struct("q", (x,)), Struct("r", (x, y)), Struct("s", (x, y, z))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 3
        assert "q/1" in str(calls[0][1])
        assert "r/2" in str(calls[1][1])
        assert "s/3" in str(calls[2][1])


class TestEdgeCases:
    """Edge cases and special scenarios."""

    def test_single_goal_body(self):
        """Single goal in body."""
        # p :- q(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Atom("p"), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Should have put sequence + call
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 1

    def test_body_with_constants_only(self):
        """Body goal with only constants."""
        # p :- q(a, b, c).
        clause = Clause(
            head=Atom("p"),
            body=(Struct("q", (Atom("a"), Atom("b"), Atom("c"))),),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Expected: three put_constant, one call
        put_consts = [i for i in instructions if i[0] == OP_PUT_CONSTANT]
        assert len(put_consts) == 3
        assert instructions[-1][0] == OP_CALL

    def test_variable_used_multiple_times_same_goal(self):
        """Variable appears multiple times in same goal."""
        # p :- q(X, X, X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Atom("p"), body=(Struct("q", (x, x, x)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # First occurrence: put_variable
        # Subsequent: put_value
        put_vars = [i for i in instructions if i[0] == OP_PUT_VARIABLE]
        put_vals = [i for i in instructions if i[0] == OP_PUT_VALUE]
        assert len(put_vars) == 1
        assert len(put_vals) == 2

    def test_list_in_body_goal(self):
        """List argument in body goal."""
        # p :- q([H|T]).
        h = Var(id=1, hint="H")
        t = Var(id=2, hint="T")
        list_term = List(items=(h,), tail=t)
        clause = Clause(head=Atom("p"), body=(Struct("q", (list_term,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # List should be compiled as ./2 structure
        put_structs = [i for i in instructions if i[0] == OP_PUT_STRUCTURE]
        assert len(put_structs) >= 1
        assert put_structs[0][1] == (".", 2)

    def test_empty_body(self):
        """Fact with no body (edge case)."""
        # p.
        clause = Clause(head=Atom("p"), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_body(clause, regmap)

        # Empty body should produce no instructions
        assert len(instructions) == 0
