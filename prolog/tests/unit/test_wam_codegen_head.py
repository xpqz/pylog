"""Tests for WAM head compilation (get sequences).

Head compilation generates get/unify instruction sequences from clause heads.
Tests verify correct get_variable/get_value/get_constant/get_structure emission
with proper register allocation and unify sequences for structures.

Note: Argument register indices (Aj) are 0-based in emitted instructions.
Test comments use human-friendly "A1, A2..." notation but assertions use 0-based ints.
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.wam.liveness import classify_vars
from prolog.wam.regalloc import allocate_registers
from prolog.wam.codegen import compile_head
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_GET_VARIABLE,
    OP_GET_VALUE,
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_UNIFY_VARIABLE,
    OP_UNIFY_VALUE,
    OP_UNIFY_CONSTANT,
)


class TestBasicGetSequences:
    """Basic get instruction emission."""

    def test_get_variable_first_occurrence(self):
        """Single variable, first occurrence in head."""
        # p(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_variable X0, A1
        assert len(instructions) == 1
        assert instructions[0][0] == OP_GET_VARIABLE
        assert instructions[0][1] == ("X", 0)  # X0
        assert instructions[0][2] == 0  # A1 (0-based)

    def test_get_value_subsequent_occurrence(self):
        """Variable appears twice - first get_variable, second get_value."""
        # p(X, X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x, x)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_variable X0, A1; get_value X0, A2
        assert len(instructions) == 2
        assert instructions[0] == (OP_GET_VARIABLE, ("X", 0), 0)  # A1 (0-based)
        assert instructions[1] == (OP_GET_VALUE, ("X", 0), 1)  # A2 (0-based)

    def test_get_constant_atom(self):
        """Atom constant in head."""
        # p(foo).
        clause = Clause(head=Struct("p", (Atom("foo"),)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_constant foo, A1
        assert len(instructions) == 1
        assert instructions[0][0] == OP_GET_CONSTANT
        assert instructions[0][1] == "foo"
        assert instructions[0][2] == 0  # A1 (0-based)

    def test_get_constant_integer(self):
        """Integer constant in head."""
        # p(42).
        clause = Clause(head=Struct("p", (Int(42),)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_constant 42, A1
        assert len(instructions) == 1
        assert instructions[0] == (OP_GET_CONSTANT, 42, 0)  # A1 (0-based)


class TestStructureCompilation:
    """Structure (get_structure + unify sequences)."""

    def test_get_structure_simple(self):
        """Simple structure with one argument."""
        # p(f(X)).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (Struct("f", (x,)),)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_structure f/1, A1; unify_variable X0
        assert len(instructions) == 2
        assert instructions[0][0] == OP_GET_STRUCTURE
        assert instructions[0][1] == ("f", 1)  # functor/arity
        assert instructions[0][2] == 0  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))

    def test_get_structure_multiple_args(self):
        """Structure with multiple arguments."""
        # p(f(X, a)).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (Struct("f", (x, Atom("a"))),)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected:
        # get_structure f/2, A1
        # unify_variable X0
        # unify_constant a
        assert len(instructions) == 3
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[2] == (OP_UNIFY_CONSTANT, "a")

    def test_get_structure_nested(self):
        """Nested structures."""
        # p(f(g(X))).
        x = Var(id=1, hint="X")
        inner = Struct("g", (x,))
        outer = Struct("f", (inner,))
        clause = Clause(head=Struct("p", (outer,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected:
        # get_structure f/1, A1
        # unify_variable X1  (temp register for g(...))
        # get_structure g/1, X1
        # unify_variable X0  (the X variable)
        assert len(instructions) == 4
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 1), 0)  # A1 (0-based)
        assert instructions[1][0] == OP_UNIFY_VARIABLE
        assert instructions[1][1][0] == "X"  # Temp register for nested struct
        assert instructions[2][0] == OP_GET_STRUCTURE
        assert instructions[2][1] == ("g", 1)
        assert instructions[3] == (OP_UNIFY_VARIABLE, ("X", 0))


class TestListCompilation:
    """List compilation ([H|T] is sugar for ./2)."""

    def test_get_list_simple(self):
        """Simple list pattern [H|T]."""
        # p([H|T]).
        h = Var(id=1, hint="H")
        t = Var(id=2, hint="T")
        list_term = List(items=(h,), tail=t)
        clause = Clause(head=Struct("p", (list_term,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_structure ./2, A1; unify_variable X0; unify_variable X1
        assert len(instructions) == 3
        assert instructions[0] == (OP_GET_STRUCTURE, (".", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))  # H
        assert instructions[2] == (OP_UNIFY_VARIABLE, ("X", 1))  # T

    def test_get_list_with_constant(self):
        """List with constant head [a|T]."""
        # p([a|T]).
        t = Var(id=1, hint="T")
        list_term = List(items=(Atom("a"),), tail=t)
        clause = Clause(head=Struct("p", (list_term,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_structure ./2, A1; unify_constant a; unify_variable X0
        assert len(instructions) == 3
        assert instructions[0] == (OP_GET_STRUCTURE, (".", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_CONSTANT, "a")
        assert instructions[2] == (OP_UNIFY_VARIABLE, ("X", 0))

    def test_get_empty_list(self):
        """Empty list []."""
        # p([]).
        clause = Clause(head=Struct("p", (Atom("[]"),)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_constant [], A1
        assert len(instructions) == 1
        assert instructions[0] == (OP_GET_CONSTANT, "[]", 0)  # A1 (0-based)

    def test_get_list_two_elements(self):
        """List with two elements [a, b|T] as nested pairs."""
        # p([a, b|T]) -> p(.(a, .(b, T)))
        t = Var(id=1, hint="T")
        list_term = List(items=(Atom("a"), Atom("b")), tail=t)
        clause = Clause(head=Struct("p", (list_term,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected:
        # get_structure ./2, A1
        # unify_constant a
        # unify_variable X1 (temp for nested .(b, T))
        # get_structure ./2, X1
        # unify_constant b
        # unify_variable X0 (T)
        assert instructions[0] == (OP_GET_STRUCTURE, (".", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_CONSTANT, "a")
        assert instructions[2][0] == OP_UNIFY_VARIABLE  # Temp for nested pair
        assert instructions[3][0] == OP_GET_STRUCTURE
        assert instructions[3][1] == (".", 2)
        assert instructions[4] == (OP_UNIFY_CONSTANT, "b")
        assert instructions[5] == (OP_UNIFY_VARIABLE, ("X", 0))  # T

    def test_get_list_three_elements_with_vars(self):
        """List with three variable elements [X, Y, Z]."""
        # p([X, Y, Z]) -> p(.(X, .(Y, .(Z, []))))
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        list_term = List(items=(x, y, z), tail=Atom("[]"))
        clause = Clause(head=Struct("p", (list_term,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected nested structure:
        # get_structure ./2, A1
        # unify_variable X0 (X)
        # unify_variable X3 (temp for .(Y, .(Z, [])))
        # get_structure ./2, X3
        # unify_variable X1 (Y)
        # unify_variable X4 (temp for .(Z, []))
        # get_structure ./2, X4
        # unify_variable X2 (Z)
        # unify_constant []
        assert instructions[0] == (OP_GET_STRUCTURE, (".", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))  # X
        # Check nested structure pattern continues
        assert instructions[2][0] == OP_UNIFY_VARIABLE  # Temp reg
        assert instructions[3][0] == OP_GET_STRUCTURE
        assert instructions[3][1] == (".", 2)


class TestPermanentVariables:
    """Allocate K when permanent variables present."""

    def test_allocate_for_permanent_vars(self):
        """Clause with permanent variables emits allocate K."""
        # p(X) :- q(X), r(X).  (X permanent)
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(Struct("q", (x,)), Struct("r", (x,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: allocate 1; get_variable Y0, A1
        assert instructions[0] == (OP_ALLOCATE, 1)
        assert instructions[1] == (OP_GET_VARIABLE, ("Y", 0), 0)

    def test_no_allocate_for_temporary_only(self):
        """Clause with only temporary variables has no allocate."""
        # p(X).  (fact, X temporary)
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_variable X0, A1 (no allocate)
        assert instructions[0][0] == OP_GET_VARIABLE
        assert instructions[0][1] == ("X", 0)
        # No allocate instruction present
        assert not any(instr[0] == OP_ALLOCATE for instr in instructions)

    def test_allocate_multiple_permanents(self):
        """Multiple permanent variables."""
        # p(X, Y, Z) :- q(X, Y, Z), r(X, Y, Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Struct("p", (x, y, z)),
            body=(
                Struct("q", (x, y, z)),
                Struct("r", (x, y, z)),
            ),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: allocate 3; get_variable Y0, A1; get_variable Y1, A2; get_variable Y2, A3
        assert instructions[0] == (OP_ALLOCATE, 3)
        assert instructions[1] == (OP_GET_VARIABLE, ("Y", 0), 0)
        assert instructions[2] == (OP_GET_VARIABLE, ("Y", 1), 1)
        assert instructions[3] == (OP_GET_VARIABLE, ("Y", 2), 2)


class TestMixedArguments:
    """Mixed constants, variables, and structures."""

    def test_mixed_constants_and_variables(self):
        """Head with mixed constants and variables."""
        # p(a, X, b, Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(head=Struct("p", (Atom("a"), x, Atom("b"), y)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_constant a, A1; get_variable X0, A2; get_constant b, A3; get_variable X1, A4
        assert instructions[0] == (OP_GET_CONSTANT, "a", 0)  # A1 (0-based)
        assert instructions[1] == (OP_GET_VARIABLE, ("X", 0), 1)  # A2 (0-based)
        assert instructions[2] == (OP_GET_CONSTANT, "b", 2)  # A3 (0-based)
        assert instructions[3] == (OP_GET_VARIABLE, ("X", 1), 3)  # A4 (0-based)

    def test_mixed_structures_and_atoms(self):
        """Structures mixed with atoms."""
        # p(f(X), a, g(Y)).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (Struct("f", (x,)), Atom("a"), Struct("g", (y,)))),
            body=(),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected complex sequence with get_structure and get_constant
        assert instructions[0][0] == OP_GET_STRUCTURE  # f/1
        # get_constant a
        # get_structure g/1


class TestVariableOccurrencePatterns:
    """Advanced variable occurrence patterns."""

    def test_repeated_vars_across_structures_with_constants(self):
        """Variable repeated across structures with constants: p(f(a, X), f(X))."""
        # p(f(a, X), f(X)).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (Struct("f", (Atom("a"), x)), Struct("f", (x,)))),
            body=(),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected:
        # get_structure f/2, A1
        # unify_constant a
        # unify_variable X0 (first occurrence)
        # get_structure f/1, A2
        # unify_value X0 (subsequent occurrence)
        assert len(instructions) == 5
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_CONSTANT, "a")
        assert instructions[2] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[3] == (OP_GET_STRUCTURE, ("f", 1), 1)  # A2 (0-based)
        assert instructions[4] == (OP_UNIFY_VALUE, ("X", 0))

    def test_permanent_variable_in_structure_with_other_args(self):
        """Permanent variable inside structure with other arguments."""
        # p(f(Y, a)) :- q(Y), r(Y).  (Y permanent, mixed with constant)
        y = Var(id=1, hint="Y")
        clause = Clause(
            head=Struct("p", (Struct("f", (y, Atom("a"))),)),
            body=(Struct("q", (y,)), Struct("r", (y,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: allocate 1; get_structure f/2, A1; unify_variable Y0; unify_constant a
        assert instructions[0] == (OP_ALLOCATE, 1)
        assert instructions[1] == (OP_GET_STRUCTURE, ("f", 2), 0)  # A1 (0-based)
        assert instructions[2] == (OP_UNIFY_VARIABLE, ("Y", 0))
        assert instructions[3] == (OP_UNIFY_CONSTANT, "a")

    def test_permanent_variable_in_structure(self):
        """Permanent variable appears inside structure argument."""
        # p(f(X)) :- q(X), r(X).  (X permanent, first occurrence in structure)
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (Struct("f", (x,)),)),
            body=(Struct("q", (x,)), Struct("r", (x,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: allocate 1; get_structure f/1, A1; unify_variable Y0
        assert instructions[0] == (OP_ALLOCATE, 1)
        assert instructions[1] == (OP_GET_STRUCTURE, ("f", 1), 0)  # A1 (0-based)
        assert instructions[2] == (OP_UNIFY_VARIABLE, ("Y", 0))

    def test_repeated_variable_in_same_structure(self):
        """Variable appears twice in same structure."""
        # p(f(X, X)).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (Struct("f", (x, x)),)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: get_structure f/2, A1; unify_variable X0; unify_value X0
        assert len(instructions) == 3
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 2), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[2] == (OP_UNIFY_VALUE, ("X", 0))

    def test_shared_var_across_nested_structures(self):
        """Variable shared across nested structures."""
        # p(f(g(X)), h(X)).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (Struct("f", (Struct("g", (x,)),)), Struct("h", (x,)))),
            body=(),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected:
        # get_structure f/1, A1
        # unify_variable X1 (temp for g(...))
        # get_structure g/1, X1
        # unify_variable X0 (first occurrence of X)
        # get_structure h/1, A2
        # unify_value X0 (subsequent occurrence)
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 1), 0)  # A1 (0-based)
        assert instructions[1][0] == OP_UNIFY_VARIABLE
        assert instructions[2][0] == OP_GET_STRUCTURE
        assert instructions[2][1] == ("g", 1)
        assert instructions[3] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[4] == (OP_GET_STRUCTURE, ("h", 1), 1)  # A2 (0-based)
        assert instructions[5] == (OP_UNIFY_VALUE, ("X", 0))


class TestEdgeCases:
    """Edge cases and special scenarios."""

    def test_head_atom_no_args(self):
        """Atom predicate with no arguments."""
        # p.
        clause = Clause(head=Atom("p"), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Expected: [] (no get instructions for 0-arity)
        assert len(instructions) == 0

    def test_deep_nesting(self):
        """Deeply nested structures."""
        # p(f(g(h(X)))).
        x = Var(id=1, hint="X")
        h_struct = Struct("h", (x,))
        g_struct = Struct("g", (h_struct,))
        f_struct = Struct("f", (g_struct,))
        clause = Clause(head=Struct("p", (f_struct,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # Should have nested get_structure sequences
        assert instructions[0][0] == OP_GET_STRUCTURE
        assert instructions[0][1] == ("f", 1)

    def test_variable_first_in_structure(self):
        """Variable appears first in structure, then elsewhere."""
        # p(f(X), X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (Struct("f", (x,)), x)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # First occurrence in structure: unify_variable
        # Second occurrence: get_value
        assert len(instructions) == 3
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 1), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[2] == (OP_GET_VALUE, ("X", 0), 1)  # A2 (0-based)

    def test_multiple_occurrences_in_different_structures(self):
        """Variable in multiple structures."""
        # p(f(X), g(X)).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (Struct("f", (x,)), Struct("g", (x,)))), body=()
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)
        instructions = compile_head(clause, regmap, len(perm))

        # First structure: unify_variable X0
        # Second structure: unify_value X0 (subsequent occurrence)
        assert len(instructions) == 4
        assert instructions[0] == (OP_GET_STRUCTURE, ("f", 1), 0)  # A1 (0-based)
        assert instructions[1] == (OP_UNIFY_VARIABLE, ("X", 0))
        assert instructions[2] == (OP_GET_STRUCTURE, ("g", 1), 1)  # A2 (0-based)
        assert instructions[3] == (OP_UNIFY_VALUE, ("X", 0))
