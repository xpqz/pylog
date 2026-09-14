"""End-to-end tests: compile a clause head with a structure, then run it.

Every other WAM test sits on one side of the compiler/machine boundary. The
codegen tests assert on emitted instruction sequences and never build a
Machine; the machine tests hand-assemble instructions and never call codegen.
Nothing crossed the boundary, which is how the machine came to be missing
every instruction the code generator emits for a structure argument:
unify_variable, unify_value and unify_constant were declared, validated by the
loader, assembled and emitted, but had no case in step(), so any clause head
carrying a structure compiled to code that halted on its second instruction.

These tests compile with the real code generator and execute the result.
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Int, Struct, Var
from prolog.wam.codegen import compile_head
from prolog.wam.heap import TAG_CON, new_con, new_ref, new_str
from prolog.wam.instructions import OP_GET_STRUCTURE, OP_UNIFY_VARIABLE
from prolog.wam.machine import Machine
from prolog.wam.regalloc import allocate_registers
from prolog.wam.unify import deref


def head_register_map(clause):
    """Allocate registers for a clause head the way the compiler proper would."""
    var_ids = set()

    def collect(term):
        if isinstance(term, Var):
            var_ids.add(term.id)
        elif isinstance(term, Struct):
            for arg in term.args:
                collect(arg)

    collect(clause.head)
    return allocate_registers(clause, var_ids, set())


def compile_clause_head(clause):
    """Compile a clause head with the real code generator."""
    return compile_head(clause, head_register_map(clause), 0)


def build_struct(m, name, *constants):
    """Build name(c1, ..., cn) on the heap and return its STR address.

    new_str lays down the STR and functor cells and leaves the argument slots
    to the caller, which must append them in order.
    """
    str_addr = new_str(m, name, len(constants))
    for value in constants:
        new_con(m, value)
    return str_addr


class TestCompiledStructureHead:
    """A head argument that is a structure must compile and then run."""

    def test_structure_head_compiles_to_the_unify_family(self):
        """The instruction this whole exercise turns on is actually emitted."""
        clause = Clause(Struct("p", (Struct("f", (Var(0, "X"),)),)), ())

        instructions = compile_clause_head(clause)

        opcodes = [i[0] for i in instructions]

        assert OP_GET_STRUCTURE in opcodes
        assert OP_UNIFY_VARIABLE in opcodes

    def test_matching_a_structure_argument_binds_the_variable(self):
        """p(f(X)) called with f(42) binds X to 42."""
        clause = Clause(Struct("p", (Struct("f", (Var(0, "X"),)),)), ())
        m = Machine()

        # Build the caller's argument f(42) and pass it in A0.
        m.X = [build_struct(m, "f", 42)]

        m.code = compile_clause_head(clause)
        m.run()

        assert m.halted is False or m.P >= len(m.code)
        # Whichever register the allocator gave X, it now holds the argument,
        # which derefs to the constant.
        bank, idx = head_register_map(clause)[0]
        assert bank == "X"
        assert m.heap[deref(m, m.X[idx])] == (TAG_CON, 42)

    def test_matching_against_an_unbound_argument_builds_the_structure(self):
        """p(f(X)) called with an unbound argument builds f(_) on the heap."""
        clause = Clause(Struct("p", (Struct("f", (Var(0, "X"),)),)), ())
        m = Machine()

        var_addr = new_ref(m)
        m.X = [var_addr]

        m.code = compile_clause_head(clause)
        m.run()

        assert m.halted is False or m.P >= len(m.code)
        # The argument is now bound to an f/1 structure.
        built = deref(m, var_addr)
        functor_cell = m.heap[m.heap[built][1]]
        assert functor_cell[1] == ("f", 1)

    def test_a_constant_inside_a_structure_matches(self):
        """p(f(a)) called with f(a) succeeds; unify_constant carries it."""
        clause = Clause(Struct("p", (Struct("f", (Atom("a"),)),)), ())
        m = Machine()

        m.X = [build_struct(m, "f", "a")]

        m.code = compile_clause_head(clause)
        m.run()

        assert m.halted is False or m.P >= len(m.code)

    def test_a_constant_inside_a_structure_rejects_a_mismatch(self):
        """p(f(a)) called with f(b) fails rather than succeeding quietly."""
        clause = Clause(Struct("p", (Struct("f", (Atom("a"),)),)), ())
        m = Machine()

        m.X = [build_struct(m, "f", "b")]

        m.code = compile_clause_head(clause)
        m.run()

        assert m.halted is True

    def test_an_integer_inside_a_structure_matches(self):
        """Integers take the same path as atoms through unify_constant."""
        clause = Clause(Struct("p", (Struct("f", (Int(7),)),)), ())
        m = Machine()

        m.X = [build_struct(m, "f", 7)]

        m.code = compile_clause_head(clause)
        m.run()

        assert m.halted is False or m.P >= len(m.code)

    def test_a_repeated_variable_in_a_structure_unifies_both_occurrences(self):
        """p(f(X, X)) requires the two arguments to be equal.

        The second occurrence compiles to unify_value, which unifies rather
        than binds, so f(1, 2) must fail where f(1, 1) succeeds.
        """
        clause = Clause(
            Struct("p", (Struct("f", (Var(0, "X"), Var(0, "X"))),)),
            (),
        )

        def run_with(first, second):
            m = Machine()
            m.X = [build_struct(m, "f", first, second)]
            m.code = compile_clause_head(clause)
            m.run()
            return m

        assert run_with(1, 1).halted is False
        assert run_with(1, 2).halted is True
