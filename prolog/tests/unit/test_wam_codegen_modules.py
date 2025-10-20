"""Unit tests for WAM module-qualified symbol compilation.

Tests module context, symbol qualification, and M:Goal syntax.

Implementation plan:
- Add module="user" kwarg to compile_body()
- Add resolver: extract (module, goal) from Struct(":", (...)) or use default
- Emit OP_CALL for non-last goals, OP_EXECUTE for last goal
- finalize_clause() already handles OP_EXECUTE-ending bodies correctly
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Struct, Var
from prolog.wam.liveness import classify_vars
from prolog.wam.regalloc import allocate_registers
from prolog.wam.codegen import compile_head, compile_body, finalize_clause
from prolog.wam.instructions import (
    OP_CALL,
    OP_EXECUTE,
    OP_PROCEED,
)


class TestDefaultModule:
    """Test default module context (user)."""

    def test_unqualified_head_default_user(self):
        """Unqualified head compiles with current module (default user)."""
        # p(a).
        clause = Clause(head=Struct("p", (Atom("a"),)), body=())
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        # compile_head doesn't need module (head context is local)
        head_instrs = compile_head(clause, register_map, len(perm_vars))
        body_instrs = compile_body(clause, register_map, module="user")
        instructions = finalize_clause(head_instrs, body_instrs, len(perm_vars))

        # Should compile without module prefix in instructions (module comes from context)
        assert instructions[-1] == (OP_PROCEED,)

    def test_unqualified_body_call_default_user(self):
        """Unqualified body call uses current module (default user)."""
        # p :- q.
        clause = Clause(head=Atom("p"), body=(Atom("q"),))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        # Find EXECUTE instruction
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "user:q/0"

    def test_multi_goal_body_unqualified(self):
        """Multiple unqualified goals all use current module."""
        # p :- q, r, s.
        clause = Clause(head=Atom("p"), body=(Atom("q"), Atom("r"), Atom("s")))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        # Find CALL and EXECUTE instructions
        calls = [i for i in body_instrs if i[0] == OP_CALL]
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]

        assert len(calls) == 2
        assert calls[0][1] == "user:q/0"
        assert calls[1][1] == "user:r/0"

        assert len(executes) == 1
        assert executes[0][1] == "user:s/0"


class TestExplicitModule:
    """Test explicit module qualification (M:Goal syntax)."""

    def test_qualified_call_single_goal(self):
        """Explicitly qualified call uses specified module."""
        # p :- m:q.
        # Represented as Struct(":", (Atom("m"), Atom("q")))
        qualified_goal = Struct(":", (Atom("m"), Atom("q")))
        clause = Clause(head=Atom("p"), body=(qualified_goal,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        # Should compile to call m:q/0 (not user:q/0)
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "m:q/0"

    def test_qualified_call_with_args(self):
        """Qualified call with arguments uses correct module."""
        # p(X) :- m:q(X, a).
        x = Var(0, "X")
        qualified_goal = Struct(":", (Atom("m"), Struct("q", (x, Atom("a")))))
        clause = Clause(head=Struct("p", (x,)), body=(qualified_goal,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        # Should call m:q/2
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "m:q/2"

    def test_mixed_qualified_unqualified(self):
        """Mix of qualified and unqualified calls."""
        # p :- q, m:r, s.
        # q and s should be user:, r should be m:
        qualified_r = Struct(":", (Atom("m"), Atom("r")))
        clause = Clause(head=Atom("p"), body=(Atom("q"), qualified_r, Atom("s")))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        calls = [i for i in body_instrs if i[0] == OP_CALL]
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]

        assert len(calls) == 2
        assert calls[0][1] == "user:q/0"
        assert calls[1][1] == "m:r/0"

        assert len(executes) == 1
        assert executes[0][1] == "user:s/0"

    def test_system_module_builtin(self):
        """Built-in predicates can be explicitly qualified under system."""
        # p :- system:true.
        qualified_true = Struct(":", (Atom("system"), Atom("true")))
        clause = Clause(head=Atom("p"), body=(qualified_true,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "system:true/0"


class TestNonDefaultCurrentModule:
    """Test compilation with non-default current module."""

    def test_unqualified_in_custom_module(self):
        """Unqualified calls use current module context."""
        # Compiling in module "mymod"
        # p :- q.
        clause = Clause(head=Atom("p"), body=(Atom("q"),))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="mymod")

        # Should call mymod:q/0 (not user:q/0)
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "mymod:q/0"

    def test_qualified_overrides_current_module(self):
        """Explicit qualification overrides current module context."""
        # Compiling in module "mymod"
        # p :- other:q.
        qualified_q = Struct(":", (Atom("other"), Atom("q")))
        clause = Clause(head=Atom("p"), body=(qualified_q,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="mymod")

        # Should call other:q/0 (not mymod:q/0)
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "other:q/0"

    def test_multiple_modules_mixed(self):
        """Complex case: custom current module with mixed qualification."""
        # Compiling in module "m1"
        # p :- q, m2:r, s, m3:t.
        # Expected: m1:q/0, m2:r/0, m1:s/0, m3:t/0
        qualified_r = Struct(":", (Atom("m2"), Atom("r")))
        qualified_t = Struct(":", (Atom("m3"), Atom("t")))
        clause = Clause(
            head=Atom("p"), body=(Atom("q"), qualified_r, Atom("s"), qualified_t)
        )
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="m1")

        calls = [i for i in body_instrs if i[0] == OP_CALL]
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]

        assert len(calls) == 3
        assert calls[0][1] == "m1:q/0"
        assert calls[1][1] == "m2:r/0"
        assert calls[2][1] == "m1:s/0"

        assert len(executes) == 1
        assert executes[0][1] == "m3:t/0"


class TestSymbolFormat:
    """Test correct symbol format construction."""

    def test_symbol_format_single_arity(self):
        """Single-arity predicates format correctly."""
        # p :- q(a).
        clause = Clause(head=Atom("p"), body=(Struct("q", (Atom("a"),)),))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "user:q/1"

    def test_symbol_format_high_arity(self):
        """High-arity predicates format correctly."""
        # p :- q(a, b, c, d, e).
        args = tuple(Atom(x) for x in ["a", "b", "c", "d", "e"])
        clause = Clause(head=Atom("p"), body=(Struct("q", args),))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "user:q/5"

    def test_symbol_format_special_chars_in_module(self):
        """Module names with underscores and numbers format correctly."""
        # Compiling in module "my_mod_123"
        # p :- q.
        clause = Clause(head=Atom("p"), body=(Atom("q"),))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="my_mod_123")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "my_mod_123:q/0"


class TestEdgeCases:
    """Test edge cases and corner scenarios."""

    def test_nested_qualification_invalid(self):
        """Nested module qualification (m1:m2:p) should be handled."""
        # Note: This is syntactically odd in Prolog, but we should handle it
        # m1:(m2:p) - the outer qualification wins
        inner = Struct(":", (Atom("m2"), Atom("p")))
        outer = Struct(":", (Atom("m1"), inner))
        clause = Clause(head=Atom("test"), body=(outer,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        # Outer qualification should take precedence: m1::/2
        # This compiles m1:(:)/2 which is technically correct but weird
        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        # The qualified term itself becomes the goal, so we get m1::/2
        assert executes[0][1].startswith("m1:")

    def test_qualification_with_variables(self):
        """Qualified calls with variable arguments."""
        # p(X, Y) :- m:q(X, Y).
        x = Var(0, "X")
        y = Var(1, "Y")
        qualified_q = Struct(":", (Atom("m"), Struct("q", (x, y))))
        clause = Clause(head=Struct("p", (x, y)), body=(qualified_q,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "m:q/2"

    def test_empty_module_name(self):
        """Empty module name (edge case, probably invalid but test behavior)."""
        # p :- '':q.
        # This is malformed but should compile with empty module
        qualified_q = Struct(":", (Atom(""), Atom("q")))
        clause = Clause(head=Atom("p"), body=(qualified_q,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == ":q/0"  # Odd but consistent


class TestBuiltinsWithArgs:
    """Test qualified built-ins with arguments."""

    def test_qualified_builtin_with_args(self):
        """System-qualified built-in with arguments formats correctly."""
        # p(X) :- system:call(X).
        x = Var(0, "X")
        qualified_call = Struct(":", (Atom("system"), Struct("call", (x,))))
        clause = Clause(head=Struct("p", (x,)), body=(qualified_call,))
        temp_vars, perm_vars = classify_vars(clause)
        register_map = allocate_registers(clause, temp_vars, perm_vars)

        body_instrs = compile_body(clause, register_map, module="user")

        executes = [i for i in body_instrs if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert executes[0][1] == "system:call/1"
