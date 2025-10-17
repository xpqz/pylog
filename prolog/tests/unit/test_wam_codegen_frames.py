"""Tests for WAM clause finalization (frame management and last call optimization).

Tests the finalization step that adds allocate/deallocate/proceed/execute to complete
clause compilation. Covers frame management for permanent variables and last call
optimization (LCO) for tail recursion.
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Var, Struct
from prolog.wam.liveness import classify_vars
from prolog.wam.regalloc import allocate_registers
from prolog.wam.codegen import compile_head, compile_body
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_DEALLOCATE,
    OP_PROCEED,
    OP_EXECUTE,
    OP_CALL,
)


# Placeholder for finalize_clause - will be implemented in prolog/wam/codegen.py
def finalize_clause(head_instructions, body_instructions, perm_count):
    """Add frame management and return instructions to complete clause.

    Args:
        head_instructions: Instructions from compile_head()
        body_instructions: Instructions from compile_body()
        perm_count: Number of permanent variables (Y registers)

    Returns:
        Complete instruction sequence with allocate/deallocate/proceed or execute
    """
    raise NotImplementedError("To be implemented after test review")


class TestAllocateAndDeallocate:
    """Frame allocation and deallocation for permanent variables."""

    def test_allocate_for_permanent_vars(self):
        """Clause with permanent variables emits allocate K at start."""
        # p(X) :- q(X), r(X).  (X is permanent - appears after call)
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(Struct("q", (x,)), Struct("r", (x,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # First instruction after head should be allocate 1
        assert instructions[0] == (OP_ALLOCATE, 1)

    def test_no_allocate_for_temporary_only(self):
        """Clause with only temporary variables has no allocate."""
        # p(X) :- q(X).  (X is temporary - single goal)
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # No allocate instruction
        allocates = [i for i in instructions if i[0] == OP_ALLOCATE]
        assert len(allocates) == 0

    def test_deallocate_before_return(self):
        """Normal return includes deallocate before proceed."""
        # p(X) :- q(X), r(X).  (X permanent, no LCO because r not last optimizable)
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(Struct("q", (x,)), Struct("r", (x,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Should end with: ..., deallocate, proceed
        assert instructions[-2][0] == OP_DEALLOCATE
        assert instructions[-1][0] == OP_PROCEED

    def test_allocate_count_matches_permanents(self):
        """allocate K where K equals number of permanent variables."""
        # p(X, Y, Z) :- q(X), r(Y), s(Z).  (all permanent)
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Struct("p", (x, y, z)),
            body=(Struct("q", (x,)), Struct("r", (y,)), Struct("s", (z,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # allocate 3 (three permanent vars)
        assert instructions[0] == (OP_ALLOCATE, 3)


class TestLastCallOptimization:
    """Last call optimization (LCO) - execute instead of call+proceed."""

    def test_lco_single_goal_temporary(self):
        """Single goal with temporary variables uses execute."""
        # p(X) :- q(X).  (X temporary, last goal -> execute)
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Last instruction should be execute, not call+proceed
        assert instructions[-1][0] == OP_EXECUTE
        proceeds = [i for i in instructions if i[0] == OP_PROCEED]
        assert len(proceeds) == 0

    def test_lco_multi_goal(self):
        """Multiple goals: call for non-last, execute for last."""
        # p(X) :- q(X), r(X).  (X permanent but last goal optimizable)
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(Struct("q", (x,)), Struct("r", (x,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # First goal uses call
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 1
        assert "user:q/1" in calls[0]

        # Last goal uses execute (with deallocate before it)
        executes = [i for i in instructions if i[0] == OP_EXECUTE]
        assert len(executes) == 1
        assert "user:r/1" in executes[0]

    def test_lco_with_permanents_deallocates_before_execute(self):
        """LCO with permanent variables: deallocate before execute."""
        # p(X, Y) :- q(X), r(Y).  (both permanent, last goal optimizable)
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(Struct("q", (x,)), Struct("r", (y,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Should end with: ..., call q/1, deallocate, execute r/1
        assert instructions[-2][0] == OP_DEALLOCATE
        assert instructions[-1][0] == OP_EXECUTE


class TestProceedVsExecute:
    """Distinguish between proceed (normal return) and execute (tail call)."""

    def test_proceed_after_non_last_call(self):
        """Non-optimizable clauses end with proceed."""
        # p :- q, r, fail.  (fail at end prevents LCO)
        clause = Clause(
            head=Atom("p"),
            body=(Atom("q"), Atom("r"), Atom("fail")),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Ends with proceed (LCO not applicable with fail)
        assert instructions[-1][0] == OP_PROCEED

    def test_execute_replaces_call_proceed(self):
        """Last goal eligible for LCO: execute instead of call+proceed."""
        # p :- q, r.  (r is last, no permanent vars -> execute)
        clause = Clause(head=Atom("p"), body=(Atom("q"), Atom("r")))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # First goal is call
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 1

        # Last instruction is execute, no proceed
        assert instructions[-1][0] == OP_EXECUTE
        proceeds = [i for i in instructions if i[0] == OP_PROCEED]
        assert len(proceeds) == 0


class TestMixedCases:
    """Facts, empty bodies, and various clause structures."""

    def test_fact_no_body(self):
        """Fact with no body emits only proceed."""
        # p.
        clause = Clause(head=Atom("p"), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Just proceed (no allocate, no calls)
        assert instructions[-1] == (OP_PROCEED,)
        allocates = [i for i in instructions if i[0] == OP_ALLOCATE]
        assert len(allocates) == 0

    def test_temporary_vars_single_goal(self):
        """Single goal with temporary variables: execute, no allocate/deallocate."""
        # p(X) :- q(X).  (X temporary)
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # No allocate, ends with execute
        allocates = [i for i in instructions if i[0] == OP_ALLOCATE]
        assert len(allocates) == 0
        assert instructions[-1][0] == OP_EXECUTE

    def test_permanent_vars_multi_goal(self):
        """Multiple goals with permanent variables: allocate, then deallocate+execute."""
        # p(X, Y) :- q(X), r(Y).  (both permanent)
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(Struct("q", (x,)), Struct("r", (y,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # allocate 2 at start
        assert instructions[0] == (OP_ALLOCATE, 2)

        # deallocate before execute at end
        assert instructions[-2][0] == OP_DEALLOCATE
        assert instructions[-1][0] == OP_EXECUTE


class TestEdgeCases:
    """Empty bodies, complex sequences, and corner cases."""

    def test_empty_body(self):
        """Empty body (fact) emits just proceed."""
        # p :- true.  (equivalent to p.)
        clause = Clause(head=Atom("p"), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Just proceed
        assert instructions[-1] == (OP_PROCEED,)

    def test_complex_body_with_lco(self):
        """Three goals: first two call, last execute."""
        # p(X) :- q(X), r(X), s(X).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(Struct("q", (x,)), Struct("r", (x,)), Struct("s", (x,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # Two calls (q and r)
        calls = [i for i in instructions if i[0] == OP_CALL]
        assert len(calls) == 2

        # Last is execute (s)
        assert instructions[-1][0] == OP_EXECUTE
        assert "user:s/1" in instructions[-1]

    def test_head_with_permanent_vars(self):
        """Head variables that become permanent are tracked correctly."""
        # p(X, Y) :- q(Y), r(X, Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(Struct("q", (y,)), Struct("r", (x, y))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        head_instrs = compile_head(clause, regmap, len(perm))
        body_instrs = compile_body(clause, regmap)
        instructions = finalize_clause(head_instrs, body_instrs, len(perm))

        # allocate for permanent vars
        assert instructions[0] == (OP_ALLOCATE, 2)

        # deallocate before execute
        assert instructions[-2][0] == OP_DEALLOCATE
        assert instructions[-1][0] == OP_EXECUTE
