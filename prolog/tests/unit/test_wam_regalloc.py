"""Tests for WAM register allocation.

Register allocation assigns X (temporary) and Y (permanent) register indices
to variables based on liveness analysis results.

Tests verify:
- Temporary variables get X registers
- Permanent variables get Y registers
- Y register indices are stable and deterministic
- Register indices are sequential (0, 1, 2, ...)
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Var, Struct
from prolog.wam.liveness import classify_vars
from prolog.wam.regalloc import allocate_registers, debug_registers


class TestBasicAllocation:
    """Basic register allocation tests."""

    def test_single_temporary_variable(self):
        """Single temporary variable gets X register."""
        # p(X) :- q(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x,)),))

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # X is temporary, should get X register
        assert x.id in regmap
        reg_type, reg_idx = regmap[x.id]
        assert reg_type == "X"
        assert isinstance(reg_idx, int)
        assert reg_idx >= 0

    def test_single_permanent_variable(self):
        """Single permanent variable gets Y0."""
        # p(X) :- q(X), r(X).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)), body=(Struct("q", (x,)), Struct("r", (x,)))
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # X is permanent, should get Y0
        assert x.id in regmap
        assert regmap[x.id] == ("Y", 0)

    def test_two_permanent_variables_sequential(self):
        """Multiple permanent variables get sequential Y indices."""
        # p(X, Y) :- q(X, Y), r(X, Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(Struct("q", (x, y)), Struct("r", (x, y))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Both permanent, sequential Y registers
        assert regmap[x.id][0] == "Y"
        assert regmap[y.id][0] == "Y"
        assert regmap[x.id][1] != regmap[y.id][1]  # Different indices
        assert set([regmap[x.id][1], regmap[y.id][1]]) == {0, 1}  # Y0, Y1


class TestStableOrdering:
    """Test that Y register assignment is deterministic."""

    def test_y_registers_stable_across_calls(self):
        """Y register assignment should be stable (deterministic)."""
        # p(X, Y, Z) :- q(X, Y), r(Y, Z), s(X, Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Struct("p", (x, y, z)),
            body=(
                Struct("q", (x, y)),
                Struct("r", (y, z)),
                Struct("s", (x, z)),
            ),
        )

        temp, perm = classify_vars(clause)

        # Call allocate_registers multiple times
        regmap1 = allocate_registers(clause, temp, perm)
        regmap2 = allocate_registers(clause, temp, perm)
        regmap3 = allocate_registers(clause, temp, perm)

        # Should be identical
        assert regmap1 == regmap2 == regmap3

    def test_y_registers_ordered_by_var_id(self):
        """Y registers ordered by variable ID for stable, deterministic allocation."""
        # p(A, B, C) :- q(A, B), r(B, C), s(A, C).
        # Var IDs: A=10, B=20, C=30
        # Y allocation sorted by ID: Y0=10, Y1=20, Y2=30
        a = Var(id=10, hint="A")
        b = Var(id=20, hint="B")
        c = Var(id=30, hint="C")
        clause = Clause(
            head=Struct("p", (a, b, c)),
            body=(
                Struct("q", (a, b)),
                Struct("r", (b, c)),
                Struct("s", (a, c)),
            ),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # All permanent - Y registers should be ordered by var ID for stability
        assert regmap[a.id] == ("Y", 0)
        assert regmap[b.id] == ("Y", 1)
        assert regmap[c.id] == ("Y", 2)


class TestMixedVariables:
    """Test clauses with both temporary and permanent variables."""

    def test_mixed_temp_and_perm(self):
        """Clause with both temporary and permanent variables."""
        # p(X, Y) :- q(X), r(Y).
        # X temporary (only in first goal), Y permanent (appears after call)
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(Struct("q", (x,)), Struct("r", (y,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # X temporary -> X register
        assert regmap[x.id][0] == "X"

        # Y permanent -> Y register
        assert regmap[y.id] == ("Y", 0)

    def test_body_only_temporary(self):
        """Variable only in body, temporary."""
        # p :- q(X), r(Y).
        # X and Y both temporary (separate goals)
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Atom("p"),
            body=(Struct("q", (x,)), Struct("r", (y,))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Both temporary
        assert regmap[x.id][0] == "X"
        assert regmap[y.id][0] == "X"


class TestComplexClauses:
    """Test register allocation for complex real-world clauses."""

    def test_append_base_case(self):
        """append([], L, L) - all vars temporary (fact)."""
        lst = Var(id=1, hint="L")
        clause = Clause(
            head=Struct("append", (Atom("[]"), lst, lst)),
            body=(),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Fact: L temporary
        assert regmap[lst.id][0] == "X"

    def test_append_recursive_case(self):
        """append([H|T], L, [H|R]) :- append(T, L, R).

        Single goal - all vars temporary (liveness: no call boundary crossed).
        """
        h = Var(id=1, hint="H")
        t = Var(id=2, hint="T")
        lst = Var(id=3, hint="L")
        r = Var(id=4, hint="R")

        # [H|T] = '.'(H, T)
        ht_list = Struct(".", (h, t))
        # [H|R] = '.'(H, R)
        hr_list = Struct(".", (h, r))

        clause = Clause(
            head=Struct("append", (ht_list, lst, hr_list)),
            body=(Struct("append", (t, lst, r)),),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Single goal - all temporary (no call boundary crossed)
        assert regmap[h.id][0] == "X"
        assert regmap[t.id][0] == "X"
        assert regmap[lst.id][0] == "X"
        assert regmap[r.id][0] == "X"

    def test_member_second_clause(self):
        """member(X, [_|T]) :- member(X, T).

        Single goal - all vars temporary (no call boundary crossed).
        """
        x = Var(id=1, hint="X")
        anon = Var(id=2, hint="_")
        t = Var(id=3, hint="T")

        # [_|T] = '.'(_, T)
        list_term = Struct(".", (anon, t))

        clause = Clause(
            head=Struct("member", (x, list_term)),
            body=(Struct("member", (x, t)),),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Single goal - all temporary
        assert regmap[x.id][0] == "X"
        assert regmap[t.id][0] == "X"


class TestEdgeCases:
    """Edge cases and special scenarios."""

    def test_fact_with_no_variables(self):
        """Fact with no variables."""
        # p.
        clause = Clause(head=Atom("p"), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # No variables
        assert regmap == {}

    def test_fact_with_variable(self):
        """Fact with single variable."""
        # p(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=())

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Fact: variable is temporary
        assert regmap[x.id][0] == "X"

    def test_many_permanent_variables(self):
        """Many permanent variables get sequential Y registers."""
        # p(V1, V2, V3, V4, V5) :- q(V1, V2, V3, V4, V5), r(V1, V2, V3, V4, V5).
        vars_list = [Var(id=i, hint=f"V{i}") for i in range(1, 6)]
        clause = Clause(
            head=Struct("p", tuple(vars_list)),
            body=(Struct("q", tuple(vars_list)), Struct("r", tuple(vars_list))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # All permanent, Y0-Y4
        y_indices = [regmap[v.id][1] for v in vars_list]
        assert sorted(y_indices) == [0, 1, 2, 3, 4]


class TestDebugOutput:
    """Test debug output formatting."""

    def test_debug_registers_runs_without_error(self):
        """debug_registers produces readable output."""
        # p(X, Y) :- q(X, Y), r(X, Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(Struct("q", (x, y)), Struct("r", (x, y))),
        )

        temp, perm = classify_vars(clause)
        regmap = allocate_registers(clause, temp, perm)

        # Should not raise exception
        debug_registers(clause, regmap)
