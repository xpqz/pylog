"""Tests for WAM variable liveness analysis.

Liveness analysis classifies variables as:
- Temporary (X): Dies before next call, single occurrence
- Permanent (Y): Lives across calls, multiple occurrences

Tests verify correct classification for:
- Facts (no permanent vars)
- Single call (vars temporary)
- Multiple calls (vars permanent)
- Complex bodies with mixed lifetime
- Head-only variables
"""

from prolog.ast.clauses import Clause
from prolog.ast.terms import Atom, Var, Struct, PrologDict
from prolog.wam.liveness import classify_vars, extract_vars


class TestSimpleCases:
    """Basic liveness classification tests."""

    def test_fact_no_permanents(self):
        """Facts have no permanent variables (no body)."""
        # p(a).
        clause = Clause(head=Struct("p", (Atom("a"),)), body=())
        temp, perm = classify_vars(clause)
        assert len(perm) == 0

    def test_fact_with_variable_no_permanents(self):
        """Fact with variable has no permanent vars (no calls)."""
        # p(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=())
        temp, perm = classify_vars(clause)
        assert len(perm) == 0
        assert x.id in temp

    def test_single_call_temporary(self):
        """Single call: variable is temporary (no preservation needed)."""
        # p(X) :- q(X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x,)),))
        temp, perm = classify_vars(clause)
        assert x.id in temp
        assert x.id not in perm

    def test_multiple_calls_permanent(self):
        """Variable across multiple calls is permanent."""
        # p(X) :- q(X), r(X).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(
                Struct("q", (x,)),
                Struct("r", (x,)),
            ),
        )
        temp, perm = classify_vars(clause)
        assert x.id not in temp
        assert x.id in perm


class TestComplexBodies:
    """Test classification with multiple variables."""

    def test_two_vars_one_permanent(self):
        """One var spans calls, another doesn't."""
        # p(X, Y) :- q(X), r(Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(
                Struct("q", (x,)),
                Struct("r", (y,)),
            ),
        )
        temp, perm = classify_vars(clause)
        # X only in first goal - temporary
        assert x.id in temp
        assert x.id not in perm
        # Y appears after call to q - permanent
        assert y.id not in temp
        assert y.id in perm

    def test_complex_body_three_vars(self):
        """Complex body with three variables."""
        # p(X, Y, Z) :- q(X, Y), r(Y, Z), s(Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Struct("p", (x, y, z)),
            body=(
                Struct("q", (x, y)),
                Struct("r", (y, z)),
                Struct("s", (z,)),
            ),
        )
        temp, perm = classify_vars(clause)
        # X only in first goal - temporary
        assert x.id in temp
        # Y in q and r (crosses call boundary) - permanent
        assert y.id in perm
        # Z in r and s (crosses call boundary) - permanent
        assert z.id in perm

    def test_all_permanent(self):
        """All variables span multiple calls."""
        # p(X, Y) :- q(X, Y), r(X, Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x, y)),
            body=(
                Struct("q", (x, y)),
                Struct("r", (x, y)),
            ),
        )
        temp, perm = classify_vars(clause)
        # Both vars span calls - both permanent
        assert x.id in perm
        assert y.id in perm
        assert len(temp) == 0


class TestHeadOnlyVariables:
    """Test variables that appear only in head."""

    def test_head_only_var_temporary(self):
        """Variable only in head is temporary."""
        # p(X, Y) :- q(Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(head=Struct("p", (x, y)), body=(Struct("q", (y,)),))
        temp, perm = classify_vars(clause)
        # X only in head, never referenced - temporary
        assert x.id in temp
        # Y in head and body (single goal) - temporary
        assert y.id in temp
        assert len(perm) == 0

    def test_head_vars_some_reused(self):
        """Some head vars reused in body, others not."""
        # p(X, Y, Z) :- q(X, Z), r(Z).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Struct("p", (x, y, z)),
            body=(
                Struct("q", (x, z)),
                Struct("r", (z,)),
            ),
        )
        temp, perm = classify_vars(clause)
        # X only in first goal - temporary
        assert x.id in temp
        # Y never used - temporary
        assert y.id in temp
        # Z spans calls - permanent
        assert z.id in perm


class TestBodyOnlyVariables:
    """Test variables introduced in body (not in head)."""

    def test_body_var_single_goal(self):
        """Variable introduced in body, spans multiple goals."""
        # p(X) :- q(X, Y), r(Y, Z).
        # Y spans goals, Z only in second goal
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        z = Var(id=3, hint="Z")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(
                Struct("q", (x, y)),
                Struct("r", (y, z)),
            ),
        )
        temp, perm = classify_vars(clause)
        # X and Y span calls - permanent
        assert x.id in perm
        assert y.id in perm
        # Z only in second goal - temporary
        assert z.id in temp

    def test_body_var_not_reused(self):
        """Variable introduced in body but not reused."""
        # p(X) :- q(X, Y), r(X).
        # where Y is only in q
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        clause = Clause(
            head=Struct("p", (x,)),
            body=(
                Struct("q", (x, y)),
                Struct("r", (x,)),
            ),
        )
        temp, perm = classify_vars(clause)
        # Y only in first goal, but appears after call in clause - permanent
        # (because it's introduced after position 0)
        # Actually, Y is in first goal only, so should be... let me think
        # According to algorithm: Y appears in goal i=0, but when we scan
        # goal i=1, Y doesn't appear, so it stays as permanent if added before
        # Wait, let me re-read the algorithm more carefully
        # The pseudocode says: if i > 0 and var in temp_vars, move to perm
        # Y is introduced at i=0, so goes into temp_vars initially
        # At i=1, Y doesn't appear, so stays in temp
        assert x.id in perm  # X spans calls
        # Y only in first goal - should be temporary
        assert y.id in temp


class TestEdgeCases:
    """Edge cases and corner conditions."""

    def test_empty_body_fact(self):
        """Fact with no body."""
        # p.
        clause = Clause(head=Atom("p"), body=())
        temp, perm = classify_vars(clause)
        assert len(temp) == 0
        assert len(perm) == 0

    def test_variable_same_goal_multiple_times(self):
        """Variable appears multiple times in same goal."""
        # p(X) :- q(X, X).
        x = Var(id=1, hint="X")
        clause = Clause(head=Struct("p", (x,)), body=(Struct("q", (x, x)),))
        temp, perm = classify_vars(clause)
        # Single goal, X is temporary
        assert x.id in temp
        assert len(perm) == 0

    def test_deep_nesting(self):
        """Variable in nested structure."""
        # p(f(X)) :- q(f(X)), r(X).
        x = Var(id=1, hint="X")
        clause = Clause(
            head=Struct("p", (Struct("f", (x,)),)),
            body=(
                Struct("q", (Struct("f", (x,)),)),
                Struct("r", (x,)),
            ),
        )
        temp, perm = classify_vars(clause)
        # X spans calls - permanent
        assert x.id in perm


class TestRealWorldExamples:
    """Test realistic predicate patterns."""

    def test_member_second_clause(self):
        """member([H|T], H) :- member(T, H)."""
        h = Var(id=1, hint="H")
        t = Var(id=2, hint="T")
        # member([H|T], X) :- member(T, X).
        clause = Clause(
            head=Struct(
                "member",
                (
                    Struct(".", (h, t)),  # [H|T]
                    h,
                ),
            ),
            body=(Struct("member", (t, h)),),
        )
        temp, perm = classify_vars(clause)
        # H and T both in head and body (single goal) - temporary
        assert h.id in temp
        assert t.id in temp

    def test_append_recursive_clause(self):
        """append([H|T], L, [H|R]) :- append(T, L, R)."""
        h = Var(id=1, hint="H")
        t = Var(id=2, hint="T")
        lst = Var(id=3, hint="L")
        r = Var(id=4, hint="R")

        clause = Clause(
            head=Struct(
                "append",
                (
                    Struct(".", (h, t)),  # [H|T]
                    lst,
                    Struct(".", (h, r)),  # [H|R]
                ),
            ),
            body=(Struct("append", (t, lst, r)),),
        )
        temp, perm = classify_vars(clause)
        # All vars in head and single goal - all temporary
        assert h.id in temp
        assert t.id in temp
        assert lst.id in temp
        assert r.id in temp
        assert len(perm) == 0

    def test_append_with_extra_goal(self):
        """append([H|T], L, [H|R]) :- append(T, L, R), validate(H)."""
        h = Var(id=1, hint="H")
        t = Var(id=2, hint="T")
        lst = Var(id=3, hint="L")
        r = Var(id=4, hint="R")

        clause = Clause(
            head=Struct(
                "append",
                (
                    Struct(".", (h, t)),
                    lst,
                    Struct(".", (h, r)),
                ),
            ),
            body=(
                Struct("append", (t, lst, r)),
                Struct("validate", (h,)),
            ),
        )
        temp, perm = classify_vars(clause)
        # H appears in both goals - permanent
        assert h.id in perm
        # T, L, R only in first goal - temporary
        assert t.id in temp
        assert lst.id in temp
        assert r.id in temp


class TestPrologDictSupport:
    """Test variable extraction from PrologDict terms."""

    def test_dict_with_variable_values(self):
        """Dict with variables in values."""
        # p(dict{a: X, b: Y}) :- q(X, Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        dict_term = PrologDict(pairs=((Atom("a"), x), (Atom("b"), y)))

        clause = Clause(head=Struct("p", (dict_term,)), body=(Struct("q", (x, y)),))

        temp, perm = classify_vars(clause)
        # X and Y in head and single goal - all temporary
        assert x.id in temp
        assert y.id in temp
        assert len(perm) == 0

    def test_dict_with_variable_keys(self):
        """Dict with variables in keys (rare but valid)."""
        k = Var(id=1, hint="K")
        v = Var(id=2, hint="V")
        # Can't actually create PrologDict with var keys due to validation,
        # so test extract_vars directly
        vars_found = extract_vars(Struct("test", (k, v)))
        assert vars_found == {1, 2}

    def test_dict_with_tagged_variable(self):
        """Dict with variable in tag."""
        # p(Tag{a: 1}) :- q(Tag).
        tag = Var(id=1, hint="Tag")
        dict_term = PrologDict(pairs=((Atom("a"), Atom("hello")),), tag=tag)

        clause = Clause(head=Struct("p", (dict_term,)), body=(Struct("q", (tag,)),))

        temp, perm = classify_vars(clause)
        # Tag in head and single goal - temporary
        assert tag.id in temp
        assert len(perm) == 0

    def test_dict_with_complex_nested_values(self):
        """Dict with structures containing variables."""
        # p(dict{a: f(X), b: g(Y)}) :- q(X), r(Y).
        x = Var(id=1, hint="X")
        y = Var(id=2, hint="Y")
        dict_term = PrologDict(
            pairs=((Atom("a"), Struct("f", (x,))), (Atom("b"), Struct("g", (y,))))
        )

        clause = Clause(
            head=Struct("p", (dict_term,)),
            body=(Struct("q", (x,)), Struct("r", (y,))),
        )

        temp, perm = classify_vars(clause)
        # X only in first goal - temporary
        # Y appears after call - permanent
        assert x.id in temp
        assert y.id in perm

    def test_dict_variable_spans_calls(self):
        """Variable inside dict spans multiple calls - must be permanent."""
        # p :- q(_{a:X}), r(X).
        # This is the critical case: X appears in dict in goal 0 and in goal 1
        x = Var(id=1, hint="X")
        dict_term = PrologDict(pairs=((Atom("a"), x),))

        clause = Clause(
            head=Atom("p"),
            body=(Struct("q", (dict_term,)), Struct("r", (x,))),
        )

        temp, perm = classify_vars(clause)
        # X spans calls - must be permanent
        assert x.id in perm, "X in dict spanning calls must be permanent"
        assert x.id not in temp
