"""Tests for retractall/1 builtin predicate.

Covers basic behavior, variable non-binding, head-only vs Head:-Body,
no-matches, and a light many-clauses guard.
"""

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine


def pi(name: str, arity: int) -> Struct:
    return Struct("/", (Atom(name), Int(arity)))


class TestRetractAllBasic:
    def test_remove_all_matches_and_succeeds(self):
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))

        # Call retractall(p(X)) — should not bind X
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("retractall", (Struct("p", (x,)),))])
        assert len(sols) == 1
        # X remains unbound
        assert isinstance(sols[0]["X"], Var)

        # All p/1 facts removed
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("p", (Int(2),))])) == 0

    def test_fails_when_not_dynamic(self):
        engine = Engine(Program(()))
        # Not declared dynamic: retractall should fail
        sols = engine.run([Struct("retractall", (Struct("q", (Int(1),)),))])
        assert len(sols) == 0


class TestHeadOnlyVsFullClause:
    def test_head_only_removes_rules(self):
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("r", 1),)))
        # r(1) :- true.
        rule = Struct(":-", (Struct("r", (Int(1),)), Atom("true")))
        assert engine._execute_builtin(Struct("assertz", (rule,)))

        # r(1) succeeds before retractall
        assert len(engine.run([Struct("r", (Int(1),))])) == 1
        # retractall(r(X)) removes the rule
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("retractall", (Struct("r", (x,)),))])
        assert len(sols) == 1
        assert len(engine.run([Struct("r", (Int(1),))])) == 0

    def test_full_clause_removes_matching_rule_only(self):
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("s", 1),)))
        # s(X) :- t(X). and s(1).
        rule = Struct(":-", (Struct("s", (Var(0, "X"),)), Struct("t", (Var(0, "X"),))))
        assert engine._execute_builtin(Struct("assertz", (rule,)))
        assert engine._execute_builtin(Struct("assertz", (Struct("s", (Int(1),)),)))

        # Full-clause retractall removes rule, keeps fact
        assert engine._execute_builtin(Struct("retractall", (rule,)))
        # s(1) fact still present
        assert len(engine.run([Struct("s", (Int(1),))])) == 1


class TestManyClausesGuard:
    def test_many_clauses_removed(self):
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("m", 1),)))
        # Insert many facts
        for i in range(50):
            assert engine._execute_builtin(Struct("assertz", (Struct("m", (Int(i),)),)))

        # Remove all
        wild = Var(engine.store.new_var("Y"), "Y")
        engine._query_vars = [(wild.id, "Y")]
        sols = engine.run([Struct("retractall", (Struct("m", (wild,)),))])
        assert len(sols) == 1
        # All removed
        assert len(engine.run([Struct("m", (Int(42),))])) == 0
