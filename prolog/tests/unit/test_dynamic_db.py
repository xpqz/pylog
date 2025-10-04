"""Tests for dynamic database builtins: dynamic/1, assertz/1, asserta/1, retract/1, abolish/1.

Covers single indicator form and list form for dynamic/1.
"""

from prolog.ast.terms import Atom, Int, Struct, List as PrologList, Var
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine


def pi(name: str, arity: int) -> Struct:
    """Helper to build Name/Arity predicate indicator term."""
    return Struct("/", (Atom(name), Int(arity)))


def plist(items):
    """Helper to build a proper Prolog list from Python items."""
    result = Atom("[]")
    for it in reversed(items):
        result = PrologList((it,), result)
    return result


class TestDynamicSingle:
    def test_dynamic_then_assert_and_query(self):
        engine = Engine(Program(()))

        # Declare dynamic predicate p/1
        ok = engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert ok is True

        # Assert a fact p(1) and query it
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        sols = engine.run([Struct("p", (Int(1),))])
        assert len(sols) == 1

    def test_assert_fails_without_dynamic(self):
        engine = Engine(Program(()))
        # No dynamic declaration for q/2
        ok = engine._execute_builtin(
            Struct("assertz", (Struct("q", (Int(1), Int(2))),))
        )
        assert ok is False

    def test_retract_and_abolish(self):
        engine = Engine(Program(()))
        # dynamic r/1
        assert engine._execute_builtin(Struct("dynamic", (pi("r", 1),)))
        # add two facts r(1), r(2)
        assert engine._execute_builtin(Struct("assertz", (Struct("r", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("r", (Int(2),)),)))

        # retract first matching clause r(1)
        assert engine._execute_builtin(Struct("retract", (Struct("r", (Int(1),)),)))
        sols = engine.run([Struct("r", (Int(1),))])
        assert len(sols) == 0
        sols = engine.run([Struct("r", (Int(2),))])
        assert len(sols) == 1

        # abolish all r/1
        assert engine._execute_builtin(Struct("abolish", (pi("r", 1),)))
        sols = engine.run([Struct("r", (Int(2),))])
        assert len(sols) == 0


class TestDynamicListForm:
    def test_dynamic_list_form_declares_multiple(self):
        engine = Engine(Program(()))

        # dynamic([a/0, b/1])
        lst = plist([pi("a", 0), pi("b", 1)])
        assert engine._execute_builtin(Struct("dynamic", (lst,)))

        # assertz(a).
        assert engine._execute_builtin(Struct("assertz", (Atom("a"),)))
        # assertz(b(42)).
        assert engine._execute_builtin(Struct("assertz", (Struct("b", (Int(42),)),)))

        # queries succeed
        assert len(engine.run([Atom("a")])) == 1
        assert len(engine.run([Struct("b", (Int(42),))])) == 1

    def test_dynamic_list_form_invalid_element_fails(self):
        engine = Engine(Program(()))
        # dynamic([not_a_pi]). Should fail
        lst = plist([Atom("foo")])
        assert engine._execute_builtin(Struct("dynamic", (lst,))) is False

    def test_dynamic_comma_conjunction_form(self):
        engine = Engine(Program(()))
        # dynamic((a/0, b/1))
        conj = Struct(",", (pi("a", 0), pi("b", 1)))
        assert engine._execute_builtin(Struct("dynamic", (conj,)))
        # Asserts now allowed
        assert engine._execute_builtin(Struct("assertz", (Atom("a"),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("b", (Int(5),)),)))
        assert len(engine.run([Atom("a")])) == 1
        assert len(engine.run([Struct("b", (Int(5),))])) == 1

    def test_abolish_list_and_comma_forms(self):
        engine = Engine(Program(()))
        # Declare and assert
        assert engine._execute_builtin(
            Struct("dynamic", (plist([pi("a", 0), pi("b", 1)]),))
        )
        assert engine._execute_builtin(Struct("assertz", (Atom("a"),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("b", (Int(7),)),)))
        assert len(engine.run([Atom("a")])) == 1
        assert len(engine.run([Struct("b", (Int(7),))])) == 1

        # Abolish list
        assert engine._execute_builtin(Struct("abolish", (plist([pi("a", 0)]),)))
        assert len(engine.run([Atom("a")])) == 0
        assert len(engine.run([Struct("b", (Int(7),))])) == 1

        # Abolish via comma-conjunction: remove remaining b/1
        conj = Struct(",", (pi("a", 0), pi("b", 1)))
        assert engine._execute_builtin(Struct("abolish", (conj,)))
        assert len(engine.run([Struct("b", (Int(7),))])) == 0


class TestRetractUnification:
    def test_retract_binds_variables(self):
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))

        # Query retract(p(X)) should bind X to first matching (1)
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("retract", (Struct("p", (x,)),))])
        assert len(sols) == 1
        assert sols[0]["X"] == Int(1)
        # Ensure p(1) was removed, p(2) remains
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("p", (Int(2),))])) == 1
