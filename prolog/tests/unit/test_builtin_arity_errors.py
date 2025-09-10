"""
Tests for builtin predicates with wrong arity.
These tests cover error paths for arity checking in builtins.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.engine.engine import Engine
from prolog.tests.helpers import program


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestUnificationBuiltinArity:
    """Test arity errors for unification builtins."""

    def test_unify_wrong_arity(self, empty_engine):
        """Test =/2 with wrong number of arguments."""
        # = with 1 argument
        query1 = Struct("=", (Int(5),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # = with 3 arguments
        query2 = Struct("=", (Int(5), Int(5), Int(5)))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0
        
        # = with 0 arguments
        query3 = Struct("=", ())
        solutions3 = empty_engine.run([query3])
        assert len(solutions3) == 0

    def test_not_unify_wrong_arity(self, empty_engine):
        """Test \\=/2 with wrong number of arguments."""
        # \\= with 1 argument
        query1 = Struct("\\=", (Int(5),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # \\= with 3 arguments
        query2 = Struct("\\=", (Int(5), Int(6), Int(7)))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0


class TestTypeCheckBuiltinArity:
    """Test arity errors for type checking builtins."""

    def test_var_wrong_arity(self, empty_engine):
        """Test var/1 with wrong number of arguments."""
        # var with 0 arguments
        query1 = Struct("var", ())
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # var with 2 arguments
        query2 = Struct("var", (Var(0, "X"), Var(1, "Y")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0

    def test_nonvar_wrong_arity(self, empty_engine):
        """Test nonvar/1 with wrong number of arguments."""
        # nonvar with 0 arguments
        query1 = Struct("nonvar", ())
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # nonvar with 2 arguments
        query2 = Struct("nonvar", (Int(5), Int(6)))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0

    def test_atom_wrong_arity(self, empty_engine):
        """Test atom/1 with wrong number of arguments."""
        # atom with 0 arguments
        query1 = Struct("atom", ())
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # atom with 2 arguments
        query2 = Struct("atom", (Atom("a"), Atom("b")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0


class TestMetaBuiltinArity:
    """Test arity errors for meta-predicates."""

    def test_call_wrong_arity(self, empty_engine):
        """Test call/1 with wrong number of arguments."""
        # call with 0 arguments
        query1 = Struct("call", ())
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # call with 2 arguments
        query2 = Struct("call", (Atom("true"), Atom("extra")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0

    def test_catch_wrong_arity(self, empty_engine):
        """Test catch/3 with wrong number of arguments."""
        # catch with 1 argument
        query1 = Struct("catch", (Atom("true"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # catch with 2 arguments
        query2 = Struct("catch", (Atom("true"), Var(0, "X")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0
        
        # catch with 4 arguments
        query3 = Struct("catch", (
            Atom("true"),
            Var(0, "X"),
            Atom("fail"),
            Atom("extra")
        ))
        solutions3 = empty_engine.run([query3])
        assert len(solutions3) == 0

    def test_throw_wrong_arity(self, empty_engine):
        """Test throw/1 with wrong number of arguments."""
        # throw with 0 arguments
        query1 = Struct("catch", (
            Struct("throw", ()),
            Var(0, "X"),
            Atom("true")
        ))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0  # throw fails
        
        # throw with 2 arguments
        query2 = Struct("catch", (
            Struct("throw", (Atom("e"), Atom("extra"))),
            Var(1, "Y"),
            Atom("true")
        ))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0  # throw fails


class TestStructureBuiltinArity:
    """Test arity errors for structure manipulation builtins."""

    def test_functor_wrong_arity(self, empty_engine):
        """Test functor/3 with wrong number of arguments."""
        # functor with 1 argument
        query1 = Struct("functor", (Atom("foo"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # functor with 2 arguments
        query2 = Struct("functor", (Atom("foo"), Atom("foo")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0
        
        # functor with 4 arguments
        query3 = Struct("functor", (
            Atom("foo"),
            Atom("foo"),
            Int(0),
            Atom("extra")
        ))
        solutions3 = empty_engine.run([query3])
        assert len(solutions3) == 0

    def test_arg_wrong_arity(self, empty_engine):
        """Test arg/3 with wrong number of arguments."""
        # arg with 1 argument
        query1 = Struct("arg", (Int(1),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # arg with 2 arguments
        query2 = Struct("arg", (Int(1), Struct("f", (Atom("a"),))))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0
        
        # arg with 4 arguments
        query3 = Struct("arg", (
            Int(1),
            Struct("f", (Atom("a"),)),
            Var(0, "X"),
            Atom("extra")
        ))
        solutions3 = empty_engine.run([query3])
        assert len(solutions3) == 0

    def test_univ_wrong_arity(self, empty_engine):
        """Test =../2 with wrong number of arguments."""
        # =.. with 1 argument
        query1 = Struct("=..", (Atom("foo"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # =.. with 3 arguments
        from prolog.ast.terms import List
        query2 = Struct("=..", (
            Atom("foo"),
            List((Atom("foo"),)),
            Atom("extra")
        ))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0


class TestControlBuiltinArity:
    """Test arity errors for control flow builtins."""

    def test_cut_wrong_arity(self, empty_engine):
        """Test !/0 with wrong number of arguments."""
        # ! with 1 argument
        query1 = Struct("!", (Atom("arg"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0
        
        # ! with 2 arguments
        query2 = Struct("!", (Atom("arg1"), Atom("arg2")))
        solutions2 = empty_engine.run([query2])
        assert len(solutions2) == 0

    def test_true_wrong_arity(self, empty_engine):
        """Test true/0 with wrong number of arguments."""
        # true with 1 argument
        query1 = Struct("true", (Atom("arg"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0

    def test_fail_wrong_arity(self, empty_engine):
        """Test fail/0 with wrong number of arguments."""
        # fail with 1 argument
        query1 = Struct("fail", (Atom("arg"),))
        solutions1 = empty_engine.run([query1])
        assert len(solutions1) == 0