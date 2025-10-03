"""Tests for extended meta-predicates.

Tests for call/2-call/8, \\+/1, copy_term/2, and unify_with_occurs_check/2.
"""

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, program


class TestExtendedCallPredicates:
    """Test call/2 through call/8 predicates."""

    def test_call_2_with_atom(self):
        """Test call/2 with atom goal."""
        prog = program(mk_fact("test", Int(1)), mk_fact("test", Int(2)))
        engine = Engine(prog)

        # call(test, X) should be equivalent to test(X)
        result = engine._builtin_call_n((Atom("test"), Int(1)))
        assert result is True

    def test_call_2_with_struct(self):
        """Test call/2 with compound goal."""
        prog = program(
            mk_fact("test", Int(1), Atom("a")), mk_fact("test", Int(2), Atom("b"))
        )
        engine = Engine(prog)

        # call(test(1), X) should be equivalent to test(1, X)
        goal_part = Struct("test", (Int(1),))
        result = engine._builtin_call_n((goal_part, Atom("a")))
        assert result is True

    def test_call_3(self):
        """Test call/3 predicate."""
        prog = program(mk_fact("triple", Int(1), Int(2), Int(3)))
        engine = Engine(prog)

        # call(triple, 1, 2, 3) should succeed
        result = engine._builtin_call_n((Atom("triple"), Int(1), Int(2), Int(3)))
        assert result is True

    def test_call_n_wrong_arity(self):
        """Test call/N with wrong number of arguments."""
        engine = Engine(Program([]))

        # call with just one argument should fail (needs at least 2)
        result = engine._builtin_call_n((Atom("test"),))
        assert result is False

        # call with no arguments should fail
        result = engine._builtin_call_n(())
        assert result is False

    def test_call_n_unbound_goal(self):
        """Test call/N with unbound variable as goal."""
        engine = Engine(Program([]))

        unbound_var = Var(engine.store.new_var("G"), "G")
        result = engine._builtin_call_n((unbound_var, Int(1)))
        assert result is False

    def test_call_n_invalid_goal(self):
        """Test call/N with invalid goal type."""
        engine = Engine(Program([]))

        # Integer as goal should fail
        result = engine._builtin_call_n((Int(42), Int(1)))
        assert result is False

        # List as goal should fail
        list_goal = PrologList((Int(1), Int(2)))
        result = engine._builtin_call_n((list_goal, Int(1)))
        assert result is False


class TestNegationAsFailure:
    """Test \\+/1 (negation as failure) predicate."""

    def test_not_provable_fails_on_true_goal(self):
        """Test \\+/1 fails when goal succeeds."""
        prog = program(mk_fact("true_fact"))
        engine = Engine(prog)

        # \\+(true_fact) should fail because true_fact succeeds
        result = engine._builtin_not_provable((Atom("true_fact"),))
        assert result is False

    def test_not_provable_succeeds_on_false_goal(self):
        """Test \\+/1 succeeds when goal fails."""
        prog = program(mk_fact("existing_fact"))
        engine = Engine(prog)

        # \\+(nonexistent_fact) should succeed because nonexistent_fact fails
        result = engine._builtin_not_provable((Atom("nonexistent_fact"),))
        assert result is True

    def test_not_provable_wrong_arity(self):
        """Test \\+/1 with wrong number of arguments."""
        engine = Engine(Program([]))

        # \\+ with no arguments should fail
        result = engine._builtin_not_provable(())
        assert result is False

        # \\+ with two arguments should fail
        result = engine._builtin_not_provable((Atom("goal1"), Atom("goal2")))
        assert result is False

    def test_not_provable_with_builtin_true(self):
        """Test \\+/1 with builtin true/0."""
        engine = Engine(Program([]))

        # \\+(true) should fail because true always succeeds
        result = engine._builtin_not_provable((Atom("true"),))
        assert result is False

    def test_not_provable_with_builtin_fail(self):
        """Test \\+/1 with builtin fail/0."""
        engine = Engine(Program([]))

        # \\+(fail) should succeed because fail always fails
        result = engine._builtin_not_provable((Atom("fail"),))
        assert result is True


class TestCopyTerm:
    """Test copy_term/2 predicate."""

    def test_copy_term_basic(self):
        """Test copy_term/2 with basic terms."""
        engine = Engine(Program([]))

        # copy_term(atom, X) should unify X with atom
        copy_var = Var(engine.store.new_var("Copy"), "Copy")
        result = engine._builtin_copy_term((Atom("hello"), copy_var))
        assert result is True

        # Check that copy_var is bound to the atom
        deref_result = engine.store.deref(copy_var.id)
        assert deref_result[0] == "BOUND"
        assert deref_result[2] == Atom("hello")

    def test_copy_term_with_variables(self):
        """Test copy_term/2 creates fresh variables."""
        engine = Engine(Program([]))

        # Create a term with a variable
        orig_var = Var(engine.store.new_var("X"), "X")
        orig_term = Struct("test", (orig_var, Int(42)))

        # Copy the term
        copy_var = Var(engine.store.new_var("Copy"), "Copy")
        result = engine._builtin_copy_term((orig_term, copy_var))
        assert result is True

        # Check that a copy was created
        deref_result = engine.store.deref(copy_var.id)
        assert deref_result[0] == "BOUND"
        copied_term = deref_result[2]
        assert isinstance(copied_term, Struct)
        assert copied_term.functor == "test"
        assert len(copied_term.args) == 2

        # The variable in the copy should be different from the original
        copied_var = copied_term.args[0]
        assert isinstance(copied_var, Var)
        assert copied_var.id != orig_var.id  # Different variable

        # But the non-variable parts should be the same
        assert copied_term.args[1] == Int(42)

    def test_copy_term_wrong_arity(self):
        """Test copy_term/2 with wrong number of arguments."""
        engine = Engine(Program([]))

        # Wrong number of arguments should fail
        result = engine._builtin_copy_term((Atom("test"),))
        assert result is False

        result = engine._builtin_copy_term((Atom("test"), Atom("copy"), Atom("extra")))
        assert result is False


class TestUnifyWithOccursCheck:
    """Test unify_with_occurs_check/2 predicate."""

    def test_unify_with_occurs_check_success(self):
        """Test unify_with_occurs_check/2 with valid unification."""
        engine = Engine(Program([]))

        # Simple unification should succeed
        var1 = Var(engine.store.new_var("X"), "X")
        result = engine._builtin_unify_with_occurs_check((var1, Int(42)))
        assert result is True

        # Check that var1 is bound to 42
        deref_result = engine.store.deref(var1.id)
        assert deref_result[0] == "BOUND"
        assert deref_result[2] == Int(42)

    def test_unify_with_occurs_check_failure(self):
        """Test unify_with_occurs_check/2 with non-unifiable terms."""
        engine = Engine(Program([]))

        # Different atoms should not unify
        result = engine._builtin_unify_with_occurs_check((Atom("a"), Atom("b")))
        assert result is False

        # Different numbers should not unify
        result = engine._builtin_unify_with_occurs_check((Int(1), Int(2)))
        assert result is False

    def test_unify_with_occurs_check_wrong_arity(self):
        """Test unify_with_occurs_check/2 with wrong number of arguments."""
        engine = Engine(Program([]))

        result = engine._builtin_unify_with_occurs_check((Atom("test"),))
        assert result is False

        result = engine._builtin_unify_with_occurs_check(
            (Atom("a"), Atom("b"), Atom("c"))
        )
        assert result is False


class TestIntegrationTests:
    """Integration tests for extended meta-predicates."""

    def test_negation_with_call(self):
        """Test \\+/1 with call/N predicates."""
        prog = program(mk_fact("exists", Int(1)))
        engine = Engine(prog)

        # \\+(call(exists, 1)) should fail because call(exists, 1) succeeds
        call_goal = Struct("call", (Atom("exists"), Int(1)))
        result = engine._builtin_not_provable((call_goal,))
        assert (
            result is False
        )  # Note: this tests the goal structure, actual execution would need solve()

    def test_copy_term_preserves_structure(self):
        """Test copy_term/2 preserves complex structure."""
        engine = Engine(Program([]))

        # Create a complex term with nested structure
        var1 = Var(engine.store.new_var("X"), "X")
        var2 = Var(engine.store.new_var("Y"), "Y")
        complex_term = Struct(
            "complex",
            (Struct("inner", (var1, Int(1))), PrologList((var2, Atom("end")))),
        )

        copy_var = Var(engine.store.new_var("Copy"), "Copy")
        result = engine._builtin_copy_term((complex_term, copy_var))
        assert result is True

        # Verify the structure was preserved
        deref_result = engine.store.deref(copy_var.id)
        assert deref_result[0] == "BOUND"
        copied = deref_result[2]
        assert isinstance(copied, Struct)
        assert copied.functor == "complex"
        assert len(copied.args) == 2

        # Check inner structure
        inner = copied.args[0]
        assert isinstance(inner, Struct)
        assert inner.functor == "inner"

        # Check list structure
        list_part = copied.args[1]
        assert isinstance(list_part, PrologList)
