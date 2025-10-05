"""Tests for Core Builtins (Stage 0 Phase 5).

Tests for builtin infrastructure and basic builtins like true/0, fail/0, and call/1.
"""

from prolog.ast.terms import Atom, Var, Struct, Int, List, PrologDict
from prolog.engine.engine import Engine


# Test helpers
from prolog.tests.helpers import mk_fact, mk_rule, program


class TestBuiltinInfrastructure:
    """Tests for builtin recognition and dispatch.

    Note: These test private APIs for unit testing. Most behavior
    should be verified via black-box Engine.run() tests.
    """

    def test_is_builtin_recognizes_true(self):
        """Test _is_builtin recognizes true/0 by name+arity (structural)."""
        engine = Engine(program())

        # true/0 should be recognized as builtin
        assert engine._is_builtin(Atom("true"))

        # true/1 should NOT be recognized (wrong arity)
        assert not engine._is_builtin(Struct("true", (Atom("a"),)))

    def test_is_builtin_recognizes_fail(self):
        """Test _is_builtin recognizes fail/0 (structural)."""
        engine = Engine(program())

        # fail/0 should be recognized as builtin
        assert engine._is_builtin(Atom("fail"))

        # fail/1 should NOT be recognized
        assert not engine._is_builtin(Struct("fail", (Atom("a"),)))

    def test_is_builtin_recognizes_call(self):
        """Test _is_builtin recognizes call/1-8 (structural)."""
        engine = Engine(program())

        # call/1 should be recognized as builtin
        assert engine._is_builtin(Struct("call", (Atom("test"),)))

        # call/0 should NOT be recognized
        assert not engine._is_builtin(Atom("call"))

        # call/2-8 should be recognized as builtin (ISO compliance)
        assert engine._is_builtin(Struct("call", (Atom("a"), Atom("b"))))
        assert engine._is_builtin(Struct("call", (Atom("a"), Atom("b"), Atom("c"))))
        assert engine._is_builtin(
            Struct("call", tuple(Atom(f"arg{i}") for i in range(8)))
        )

        # call/9 should NOT be recognized (beyond ISO range)
        assert not engine._is_builtin(
            Struct("call", tuple(Atom(f"arg{i}") for i in range(9)))
        )

    def test_is_builtin_recognizes_cut(self):
        """Test _is_builtin still recognizes cut (!) (structural)."""
        engine = Engine(program())

        # Cut should still be recognized
        assert engine._is_builtin(Atom("!"))

    def test_unknown_predicate_not_builtin(self):
        """Test unknown predicates are not recognized as builtins."""
        engine = Engine(program())

        assert not engine._is_builtin(Atom("unknown"))
        assert not engine._is_builtin(Struct("foo", (Atom("bar"),)))

    def test_builtin_fail_overrides_user_fact(self):
        """Test builtin fail/0 takes precedence over user fail/0 fact (semantic).

        This is the discriminative precedence test - builtin and user
        definitions disagree, proving which one actually runs.
        """
        prog = program(mk_fact("fail"))  # User defines fail/0 as a fact (would succeed)
        engine = Engine(prog)

        # Query: fail
        # User fail/0 would succeed, builtin fail/0 fails
        solutions = engine.run([Atom("fail")])

        # Should fail (builtin fail/0 wins over user fact)
        assert len(solutions) == 0

    def test_builtin_true_overrides_user_rule(self):
        """Test builtin true/0 overrides user rule with side effects (semantic)."""
        prog = program(
            # User true/0 that would bind a variable via helper
            mk_rule("true", (), Struct("helper", (Atom("bound"),))),
            mk_fact("helper", Atom("bound")),
        )
        engine = Engine(prog)

        # Query: true
        # User true/0 would create bindings, builtin true/0 doesn't
        solutions = engine.run([Atom("true")])

        # Should succeed with no bindings (builtin true/0 wins)
        assert len(solutions) == 1
        assert solutions[0] == {}  # No variables to bind

    def test_execute_builtin_dispatches_correctly(self):
        """Test _execute_builtin calls the right builtin (unit test)."""
        engine = Engine(program())

        # true should succeed
        assert engine._execute_builtin(Atom("true"))

        # fail should fail
        assert not engine._execute_builtin(Atom("fail"))

        # Unknown builtin should fail
        assert not engine._execute_builtin(Atom("unknown"))


class TestTrueAndFail:
    """Tests for true/0 and fail/0 builtins."""

    def test_true_always_succeeds(self):
        """Test true/0 always succeeds."""
        prog = program()
        engine = Engine(prog)

        # Query: true
        solutions = engine.run([Atom("true")])

        # Should have one solution with no bindings
        assert len(solutions) == 1
        assert solutions[0] == {}

    def test_fail_always_fails(self):
        """Test fail/0 always fails."""
        prog = program()
        engine = Engine(prog)

        # Query: fail
        solutions = engine.run([Atom("fail")])

        # Should have no solutions
        assert len(solutions) == 0

    def test_true_in_conjunction_continues(self):
        """Test true in conjunction allows continuation."""
        prog = program(mk_fact("test", Atom("a")), mk_fact("test", Atom("b")))
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Query: true, test(X)
        solutions = engine.run([Atom("true"), Struct("test", (x_var,))])

        # Should find both solutions (true doesn't affect search)
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")

    def test_fail_causes_backtracking(self):
        """Test fail causes immediate backtracking."""
        prog = program(
            mk_fact("choice", Atom("first")), mk_fact("choice", Atom("second"))
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Query: choice(X), fail
        # Each choice will be tried, but fail causes backtrack
        solutions = engine.run([Struct("choice", (x_var,)), Atom("fail")])

        # No solutions (fail after each choice)
        assert len(solutions) == 0

    def test_no_trail_entries_from_true_fail(self):
        """Test true/fail don't create trail entries (pure)."""
        prog = program()
        engine = Engine(prog)

        # Mark trail before
        trail_before = len(engine.trail)

        # Execute true
        engine.run([Atom("true")])

        # Trail should be unchanged (true is pure)
        assert len(engine.trail) == trail_before

        # Execute fail
        engine.run([Atom("fail")])

        # Trail should still be unchanged (fail is pure)
        assert len(engine.trail) == trail_before

    def test_true_fail_with_cut(self):
        """Test interaction of true/fail with cut."""
        prog = program(
            mk_rule(
                "test",
                (Var(0, "X"),),
                Struct("choice", (Var(0, "X"),)),
                Atom("!"),
                Atom("fail"),
            ),
            mk_fact("choice", Atom("a")),
            mk_fact("choice", Atom("b")),
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Query: test(X)
        # Should match first choice, cut, then fail
        # Cut prevents trying second choice
        solutions = engine.run([Struct("test", (x_var,))])

        # No solutions (cut then fail)
        assert len(solutions) == 0

    def test_true_idempotent(self):
        """Test multiple true/0 in conjunction still one solution."""
        prog = program()
        engine = Engine(prog)

        # Query: true, true, true
        solutions = engine.run([Atom("true"), Atom("true"), Atom("true")])

        # Should have exactly one solution
        assert len(solutions) == 1
        assert solutions[0] == {}

    def test_fail_dominates_conjunction(self):
        """Test fail in any position causes failure."""
        prog = program()
        engine = Engine(prog)

        # Query: true, fail, true
        solutions = engine.run([Atom("true"), Atom("fail"), Atom("true")])
        assert len(solutions) == 0

        # Query: fail, true
        solutions = engine.run([Atom("fail"), Atom("true")])
        assert len(solutions) == 0


class TestCall:
    """Tests for call/1 builtin."""

    def test_call_with_atom_goal(self):
        """Test call/1 with atom goal (0-arity)."""
        prog = program(mk_fact("test"))
        engine = Engine(prog)

        # Query: call(test)
        solutions = engine.run([Struct("call", (Atom("test"),))])

        # Should succeed
        assert len(solutions) == 1
        assert solutions[0] == {}

    def test_call_with_struct_goal(self):
        """Test call/1 with struct goal."""
        prog = program(mk_fact("pred", Atom("a")), mk_fact("pred", Atom("b")))
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Query: call(pred(X))
        goal = Struct("pred", (x_var,))
        solutions = engine.run([Struct("call", (goal,))])

        # Should find both solutions
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")

    def test_call_with_variable_bound_to_goal(self):
        """Test call/1 with variable bound to goal (deref once)."""
        prog = program(
            mk_rule(
                "helper",
                (Var(0, "Goal"),),
                # Unify Goal = test
                Struct("bind", (Var(0, "Goal"), Atom("test"))),
                Struct("call", (Var(0, "Goal"),)),
            ),
            mk_fact("bind", Atom("test"), Atom("test")),
            mk_fact("test"),
        )
        engine = Engine(prog)

        g_var = Var(engine.store.new_var("G"), "G")
        engine._query_vars = [(g_var.id, "G")]

        solutions = engine.run([Struct("helper", (g_var,))])

        # Should succeed with G bound to test
        assert len(solutions) == 1
        assert solutions[0]["G"] == Atom("test")

    def test_call_with_deep_variable_deref(self):
        """Test call/1 dereferences through variable chains."""
        prog = program(
            mk_rule(
                "deep",
                (Var(0, "G0"),),
                # G0 = G1, G1 = G2, G2 = test, call(G0)
                Struct("chain", (Var(0, "G0"), Var(1, "G1"))),
                Struct("chain", (Var(1, "G1"), Var(2, "G2"))),
                Struct("bind", (Var(2, "G2"), Atom("test"))),
                Struct("call", (Var(0, "G0"),)),
            ),
            mk_fact("chain", Var(0, "X"), Var(0, "X")),  # Unifies two vars
            mk_fact("bind", Atom("test"), Atom("test")),
            mk_fact("test"),
        )
        engine = Engine(prog)

        g0_var = Var(engine.store.new_var("G0"), "G0")
        engine._query_vars = [(g0_var.id, "G0")]

        solutions = engine.run([Struct("deep", (g0_var,))])

        # Should succeed after deep deref
        assert len(solutions) == 1

    def test_call_with_unbound_variable_fails(self):
        """Test call/1 with unbound variable fails."""
        prog = program()
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Query: call(X) with X unbound
        solutions = engine.run([Struct("call", (x_var,))])

        # Should fail (can't call unbound variable)
        assert len(solutions) == 0

    def test_call_with_integer_fails(self):
        """Test call/1 with integer term fails (type error)."""
        prog = program()
        engine = Engine(prog)

        # Query: call(123) - integer is not callable
        solutions = engine.run([Struct("call", (Int(123),))])

        # Should fail (can't call an integer)
        assert len(solutions) == 0

    def test_call_undefined_callable_fails(self):
        """Test call/1 with undefined but valid callable fails."""
        prog = program()
        engine = Engine(prog)

        # Query: call(nonexistent) - valid atom but no definition
        solutions = engine.run([Struct("call", (Atom("nonexistent"),))])
        assert len(solutions) == 0

        # Query: call(undefined(x)) - valid struct but no definition
        solutions = engine.run([Struct("call", (Struct("undefined", (Atom("x"),)),))])
        assert len(solutions) == 0

    def test_call_preserves_trail_and_store(self):
        """Test call/1 itself doesn't modify trail or store."""
        prog = program(mk_fact("pure_test"))
        engine = Engine(prog)

        # Mark trail and store before
        trail_before = len(engine.trail)
        store_before = len(engine.store.cells)

        # Execute call(pure_test)
        solutions = engine.run([Struct("call", (Atom("pure_test"),))])
        assert len(solutions) == 1

        # Trail and store should be unchanged (call itself is pure)
        assert len(engine.trail) == trail_before
        assert len(engine.store.cells) == store_before

    def test_call_with_cut_executes_cut(self):
        """Test call(!) executes cut."""
        prog = program(
            mk_rule(
                "test",
                (Var(0, "X"),),
                Struct("choice", (Var(0, "X"),)),
                Struct("call", (Atom("!"),)),
            ),  # call(!)
            mk_fact("choice", Atom("first")),
            mk_fact("choice", Atom("second")),
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Query: test(X)
        solutions = engine.run([Struct("test", (x_var,))])

        # Should only get first choice (cut via call)
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("first")

    def test_call_cut_with_surrounding_choicepoints(self):
        """Test call(!) removes all choicepoints in the clause."""
        prog = program(
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("a", (Var(0, "X"),)),  # Creates choicepoint
                Struct("b", (Var(1, "Y"),)),  # Creates another choicepoint
                Struct("call", (Atom("!"),)),
            ),  # Cut via call - removes ALL
            mk_fact("a", Atom("a1")),
            mk_fact("a", Atom("a2")),
            mk_fact("b", Atom("b1")),
            mk_fact("b", Atom("b2")),
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        y_var = Var(engine.store.new_var("Y"), "Y")
        engine._query_vars = [(x_var.id, "X"), (y_var.id, "Y")]

        solutions = engine.run([Struct("test", (x_var, y_var))])

        # Cut removes ALL choicepoints created since entering test/2
        # This is standard ISO Prolog behavior
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a1")
        assert solutions[0]["Y"] == Atom("b1")

    def test_call_pushes_goal(self):
        """Test call pushes goal (doesn't execute immediately).

        This is important for proper goal stack management.
        """
        prog = program(
            mk_rule("test", (), Struct("call", (Atom("a"),)), Atom("b")),
            mk_fact("a"),
            mk_fact("b"),
        )
        engine = Engine(prog)

        # Query: test
        # Should execute a then b (call doesn't bypass goal stack)
        solutions = engine.run([Atom("test")])

        assert len(solutions) == 1

    def test_call_with_builtin(self):
        """Test call/1 can call builtins."""
        prog = program()
        engine = Engine(prog)

        # Query: call(true)
        solutions = engine.run([Struct("call", (Atom("true"),))])
        assert len(solutions) == 1

        # Query: call(fail)
        solutions = engine.run([Struct("call", (Atom("fail"),))])
        assert len(solutions) == 0

    def test_nested_call(self):
        """Test nested call/1 works correctly."""
        prog = program(mk_fact("test"))
        engine = Engine(prog)

        # Query: call(call(test))
        inner_call = Struct("call", (Atom("test"),))
        solutions = engine.run([Struct("call", (inner_call,))])

        # Should succeed
        assert len(solutions) == 1

    def test_call_wrong_arity_goal_fails(self):
        """Test call/1 receiving wrong arity term fails gracefully."""
        prog = program()
        engine = Engine(prog)

        # Try to call a 'call' struct with wrong arity as goal
        # call(call(a,b)) - inner 'call' has arity 2, not callable
        bad_call = Struct("call", (Atom("a"), Atom("b")))
        solutions = engine.run([Struct("call", (bad_call,))])

        # Should fail (call/2 is not a valid callable)
        assert len(solutions) == 0

    def test_call_partially_instantiated_struct_no_match(self):
        """Test call/1 with struct that matches no clauses fails cleanly."""
        prog = program(
            mk_fact("pred", Atom("a"), Atom("1")), mk_fact("pred", Atom("b"), Atom("2"))
        )
        engine = Engine(prog)

        # Query: call(pred(c, X)) - no clause for pred(c, _)
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        goal = Struct("pred", (Atom("c"), x_var))
        solutions = engine.run([Struct("call", (goal,))])

        # Should fail cleanly with no side effects
        assert len(solutions) == 0


class TestBuiltinIntegration:
    """Integration tests for builtins with other features."""

    def test_builtins_with_backtracking(self):
        """Test builtins interact correctly with backtracking."""
        prog = program(
            mk_rule(
                "test",
                (Var(0, "X"),),
                Struct("choice", (Var(0, "X"),)),
                Atom("true"),  # Builtin in middle
                Struct("check", (Var(0, "X"),)),
            ),
            mk_fact("choice", Atom("a")),
            mk_fact("choice", Atom("b")),
            mk_fact("check", Atom("b")),  # Only b passes check
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("test", (x_var,))])

        # Should backtrack through 'a' and find 'b'
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("b")

    def test_call_with_complex_goal(self):
        """Test call/1 with complex structured goal."""
        prog = program(
            mk_fact("pred", Atom("x"), Atom("1")), mk_fact("pred", Atom("y"), Atom("2"))
        )
        engine = Engine(prog)

        a_var = Var(engine.store.new_var("A"), "A")
        b_var = Var(engine.store.new_var("B"), "B")
        engine._query_vars = [(a_var.id, "A"), (b_var.id, "B")]

        # Query: call(pred(A, B))
        goal = Struct("pred", (a_var, b_var))
        solutions = engine.run([Struct("call", (goal,))])

        # Should find both solutions
        assert len(solutions) == 2
        assert solutions[0]["A"] == Atom("x")
        assert solutions[0]["B"] == Atom("1")
        assert solutions[1]["A"] == Atom("y")
        assert solutions[1]["B"] == Atom("2")

    def test_builtins_preserve_cut_barrier(self):
        """Test builtins don't interfere with cut barrier."""
        prog = program(
            mk_rule(
                "test",
                (Var(0, "X"),),
                Struct("choice", (Var(0, "X"),)),
                Atom("true"),  # Builtin before cut
                Atom("!"),
                Atom("true"),
            ),  # Builtin after cut
            mk_fact("choice", Atom("first")),
            mk_fact("choice", Atom("second")),
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("test", (x_var,))])

        # Cut should still work despite builtins
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("first")


# =============================================================================
# MERGED FROM test_builtin_arity_errors.py
# =============================================================================


class TestUnificationBuiltinArity:
    """Test arity errors for unification builtins."""

    def test_unify_wrong_arity(self):
        """Test =/2 with wrong number of arguments."""
        engine = Engine(program())

        # = with 1 argument
        query1 = Struct("=", (Int(5),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # = with 3 arguments
        query2 = Struct("=", (Int(5), Int(5), Int(5)))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

        # = with 0 arguments
        query3 = Struct("=", ())
        solutions3 = engine.run([query3])
        assert len(solutions3) == 0

    def test_not_unify_wrong_arity(self):
        """Test \\=/2 with wrong number of arguments."""
        engine = Engine(program())

        # \\= with 1 argument
        query1 = Struct("\\=", (Int(5),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # \\= with 3 arguments
        query2 = Struct("\\=", (Int(5), Int(6), Int(7)))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0


class TestTypeCheckBuiltinArity:
    """Test arity errors for type checking builtins."""

    def test_var_wrong_arity(self):
        """Test var/1 with wrong number of arguments."""
        engine = Engine(program())

        # var with 0 arguments
        query1 = Struct("var", ())
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # var with 2 arguments
        query2 = Struct("var", (Var(0, "X"), Var(1, "Y")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

    def test_nonvar_wrong_arity(self):
        """Test nonvar/1 with wrong number of arguments."""
        engine = Engine(program())

        # nonvar with 0 arguments
        query1 = Struct("nonvar", ())
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # nonvar with 2 arguments
        query2 = Struct("nonvar", (Int(5), Int(6)))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

    def test_atom_wrong_arity(self):
        """Test atom/1 with wrong number of arguments."""
        engine = Engine(program())

        # atom with 0 arguments
        query1 = Struct("atom", ())
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # atom with 2 arguments
        query2 = Struct("atom", (Atom("a"), Atom("b")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0


class TestMetaBuiltinArity:
    """Test arity errors for meta-predicates."""

    def test_call_wrong_arity(self):
        """Test call/1 with wrong number of arguments."""
        engine = Engine(program())

        # call with 0 arguments
        query1 = Struct("call", ())
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # call with 2 arguments
        query2 = Struct("call", (Atom("true"), Atom("extra")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

    def test_catch_wrong_arity(self):
        """Test catch/3 with wrong number of arguments."""
        engine = Engine(program())

        # catch with 1 argument
        query1 = Struct("catch", (Atom("true"),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # catch with 2 arguments
        query2 = Struct("catch", (Atom("true"), Var(0, "X")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

        # catch with 4 arguments
        query3 = Struct(
            "catch", (Atom("true"), Var(0, "X"), Atom("fail"), Atom("extra"))
        )
        solutions3 = engine.run([query3])
        assert len(solutions3) == 0

    def test_throw_wrong_arity(self):
        """Test throw/1 with wrong number of arguments."""
        engine = Engine(program())

        # throw with 0 arguments
        query1 = Struct("catch", (Struct("throw", ()), Var(0, "X"), Atom("true")))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0  # throw fails

        # throw with 2 arguments
        query2 = Struct(
            "catch",
            (Struct("throw", (Atom("e"), Atom("extra"))), Var(1, "Y"), Atom("true")),
        )
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0  # throw fails


class TestStructureBuiltinArity:
    """Test arity errors for structure manipulation builtins."""

    def test_functor_wrong_arity(self):
        """Test functor/3 with wrong number of arguments."""
        engine = Engine(program())

        # functor with 1 argument
        query1 = Struct("functor", (Atom("foo"),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # functor with 2 arguments
        query2 = Struct("functor", (Atom("foo"), Atom("foo")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

        # functor with 4 arguments
        query3 = Struct("functor", (Atom("foo"), Atom("foo"), Int(0), Atom("extra")))
        solutions3 = engine.run([query3])
        assert len(solutions3) == 0

    def test_arg_wrong_arity(self):
        """Test arg/3 with wrong number of arguments."""
        engine = Engine(program())

        # arg with 1 argument
        query1 = Struct("arg", (Int(1),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # arg with 2 arguments
        query2 = Struct("arg", (Int(1), Struct("f", (Atom("a"),))))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

        # arg with 4 arguments
        query3 = Struct(
            "arg", (Int(1), Struct("f", (Atom("a"),)), Var(0, "X"), Atom("extra"))
        )
        solutions3 = engine.run([query3])
        assert len(solutions3) == 0

    def test_univ_wrong_arity(self):
        """Test =../2 with wrong number of arguments."""
        engine = Engine(program())

        # =.. with 1 argument
        query1 = Struct("=..", (Atom("foo"),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # =.. with 3 arguments
        query2 = Struct("=..", (Atom("foo"), List((Atom("foo"),)), Atom("extra")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0


class TestControlBuiltinArity:
    """Test arity errors for control flow builtins."""

    def test_cut_wrong_arity(self):
        """Test !/0 with wrong number of arguments."""
        engine = Engine(program())

        # ! with 1 argument
        query1 = Struct("!", (Atom("arg"),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

        # ! with 2 arguments
        query2 = Struct("!", (Atom("arg1"), Atom("arg2")))
        solutions2 = engine.run([query2])
        assert len(solutions2) == 0

    def test_true_wrong_arity(self):
        """Test true/0 with wrong number of arguments."""
        engine = Engine(program())

        # true with 1 argument
        query1 = Struct("true", (Atom("arg"),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0

    def test_fail_wrong_arity(self):
        """Test fail/0 with wrong number of arguments."""
        engine = Engine(program())

        # fail with 1 argument
        query1 = Struct("fail", (Atom("arg"),))
        solutions1 = engine.run([query1])
        assert len(solutions1) == 0


# =============================================================================
# MERGED FROM test_builtins_type_checking.py
# =============================================================================


class TestNotUnifiable:
    """Tests for \\=/2 (not unifiable) predicate."""

    def test_not_unifiable_different_atoms(self):
        """Test \\= succeeds with different atoms."""
        engine = Engine(program())
        query = Struct("\\=", (Atom("a"), Atom("b")))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_same_atoms_fails(self):
        """Test \\= fails with same atoms."""
        engine = Engine(program())
        query = Struct("\\=", (Atom("a"), Atom("a")))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_different_numbers(self):
        """Test \\= succeeds with different numbers."""
        engine = Engine(program())
        query = Struct("\\=", (Int(1), Int(2)))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_same_numbers_fails(self):
        """Test \\= fails with same numbers."""
        engine = Engine(program())
        query = Struct("\\=", (Int(42), Int(42)))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_different_structures(self):
        """Test \\= succeeds with different structures."""
        engine = Engine(program())
        s1 = Struct("f", (Int(1), Int(2)))
        s2 = Struct("f", (Int(1), Int(3)))
        query = Struct("\\=", (s1, s2))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_different_functors(self):
        """Test \\= succeeds with structures having different functors."""
        engine = Engine(program())
        s1 = Struct("f", (Int(1),))
        s2 = Struct("g", (Int(1),))
        query = Struct("\\=", (s1, s2))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_var_and_ground_fails(self):
        """Test \\= fails with variable and ground term (they can unify)."""
        engine = Engine(program())
        X = Var(0, "X")
        query = Struct("\\=", (X, Int(5)))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_variables_always_fails(self):
        """Test \\= always fails with two unbound variables."""
        engine = Engine(program())
        X = Var(0, "X")
        Y = Var(1, "Y")
        query = Struct("\\=", (X, Y))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_lists(self):
        """Test \\= with lists."""
        engine = Engine(program())
        list1 = List((Int(1), Int(2)))
        list2 = List((Int(1), Int(3)))
        query = Struct("\\=", (list1, list2))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_partial_can_unify_fails(self):
        """Test \\= fails when structures can be unified through variables."""
        engine = Engine(program())
        X = Var(0, "X")
        s1 = Struct("f", (X, Int(2)))
        s2 = Struct("f", (Int(1), Int(2)))
        query = Struct("\\=", (s1, s2))
        solutions = engine.run([query])
        assert len(solutions) == 0  # They can unify if X = 1

    def test_not_unifiable_wrong_arity_fails(self):
        """Test \\= with wrong arity (should fail as builtin check)."""
        engine = Engine(program())
        # Try with 1 argument (wrong arity)
        query = Struct("\\=", (Int(1),))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_not_unifiable_with_bound_var(self):
        """Test \\= with a bound variable."""
        engine = Engine(program())
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = 1, Y = 2, X \\= Y
        query = Struct(
            ",",
            (
                Struct("=", (X, Int(1))),
                Struct(",", (Struct("=", (Y, Int(2))), Struct("\\=", (X, Y)))),
            ),
        )
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)

    def test_not_unifiable_cyclic_structure(self):
        """Test \\= with potentially cyclic structures."""
        engine = Engine(program())
        X = Var(0, "X")
        # X \\= f(X) - should succeed as they can't unify (occurs check)
        query = Struct("\\=", (X, Struct("f", (X,))))
        solutions = engine.run([query])
        # With occurs check on, X cannot unify with f(X)
        if engine.occurs_check:
            assert len(solutions) == 1
        else:
            # Without occurs check, they could unify
            assert len(solutions) == 0

    def test_not_unifiable_empty_list_vs_nonempty(self):
        """Test \\= between empty list and non-empty list."""
        engine = Engine(program())
        query = Struct("\\=", (Atom("[]"), List((Int(1),))))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_not_unifiable_nested_structures(self):
        """Test \\= with deeply nested structures."""
        engine = Engine(program())
        s1 = Struct("f", (Struct("g", (Int(1),)),))
        s2 = Struct("f", (Struct("g", (Int(2),)),))
        query = Struct("\\=", (s1, s2))
        solutions = engine.run([query])
        assert len(solutions) == 1


class TestVarPredicate:
    """Tests for var/1 predicate."""

    def test_var_with_unbound_variable(self):
        """Test var/1 succeeds with unbound variable."""
        engine = Engine(program())
        X = Var(0, "X")
        query = Struct("var", (X,))
        solutions = engine.run([query])
        assert len(solutions) == 1
        # X remains unbound in the solution
        assert isinstance(solutions[0]["X"], Var)

    def test_var_with_bound_variable_fails(self):
        """Test var/1 fails with bound variable."""
        engine = Engine(program())
        X = Var(0, "X")
        # X = 5, var(X)
        query = Struct(",", (Struct("=", (X, Int(5))), Struct("var", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_var_with_atom_fails(self):
        """Test var/1 fails with atom."""
        engine = Engine(program())
        query = Struct("var", (Atom("abc"),))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_var_with_number_fails(self):
        """Test var/1 fails with number."""
        engine = Engine(program())
        query = Struct("var", (Int(42),))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_var_with_structure_fails(self):
        """Test var/1 fails with structure."""
        engine = Engine(program())
        query = Struct("var", (Struct("f", (Int(1), Int(2))),))
        solutions = engine.run([query])
        assert len(solutions) == 0


class TestNonvarPredicate:
    """Tests for nonvar/1 predicate."""

    def test_nonvar_with_atom(self):
        """Test nonvar/1 succeeds with atom."""
        engine = Engine(program())
        query = Struct("nonvar", (Atom("abc"),))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_nonvar_with_number(self):
        """Test nonvar/1 succeeds with number."""
        engine = Engine(program())
        query = Struct("nonvar", (Int(42),))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_nonvar_with_structure(self):
        """Test nonvar/1 succeeds with structure."""
        engine = Engine(program())
        query = Struct("nonvar", (Struct("f", (Int(1), Int(2))),))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_nonvar_with_unbound_variable_fails(self):
        """Test nonvar/1 fails with unbound variable."""
        engine = Engine(program())
        X = Var(0, "X")
        query = Struct("nonvar", (X,))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_nonvar_with_bound_variable(self):
        """Test nonvar/1 succeeds with bound variable."""
        engine = Engine(program())
        X = Var(0, "X")
        # X = 5, nonvar(X)
        query = Struct(",", (Struct("=", (X, Int(5))), Struct("nonvar", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(5)


class TestAtomPredicate:
    """Tests for atom/1 predicate."""

    def test_atom_with_atom(self):
        """Test atom/1 succeeds with atom."""
        engine = Engine(program())
        query = Struct("atom", (Atom("abc"),))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_atom_with_empty_list(self):
        """Test atom/1 succeeds with empty list []."""
        engine = Engine(program())
        query = Struct("atom", (Atom("[]"),))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_atom_with_number_fails(self):
        """Test atom/1 fails with number."""
        engine = Engine(program())
        query = Struct("atom", (Int(42),))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_structure_fails(self):
        """Test atom/1 fails with structure."""
        engine = Engine(program())
        query = Struct("atom", (Struct("f", (Int(1),)),))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_list_fails(self):
        """Test atom/1 fails with non-empty list."""
        engine = Engine(program())
        query = Struct("atom", (List((Int(1), Int(2))),))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_unbound_variable_fails(self):
        """Test atom/1 fails with unbound variable."""
        engine = Engine(program())
        X = Var(0, "X")
        query = Struct("atom", (X,))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_atom_with_var_bound_to_empty_list(self):
        """Test atom/1 with variable bound to []."""
        engine = Engine(program())
        X = Var(0, "X")
        # X = [], atom(X)
        query = Struct(",", (Struct("=", (X, Atom("[]"))), Struct("atom", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("[]")

    def test_atom_with_variable_bound_to_atom(self):
        """Test atom/1 with variable bound to atom."""
        engine = Engine(program())
        X = Var(0, "X")
        # X = abc, atom(X)
        query = Struct(",", (Struct("=", (X, Atom("abc"))), Struct("atom", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("abc")

    def test_atom_with_variable_bound_to_number_fails(self):
        """Test atom/1 fails with variable bound to number."""
        engine = Engine(program())
        X = Var(0, "X")
        # X = 42, atom(X)
        query = Struct(",", (Struct("=", (X, Int(42))), Struct("atom", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 0


class TestMixedTypeChecking:
    """Tests for combinations of type-checking predicates."""

    def test_var_nonvar_exclusive(self):
        """Test that var/1 and nonvar/1 are mutually exclusive."""
        engine = Engine(program())
        X = Var(0, "X")
        # var(X), nonvar(X) - should fail
        query = Struct(",", (Struct("var", (X,)), Struct("nonvar", (X,))))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_bound_var_type_checking(self):
        """Test type checking with bound variables."""
        engine = Engine(program())
        X = Var(0, "X")
        Y = Var(1, "Y")
        # X = abc, Y = 42, atom(X), nonvar(Y)
        query = Struct(
            ",",
            (
                Struct("=", (X, Atom("abc"))),
                Struct(
                    ",",
                    (
                        Struct("=", (Y, Int(42))),
                        Struct(",", (Struct("atom", (X,)), Struct("nonvar", (Y,)))),
                    ),
                ),
            ),
        )
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("abc")
        assert solutions[0]["Y"] == Int(42)

    def test_type_check_in_compound_goal(self):
        """Test type checking predicates in compound goals."""
        engine = Engine(program())
        X = Var(0, "X")
        # (var(X) ; atom(X)), X = abc, atom(X)
        query = Struct(
            ",",
            (
                Struct(";", (Struct("var", (X,)), Struct("atom", (X,)))),
                Struct(",", (Struct("=", (X, Atom("abc"))), Struct("atom", (X,)))),
            ),
        )
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("abc")


class TestDictBuiltins:
    """Tests for dict manipulation builtins: dict_create/3, get_dict/3, put_dict/3."""

    def test_dict_create_basic_key_value_pairs(self):
        """Test dict_create/3 with basic key-value pairs."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a-1, b-2])
        data = List(
            (Struct("-", (Atom("a"), Int(1))), Struct("-", (Atom("b"), Int(2))))
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        assert dict_result.pairs == ((Atom("a"), Int(1)), (Atom("b"), Int(2)))
        assert dict_result.tag is None

    def test_dict_create_key_colon_value_pairs(self):
        """Test dict_create/3 with key:value pairs."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a:1, b:2])
        data = List(
            (Struct(":", (Atom("a"), Int(1))), Struct(":", (Atom("b"), Int(2))))
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        assert dict_result.pairs == ((Atom("a"), Int(1)), (Atom("b"), Int(2)))

    def test_dict_create_key_value_function_pairs(self):
        """Test dict_create/3 with key(value) pairs."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a(1), b(2)])
        data = List((Struct("a", (Int(1),)), Struct("b", (Int(2),))))
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        assert dict_result.pairs == ((Atom("a"), Int(1)), (Atom("b"), Int(2)))

    def test_dict_create_empty_dict(self):
        """Test dict_create/3 with empty data list."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [])
        data = List(())
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        assert dict_result.pairs == ()

    def test_dict_create_sorted_output(self):
        """Test dict_create/3 creates sorted output."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [z-3, a-1, m-2])
        data = List(
            (
                Struct("-", (Atom("z"), Int(3))),
                Struct("-", (Atom("a"), Int(1))),
                Struct("-", (Atom("m"), Int(2))),
            )
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        # Should be sorted: a, m, z
        assert dict_result.pairs == (
            (Atom("a"), Int(1)),
            (Atom("m"), Int(2)),
            (Atom("z"), Int(3)),
        )

    def test_dict_create_duplicate_keys_fails(self):
        """Test dict_create/3 fails with duplicate keys."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a-1, a-2]) - should fail
        data = List(
            (Struct("-", (Atom("a"), Int(1))), Struct("-", (Atom("a"), Int(2))))
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_dict_create_invalid_key_type_fails(self):
        """Test dict_create/3 fails with invalid key types."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [f(x)-1]) - compound key should fail
        data = List((Struct("-", (Struct("f", (Atom("x"),)), Int(1))),))
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_dict_create_invalid_data_format_fails(self):
        """Test dict_create/3 fails with invalid data format."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a]) - not a pair, should fail
        data = List((Atom("a"),))
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_dict_create_non_none_tag_fails(self):
        """Test dict_create/3 fails with non-none tag (not supported yet)."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, person, [name-alice]) - should fail, only none tag supported
        data = List((Struct("-", (Atom("name"), Atom("alice"))),))
        query = Struct("dict_create", (D, Atom("person"), data))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_dict_create_variable_tag_fails(self):
        """Test dict_create/3 fails with variable tag."""
        engine = Engine(program())
        D = Var(0, "D")
        T = Var(1, "T")
        # dict_create(D, T, [a-1]) - should fail, tag must be ground
        data = List((Struct("-", (Atom("a"), Int(1))),))
        query = Struct("dict_create", (D, T, data))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_dict_create_integer_keys(self):
        """Test dict_create/3 with integer keys."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [1-foo, 2-bar])
        data = List(
            (Struct("-", (Int(1), Atom("foo"))), Struct("-", (Int(2), Atom("bar"))))
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        # Atoms sort before integers, so if we had mixed types, atoms would come first
        # But here we only have integers
        assert dict_result.pairs == ((Int(1), Atom("foo")), (Int(2), Atom("bar")))

    def test_dict_create_mixed_formats(self):
        """Test dict_create/3 with mixed data formats."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a-1, b:2, c(3)])
        data = List(
            (
                Struct("-", (Atom("a"), Int(1))),
                Struct(":", (Atom("b"), Int(2))),
                Struct("c", (Int(3),)),
            )
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 1
        dict_result = solutions[0]["D"]
        assert isinstance(dict_result, PrologDict)
        assert dict_result.pairs == (
            (Atom("a"), Int(1)),
            (Atom("b"), Int(2)),
            (Atom("c"), Int(3)),
        )

    def test_dict_create_cross_format_duplicate_keys_fails(self):
        """Test dict_create/3 fails with duplicate keys across different formats."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, [a-1, a:2]) - should fail, duplicate key 'a'
        data = List(
            (Struct("-", (Atom("a"), Int(1))), Struct(":", (Atom("a"), Int(2))))
        )
        query = Struct("dict_create", (D, Atom("none"), data))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_dict_create_non_list_data_fails(self):
        """Test dict_create/3 fails with non-list data argument."""
        engine = Engine(program())
        D = Var(0, "D")
        # dict_create(D, none, pairs) - should fail, third arg must be list
        query = Struct("dict_create", (D, Atom("none"), Atom("pairs")))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_get_dict_basic_lookup(self):
        """Test get_dict/3 basic key lookup."""
        engine = Engine(program())
        V = Var(0, "V")
        test_dict = PrologDict(((Atom("name"), Atom("alice")), (Atom("age"), Int(30))))
        # get_dict(name, {name:alice, age:30}, V)
        query = Struct("get_dict", (Atom("name"), test_dict, V))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["V"] == Atom("alice")

    def test_get_dict_key_not_found_fails(self):
        """Test get_dict/3 fails when key not found."""
        engine = Engine(program())
        V = Var(0, "V")
        test_dict = PrologDict(((Atom("name"), Atom("alice")), (Atom("age"), Int(30))))
        # get_dict(city, {name:alice, age:30}, V) - should fail
        query = Struct("get_dict", (Atom("city"), test_dict, V))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_get_dict_empty_dict_fails(self):
        """Test get_dict/3 fails on empty dict."""
        engine = Engine(program())
        V = Var(0, "V")
        test_dict = PrologDict(())
        # get_dict(key, {}, V) - should fail
        query = Struct("get_dict", (Atom("key"), test_dict, V))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_get_dict_non_dict_fails(self):
        """Test get_dict/3 fails with non-dict argument."""
        engine = Engine(program())
        V = Var(0, "V")
        # get_dict(key, [a, b], V) - should fail with list
        query = Struct("get_dict", (Atom("key"), List((Atom("a"), Atom("b"))), V))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_get_dict_unification_success(self):
        """Test get_dict/3 succeeds when value matches bound variable."""
        engine = Engine(program())
        test_dict = PrologDict(((Atom("age"), Int(30)),))
        # get_dict(age, {age:30}, 30) - should succeed
        query = Struct("get_dict", (Atom("age"), test_dict, Int(30)))
        solutions = engine.run([query])
        assert len(solutions) == 1

    def test_get_dict_unification_failure(self):
        """Test get_dict/3 fails when value doesn't match bound variable."""
        engine = Engine(program())
        test_dict = PrologDict(((Atom("age"), Int(30)),))
        # get_dict(age, {age:30}, 99) - should fail
        query = Struct("get_dict", (Atom("age"), test_dict, Int(99)))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_get_dict_integer_key(self):
        """Test get_dict/3 with integer key."""
        engine = Engine(program())
        V = Var(0, "V")
        test_dict = PrologDict(((Int(1), Atom("foo")), (Int(2), Atom("bar"))))
        # get_dict(1, {1:foo, 2:bar}, V)
        query = Struct("get_dict", (Int(1), test_dict, V))
        solutions = engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["V"] == Atom("foo")

    def test_get_dict_variable_key_fails(self):
        """Test get_dict/3 fails with variable key."""
        engine = Engine(program())
        K = Var(0, "K")
        V = Var(1, "V")
        test_dict = PrologDict(((Atom("name"), Atom("alice")),))
        # get_dict(K, {name:alice}, V) - should fail, key must be ground
        query = Struct("get_dict", (K, test_dict, V))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_get_dict_compound_key_fails(self):
        """Test get_dict/3 fails with compound key."""
        engine = Engine(program())
        V = Var(0, "V")
        test_dict = PrologDict(((Atom("name"), Atom("alice")),))
        # get_dict(f(x), {name:alice}, V) - should fail, compound key not allowed
        query = Struct("get_dict", (Struct("f", (Atom("x"),)), test_dict, V))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_put_dict_add_new_key(self):
        """Test put_dict/3 adding new key to dict."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        # put_dict([c-3], {a:1, b:2}, D)
        new_pairs = List((Struct("-", (Atom("c"), Int(3))),))
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        # Should be sorted: a, b, c
        assert result_dict.pairs == (
            (Atom("a"), Int(1)),
            (Atom("b"), Int(2)),
            (Atom("c"), Int(3)),
        )

    def test_put_dict_update_existing_key(self):
        """Test put_dict/3 updating existing key."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        # put_dict([a-99], {a:1, b:2}, D)
        new_pairs = List((Struct("-", (Atom("a"), Int(99))),))
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        assert result_dict.pairs == ((Atom("a"), Int(99)), (Atom("b"), Int(2)))

    def test_put_dict_multiple_pairs(self):
        """Test put_dict/3 with multiple key-value pairs."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)),))
        # put_dict([b-2, c-3], {a:1}, D)
        new_pairs = List(
            (Struct("-", (Atom("b"), Int(2))), Struct("-", (Atom("c"), Int(3))))
        )
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        assert result_dict.pairs == (
            (Atom("a"), Int(1)),
            (Atom("b"), Int(2)),
            (Atom("c"), Int(3)),
        )

    def test_put_dict_empty_pairs_list(self):
        """Test put_dict/3 with empty pairs list returns original dict."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        # put_dict([], {a:1, b:2}, D)
        new_pairs = List(())
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        assert result_dict.pairs == input_dict.pairs

    def test_put_dict_on_empty_dict(self):
        """Test put_dict/3 on empty dict."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(())
        # put_dict([a-1, b-2], {}, D)
        new_pairs = List(
            (Struct("-", (Atom("a"), Int(1))), Struct("-", (Atom("b"), Int(2))))
        )
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        assert result_dict.pairs == ((Atom("a"), Int(1)), (Atom("b"), Int(2)))

    def test_put_dict_non_dict_fails(self):
        """Test put_dict/3 fails with non-dict argument."""
        engine = Engine(program())
        D = Var(0, "D")
        # put_dict([a-1], [x, y], D) - should fail with list
        new_pairs = List((Struct("-", (Atom("a"), Int(1))),))
        query = Struct("put_dict", (new_pairs, List((Atom("x"), Atom("y"))), D))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_put_dict_invalid_pair_format_fails(self):
        """Test put_dict/3 fails with invalid pair format."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)),))
        # put_dict([notapair], {a:1}, D) - should fail
        new_pairs = List((Atom("notapair"),))
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_put_dict_mixed_formats(self):
        """Test put_dict/3 with mixed key-value formats."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)),))
        # put_dict([b:2, c(3)], {a:1}, D)
        new_pairs = List((Struct(":", (Atom("b"), Int(2))), Struct("c", (Int(3),))))
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        assert result_dict.pairs == (
            (Atom("a"), Int(1)),
            (Atom("b"), Int(2)),
            (Atom("c"), Int(3)),
        )

    def test_put_dict_duplicate_keys_last_wins(self):
        """Test put_dict/3 with duplicate keys in new pairs (last wins)."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)),))
        # put_dict([b-2, b-99], {a:1}, D) - last value should win
        new_pairs = List(
            (Struct("-", (Atom("b"), Int(2))), Struct("-", (Atom("b"), Int(99))))
        )
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        assert isinstance(result_dict, PrologDict)
        assert result_dict.pairs == ((Atom("a"), Int(1)), (Atom("b"), Int(99)))

    def test_put_dict_non_list_pairs_fails(self):
        """Test put_dict/3 fails with non-list pairs argument."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)),))
        # put_dict(a-1, {a:1}, D) - should fail, first arg must be list
        query = Struct("put_dict", (Struct("-", (Atom("a"), Int(1))), input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 0

    def test_put_dict_immutability(self):
        """Test put_dict/3 doesn't mutate input dict."""
        engine = Engine(program())
        D = Var(0, "D")
        input_dict = PrologDict(((Atom("a"), Int(1)), (Atom("b"), Int(2))))
        original_pairs = input_dict.pairs
        # put_dict([c-3], {a:1, b:2}, D)
        new_pairs = List((Struct("-", (Atom("c"), Int(3))),))
        query = Struct("put_dict", (new_pairs, input_dict, D))
        solutions = engine.run([query])
        assert len(solutions) == 1
        result_dict = solutions[0]["D"]
        # Input dict should be unchanged
        assert input_dict.pairs == original_pairs
        # Result should be different object
        assert result_dict is not input_dict
        # Result should have new key
        assert result_dict.pairs == (
            (Atom("a"), Int(1)),
            (Atom("b"), Int(2)),
            (Atom("c"), Int(3)),
        )


class TestDictBuiltinArity:
    """Tests for dict builtin arity errors."""

    def test_dict_create_wrong_arity(self):
        """Test dict_create with wrong arity fails."""
        engine = Engine(program())
        # dict_create/2 should not be recognized as builtin
        query = Struct("dict_create", (Var(0, "D"), Atom("none")))
        solutions = engine.run([query])
        assert len(solutions) == 0  # Should fail to find predicate

    def test_get_dict_wrong_arity(self):
        """Test get_dict with wrong arity fails."""
        engine = Engine(program())
        # get_dict/2 should not be recognized as builtin
        query = Struct("get_dict", (Atom("key"), PrologDict(())))
        solutions = engine.run([query])
        assert len(solutions) == 0  # Should fail to find predicate

    def test_put_dict_wrong_arity(self):
        """Test put_dict with wrong arity fails."""
        engine = Engine(program())
        # put_dict/2 should not be recognized as builtin
        query = Struct("put_dict", (List(()), PrologDict(())))
        solutions = engine.run([query])
        assert len(solutions) == 0  # Should fail to find predicate
