"""Tests for Core Builtins (Stage 0 Phase 5).

Tests for builtin infrastructure and basic builtins like true/0, fail/0, and call/1.
"""

from prolog.ast.terms import Atom, Var, Struct, Int
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
        """Test _is_builtin recognizes call/1 (structural)."""
        engine = Engine(program())

        # call/1 should be recognized as builtin
        assert engine._is_builtin(Struct("call", (Atom("test"),)))

        # call/0 should NOT be recognized
        assert not engine._is_builtin(Atom("call"))

        # call/2 should NOT be recognized
        assert not engine._is_builtin(Struct("call", (Atom("a"), Atom("b"))))

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
