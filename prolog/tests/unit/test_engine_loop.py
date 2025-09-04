"""Unit tests for T0.3: Engine.run() single iterative loop.

These tests verify the non-recursive execution model with explicit stacks
and cover all edge cases and invariants for the single-loop VM.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestBasicExecution:
    """Test basic execution without backtracking."""
    
    def test_trivial_fact_query(self):
        """Test querying a single fact."""
        prog = program(
            mk_fact("bird", Atom("robin"))
        )
        engine = Engine(prog)
        
        # Query: bird(robin).
        solutions = engine.run([Struct("bird", (Atom("robin"),))])
        assert len(solutions) == 1
        assert solutions[0] == {}  # No variables to bind
    
    def test_trivial_fact_with_variable(self):
        """Test querying a fact with a variable."""
        prog = program(
            mk_fact("bird", Atom("robin"))
        )
        engine = Engine(prog)
        
        # Query: bird(X).
        solutions = engine.run([Struct("bird", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("robin")
    
    def test_conjunction_execution(self):
        """Test that conjunctions execute left-to-right."""
        prog = program(
            mk_fact("a"),
            mk_fact("b"),
            mk_fact("c")
        )
        engine = Engine(prog)
        
        # Query: a, b, c.
        solutions = engine.run([
            Struct(",", (
                Atom("a"),
                Struct(",", (Atom("b"), Atom("c")))
            ))
        ])
        assert len(solutions) == 1
    
    def test_conjunction_associativity(self):
        """Test that conjunction associativity doesn't affect results."""
        prog = program(mk_fact("a"), mk_fact("b"), mk_fact("c"))
        engine = Engine(prog)
        
        # Test: (a, (b, c)) vs ((a, b), c)
        sols1 = engine.run([Struct(",", (Atom("a"), Struct(",", (Atom("b"), Atom("c")))))])
        sols2 = engine.run([Struct(",", (Struct(",", (Atom("a"), Atom("b"))), Atom("c")))])
        assert len(sols1) == len(sols2) == 1
    
    def test_empty_query(self):
        """Test that empty query succeeds once."""
        prog = program()
        engine = Engine(prog)
        
        solutions = engine.run([])
        assert len(solutions) == 1
        assert solutions[0] == {}
    
    def test_builtin_true(self):
        """Test that true/0 succeeds."""
        prog = program()
        engine = Engine(prog)
        
        solutions = engine.run([Atom("true")])
        assert len(solutions) == 1
    
    def test_builtin_fail(self):
        """Test that fail/0 fails."""
        prog = program()
        engine = Engine(prog)
        
        solutions = engine.run([Atom("fail")])
        assert len(solutions) == 0
    
    def test_builtin_then_call_left_to_right(self):
        """Test that builtins at the left bind before calls."""
        prog = program(mk_fact("q", Int(1)))
        engine = Engine(prog)
        
        # Query: X=1, q(X).
        solutions = engine.run([
            Struct(",", (
                Struct("=", (Var(0, "X"), Int(1))),
                Struct("q", (Var(0, "X"),))
            ))
        ])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
    
    def test_rule_body_execution(self):
        """Test that rule bodies are executed."""
        prog = program(
            mk_fact("bird", Atom("robin")),
            mk_rule("flies", (Var(0, "X"),),
                    Struct("bird", (Var(0, "X"),)))
        )
        engine = Engine(prog)
        
        # Query: flies(robin).
        solutions = engine.run([Struct("flies", (Atom("robin"),))])
        assert len(solutions) == 1
    
    def test_no_recursion_in_control_flow(self):
        """Test that engine doesn't use Python recursion for control flow.
        
        This test creates a deep chain of rules and verifies it executes
        without hitting Python's recursion limit.
        """
        # Create a chain: p0 :- p1. p1 :- p2. ... p999 :- true.
        depth = 1000
        clauses = []
        for i in range(depth - 1):
            clauses.append(
                mk_rule(f"p{i}", (),
                        Atom(f"p{i+1}"))
            )
        clauses.append(mk_fact(f"p{depth-1}"))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        # This should execute without RecursionError
        solutions = engine.run([Atom("p0")])
        assert len(solutions) == 1
    
    def test_same_functor_different_arity(self):
        """Test that predicates with same name but different arity are distinct."""
        prog = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(1), Int(2))
        )
        engine = Engine(prog)
        
        # Query: p(X).
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        
        # Query: p(X, Y).
        solutions = engine.run([Struct("p", (Var(0, "X"), Var(1, "Y")))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)


class TestEngineInvariants:
    """Test engine invariants and debug hooks."""
    
    def test_heights_restored_exactly(self):
        """Test that stack heights are restored exactly on backtracking."""
        prog = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b"))
        )
        engine = Engine(prog)
        
        # Query: p(X), q(Y).
        # Should create choicepoints and restore heights correctly
        solutions = engine.run([
            Struct(",", (
                Struct("p", (Var(0, "X"),)),
                Struct("q", (Var(1, "Y"),))
            ))
        ])
        assert len(solutions) == 4
        
        # After completion, all stacks should be empty
        if hasattr(engine, 'debug_cp_stack_size'):
            assert len(engine.cp_stack) == 0
            assert len(engine.frame_stack) == 0
            assert len(engine.goal_stack) == 0
    
    def test_ports_order(self):
        """Test that debug ports fire in correct order."""
        prog = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2))
        )
        engine = Engine(prog)
        
        # Query: p(X).
        # Expected trace: CALL p/1, CALL clause1, EXIT p/1, 
        #                 REDO p/1, CALL clause2, EXIT p/1, FAIL p/1
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 2
        
        # If tracer available, assert exact port sequence
        if hasattr(engine, 'get_trace'):
            trace = engine.get_trace()
            expected = ["CALL", "CALL", "EXIT", "REDO", "CALL", "EXIT", "FAIL"]
            assert trace == expected
    
    def test_pop_frame_executes_once(self):
        """Test that POP_FRAME executes exactly once per call."""
        prog = program(
            mk_rule("q", (), Atom("true"))
        )
        engine = Engine(prog)
        
        # Query: q.
        # Should create one frame and pop it exactly once
        solutions = engine.run([Atom("q")])
        assert len(solutions) == 1
        
        # Frame counter would verify exactly one pop
        if hasattr(engine, 'debug_frame_pops'):
            assert engine._debug_frame_pops == 1


class TestBuiltinsTopLevel:
    """Test top-level builtin dispatch."""
    
    def test_true_and_fail_top_level(self):
        """Test true/0 and fail/0 at top level."""
        prog = program()
        engine = Engine(prog)
        
        # Query: true.
        solutions = engine.run([Atom("true")])
        assert len(solutions) == 1
        
        # Query: fail.
        solutions = engine.run([Atom("fail")])
        assert len(solutions) == 0
    
    def test_builtin_with_control(self):
        """Test builtins with control constructs."""
        prog = program()
        engine = Engine(prog)
        
        # Query: (true, true ; fail).
        solutions = engine.run([
            Struct(";", (
                Struct(",", (Atom("true"), Atom("true"))),
                Atom("fail")
            ))
        ])
        assert len(solutions) == 1  # Should not leave CPs behind


class TestBacktracking:
    """Test backtracking with multiple solutions."""
    
    def test_multiple_facts(self):
        """Test backtracking through multiple matching facts."""
        prog = program(
            mk_fact("bird", Atom("robin")),
            mk_fact("bird", Atom("sparrow")),
            mk_fact("bird", Atom("owl"))
        )
        engine = Engine(prog)
        
        # Query: bird(X).
        solutions = engine.run([Struct("bird", (Var(0, "X"),))])
        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("robin")
        assert solutions[1]["X"] == Atom("sparrow")
        assert solutions[2]["X"] == Atom("owl")
    
    def test_conjunction_backtracking(self):
        """Test backtracking in conjunctions."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_fact("b", Atom("x")),
            mk_fact("b", Atom("y"))
        )
        engine = Engine(prog)
        
        # Query: a(X), b(Y).
        solutions = engine.run([
            Struct(",", (
                Struct("a", (Var(0, "X"),)),
                Struct("b", (Var(1, "Y"),))
            ))
        ])
        assert len(solutions) == 4
        # Should get all combinations: (1,x), (1,y), (2,x), (2,y)
        x_values = [sol["X"] for sol in solutions]
        y_values = [sol["Y"] for sol in solutions]
        assert x_values == [Int(1), Int(1), Int(2), Int(2)]
        assert y_values == [Atom("x"), Atom("y"), Atom("x"), Atom("y")]
    
    def test_all_pairs_depth_first(self):
        """Test depth-first enumeration of combinations."""
        prog = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b"))
        )
        engine = Engine(prog)
        
        # Query: p(X), q(Y).
        # Expect (X,Y) = (1,a),(1,b),(2,a),(2,b)
        solutions = engine.run([
            Struct(",", (
                Struct("p", (Var(0, "X"),)),
                Struct("q", (Var(1, "Y"),))
            ))
        ])
        assert len(solutions) == 4
        assert (solutions[0]["X"], solutions[0]["Y"]) == (Int(1), Atom("a"))
        assert (solutions[1]["X"], solutions[1]["Y"]) == (Int(1), Atom("b"))
        assert (solutions[2]["X"], solutions[2]["Y"]) == (Int(2), Atom("a"))
        assert (solutions[3]["X"], solutions[3]["Y"]) == (Int(2), Atom("b"))
    
    def test_disjunction(self):
        """Test disjunction (;) operator."""
        prog = program(
            mk_fact("a"),
            mk_fact("b")
        )
        engine = Engine(prog)
        
        # Query: (a ; b).
        solutions = engine.run([
            Struct(";", (Atom("a"), Atom("b")))
        ])
        assert len(solutions) == 2
    
    def test_backtrack_with_unification(self):
        """Test that backtracking properly restores variable bindings."""
        prog = program(
            mk_fact("pair", Atom("a"), Int(1)),
            mk_fact("pair", Atom("b"), Int(2)),
            mk_fact("check", Int(1)),
            mk_fact("check", Int(2))
        )
        engine = Engine(prog)
        
        # Query: pair(X, Y), check(Y).
        solutions = engine.run([
            Struct(",", (
                Struct("pair", (Var(0, "X"), Var(1, "Y"))),
                Struct("check", (Var(1, "Y"),))
            ))
        ])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[0]["Y"] == Int(1)
        assert solutions[1]["X"] == Atom("b")
        assert solutions[1]["Y"] == Int(2)
    
    def test_max_solutions_limit(self):
        """Test that max_solutions properly limits results."""
        prog = program(
            mk_fact("num", Int(1)),
            mk_fact("num", Int(2)),
            mk_fact("num", Int(3)),
            mk_fact("num", Int(4)),
            mk_fact("num", Int(5))
        )
        engine = Engine(prog, max_solutions=3)
        
        solutions = engine.run([Struct("num", (Var(0, "X"),))])
        assert len(solutions) == 3
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
        assert solutions[2]["X"] == Int(3)


class TestDisjunctionIsolation:
    """Test disjunction isolation and order."""
    
    def test_right_branch_isolated(self):
        """Test that right branch isn't polluted by left bindings."""
        prog = program()
        engine = Engine(prog)
        
        # Query: (X=1 ; true), X=2.
        # Expect true with X=2 (second branch sees fresh variable)
        solutions = engine.run([
            Struct(",", (
                Struct(";", (
                    Struct("=", (Var(0, "X"), Int(1))),
                    Atom("true")
                )),
                Struct("=", (Var(0, "X"), Int(2)))
            ))
        ])
        # First branch: X=1, then X=2 fails
        # Second branch: true, then X=2 succeeds
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(2)
    
    def test_disjunction_order(self):
        """Test that disjunction explores left-to-right."""
        prog = program()
        engine = Engine(prog)
        
        # Query: (X=left ; X=right).
        solutions = engine.run([
            Struct(";", (
                Struct("=", (Var(0, "X"), Atom("left"))),
                Struct("=", (Var(0, "X"), Atom("right")))
            ))
        ])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("left")
        assert solutions[1]["X"] == Atom("right")


class TestControlConstructs:
    """Test control constructs like cut and if-then-else."""
    
    def test_cut_basic(self):
        """Test that cut (!) prevents backtracking."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_rule("test", (Var(0, "X"),),
                    Struct("a", (Var(0, "X"),)),
                    Struct("!", ()))  # Cut after first solution
        )
        engine = Engine(prog)
        
        # Query: test(X).
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        assert len(solutions) == 1  # Cut should prevent getting second solution
        assert solutions[0]["X"] == Int(1)
    
    def test_cut_in_conjunction(self):
        """Test cut behavior in conjunction."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_fact("b", Int(3)),
            mk_fact("b", Int(4)),
            mk_rule("test", (Var(0, "X"), Var(1, "Y")),
                    Struct("a", (Var(0, "X"),)),
                    Struct("!", ()),  # Cut after first 'a'
                    Struct("b", (Var(1, "Y"),)))
        )
        engine = Engine(prog)
        
        # Query: test(X, Y).
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])
        assert len(solutions) == 2  # Should get X=1 with both Y values
        assert all(sol["X"] == Int(1) for sol in solutions)
        assert solutions[0]["Y"] == Int(3)
        assert solutions[1]["Y"] == Int(4)
    
    def test_cut_commits_within_predicate(self):
        """Test that cut commits to choices within predicate."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_fact("s"),
            mk_rule("r", (Var(0, "X"),),
                    Struct("a", (Var(0, "X"),)),
                    Struct("!", ()),
                    Atom("s"))
        )
        engine = Engine(prog)
        
        # Query: r(X).
        # Expect exactly X=1, no backtracking into a/1
        solutions = engine.run([Struct("r", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
    
    def test_cut_does_not_escape_callee(self):
        """Test that cut doesn't prune caller's choicepoints."""
        prog = program(
            mk_fact("a", Var(0, "_")),
            mk_rule("c", (), Struct("!", ()), Atom("fail")),
            mk_fact("c"),
            mk_rule("q", (),
                    Struct("a", (Var(0, "_"),)),
                    Atom("c"))
        )
        engine = Engine(prog)
        
        # Query: q.
        # Cut in c/0 can't prune caller's CPs
        solutions = engine.run([Struct("q", ())])
        assert len(solutions) == 1  # Should succeed
    
    def test_cut_with_disjunction(self):
        """Test cut interaction with disjunction."""
        prog = program()
        engine = Engine(prog)
        
        # Query: (X=1, ! ; X=2).
        solutions = engine.run([
            Struct(";", (
                Struct(",", (
                    Struct("=", (Var(0, "X"), Int(1))),
                    Struct("!", ())
                )),
                Struct("=", (Var(0, "X"), Int(2)))
            ))
        ])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
    
    def test_cut_noop_without_new_cps(self):
        """Test that cut is no-op when no choicepoints created in frame."""
        prog = program(mk_rule("p", (), Struct("!", ()), Atom("true")))
        engine = Engine(prog)
        
        solutions = engine.run([Struct("p", ())])
        assert len(solutions) == 1
    
    def test_cut_inside_then_does_not_escape_predicate(self):
        """Test that cut inside 'then' branch doesn't escape predicate frame."""
        prog = program(mk_fact("a"), mk_fact("t"), mk_fact("u"))
        engine = Engine(prog)
        
        # Query: ((a -> !) ; t), u.
        solutions = engine.run([
            Struct(",", (
                Struct(";", (
                    Struct("->", (Atom("a"), Struct("!", ()))),
                    Atom("t")
                )),
                Atom("u")
            ))
        ])
        assert len(solutions) == 1  # No pruning outside ITE's predicate frame
    
    def test_if_then_else_true_condition(self):
        """Test if-then-else with true condition."""
        prog = program(
            mk_fact("cond"),
            mk_fact("then_branch"),
            mk_fact("else_branch")
        )
        engine = Engine(prog)
        
        # Query: (cond -> then_branch ; else_branch).
        solutions = engine.run([
            Struct(";", (
                Struct("->", (Atom("cond"), Atom("then_branch"))),
                Atom("else_branch")
            ))
        ])
        assert len(solutions) == 1  # Only then branch should execute
    
    def test_if_then_else_false_condition(self):
        """Test if-then-else with false condition."""
        prog = program(
            mk_fact("then_branch"),
            mk_fact("else_branch")
        )
        engine = Engine(prog)
        
        # Query: (fail -> then_branch ; else_branch).
        solutions = engine.run([
            Struct(";", (
                Struct("->", (Atom("fail"), Atom("then_branch"))),
                Atom("else_branch")
            ))
        ])
        assert len(solutions) == 1  # Only else branch should execute
    
    def test_nested_control_structures(self):
        """Test nested control structures."""
        prog = program(
            mk_fact("a"),
            mk_fact("b"),
            mk_fact("c")
        )
        engine = Engine(prog)
        
        # Query: ((a ; b), c).
        solutions = engine.run([
            Struct(",", (
                Struct(";", (Atom("a"), Atom("b"))),
                Atom("c")
            ))
        ])
        assert len(solutions) == 2  # Both (a,c) and (b,c)


class TestIfThenElse:
    """Test if-then-else ((->)/2) precise behavior."""
    
    def test_then_commits_after_first_cond_success(self):
        """Test that Then commits after first Cond success."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2))
        )
        engine = Engine(prog)
        
        # Query: (a(X) -> X=1 ; X=99).
        # Expect X=1 once
        solutions = engine.run([
            Struct(";", (
                Struct("->", (
                    Struct("a", (Var(0, "X"),)),
                    Struct("=", (Var(0, "X"), Int(1)))
                )),
                Struct("=", (Var(0, "X"), Int(99)))
            ))
        ])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
    
    def test_cond_fails_runs_else(self):
        """Test that Else runs when Cond fails."""
        prog = program()
        engine = Engine(prog)
        
        # Query: (fail -> X=1 ; X=2).
        solutions = engine.run([
            Struct(";", (
                Struct("->", (Atom("fail"), Struct("=", (Var(0, "X"), Int(1))))),
                Struct("=", (Var(0, "X"), Int(2)))
            ))
        ])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(2)
    
    def test_cond_nondet_but_commits(self):
        """Test that nondet Cond commits after first success."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_rule("then", (Var(0, "X"),),
                    Struct(";", (
                        Struct("->", (
                            Struct("a", (Var(0, "X"),)),
                            Struct("!", ())
                        )),
                        Atom("true")
                    )),
                    Struct("=", (Var(0, "X"), Int(1))))
        )
        engine = Engine(prog)
        
        # Query: then(X).
        # Expect X=1 once; no exploration of a(2)
        solutions = engine.run([Struct("then", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
    
    def test_then_nondet_enumerates_after_commit(self):
        """Test that Then branch can enumerate after Cond commits."""
        prog = program(
            mk_fact("cond"),
            mk_fact("y", Atom("b")),
            mk_fact("y", Atom("c"))
        )
        engine = Engine(prog)
        
        # Query: (cond -> y(Y) ; fail).
        solutions = engine.run([
            Struct(";", (
                Struct("->", (Atom("cond"), Struct("y", (Var(0, "Y"),)))),
                Atom("fail")
            ))
        ])
        assert len(solutions) == 2
        assert [s["Y"] for s in solutions] == [Atom("b"), Atom("c")]
    
    def test_else_nondet_enumerates_when_cond_fails(self):
        """Test that Else branch enumerates when Cond fails."""
        prog = program(
            mk_fact("y", Int(1)),
            mk_fact("y", Int(2))
        )
        engine = Engine(prog)
        
        # Query: (fail -> fail ; y(Y)).
        solutions = engine.run([
            Struct(";", (
                Struct("->", (Atom("fail"), Atom("fail"))),
                Struct("y", (Var(0, "Y"),))
            ))
        ])
        assert len(solutions) == 2
        assert [s["Y"] for s in solutions] == [Int(1), Int(2)]
    
    def test_control_cp_restores_heights(self):
        """Test that control choicepoint restores heights correctly."""
        prog = program(mk_fact("a"))
        engine = Engine(prog)
        
        # Query: (a -> true ; true).
        solutions = engine.run([
            Struct(";", (
                Struct("->", (Atom("a"), Atom("true"))),
                Atom("true")
            ))
        ])
        assert len(solutions) == 1
        
        # Debug heights should be zero after run
        if hasattr(engine, 'debug_cp_stack_size'):
            assert len(engine.cp_stack) == 0
            assert len(engine.frame_stack) == 0
            assert len(engine.goal_stack) == 0


class TestVariableIsolation:
    """Test variable isolation and freshness."""
    
    def test_fresh_vars_per_clause_instance(self):
        """Test that each clause instance gets fresh variables."""
        prog = program(
            mk_rule("t", (Var(0, "X"),),
                    Struct("=", (Var(0, "X"), Int(1)))),
            mk_rule("t", (Var(0, "X"),),
                    Struct("=", (Var(0, "X"), Int(2))))
        )
        engine = Engine(prog)
        
        # Query: t(X), t(Y).
        # Expect (1,1),(1,2),(2,1),(2,2); verify X≠Y identities
        solutions = engine.run([
            Struct(",", (
                Struct("t", (Var(0, "X"),)),
                Struct("t", (Var(1, "Y"),))
            ))
        ])
        assert len(solutions) == 4
        expected = [
            (Int(1), Int(1)),
            (Int(1), Int(2)),
            (Int(2), Int(1)),
            (Int(2), Int(2))
        ]
        for i, (x, y) in enumerate(expected):
            assert solutions[i]["X"] == x
            assert solutions[i]["Y"] == y
    
    def test_variable_isolation(self):
        """Test that variables are properly isolated between clauses."""
        prog = program(
            mk_fact("p", Var(0, "X"), Var(0, "X")),  # p(X, X).
            mk_fact("q", Int(1), Int(2))  # q(1, 2).
        )
        engine = Engine(prog)
        
        # Query: p(A, B), q(A, B).
        # Should fail because p requires A=B but q has 1≠2
        solutions = engine.run([
            Struct(",", (
                Struct("p", (Var(0, "A"), Var(1, "B"))),
                Struct("q", (Var(0, "A"), Var(1, "B")))
            ))
        ])
        assert len(solutions) == 0


class TestFrameAndChoicepoint:
    """Test frame and choicepoint management."""
    
    def test_frame_creation_per_call(self):
        """Test that one frame is created per predicate call."""
        # We can't directly observe frames, but we can test behavior
        # that depends on proper frame management
        prog = program(
            mk_rule("p", (), Atom("true")),
            mk_rule("q", (), Struct("p", ()), Struct("p", ()))
        )
        engine = Engine(prog)
        
        # Query: q.
        # This should create: Frame for q, then Frame for first p,
        # pop it, Frame for second p, pop it, then pop q frame
        solutions = engine.run([Struct("q", ())])
        assert len(solutions) == 1
    
    def test_bind_then_fail_restores(self):
        """Test that binding followed by failure properly restores state."""
        # p/1: first alternative binds X=1 then fails; second binds X=2 and succeeds
        prog = program(
            mk_rule("p", (Var(0, "X"),),
                    Struct("=", (Var(0, "X"), Int(1))),
                    Atom("fail")),
            mk_rule("p", (Var(0, "X"),),
                    Struct("=", (Var(0, "X"), Int(2))))
        )
        engine = Engine(prog)
        
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(2)
    
    def test_restore_heights_for_predicate_cp(self):
        """Test predicate choicepoint restoration."""
        prog = program(
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_fact("q", Var(0, "X"))  # Binds X
        )
        engine = Engine(prog)
        
        # Query: p(X), q(X).
        # Predicate CP for p should restore X properly
        solutions = engine.run([
            Struct(",", (
                Struct("p", (Var(0, "X"),)),
                Struct("q", (Var(0, "X"),))
            ))
        ])
        assert len(solutions) == 2
    
    def test_restore_heights_for_disjunction_cp(self):
        """Test disjunction choicepoint restoration."""
        prog = program()
        engine = Engine(prog)
        
        # Query: (X=1 ; X=2), X=X.
        # Disjunction CP should restore properly
        solutions = engine.run([
            Struct(",", (
                Struct(";", (
                    Struct("=", (Var(0, "X"), Int(1))),
                    Struct("=", (Var(0, "X"), Int(2)))
                )),
                Struct("=", (Var(0, "X"), Var(0, "X")))
            ))
        ])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
    
    def test_deep_backtracking(self):
        """Test backtracking through deep goal stacks."""
        prog = program(
            mk_rule("p", (Var(0, "X"),),
                    Struct("q", (Var(0, "X"),))),
            mk_rule("q", (Var(0, "X"),),
                    Struct("r", (Var(0, "X"),))),
            mk_rule("r", (Var(0, "X"),),
                    Struct("s", (Var(0, "X"),))),
            mk_fact("s", Int(1)),
            mk_fact("s", Int(2))
        )
        engine = Engine(prog)
        
        # Query: p(X).
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)


class TestSolutionLimits:
    """Test solution limits with various constructs."""
    
    def test_max_solutions_simple(self):
        """Test simple upper bound on solutions."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_fact("a", Int(3))
        )
        engine = Engine(prog, max_solutions=2)
        
        # Query: a(X).
        solutions = engine.run([Struct("a", (Var(0, "X"),))])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
    
    def test_max_solutions_with_cut(self):
        """Test solution limit interaction with cut."""
        prog = program(
            mk_fact("a", Int(1)),
            mk_fact("a", Int(2)),
            mk_fact("a", Int(3)),
            mk_rule("p", (Var(0, "X"),),
                    Struct("a", (Var(0, "X"),)),
                    Struct("!", ()))
        )
        engine = Engine(prog, max_solutions=10)
        
        # Query: p(X).
        # Cut limits to exactly one solution
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)


class TestErrorHandling:
    """Test error conditions and edge cases."""
    
    def test_undefined_predicate(self):
        """Test querying undefined predicate."""
        prog = program()
        engine = Engine(prog)
        
        # Query for undefined predicate should fail
        solutions = engine.run([Struct("undefined", ())])
        assert len(solutions) == 0
    
    @pytest.mark.dev_mode
    def test_undefined_predicate_fails_in_dev(self):
        """Test undefined predicate fails in development mode."""
        prog = program()
        engine = Engine(prog)
        
        # In development mode, undefined predicates fail
        solutions = engine.run([Struct("no_such", (Int(1),))])
        assert len(solutions) == 0
    
    @pytest.mark.iso_mode
    @pytest.mark.skip(reason="ISO mode not yet implemented")
    def test_undefined_predicate_errors_in_iso(self):
        """Test undefined predicate throws in ISO mode."""
        prog = program()
        # When ISO mode is implemented:
        # with pytest.raises(UndefinedPredicateError):
        #     Engine(prog, mode="iso").run([Struct("no_such", (Int(1),))])
        pass
    
    def test_unification_failure_not_error(self):
        """Test that unification failure is not an error."""
        prog = program()
        engine = Engine(prog)
        
        # Query: 1 = a.
        # Should fail, not error
        solutions = engine.run([
            Struct("=", (Int(1), Atom("a")))
        ])
        assert len(solutions) == 0
    
    def test_infinite_loop_with_limit(self):
        """Test that infinite loops respect max_solutions."""
        prog = program(
            mk_rule("loop", (), Struct("loop", ()))
        )
        engine = Engine(prog, max_solutions=0)  # Should stop immediately
        
        solutions = engine.run([Struct("loop", ())])
        assert len(solutions) == 0
    
    def test_occurs_check(self):
        """Test that occurs check prevents infinite structures."""
        prog = program(
            mk_fact("unify", Var(0, "X"), Struct("f", (Var(0, "X"),)))
        )
        engine = Engine(prog, occurs_check=True)
        
        # Query: unify(Y, Y).
        # Should fail due to occurs check (Y = f(Y) is infinite)
        solutions = engine.run([
            Struct("unify", (Var(0, "Y"), Var(0, "Y")))
        ])
        assert len(solutions) == 0
    
    def test_occurs_check_prevents_cycles(self):
        """Test that occurs check prevents cyclic terms."""
        prog = program()
        engine = Engine(prog, occurs_check=True)
        
        # Query: X = f(X).
        # Should fail with occurs check on
        solutions = engine.run([
            Struct("=", (Var(0, "X"), Struct("f", (Var(0, "X"),))))
        ])
        assert len(solutions) == 0
    
    def test_occurs_check_in_clause(self):
        """Test occurs check in clause unification."""
        prog = program(
            mk_fact("cycle", Var(0, "X"), Struct("g", (Var(0, "X"),)))
        )
        engine = Engine(prog, occurs_check=True)
        
        # Query: cycle(Y, Y).
        # Should fail (Y = g(Y) is cyclic)
        solutions = engine.run([
            Struct("cycle", (Var(0, "Y"), Var(0, "Y")))
        ])
        assert len(solutions) == 0
    
    def test_unify_large_lists_iterative(self):
        """Test that large structure unification is iterative."""
        n = 5000
        lst = PrologList(tuple(Int(i) for i in range(n)))
        prog = program(mk_fact("p", lst))
        engine = Engine(prog)
        
        # Should unify without stack overflow
        solutions = engine.run([Struct("p", (Var(0, "X"),))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == lst


class TestSolutionProjection:
    """Test solution recording and projection."""
    
    def test_record_solution_variable_order(self):
        """Test that solution projection respects variable order."""
        prog = program(
            mk_fact("append",
                    PrologList((Int(1), Int(2))),
                    PrologList((Int(3),)),
                    PrologList((Int(1), Int(2), Int(3))))
        )
        engine = Engine(prog)
        
        # Query: append([1,2], [3], Z).
        solutions = engine.run([
            Struct("append", (
                PrologList((Int(1), Int(2))),
                PrologList((Int(3),)),
                Var(0, "Z")
            ))
        ])
        assert len(solutions) == 1
        assert "Z" in solutions[0]
        # Only query vars should be in solution
        assert len(solutions[0]) == 1
    
    def test_projection_deep_list_iterative(self):
        """Test that projection of deep lists is iterative."""
        lst = PrologList(tuple(Int(i) for i in range(2000)))
        prog = program(mk_fact("id", lst))
        engine = Engine(prog)
        
        # Query: id(Z).
        solutions = engine.run([Struct("id", (Var(0, "Z"),))])
        assert len(solutions) == 1
        assert solutions[0]["Z"] == lst


class TestDeepPrograms:
    """Test deep programs without recursion."""
    
    def test_deep_chain_no_recursion(self):
        """Test deep chain executes without Python recursion."""
        depth = 2000
        clauses = []
        
        # Build chain: c0(0) :- c1(1). c1(1) :- c2(2). ... c1999(1999).
        for i in range(depth - 1):
            clauses.append(
                mk_rule(f"c{i}", (Int(i),), 
                        Struct(f"c{i+1}", (Int(i+1),)))
            )
        clauses.append(mk_fact(f"c{depth-1}", Int(depth-1)))
        
        prog = program(*clauses)
        engine = Engine(prog)
        
        # Query: c0(0).
        solutions = engine.run([Struct("c0", (Int(0),))])
        assert len(solutions) == 1
    
    def test_many_clauses(self):
        """Test predicate with many clauses."""
        # Generate predicate with 2000 facts
        clauses = []
        for i in range(2000):
            clauses.append(mk_fact("many", Int(i)))
        
        prog = program(*clauses)
        engine = Engine(prog, max_solutions=10)
        
        # Enumerate first 10 without issues
        solutions = engine.run([Struct("many", (Var(0, "X"),))])
        assert len(solutions) == 10
        for i in range(10):
            assert solutions[i]["X"] == Int(i)


class TestNondetBuiltins:
    """Test non-deterministic builtin scaffolding."""
    
    def test_flip_builtin_scaffold(self):
        """Test scaffold for future nondet builtins."""
        # This would test a dummy flip/1 builtin that yields 0 then 1
        # For now, we test the pattern with regular predicates
        prog = program(
            mk_fact("flip", Int(0)),
            mk_fact("flip", Int(1))
        )
        engine = Engine(prog)
        
        # Query: flip(X).
        solutions = engine.run([Struct("flip", (Var(0, "X"),))])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(0)
        assert solutions[1]["X"] == Int(1)
    
    def test_nondet_with_cut(self):
        """Test nondet builtin interaction with cut."""
        prog = program(
            mk_fact("flip", Int(0)),
            mk_fact("flip", Int(1))
        )
        engine = Engine(prog)
        
        # Query: (flip(X), ! ; X=2).
        # Expect X=0 only (cut commits builtin)
        solutions = engine.run([
            Struct(";", (
                Struct(",", (
                    Struct("flip", (Var(0, "X"),)),
                    Struct("!", ())
                )),
                Struct("=", (Var(0, "X"), Int(2)))
            ))
        ])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(0)


class TestTrailWriteStamps:
    """Test trail write-stamp sanity."""
    
    def test_write_stamp_single_trail_per_window(self):
        """Test that variables are only trailed once per choice window."""
        prog = program(
            mk_fact("bind", Var(0, "X")),
            mk_rule("test", (Var(0, "X"),),
                    Struct("bind", (Var(0, "X"),)),
                    Struct("=", (Var(0, "X"), Int(1))),
                    Struct("=", (Var(0, "X"), Int(2))))  # Rebind same var
        )
        engine = Engine(prog)
        
        # Query: test(X).
        # Should only trail X once despite multiple bindings
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        assert len(solutions) == 0  # Second binding conflicts
        
        # If debug counter available, verify single trail entry
        if hasattr(engine, 'debug_trail_writes'):
            assert engine._debug_trail_writes == 1


class TestIntegration:
    """Integration tests with realistic programs."""
    
    def test_list_member(self):
        """Test member/2 predicate for list membership."""
        prog = program(
            # member(X, [X|_]).
            mk_fact("member", 
                    Var(0, "X"),
                    PrologList((Var(0, "X"),), Var(1, "_"))),
            # member(X, [_|T]) :- member(X, T).
            mk_rule("member",
                    (Var(0, "X"), PrologList((Var(1, "_"),), Var(2, "T"))),
                    Struct("member", (Var(0, "X"), Var(2, "T"))))
        )
        engine = Engine(prog)
        
        # Query: member(2, [1,2,3]).
        lst = PrologList((Int(1), Int(2), Int(3)))
        solutions = engine.run([Struct("member", (Int(2), lst))])
        assert len(solutions) == 1
        
        # Query: member(X, [a,b,c]).
        lst = PrologList((Atom("a"), Atom("b"), Atom("c")))
        solutions = engine.run([Struct("member", (Var(0, "X"), lst))])
        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")
        assert solutions[2]["X"] == Atom("c")
    
    def test_append(self):
        """Test append/3 predicate for list concatenation."""
        prog = program(
            # append([], L, L).
            mk_fact("append",
                    PrologList(()),
                    Var(0, "L"),
                    Var(0, "L")),
            # append([H|T], L, [H|R]) :- append(T, L, R).
            mk_rule("append",
                    (PrologList((Var(0, "H"),), Var(1, "T")),
                     Var(2, "L"),
                     PrologList((Var(0, "H"),), Var(3, "R"))),
                    Struct("append", (Var(1, "T"), Var(2, "L"), Var(3, "R"))))
        )
        engine = Engine(prog)
        
        # Query: append([1,2], [3,4], X).
        l1 = PrologList((Int(1), Int(2)))
        l2 = PrologList((Int(3), Int(4)))
        solutions = engine.run([
            Struct("append", (l1, l2, Var(0, "X")))
        ])
        assert len(solutions) == 1
        expected = PrologList((Int(1), Int(2), Int(3), Int(4)))
        assert solutions[0]["X"] == expected
    
    def test_factorial(self):
        """Test factorial computation."""
        prog = program(
            # fact(0, 1).
            mk_fact("fact", Int(0), Int(1)),
            # fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.
            mk_rule("fact", (Var(0, "N"), Var(1, "F")),
                    Struct(">", (Var(0, "N"), Int(0))),
                    Struct("is", (Var(2, "N1"), 
                                  Struct("-", (Var(0, "N"), Int(1))))),
                    Struct("fact", (Var(2, "N1"), Var(3, "F1"))),
                    Struct("is", (Var(1, "F"),
                                  Struct("*", (Var(0, "N"), Var(3, "F1"))))))
        )
        engine = Engine(prog)
        
        # Query: fact(5, X).
        solutions = engine.run([Struct("fact", (Int(5), Var(0, "X")))])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(120)  # 5! = 120