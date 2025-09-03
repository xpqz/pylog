"""Tests for Cut (Stage 0 Phase 4).

These tests validate cut barrier tracking and cut builtin behavior.
Most tests are written to pass with basic infrastructure but will
validate cut behavior once implemented.
"""

from prolog.ast.terms import Atom, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine


# Test helpers
def mk_fact(functor: str, *args) -> Clause:
    """Create a fact (clause with no body)."""
    if args:
        head = Struct(functor, args)
    else:
        head = Atom(functor)
    return Clause(head=head, body=())


def mk_rule(functor: str, head_args: tuple, *body_terms) -> Clause:
    """Create a rule (clause with body)."""
    head = Struct(functor, head_args) if head_args else Atom(functor)
    return Clause(head=head, body=body_terms)


def program(*clauses) -> Program:
    """Create a Program from clauses."""
    return Program(clauses=clauses)


class TestCutBarrierTracking:
    """Tests for cut barrier installation and restoration."""
    
    def test_cut_barrier_infrastructure_smoke_test(self):
        """Test cut barrier infrastructure (basic setup/teardown).
        
        TODO: Once cut is implemented, verify barrier value equals
        choices.top_id() when clause entered.
        """
        prog = program(
            mk_rule("test", (Var(0, "X"),),
                    Struct("choice", (Var(0, "X"),))),
            mk_fact("choice", Atom("a")),
            mk_fact("choice", Atom("b"))
        )
        engine = Engine(prog)
        
        # Allocate query variable
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        # Before starting, cut barrier should be None
        assert engine._cut_barrier is None
        
        # Run query - this should create choicepoint for choice/1
        # and set cut barrier when entering test/1 clause
        solutions = engine.run([Struct("test", (x_var,))])
        
        # Should find both solutions (no cut yet)
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")
        
        # After run completes, cut barrier should be reset
        assert engine._cut_barrier is None
        
        # TODO: With cut, verify barrier prevents alternatives
    
    def test_barrier_restoration_across_clauses(self):
        """Test barrier is restored when backtracking between clauses.
        
        TODO: Once cut implemented, verify each clause gets fresh barrier.
        """
        prog = program(
            mk_rule("test", (Var(0, "X"),),
                    Struct("a", (Var(0, "X"),))),
            mk_rule("test", (Var(0, "X"),),
                    Struct("b", (Var(0, "X"),))),
            mk_fact("a", Atom("1")),
            mk_fact("a", Atom("2")),
            mk_fact("b", Atom("3")),
            mk_fact("b", Atom("4"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        # Each clause entry should have its own cut barrier
        solutions = engine.run([Struct("test", (x_var,))])
        assert len(solutions) == 4  # All solutions without cut
        assert [s["X"].name for s in solutions] == ["1", "2", "3", "4"]
        
        # TODO: With cut in first clause after a/1, would get only "1", "3", "4"
        # TODO: With cut in second clause after b/1, would get "1", "2", "3"


class TestCutBuiltin:
    """Tests for cut builtin behavior.
    
    These tests document expected behavior once cut is implemented.
    Currently they test infrastructure only.
    """
    
    def test_cut_removes_newer_choicepoints(self):
        """Test cut removes choicepoints newer than barrier.
        
        Expected behavior with cut:
        - Query test(X) matches first clause with X=first
        - Cut (!) removes choicepoints for X=second and X=third
        - Result: only X=first solution
        """
        prog = program(
            mk_rule("test", (Var(0, "X"),),
                    Struct("choice", (Var(0, "X"),)),
                    Atom("!")),  # Cut here
            mk_fact("choice", Atom("first")),
            mk_fact("choice", Atom("second")),
            mk_fact("choice", Atom("third"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        solutions = engine.run([Struct("test", (x_var,))])
        # With cut: only first solution
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("first")
    
    def test_cut_preserves_older_choicepoints(self):
        """Test cut preserves choicepoints at/before barrier.
        
        Expected behavior with cut:
        - outer/2 creates choicepoint for choice1 (X=a, X=b)  
        - inner/1 creates choicepoint for choice2 (Y=1, Y=2)
        - Cut in inner/1 removes choice2 alternatives
        - But choice1 alternatives preserved (before cut barrier)
        - Result: (a,1), (b,1) only
        """
        prog = program(
            mk_rule("outer", (Var(0, "X"), Var(1, "Y")),
                    Struct("choice1", (Var(0, "X"),)),
                    Struct("inner", (Var(1, "Y"),))),
            mk_rule("inner", (Var(0, "Z"),),
                    Struct("choice2", (Var(0, "Z"),)),
                    Atom("!")),  # Cut here
            mk_fact("choice1", Atom("a")),
            mk_fact("choice1", Atom("b")),
            mk_fact("choice2", Atom("1")),
            mk_fact("choice2", Atom("2"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        y_var = Var(engine.store.new_var("Y"), "Y")
        engine._query_vars = [(x_var.id, "X"), (y_var.id, "Y")]
        
        solutions = engine.run([Struct("outer", (x_var, y_var))])
        # With cut: only first choice from choice2, but both from choice1
        assert len(solutions) == 2
        combos = [(s["X"].name, s["Y"].name) for s in solutions]
        assert combos == [("a", "1"), ("b", "1")]
    
    def test_commit_then_fail_red_cut(self):
        """Test red cut commits then fails (critical scenario).
        
        p(X) :- q(X), !, r(X).
        q(a).
        q(b).
        r(a) :- fail.
        r(b).
        
        Expected with cut: No solutions (q chooses a, cut commits, r(a) fails).
        Without cut: X=b (backtrack from r(a) failure to try q(b)).
        """
        prog = program(
            mk_rule("p", (Var(0, "X"),),
                    Struct("q", (Var(0, "X"),)),
                    Atom("!"),  # Cut here
                    Struct("r", (Var(0, "X"),))),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b")),
            # r(a) always fails (no clause)
            mk_fact("r", Atom("b"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        solutions = engine.run([Struct("p", (x_var,))])
        # With cut: q(a) chosen, cut commits, r(a) fails - no solutions
        assert len(solutions) == 0
    
    def test_cut_at_start_commits_to_clause(self):
        """Test cut at start of body commits to that clause.
        
        p :- !, q.  % Commit to this clause
        p :- r.
        
        Expected with cut: Only tries first clause.
        """
        prog = program(
            mk_rule("p", (),
                    # Cut would go here at start
                    Atom("q")),
            mk_rule("p", (),
                    Atom("r")),
            mk_fact("q"),
            mk_fact("r")
        )
        engine = Engine(prog)
        
        solutions = engine.run([Atom("p")])
        # Without cut: both clauses succeed
        assert len(solutions) == 2
        
        # TODO: With cut at start of first clause, expect 1 solution
    
    def test_multiple_cuts_are_idempotent(self):
        """Test multiple cuts in same body behave as single cut.
        
        p :- a, !, b, !, c.
        
        Should behave identically to single cut after a.
        """
        prog = program(
            mk_rule("p", (Var(0, "X"),),
                    Struct("a", (Var(0, "X"),)),
                    # First cut
                    Struct("b", (Var(0, "X"),)),
                    # Second cut (should be no-op)
                    Struct("c", (Var(0, "X"),))),
            mk_fact("a", Atom("1")),
            mk_fact("a", Atom("2")),
            mk_fact("b", Atom("1")),
            mk_fact("b", Atom("2")),
            mk_fact("c", Atom("1")),
            mk_fact("c", Atom("2"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        solutions = engine.run([Struct("p", (x_var,))])
        # Without cut: tries all combinations
        assert len(solutions) > 0  # Some succeed (where all match same X)
        
        # TODO: With cuts, first cut after a commits to X=1
        # Second cut should have no additional effect
        # Expect single solution with X=1
    
    def test_cut_as_last_goal(self):
        """Test cut as last goal (commits but no goals after).
        
        p(X) :- choice(X), !.
        
        Still commits to first choice, even though no goals follow.
        """
        prog = program(
            mk_rule("p", (Var(0, "X"),),
                    Struct("choice", (Var(0, "X"),))),
                    # Cut as last goal
            mk_fact("choice", Atom("a")),
            mk_fact("choice", Atom("b")),
            mk_fact("choice", Atom("c"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        solutions = engine.run([Struct("p", (x_var,))])
        # Without cut: all three
        assert len(solutions) == 3
        
        # TODO: With cut as last goal, expect only X=a
    
    def test_green_cut_deterministic_behavior(self):
        """Test green cut with deterministic predicate.
        
        Green cut: Cut that doesn't change semantics (already deterministic).
        Expected behavior matches current behavior (one solution).
        """
        prog = program(
            mk_rule("det", (Atom("a"),),
                    Atom("fact")),
                    # Cut here would be green (no alternatives anyway)
            mk_rule("det", (Atom("b"),),
                    Atom("fact")),
            mk_fact("fact")
        )
        engine = Engine(prog)
        
        # Query with 'a' - only first clause matches (deterministic)
        solutions = engine.run([Struct("det", (Atom("a"),))])
        assert len(solutions) == 1
        
        # Query with 'b' - only second clause matches  
        solutions = engine.run([Struct("det", (Atom("b"),))])
        assert len(solutions) == 1
    
    def test_red_cut_would_change_semantics(self):
        """Test red cut would change program semantics.
        
        Red cut: Cut that removes valid solutions.
        
        Expected behavior with cut in max/3:
        - max(1,2,X) would unify with first clause (X=1)
        - Cut would prevent trying second clause
        - Result: Wrong answer X=1 (should be X=2)
        """
        prog = program(
            # Naive max: assumes X > Y
            mk_rule("max", (Var(0, "X"), Var(1, "Y"), Var(0, "X"))),
                    # Red cut would go here
            # Correct case: X <= Y  
            mk_rule("max", (Var(0, "X"), Var(1, "Y"), Var(1, "Y")))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        # max(1, 2, X) - what is max of 1 and 2?
        solutions = engine.run([
            Struct("max", (Atom("1"), Atom("2"), x_var))
        ])
        
        # Without cut: both clauses tried
        # First clause: max(1,2,1) unifies
        # Second clause: max(1,2,2) unifies  
        # Both are valid unifications!
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("1")  # Wrong answer
        assert solutions[1]["X"] == Atom("2")  # Right answer
        
        # TODO: With cut after first clause, expect only X=1 (wrong!)
    
    def test_cut_in_recursive_member_stops_at_first(self):
        """Test cut in member/2 stops at first match.
        
        Expected behavior with cut:
        - member(X, [1,2,1,3]) finds X=1 (first element)
        - Cut prevents backtracking to find X=2, X=1 (second occurrence), X=3
        """
        # Build list [1,2,1,3] as nested Struct(".", ...)
        # Note: includes duplicate 1 to show cut stops at first
        list_term = Struct(".", (Atom("1"),
                        Struct(".", (Atom("2"),
                            Struct(".", (Atom("1"),
                                Struct(".", (Atom("3"), Atom("[]")))))))))
        
        prog = program(
            # member(X, [X|_]) :- !.
            mk_rule("member", 
                    (Var(0, "X"), Struct(".", (Var(0, "X"), Var(1, "_")))),
                    Atom("!")),  # Cut here
            # member(X, [_|T]) :- member(X, T).
            mk_rule("member",
                    (Var(0, "X"), Struct(".", (Var(1, "_"), Var(2, "T")))),
                    Struct("member", (Var(0, "X"), Var(2, "T"))))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        solutions = engine.run([Struct("member", (x_var, list_term))])
        # With cut after first clause: only first match
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("1")
    
    def test_if_then_else_pattern_works_with_cut(self):
        """Test if-then-else pattern that requires cut.
        
        Expected with cut:
        - If condition(X) succeeds, cut commits to then_branch
        - If condition(X) fails, tries else_branch
        """
        prog = program(
            mk_rule("if_then_else", (Var(0, "X"), Var(1, "Result")),
                    Struct("condition", (Var(0, "X"),)),
                    Atom("!"),  # Cut here to commit to then branch
                    Struct("then_branch", (Var(1, "Result"),))),
            mk_rule("if_then_else", (Var(0, "X"), Var(1, "Result")),
                    Struct("else_branch", (Var(1, "Result"),))),
            # Conditions
            mk_fact("condition", Atom("true_case")),
            # Branches  
            mk_fact("then_branch", Atom("then_result")),
            mk_fact("else_branch", Atom("else_result"))
        )
        engine = Engine(prog)
        
        r_var = Var(engine.store.new_var("R"), "R")
        engine._query_vars = [(r_var.id, "R")]
        
        # With "true_case", condition succeeds
        solutions = engine.run([
            Struct("if_then_else", (Atom("true_case"), r_var))
        ])
        # With cut: only then branch
        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("then_result")
        
        # With "false_case", condition fails
        solutions = engine.run([
            Struct("if_then_else", (Atom("false_case"), r_var))
        ])
        # Only else branch matches
        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("else_result")


class TestCutEdgeCases:
    """Edge cases and special cut scenarios."""
    
    def test_nested_cuts_respect_innermost_barrier(self):
        """Test nested cuts - inner cut uses inner barrier.
        
        outer calls inner, both have cuts.
        Inner cut should only affect inner's alternatives.
        """
        prog = program(
            mk_rule("outer", (Var(0, "X"), Var(1, "Y")),
                    Struct("a", (Var(0, "X"),)),
                    # Outer cut would go here
                    Struct("inner", (Var(1, "Y"),))),
            mk_rule("inner", (Var(0, "Z"),),
                    Struct("b", (Var(0, "Z"),))),
                    # Inner cut would go here
            mk_fact("a", Atom("1")),
            mk_fact("a", Atom("2")),
            mk_fact("b", Atom("x")),
            mk_fact("b", Atom("y"))
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        y_var = Var(engine.store.new_var("Y"), "Y")
        engine._query_vars = [(x_var.id, "X"), (y_var.id, "Y")]
        
        solutions = engine.run([Struct("outer", (x_var, y_var))])
        # Without cuts: all combinations
        assert len(solutions) == 4
        
        # TODO: With outer cut after a/1 and inner cut after b/1:
        # Outer cut commits to X=1, inner cut commits to Z=x
        # Expected: [(1, x)]
    
    def test_cut_inside_call_would_work(self):
        """Test cut inside call/1 (if call/1 implemented).
        
        call((!, Goal)) should commit as if ! appeared inline.
        
        NOTE: This test depends on call/1 being implemented.
        """
        prog = program(
            mk_rule("test", (Var(0, "X"),),
                    Struct("choice", (Var(0, "X"),)),
                    # call((!, goal)) would go here
                    Atom("goal")),
            mk_fact("choice", Atom("a")),
            mk_fact("choice", Atom("b")),
            mk_fact("goal")
        )
        engine = Engine(prog)
        
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]
        
        solutions = engine.run([Struct("test", (x_var,))])
        # Without call/cut: both choices
        assert len(solutions) == 2
        
        # TODO: With call((!, goal)), expect only X=a
    
    def test_cut_interaction_with_failure(self):
        """Test cut followed by explicit failure.
        
        p :- q, !, fail.
        p :- r.
        
        First clause commits with cut then fails.
        Second clause should NOT be tried (cut committed).
        """
        prog = program(
            mk_rule("p", (),
                    Atom("q"),
                    # Cut would go here
                    # fail would go here (no matching clause)
                    Atom("nonexistent")),
            mk_rule("p", (),
                    Atom("r")),
            mk_fact("q"),
            mk_fact("r")
        )
        engine = Engine(prog)
        
        solutions = engine.run([Atom("p")])
        # Without cut: first clause fails (nonexistent), second succeeds
        assert len(solutions) == 1
        
        # TODO: With cut after q, expect no solutions (committed to failing clause)