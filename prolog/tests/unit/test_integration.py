"""Integration tests for the Prolog engine.

These tests verify that all components work together correctly
for complex scenarios involving deep recursion, backtracking,
and interactions between features.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import (
    mk_fact, mk_rule, program,
    assert_no_recursion
)


class TestDeepRecursion:
    """Test deep recursion handling without Python stack overflow."""
    
    def test_deep_zero_base_case(self):
        """Test deep(zero) succeeds."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("deep", (Atom("zero"),))])
        assert len(solutions) == 1
    
    def test_deep_recursive_case(self):
        """Test deep(s(N)) :- deep(N)."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        engine = Engine(prog)
        # Test with s(s(zero))
        term = Struct("s", (Struct("s", (Atom("zero"),)),))
        solutions = engine.run([Struct("deep", (term,))])
        assert len(solutions) == 1
    
    def test_deep_1000_levels(self):
        """Test deep(s(...s(zero)...)) with 1000 levels."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        # Build s(s(...s(zero)...)) with 1000 levels
        term = Atom("zero")
        for _ in range(1000):
            term = Struct("s", (term,))
        
        engine = Engine(prog)
        solutions = engine.run([Struct("deep", (term,))])
        assert len(solutions) == 1
    
    @pytest.mark.slow
    def test_deep_5000_levels(self):
        """Test deep(s(...s(zero)...)) with 5000 levels."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        # Build s(s(...s(zero)...)) with 5000 levels
        term = Atom("zero")
        for _ in range(5000):
            term = Struct("s", (term,))
        
        engine = Engine(prog)
        solutions = engine.run([Struct("deep", (term,))])
        assert len(solutions) == 1
    
    def test_assert_no_python_recursion(self):
        """Test that deep recursion uses no Python recursion."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        # Build deep term
        term = Atom("zero")
        for _ in range(100):
            term = Struct("s", (term,))
        
        engine = Engine(prog)
        
        # Check recursion limit during execution
        with assert_no_recursion():
            solutions = engine.run([Struct("deep", (term,))])
            assert len(solutions) == 1
    
    def test_deep_failure_unwinds_correctly(self):
        """Test that deep recursion failure unwinds correctly."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        # Build s(s(...s(one)...)) - will fail since only zero is base case
        term = Atom("one")  # Wrong base case
        for _ in range(100):
            term = Struct("s", (term,))
        
        engine = Engine(prog)
        solutions = engine.run([Struct("deep", (term,))])
        assert len(solutions) == 0  # Should fail and unwind cleanly
    
    def test_deep_with_variable_generates_solutions(self):
        """Test deep(X) generates all solutions."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        engine = Engine(prog, max_solutions=5)
        solutions = engine.run([Struct("deep", (Var(0, "X"),))])
        
        # Should generate zero, s(zero), s(s(zero)), etc.
        assert len(solutions) == 5
        assert solutions[0]["X"] == Atom("zero")
        assert solutions[1]["X"] == Struct("s", (Atom("zero"),))
        assert solutions[2]["X"] == Struct("s", (Struct("s", (Atom("zero"),)),))


class TestComplexBacktracking:
    """Test complex backtracking scenarios."""
    
    def test_multiple_choicepoints(self):
        """Test multiple nested choicepoints."""
        prog = program(
            # path(X, Y) with multiple paths
            mk_fact("edge", Atom("a"), Atom("b")),
            mk_fact("edge", Atom("b"), Atom("c")),
            mk_fact("edge", Atom("a"), Atom("d")),
            mk_fact("edge", Atom("d"), Atom("c")),
            
            # path/2: direct edge or via intermediate
            mk_rule("path", (Var(0, "X"), Var(1, "Y")),
                    Struct("edge", (Var(0, "X"), Var(1, "Y")))),
            mk_rule("path", (Var(0, "X"), Var(2, "Y")),
                    Struct("edge", (Var(0, "X"), Var(1, "Z"))),
                    Struct("path", (Var(1, "Z"), Var(2, "Y"))))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("path", (Atom("a"), Atom("c")))])
        
        # Should find both paths: a->b->c and a->d->c
        assert len(solutions) == 2
    
    def test_solutions_in_correct_order(self):
        """Test that solutions come in left-to-right, depth-first order."""
        prog = program(
            mk_fact("p", Atom("1")),
            mk_fact("p", Atom("2")),
            mk_fact("q", Atom("a")),
            mk_fact("q", Atom("b")),
            
            mk_rule("test", (Var(0, "X"), Var(1, "Y")),
                    Struct("p", (Var(0, "X"),)),
                    Struct("q", (Var(1, "Y"),)))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])
        
        # Should get solutions in order: (1,a), (1,b), (2,a), (2,b)
        assert len(solutions) == 4
        assert solutions[0] == {"X": Atom("1"), "Y": Atom("a")}
        assert solutions[1] == {"X": Atom("1"), "Y": Atom("b")}
        assert solutions[2] == {"X": Atom("2"), "Y": Atom("a")}
        assert solutions[3] == {"X": Atom("2"), "Y": Atom("b")}
    
    def test_cut_in_first_clause(self):
        """Test cut in first clause prevents alternatives."""
        prog = program(
            mk_rule("test", (Var(0, "X"),),
                    Struct("p", (Var(0, "X"),)),
                    Atom("!")),  # Cut here
            mk_fact("test", Atom("alternative")),
            
            mk_fact("p", Atom("first")),
            mk_fact("p", Atom("second"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        # Cut prevents both alternative clause and backtracking in p
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("first")
    
    def test_cut_in_middle_clause(self):
        """Test cut in middle clause."""
        prog = program(
            mk_fact("test", Atom("first")),
            mk_rule("test", (Atom("second"),), Atom("!")),
            mk_fact("test", Atom("third"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        # Should get first and second, but not third
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("first")
        assert solutions[1]["X"] == Atom("second")
    
    def test_cut_after_failure(self):
        """Test cut after a failing goal."""
        prog = program(
            mk_rule("test", (Atom("a"),),
                    Atom("fail"),
                    Atom("!"),  # Never reached
                    Atom("true")),
            mk_fact("test", Atom("b"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        # First clause fails before cut, so we get second clause
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("b")
    
    def test_nested_cuts(self):
        """Test nested cuts with proper scoping."""
        prog = program(
            # outer(X) calls inner(X) which has a cut
            mk_rule("inner", (Atom("a"),), Atom("!")),
            mk_fact("inner", Atom("b")),
            
            mk_rule("outer", (Var(0, "X"),),
                    Struct("inner", (Var(0, "X"),))),
            mk_fact("outer", Atom("c"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("outer", (Var(0, "X"),))])
        
        # inner cut only affects inner, so outer still tries second clause
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("c")
    
    def test_complex_backtracking_with_shared_variables(self):
        """Test backtracking with shared variables across goals."""
        prog = program(
            mk_fact("p", Atom("1"), Atom("a")),
            mk_fact("p", Atom("2"), Atom("b")),
            mk_fact("q", Atom("a"), Atom("x")),
            mk_fact("q", Atom("b"), Atom("y")),
            
            mk_rule("test", (Var(0, "X"), Var(2, "Z")),
                    Struct("p", (Var(0, "X"), Var(1, "Y"))),
                    Struct("q", (Var(1, "Y"), Var(2, "Z"))))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Z")))])
        
        # Y must unify in both p and q
        assert len(solutions) == 2
        assert solutions[0] == {"X": Atom("1"), "Z": Atom("x")}
        assert solutions[1] == {"X": Atom("2"), "Z": Atom("y")}
    
    def test_backtracking_through_recursive_calls(self):
        """Test backtracking through recursive predicate calls."""
        prog = program(
            # List member predicate with backtracking
            mk_rule("member", (Var(0, "X"), List((Var(0, "X"),), Var(1, "_")))),
            mk_rule("member", (Var(0, "X"), List((Var(1, "_"),), Var(2, "T"))),
                    Struct("member", (Var(0, "X"), Var(2, "T"))))
        )
        
        engine = Engine(prog)
        lst = List((Atom("a"), Atom("b"), Atom("c")))
        solutions = engine.run([Struct("member", (Var(0, "X"), lst))])
        
        # Should find all three elements
        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")
        assert solutions[2]["X"] == Atom("c")


class TestCutBoundaries:
    """Test cut boundary behavior with complex choicepoint structures."""
    
    def test_cut_with_multiple_depth_choicepoints(self):
        """Test cut only prunes choicepoints younger than clause barrier."""
        prog = program(
            # Outer predicate with choices
            mk_fact("outer", Atom("o1")),
            mk_fact("outer", Atom("o2")),
            
            # Middle predicate that calls inner with cut
            mk_rule("middle", (Var(0, "X"), Var(1, "Y")),
                    Struct("outer", (Var(0, "X"),)),
                    Struct("inner", (Var(1, "Y"),))),
            
            # Inner predicate with cut - red cut behavior
            mk_rule("inner", (Atom("i1"),), Atom("!")),
            mk_fact("inner", Atom("i2")),
            
            # Test predicate creates multiple choicepoint levels
            mk_rule("test", (Var(0, "X"), Var(1, "Y")),
                    Struct("middle", (Var(0, "X"), Var(1, "Y"))))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])
        
        # Cut in inner should prevent i2, but outer choices remain
        # Expected: (o1, i1), (o2, i1)
        assert len(solutions) == 2
        assert solutions[0] == {"X": Atom("o1"), "Y": Atom("i1")}
        assert solutions[1] == {"X": Atom("o2"), "Y": Atom("i1")}
    
    def test_sibling_choicepoints_with_cut(self):
        """Test cut with sibling choicepoints at same level."""
        prog = program(
            # Two independent choice predicates
            mk_fact("choice1", Atom("a")),
            mk_fact("choice1", Atom("b")),
            
            mk_fact("choice2", Atom("x")),
            mk_fact("choice2", Atom("y")),
            
            # Test with cut between them
            mk_rule("test", (Var(0, "A"), Var(1, "B")),
                    Struct("choice1", (Var(0, "A"),)),
                    Atom("!"),  # Cut here
                    Struct("choice2", (Var(1, "B"),)))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "A"), Var(1, "B")))])
        
        # Cut should commit to first choice1 but allow choice2 to backtrack
        # Expected: (a, x), (a, y)
        assert len(solutions) == 2
        assert solutions[0] == {"A": Atom("a"), "B": Atom("x")}
        assert solutions[1] == {"A": Atom("a"), "B": Atom("y")}


class TestResourceManagement:
    """Test resource management and leak prevention."""
    
    def test_max_solutions_no_resource_leak(self):
        """Test early termination with max_solutions doesn't leak resources."""
        prog = program(
            mk_fact("infinite", Atom("a")),
            mk_rule("infinite", (Var(0, "X"),),
                    Struct("infinite", (Var(0, "X"),)))
        )
        
        engine = Engine(prog, max_solutions=5)
        
        # Capture state before
        initial_cells = len(engine.store.cells)
        
        solutions = engine.run([Struct("infinite", (Var(0, "X"),))])
        
        # Should stop at 5 solutions
        assert len(solutions) == 5
        
        # Trail should be empty after query completes
        assert len(engine.trail) == 0
        
        # Store should only have grown by query variables
        # (some growth is expected for query var allocation)
        final_cells = len(engine.store.cells)
        assert final_cells - initial_cells < 10  # Reasonable bound
    
    def test_deep_recursion_unwind_cleans_state(self):
        """Test deep recursion failure unwinds cleanly."""
        prog = program(
            mk_fact("deep", Atom("zero")),
            mk_rule("deep", (Struct("s", (Var(0, "N"),)),),
                    Struct("deep", (Var(0, "N"),)))
        )
        
        # Build deep term that will fail
        term = Atom("one")  # Wrong base case
        for _ in range(100):
            term = Struct("s", (term,))
        
        engine = Engine(prog)
        
        # Run query that will fail after deep recursion
        solutions = engine.run([Struct("deep", (term,))])
        
        # Should fail
        assert len(solutions) == 0
        
        # All state should be cleaned up
        assert engine.trail.position() == 0
        assert engine.goal_stack.height() == 0
        assert len(engine.cp_stack) == 0
    
    def test_call_backtracking_purity(self):
        """Test call/1 doesn't leak state during backtracking."""
        prog = program(
            mk_fact("p", Atom("1")),
            mk_fact("p", Atom("2")),
            
            mk_rule("test", (Var(0, "X"),),
                    Struct("call", (Struct("p", (Var(0, "X"),)),)))
        )
        
        engine = Engine(prog)
        
        # Capture trail size during execution
        trail_sizes = []
        
        # Hook to capture trail size (if we had hooks)
        # For now, just verify final state
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        # Should get both solutions
        assert len(solutions) == 2
        
        # Trail should be clean after completion
        assert len(engine.trail) == 0


class TestMixedClauseOrdering:
    """Test solution ordering with mixed facts and rules."""
    
    def test_fact_rule_fact_ordering(self):
        """Test predicate with fact, rule, fact maintains order."""
        prog = program(
            # Fact
            mk_fact("mixed", Atom("fact1")),
            
            # Rule that produces multiple solutions
            mk_rule("mixed", (Var(0, "X"),),
                    Struct("helper", (Var(0, "X"),))),
            
            # Another fact
            mk_fact("mixed", Atom("fact2")),
            
            # Helper for the rule
            mk_fact("helper", Atom("rule1")),
            mk_fact("helper", Atom("rule2"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("mixed", (Var(0, "X"),))])
        
        # Should get solutions in clause order:
        # fact1, then rule's solutions (rule1, rule2), then fact2
        assert len(solutions) == 4
        assert solutions[0]["X"] == Atom("fact1")
        assert solutions[1]["X"] == Atom("rule1")
        assert solutions[2]["X"] == Atom("rule2")
        assert solutions[3]["X"] == Atom("fact2")
    
    def test_interleaved_facts_and_rules(self):
        """Test complex interleaving of facts and rules."""
        prog = program(
            mk_fact("complex", Atom("a"), Atom("1")),
            
            mk_rule("complex", (Atom("b"), Var(0, "Y")),
                    Struct("gen", (Var(0, "Y"),))),
            
            mk_fact("complex", Atom("c"), Atom("2")),
            
            mk_rule("complex", (Atom("d"), Var(0, "Y")),
                    Struct("gen2", (Var(0, "Y"),))),
            
            # Generators
            mk_fact("gen", Atom("g1")),
            mk_fact("gen", Atom("g2")),
            mk_fact("gen2", Atom("h1"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("complex", (Var(0, "X"), Var(1, "Y")))])
        
        # Verify order follows clause definition order
        assert len(solutions) == 5
        assert (solutions[0]["X"], solutions[0]["Y"]) == (Atom("a"), Atom("1"))
        assert (solutions[1]["X"], solutions[1]["Y"]) == (Atom("b"), Atom("g1"))
        assert (solutions[2]["X"], solutions[2]["Y"]) == (Atom("b"), Atom("g2"))
        assert (solutions[3]["X"], solutions[3]["Y"]) == (Atom("c"), Atom("2"))
        assert (solutions[4]["X"], solutions[4]["Y"]) == (Atom("d"), Atom("h1"))


class TestBuiltinInteraction:
    """Test interaction between builtins and other features."""
    
    def test_call_with_backtracking(self):
        """Test call/1 with backtracking goals."""
        prog = program(
            mk_fact("p", Atom("1")),
            mk_fact("p", Atom("2")),
            
            mk_rule("test", (Var(0, "X"),),
                    Struct("call", (Struct("p", (Var(0, "X"),)),)))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("1")
        assert solutions[1]["X"] == Atom("2")
    
    def test_call_with_cut(self):
        """Test call(!) executes cut in proper context."""
        prog = program(
            mk_rule("test", (Atom("a"),),
                    Struct("call", (Atom("!"),))),
            mk_fact("test", Atom("b"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        # call(!) should execute cut, preventing second clause
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")
    
    def test_true_fail_in_conjunction(self):
        """Test true and fail in conjunctions."""
        prog = program(
            mk_fact("p", Atom("1")),
            mk_fact("p", Atom("2")),
            
            # test1(X) :- p(X), true, p(X).
            mk_rule("test1", (Var(0, "X"),),
                    Struct("p", (Var(0, "X"),)),
                    Atom("true"),
                    Struct("p", (Var(0, "X"),))),
            
            # test2(X) :- p(X), fail, p(X).
            mk_rule("test2", (Var(0, "X"),),
                    Struct("p", (Var(0, "X"),)),
                    Atom("fail"),
                    Struct("p", (Var(0, "X"),)))
        )
        
        engine = Engine(prog)
        
        # true should not affect solutions
        solutions = engine.run([Struct("test1", (Var(0, "X"),))])
        assert len(solutions) == 2
        
        # fail should cause backtracking
        solutions = engine.run([Struct("test2", (Var(0, "X"),))])
        assert len(solutions) == 0
    
    def test_builtin_precedence_complex(self):
        """Test builtin precedence in complex scenarios."""
        prog = program(
            # User defines their own 'call' predicate
            mk_fact("call", Atom("user_call")),
            
            # test uses builtin call/1
            mk_rule("test", (Var(0, "X"),),
                    Struct("call", (Struct("p", (Var(0, "X"),)),))),
            
            mk_fact("p", Atom("result"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])
        
        # Builtin call/1 should be used, not user's call/1
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("result")
    
    def test_call_undefined_predicate_fails(self):
        """Test call/1 with undefined predicate fails cleanly."""
        prog = program(
            mk_rule("test", (),
                    Struct("call", (Struct("nonexistent", ()),)))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", ())])
        
        # Should fail with no solutions
        assert len(solutions) == 0
        
        # Verify clean failure - no trail/store side effects
        assert len(engine.trail) == 0
    
    def test_call_with_non_callable_fails(self):
        """Test call/1 with non-callable terms fails."""
        from prolog.ast.terms import Int
        
        prog = program(
            # Try to call an integer
            mk_rule("test_int", (),
                    Struct("call", (Int(123),))),
            
            # Try to call a list
            mk_rule("test_list", (),
                    Struct("call", (List((Atom("a"), Atom("b"))),)))
        )
        
        engine = Engine(prog)
        
        # Integer should fail
        solutions = engine.run([Struct("test_int", ())])
        assert len(solutions) == 0
        
        # List should fail
        solutions = engine.run([Struct("test_list", ())])
        assert len(solutions) == 0
    
    def test_call_deep_dereference_chain(self):
        """Test call/1 follows deep variable chains."""
        prog = program(
            # X = Y, Y = Z, Z = p(result)
            mk_rule("test", (Var(0, "Result"),),
                    Struct("=", (Var(1, "X"), Var(2, "Y"))),
                    Struct("=", (Var(2, "Y"), Var(3, "Z"))),
                    Struct("=", (Var(3, "Z"), Struct("p", (Var(0, "Result"),)))),
                    Struct("call", (Var(1, "X"),))),
            
            # Simple unification
            mk_fact("=", Var(0, "A"), Var(0, "A")),
            
            mk_fact("p", Atom("success"))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "R"),))])
        
        # call should dereference X -> Y -> Z -> p(result)
        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("success")
    
    def test_builtin_fail_precedence(self):
        """Test fail/0 builtin takes precedence over user fact."""
        prog = program(
            # User defines fail as a fact (would succeed)
            mk_fact("fail"),
            
            # Test that fail actually fails (builtin behavior)
            mk_rule("test", (),
                    Atom("fail"),
                    Struct("never_reached", ()))
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", ())])
        
        # Builtin fail/0 should fail, not succeed like user's fact
        assert len(solutions) == 0
    
    def test_builtin_true_precedence(self):
        """Test true/0 builtin takes precedence over user rule."""
        prog = program(
            # User defines true to fail (opposite of builtin)
            mk_rule("true", (), Atom("fail")),
            
            # Test that true actually succeeds (builtin behavior)
            mk_rule("test", (),
                    Atom("true"),
                    Struct("reached", ())),
            
            mk_fact("reached")
        )
        
        engine = Engine(prog)
        solutions = engine.run([Struct("test", ())])
        
        # Builtin true/0 should succeed, not fail like user's rule
        assert len(solutions) == 1