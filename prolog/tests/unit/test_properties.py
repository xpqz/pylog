"""Property-based tests for the Prolog engine.

These tests verify high-level properties that should hold
for any valid Prolog implementation.
"""

import pytest
import random
from prolog.ast.terms import Atom, Var, Struct
from prolog.engine.engine import Engine
from prolog.tests.helpers import mk_fact, mk_rule, program


class TestSolutionCompleteness:
    """Test that all valid solutions are found."""

    def test_all_valid_solutions_found(self):
        """Test that all valid solutions are found for simple predicates."""
        # Generate random facts
        random.seed(42)
        facts = []
        expected_solutions = set()

        for i in range(10):
            value = Atom(f"val_{i}")
            facts.append(mk_fact("test", value))
            expected_solutions.add(value)  # Direct structural comparison

        prog = program(*facts)
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])

        # Check all facts are found (using structural equality)
        found_solutions = {sol["X"] for sol in solutions}
        assert found_solutions == expected_solutions

        # Verify only query variables appear in solutions
        for sol in solutions:
            assert set(sol.keys()) == {"X"}

    def test_no_duplicate_solutions(self):
        """Test that no duplicate solutions are returned."""
        prog = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_fact("q", Atom("x")),
            mk_fact("q", Atom("y")),
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("p", (Var(0, "X"),)),
                Struct("q", (Var(1, "Y"),)),
            ),
        )

        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

        # Check for duplicates using structural comparison
        solution_tuples = [(s["X"], s["Y"]) for s in solutions]
        assert len(solution_tuples) == len(set(solution_tuples))

        # Verify solution keys
        for sol in solutions:
            assert set(sol.keys()) == {"X", "Y"}

    def test_correct_depth_first_order(self):
        """Test left-to-right, depth-first solution order."""
        prog = program(
            # First clause tries p1, p2
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("p1", (Var(0, "X"),)),
                Struct("p2", (Var(1, "Y"),)),
            ),
            # Second clause tries p3
            mk_rule("test", (Atom("c"), Var(0, "Y")), Struct("p3", (Var(0, "Y"),))),
            mk_fact("p1", Atom("a")),
            mk_fact("p1", Atom("b")),
            mk_fact("p2", Atom("1")),
            mk_fact("p2", Atom("2")),
            mk_fact("p3", Atom("3")),
        )

        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

        # Expected order: (a,1), (a,2), (b,1), (b,2), (c,3)
        assert len(solutions) == 5
        assert (solutions[0]["X"], solutions[0]["Y"]) == (Atom("a"), Atom("1"))
        assert (solutions[1]["X"], solutions[1]["Y"]) == (Atom("a"), Atom("2"))
        assert (solutions[2]["X"], solutions[2]["Y"]) == (Atom("b"), Atom("1"))
        assert (solutions[3]["X"], solutions[3]["Y"]) == (Atom("b"), Atom("2"))
        assert (solutions[4]["X"], solutions[4]["Y"]) == (Atom("c"), Atom("3"))

    def test_solutions_follow_clause_order(self):
        """Test that solutions follow source clause order."""
        # Create clauses in specific order
        clauses = []
        expected = []
        for i in range(5):
            atom = Atom(f"clause_{i}")
            clauses.append(mk_fact("test", atom))
            expected.append(atom)

        prog = program(*clauses)
        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])

        # Solutions should come in clause order (structural comparison)
        actual = [sol["X"] for sol in solutions]
        assert actual == expected

    def test_deterministic_variable_assignment(self):
        """Test that variable assignment is deterministic."""
        prog = program(
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("unify", (Var(0, "X"), Var(1, "Y"))),
            ),
            mk_fact("unify", Var(0, "A"), Var(0, "A")),  # X = Y through unification
        )

        # Run multiple times to check determinism
        for seed in [1, 2, 3]:
            random.seed(seed)
            engine = Engine(prog)
            solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

            # Should always get one solution with X = Y
            assert len(solutions) == 1
            # Both should be bound to same variable
            assert solutions[0]["X"] == solutions[0]["Y"]

    @pytest.mark.parametrize("seed", [42, 123, 999])
    def test_property_with_multiple_seeds(self, seed):
        """Test properties hold with different random seeds."""
        random.seed(seed)

        # Generate random program
        num_facts = random.randint(5, 15)
        facts = []
        expected = set()
        for i in range(num_facts):
            atom = Atom(f"v_{i}")
            facts.append(mk_fact("p", atom))
            expected.add(atom)

        prog = program(*facts)
        engine = Engine(prog)
        solutions = engine.run([Struct("p", (Var(0, "X"),))])

        # Properties that should always hold
        assert len(solutions) == num_facts  # All solutions found
        found = {s["X"] for s in solutions}
        assert found == expected  # Correct solutions, no duplicates
        assert len(found) == num_facts  # No duplicates

    def test_run_twice_determinism(self):
        """Test that running the same query twice yields identical results."""
        prog = program(
            mk_fact("p", Atom("a")),
            mk_fact("p", Atom("b")),
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("p", (Var(0, "X"),)),
                Struct("p", (Var(1, "Y"),)),
            ),
        )

        # Run twice with fresh engines
        engine1 = Engine(prog)
        solutions1 = engine1.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

        engine2 = Engine(prog)
        solutions2 = engine2.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

        # Should get identical solutions in same order
        assert len(solutions1) == len(solutions2)
        for s1, s2 in zip(solutions1, solutions2):
            assert s1 == s2  # Exact structural equality


class TestStateIsolation:
    """Test that state is properly isolated between solutions."""

    def test_failed_goals_dont_leak_state(self):
        """Test that failed goals don't leak bindings."""
        prog = program(
            # First clause binds X to 'a' then fails
            mk_rule(
                "test",
                (Var(0, "X"),),
                Struct("unify", (Var(0, "X"), Atom("leaked"))),
                Atom("fail"),
            ),
            # Second clause should see X unbound
            mk_rule(
                "test", (Var(0, "X"),), Struct("unify", (Var(0, "X"), Atom("clean")))
            ),
            mk_fact("unify", Var(0, "Y"), Var(0, "Y")),
        )

        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])

        # Should only get the clean solution
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("clean")

    def test_backtracking_fully_restores_state(self):
        """Test that backtracking fully restores previous state."""
        prog = program(
            mk_fact("p", Atom("1")),
            mk_fact("p", Atom("2")),
            # Modifies X in each branch
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("p", (Var(0, "X"),)),
                Struct("modify", (Var(0, "X"), Var(1, "Y"))),
            ),
            # Each call to modify creates different binding
            mk_fact("modify", Atom("1"), Atom("mod1")),
            mk_fact("modify", Atom("2"), Atom("mod2")),
        )

        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

        # Each solution should be independent
        assert len(solutions) == 2
        assert solutions[0] == {"X": Atom("1"), "Y": Atom("mod1")}
        assert solutions[1] == {"X": Atom("2"), "Y": Atom("mod2")}

    def test_renamed_variables_dont_appear_in_solutions(self):
        """Test that renamed clause variables don't leak into solutions."""
        prog = program(
            # Clause with internal variable Z
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y")),
                Struct("helper", (Var(0, "X"), Var(2, "Z"))),
                Struct("helper", (Var(2, "Z"), Var(1, "Y"))),
            ),
            mk_fact("helper", Atom("a"), Atom("b")),
            mk_fact("helper", Atom("b"), Atom("c")),
        )

        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"), Var(1, "Y")))])

        # Solutions should only have query variables X and Y
        assert len(solutions) == 1
        assert set(solutions[0].keys()) == {"X", "Y"}
        assert solutions[0]["X"] == Atom("a")
        assert solutions[0]["Y"] == Atom("c")

    def test_store_trail_goalstack_restored_after_backtrack(self):
        """Test that all state components are restored after backtracking."""
        prog = program(
            mk_rule("test", (Var(0, "X"),), Struct("branch", (Var(0, "X"),))),
            # First branch modifies state then fails
            mk_rule(
                "branch", (Atom("first"),), Struct("modify_state", ()), Atom("fail")
            ),
            # Second branch should see clean state
            mk_fact("branch", Atom("second")),
            # modify_state creates bindings and goals
            mk_rule(
                "modify_state",
                (),
                Struct("p", (Var(0, "_"),)),
                Struct("q", (Var(1, "_"),)),
            ),
            mk_fact("p", Atom("p_val")),
            mk_fact("q", Atom("q_val")),
        )

        engine = Engine(prog)

        solutions = engine.run([Struct("test", (Var(0, "X"),))])

        # Should get only second branch
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("second")

        # State should be restored (allowing for query variables)
        # Query adds some variables, but no leaked bindings from failed branch
        assert len(engine.trail) == 0  # Trail should be empty after success

        # Store should not have grown excessively
        # Only query vars + minimal clause renaming overhead
        store_growth = len(engine.store.cells)
        assert store_growth < 20  # Reasonable bound for this query

    def test_cut_doesnt_leak_state(self):
        """Test that cut doesn't leak state to subsequent solutions."""
        prog = program(
            # First clause with cut
            mk_rule(
                "test",
                (Atom("first"),),
                Struct("bind", (Var(0, "Y"), Atom("bound"))),
                Atom("!"),
                Atom("fail"),
            ),  # Fail after cut
            # Second clause should not see Y binding
            mk_fact("test", Atom("second")),
            mk_fact("bind", Var(0, "X"), Var(0, "X")),
        )

        engine = Engine(prog)
        solutions = engine.run([Struct("test", (Var(0, "X"),))])

        # Cut prevents backtracking, but failure after cut
        # means we don't get first solution
        # Second clause is never tried due to cut
        assert len(solutions) == 0

    def test_nested_backtracking_isolation(self):
        """Test state isolation in nested backtracking."""
        prog = program(
            mk_rule(
                "outer",
                (Var(0, "X"), Var(1, "Y")),
                Struct("inner1", (Var(0, "X"),)),
                Struct("inner2", (Var(1, "Y"),)),
            ),
            # inner1 has multiple solutions
            mk_fact("inner1", Atom("a")),
            mk_fact("inner1", Atom("b")),
            # inner2 also has multiple solutions
            mk_fact("inner2", Atom("1")),
            mk_fact("inner2", Atom("2")),
        )

        engine = Engine(prog)
        initial_store_size = len(engine.store.cells)

        solutions = engine.run([Struct("outer", (Var(0, "X"), Var(1, "Y")))])

        # Should get all combinations
        assert len(solutions) == 4

        # Each solution should be independent (using structural comparison)
        solution_set = {(s["X"], s["Y"]) for s in solutions}
        expected = {
            (Atom("a"), Atom("1")),
            (Atom("a"), Atom("2")),
            (Atom("b"), Atom("1")),
            (Atom("b"), Atom("2")),
        }
        assert solution_set == expected

        # Verify clean state after completion
        assert len(engine.trail) == 0

        # Store shouldn't have grown too much
        final_store_size = len(engine.store.cells)
        store_growth = final_store_size - initial_store_size
        assert store_growth < 15  # Reasonable for 2 query vars + renaming

    def test_backtracking_store_cleanliness(self):
        """Test that store doesn't accumulate junk during backtracking."""
        prog = program(
            # Generate many choicepoints and backtracking
            mk_fact("choice", Atom("a")),
            mk_fact("choice", Atom("b")),
            mk_fact("choice", Atom("c")),
            mk_rule(
                "test",
                (Var(0, "X"), Var(1, "Y"), Var(2, "Z")),
                Struct("choice", (Var(0, "X"),)),
                Struct("choice", (Var(1, "Y"),)),
                Struct("choice", (Var(2, "Z"),)),
                # Force some failures to trigger backtracking
                Struct("different", (Var(0, "X"), Var(1, "Y"))),
                Struct("different", (Var(1, "Y"), Var(2, "Z"))),
            ),
            # different/2 succeeds only if args differ
            mk_rule("different", (Var(0, "X"), Var(0, "X")), Atom("!"), Atom("fail")),
            mk_fact("different", Var(0, "_"), Var(1, "_")),
        )

        engine = Engine(prog)
        initial_cells = len(engine.store.cells)

        solutions = engine.run(
            [Struct("test", (Var(0, "X"), Var(1, "Y"), Var(2, "Z")))]
        )

        # Should get solutions where all three are different
        assert len(solutions) > 0

        # Verify state is clean
        assert len(engine.trail) == 0

        # Store growth should be minimal (just query vars + some renaming)
        final_cells = len(engine.store.cells)
        assert final_cells - initial_cells < 30  # Generous bound for 3 query vars
