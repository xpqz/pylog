"""Tests for Backtracking (Stage 0 Phase 3)."""

from prolog.ast.terms import Atom, Var, Struct
from prolog.engine.engine import Engine


# Test helpers
from prolog.tests.helpers import mk_fact, mk_rule, program


class TestChoicepointCreation:
    """Tests for choicepoint creation during clause selection."""

    def test_choicepoint_created_when_multiple_clauses_match(self):
        """Test choicepoint is created when multiple clauses match."""
        # Multiple facts for the same predicate
        prog = program(
            mk_fact("color", Atom("red")),
            mk_fact("color", Atom("green")),
            mk_fact("color", Atom("blue")),
        )
        engine = Engine(prog)

        # Query: color(X)
        # Allocate query variable explicitly
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("color", (x_var,))])

        # Should find all three solutions via backtracking
        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("red")
        assert solutions[1]["X"] == Atom("green")
        assert solutions[2]["X"] == Atom("blue")

    def test_no_choicepoint_when_single_clause_matches(self):
        """Test no choicepoint is created when only one clause matches."""
        prog = program(mk_fact("unique", Atom("only")))
        engine = Engine(prog)

        # Query: unique(X)
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("unique", (x_var,))])

        # Single solution, no backtracking needed
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("only")

    def test_choicepoint_preserves_pre_goal_snapshot(self):
        """Test choicepoint preserves pre-goal snapshot (goal still on top)."""
        prog = program(mk_fact("test", Atom("a")), mk_fact("test", Atom("b")))
        engine = Engine(prog)

        # Query that creates a choicepoint
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("test", (x_var,))])
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")

    def test_choicepoint_cursor_positioned_at_next_clause(self):
        """Test choicepoint cursor is positioned at the next clause."""
        prog = program(
            mk_fact("seq", Atom("first")),
            mk_fact("seq", Atom("second")),
            mk_fact("seq", Atom("third")),
        )
        engine = Engine(prog)

        # Query should try clauses in order
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("seq", (x_var,))])

        # Verify order matches clause definition order
        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("first")
        assert solutions[1]["X"] == Atom("second")
        assert solutions[2]["X"] == Atom("third")

    def test_goal_retry_correctness_three_clauses(self):
        """Test goal retry correctness with 3 clauses yields 3 solutions in order."""
        prog = program(
            mk_fact("num", Atom("one")),
            mk_fact("num", Atom("two")),
            mk_fact("num", Atom("three")),
        )
        engine = Engine(prog)

        n_var = Var(engine.store.new_var("N"), "N")
        engine._query_vars = [(n_var.id, "N")]

        solutions = engine.run([Struct("num", (n_var,))])

        assert len(solutions) == 3
        assert solutions[0]["N"] == Atom("one")
        assert solutions[1]["N"] == Atom("two")
        assert solutions[2]["N"] == Atom("three")


class TestBacktrackingImplementation:
    """Tests for backtracking behavior."""

    def test_backtrack_restores_store_state_exactly(self):
        """Test backtrack restores store state exactly (undo_to + shrink)."""
        prog = program(
            mk_fact("pair", Atom("a"), Atom("1")), mk_fact("pair", Atom("b"), Atom("2"))
        )
        engine = Engine(prog)

        # Allocate query variables
        x_var = Var(engine.store.new_var("X"), "X")
        y_var = Var(engine.store.new_var("Y"), "Y")
        engine._query_vars = [(x_var.id, "X"), (y_var.id, "Y")]

        # Snapshot store state before query
        store_snap = [(c.tag, c.ref, c.term, c.rank) for c in engine.store.cells]
        trail_len = len(engine.trail)

        # Query with variables that get bound then unbound
        solutions = engine.run([Struct("pair", (x_var, y_var))])

        # Both solutions found via backtracking
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("a")
        assert solutions[0]["Y"] == Atom("1")
        assert solutions[1]["X"] == Atom("b")
        assert solutions[1]["Y"] == Atom("2")

        # After completion, store should be restored
        # Query vars exist but are unbound
        assert len(engine.store.cells) == 2  # Just the query vars
        assert engine.store.cells[0].tag == "unbound"
        assert engine.store.cells[1].tag == "unbound"

    def test_backtrack_restores_goal_stack_from_snapshot(self):
        """Test backtrack restores goal stack from snapshot."""
        prog = program(
            mk_rule(
                "path",
                (Var(0, "X"), Var(1, "Y")),
                Struct("edge", (Var(0, "X"), Var(1, "Y"))),
            ),
            mk_rule(
                "path",
                (Var(0, "X"), Var(2, "Z")),
                Struct("edge", (Var(0, "X"), Var(1, "Y"))),
                Struct("path", (Var(1, "Y"), Var(2, "Z"))),
            ),
            mk_fact("edge", Atom("a"), Atom("b")),
            mk_fact("edge", Atom("b"), Atom("c")),
        )
        engine = Engine(prog)

        # Query: path(a, X)
        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("path", (Atom("a"), x_var))])

        # Should find both direct (a->b) and transitive (a->c) paths
        assert len(solutions) == 2
        paths = [s["X"].name for s in solutions]
        # Depth-first order: direct path first, then via recursion
        assert paths == ["b", "c"]

    def test_backtrack_restores_cut_barrier(self):
        """Test backtrack restores cut_barrier."""
        # This test would need cut implementation
        # Placeholder for now
        pass

    def test_backtrack_reselects_same_goal_tries_next_clause(self):
        """Test backtrack re-selects same goal and tries next clause."""
        prog = program(
            mk_fact("choice", Atom("option1")),
            mk_fact("choice", Atom("option2")),
            mk_fact("choice", Atom("option3")),
        )
        engine = Engine(prog)

        c_var = Var(engine.store.new_var("C"), "C")
        engine._query_vars = [(c_var.id, "C")]

        solutions = engine.run([Struct("choice", (c_var,))])

        # All three options should be found in order
        assert len(solutions) == 3
        assert [s["C"].name for s in solutions] == ["option1", "option2", "option3"]

    def test_backtrack_chains_to_earlier_choicepoint_when_exhausted(self):
        """Test backtrack chains to earlier choicepoint when exhausted."""
        prog = program(
            mk_fact("first", Atom("a")),
            mk_fact("first", Atom("b")),
            mk_fact("second", Atom("1")),
            mk_fact("second", Atom("2")),
        )
        engine = Engine(prog)

        # Allocate query variables
        x_var = Var(engine.store.new_var("X"), "X")
        y_var = Var(engine.store.new_var("Y"), "Y")
        engine._query_vars = [(x_var.id, "X"), (y_var.id, "Y")]

        # Query with two choice points
        solutions = engine.run([Struct("first", (x_var,)), Struct("second", (y_var,))])

        # Should get all combinations: (a,1), (a,2), (b,1), (b,2)
        assert len(solutions) == 4

        # Check we get all combinations in depth-first order
        combos = [(s["X"].name, s["Y"].name) for s in solutions]
        assert combos == [("a", "1"), ("a", "2"), ("b", "1"), ("b", "2")]

    def test_backtrack_returns_false_when_no_choicepoints(self):
        """Test backtrack returns false when no choicepoints remain."""
        prog = program(mk_fact("single", Atom("only")))
        engine = Engine(prog)

        # Query that can't match
        solutions = engine.run([Struct("single", (Atom("wrong"),))])

        # No solutions found
        assert len(solutions) == 0

    def test_state_fully_restored_between_solution_attempts(self):
        """Test state is fully restored between solution attempts."""
        prog = program(
            mk_rule(
                "test", (Var(0, "X"),), Struct("bind", (Var(0, "X"), Atom("first")))
            ),
            mk_rule(
                "test", (Var(0, "X"),), Struct("bind", (Var(0, "X"), Atom("second")))
            ),
            mk_fact("bind", Atom("first"), Atom("first")),
            mk_fact("bind", Atom("second"), Atom("second")),
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        solutions = engine.run([Struct("test", (x_var,))])

        # Both rules should succeed independently
        assert len(solutions) == 2
        assert solutions[0]["X"] == Atom("first")
        assert solutions[1]["X"] == Atom("second")

    def test_arity_zero_backtracking(self):
        """Test backtracking with arity-0 predicates."""
        prog = program(
            mk_fact("truth"),  # Arity-0 fact
            mk_fact("truth"),  # Another truth
            mk_fact("truth"),  # Third truth
        )
        engine = Engine(prog)

        # Query: truth (arity-0)
        solutions = engine.run([Atom("truth")])

        # Should find all three "truth" facts
        assert len(solutions) == 3
        # All solutions are identical for arity-0 predicates
        for sol in solutions:
            assert sol == {}  # No variables to bind


class TestMultipleSolutions:
    """Tests for collecting multiple solutions via backtracking."""

    def test_collect_all_solutions_for_multi_clause_predicate(self):
        """Test collecting all solutions for multi-clause predicate."""
        prog = program(
            mk_fact("fruit", Atom("apple")),
            mk_fact("fruit", Atom("banana")),
            mk_fact("fruit", Atom("cherry")),
            mk_fact("fruit", Atom("date")),
        )
        engine = Engine(prog)

        f_var = Var(engine.store.new_var("F"), "F")
        engine._query_vars = [(f_var.id, "F")]

        solutions = engine.run([Struct("fruit", (f_var,))])

        assert len(solutions) == 4
        fruits = [s["F"].name for s in solutions]
        assert fruits == ["apple", "banana", "cherry", "date"]

    def test_solutions_in_source_clause_order(self):
        """Test solutions are returned in source clause order (left-to-right, depth-first)."""
        prog = program(
            mk_fact("ordered", Atom("1st")),
            mk_fact("ordered", Atom("2nd")),
            mk_fact("ordered", Atom("3rd")),
            mk_fact("ordered", Atom("4th")),
            mk_fact("ordered", Atom("5th")),
        )
        engine = Engine(prog)

        n_var = Var(engine.store.new_var("N"), "N")
        engine._query_vars = [(n_var.id, "N")]

        solutions = engine.run([Struct("ordered", (n_var,))])

        assert len(solutions) == 5
        order = [s["N"].name for s in solutions]
        assert order == ["1st", "2nd", "3rd", "4th", "5th"]

    def test_max_solutions_limits_results_and_stops_searching(self):
        """Test max_solutions limits results and stops searching."""
        prog = program(
            mk_fact("many", Atom("a")),
            mk_fact("many", Atom("b")),
            mk_fact("many", Atom("c")),
            mk_fact("many", Atom("d")),
            mk_fact("many", Atom("e")),
        )
        engine = Engine(prog)

        x_var = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x_var.id, "X")]

        # Override max_solutions for this query
        solutions = engine.run([Struct("many", (x_var,))], max_solutions=3)

        # Should stop after 3 solutions
        assert len(solutions) == 3
        assert solutions[0]["X"] == Atom("a")
        assert solutions[1]["X"] == Atom("b")
        assert solutions[2]["X"] == Atom("c")
        # d and e should not be found

    def test_backtracking_through_nested_rules_preserves_order(self):
        """Test backtracking through nested rules preserves order."""
        prog = program(
            mk_rule(
                "double",
                (Var(0, "X"), Var(1, "Y")),
                Struct("num", (Var(0, "X"),)),
                Struct("num", (Var(1, "Y"),)),
            ),
            mk_fact("num", Atom("1")),
            mk_fact("num", Atom("2")),
            mk_fact("num", Atom("3")),
        )
        engine = Engine(prog)

        a_var = Var(engine.store.new_var("A"), "A")
        b_var = Var(engine.store.new_var("B"), "B")
        engine._query_vars = [(a_var.id, "A"), (b_var.id, "B")]

        solutions = engine.run([Struct("double", (a_var, b_var))])

        # Should get all pairs in order: (1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)
        assert len(solutions) == 9

        pairs = [(s["A"].name, s["B"].name) for s in solutions]
        expected = [
            ("1", "1"),
            ("1", "2"),
            ("1", "3"),
            ("2", "1"),
            ("2", "2"),
            ("2", "3"),
            ("3", "1"),
            ("3", "2"),
            ("3", "3"),
        ]
        assert pairs == expected

    def test_state_isolation_failing_branch_doesnt_affect_later_success(self):
        """Test state isolation - failing branch doesn't affect later success."""
        # Program where one branch fails due to missing fact
        prog = program(
            mk_rule(
                "result",
                (Var(0, "X"),),
                Struct("option", (Var(0, "X"),)),
                Struct("check", (Var(0, "X"),)),
            ),
            mk_fact("option", Atom("good")),
            mk_fact("option", Atom("bad")),
            mk_fact("check", Atom("good")),
            # No check(bad) so that branch fails
        )
        engine = Engine(prog)

        r_var = Var(engine.store.new_var("R"), "R")
        engine._query_vars = [(r_var.id, "R")]

        solutions = engine.run([Struct("result", (r_var,))])

        # Should only get the "good" solution
        assert len(solutions) == 1
        assert solutions[0]["R"] == Atom("good")
