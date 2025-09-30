"""Test for reification propagation during unification fix.

This test documents the fix for a critical issue where the equality
propagator would bind variables during unification, causing "Variable
already bound" errors. The fix ensures that:

1. Propagators use safe_bind_singleton to avoid double-binding
2. Unification checks if variables were bound during hook execution
3. The trichotomy property holds with proper solution counts
"""

from prolog.parser.reader import Reader
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.ast.terms import Int


class TestReificationUnifyFix:
    """Test cases documenting the reification unification fix."""

    def test_equality_reification_with_unification(self):
        """Test B #<=> (X #= Y) followed by unification."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # This used to fail with "Variable already bound"
        query = reader.read_term(
            """
            X in 1..2, Y in 1..2,
            B #<=> (X #= Y),
            B = 1,
            X = 1
        """
        )

        solutions = list(engine.solve(query))
        assert len(solutions) == 1

        # Verify Y was correctly bound to 1
        sol = solutions[0]
        y = sol["Y"]
        if isinstance(y, Int):
            y_val = y.value
        else:
            cell = engine.store.cells[y.id]
            y_val = cell.term.value if cell.tag == "bound" else None
        assert y_val == 1

    def test_equality_reification_with_labeling(self):
        """Test B #<=> (X #= Y) with labeling."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..2, Y in 1..2,
            B #<=> (X #= Y),
            label([B, X, Y])
        """
        )

        solutions = list(engine.solve(query))
        assert len(solutions) == 4

        # Count solutions by B value
        b_counts = {0: 0, 1: 0}
        for sol in solutions:
            b = sol["B"]
            if isinstance(b, Int):
                b_val = b.value
            else:
                cell = engine.store.cells[b.id]
                b_val = cell.term.value
            b_counts[b_val] += 1

        # Should have 2 solutions with B=0 (X!=Y) and 2 with B=1 (X=Y)
        assert b_counts[0] == 2  # (1,2) and (2,1)
        assert b_counts[1] == 2  # (1,1) and (2,2)

    def test_trichotomy_property(self):
        """Test that exactly one of <, =, > holds (trichotomy)."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        query = reader.read_term(
            """
            X in 1..3, Y in 1..3,
            B1 #<=> (X #< Y),
            B2 #<=> (X #= Y),
            B3 #<=> (X #> Y),
            B1 + B2 + B3 #= 1,
            label([B1, B2, B3, X, Y])
        """
        )

        solutions = list(engine.solve(query))

        # Should have exactly 9 solutions (3x3 grid)
        assert len(solutions) == 9

        # Group solutions by constraint type
        constraint_groups = {"<": [], "=": [], ">": []}

        for sol in solutions:
            # Get B values
            b1 = (
                sol["B1"].value
                if isinstance(sol["B1"], Int)
                else engine.store.cells[sol["B1"].id].term.value
            )
            b2 = (
                sol["B2"].value
                if isinstance(sol["B2"], Int)
                else engine.store.cells[sol["B2"].id].term.value
            )
            b3 = (
                sol["B3"].value
                if isinstance(sol["B3"], Int)
                else engine.store.cells[sol["B3"].id].term.value
            )

            # Get X, Y values
            x = (
                sol["X"].value
                if isinstance(sol["X"], Int)
                else engine.store.cells[sol["X"].id].term.value
            )
            y = (
                sol["Y"].value
                if isinstance(sol["Y"], Int)
                else engine.store.cells[sol["Y"].id].term.value
            )

            # Exactly one B should be 1
            assert b1 + b2 + b3 == 1

            if b1 == 1:
                assert x < y
                constraint_groups["<"].append((x, y))
            elif b2 == 1:
                assert x == y
                constraint_groups["="].append((x, y))
            else:
                assert b3 == 1
                assert x > y
                constraint_groups[">"].append((x, y))

        # Each constraint type should have 3 solutions
        assert len(constraint_groups["<"]) == 3
        assert len(constraint_groups["="]) == 3  # This was 0 before the fix!
        assert len(constraint_groups[">"]) == 3

        # Verify specific solutions
        assert (1, 1) in constraint_groups["="]
        assert (2, 2) in constraint_groups["="]
        assert (3, 3) in constraint_groups["="]

    def test_reification_during_propagation(self):
        """Test that reification works correctly when triggered during propagation."""
        reader = Reader()
        program = Program(())
        engine = Engine(program)

        # Complex case: B2=1 triggers equality, then X is labeled
        query = reader.read_term(
            """
            X in 1..3, Y in 1..3,
            B2 #<=> (X #= Y),
            B2 = 1,
            label([X, Y])
        """
        )

        solutions = list(engine.solve(query))

        # Should have 3 solutions: (1,1), (2,2), (3,3)
        assert len(solutions) == 3

        for sol in solutions:
            x = (
                sol["X"].value
                if isinstance(sol["X"], Int)
                else engine.store.cells[sol["X"].id].term.value
            )
            y = (
                sol["Y"].value
                if isinstance(sol["Y"], Int)
                else engine.store.cells[sol["Y"].id].term.value
            )
            assert x == y  # Equality must hold
