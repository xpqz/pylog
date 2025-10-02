"""Scenario tests for table/2 constraint showing real-world usage."""

from prolog.engine.engine import Engine, Program


class TestTableScenarios:
    """Test table/2 constraint in practical scenarios."""

    def test_compatible_room_assignments(self):
        """Use table to model compatible room assignments between people."""
        engine = Engine(Program([]))

        # Alice, Bob, Carol need rooms 1, 2, 3
        # Compatibility constraints via table
        query = """
        ?- Alice in 1..3, Bob in 1..3, Carol in 1..3,
           all_different([Alice, Bob, Carol]),
           table([Alice, Bob], [[1,2], [1,3], [2,1], [3,2]]),
           table([Bob, Carol], [[1,3], [2,1], [2,3]]),
           label([Alice, Bob, Carol]).
        """
        solutions = list(engine.query(query))

        # Should find compatible assignments
        assert len(solutions) >= 1

        # Verify all constraints are satisfied
        for sol in solutions:
            alice, bob, carol = sol["Alice"].value, sol["Bob"].value, sol["Carol"].value
            # All different
            assert len({alice, bob, carol}) == 3
            # Alice-Bob compatibility
            assert (alice, bob) in [(1, 2), (1, 3), (2, 1), (3, 2)]
            # Bob-Carol compatibility
            assert (bob, carol) in [(1, 3), (2, 1), (2, 3)]

    def test_course_scheduling(self):
        """Model course scheduling with time slot constraints."""
        engine = Engine(Program([]))

        # Courses A, B, C need time slots
        # Some courses cannot be scheduled at certain times
        query = """
        ?- A in 1..4, B in 1..4, C in 1..4,
           all_different([A, B, C]),
           table([A], [[1], [2], [4]]),
           table([B], [[2], [3], [4]]),
           table([C], [[1], [3], [4]]),
           label([A, B, C]).
        """
        solutions = list(engine.query(query))

        # Should find valid schedules
        assert len(solutions) >= 1

        for sol in solutions:
            a, b, c = sol["A"].value, sol["B"].value, sol["C"].value
            # All different time slots
            assert len({a, b, c}) == 3
            # A can only be in slots 1, 2, or 4
            assert a in [1, 2, 4]
            # B can only be in slots 2, 3, or 4
            assert b in [2, 3, 4]
            # C can only be in slots 1, 3, or 4
            assert c in [1, 3, 4]

    def test_map_coloring(self):
        """Use table for map coloring with adjacency constraints."""
        engine = Engine(Program([]))

        # Color a simple map with 3 regions using 3 colors
        # Adjacency defined by table constraint
        query = """
        ?- R1 in 1..3, R2 in 1..3, R3 in 1..3,
           table([R1, R2], [[1,2], [1,3], [2,1], [2,3], [3,1], [3,2]]),
           table([R2, R3], [[1,2], [1,3], [2,1], [2,3], [3,1], [3,2]]),
           table([R1, R3], [[1,2], [1,3], [2,1], [2,3], [3,1], [3,2]]),
           label([R1, R2, R3]).
        """
        solutions = list(engine.query(query))

        # Should find valid colorings
        assert len(solutions) >= 1

        for sol in solutions:
            r1, r2, r3 = sol["R1"].value, sol["R2"].value, sol["R3"].value
            # Adjacent regions have different colors
            assert r1 != r2
            assert r2 != r3
            assert r1 != r3

    def test_work_shift_assignment(self):
        """Model work shift assignments with availability tables."""
        engine = Engine(Program([]))

        # Workers W1, W2, W3 assigned to shifts 1, 2, 3
        # Availability constraints via table
        query = """
        ?- W1 in 1..3, W2 in 1..3, W3 in 1..3,
           all_different([W1, W2, W3]),
           table([W1], [[1], [3]]),
           table([W2], [[1], [2]]),
           table([W3], [[2], [3]]),
           label([W1, W2, W3]).
        """
        solutions = list(engine.query(query))

        # Should find valid assignments
        assert len(solutions) >= 1

        for sol in solutions:
            w1, w2, w3 = sol["W1"].value, sol["W2"].value, sol["W3"].value
            # Check availability constraints
            assert w1 in [1, 3]  # W1 available for shifts 1, 3
            assert w2 in [1, 2]  # W2 available for shifts 1, 2
            assert w3 in [2, 3]  # W3 available for shifts 2, 3
            # All workers get different shifts
            assert len({w1, w2, w3}) == 3

    def test_menu_combinations(self):
        """Model valid menu combinations using table constraints."""
        engine = Engine(Program([]))

        # Appetizer, Main, Dessert combinations
        # Some combinations are not allowed
        query = """
        ?- App in 1..3, Main in 1..3, Dessert in 1..3,
           table([App, Main], [[1,1], [1,2], [2,1], [2,3], [3,2], [3,3]]),
           table([Main, Dessert], [[1,1], [1,3], [2,2], [2,3], [3,1], [3,2]]),
           label([App, Main, Dessert]).
        """
        solutions = list(engine.query(query))

        # Should find valid combinations
        assert len(solutions) >= 1

        for sol in solutions:
            app, main, dessert = (
                sol["App"].value,
                sol["Main"].value,
                sol["Dessert"].value,
            )
            # Verify valid App-Main combinations
            assert (app, main) in [(1, 1), (1, 2), (2, 1), (2, 3), (3, 2), (3, 3)]
            # Verify valid Main-Dessert combinations
            assert (main, dessert) in [(1, 1), (1, 3), (2, 2), (2, 3), (3, 1), (3, 2)]

    def test_network_routing(self):
        """Model network routing with path constraints."""
        engine = Engine(Program([]))

        # Route from node 1 to node 4 through intermediate nodes
        # Valid connections defined by table
        query = """
        ?- Start in 1..1, Node2 in 2..3, Node3 in 2..4, End in 4..4,
           table([Start, Node2], [[1,2], [1,3]]),
           table([Node2, Node3], [[2,3], [2,4], [3,2], [3,4]]),
           table([Node3, End], [[2,4], [3,4], [4,4]]),
           label([Start, Node2, Node3, End]).
        """
        solutions = list(engine.query(query))

        # Should find valid paths
        assert len(solutions) >= 1

        for sol in solutions:
            start, node2, node3, end = (
                sol["Start"].value,
                sol["Node2"].value,
                sol["Node3"].value,
                sol["End"].value,
            )
            assert start == 1
            assert end == 4
            # Verify valid connections
            assert (start, node2) in [(1, 2), (1, 3)]
            assert (node2, node3) in [(2, 3), (2, 4), (3, 2), (3, 4)]
            assert (node3, end) in [(2, 4), (3, 4), (4, 4)]

    def test_product_configuration(self):
        """Model product configuration with compatibility table."""
        engine = Engine(Program([]))

        # Configure CPU, RAM, Storage with compatibility constraints
        query = """
        ?- CPU in 1..3, RAM in 1..3, Storage in 1..3,
           table([CPU, RAM], [[1,1], [1,2], [2,2], [2,3], [3,3]]),
           table([CPU, Storage], [[1,1], [1,2], [2,1], [2,2], [2,3], [3,2], [3,3]]),
           table([RAM, Storage], [[1,1], [1,2], [2,2], [2,3], [3,3]]),
           label([CPU, RAM, Storage]).
        """
        solutions = list(engine.query(query))

        # Should find compatible configurations
        assert len(solutions) >= 1

        for sol in solutions:
            cpu, ram, storage = sol["CPU"].value, sol["RAM"].value, sol["Storage"].value
            # Verify CPU-RAM compatibility
            assert (cpu, ram) in [(1, 1), (1, 2), (2, 2), (2, 3), (3, 3)]
            # Verify CPU-Storage compatibility
            assert (cpu, storage) in [
                (1, 1),
                (1, 2),
                (2, 1),
                (2, 2),
                (2, 3),
                (3, 2),
                (3, 3),
            ]
            # Verify RAM-Storage compatibility
            assert (ram, storage) in [(1, 1), (1, 2), (2, 2), (2, 3), (3, 3)]

    def test_table_with_arithmetic(self):
        """Combine table constraints with arithmetic."""
        engine = Engine(Program([]))

        # Choose X, Y from table and compute their sum
        query = """
        ?- X in 1..5, Y in 1..5, Sum in 1..10,
           table([X, Y], [[1,3], [2,4], [3,1], [4,2], [5,5]]),
           Sum #= X + Y,
           label([X, Y, Sum]).
        """
        solutions = list(engine.query(query))

        # Should find all valid combinations
        assert len(solutions) == 5

        expected = {(1, 3, 4), (2, 4, 6), (3, 1, 4), (4, 2, 6), (5, 5, 10)}
        actual = {
            (sol["X"].value, sol["Y"].value, sol["Sum"].value) for sol in solutions
        }
        assert actual == expected

    def test_complex_scheduling(self):
        """Complex scheduling example with multiple table constraints."""
        engine = Engine(Program([]))

        # Schedule 3 tasks on 2 machines with precedence and resource constraints
        query = """
        ?- T1 in 1..4, T2 in 1..4, T3 in 1..4,
           M1 in 1..2, M2 in 1..2, M3 in 1..2,
           table([T1, M1], [[1,1], [1,2], [2,1], [3,2]]),
           table([T2, M2], [[2,1], [2,2], [3,1], [4,2]]),
           table([T3, M3], [[3,1], [3,2], [4,1], [4,2]]),
           T1 #< T2,
           T2 #< T3,
           label([T1, T2, T3, M1, M2, M3]).
        """
        solutions = list(engine.query(query))

        # Should find valid schedules
        assert len(solutions) >= 1

        for sol in solutions:
            t1, t2, t3 = sol["T1"].value, sol["T2"].value, sol["T3"].value
            m1, m2, m3 = sol["M1"].value, sol["M2"].value, sol["M3"].value

            # Verify precedence constraints
            assert t1 < t2 < t3

            # Verify task-machine compatibility
            assert (t1, m1) in [(1, 1), (1, 2), (2, 1), (3, 2)]
            assert (t2, m2) in [(2, 1), (2, 2), (3, 1), (4, 2)]
            assert (t3, m3) in [(3, 1), (3, 2), (4, 1), (4, 2)]
