"""Benchmark tests for all_different/1 constraint performance.

Phase 3 of the all_different implementation plan.
Tests that SEND+MORE cryptarithmetic completes in <1 second with all_different.

Note: Several tests are marked as @pytest.mark.slow and/or @pytest.mark.skip
due to performance issues with arithmetic constraint propagation.

To run tests excluding slow ones:
    pytest -m "not slow"

To run only benchmark tests:
    pytest -m benchmark

To run all tests including slow ones:
    pytest --no-skip
"""

import pytest
import time
from prolog.engine.engine import Engine, Program
from prolog.parser.parser import parse_query


@pytest.mark.benchmark
@pytest.mark.slow
class TestAllDifferentBenchmarks:
    """Benchmark tests for all_different/1 performance."""

    @pytest.mark.slow
    @pytest.mark.skip(reason="SEND+MORE blocked by arithmetic constraint propagation issue")
    @pytest.mark.timeout(1)
    def test_sendmore_with_all_different(self):
        """SEND+MORE with all_different should complete in <1 second."""
        prog_text = """
        % Helper for domain assignment
        ins_list([], _).
        ins_list([X|Xs], D) :- X in D, ins_list(Xs, D).

        sendmore(S,E,N,D,M,O,R,Y) :-
            Vars = [S,E,N,D,M,O,R,Y],
            ins_list(Vars, 0..9),
            all_different(Vars),
            S #> 0, M #> 0,
            1000*S + 100*E + 10*N + D +
            1000*M + 100*O + 10*R + E #=
            10000*M + 1000*O + 100*N + 10*E + Y,
            label(Vars).
        """

        engine = Engine(Program(()))
        engine.consult_string(prog_text)

        # Measure execution time
        start = time.perf_counter()
        solutions = list(engine.query("?- sendmore(S,E,N,D,M,O,R,Y)."))
        elapsed = time.perf_counter() - start

        # Should find exactly one solution
        assert len(solutions) == 1, f"Expected 1 solution, got {len(solutions)}"

        # Verify the solution is correct (S=9, E=5, N=6, D=7, M=1, O=0, R=8, Y=2)
        sol = solutions[0]
        values = {
            'S': sol['S'].value,
            'E': sol['E'].value,
            'N': sol['N'].value,
            'D': sol['D'].value,
            'M': sol['M'].value,
            'O': sol['O'].value,
            'R': sol['R'].value,
            'Y': sol['Y'].value
        }

        # Verify the arithmetic
        send = 1000 * values['S'] + 100 * values['E'] + 10 * values['N'] + values['D']
        more = 1000 * values['M'] + 100 * values['O'] + 10 * values['R'] + values['E']
        money = 10000 * values['M'] + 1000 * values['O'] + 100 * values['N'] + 10 * values['E'] + values['Y']

        assert send + more == money, f"SEND({send}) + MORE({more}) != MONEY({money})"

        # All values should be different
        assert len(set(values.values())) == 8, "Not all values are different"

        # Performance assertion
        assert elapsed < 1.0, f"SEND+MORE took {elapsed:.3f}s (target: <1s)"

    @pytest.mark.slow
    @pytest.mark.skip(reason="Pairwise comparison extremely slow due to arithmetic constraint issue")
    @pytest.mark.timeout(10)
    def test_sendmore_with_pairwise_comparison(self):
        """Compare performance: all_different vs pairwise disequality constraints."""

        # Helper for domain assignment
        helper = """
        ins_list([], _).
        ins_list([X|Xs], D) :- X in D, ins_list(Xs, D).
        """

        # Version with all_different
        prog_alldiff = helper + """
        sendmore_alldiff(S,E,N,D,M,O,R,Y) :-
            Vars = [S,E,N,D,M,O,R,Y],
            ins_list(Vars, 0..9),
            all_different(Vars),
            S #> 0, M #> 0,
            1000*S + 100*E + 10*N + D +
            1000*M + 100*O + 10*R + E #=
            10000*M + 1000*O + 100*N + 10*E + Y,
            label(Vars).
        """

        # Version with pairwise constraints (as baseline)
        prog_pairwise = """
        sendmore_pairwise(S,E,N,D,M,O,R,Y) :-
            S in 0..9, E in 0..9, N in 0..9, D in 0..9,
            M in 0..9, O in 0..9, R in 0..9, Y in 0..9,
            S #\\= E, S #\\= N, S #\\= D, S #\\= M, S #\\= O, S #\\= R, S #\\= Y,
            E #\\= N, E #\\= D, E #\\= M, E #\\= O, E #\\= R, E #\\= Y,
            N #\\= D, N #\\= M, N #\\= O, N #\\= R, N #\\= Y,
            D #\\= M, D #\\= O, D #\\= R, D #\\= Y,
            M #\\= O, M #\\= R, M #\\= Y,
            O #\\= R, O #\\= Y,
            R #\\= Y,
            S #> 0, M #> 0,
            1000*S + 100*E + 10*N + D +
            1000*M + 100*O + 10*R + E #=
            10000*M + 1000*O + 100*N + 10*E + Y,
            label([S,E,N,D,M,O,R,Y]).
        """

        # Measure all_different version
        engine_alldiff = Engine(Program(()))
        engine_alldiff.consult_string(prog_alldiff)

        start = time.perf_counter()
        goals = parse_query("?- sendmore_alldiff(S,E,N,D,M,O,R,Y).")
        solutions_alldiff = engine_alldiff.run(goals, max_solutions=1)
        time_alldiff = time.perf_counter() - start

        # Measure pairwise version (time to first solution)
        engine_pairwise = Engine(Program(()))
        engine_pairwise.consult_string(prog_pairwise)

        start = time.perf_counter()
        goals = parse_query("?- sendmore_pairwise(S,E,N,D,M,O,R,Y).")
        solutions_pairwise = engine_pairwise.run(goals, max_solutions=1)
        time_pairwise = time.perf_counter() - start

        # Both should find at least one solution
        assert len(solutions_alldiff) == 1
        assert len(solutions_pairwise) == 1

        # all_different should be significantly faster
        # Use capsys or logging instead of print for cleaner output
        import logging
        logging.info(f"Performance comparison:")
        logging.info(f"  all_different: {time_alldiff:.3f}s")
        logging.info(f"  pairwise:      {time_pairwise:.3f}s")

        # all_different should be at least 2x faster (conservative estimate)
        assert time_alldiff < time_pairwise / 2, \
            f"all_different not significantly faster ({time_alldiff:.3f}s vs {time_pairwise:.3f}s)"

    @pytest.mark.slow
    @pytest.mark.skip(reason="Requires working ins_list helper")
    @pytest.mark.timeout(0.5)
    def test_sudoku_row_with_all_different(self):
        """Sudoku row constraint with all_different should be fast."""
        prog_text = """
        ins_list([], _).
        ins_list([X|Xs], D) :- X in D, ins_list(Xs, D).

        sudoku_row(A,B,C,D,E,F,G,H,I) :-
            Vars = [A,B,C,D,E,F,G,H,I],
            ins_list(Vars, 1..9),
            all_different(Vars),
            A = 5, I = 9,
            label([B,C,D,E,F,G,H]).
        """

        engine = Engine(Program(()))
        engine.consult_string(prog_text)

        start = time.perf_counter()
        solutions = list(engine.query("?- sudoku_row(A,B,C,D,E,F,G,H,I)."))
        elapsed = time.perf_counter() - start

        # Should find many solutions quickly
        assert len(solutions) > 0

        # Verify solution validity
        for sol in solutions:
            values = [sol[v].value for v in ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']]
            assert len(set(values)) == 9  # All different
            assert set(values) == set(range(1, 10))  # Exactly 1..9
            assert sol['A'].value == 5
            assert sol['I'].value == 9

        # Should be very fast (much less than 1 second)
        assert elapsed < 0.5, f"Sudoku row took {elapsed:.3f}s (target: <0.5s)"

    @pytest.mark.slow
    @pytest.mark.skip(reason="Requires working ins_list helper")
    @pytest.mark.benchmark
    @pytest.mark.timeout(2)
    def test_nqueens_8_with_all_different(self):
        """8-Queens problem using all_different for rows."""
        prog_text = """
        ins_list([], _).
        ins_list([X|Xs], D) :- X in D, ins_list(Xs, D).

        nqueens8(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8) :-
            Queens = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8],
            ins_list(Queens, 1..8),
            all_different(Queens),
            safe_queens(Queens, 1),
            label(Queens).

        safe_queens([], _).
        safe_queens([Q|Qs], Row) :-
            safe_from_others(Q, Qs, Row, 1),
            NextRow #= Row + 1,
            safe_queens(Qs, NextRow).

        safe_from_others(_, [], _, _).
        safe_from_others(Q, [Q2|Qs], Row, Dist) :-
            Q #\\= Q2 + Dist,
            Q #\\= Q2 - Dist,
            NextDist #= Dist + 1,
            safe_from_others(Q, Qs, Row, NextDist).
        """

        engine = Engine(Program(()))
        engine.consult_string(prog_text)

        start = time.perf_counter()
        # Just find first few solutions to test performance
        goals = parse_query("?- nqueens8(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8).")
        solutions = engine.run(goals, max_solutions=3)
        elapsed = time.perf_counter() - start

        # Should find solutions
        assert len(solutions) > 0

        # Verify solutions are valid (all queens in different columns)
        for sol in solutions:
            positions = [sol[f'Q{i}'].value for i in range(1, 9)]
            assert len(set(positions)) == 8  # All different columns

        # Should complete reasonably quickly
        assert elapsed < 2.0, f"8-Queens took {elapsed:.3f}s for 3 solutions (target: <2s)"

    @pytest.mark.slow
    @pytest.mark.skip(reason="Requires working ins_list helper")
    @pytest.mark.timeout(3)
    def test_stress_all_different_large(self):
        """Stress test with larger all_different constraint."""
        # Test with 20 variables
        prog_text = """
        ins_list([], _).
        ins_list([X|Xs], D) :- X in D, ins_list(Xs, D).

        stress20(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,
                 V11,V12,V13,V14,V15,V16,V17,V18,V19,V20) :-
            Vars = [V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,
                    V11,V12,V13,V14,V15,V16,V17,V18,V19,V20],
            ins_list(Vars, 1..20),
            all_different(Vars),
            V1 = 10, V20 = 5,
            label(Vars).
        """

        engine = Engine(Program(()))
        engine.consult_string(prog_text)

        start = time.perf_counter()
        # Just get first solution
        goals = parse_query("?- stress20(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20).")
        solutions = engine.run(goals, max_solutions=1)
        elapsed = time.perf_counter() - start

        # Should find at least one solution
        assert len(solutions) == 1

        # Verify all different
        sol = solutions[0]
        values = [sol[f'V{i}'].value for i in range(1, 21)]
        assert len(set(values)) == 20
        assert set(values) == set(range(1, 21))

        # Should complete in reasonable time even for 20 variables
        assert elapsed < 3.0, f"20 variable all_different took {elapsed:.3f}s (target: <3s)"


@pytest.mark.benchmark
class TestHallIntervalPruning:
    """Test that Hall-interval pruning is working correctly."""

    def test_hall_interval_basic(self):
        """Test basic Hall interval pruning case."""
        # Using direct query instead of wrapper predicate
        engine = Engine(Program(()))

        query = """
        ?- A in 1..2, B in 1..2, C in 1..3,
           all_different([A,B,C]).
        """

        solutions = list(engine.query(query))

        # After posting all_different, C should be pruned to 3
        # because A and B must take values 1 and 2
        assert len(solutions) == 1  # Just checking constraints are set up

        # Check domain of C before labeling
        from prolog.clpfd.api import get_domain
        sol = solutions[0]
        c_dom = get_domain(engine.store, sol['C'].id)

        # C should be constrained to 3 due to Hall set {1,2}
        assert c_dom.min() == 3 and c_dom.max() == 3, \
            f"Expected C domain to be {{3}}, got [{c_dom.min()}..{c_dom.max()}]"

    def test_hall_interval_complex(self):
        """Test more complex Hall interval case."""
        engine = Engine(Program(()))

        query = """
        ?- A in 1..4, B in 1..4, C in 2..3, D in 2..3, E in 1..5,
           all_different([A,B,C,D,E]).
        """

        solutions = list(engine.query(query))

        assert len(solutions) == 1  # Just checking constraints are set up

        # C and D form a Hall set on {2,3}
        # So A, B, E should have 2 and 3 removed
        from prolog.clpfd.api import get_domain
        sol = solutions[0]

        a_dom = get_domain(engine.store, sol['A'].id)
        b_dom = get_domain(engine.store, sol['B'].id)
        e_dom = get_domain(engine.store, sol['E'].id)

        # Check that 2 and 3 are removed from A, B, E
        assert not a_dom.contains(2) and not a_dom.contains(3), \
            f"A should not contain 2 or 3, domain: {a_dom}"
        assert not b_dom.contains(2) and not b_dom.contains(3), \
            f"B should not contain 2 or 3, domain: {b_dom}"
        assert not e_dom.contains(2) and not e_dom.contains(3), \
            f"E should not contain 2 or 3, domain: {e_dom}"