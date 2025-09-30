"""
Performance benchmarks for CLP(FD) reification system.

Tests measure overhead, scaling, and pathological cases for reification
vs standard CLP(FD) constraints, following PyLog's benchmark patterns.
"""

import gc
import os
import pytest
import statistics
import sys
import time
import tracemalloc
from typing import Tuple

from prolog.engine.engine import Engine, Program
from prolog.parser.reader import Reader


@pytest.mark.benchmark
@pytest.mark.perf
class TestReificationPerformance:
    """Performance benchmarks for CLP(FD) reification."""

    def should_enforce(self) -> bool:
        """Check if performance thresholds should be enforced."""
        return os.environ.get("CI_ENFORCE_PERF", "").lower() in ("1", "true", "yes")

    def get_threshold(self, name: str, default: float) -> float:
        """
        Get performance threshold from environment or default.

        Note: Current default thresholds represent optimization targets rather
        than current performance. Current reification overhead is ~10,800% for
        simple cases. Use CI_ENFORCE_PERF=0 for baseline establishment without
        enforcement.
        """
        env_var = f"PERF_REIF_{name.upper()}_MAX"
        return float(os.environ.get(env_var, str(default)))

    def measure_execution_time(
        self, engine: Engine, query: str, iterations: int = 5, warmup: int = 1
    ) -> Tuple[float, float]:
        """
        Measure execution time with warmup and GC control.

        Returns:
            Tuple of (median_time, iqr)
        """
        reader = Reader()
        goals = reader.read_query(query)

        # Warmup iterations
        for _ in range(warmup):
            list(engine.run(goals))

        # Disable GC during measurement
        gc_was_enabled = gc.isenabled()
        gc.disable()

        times = []
        try:
            for _ in range(iterations):
                gc.collect()  # Clean slate before each measurement
                start = time.perf_counter()
                list(engine.run(goals))
                end = time.perf_counter()
                times.append(end - start)
        finally:
            # Restore GC state
            if gc_was_enabled:
                gc.enable()

        times.sort()
        median_time = statistics.median(times)

        # Calculate IQR for variability measure
        q1 = times[len(times) // 4]
        q3 = times[3 * len(times) // 4]
        iqr = q3 - q1

        return median_time, iqr

    def create_clpfd_program(self, size: str = "medium") -> Program:
        """Create a test program with standard CLP(FD) constraints."""
        reader = Reader()

        if size == "small":
            # 10 variables, simple constraints
            code = """
            test_clpfd(Xs) :-
                Xs = [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
                Xs ins 1..20,
                X1 #< X2, X2 #< X3, X3 #< X4, X4 #< X5,
                X5 #< X6, X6 #< X7, X7 #< X8, X8 #< X9, X9 #< X10,
                all_different(Xs),
                label(Xs).
            """
        elif size == "medium":
            # 20 variables, more constraints
            code = """
            test_clpfd(Xs) :-
                length(Xs, 20),
                Xs ins 1..50,
                constrain_pairs(Xs),
                all_different(Xs),
                label(Xs).

            constrain_pairs([]).
            constrain_pairs([_]).
            constrain_pairs([X,Y|Rest]) :-
                X #< Y,
                X + Y #=< 60,
                constrain_pairs([Y|Rest]).
            """
        else:  # large
            # 50 variables, complex constraints
            code = """
            test_clpfd(Xs) :-
                length(Xs, 50),
                Xs ins 1..100,
                constrain_all(Xs),
                all_different(Xs),
                label(Xs).

            constrain_all([]).
            constrain_all([_]).
            constrain_all([X,Y|Rest]) :-
                X #< Y,
                X + Y #=< 150,
                abs(X - Y) #>= 2,
                constrain_all([Y|Rest]).

            abs(X, Y) :- X #>= 0, Y #= X.
            abs(X, Y) :- X #< 0, Y #= -X.
            """

        clauses = reader.read_program(code)
        return Program(tuple(clauses))

    def create_reified_program(self, size: str = "medium") -> Program:
        """Create a test program with reified constraints."""
        reader = Reader()

        if size == "small":
            code = """
            test_reified(Xs, Bs) :-
                Xs = [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
                Xs ins 1..20,
                Bs = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
                B1 #<=> (X1 #< X2), B2 #<=> (X2 #< X3),
                B3 #<=> (X3 #< X4), B4 #<=> (X4 #< X5),
                B5 #<=> (X5 #< X6), B6 #<=> (X6 #< X7),
                B7 #<=> (X7 #< X8), B8 #<=> (X8 #< X9),
                B9 #<=> (X9 #< X10),
                sum(Bs, #>=, 7),
                all_different(Xs),
                label(Xs), label(Bs).

            sum([], 0).
            sum([H|T], S) :- sum(T, S1), S #= H + S1.
            """
        elif size == "medium":
            code = """
            test_reified(Xs, Bs) :-
                length(Xs, 20),
                Xs ins 1..50,
                constrain_reified(Xs, Bs),
                all_different(Xs),
                label(Xs), label(Bs).

            constrain_reified([], []).
            constrain_reified([_], []).
            constrain_reified([X,Y|Rest], [B1,B2|BRest]) :-
                B1 #<=> (X #< Y),
                B2 #<=> (X + Y #=< 60),
                constrain_reified([Y|Rest], BRest).
            """
        else:  # large
            code = """
            test_reified(Xs, Bs) :-
                length(Xs, 50),
                Xs ins 1..100,
                constrain_reified_all(Xs, Bs),
                all_different(Xs),
                label(Xs), label(Bs).

            constrain_reified_all([], []).
            constrain_reified_all([_], []).
            constrain_reified_all([X,Y|Rest], [B1,B2,B3|BRest]) :-
                B1 #<=> (X #< Y),
                B2 #<=> (X + Y #=< 150),
                B3 #<=> (abs(X - Y) #>= 2),
                constrain_reified_all([Y|Rest], BRest).

            abs(X, Y) :- X #>= 0, Y #= X.
            abs(X, Y) :- X #< 0, Y #= -X.
            """

        clauses = reader.read_program(code)
        return Program(tuple(clauses))

    # ========== BASELINE TESTS ==========

    def test_baseline_clpfd_no_regression(self):
        """Ensure reification module doesn't slow down non-reified constraints."""

        # Create standard CLP(FD) program
        program = self.create_clpfd_program("medium")

        # Measure without any reification loaded
        # (In PyLog, reification is always available, so we just measure standard constraints)
        engine_baseline = Engine(program)
        baseline_time, baseline_iqr = self.measure_execution_time(
            engine_baseline, "?- test_clpfd(Xs)."
        )

        # Measure with a dummy reified constraint posted but not used
        # This simulates having the reification module loaded
        engine_with_module = Engine(program)
        # Post a trivial reified constraint that doesn't affect the test
        dummy_setup = "?- B #<=> (1 #= 1)."
        list(engine_with_module.run(Reader().read_query(dummy_setup)))

        with_module_time, with_iqr = self.measure_execution_time(
            engine_with_module, "?- test_clpfd(Xs)."
        )

        overhead = (
            ((with_module_time - baseline_time) / baseline_time) * 100
            if baseline_time > 0
            else 0
        )

        print(f"\nModule loading overhead: {overhead:.1f}%")
        print(f"  Baseline: {baseline_time:.3f}s (IQR: {baseline_iqr:.3f}s)")
        print(f"  With module: {with_module_time:.3f}s (IQR: {with_iqr:.3f}s)")

        if self.should_enforce():
            threshold = self.get_threshold("module", 5.0)
            assert (
                overhead <= threshold
            ), f"Module overhead {overhead:.1f}% exceeds threshold {threshold}%"

    # ========== OVERHEAD TESTS ==========

    def test_reification_overhead_simple(self):
        """Compare B #<=> (X #= Y) vs direct X #= Y posting."""

        reader = Reader()

        # Direct constraint
        direct_code = """
        test_direct(X, Y) :-
            X in 1..100, Y in 1..100,
            X #= Y,
            label([X,Y]).
        """
        direct_clauses = reader.read_program(direct_code)
        direct_program = Program(tuple(direct_clauses))
        engine_direct = Engine(direct_program)
        direct_time, direct_iqr = self.measure_execution_time(
            engine_direct, "?- test_direct(X, Y)."
        )

        # Reified constraint
        reified_code = """
        test_reified(X, Y, B) :-
            X in 1..100, Y in 1..100,
            B #<=> (X #= Y),
            label([B,X,Y]).
        """
        reified_clauses = reader.read_program(reified_code)
        reified_program = Program(tuple(reified_clauses))
        engine_reified = Engine(reified_program)
        reified_time, reified_iqr = self.measure_execution_time(
            engine_reified, "?- test_reified(X, Y, B)."
        )

        overhead = (
            ((reified_time - direct_time) / direct_time) * 100 if direct_time > 0 else 0
        )

        print(f"\nSimple reification overhead: {overhead:.1f}%")
        print(f"  Direct: {direct_time:.3f}s (IQR: {direct_iqr:.3f}s)")
        print(f"  Reified: {reified_time:.3f}s (IQR: {reified_iqr:.3f}s)")

        if self.should_enforce():
            threshold = self.get_threshold("simple", 20.0)
            assert (
                overhead <= threshold
            ), f"Simple reification overhead {overhead:.1f}% exceeds threshold {threshold}%"

    def test_reification_overhead_by_constraint_type(self):
        """Test overhead for each constraint type."""

        reader = Reader()
        constraints = [
            ("equality", "X #= Y"),
            ("less_than", "X #< Y"),
            ("less_equal", "X #=< Y"),
            ("greater_than", "X #> Y"),
            ("greater_equal", "X #>= Y"),
            ("not_equal", "X #\\= Y"),
        ]

        results = {}

        for name, constraint in constraints:
            # Direct version
            direct_code = f"""
            test_direct(X, Y) :-
                X in 1..50, Y in 1..50,
                {constraint},
                label([X,Y]).
            """
            direct_clauses = reader.read_program(direct_code)
            direct_program = Program(tuple(direct_clauses))
            engine_direct = Engine(direct_program)
            direct_time, _ = self.measure_execution_time(
                engine_direct,
                "?- test_direct(X, Y).",
                iterations=3,  # Fewer iterations for this test
            )

            # Reified version
            reified_code = f"""
            test_reified(X, Y, B) :-
                X in 1..50, Y in 1..50,
                B #<=> ({constraint}),
                label([B,X,Y]).
            """
            reified_clauses = reader.read_program(reified_code)
            reified_program = Program(tuple(reified_clauses))
            engine_reified = Engine(reified_program)
            reified_time, _ = self.measure_execution_time(
                engine_reified, "?- test_reified(X, Y, B).", iterations=3
            )

            overhead = (
                ((reified_time - direct_time) / direct_time) * 100
                if direct_time > 0
                else 0
            )
            results[name] = overhead

            print(f"  {name}: {overhead:.1f}% overhead")

        # Check all are within threshold
        if self.should_enforce():
            threshold = self.get_threshold("constraint_types", 30.0)
            for name, overhead in results.items():
                assert (
                    overhead <= threshold
                ), f"{name} overhead {overhead:.1f}% exceeds threshold {threshold}%"

    def test_reification_posting_overhead(self):
        """Measure just the constraint posting overhead without solving."""

        reader = Reader()

        # Direct constraint posting only
        direct_code = """
        test_direct_posting(X, Y) :-
            X in 1..10, Y in 1..10,
            X #= Y.
        """
        direct_clauses = reader.read_program(direct_code)
        direct_program = Program(tuple(direct_clauses))

        # Reified constraint posting only
        reified_code = """
        test_reified_posting(X, Y, B) :-
            X in 1..10, Y in 1..10,
            B #<=> (X #= Y).
        """
        reified_clauses = reader.read_program(reified_code)
        reified_program = Program(tuple(reified_clauses))

        # Measure just posting time (no labeling/solving)
        engine_direct = Engine(direct_program)
        direct_time, direct_iqr = self.measure_execution_time(
            engine_direct, "?- test_direct_posting(X, Y).", iterations=10
        )

        engine_reified = Engine(reified_program)
        reified_time, reified_iqr = self.measure_execution_time(
            engine_reified, "?- test_reified_posting(X, Y, B).", iterations=10
        )

        overhead = (
            ((reified_time - direct_time) / direct_time) * 100 if direct_time > 0 else 0
        )

        print(f"\nReification posting overhead: {overhead:.1f}%")
        print(f"  Direct posting: {direct_time:.4f}s (IQR: {direct_iqr:.4f}s)")
        print(f"  Reified posting: {reified_time:.4f}s (IQR: {reified_iqr:.4f}s)")

        if self.should_enforce():
            threshold = self.get_threshold("posting", 15.0)
            assert (
                overhead <= threshold
            ), f"Posting overhead {overhead:.1f}% exceeds threshold {threshold}%"

    # ========== SCALING TESTS ==========

    def test_reification_chain_scaling(self):
        """Test scaling with chains of reified constraints."""

        reader = Reader()
        sizes = [10, 20, 40, 80]
        times = {}

        for size in sizes:
            # Create chain of reified constraints
            constraints = []
            booleans = []
            for i in range(size - 1):
                constraints.append(f"B{i} #<=> (X{i} #< X{i+1})")
                booleans.append(f"B{i}")

            code = f"""
            test_chain(Xs, Bs) :-
                Xs = [{','.join([f'X{i}' for i in range(size)])}],
                Bs = [{','.join(booleans)}],
                Xs ins 1..{size*2},
                {', '.join(constraints)},
                sum(Bs) #>= {size//2},
                all_different(Xs),
                label(Xs), label(Bs).

            sum([]) #= 0.
            sum([H|T]) #= H + S :- sum(T) #= S.
            """

            clauses = reader.read_program(code)
            program = Program(tuple(clauses))
            engine = Engine(program)

            times[size], _ = self.measure_execution_time(
                engine, "?- test_chain(Xs, Bs).", iterations=3, warmup=1
            )

            print(f"  Size {size}: {times[size]:.3f}s")

        # Check scaling factor
        if len(times) >= 2 and times[sizes[0]] > 0:
            scaling = times[sizes[-1]] / times[sizes[0]]
            print(f"\nScaling factor ({sizes[-1]}/{sizes[0]} vars): {scaling:.1f}x")

            if self.should_enforce():
                # Should scale sub-quadratically
                expected_scaling = (sizes[-1] / sizes[0]) ** 1.5  # Allow O(n^1.5)
                threshold = self.get_threshold("scaling", expected_scaling)
                assert (
                    scaling <= threshold
                ), f"Scaling {scaling:.1f}x exceeds threshold {threshold:.1f}x"

    def test_reification_network_scaling(self):
        """Test scaling with interconnected reified constraints."""

        reader = Reader()
        sizes = [5, 10, 15, 20]
        times = {}

        for size in sizes:
            # Create network of interconnected constraints
            code = f"""
            test_network(Xs, Bs) :-
                length(Xs, {size}),
                length(Bs, {size*(size-1)//2}),
                Xs ins 1..{size*3},
                create_network(Xs, Bs),
                sum(Bs) #>= {size},
                label(Xs), label(Bs).

            create_network([], []).
            create_network([_], []).
            create_network([X|Rest], Bs) :-
                create_pairs(X, Rest, Bs1, BsRest),
                create_network(Rest, BsRest),
                append(Bs1, BsRest, Bs).

            create_pairs(_, [], [], []).
            create_pairs(X, [Y|Ys], [B|Bs], Rest) :-
                B #<=> (X #< Y),
                create_pairs(X, Ys, Bs, Rest).

            sum([]) #= 0.
            sum([H|T]) #= H + S :- sum(T) #= S.

            append([], L, L).
            append([H|T], L, [H|R]) :- append(T, L, R).

            length([], 0).
            length([_|T], N) :- length(T, N1), N #= N1 + 1.
            """

            clauses = reader.read_program(code)
            program = Program(tuple(clauses))
            engine = Engine(program)

            times[size], _ = self.measure_execution_time(
                engine,
                "?- test_network(Xs, Bs).",
                iterations=2,
                warmup=1,  # Fewer iterations due to complexity
            )

            print(f"  Network size {size}: {times[size]:.3f}s")

        # Check that scaling is reasonable
        if len(times) >= 2 and times[sizes[0]] > 0:
            scaling = times[sizes[-1]] / times[sizes[0]]
            print(f"\nNetwork scaling factor: {scaling:.1f}x")

    # ========== PATHOLOGICAL CASES ==========

    @pytest.mark.timeout(2)
    def test_no_infinite_propagation_loops(self):
        """Ensure self-notification guards prevent infinite loops."""

        reader = Reader()

        # Create potentially looping constraint network
        code = """
        test_loop(X, Y, Z, B1, B2, B3) :-
            X in 1..10, Y in 1..10, Z in 1..10,
            B1 #<=> (X #= Y),
            B2 #<=> (Y #= Z),
            B3 #<=> (Z #= X),
            B1 #= B2, B2 #= B3,
            label([X, Y, Z, B1, B2, B3]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        # Should terminate quickly
        start = time.perf_counter()
        solutions = list(
            engine.run(Reader().read_query("?- test_loop(X, Y, Z, B1, B2, B3)."))
        )
        elapsed = time.perf_counter() - start

        print(
            f"\nLoop test completed in {elapsed:.3f}s with {len(solutions)} solutions"
        )

        # Should complete very quickly (not hang)
        assert elapsed < 1.0, f"Potential infinite loop detected: took {elapsed:.3f}s"

    def test_deep_reification_nesting(self):
        """Test deeply nested reification doesn't cause exponential blowup."""

        reader = Reader()

        # Create deeply nested reification (10 levels)
        code = """
        test_nested(X, Y, Bs) :-
            X in 1..10, Y in 1..10,
            Bs = [B1, B2, B3, B4, B5, B6, B7, B8, B9, B10],
            B1 #<=> (X #= Y),
            B2 #<=> (B1 #= 1),
            B3 #<=> (B2 #= 1),
            B4 #<=> (B3 #= 1),
            B5 #<=> (B4 #= 1),
            B6 #<=> (B5 #= 1),
            B7 #<=> (B6 #= 1),
            B8 #<=> (B7 #= 1),
            B9 #<=> (B8 #= 1),
            B10 #<=> (B9 #= 1),
            label([X, Y|Bs]).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        start = time.perf_counter()
        solutions = list(engine.run(Reader().read_query("?- test_nested(X, Y, Bs).")))
        elapsed = time.perf_counter() - start

        print(f"\nNested reification test: {elapsed:.3f}s, {len(solutions)} solutions")

        # Should complete reasonably quickly
        assert elapsed < 2.0, f"Deep nesting caused slowdown: {elapsed:.3f}s"

    # ========== MEMORY TESTS ==========

    def test_memory_overhead(self):
        """Measure memory overhead of reified constraints."""

        reader = Reader()

        # Create large problem
        size = 100

        # Baseline memory - direct constraints
        direct_code = f"""
        test_direct(Xs) :-
            length(Xs, {size}),
            Xs ins 1..{size*2},
            constrain_direct(Xs),
            label(Xs).

        constrain_direct([]).
        constrain_direct([_]).
        constrain_direct([X,Y|Rest]) :-
            X #< Y,
            constrain_direct([Y|Rest]).

        length([], 0).
        length([_|T], N) :- length(T, N1), N #= N1 + 1.
        """

        direct_clauses = reader.read_program(direct_code)
        direct_program = Program(tuple(direct_clauses))

        # Measure baseline memory
        tracemalloc.start()
        engine_direct = Engine(direct_program)
        list(
            engine_direct.run(
                Reader().read_query("?- test_direct(Xs)."), max_solutions=1
            )
        )
        baseline_current, baseline_peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()

        # Reified version
        reified_code = f"""
        test_reified(Xs, Bs) :-
            length(Xs, {size}),
            length(Bs, {size-1}),
            Xs ins 1..{size*2},
            constrain_reified(Xs, Bs),
            label(Xs), label(Bs).

        constrain_reified([], []).
        constrain_reified([_], []).
        constrain_reified([X,Y|Rest], [B|BRest]) :-
            B #<=> (X #< Y),
            constrain_reified([Y|Rest], BRest).

        length([], 0).
        length([_|T], N) :- length(T, N1), N #= N1 + 1.
        """

        reified_clauses = reader.read_program(reified_code)
        reified_program = Program(tuple(reified_clauses))

        # Measure reified memory
        tracemalloc.start()
        engine_reified = Engine(reified_program)
        list(
            engine_reified.run(
                Reader().read_query("?- test_reified(Xs, Bs)."), max_solutions=1
            )
        )
        reified_current, reified_peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()

        overhead = (
            ((reified_peak - baseline_peak) / baseline_peak) * 100
            if baseline_peak > 0
            else 0
        )

        print(f"\nMemory overhead: {overhead:.1f}%")
        print(f"  Baseline peak: {baseline_peak / 1024 / 1024:.1f} MB")
        print(f"  Reified peak: {reified_peak / 1024 / 1024:.1f} MB")

        if self.should_enforce():
            threshold = self.get_threshold("memory", 50.0)
            assert (
                overhead <= threshold
            ), f"Memory overhead {overhead:.1f}% exceeds threshold {threshold}%"

    # ========== SCENARIO BENCHMARKS ==========

    def test_boolean_circuit_satisfiability(self):
        """Model Boolean circuit satisfiability using reification."""

        reader = Reader()

        # Model a simple Boolean circuit
        code = """
        % Boolean circuit with AND, OR, NOT gates
        circuit(Inputs, Outputs) :-
            Inputs = [A, B, C, D],
            Outputs = [Out],
            Inputs ins 0..1,

            % Internal wires
            W1 #<=> (A #= 1),     % Buffer
            W2 #<=> (B #= 1),     % Buffer
            W3 #<=> (W1 #= 1),    % W3 = W1 AND W2
            W3 #<=> (W2 #= 1),
            W4 #<=> (C #= 0),     % NOT C
            W5 #<=> (W3 #= 1),    % W5 = W3 OR W4
            W5 #>= W4,
            W6 #<=> (D #= 1),     % Buffer
            Out #<=> (W5 #= 1),   % Out = W5 AND W6
            Out #<=> (W6 #= 1),

            % Find satisfying assignment
            Out #= 1,
            label(Inputs).
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        start = time.perf_counter()
        solutions = list(
            engine.run(Reader().read_query("?- circuit(Inputs, Outputs)."))
        )
        elapsed = time.perf_counter() - start

        print(f"\nBoolean circuit SAT: {elapsed:.3f}s, {len(solutions)} solutions")

        # Should find solutions efficiently
        assert elapsed < 1.0, f"Circuit SAT took too long: {elapsed:.3f}s"
        assert len(solutions) > 0, "Should find satisfying assignments"

    def test_conditional_scheduling(self):
        """Test scheduling with conditional constraints via reification."""

        reader = Reader()

        # Schedule tasks with conditional dependencies
        code = """
        schedule(Starts, Conditions) :-
            % 5 tasks with start times
            Starts = [S1, S2, S3, S4, S5],
            Starts ins 0..20,

            % Conditional constraints
            Conditions = [C1, C2, C3],

            % Task 2 after Task 1 if condition C1
            C1 #<=> (S2 #>= S1 + 3),

            % Task 3 and 4 can't overlap if condition C2
            C2 #<=> ((S3 + 2 #=< S4) #\\/ (S4 + 2 #=< S3)),

            % Task 5 must be last if condition C3
            C3 #<=> (S5 #>= S1 + 5),
            C3 #<=> (S5 #>= S2 + 5),
            C3 #<=> (S5 #>= S3 + 5),
            C3 #<=> (S5 #>= S4 + 5),

            % At least 2 conditions must hold
            C1 + C2 + C3 #>= 2,

            % Minimize makespan
            max_list(Starts, Max),
            Max #=< 15,

            label(Conditions),
            label(Starts).

        max_list([X], X).
        max_list([X|Xs], Max) :-
            max_list(Xs, MaxRest),
            Max #= max(X, MaxRest).

        max(X, Y) #= X :- X #>= Y.
        max(X, Y) #= Y :- Y #> X.
        """

        clauses = reader.read_program(code)
        program = Program(tuple(clauses))
        engine = Engine(program)

        start = time.perf_counter()
        solutions = list(
            engine.run(
                Reader().read_query("?- schedule(Starts, Conditions)."),
                max_solutions=10,
            )
        )
        elapsed = time.perf_counter() - start

        print(
            f"\nConditional scheduling: {elapsed:.3f}s, {len(solutions)} solutions found"
        )

        # Should find feasible schedules
        assert elapsed < 3.0, f"Scheduling took too long: {elapsed:.3f}s"
        assert len(solutions) > 0, "Should find feasible schedules"


if __name__ == "__main__":
    # Allow running directly for debugging
    test = TestReificationPerformance()

    print("Running reification performance benchmarks...")
    print("=" * 60)

    # Run key tests with error handling
    tests = [
        ("Baseline CLP(FD) regression", test.test_baseline_clpfd_no_regression),
        ("Simple reification overhead", test.test_reification_overhead_simple),
        ("Reification posting overhead", test.test_reification_posting_overhead),
        ("Chain scaling", test.test_reification_chain_scaling),
        ("Infinite loop prevention", test.test_no_infinite_propagation_loops),
        ("Memory overhead", test.test_memory_overhead),
    ]

    failed_tests = []

    for test_name, test_func in tests:
        try:
            print(f"\n🧪 Running: {test_name}")
            test_func()
            print(f"✅ Passed: {test_name}")
        except Exception as e:
            print(f"❌ Failed: {test_name} - {e}")
            failed_tests.append((test_name, str(e)))

    print("=" * 60)
    if failed_tests:
        print(f"❌ {len(failed_tests)} test(s) failed:")
        for name, error in failed_tests:
            print(f"  - {name}: {error}")
        sys.exit(1)
    else:
        print("✅ All benchmarks completed successfully!")
        sys.exit(0)
