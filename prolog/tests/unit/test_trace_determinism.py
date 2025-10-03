"""
Tests for trace determinism and reproducibility.

Ensures that:
- Identical runs produce identical traces
- JSONL traces are byte-for-byte equal with timestamps disabled
- Variable names are deterministic
- Step IDs are sequential (post-filter)
- 4-port stream is identical with/without indexing
"""

import json

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine
from prolog.debug.sinks import CollectorSink, JSONLTraceSink


class TestTraceDeterminism:
    """Test trace determinism and reproducibility."""

    def test_identical_runs_produce_identical_traces(self):
        """Test that identical runs produce identical trace sequences."""
        # Create a test program with backtracking
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
            Clause(head=Struct("p", (Atom("c"),)), body=()),
            Clause(
                head=Struct("q", (Var(0, "X"),)), body=(Struct("p", (Var(0, "X"),)),)
            ),
        ]
        program = Program(tuple(clauses))

        # Run 1
        engine1 = Engine(program, trace=True)
        collector1 = CollectorSink()
        engine1.tracer.add_sink(collector1)
        results1 = list(engine1.query("q(X)"))

        # Run 2 - should be identical
        engine2 = Engine(program, trace=True)
        collector2 = CollectorSink()
        engine2.tracer.add_sink(collector2)
        results2 = list(engine2.query("q(X)"))

        # Results should be identical
        assert results1 == results2

        # Traces should be identical (except for timestamps and run_id)
        events1 = collector1.events
        events2 = collector2.events

        assert len(events1) == len(events2)

        for ev1, ev2 in zip(events1, events2):
            # Compare everything except timestamp and run_id
            assert ev1.step_id == ev2.step_id
            assert ev1.port == ev2.port
            assert ev1.frame_depth == ev2.frame_depth
            assert ev1.cp_depth == ev2.cp_depth
            assert ev1.goal_height == ev2.goal_height
            assert ev1.write_stamp == ev2.write_stamp
            assert ev1.pred_id == ev2.pred_id
            assert ev1.goal == ev2.goal

    def test_jsonl_byte_for_byte_equal_without_timestamps(self, tmp_path):
        """Test JSONL traces are byte-for-byte equal (timestamps not in output by default)."""
        clauses = [
            Clause(head=Struct("fact", (Int(1),)), body=()),
            Clause(head=Struct("fact", (Int(2),)), body=()),
            Clause(head=Struct("fact", (Int(3),)), body=()),
        ]
        program = Program(tuple(clauses))

        # Run 1 - create JSONL file
        trace_file1 = tmp_path / "trace1.jsonl"
        engine1 = Engine(program, trace=True)
        with open(trace_file1, "w") as f:
            sink1 = JSONLTraceSink(f)
            engine1.tracer.add_sink(sink1)
            list(engine1.query("fact(X)"))

        # Run 2 - should produce identical JSONL
        trace_file2 = tmp_path / "trace2.jsonl"
        engine2 = Engine(program, trace=True)
        with open(trace_file2, "w") as f:
            sink2 = JSONLTraceSink(f)
            engine2.tracer.add_sink(sink2)
            list(engine2.query("fact(X)"))

        # Files should be byte-for-byte identical (except for run_id)
        # Since run_id will differ, we need to parse and compare

        with open(trace_file1) as f:
            lines1 = [json.loads(line) for line in f]
        with open(trace_file2) as f:
            lines2 = [json.loads(line) for line in f]

        assert len(lines1) == len(lines2)

        # Compare everything except run_id
        for l1, l2 in zip(lines1, lines2):
            # Remove run_id for comparison
            l1.pop("rid", None)
            l2.pop("rid", None)
            assert l1 == l2

    def test_variable_names_deterministic(self):
        """Test that variable names are generated deterministically."""
        clauses = [
            Clause(
                head=Struct("append", (Atom("[]"), Var(0, "L"), Var(0, "L"))), body=()
            ),
            Clause(
                head=Struct(
                    "append",
                    (
                        Struct(".", (Var(0, "H"), Var(1, "T1"))),
                        Var(2, "L2"),
                        Struct(".", (Var(0, "H"), Var(3, "T3"))),
                    ),
                ),
                body=(Struct("append", (Var(1, "T1"), Var(2, "L2"), Var(3, "T3"))),),
            ),
        ]
        program = Program(tuple(clauses))

        # Multiple runs should use identical variable names
        runs = []
        for _ in range(3):
            engine = Engine(program, trace=True)
            collector = CollectorSink()
            engine.tracer.add_sink(collector)
            list(engine.query("append([1], [2], X)"))
            runs.append(collector.events)

        # All runs should have identical variable names in goals
        for i in range(1, len(runs)):
            assert len(runs[i]) == len(runs[0])
            for ev_curr, ev_first in zip(runs[i], runs[0]):
                # Goal representations should be identical
                assert str(ev_curr.goal) == str(ev_first.goal)

    @pytest.mark.skip(reason="Filtering not yet implemented")
    def test_step_ids_sequential_post_filter(self):
        """Test that step_ids are sequential after filtering."""
        # This test will be enabled when filtering is implemented
        pass

    def test_trace_identical_with_and_without_indexing(self):
        """Test 4-port trace is identical with/without indexing."""
        clauses = [
            Clause(head=Struct("color", (Atom("red"),)), body=()),
            Clause(head=Struct("color", (Atom("green"),)), body=()),
            Clause(head=Struct("color", (Atom("blue"),)), body=()),
            Clause(
                head=Struct("test", (Var(0, "X"),)),
                body=(Struct("color", (Var(0, "X"),)),),
            ),
        ]
        program = Program(tuple(clauses))

        # Run without indexing
        engine_no_idx = Engine(program, trace=True, use_indexing=False)
        collector_no_idx = CollectorSink()
        engine_no_idx.tracer.add_sink(collector_no_idx)
        results_no_idx = list(engine_no_idx.query("test(X)"))

        # Run with indexing
        engine_idx = Engine(program, trace=True, use_indexing=True)
        collector_idx = CollectorSink()
        engine_idx.tracer.add_sink(collector_idx)
        results_idx = list(engine_idx.query("test(X)"))

        # Results should be identical
        assert results_no_idx == results_idx

        # Extract just the 4-port events
        def get_port_events(events):
            return [
                (ev.port, ev.pred_id, str(ev.goal))
                for ev in events
                if ev.port in ["call", "exit", "redo", "fail"]
            ]

        ports_no_idx = get_port_events(collector_no_idx.events)
        ports_idx = get_port_events(collector_idx.events)

        # 4-port sequences should be identical
        assert ports_no_idx == ports_idx

    def test_determinism_with_cut(self):
        """Test trace determinism with cut operator."""
        clauses = [
            Clause(head=Struct("det", (Int(1),)), body=(Atom("!"),)),
            Clause(head=Struct("det", (Int(2),)), body=()),
            Clause(head=Struct("det", (Int(3),)), body=()),
        ]
        program = Program(tuple(clauses))

        # Multiple runs with cut should be identical
        traces = []
        for _ in range(3):
            engine = Engine(program, trace=True)
            collector = CollectorSink()
            engine.tracer.add_sink(collector)
            list(engine.query("det(X)"))
            traces.append(collector.events)

        # All traces should be identical (modulo timestamp/run_id)
        for trace in traces[1:]:
            assert len(trace) == len(traces[0])
            for ev_curr, ev_first in zip(trace, traces[0]):
                assert ev_curr.port == ev_first.port
                assert ev_curr.pred_id == ev_first.pred_id
                assert str(ev_curr.goal) == str(ev_first.goal)

    def test_determinism_with_if_then_else(self):
        """Test trace determinism with if-then-else."""
        clauses = [
            Clause(head=Struct("cond", (Int(1),)), body=()),
            Clause(
                head=Struct("test", (Var(0, "X"), Var(1, "Y"))),
                body=(
                    Struct(
                        ";",
                        (
                            Struct(
                                "->",
                                (
                                    Struct("cond", (Var(0, "X"),)),
                                    Struct("=", (Var(1, "Y"), Atom("yes"))),
                                ),
                            ),
                            Struct("=", (Var(1, "Y"), Atom("no"))),
                        ),
                    ),
                ),
            ),
        ]
        program = Program(tuple(clauses))

        # Run multiple times
        traces = []
        for _ in range(3):
            engine = Engine(program, trace=True)
            collector = CollectorSink()
            engine.tracer.add_sink(collector)
            # Test both branches
            list(engine.query("test(1, Y)"))  # then branch
            list(engine.query("test(2, Y)"))  # else branch
            traces.append(collector.events)

        # All should be identical
        for trace in traces[1:]:
            assert len(trace) == len(traces[0])
            for ev_curr, ev_first in zip(trace, traces[0]):
                assert ev_curr.port == ev_first.port
                assert ev_curr.pred_id == ev_first.pred_id

    def test_determinism_with_large_backtracking(self):
        """Test determinism with programs that generate many choicepoints."""
        # Generate a program with many facts
        clauses = []
        for i in range(20):
            clauses.append(Clause(head=Struct("num", (Int(i),)), body=()))

        # Add a rule that creates lots of backtracking
        clauses.append(
            Clause(
                head=Struct("pair", (Var(0, "X"), Var(1, "Y"))),
                body=(
                    Struct("num", (Var(0, "X"),)),
                    Struct("num", (Var(1, "Y"),)),
                    Struct("<", (Var(0, "X"), Var(1, "Y"))),
                ),
            )
        )
        program = Program(tuple(clauses))

        # Multiple runs should be deterministic
        traces = []
        for _ in range(2):  # Just 2 runs for large trace
            engine = Engine(program, trace=True)
            collector = CollectorSink()
            engine.tracer.add_sink(collector)
            results = list(engine.query("pair(X, Y)"))
            traces.append((results, collector.events))

        # Results and traces should be identical
        assert traces[0][0] == traces[1][0]
        assert len(traces[0][1]) == len(traces[1][1])

        # Verify port sequences match
        for ev0, ev1 in zip(traces[0][1], traces[1][1]):
            assert ev0.port == ev1.port
            assert ev0.pred_id == ev1.pred_id


class TestVariableNaming:
    """Test deterministic variable naming."""

    def test_renamed_variables_consistent(self):
        """Test that renamed variables are consistent across runs."""
        clauses = [
            Clause(
                head=Struct("rule", (Var(0, "X"), Var(1, "Y"))),
                body=(Struct("=", (Var(0, "X"), Var(1, "Y"))),),
            ),
        ]
        program = Program(tuple(clauses))

        # Multiple engines should rename variables identically
        renamings = []
        for _ in range(5):
            engine = Engine(program)
            # The renamer should produce identical names
            results = list(engine.query("rule(A, B)"))
            renamings.append(results)

        # All renamings should be identical
        for renamed in renamings[1:]:
            assert renamed == renamings[0]

    def test_fresh_variable_generation_deterministic(self):
        """Test that fresh variable generation is deterministic."""
        # Program that generates fresh variables internally
        clauses = [
            Clause(
                head=Struct("gen", (Var(0, "X"),)),
                body=(Struct("=", (Var(0, "X"), Struct("f", (Var(1, "_"),)))),),
            ),
        ]
        program = Program(tuple(clauses))

        # Multiple runs should generate variables identically
        results_list = []
        for _ in range(3):
            engine = Engine(program)
            results = list(engine.query("gen(X)"))
            results_list.append(results)

        # All should be identical
        for results in results_list[1:]:
            assert results == results_list[0]
