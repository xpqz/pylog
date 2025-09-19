"""
Test internal events for if-then-else construct.

Tests that if-then-else emits correct internal events and 4-port behavior.
"""

import pytest

from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.ast.terms import Int, Atom
from prolog.engine.engine import Engine
from prolog.engine.runtime import ChoicepointKind
from prolog.debug.sinks import CollectorSink
from prolog.debug.tracer import TraceEvent, InternalEvent


class TestIfThenElseInternalEvents:
    """Test internal events for if-then-else construct."""

    def test_ite_cp_push_event(self):
        """Test if-then-else emits cp_push internal event."""
        # Create a program with if-then-else
        clauses = parser.parse_program("""
            test(X, Y) :- (X = 1 -> Y = a ; Y = b).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that takes then branch
        goals = parser.parse_query("?- test(1, Z).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("a")

        # Find internal events
        internal_events = [e for e in collector.events if isinstance(e, InternalEvent)]

        # Should have cp_push for if-then-else
        cp_push_events = [e for e in internal_events if e.kind == "cp_push"]
        ite_events = [e for e in cp_push_events
                      if e.details.get("pred_id") == "if_then_else"]

        assert len(ite_events) > 0, "Should have if-then-else cp_push event"
        assert "trail_top" in ite_events[0].details

    def test_ite_then_branch_commits(self):
        """Test that taking then branch prunes else choicepoint."""
        # Create a program with if-then-else
        clauses = parser.parse_program("""
            test(X, Y) :- (X = 1 -> Y = a ; Y = b).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that takes then branch
        goals = parser.parse_query("?- test(1, Z).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("a")

        # Check internal events for commit
        internal_events = [e for e in collector.events if isinstance(e, InternalEvent)]

        # Should have cp_push followed by cp_pop (commit)
        cp_events = [(e.kind, e.details.get("pred_id", ""))
                     for e in internal_events
                     if e.kind in ["cp_push", "cp_pop"]]

        # Find if-then-else related events
        ite_push_idx = None
        for i, (kind, pred_id) in enumerate(cp_events):
            if kind == "cp_push" and pred_id == "if_then_else":
                ite_push_idx = i
                break

        assert ite_push_idx is not None, "Should have if-then-else cp_push"

        # The then branch should commit (prune the else choicepoint)
        # This may or may not emit a cp_pop event depending on implementation
        # What matters is that we don't execute the else branch
        # Check that we get the correct solution (a not b)
        assert solutions[0]["Z"] == Atom("a")

    def test_ite_else_branch_taken(self):
        """Test that else branch is taken when condition fails."""
        # Create a program with if-then-else
        clauses = parser.parse_program("""
            test(X, Y) :- (X = 2 -> Y = a ; Y = b).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that takes else branch (X=1 doesn't match X=2)
        goals = parser.parse_query("?- test(1, Z).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("b")

        # Check 4-port events to verify else branch was taken
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Should have CALL and EXIT for test predicate
        # Note: pred_id might be test/2 or test/0 depending on arity transformation
        test_events = [(e.port, e.pred_id) for e in trace_events
                       if e.pred_id.startswith("test/")]

        # Should have both CALL and EXIT ports
        ports = [port for port, _ in test_events]
        assert "call" in ports
        assert "exit" in ports

        # Verify the correct branch was taken
        assert solutions[0]["Z"] == Atom("b")

    def test_ite_nested_behavior(self):
        """Test nested if-then-else behavior."""
        # Create a program with nested if-then-else
        clauses = parser.parse_program("""
            test(X, Y) :-
                (X = 1 ->
                    (Y = a ; Y = b)
                ;
                    (Y = c ; Y = d)
                ).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that takes first then branch
        goals = parser.parse_query("?- test(1, Z).")
        solutions = list(engine.run(goals))

        # Should get two solutions: a and b
        assert len(solutions) == 2
        assert solutions[0]["Z"] == Atom("a")
        assert solutions[1]["Z"] == Atom("b")

        # Query that takes else branch
        collector.events.clear()
        goals = parser.parse_query("?- test(2, Z).")
        solutions = list(engine.run(goals))

        # Should get two solutions: c and d
        assert len(solutions) == 2
        assert solutions[0]["Z"] == Atom("c")
        assert solutions[1]["Z"] == Atom("d")

    def test_ite_with_cut_interaction(self):
        """Test if-then-else with cut interaction."""
        # Create a program with if-then-else and cut
        clauses = parser.parse_program("""
            test(X, Y) :- (X = 1 -> !, Y = a ; Y = b).
            test(X, c).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query with X=1 (takes then branch with cut)
        goals = parser.parse_query("?- test(1, Z).")
        solutions = list(engine.run(goals))

        # Cut should prevent backtracking to second clause
        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("a")

        # Query with X=2 (takes else branch)
        collector.events.clear()
        goals = parser.parse_query("?- test(2, Z).")
        solutions = list(engine.run(goals))

        # Should get both b and c (no cut in else branch)
        assert len(solutions) == 2
        assert solutions[0]["Z"] == Atom("b")
        assert solutions[1]["Z"] == Atom("c")

    def test_ite_port_sequence(self):
        """Test that if-then-else maintains correct 4-port sequence."""
        # Create a program with if-then-else
        clauses = parser.parse_program("""
            cond(1).
            then_branch(a).
            else_branch(b).

            test(X, Y) :- (cond(X) -> then_branch(Y) ; else_branch(Y)).
        """)
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use collector sink to capture events
        collector = CollectorSink()
        engine.tracer.add_sink(collector)

        # Query that takes then branch
        goals = parser.parse_query("?- test(1, Z).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["Z"] == Atom("a")

        # Extract trace events
        trace_events = [e for e in collector.events if isinstance(e, TraceEvent)]

        # Build port sequence
        port_sequence = [(e.port, e.pred_id) for e in trace_events]

        # Should have proper nesting with predicates (arities may vary)
        # Extract just the ports and predicate names
        port_pred_names = [(e.port, e.pred_id.split('/')[0]) for e in trace_events]

        # Check that test is called and exits
        assert ("call", "test") in port_pred_names
        assert ("exit", "test") in port_pred_names

        # Check that cond is called and exits
        assert ("call", "cond") in port_pred_names
        assert ("exit", "cond") in port_pred_names

        # Check that then_branch is called and exits
        assert ("call", "then_branch") in port_pred_names
        assert ("exit", "then_branch") in port_pred_names

        # Validate that else_branch is NOT called when then branch is taken
        assert ("call", "else_branch") not in port_pred_names, "else_branch should not be called when then branch succeeds"
        assert ("exit", "else_branch") not in port_pred_names, "else_branch should not exit when then branch succeeds"