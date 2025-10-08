"""
Tests for dispatch module extraction from Engine.

These tests ensure that extracting dispatch method bodies to a dedicated
dispatch.py module preserves exact behavior including:
- Goal dispatch behavior
- Port emission ordering
- Choicepoint management
- Frame management
- Error handling
- Metrics recording
- Tracer integration

Following TDD approach - these tests define the contract for the extraction.
"""

import pytest
import inspect
from prolog.ast.terms import Atom, Struct, Var
from prolog.engine.engine import Engine
from prolog.engine.errors import UndefinedPredicateError
from prolog.tests.helpers import mk_fact, mk_rule, program
from unittest.mock import Mock, patch


class TestDispatchExtraction:
    """Test dispatch behavior before and after extraction."""

    def setup_method(self):
        """Set up test environment."""
        # Start with empty program - tests will override as needed
        empty_prog = program()
        self.engine = Engine(empty_prog)

    def test_dispatch_predicate_basic_behavior(self):
        """Test basic predicate dispatch behavior."""
        # Define a simple predicate: test_pred(X) :- var(X).
        prog = program(
            mk_rule("test_pred", (Var(0, "X"),), Struct("var", (Var(0, "X"),)))
        )
        self.engine = Engine(prog)

        # Create goal
        goal_term = Struct("test_pred", (Var(0, "X"),))

        # Mock tracer to capture behavior
        with (
            patch.object(self.engine, "_port") as mock_port,
            patch.object(self.engine, "_trace_port") as mock_trace,
        ):

            # Execute goal through solve
            solutions = list(self.engine.solve(goal_term))

            # Verify we get expected solution
            assert len(solutions) == 1

            # Verify port emissions occurred
            assert mock_port.called
            assert mock_trace.called

            # Verify CALL ports were emitted (one for predicate, one for builtin)
            call_calls = [
                call for call in mock_port.call_args_list if call[0][0] == "CALL"
            ]
            assert (
                len(call_calls) >= 1
            ), "Should emit CALL ports for predicate and builtin calls"

            # Check that test_pred/1 CALL was emitted
            pred_calls = [call for call in call_calls if call[0][1] == "test_pred/1"]
            assert (
                len(pred_calls) == 1
            ), "Should emit CALL exactly once for predicate dispatch"

    def test_dispatch_predicate_with_choicepoints(self):
        """Test predicate dispatch with multiple clauses."""
        prog = program(
            mk_fact("choice_test", Atom("a")),
            mk_fact("choice_test", Atom("b")),
            mk_fact("choice_test", Atom("c")),
        )
        self.engine = Engine(prog)

        goal_term = Struct("choice_test", (Var(0, "X"),))
        solutions = list(self.engine.solve(goal_term))

        # Should find exactly 3 solutions
        assert len(solutions) == 3

    def test_dispatch_choicepoint_events(self):
        """Test choicepoint events are emitted correctly."""
        prog = program(
            mk_fact("choice_test", Atom("a")),
            mk_fact("choice_test", Atom("b")),
        )
        self.engine = Engine(prog)

        # Mock tracer to capture events
        mock_tracer = Mock()
        self.engine.tracer = mock_tracer

        goal_term = Struct("choice_test", (Var(0, "X"),))
        list(self.engine.solve(goal_term))

        # Verify choicepoint push events
        cp_push_calls = [
            call
            for call in mock_tracer.emit_internal_event.call_args_list
            if call[0][0] == "cp_push"
        ]
        assert len(cp_push_calls) >= 1, "Should emit cp_push for multiple clauses"

    def test_dispatch_builtin_behavior(self):
        """Test builtin dispatch behavior."""
        goal_term = Struct("var", (Var(0, "X"),))

        with patch.object(self.engine, "_port") as mock_port:
            result = list(self.engine.solve(goal_term))

            # var/1 should succeed for unbound variable
            assert len(result) == 1

            # Verify CALL and EXIT ports were emitted
            port_calls = [call[0][0] for call in mock_port.call_args_list]
            assert "CALL" in port_calls
            assert "EXIT" in port_calls

    def test_dispatch_builtin_failure(self):
        """Test builtin dispatch failure behavior."""
        # var/1 should fail for bound variable
        goal_term = Struct("var", (Atom("bound"),))

        with patch.object(self.engine, "_port") as mock_port:
            result = list(self.engine.solve(goal_term))

            # Should get no solutions
            assert len(result) == 0

            # Verify CALL and FAIL ports were emitted
            port_calls = [call[0][0] for call in mock_port.call_args_list]
            assert "CALL" in port_calls
            assert "FAIL" in port_calls

    def test_dispatch_conjunction_behavior(self):
        """Test conjunction dispatch behavior."""
        # test_conj :- var(X), var(Y).
        prog = program(
            mk_rule(
                "test_conj",
                (),
                Struct(
                    ",", (Struct("var", (Var(0, "X"),)), Struct("var", (Var(1, "Y"),)))
                ),
            )
        )
        self.engine = Engine(prog)

        goal_term = Atom("test_conj")

        # Should succeed (both var/1 calls succeed)
        result = list(self.engine.solve(goal_term))
        assert len(result) == 1

    def test_dispatch_conjunction_failure_first(self):
        """Test conjunction fails when first goal fails."""
        # test_conj :- atom(a), var(X). - First goal fails: a is bound
        prog = program(
            mk_rule(
                "test_conj",
                (),
                Struct(
                    ",",
                    (
                        Struct("atom", (Var(0, "X"),)),  # Fails: X unbound
                        Struct("var", (Var(1, "Y"),)),
                    ),
                ),
            )
        )
        self.engine = Engine(prog)

        goal_term = Atom("test_conj")
        result = list(self.engine.solve(goal_term))
        assert len(result) == 0, "Conjunction should fail when first goal fails"

    def test_dispatch_conjunction_failure_second(self):
        """Test conjunction fails when second goal fails."""
        # test_conj :- var(X), nonvar(X). - Second goal fails: X is still a var
        prog = program(
            mk_rule(
                "test_conj",
                (),
                Struct(
                    ",",
                    (Struct("var", (Var(0, "X"),)), Struct("nonvar", (Var(0, "X"),))),
                ),
            )  # Fails: X is still var
        )
        self.engine = Engine(prog)

        goal_term = Atom("test_conj")
        result = list(self.engine.solve(goal_term))
        assert len(result) == 0, "Conjunction should fail when second goal fails"

    def test_dispatch_disjunction_behavior(self):
        """Test disjunction dispatch behavior - both branches should produce solutions."""
        # test_disj :- var(X) ; var(Y).
        prog = program(
            mk_rule(
                "test_disj",
                (),
                Struct(
                    ";", (Struct("var", (Var(0, "X"),)), Struct("var", (Var(1, "Y"),)))
                ),
            )
        )
        self.engine = Engine(prog)

        goal_term = Atom("test_disj")
        result = list(self.engine.solve(goal_term))

        # Disjunction should produce solutions from both branches
        assert len(result) == 2, "Both branches of disjunction should succeed"

    def test_dispatch_disjunction_events(self):
        """Test disjunction choicepoint events."""
        # test_disj :- var(X) ; var(Y).
        prog = program(
            mk_rule(
                "test_disj",
                (),
                Struct(
                    ";", (Struct("var", (Var(0, "X"),)), Struct("var", (Var(1, "Y"),)))
                ),
            )
        )
        self.engine = Engine(prog)

        # Mock tracer to capture events
        mock_tracer = Mock()
        self.engine.tracer = mock_tracer

        goal_term = Atom("test_disj")
        list(self.engine.solve(goal_term))

        # Verify choicepoint was created for disjunction
        cp_events = [
            call
            for call in mock_tracer.emit_internal_event.call_args_list
            if call[0][0] == "cp_push"
        ]
        # Should have choicepoint for disjunction
        assert any("disjunction" in str(call) for call in cp_events)

    def test_dispatch_if_then_else_behavior(self):
        """Test if-then-else dispatch behavior."""
        # test_ite :- (var(X) -> atom(a) ; atom(b)).
        prog = program(
            mk_rule(
                "test_ite",
                (),
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Struct("var", (Var(0, "X"),)),
                                Struct("atom", (Atom("a"),)),
                            ),
                        ),
                        Struct("atom", (Atom("b"),)),
                    ),
                ),
            )
        )
        self.engine = Engine(prog)

        goal_term = Atom("test_ite")
        result = list(self.engine.solve(goal_term))

        # Should succeed through then branch
        assert len(result) == 1

    def test_dispatch_if_then_else_events(self):
        """Test if-then-else choicepoint events."""
        # test_ite :- (var(X) -> atom(a) ; atom(b)).
        prog = program(
            mk_rule(
                "test_ite",
                (),
                Struct(
                    ";",
                    (
                        Struct(
                            "->",
                            (
                                Struct("var", (Var(0, "X"),)),
                                Struct("atom", (Atom("a"),)),
                            ),
                        ),
                        Struct("atom", (Atom("b"),)),
                    ),
                ),
            )
        )
        self.engine = Engine(prog)

        # Mock tracer to capture events
        mock_tracer = Mock()
        self.engine.tracer = mock_tracer

        goal_term = Atom("test_ite")
        list(self.engine.solve(goal_term))

        # Verify if-then-else choicepoint was created
        cp_events = [
            call
            for call in mock_tracer.emit_internal_event.call_args_list
            if call[0][0] == "cp_push"
        ]
        assert any("if_then_else" in str(call) for call in cp_events)

    def test_dispatch_cut_behavior(self):
        """Test cut dispatch behavior."""
        prog = program(
            mk_rule("test_cut", (Atom("a"),), Atom("!")),
            mk_fact("test_cut", Atom("b")),
            mk_fact("test_cut", Atom("c")),
        )
        self.engine = Engine(prog)

        goal_term = Struct("test_cut", (Var(0, "X"),))
        result = list(self.engine.solve(goal_term))

        # Cut should prevent backtracking, only get first solution
        assert len(result) == 1, "Cut should prevent backtracking to other solutions"
        assert result[0]["X"] == Atom("a")

    def test_dispatch_cut_events(self):
        """Test cut event emission."""
        prog = program(
            mk_rule("test_cut", (Atom("a"),), Atom("!")),
            mk_fact("test_cut", Atom("b")),
        )
        self.engine = Engine(prog)

        # Mock tracer to capture events
        mock_tracer = Mock()
        self.engine.tracer = mock_tracer

        goal_term = Struct("test_cut", (Var(0, "X"),))
        list(self.engine.solve(goal_term))

        # Verify cut event was emitted
        cut_events = [
            call
            for call in mock_tracer.emit_internal_event.call_args_list
            if call[0][0] == "cut_commit"
        ]
        assert len(cut_events) >= 1, "Should emit cut_commit event"

    def test_dispatch_metrics_integration(self):
        """Test that dispatch preserves metrics recording."""
        # Create program with a predicate that will trigger metrics
        prog = program(mk_fact("test_metrics", Atom("a")))
        self.engine = Engine(prog)
        self.engine.metrics = Mock()

        goal_term = Struct("test_metrics", (Atom("a"),))
        list(self.engine.solve(goal_term))

        # Verify metrics were recorded for predicate dispatch
        assert self.engine.metrics.record_call.called

    def test_dispatch_error_handling(self):
        """Test dispatch error handling behavior."""
        # Test undefined predicate in ISO mode
        self.engine.mode = "iso"
        goal_term = Struct("undefined_pred", (Atom("x"),))

        with pytest.raises(UndefinedPredicateError):
            list(self.engine.solve(goal_term))

    def test_dispatch_port_emission_order(self):
        """Test that port emission order is preserved."""
        # test_ports(X) :- var(X).
        prog = program(
            mk_rule("test_ports", (Var(0, "X"),), Struct("var", (Var(0, "X"),)))
        )
        self.engine = Engine(prog)

        goal_term = Struct("test_ports", (Var(0, "X"),))

        with patch.object(self.engine, "_port") as mock_port:
            list(self.engine.solve(goal_term))

            # Extract port sequence
            port_sequence = [call[0][0] for call in mock_port.call_args_list]

            # Should see CALL, then EXIT for successful execution
            call_idx = port_sequence.index("CALL")
            exit_indices = [i for i, port in enumerate(port_sequence) if port == "EXIT"]

            # At least one EXIT should come after CALL
            assert any(exit_idx > call_idx for exit_idx in exit_indices)

    def test_dispatch_frame_management(self):
        """Test that frame management is preserved."""
        # test_frame(X) :- var(X).
        prog = program(
            mk_rule("test_frame", (Var(0, "X"),), Struct("var", (Var(0, "X"),)))
        )
        self.engine = Engine(prog)

        goal_term = Struct("test_frame", (Var(0, "X"),))

        with patch.object(self.engine, "tracer") as mock_tracer:
            list(self.engine.solve(goal_term))

            # Verify frame push/pop events if tracer available
            if mock_tracer and hasattr(mock_tracer, "emit_internal_event"):
                frame_events = [
                    call[0][0]
                    for call in mock_tracer.emit_internal_event.call_args_list
                ]
                # Should have frame lifecycle events
                assert any("frame" in event for event in frame_events)

    def test_dispatch_goal_stack_management(self):
        """Test goal stack height tracking during dispatch."""
        # test_stack :- var(X), var(Y).
        prog = program(
            mk_rule(
                "test_stack",
                (),
                Struct(
                    ",", (Struct("var", (Var(0, "X"),)), Struct("var", (Var(1, "Y"),)))
                ),
            )
        )
        self.engine = Engine(prog)

        goal_term = Atom("test_stack")

        # Track goal stack heights during execution
        initial_height = self.engine.goal_stack.height()

        result = list(self.engine.solve(goal_term))

        # Should succeed and restore goal stack
        assert len(result) == 1

        # Goal stack should be back to initial state after completion
        final_height = self.engine.goal_stack.height()
        assert final_height == initial_height

    def test_dispatch_trail_management(self):
        """Test trail management during dispatch."""
        # test_trail(X) :- X = bound.
        prog = program(
            mk_rule(
                "test_trail", (Var(0, "X"),), Struct("=", (Var(0, "X"), Atom("bound")))
            )
        )
        self.engine = Engine(prog)

        goal_term = Struct("test_trail", (Var(0, "X"),))

        result = list(self.engine.solve(goal_term))

        # Should succeed with X bound
        assert len(result) == 1
        assert result[0]["X"] == Atom("bound")

        # Trail should have grown during execution
        # (actual trail state after completion depends on implementation)

    def test_dispatch_signature_preservation(self):
        """Test that all dispatch methods maintain expected signatures."""

        # These should all be callable without errors (even if they fail)
        assert hasattr(self.engine, "_dispatch_predicate")
        assert hasattr(self.engine, "_dispatch_builtin")
        assert hasattr(self.engine, "_dispatch_conjunction")
        assert hasattr(self.engine, "_dispatch_disjunction")
        assert hasattr(self.engine, "_dispatch_if_then_else")
        assert hasattr(self.engine, "_dispatch_cut")

        # Verify they have expected signature patterns (inspect without calling)
        pred_sig = inspect.signature(self.engine._dispatch_predicate)
        assert (
            len(pred_sig.parameters) == 3
        ), "Should have 3 parameters: goal, call_depth, call_emitted (self excluded by inspect)"

        builtin_sig = inspect.signature(self.engine._dispatch_builtin)
        assert (
            len(builtin_sig.parameters) == 3
        ), "Should have 3 parameters: goal, call_depth, call_emitted (self excluded by inspect)"


class TestDispatchModuleBehavior:
    """Tests for behavior that must be preserved after extraction to dispatch.py"""

    def setup_method(self):
        """Set up test environment."""
        # Start with empty program - tests will override as needed
        empty_prog = program()
        self.engine = Engine(empty_prog)

    def test_dispatch_module_import(self):
        """Test that dispatch module can be imported after extraction."""
        # This test will verify the module exists after extraction
        # For now, just ensure the Engine still works
        goal_term = Struct("var", (Var(0, "X"),))
        result = list(self.engine.solve(goal_term))
        assert len(result) == 1

    @pytest.mark.skip(reason="TODO: Implement after extraction")
    def test_dispatch_function_signatures(self):
        """Test expected signatures for extracted dispatch functions."""
        # After extraction, these functions should exist:
        # - dispatch_predicate(engine, goal, depth, call_emitted)
        # - dispatch_builtin(engine, goal, depth, call_emitted)
        # - dispatch_conjunction(engine, goal, depth, call_emitted)
        # - dispatch_disjunction(engine, goal, depth, call_emitted)
        # - dispatch_if_then_else(engine, goal, depth, call_emitted)
        # - dispatch_cut(engine, goal, depth, call_emitted)

        # This test documents the expected API after extraction
        pass

    @pytest.mark.skip(reason="TODO: Implement after extraction")
    def test_engine_delegation_behavior(self):
        """Test that Engine methods become thin delegates after extraction."""
        # After extraction, Engine._dispatch_* methods should delegate to dispatch module
        # This test will verify the delegation preserves behavior exactly
        pass


if __name__ == "__main__":
    pytest.main([__file__])
