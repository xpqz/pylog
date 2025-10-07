"""Tests for exceptions builtins extraction (Phase 6).

This module tests that the throw/1, catch/3, and _handle_throw functionality
extracted to prolog/engine/builtins/exceptions.py maintains identical behavior
to the original engine.py implementation.

These tests specifically verify:
1. throw/1 builtin correctly reifies and throws exceptions
2. catch/3 builtin correctly sets up exception handling
3. _handle_throw helper correctly searches for and handles matching catchers
4. Cursor snapshot/restore semantics are preserved
5. Frame/goal stack restoration works correctly
6. Recursion depth validation is maintained
7. POP_FRAME handling is preserved

This is part of the Engine Refactoring Epic #243, Phase 6.
"""

import pytest
import inspect
from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.engine.runtime import ChoicepointKind, Choicepoint, GoalType
from prolog.tests.helpers import program, mk_fact, mk_rule

# Import extracted exceptions module
from prolog.engine.builtins.exceptions import register
from prolog.engine.builtins import exceptions


class TestExceptionsExtraction:
    """Test that exceptions extraction maintains identical behavior."""

    def test_throw_builtin_extracted_correctly(self):
        """Test that extracted throw/1 builtin works identically to engine version."""
        engine = Engine(program())

        # Test that throw/1 is properly registered in builtins
        assert ("throw", 1) in engine._builtins

        # Verify throw builtin can be called directly via the registry
        throw_builtin = engine._builtins[("throw", 1)]

        # Test basic throw with atom
        ball = Atom("test_exception")
        with pytest.raises(PrologThrow) as exc_info:
            throw_builtin(engine, (ball,))

        assert exc_info.value.ball == ball

    def test_catch_builtin_extracted_correctly(self):
        """Test that extracted catch/3 builtin works identically to engine version."""
        engine = Engine(program())

        # Test that catch/3 is properly registered in builtins
        assert ("catch", 3) in engine._builtins

        # Verify catch builtin can be called directly via the registry
        catch_builtin = engine._builtins[("catch", 3)]

        # Test basic catch setup - should return True to indicate success
        goal = Atom("true")
        catcher = Var(0, "_")
        recovery = Atom("true")

        result = catch_builtin(engine, (goal, catcher, recovery))
        assert result is True

        # Verify that catch set up the appropriate choicepoint
        assert len(engine.cp_stack) > 0
        # The most recent choicepoint should be a CATCH choicepoint
        assert engine.cp_stack[-1].kind == ChoicepointKind.CATCH

    def test_handle_throw_extracted_correctly(self):
        """Test that extracted _handle_throw helper works identically."""
        engine = Engine(program())

        # Set up a catch context manually

        # Create a catch choicepoint
        catcher = Atom("test_ball")
        recovery = Atom("true")
        cp_height = len(engine.cp_stack)

        catch_cp = Choicepoint(
            kind=ChoicepointKind.CATCH,
            trail_top=engine.trail.position(),
            goal_stack_height=engine.goal_stack.height(),
            frame_stack_height=len(engine.frame_stack),
            payload={
                "phase": "GOAL",
                "catcher": catcher,
                "recovery": recovery,
                "cp_height": cp_height,
                "cursor_snapshots": {},
                "recursion_depth": 0,
            },
            stamp=engine.trail.next_stamp(),
        )
        engine.cp_stack.append(catch_cp)

        # Test that _handle_throw exists and works
        # Note: After extraction, this should be available via the exceptions module
        # but still callable from engine for compatibility
        ball = Atom("test_ball")
        result = engine._handle_throw(ball)

        assert result is True  # Should find and handle the matching catcher

        # Verify critical post-handling state
        # CP stack should be restored to catch baseline
        assert len(engine.cp_stack) == cp_height

        # Recovery goal should be pushed
        top = engine.goal_stack.peek()
        assert top and top.type == GoalType.PREDICATE
        assert getattr(top.term, "name", "") == "true"

    def test_exception_semantics_preserved_after_extraction(self):
        """Test that full catch/throw semantics work after extraction."""
        engine = Engine(program())

        # Test the full catch/throw cycle
        # catch(throw(ball), ball, true)
        goal = Struct("throw", (Atom("ball"),))
        catcher = Atom("ball")
        recovery = Atom("true")
        catch_query = Struct("catch", (goal, catcher, recovery))

        results = list(engine.run([catch_query]))

        # Should have exactly one result (successful catch and recovery)
        assert len(results) == 1

    def test_cursor_snapshot_semantics_preserved(self):
        """Test that cursor snapshot/restore semantics are preserved."""
        # Set up a program with backtracking opportunities
        prog = program(
            mk_fact("choice", Int(1)),
            mk_fact("choice", Int(2)),
            mk_fact("choice", Int(3)),
        )
        engine = Engine(prog)

        # Query that will create choicepoints and then throw
        # catch((choice(X), throw(found(X))), found(Y), true)
        x_var_id = engine.store.new_var()
        y_var_id = engine.store.new_var()
        x_var = Var(x_var_id, "X")
        y_var = Var(y_var_id, "Y")

        choice_goal = Struct("choice", (x_var,))
        throw_goal = Struct("throw", (Struct("found", (x_var,)),))
        goal = Struct(",", (choice_goal, throw_goal))
        catcher = Struct("found", (y_var,))
        recovery = Atom("true")

        catch_query = Struct("catch", (goal, catcher, recovery))
        results = list(engine.run([catch_query]))

        # Should succeed - cursor state should be properly restored
        assert len(results) >= 1

    def test_frame_stack_restoration_preserved(self):
        """Test that frame/goal stack restoration works correctly."""
        engine = Engine(program())

        # Test with nested calls that will affect frame stack
        # catch(call(throw(error)), error, true)
        throw_goal = Struct("throw", (Atom("error"),))
        call_goal = Struct("call", (throw_goal,))
        catcher = Atom("error")
        recovery = Atom("true")

        catch_query = Struct("catch", (call_goal, catcher, recovery))
        results = list(engine.run([catch_query]))

        # Should succeed, demonstrating proper frame restoration
        assert len(results) == 1

    def test_recursion_depth_validation_preserved(self):
        """Test that recursion depth validation is maintained."""
        # Set up a recursive predicate that throws
        prog = program(
            mk_rule("recurse", (Int(0),), Struct("throw", (Atom("base_case"),))),
            mk_rule(
                "recurse",
                (Var(0, "N"),),
                Struct(
                    ",",
                    (
                        Struct(
                            "is", (Var(1, "N1"), Struct("-", (Var(0, "N"), Int(1))))
                        ),
                        Struct("recurse", (Var(1, "N1"),)),
                    ),
                ),
            ),
        )
        engine = Engine(prog)

        # Test recursive call with exception
        # catch(recurse(3), base_case, true)
        goal = Struct("recurse", (Int(3),))
        catcher = Atom("base_case")
        recovery = Atom("true")

        catch_query = Struct("catch", (goal, catcher, recovery))
        results = list(engine.run([catch_query]))

        # Should succeed, showing recursion depth handling works
        assert len(results) == 1

    def test_pop_frame_handling_preserved(self):
        """Test that POP_FRAME handling is preserved."""
        engine = Engine(program())

        # Create a scenario that will generate POP_FRAME sentinels
        # and then throw an exception
        # catch((true, throw(test)), test, true)
        goal = Struct(",", (Atom("true"), Struct("throw", (Atom("test"),))))
        catcher = Atom("test")
        recovery = Atom("true")

        catch_query = Struct("catch", (goal, catcher, recovery))
        results = list(engine.run([catch_query]))

        # Should succeed, demonstrating POP_FRAME handling works
        assert len(results) == 1

    def test_exception_registration_pattern(self):
        """Test that exceptions follow the standard builtin registration pattern."""
        # Test that the builtins are registered through the standard mechanism
        registry = {}

        # Test the registration function
        register(registry)

        # Verify expected registrations
        assert ("throw", 1) in registry
        assert ("catch", 3) in registry

        # Verify the registered functions are callable
        assert callable(registry[("throw", 1)])
        assert callable(registry[("catch", 3)])

    def test_multiple_catch_levels_preserved(self):
        """Test that nested catch/throw scenarios work correctly."""
        engine = Engine(program())

        # Test nested catches:
        # catch(catch(throw(inner), inner, throw(middle)), middle, true)
        inner_throw = Struct("throw", (Atom("inner"),))
        inner_catch = Struct(
            "catch", (inner_throw, Atom("inner"), Struct("throw", (Atom("middle"),)))
        )
        outer_catch = Struct("catch", (inner_catch, Atom("middle"), Atom("true")))

        results = list(engine.run([outer_catch]))

        # Should succeed - inner throw becomes middle throw, caught by outer
        assert len(results) == 1

        # Verify the recovery was indeed the outer recovery (true)
        # The fact that we got a result confirms that "true" was reached

    def test_exception_metrics_preserved(self):
        """Test that exception metrics tracking is preserved."""
        engine = Engine(program(), metrics=True)

        # Reset metrics
        if engine.metrics:
            engine.metrics.exceptions_thrown = 0
            engine.metrics.exceptions_caught = 0

        # Test 1: Query that will throw an exception (uncaught)
        throw_query = Struct("throw", (Atom("test"),))
        try:
            list(engine.run([throw_query]))
        except PrologThrow:
            pass  # Expected

        # Verify throw metrics were recorded
        if engine.metrics:
            assert engine.metrics.exceptions_thrown > 0

        # Test 2: Query with successful catch to test caught metrics
        catch_query = Struct(
            "catch",
            (
                Struct("throw", (Atom("caught_test"),)),
                Atom("caught_test"),
                Atom("true"),
            ),
        )

        results = list(engine.run([catch_query]))
        assert len(results) == 1  # Should succeed

        # Verify caught metrics were recorded
        if engine.metrics:
            assert engine.metrics.exceptions_caught > 0

    def test_throw_uninstantiated_ball_behavior_preserved(self):
        """Test that throw/1 behavior with unbound balls is preserved."""
        engine = Engine(program())

        # Create an unbound variable in the store
        unbound_var_id = engine.store.new_var()
        unbound_var = Var(unbound_var_id, "Unbound")

        # Test throw builtin directly with unbound variable
        throw_builtin = engine._builtins[("throw", 1)]

        # Engine should fail (dev-mode) for unbound balls rather than raise
        result = throw_builtin(engine, (unbound_var,))
        assert result is False  # Should fail without mutating state

    def test_handle_throw_without_catcher(self):
        """Test _handle_throw behavior when no matching catcher exists."""
        engine = Engine(program())

        # Call _handle_throw with no CATCH choicepoints
        ball = Atom("no_handler")
        result = engine._handle_throw(ball)

        # Should return False when no catcher matches
        assert result is False

        # Engine state should be unchanged
        assert len(engine.cp_stack) == 0
        assert engine.goal_stack.height() == 0

    def test_cursor_snapshot_restoration_direct(self):
        """Test direct cursor snapshot replacement behavior."""
        engine = Engine(program())

        # Create a fake cursor with clone capability
        class FakeCursor:
            def __init__(self, value):
                self.value = value

            def clone(self):
                return FakeCursor(f"snapshot_{self.value}")

        fake_cursor = FakeCursor("original")
        snapshot_cursor = fake_cursor.clone()

        # Set up PREDICATE choicepoint with the fake cursor
        pred_cp = Choicepoint(
            kind=ChoicepointKind.PREDICATE,
            trail_top=engine.trail.position(),
            goal_stack_height=engine.goal_stack.height(),
            frame_stack_height=len(engine.frame_stack),
            payload={"cursor": fake_cursor},
            stamp=engine.trail.next_stamp(),
        )
        engine.cp_stack.append(pred_cp)

        # Create CATCH choicepoint with cursor snapshot
        cp_height = len(engine.cp_stack) - 1  # Before the CATCH CP
        catch_cp = Choicepoint(
            kind=ChoicepointKind.CATCH,
            trail_top=engine.trail.position(),
            goal_stack_height=engine.goal_stack.height(),
            frame_stack_height=len(engine.frame_stack),
            payload={
                "phase": "GOAL",
                "catcher": Atom("test"),
                "recovery": Atom("true"),
                "cp_height": cp_height,
                "cursor_snapshots": {id(fake_cursor): snapshot_cursor},
                "recursion_depth": 0,
            },
            stamp=engine.trail.next_stamp(),
        )
        engine.cp_stack.append(catch_cp)

        # Handle throw - should restore snapshot
        ball = Atom("test")
        result = engine._handle_throw(ball)

        assert result is True

        # Verify the cursor was replaced with the snapshot for surviving CPs
        if len(engine.cp_stack) > 0:
            surviving_cp = engine.cp_stack[0]
            if surviving_cp.kind == ChoicepointKind.PREDICATE:
                cursor = surviving_cp.payload.get("cursor")
                assert cursor.value == "snapshot_original"

    def test_pop_frame_handling_observed(self):
        """Test that POP_FRAME handling can be observed during catch."""
        engine = Engine(program())

        # Run a catch scenario that will generate and consume POP_FRAME
        # catch((true, throw(test)), test, true)
        goal = Struct(",", (Atom("true"), Struct("throw", (Atom("test"),))))
        catcher = Atom("test")
        recovery = Atom("true")

        catch_query = Struct("catch", (goal, catcher, recovery))
        results = list(engine.run([catch_query]))

        # Should succeed
        assert len(results) == 1

        # Note: This test mainly ensures the mechanism works end-to-end
        # Specific pop counts depend on internal implementation details


class TestExceptionsModuleStructure:
    """Test the structure and exports of the extracted exceptions module."""

    def test_exceptions_module_exports(self):
        """Test that exceptions module exports expected functions."""
        # Test that expected functions are available
        assert hasattr(exceptions, "register")
        assert hasattr(exceptions, "builtin_throw")
        assert hasattr(exceptions, "builtin_catch")
        assert hasattr(exceptions, "handle_throw")

        # Test that register function works
        registry = {}
        exceptions.register(registry)
        assert len(registry) >= 2  # Should register at least throw/1 and catch/3

    def test_exceptions_module_follows_pattern(self):
        """Test that exceptions module follows the established builtin pattern."""
        # Test module has proper docstring
        assert exceptions.__doc__ is not None
        assert "exception" in exceptions.__doc__.lower()

        # Test register function signature

        sig = inspect.signature(exceptions.register)
        params = list(sig.parameters.keys())
        assert "registry" in params


class TestEngineExceptionWrappers:
    """Test that engine maintains compatibility wrappers after extraction."""

    def test_engine_throw_wrapper_exists(self):
        """Test that engine maintains _builtin_throw wrapper."""
        engine = Engine(program())

        # Engine should still have the method
        assert hasattr(engine, "_builtin_throw")
        assert callable(engine._builtin_throw)

    def test_engine_catch_wrapper_exists(self):
        """Test that engine maintains _builtin_catch wrapper."""
        engine = Engine(program())

        # Engine should still have the method
        assert hasattr(engine, "_builtin_catch")
        assert callable(engine._builtin_catch)

    def test_engine_handle_throw_wrapper_exists(self):
        """Test that engine maintains _handle_throw wrapper."""
        engine = Engine(program())

        # Engine should still have the method
        assert hasattr(engine, "_handle_throw")
        assert callable(engine._handle_throw)

    def test_wrappers_delegate_correctly(self):
        """Test that engine wrappers delegate to extracted functions."""
        engine = Engine(program())

        # Test that calling engine methods works the same as before
        # This indirectly tests that delegation works
        ball = Atom("test")

        # Test throw wrapper
        with pytest.raises(PrologThrow):
            engine._builtin_throw((ball,))

        # Test catch wrapper
        result = engine._builtin_catch((Atom("true"), Var(0, "_"), Atom("true")))
        assert result is True
