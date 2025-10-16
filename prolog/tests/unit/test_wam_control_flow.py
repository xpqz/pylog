"""Unit tests for WAM control flow instructions.

Tests for call/execute/proceed instructions and predicate invocation.

Control flow semantics:
- call Pred: Save CP = P + 1, jump to Pred
- execute Pred: Jump to Pred without saving CP (tail call)
- proceed: Return to saved CP
"""

import pytest
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_CALL,
    OP_DEALLOCATE,
    OP_EXECUTE,
    OP_HALT,
    OP_PROCEED,
)
from prolog.wam.machine import Machine


class TestCallInstruction:
    """Test call Pred instruction."""

    def test_call_saves_cp_and_jumps(self):
        """call saves return address in CP and jumps to target."""
        m = Machine()
        # Code layout:
        # 0: call 3
        # 1: halt
        # 2: (unreachable)
        # 3: proceed  (target)
        m.code = [(OP_CALL, 3), (OP_HALT,), (OP_HALT,), (OP_PROCEED,)]

        assert m.P == 0
        assert m.CP is None

        m.step()  # Execute call 3

        assert m.P == 3  # Jumped to target
        assert m.CP == 1  # Saved return address (P + 1)

    def test_call_with_symbol_name(self):
        """call with symbol name resolves to predicate address."""
        m = Machine()
        m.register_predicate("user:foo/0", 5)

        # 0: call "user:foo/0"
        # 1: halt
        # ...
        # 5: proceed  (foo/0 entry)
        m.code = (
            [(OP_CALL, "user:foo/0"), (OP_HALT,)] + [(OP_HALT,)] * 3 + [(OP_PROCEED,)]
        )

        m.step()  # Execute call

        assert m.P == 5
        assert m.CP == 1

    def test_call_undefined_predicate_errors(self):
        """call with undefined predicate symbol raises error."""
        m = Machine()
        m.code = [(OP_CALL, "user:undefined/0"), (OP_HALT,)]

        # Should fail due to undefined predicate
        with pytest.raises((RuntimeError, KeyError)):
            m.step()

    def test_nested_calls(self):
        """Nested calls with frames maintain CP chain correctly.

        A predicate that makes further calls must allocate a frame to save CP,
        otherwise the nested call will overwrite CP and the return address is lost.
        """
        m = Machine()
        # 0: call 3           # Main calls A
        # 1: halt
        # 2: (pad)
        # 3: allocate 0       # A allocates frame (saves CP=1)
        # 4: call 7           # A calls B
        # 5: deallocate       # A deallocates frame (restores CP=1)
        # 6: proceed          # A returns
        # 7: proceed          # B returns (no further calls, no frame needed)
        m.code = [
            (OP_CALL, 3),
            (OP_HALT,),
            (OP_HALT,),
            (OP_ALLOCATE, 0),
            (OP_CALL, 7),
            (OP_DEALLOCATE,),
            (OP_PROCEED,),
            (OP_PROCEED,),
        ]

        m.step()  # call 3 (main calls A)
        assert m.P == 3
        assert m.CP == 1

        m.step()  # allocate 0 in A (saves CP=1 in frame)
        assert m.E is not None
        frame_E = m.E
        assert m.frames[frame_E + 1] == 1  # saved_CP

        m.step()  # call 7 in A (A calls B, overwrites CP)
        assert m.P == 7
        assert m.CP == 5  # Return to A at deallocate

        m.step()  # proceed from B
        assert m.P == 5  # Back in A at deallocate

        m.step()  # deallocate in A (restores CP=1 from frame)
        assert m.CP == 1
        assert m.E is None

        m.step()  # proceed from A
        assert m.P == 1  # Back to main


class TestExecuteInstruction:
    """Test execute Pred instruction (tail call)."""

    def test_execute_jumps_without_saving_cp(self):
        """execute jumps to target without saving CP."""
        m = Machine()
        m.CP = 99  # Some existing CP

        # 0: execute 2
        # 1: (unreachable)
        # 2: proceed
        m.code = [(OP_EXECUTE, 2), (OP_HALT,), (OP_PROCEED,)]

        m.step()  # Execute execute 2

        assert m.P == 2
        assert m.CP == 99  # CP unchanged

    def test_execute_with_symbol_name(self):
        """execute with symbol name resolves to predicate address."""
        m = Machine()
        m.register_predicate("user:tail/0", 5)
        m.CP = 42

        # 0: execute "user:tail/0"
        # ...
        # 5: proceed
        m.code = [(OP_EXECUTE, "user:tail/0")] + [(OP_HALT,)] * 4 + [(OP_PROCEED,)]

        m.step()  # Execute execute

        assert m.P == 5
        assert m.CP == 42  # CP unchanged

    def test_execute_tail_call_optimization(self):
        """execute enables tail call optimization (no CP growth)."""
        m = Machine()
        m.code = [
            (OP_CALL, 3),  # 0: A calls B
            (OP_HALT,),  # 1: halt
            (OP_HALT,),  # 2: pad
            (OP_EXECUTE, 5),  # 3: B tail-calls C
            (OP_HALT,),  # 4: (unreachable)
            (OP_PROCEED,),  # 5: C proceeds
        ]

        m.step()  # call 3
        assert m.CP == 1

        m.step()  # execute 5 (tail call)
        assert m.CP == 1  # CP unchanged (still points to caller of B)
        assert m.P == 5

        m.step()  # proceed
        assert m.P == 1  # Returns to original caller


class TestProceedInstruction:
    """Test proceed instruction (return)."""

    def test_proceed_returns_to_cp(self):
        """proceed sets P to CP."""
        m = Machine()
        m.CP = 42
        m.code = [(OP_PROCEED,)] + [(OP_HALT,)] * 42

        m.step()  # Execute proceed

        assert m.P == 42

    def test_proceed_with_none_cp_halts(self):
        """proceed with CP=None halts execution."""
        m = Machine()
        m.code = [(OP_PROCEED,), (OP_HALT,)]

        assert m.CP is None
        result = m.step()

        assert result is False or m.halted

    def test_proceed_after_call(self):
        """proceed after call returns to caller."""
        m = Machine()
        # 0: call 2
        # 1: halt
        # 2: proceed
        m.code = [(OP_CALL, 2), (OP_HALT,), (OP_PROCEED,)]

        m.step()  # call 2
        assert m.P == 2
        assert m.CP == 1

        m.step()  # proceed
        assert m.P == 1


class TestPredicateTable:
    """Test predicate symbol table."""

    def test_register_predicate(self):
        """register_predicate adds entry to table."""
        m = Machine()
        m.register_predicate("user:foo/2", 100)

        assert "user:foo/2" in m.predicate_table
        assert m.predicate_table["user:foo/2"] == 100

    def test_resolve_predicate(self):
        """resolve_predicate looks up entry point."""
        m = Machine()
        m.register_predicate("user:bar/1", 42)

        addr = m.resolve_predicate("user:bar/1")
        assert addr == 42

    def test_resolve_undefined_predicate(self):
        """resolve_predicate raises error for undefined predicate."""
        m = Machine()

        with pytest.raises((RuntimeError, KeyError)):
            m.resolve_predicate("user:undefined/0")

    def test_multiple_predicates(self):
        """Multiple predicates can be registered."""
        m = Machine()
        m.register_predicate("user:p/0", 10)
        m.register_predicate("user:q/1", 20)
        m.register_predicate("lists:member/2", 30)

        assert m.resolve_predicate("user:p/0") == 10
        assert m.resolve_predicate("user:q/1") == 20
        assert m.resolve_predicate("lists:member/2") == 30


class TestControlFlowIntegration:
    """Integration tests combining control flow operations."""

    def test_simple_call_return_sequence(self):
        """Simple call and return."""
        m = Machine()
        # 0: call 2
        # 1: halt
        # 2: proceed
        m.code = [(OP_CALL, 2), (OP_HALT,), (OP_PROCEED,)]

        m.step()  # call 2
        m.step()  # proceed
        assert m.P == 1

        m.step()  # halt
        assert m.halted

    def test_call_with_allocate_deallocate(self):
        """call combined with frame allocation and proper teardown."""
        m = Machine()
        # 0: call 3
        # 1: halt
        # 2: (pad)
        # 3: allocate 1
        # 4: deallocate
        # 5: proceed
        m.code = [
            (OP_CALL, 3),
            (OP_HALT,),
            (OP_HALT,),
            (OP_ALLOCATE, 1),
            (OP_DEALLOCATE,),
            (OP_PROCEED,),
        ]

        m.step()  # call 3
        assert m.CP == 1

        m.step()  # allocate 1
        assert m.E is not None

        m.step()  # deallocate
        assert m.E is None
        assert m.CP == 1  # Restored from frame

        m.step()  # proceed
        assert m.P == 1

    def test_three_level_call_chain(self):
        """Three levels of nested calls with proper frame management.

        Each predicate that makes further calls must allocate/deallocate a frame.
        """
        m = Machine()
        # 0: call 3           # main calls A
        # 1: halt
        # 2: (pad)
        # 3: allocate 0       # A allocates frame (saves CP=1)
        # 4: call 8           # A calls B
        # 5: deallocate       # A deallocates (restores CP=1)
        # 6: proceed          # A returns
        # 7: (pad)
        # 8: allocate 0       # B allocates frame (saves CP=5)
        # 9: call 13          # B calls C
        # 10: deallocate      # B deallocates (restores CP=5)
        # 11: proceed         # B returns
        # 12: (pad)
        # 13: proceed         # C returns (no further calls, no frame)
        m.code = [
            (OP_CALL, 3),
            (OP_HALT,),
            (OP_HALT,),
            (OP_ALLOCATE, 0),
            (OP_CALL, 8),
            (OP_DEALLOCATE,),
            (OP_PROCEED,),
            (OP_HALT,),
            (OP_ALLOCATE, 0),
            (OP_CALL, 13),
            (OP_DEALLOCATE,),
            (OP_PROCEED,),
            (OP_HALT,),
            (OP_PROCEED,),
        ]

        m.step()  # main calls A
        assert m.P == 3
        assert m.CP == 1

        m.step()  # A allocates
        assert m.E == 0
        assert m.frames[0 + 1] == 1  # saved_CP

        m.step()  # A calls B
        assert m.P == 8
        assert m.CP == 5

        m.step()  # B allocates
        assert m.E > 0
        E_B = m.E
        assert m.frames[E_B + 1] == 5  # saved_CP

        m.step()  # B calls C
        assert m.P == 13
        assert m.CP == 10

        m.step()  # C proceeds
        assert m.P == 10

        m.step()  # B deallocates
        assert m.CP == 5  # Restored from B's frame
        assert m.E == 0  # Back to A's frame

        m.step()  # B proceeds
        assert m.P == 5

        m.step()  # A deallocates
        assert m.CP == 1  # Restored from A's frame
        assert m.E is None

        m.step()  # A proceeds
        assert m.P == 1

    def test_tail_recursion_with_execute(self):
        """Tail recursion via execute maintains constant CP."""
        m = Machine()
        # Simulates tail-recursive countdown
        # 0: call 2       # Start
        # 1: halt
        # 2: execute 2    # Tail recursive call
        m.code = [(OP_CALL, 2), (OP_HALT,), (OP_EXECUTE, 2)]

        m.step()  # Initial call
        initial_cp = m.CP
        assert initial_cp == 1

        # Multiple tail calls
        for _ in range(5):
            m.step()  # execute 2
            assert m.CP == initial_cp  # CP never changes
            assert m.P == 2

    def test_mixed_call_and_execute(self):
        """Mix of call and execute in same execution."""
        m = Machine()
        # 0: call 3       # A calls B normally
        # 1: halt
        # 2: (pad)
        # 3: execute 5    # B tail-calls C
        # 4: (unreachable)
        # 5: proceed      # C returns
        m.code = [
            (OP_CALL, 3),
            (OP_HALT,),
            (OP_HALT,),
            (OP_EXECUTE, 5),
            (OP_HALT,),
            (OP_PROCEED,),
        ]

        m.step()  # call 3
        assert m.CP == 1

        m.step()  # execute 5
        assert m.CP == 1  # Still points to original caller
        assert m.P == 5

        m.step()  # proceed
        assert m.P == 1  # Returns to original caller

    def test_predicate_symbols_in_nested_calls(self):
        """Predicate symbols work in nested calls with frames."""
        m = Machine()
        m.register_predicate("user:a/0", 3)
        m.register_predicate("user:b/0", 7)

        # 0: call "user:a/0"
        # 1: halt
        # 2: (pad)
        # 3: allocate 0       # a/0 allocates frame (saves CP=1)
        # 4: call "user:b/0"
        # 5: deallocate       # a/0 deallocates (restores CP=1)
        # 6: proceed          # a/0 returns
        # 7: proceed          # b/0 returns (no further calls, no frame)
        m.code = [
            (OP_CALL, "user:a/0"),
            (OP_HALT,),
            (OP_HALT,),
            (OP_ALLOCATE, 0),
            (OP_CALL, "user:b/0"),
            (OP_DEALLOCATE,),
            (OP_PROCEED,),
            (OP_PROCEED,),
        ]

        m.step()  # call a/0
        assert m.P == 3

        m.step()  # allocate in a/0
        assert m.E is not None

        m.step()  # call b/0
        assert m.P == 7

        m.step()  # proceed from b/0
        assert m.P == 5

        m.step()  # deallocate in a/0
        assert m.E is None

        m.step()  # proceed from a/0
        assert m.P == 1


class TestControlFlowEdgeCases:
    """Test edge cases and error conditions."""

    def test_call_to_self_is_valid(self):
        """call can jump to itself (recursion)."""
        m = Machine()
        # 0: call 0  (recursive call to self)
        m.code = [(OP_CALL, 0)]

        m.step()
        assert m.P == 0
        assert m.CP == 1

    def test_execute_to_self_is_valid(self):
        """execute can jump to itself (tail recursion)."""
        m = Machine()
        m.CP = 99
        # 0: execute 0
        m.code = [(OP_EXECUTE, 0)]

        m.step()
        assert m.P == 0
        assert m.CP == 99

    def test_call_with_absolute_address_zero(self):
        """call with address 0 is valid."""
        m = Machine()
        # 0: proceed
        # 1: call 0
        m.code = [(OP_PROCEED,), (OP_CALL, 0)]

        m.P = 1
        m.step()  # call 0
        assert m.P == 0
        assert m.CP == 2

    def test_multiple_sequential_executes(self):
        """Multiple execute instructions in sequence."""
        m = Machine()
        m.CP = 100
        # 0: execute 1
        # 1: execute 2
        # 2: execute 3
        # 3: proceed
        m.code = [(OP_EXECUTE, 1), (OP_EXECUTE, 2), (OP_EXECUTE, 3), (OP_PROCEED,)]

        for i in range(1, 4):
            m.step()
            assert m.P == i
            assert m.CP == 100

        m.step()  # proceed
        assert m.P == 100

    def test_proceed_advances_correctly_after_call(self):
        """proceed uses saved CP, not current P."""
        m = Machine()
        # 0: call 5
        # 1: halt
        # 2-4: (pad)
        # 5: proceed
        m.code = [(OP_CALL, 5)] + [(OP_HALT,)] * 4 + [(OP_PROCEED,)]

        m.step()  # call 5
        assert m.P == 5
        assert m.CP == 1

        m.step()  # proceed
        assert m.P == 1  # Goes to CP, not P+1
