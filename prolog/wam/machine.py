"""WAM machine state and execution loop.

Environment Frame Layout
------------------------
Environment frames store permanent variables (Y registers) and control info:
    [prev_E, saved_CP, n_slots, Y0, Y1, Y2, ...]

Where:
- prev_E: Previous environment frame pointer (int or None)
- saved_CP: Continuation pointer to restore on return (int or None)
- n_slots: Number of Y register slots in this frame (int)
- Y0, Y1, ...: Permanent variable slots (any value)

The E register points to the start of the current frame in the frames list.
Y registers are accessed at offset E + 3 + yi (after prev_E, saved_CP, n_slots).

Choicepoint Layout
------------------
Choicepoint records enable backtracking by saving machine state:
    [prev_B, saved_CP, saved_E, saved_P, saved_H, saved_TR, saved_HB, alt_ptr]

Where:
- prev_B: Previous choicepoint pointer (int or None)
- saved_CP: Continuation pointer at choice time
- saved_E: Environment pointer at choice time
- saved_P: Program counter at choice time
- saved_H: Heap top at choice time
- saved_TR: Trail pointer at choice time
- saved_HB: Heap backtrack boundary at choice time
- alt_ptr: Alternative clause pointer for next try (int or None)

The B register points to the start of the current choicepoint in cp_stack.

Phase 0 Note
------------
Frame and choicepoint manipulation will be implemented in subsequent phases.
Phase 0 provides only the data structure scaffolding.
"""

from prolog.wam.heap import is_ref, is_str, new_con, new_ref, new_str
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_CALL,
    OP_CUT,
    OP_DEALLOCATE,
    OP_DBG_SNAP,
    OP_EXECUTE,
    OP_GET_CONSTANT,
    OP_GET_LEVEL,
    OP_GET_STRUCTURE,
    OP_GET_VALUE,
    OP_GET_VARIABLE,
    OP_HALT,
    OP_NECK_CUT,
    OP_NOOP,
    OP_PROCEED,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_PUT_VALUE,
    OP_PUT_VARIABLE,
    OP_RETRY_ME_ELSE,
    OP_SET_X,
    OP_TRUST_ME,
    OP_TRY_ME_ELSE,
)
from prolog.wam.unify import bind, deref, unify, untrail


class ExceptionFrame:
    """Exception frame for catch/3 unwinding.

    Saves complete machine state at catch entry point to enable
    proper unwinding on throw/1.

    Attributes:
        prev_frame: Index of previous exception frame (or None if bottom)
        ball_pattern: Heap address of catch pattern for matching
        handler_label: Code address of handler to execute on match
        CP: Saved continuation pointer
        E: Saved environment pointer
        B: Saved choicepoint pointer
        H: Saved heap top
        TR: Saved trail pointer
        HB: Saved heap backtrack boundary
    """

    def __init__(
        self,
        prev_frame: int | None,
        ball_pattern: int,
        handler_label: int,
        CP: int | None,
        E: int | None,
        B: int | None,
        H: int,
        TR: int,
        HB: int,
    ):
        self.prev_frame = prev_frame
        self.ball_pattern = ball_pattern
        self.handler_label = handler_label
        self.CP = CP
        self.E = E
        self.B = B
        self.H = H
        self.TR = TR
        self.HB = HB


def push_exception_frame(machine, ball_pattern: int, handler_label: int):
    """Push new exception frame onto stack.

    Captures current machine state for potential unwinding.

    Args:
        machine: WAM machine
        ball_pattern: Heap address of catch pattern
        handler_label: Code address of handler
    """
    # Debug invariant: H must equal len(heap)
    assert machine.H == len(
        machine.heap
    ), f"Heap invariant violated: H={machine.H}, len(heap)={len(machine.heap)}"

    frame = ExceptionFrame(
        prev_frame=machine.EF,
        ball_pattern=ball_pattern,
        handler_label=handler_label,
        CP=machine.CP,
        E=machine.E,
        B=machine.B,
        H=machine.H,
        TR=machine.TR,
        HB=machine.HB,
    )

    idx = len(machine.exception_frames)
    machine.exception_frames.append(frame)
    machine.EF = idx


def pop_exception_frame(machine):
    """Pop exception frame from stack.

    Restores EF to previous frame (normal catch exit).
    Frame remains in list but is no longer active.

    Args:
        machine: WAM machine
    """
    if machine.EF is None:
        return  # No active frame

    frame = machine.exception_frames[machine.EF]
    machine.EF = frame.prev_frame


class Machine:
    """Warren Abstract Machine for Prolog execution.

    Registers:
        P: Program counter (index into code area)
        H: Heap top pointer
        HB: Heap backtrack boundary
        B: Choicepoint stack pointer
        E: Environment frame pointer
        CP: Continuation pointer (return address)
        TR: Trail pointer
        S: Structure argument pointer (Phase 1)
        EF: Exception frame pointer (None when no exception context)
        X: Argument/temporary registers (list)

    Data areas:
        heap: List of tagged cells
        frames: Environment frames for permanent variables
        cp_stack: Choicepoint records for backtracking
        trail: Trail of bindings for undo on backtrack
        exception_frames: Exception frames for catch/3 (indexed by EF)
    """

    def __init__(self):
        """Initialize machine with empty state."""
        # Registers
        self.P: int = 0
        self.H: int = 0
        self.HB: int = 0
        self.B: int | None = None
        self.E: int | None = None
        self.CP: int | None = None
        self.TR: int = 0
        self.S: int | None = None  # Structure argument pointer (Phase 1)
        self.EF: int | None = None  # Exception frame pointer (None = no active frame)

        # Unification mode flag (Phase 1)
        self.unify_mode: str | None = None  # "read" or "write"

        # Register banks
        self.X: list = []

        # Data areas
        self.heap: list = []
        self.frames: list = []  # Environment stack
        self.cp_stack: list = []  # Choicepoint stack
        self.trail: list = []
        self.exception_frames: list = []  # Exception frames for catch/3

        # Execution control
        self.halted: bool = False

        # Code area (loaded by loader)
        self.code: list[tuple] = []

        # Predicate symbol table (Phase 2)
        # Maps "module:name/arity" -> code address (int)
        self.predicate_table: dict[str, int] = {}

        # Optional trace sink
        self.trace_sink = None

    def get_y(self, yi: int):
        """Read Y register from current environment frame.

        Args:
            yi: Y register index (0-based)

        Returns:
            Value stored in Y[yi]

        Raises:
            IndexError: If yi is out of bounds for current frame
            AssertionError: If E is None (no active frame)
        """
        assert self.E is not None, "No active environment frame"
        # Frame layout: [prev_E, saved_CP, n_slots, Y0, Y1, ...]
        n_slots = self.frames[self.E + 2]
        if yi < 0 or yi >= n_slots:
            raise IndexError(
                f"Y register Y{yi} out of bounds for frame with {n_slots} slots"
            )
        # Y registers start at E + 3 (after prev_E, saved_CP, n_slots)
        addr = self.E + 3 + yi
        return self.frames[addr]

    def set_y(self, yi: int, value) -> None:
        """Write Y register in current environment frame.

        Args:
            yi: Y register index (0-based)
            value: Value to store in Y[yi]

        Raises:
            IndexError: If yi is out of bounds for current frame
            AssertionError: If E is None (no active frame)
        """
        assert self.E is not None, "No active environment frame"
        # Frame layout: [prev_E, saved_CP, n_slots, Y0, Y1, ...]
        n_slots = self.frames[self.E + 2]
        if yi < 0 or yi >= n_slots:
            raise IndexError(
                f"Y register Y{yi} out of bounds for frame with {n_slots} slots"
            )
        # Y registers start at E + 3 (after prev_E, saved_CP, n_slots)
        addr = self.E + 3 + yi
        self.frames[addr] = value

    def register_predicate(self, name: str, address: int) -> None:
        """Register predicate entry point in symbol table.

        Args:
            name: Predicate identifier (format: "module:name/arity")
            address: Code address of predicate entry point
        """
        self.predicate_table[name] = address

    def resolve_predicate(self, name: str) -> int:
        """Resolve predicate name to code address.

        Args:
            name: Predicate identifier (format: "module:name/arity")

        Returns:
            Code address of predicate entry point

        Raises:
            RuntimeError: If predicate is not registered
        """
        if name not in self.predicate_table:
            raise RuntimeError(f"Undefined predicate: {name}")
        return self.predicate_table[name]

    def _restore_from_choicepoint(
        self,
        saved_CP: int | None,
        saved_E: int | None,
        saved_H: int,
        saved_TR: int,
        saved_HB: int,
    ) -> None:
        """Restore machine state from choicepoint snapshot.

        Restores CP, E, unwinds trail, truncates heap and trail, and restores HB.
        Common logic for retry_me_else and trust_me instructions.

        Args:
            saved_CP: Continuation pointer to restore
            saved_E: Environment frame pointer to restore
            saved_H: Heap top to restore
            saved_TR: Trail pointer to restore
            saved_HB: Heap backtrack boundary to restore
        """
        # Restore registers
        self.CP = saved_CP
        self.E = saved_E

        # Unwind trail from current TR down to saved_TR
        untrail(self, saved_TR)
        # Truncate trail list to saved_TR
        del self.trail[saved_TR:]

        # Truncate heap to saved_H
        del self.heap[saved_H:]
        self.H = saved_H

        # Restore HB
        self.HB = saved_HB

    def _check_choicepoint_invariants(self) -> None:
        """Verify choicepoint stack invariants.

        Raises:
            AssertionError: If any invariant is violated

        Invariants checked:
        - cp_stack length is multiple of 8 (choicepoint record size)
        - B points to valid choicepoint start or is None
        - All prev_B pointers form valid chain
        """
        # cp_stack must contain whole choicepoint records (8 fields each)
        assert (
            len(self.cp_stack) % 8 == 0
        ), f"cp_stack length {len(self.cp_stack)} is not a multiple of 8"

        # B must be None or point to a valid choicepoint start
        if self.B is not None:
            assert isinstance(self.B, int), f"B must be int or None, got {type(self.B)}"
            assert self.B >= 0, f"B={self.B} is negative"
            assert (
                self.B % 8 == 0
            ), f"B={self.B} does not point to choicepoint start (not multiple of 8)"
            assert self.B < len(
                self.cp_stack
            ), f"B={self.B} points beyond cp_stack length {len(self.cp_stack)}"

            # Verify prev_B chain integrity
            visited = set()
            current_B = self.B
            while current_B is not None:
                # Detect cycles
                assert (
                    current_B not in visited
                ), f"Cycle detected in choicepoint chain at B={current_B}"
                visited.add(current_B)

                # Check valid index
                assert current_B < len(
                    self.cp_stack
                ), f"Choicepoint B={current_B} points beyond cp_stack"

                # Get prev_B from current choicepoint
                prev_B = self.cp_stack[current_B]

                # prev_B must be None or valid choicepoint pointer
                if prev_B is not None:
                    assert isinstance(
                        prev_B, int
                    ), f"prev_B at {current_B} is {type(prev_B)}, not int or None"
                    assert prev_B >= 0, f"prev_B={prev_B} at {current_B} is negative"
                    assert (
                        prev_B % 8 == 0
                    ), f"prev_B={prev_B} at {current_B} not aligned to choicepoint"
                    assert (
                        prev_B < current_B
                    ), f"prev_B={prev_B} at {current_B} does not point backward"

                current_B = prev_B

    def check_invariants(self) -> None:
        """Verify machine state invariants.

        Raises:
            AssertionError: If any invariant is violated

        Invariants checked:
        - Heap pointer H is within valid range [0, len(heap)]
        - All REF cells point to valid heap addresses
        - Unbound REF cells are canonical (self-referential)
        - All cell addresses are within heap bounds
        - Choicepoint stack integrity (via _check_choicepoint_invariants)

        Future: Once the loader is wired, may add assertion that STR cells'
        functor addresses point to CON cells with (name, arity) tuples.
        Currently relaxed to allow manual heap construction in tests.
        """
        # Heap pointer within bounds
        assert (
            0 <= self.H <= len(self.heap)
        ), f"Heap pointer H={self.H} out of range [0, {len(self.heap)}]"

        # Choicepoint stack invariants
        self._check_choicepoint_invariants()

        # Check all heap cells for valid addresses
        for i, cell in enumerate(self.heap):
            if not isinstance(cell, tuple) or len(cell) < 1:
                continue

            tag = cell[0]

            # REF cells: check address validity and canonical form
            if tag == 0:  # TAG_REF
                if len(cell) >= 2:
                    addr = cell[1]
                    assert isinstance(
                        addr, int
                    ), f"REF cell at {i} has non-integer address: {cell}"
                    assert (
                        0 <= addr < len(self.heap)
                    ), f"REF cell at {i} points to invalid address {addr}"
                    # Canonical form: unbound refs point to themselves
                    if addr == i:
                        # Self-referential = unbound (canonical)
                        pass

            # STR cells: check functor address
            elif tag == 1:  # TAG_STR
                if len(cell) >= 2:
                    functor_addr = cell[1]
                    assert isinstance(
                        functor_addr, int
                    ), f"STR cell at {i} has non-integer functor addr: {cell}"
                    assert (
                        0 <= functor_addr < len(self.heap)
                    ), f"STR cell at {i} points to invalid functor address {functor_addr}"

            # LIST cells: check head and tail addresses
            elif tag == 3:  # TAG_LIST
                if len(cell) >= 3:
                    head_addr = cell[1]
                    tail_addr = cell[2]
                    assert isinstance(
                        head_addr, int
                    ), f"LIST cell at {i} has non-integer head addr: {cell}"
                    assert isinstance(
                        tail_addr, int
                    ), f"LIST cell at {i} has non-integer tail addr: {cell}"
                    assert (
                        0 <= head_addr < len(self.heap)
                    ), f"LIST cell at {i} has invalid head address {head_addr}"
                    assert (
                        0 <= tail_addr < len(self.heap)
                    ), f"LIST cell at {i} has invalid tail address {tail_addr}"

    def step(self) -> bool:
        """Execute single instruction at P.

        Returns:
            True if execution should continue, False if halted or P out of bounds
        """
        if self.halted or self.P < 0 or self.P >= len(self.code):
            return False

        # Fetch instruction
        instruction = self.code[self.P]

        # Notify trace sink if present
        if self.trace_sink is not None:
            self.trace_sink.on_step(self, instruction)

        # Decode opcode
        opcode = instruction[0]
        args = instruction[1:] if len(instruction) > 1 else ()

        # Dispatch
        if opcode == OP_NOOP:
            self.P += 1
        elif opcode == OP_HALT:
            self.halted = True
            return False
        elif opcode == OP_SET_X:
            # set_x reg_idx, value
            reg_idx, value = args
            # Ensure X list is large enough
            while len(self.X) <= reg_idx:
                self.X.append(None)
            self.X[reg_idx] = value
            self.P += 1
        elif opcode == OP_DBG_SNAP:
            # Capture snapshot (currently no-op; full implementation in testing)
            self.P += 1
        elif opcode == OP_PUT_VARIABLE:
            # put_variable Xi, Aj
            xi, aj = args
            # Create new unbound REF
            addr = new_ref(self)
            # Extend X if needed
            while len(self.X) <= max(xi, aj):
                self.X.append(None)
            # Store in both Xi and Aj
            self.X[xi] = addr
            self.X[aj] = addr
            self.P += 1
        elif opcode == OP_PUT_VALUE:
            # put_value Xi, Aj
            xi, aj = args
            # Extend X if needed for both Xi and Aj
            while len(self.X) <= max(xi, aj):
                self.X.append(None)
            # Copy Xi to Aj
            self.X[aj] = self.X[xi]
            self.P += 1
        elif opcode == OP_PUT_CONSTANT:
            # put_constant C, Aj
            const_value, aj = args
            # Allocate constant on heap
            addr = new_con(self, const_value)
            # Extend X if needed
            while len(self.X) <= aj:
                self.X.append(None)
            # Store in Aj
            self.X[aj] = addr
            self.P += 1
        elif opcode == OP_PUT_STRUCTURE:
            # put_structure F/N, Aj
            functor, aj = args  # functor is (name, arity) tuple
            name, arity = functor
            # Allocate structure (STR then functor)
            str_addr = new_str(self, name, arity)
            # Extend X if needed
            while len(self.X) <= aj:
                self.X.append(None)
            # Store STR address in Aj
            self.X[aj] = str_addr
            # Set write mode
            self.unify_mode = "write"
            # Set S to first argument slot (functor_addr + 1)
            functor_addr = str_addr + 1
            self.S = functor_addr + 1
            self.P += 1
        elif opcode == OP_GET_VARIABLE:
            # get_variable Xi, Aj
            xi, aj = args
            # Extend X if needed for both Xi and Aj
            while len(self.X) <= max(xi, aj):
                self.X.append(None)
            # Copy Aj to Xi
            self.X[xi] = self.X[aj]
            self.P += 1
        elif opcode == OP_GET_VALUE:
            # get_value Xi, Aj
            xi, aj = args
            # Unify Xi and Aj
            # Handle None gracefully - halt if either is None
            if self.X[xi] is None or self.X[aj] is None:
                self.halted = True
                return False
            if not unify(self, self.X[xi], self.X[aj]):
                # Unification failed
                self.halted = True
                return False
            self.P += 1
        elif opcode == OP_GET_CONSTANT:
            # get_constant C, Aj
            const_value, aj = args
            if self.X[aj] is None:
                self.halted = True
                return False
            # Deref Aj to see what we have
            addr = deref(self, self.X[aj])
            cell = self.heap[addr]
            tag = cell[0]

            # TAG_CON = 2
            if tag == 2:  # TAG_CON
                # Already a constant: check match
                if cell[1] == const_value:
                    # Match: succeed without allocation
                    self.P += 1
                else:
                    # Mismatch: fail without allocation
                    self.halted = True
                    return False
            elif tag == 0:  # TAG_REF (unbound variable)
                # Allocate constant and bind
                const_addr = new_con(self, const_value)
                bind(self, addr, const_addr)
                self.P += 1
            else:
                # Type mismatch (STR or LIST): fail without allocation
                self.halted = True
                return False
        elif opcode == OP_GET_STRUCTURE:
            # get_structure F/N, Aj
            functor, aj = args  # functor is (name, arity) tuple
            name, arity = functor

            # Deref Aj
            addr = deref(self, self.X[aj])
            cell = self.heap[addr]

            if is_ref(cell):
                # Unbound variable: build structure and bind
                str_addr = new_str(self, name, arity)
                bind(self, addr, str_addr)
                self.unify_mode = "write"
                functor_addr = str_addr + 1
                self.S = functor_addr + 1
            elif is_str(cell):
                # Structure: check functor match
                functor_addr = cell[1]
                functor_cell = self.heap[functor_addr]
                if functor_cell[1] != (name, arity):
                    # Functor mismatch
                    self.halted = True
                    return False
                # Functor matches: enter read mode
                self.unify_mode = "read"
                self.S = functor_addr + 1
            else:
                # Type mismatch (not REF or STR)
                self.halted = True
                return False

            self.P += 1
        elif opcode == OP_ALLOCATE:
            # allocate N
            (n,) = args

            # Create frame: [prev_E, saved_CP, n_slots, Y0, Y1, ..., Y_{N-1}]
            frame = [self.E, self.CP, n] + [None] * n

            # Push frame by extending frames list
            frame_addr = len(self.frames)
            self.frames.extend(frame)
            self.E = frame_addr

            self.P += 1
        elif opcode == OP_DEALLOCATE:
            # deallocate
            if self.E is None:
                # No frame to deallocate - error condition
                self.halted = True
                return False

            # Get frame size from n_slots field
            n_slots = self.frames[self.E + 2]
            frame_size = 3 + n_slots  # prev_E, saved_CP, n_slots, Y0...Y_{n-1}

            # Restore from current frame
            self.CP = self.frames[self.E + 1]
            prev_E = self.frames[self.E + 0]

            # Actually free the frame memory by removing it from frames list
            # Remove frame: [E .. E+frame_size)
            del self.frames[self.E : self.E + frame_size]

            # Restore E (no adjustment needed if removing from end, but prev_E may need adjustment)
            # If prev_E was after this frame, adjust it
            if prev_E is not None and prev_E > self.E:
                # prev_E pointed past the frame we just deleted
                self.E = prev_E - frame_size
            else:
                self.E = prev_E

            self.P += 1
        elif opcode == OP_CALL:
            # call Pred
            (pred,) = args

            # Save return address
            self.CP = self.P + 1

            # Resolve predicate (symbol or absolute address)
            if isinstance(pred, str):
                # Symbol reference
                entry = self.resolve_predicate(pred)
            else:
                # Absolute address
                entry = pred

            # Jump to predicate
            self.P = entry
        elif opcode == OP_EXECUTE:
            # execute Pred (tail call)
            (pred,) = args

            # Resolve predicate (symbol or absolute address)
            if isinstance(pred, str):
                # Symbol reference
                entry = self.resolve_predicate(pred)
            else:
                # Absolute address
                entry = pred

            # Jump to predicate without saving CP
            self.P = entry
        elif opcode == OP_PROCEED:
            # proceed (return)
            if self.CP is None:
                # No return address - halt
                self.halted = True
                return False

            # Return to saved address
            self.P = self.CP
        elif opcode == OP_TRY_ME_ELSE:
            # try_me_else Label
            (alt_label,) = args

            # Set HB to current H (heap backtrack boundary) BEFORE creating choicepoint
            # so saved_HB equals saved_H
            self.HB = self.H

            # Create choicepoint record: [prev_B, saved_CP, saved_E, saved_P, saved_H, saved_TR, saved_HB, alt_ptr]
            choicepoint = [
                self.B,  # prev_B
                self.CP,  # saved_CP
                self.E,  # saved_E
                self.P,  # saved_P (before incrementing)
                self.H,  # saved_H
                self.TR,  # saved_TR
                self.HB,  # saved_HB (now equals H)
                alt_label,  # alt_ptr
            ]

            # Push choicepoint to cp_stack
            choicepoint_addr = len(self.cp_stack)
            self.cp_stack.extend(choicepoint)

            # Update B to point to new choicepoint
            self.B = choicepoint_addr

            # Advance P to next instruction
            self.P += 1
        elif opcode == OP_RETRY_ME_ELSE:
            # retry_me_else Label
            (alt_label,) = args

            # Check if B is None (no choicepoint)
            if self.B is None:
                self.halted = True
                return False

            # Restore state from choicepoint at B
            # Choicepoint layout: [prev_B, saved_CP, saved_E, saved_P, saved_H, saved_TR, saved_HB, alt_ptr]
            saved_CP = self.cp_stack[self.B + 1]
            saved_E = self.cp_stack[self.B + 2]
            # saved_P not used - retry_me_else advances P normally
            saved_H = self.cp_stack[self.B + 4]
            saved_TR = self.cp_stack[self.B + 5]
            saved_HB = self.cp_stack[self.B + 6]

            # Restore machine state from choicepoint
            self._restore_from_choicepoint(
                saved_CP, saved_E, saved_H, saved_TR, saved_HB
            )

            # Update alt_ptr in current choicepoint to new label
            self.cp_stack[self.B + 7] = alt_label

            # Advance P to next instruction
            self.P += 1
        elif opcode == OP_TRUST_ME:
            # trust_me (no arguments)

            # Check if B is None (no choicepoint)
            if self.B is None:
                self.halted = True
                return False

            # Restore state from choicepoint at B
            # Choicepoint layout: [prev_B, saved_CP, saved_E, saved_P, saved_H, saved_TR, saved_HB, alt_ptr]
            prev_B = self.cp_stack[self.B + 0]
            saved_CP = self.cp_stack[self.B + 1]
            saved_E = self.cp_stack[self.B + 2]
            # saved_P not used - trust_me advances P normally
            saved_H = self.cp_stack[self.B + 4]
            saved_TR = self.cp_stack[self.B + 5]
            saved_HB = self.cp_stack[self.B + 6]

            # Restore machine state from choicepoint
            self._restore_from_choicepoint(
                saved_CP, saved_E, saved_H, saved_TR, saved_HB
            )

            # Pop choicepoint by removing 8 fields from cp_stack
            del self.cp_stack[self.B : self.B + 8]

            # Restore B to prev_B
            self.B = prev_B

            # Advance P to next instruction
            self.P += 1
        elif opcode == OP_GET_LEVEL:
            # get_level Yk
            (yk,) = args

            # Require active environment
            if self.E is None:
                raise RuntimeError("get_level requires active environment frame")

            # Save current B into Yk
            self.set_y(yk, self.B)

            # Advance P to next instruction
            self.P += 1
        elif opcode == OP_CUT:
            # cut Yk
            (yk,) = args

            # Retrieve saved level from Yk
            saved_level = self.get_y(yk)

            # Validate alignment for non-None level (detect corruption early)
            if saved_level is not None:
                assert isinstance(
                    saved_level, int
                ), f"Cut level must be int or None, got {type(saved_level)}"
                assert (
                    saved_level % 8 == 0
                ), f"Cut level {saved_level} not aligned to choicepoint boundary"
                assert saved_level >= 0, f"Cut level {saved_level} is negative"

            # Prune choicepoints by setting B to saved level
            self.B = saved_level

            # Update HB to match new B
            if self.B is not None:
                # HB comes from choicepoint's saved_HB field (index 6)
                if self.B + 6 >= len(self.cp_stack):
                    raise IndexError(
                        f"Cut level B={self.B} points beyond cp_stack length {len(self.cp_stack)}"
                    )
                self.HB = self.cp_stack[self.B + 6]
            else:
                # No choicepoints - HB is 0
                self.HB = 0

            # Advance P to next instruction
            self.P += 1
        elif opcode == OP_NECK_CUT:
            # neck_cut (no arguments)

            # If no choicepoint, this is a no-op
            if self.B is not None:
                # Get prev_B from current choicepoint
                prev_B = self.cp_stack[self.B + 0]

                # Restore B to prev_B
                self.B = prev_B

                # Update HB from new B
                if prev_B is not None:
                    self.HB = self.cp_stack[prev_B + 6]
                else:
                    self.HB = 0

            # Advance P to next instruction
            self.P += 1
        else:
            # Unknown opcode - halt with error
            self.halted = True
            return False

        return not self.halted

    def run(self, max_steps: int = 1000) -> None:
        """Execute instructions until halt or max_steps reached.

        Args:
            max_steps: Maximum number of instructions to execute
        """
        steps = 0
        while steps < max_steps and self.step():
            steps += 1

    def reset(self) -> None:
        """Reset machine to initial state."""
        self.P = 0
        self.H = 0
        self.HB = 0
        self.B = None
        self.E = None
        self.CP = None
        self.TR = 0
        self.S = None
        self.EF = None  # No active exception frame
        self.unify_mode = None
        self.X = []
        self.heap = []
        self.frames = []
        self.cp_stack = []
        self.trail = []
        self.exception_frames = []  # Clear exception stack
        self.halted = False


class UnhandledPrologException(Exception):
    """Exception raised when throw/1 finds no matching catch frame.

    Attributes:
        ball_addr: Heap address of the exception ball term
    """

    def __init__(self, ball_addr: int):
        self.ball_addr = ball_addr
        super().__init__(f"Unhandled Prolog exception at heap address {ball_addr}")


def untrail_to(machine, target_TR: int) -> None:
    """Untrail bindings and truncate trail to target_TR.

    Restores all trailed bindings from current TR down to target_TR,
    then truncates the trail list.

    Args:
        machine: WAM machine
        target_TR: Trail pointer to restore to
    """
    # Only untrail entries that actually exist in the trail
    # (TR might be > len(trail) if trail was artificially set in tests)
    actual_TR = min(machine.TR, len(machine.trail))

    # Temporarily set machine.TR to actual value for untrail()
    machine.TR = actual_TR

    # Unwind trail from actual_TR down to target_TR
    if actual_TR > target_TR:
        untrail(machine, target_TR)

    # Truncate trail list and set TR
    del machine.trail[target_TR:]
    machine.TR = target_TR


def instr_throw(machine) -> None:
    """Execute throw/1 instruction - unwind exception frames to find handler.

    Algorithm:
    1. Get ball from X[0]
    2. Iterate exception frames from EF (innermost first)
    3. For each frame:
       - Restore CP, E, B, HB from frame (HB critical for trailing decisions)
       - Try unify(ball, pattern) with restored HB but current H and TR
       - If match:
         * Untrail to frame.TR (undo all bindings since frame)
         * Truncate heap to frame.H
         * Set P=handler, pop frame, return
       - If no match:
         * Untrail to frame.TR (undo bindings from this attempt + earlier)
         * Pop frame, continue to next
    4. No match found: raise UnhandledPrologException

    Critical: HB must be restored before unification for correct trailing
    decisions, but H and TR remain at current values until after unification
    succeeds (so ball/pattern are accessible and trail records all bindings).

    Args:
        machine: WAM machine

    Raises:
        UnhandledPrologException: If no exception frame matches the ball
    """
    # Debug invariant: H must equal len(heap)
    assert machine.H == len(
        machine.heap
    ), f"Heap invariant violated: H={machine.H}, len(heap)={len(machine.heap)}"

    # Get ball from X[0]
    ball_addr = machine.X[0]

    # Iterate frames from EF (innermost first)
    ef = machine.EF
    while ef is not None:
        frame = machine.exception_frames[ef]

        # Restore registers needed for correct unification (critical: HB for trailing)
        # Don't restore H or TR yet - need current heap and trail
        machine.CP = frame.CP
        machine.E = frame.E
        machine.B = frame.B
        machine.HB = frame.HB

        # Try unifying ball with pattern (with correct HB for trailing decisions)
        if unify(machine, ball_addr, frame.ball_pattern):
            # Match! Restore H/TR, truncate heap/trail, jump to handler
            untrail_to(machine, frame.TR)
            del machine.heap[frame.H :]
            machine.H = frame.H
            machine.P = frame.handler_label
            machine.EF = frame.prev_frame
            return

        # No match - untrail to undo bindings from failed unification attempt
        # (including any that existed before the frame)
        untrail_to(machine, frame.TR)

        # Pop frame and continue to next
        machine.EF = frame.prev_frame
        ef = machine.EF

    # No matching frame found - raise UnhandledPrologException
    raise UnhandledPrologException(ball_addr)


def instr_catch_setup(machine, handler_label: int, ball_pattern_addr: int) -> None:
    """Execute catch_setup instruction - push exception frame for catch/3.

    Sets up exception frame with:
    - handler_label: Code address to jump to if exception matches
    - ball_pattern_addr: Heap address of pattern to unify with thrown ball

    Args:
        machine: WAM machine
        handler_label: Code address of exception handler
        ball_pattern_addr: Heap address of catch pattern term

    Side effects:
        - Pushes new exception frame onto stack
        - Updates EF to point to new frame
        - Advances P to next instruction
    """
    push_exception_frame(machine, ball_pattern_addr, handler_label)
    machine.P += 1


def instr_catch_cleanup(machine) -> None:
    """Execute catch_cleanup instruction - pop exception frame on normal exit.

    Called when catch/3 Goal completes successfully without throwing.
    Pops the exception frame that was pushed by catch_setup.

    Args:
        machine: WAM machine

    Side effects:
        - Pops exception frame from stack
        - Updates EF to previous frame
        - Advances P to next instruction
    """
    pop_exception_frame(machine)
    machine.P += 1
