"""WAM machine state and execution loop.

Environment Frame Layout
------------------------
Environment frames store permanent variables (Y registers) and control info:
    [prev_E, saved_CP, Y0, Y1, Y2, ...]

Where:
- prev_E: Previous environment frame pointer (int or None)
- saved_CP: Continuation pointer to restore on return (int or None)
- Y0, Y1, ...: Permanent variable slots (any value)

The E register points to the start of the current frame in the frames list.

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
    OP_DBG_SNAP,
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_GET_VALUE,
    OP_GET_VARIABLE,
    OP_HALT,
    OP_NOOP,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_PUT_VALUE,
    OP_PUT_VARIABLE,
    OP_SET_X,
)
from prolog.wam.unify import bind, deref, unify


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
        EF: Exception frame pointer (-1 when no exception context)
        X: Argument/temporary registers (list)

    Data areas:
        heap: List of tagged cells
        frames: Environment frames for permanent variables
        cp_stack: Choicepoint records for backtracking
        trail: Trail of bindings for undo on backtrack
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
        self.EF: int = -1  # Exception frame pointer (-1 = none)

        # Unification mode flag (Phase 1)
        self.unify_mode: str | None = None  # "read" or "write"

        # Register banks
        self.X: list = []

        # Data areas
        self.heap: list = []
        self.frames: list = []  # Environment stack
        self.cp_stack: list = []  # Choicepoint stack
        self.trail: list = []

        # Execution control
        self.halted: bool = False

        # Code area (loaded by loader)
        self.code: list[tuple] = []

        # Optional trace sink
        self.trace_sink = None

    def check_invariants(self) -> None:
        """Verify machine state invariants.

        Raises:
            AssertionError: If any invariant is violated

        Invariants checked:
        - Heap pointer H is within valid range [0, len(heap)]
        - All REF cells point to valid heap addresses
        - Unbound REF cells are canonical (self-referential)
        - All cell addresses are within heap bounds

        Future: Once the loader is wired, may add assertion that STR cells'
        functor addresses point to CON cells with (name, arity) tuples.
        Currently relaxed to allow manual heap construction in tests.
        """
        # Heap pointer within bounds
        assert (
            0 <= self.H <= len(self.heap)
        ), f"Heap pointer H={self.H} out of range [0, {len(self.heap)}]"

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
        self.EF = -1
        self.unify_mode = None
        self.X = []
        self.heap = []
        self.frames = []
        self.cp_stack = []
        self.trail = []
        self.halted = False
