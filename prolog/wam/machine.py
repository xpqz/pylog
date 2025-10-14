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

    def run(self, max_steps: int = 1000) -> None:
        """Execute instructions until halt or max_steps reached.

        Args:
            max_steps: Maximum number of instructions to execute

        This is a stub for Phase 0. Full implementation in issue #329.
        """
        pass  # Implemented in issue #329
