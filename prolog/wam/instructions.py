"""WAM instruction set and opcode definitions.

Instructions are encoded as tuples: (opcode: int, *args)
Opcodes are small integers for efficient dispatch.

Phase 0 minimal instruction set:
- OP_NOOP: No operation, advances P
- OP_HALT: Stop execution
- OP_SET_X: Test utility to write X register (will be removed in later phases)
- OP_DBG_SNAP: Capture debug snapshot (debug mode only)
"""

# Opcode constants
OP_NOOP = 0
OP_HALT = 1
OP_SET_X = 2
OP_DBG_SNAP = 3

# Opcode name mapping for debugging and pretty-printing
_OPCODE_NAMES = {
    OP_NOOP: "noop",
    OP_HALT: "halt",
    OP_SET_X: "set_x",
    OP_DBG_SNAP: "dbg_snap",
}

# Reverse mapping for name->opcode lookup
_NAME_TO_OPCODE = {name: op for op, name in _OPCODE_NAMES.items()}

# Instruction arity validation table
_INSTRUCTION_ARITY = {
    OP_NOOP: 0,
    OP_HALT: 0,
    OP_SET_X: 2,  # set_x reg_idx, value
    OP_DBG_SNAP: 0,
}


def opcode_name(opcode: int) -> str:
    """Convert opcode integer to name string."""
    if opcode in _OPCODE_NAMES:
        return _OPCODE_NAMES[opcode]
    return f"<unknown:{opcode}>"


def name_to_opcode(name: str) -> int:
    """Convert instruction name to opcode integer."""
    if name in _NAME_TO_OPCODE:
        return _NAME_TO_OPCODE[name]
    raise ValueError(f"Unknown instruction name: {name}")


def validate_instruction(instruction: tuple) -> None:
    """Validate instruction tuple has correct arity for its opcode.

    Raises:
        ValueError: If instruction arity doesn't match expected arity.
    """
    if not instruction:
        raise ValueError("Empty instruction tuple")

    opcode = instruction[0]
    if opcode not in _INSTRUCTION_ARITY:
        raise ValueError(f"Unknown opcode: {opcode}")

    expected_arity = _INSTRUCTION_ARITY[opcode]
    actual_arity = len(instruction) - 1  # Subtract opcode itself

    if actual_arity != expected_arity:
        name = opcode_name(opcode)
        raise ValueError(
            f"Instruction {name} expects {expected_arity} arguments, "
            f"got {actual_arity}"
        )
