"""WAM bytecode loader and validator.

Responsibilities
- Validate assembled bytecode tuples against opcode arity table
- Perform basic operand validation (register shapes, functors, etc.)
- Build a predicate registry from the provided symbol table
- Return a code area and registry for execution

Public API
- load_program(bytecode: dict) -> dict {"code": [...], "registry": PredicateRegistry()}
- PredicateRegistry: add(), lookup(), list()
- BytecodeLoadError: structured error for loader failures
"""

from __future__ import annotations

from typing import Dict, List, Optional, Tuple, Union

from .instructions import (
    OP_CALL,
    OP_CATCH_CLEANUP,
    OP_CATCH_SETUP,
    OP_EXECUTE,
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_GET_VALUE,
    OP_GET_VARIABLE,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_PUT_VALUE,
    OP_PUT_VARIABLE,
    OP_THROW,
    OP_UNIFY_CONSTANT,
    OP_UNIFY_VALUE,
    OP_UNIFY_VARIABLE,
    validate_instruction,
)


class BytecodeLoadError(Exception):
    """Error raised by the bytecode loader with structured context.

    Attributes (not exhaustive; set when relevant):
        code: short machine-readable error code
        pc: program counter where the error was detected
        opcode: the opcode integer at fault (if available)
        message: human-readable description via str(error)
    """

    def __init__(self, message: str, *, code: str = "LOAD_ERROR", **kwargs):
        super().__init__(message)
        self.code = code
        for k, v in kwargs.items():
            setattr(self, k, v)

    def __str__(self) -> str:  # keep message informative with optional pc
        base = super().__str__()
        pc = getattr(self, "pc", None)
        if pc is not None and "pc" not in base.lower():
            return f"{base} (pc={pc})"
        return base


class PredicateRegistry:
    """Registry of predicate entry points keyed by "module:name/arity"."""

    def __init__(self) -> None:
        self._table: Dict[str, int] = {}

    def add(self, symbol: str, offset: int) -> None:
        # Overwrite duplicates by default (tests accept update or raise)
        self._table[symbol] = int(offset)

    def lookup(self, symbol: str) -> Optional[int]:
        return self._table.get(symbol)

    def list(self) -> List[str]:
        return list(self._table.keys())


Register = Union[int, Tuple[str, int]]


def _is_nonneg_int(x: object) -> bool:
    return isinstance(x, int) and x >= 0


def _validate_xy_reg(operand: object) -> None:
    # Accept Xi as int or Yi as ("Y", idx)
    if isinstance(operand, int):
        if operand < 0:
            raise BytecodeLoadError(
                "register index must be non-negative", code="BAD_REGISTER"
            )
        return
    if isinstance(operand, tuple) and len(operand) == 2:
        kind, idx = operand
        if kind == "Y" and isinstance(idx, int) and idx >= 0:
            return
    raise BytecodeLoadError("invalid XY register operand", code="BAD_REGISTER")


def _validate_a_reg(operand: object) -> None:
    if not _is_nonneg_int(operand):
        raise BytecodeLoadError("invalid argument register", code="BAD_REGISTER")


def _validate_functor(f: object) -> None:
    if not (
        isinstance(f, tuple)
        and len(f) == 2
        and isinstance(f[0], str)
        and isinstance(f[1], int)
    ):
        raise BytecodeLoadError(
            "invalid functor operand; expected (name, arity)", code="BAD_FUNCTOR"
        )


def _validate_instruction_operands(instr: tuple, pc: int) -> None:
    opcode = instr[0]
    # General arity and opcode validation
    try:
        validate_instruction(instr)
    except Exception as e:
        msg = str(e).lower()
        if "unknown opcode" in msg:
            raise BytecodeLoadError(
                f"unknown opcode {instr[0]}",
                code="UNKNOWN_OPCODE",
                pc=pc,
                opcode=instr[0],
            )
        # Arity mismatch: include the word 'arity' in the message for tests
        raise BytecodeLoadError(
            f"arity mismatch: {e}", code="OP_ARITY_MISMATCH", pc=pc, opcode=instr[0]
        )

    # Per-opcode operand shape validation
    if opcode == OP_GET_VARIABLE:
        _, xy, aj = instr
        _validate_xy_reg(xy)
        _validate_a_reg(aj)
    elif opcode == OP_GET_VALUE:
        _, xy, aj = instr
        _validate_xy_reg(xy)
        _validate_a_reg(aj)
    elif opcode == OP_GET_CONSTANT:
        # Accept broad constant types; ensure A register
        _, _const, aj = instr
        _validate_a_reg(aj)
    elif opcode == OP_GET_STRUCTURE:
        _, functor, aj = instr
        _validate_functor(functor)
        _validate_a_reg(aj)
    elif opcode == OP_PUT_VARIABLE:
        _, xy, aj = instr
        _validate_xy_reg(xy)
        _validate_a_reg(aj)
    elif opcode == OP_PUT_VALUE:
        _, xy, aj = instr
        _validate_xy_reg(xy)
        _validate_a_reg(aj)
    elif opcode == OP_PUT_CONSTANT:
        _, _const, aj = instr
        _validate_a_reg(aj)
    elif opcode == OP_PUT_STRUCTURE:
        _, functor, xy = instr
        _validate_functor(functor)
        _validate_xy_reg(xy)
    elif opcode == OP_UNIFY_VARIABLE:
        _, xy = instr
        _validate_xy_reg(xy)
    elif opcode == OP_UNIFY_VALUE:
        _, xy = instr
        _validate_xy_reg(xy)
    elif opcode == OP_UNIFY_CONSTANT:
        # Accept any constant types
        pass
    elif opcode in (OP_CALL, OP_EXECUTE):
        _, target = instr
        if not isinstance(target, str):
            raise BytecodeLoadError(
                "call target must be a string", code="BAD_CALL_TARGET", pc=pc
            )
    elif opcode == OP_CATCH_SETUP:
        # catch_setup Handler_Label, Ball_Pattern_Addr
        _, handler, pattern = instr
        if not isinstance(handler, int) or not isinstance(pattern, int):
            raise BytecodeLoadError(
                "catch_setup expects integer handler label and pattern address",
                code="BAD_CATCH_SETUP_OPERANDS",
                pc=pc,
            )
        if handler < 0 or pattern < 0:
            raise BytecodeLoadError(
                "catch_setup operands must be non-negative",
                code="BAD_CATCH_SETUP_OPERANDS",
                pc=pc,
            )
    elif opcode == OP_CATCH_CLEANUP:
        # No operands beyond arity check
        pass
    elif opcode == OP_THROW:
        # No operands; ball is in X[0]
        pass
    else:
        # Remaining opcodes either have no extra operand checks here or are validated by arity
        pass


def load_program(bytecode: dict) -> dict:
    """Validate and load bytecode into a code area and registry.

    Args:
        bytecode: dict with keys "code" (list of instruction tuples) and
                  "symbols" (mapping predicate symbols -> offsets)

    Returns:
        dict: {"code": code_list, "registry": PredicateRegistry}

    Raises:
        BytecodeLoadError: on validation or structure errors.
    """
    if not isinstance(bytecode, dict):
        raise BytecodeLoadError("bytecode must be a dict", code="BAD_INPUT")

    code = bytecode.get("code")
    symbols = bytecode.get("symbols", {})

    if code is None or not isinstance(code, list):
        raise BytecodeLoadError("bytecode['code'] must be a list", code="BAD_INPUT")
    if not isinstance(symbols, dict):
        raise BytecodeLoadError("bytecode['symbols'] must be a dict", code="BAD_INPUT")

    # Validate instructions one by one with pc context
    for pc, instr in enumerate(code):
        if not isinstance(instr, tuple) or not instr:
            raise BytecodeLoadError(
                "invalid instruction encoding", code="BAD_INSTRUCTION", pc=pc
            )
        _validate_instruction_operands(instr, pc)

    # Build registry from symbols; validate offsets are within code area
    registry = PredicateRegistry()
    n = len(code)
    for sym, off in symbols.items():
        if not isinstance(off, int):
            raise BytecodeLoadError(
                f"symbol offset for {sym} must be int", code="BAD_SYMBOL_OFFSET"
            )
        if off < 0 or off >= n:
            raise BytecodeLoadError(
                f"symbol {sym} points outside code area", code="SYMBOL_OUT_OF_RANGE"
            )
        registry.add(sym, off)

    return {"code": list(code), "registry": registry}


__all__ = [
    "BytecodeLoadError",
    "PredicateRegistry",
    "load_program",
]
