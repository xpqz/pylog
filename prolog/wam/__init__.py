"""Warren Abstract Machine (WAM) implementation for PyLog.

This package implements a register-based abstract machine for executing Prolog
programs with improved performance characteristics compared to tree-walking
interpretation.

Architecture:
- machine.py: Core Machine class with registers, stacks, and execution loop
- heap.py: Heap management and cell tagging (REF, STR, CON, LIST)
- instructions.py: Opcode definitions and instruction encoding
- debug.py: Snapshot capture and pretty-printing for observability

The WAM is designed to eventually replace the tree-walking interpreter whilst
maintaining identical semantics and test compatibility.

Reference: docs/plans/wam/WAM-ARCH.md
"""

from prolog.wam.machine import Machine

__all__ = ["Machine"]
