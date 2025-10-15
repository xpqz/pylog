# Errors

Common issues:

- Missing full stop `.` at end of goal or clause.
- Unknown operator or term syntax. Check operators and lists.
- Non‑associative operator used in a chain. For example `B1 #<=> B2 #<=> B3` needs parentheses: `B1 #<=> (B2 #<=> B3)`.
- Non‑linear CLP(FD) term like `X*Y`. Only linear arithmetic is supported.
- Empty domain after posting constraints. Relax constraints or domains.

Use smaller examples to isolate errors and add constraints step by step.

---

## WAM Bytecode Load Errors (experimental)

When using the WAM backend, malformed bytecode is rejected by the loader with structured errors.

Schema (representative):
```json
{
  "type": "BytecodeLoadError",
  "code": "OP_ARITY_MISMATCH | UNKNOWN_OPCODE | BAD_OPERAND | UNRESOLVED_LABEL | MODULE_NOT_FOUND | REGISTER_OOB | VERSION_MISMATCH",
  "message": "summary",
  "opcode": 37,
  "expectedArity": 2,
  "foundArity": 3,
  "label": "L42",
  "predicate": "m:p/2",
  "pc": 128,
  "module": "m",
  "hint": "optional guidance"
}
```

Examples:
```json
{ "type": "BytecodeLoadError", "code": "OP_ARITY_MISMATCH", "message": "opcode 37 expects 2 args, found 3", "opcode": 37, "expectedArity": 2, "foundArity": 3, "label": "L42", "predicate": "m:p/2", "pc": 57 }
{ "type": "BytecodeLoadError", "code": "UNKNOWN_OPCODE", "message": "unknown opcode 255", "opcode": 255, "pc": 3 }
{ "type": "BytecodeLoadError", "code": "UNRESOLVED_LABEL", "message": "label L_foo not defined", "label": "L_foo", "predicate": "user:q/1" }
```

These errors should never crash the engine; they are reported to the caller and, when the dual‑engine flag is enabled, execution can fall back to the tree‑walker.
