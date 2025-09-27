# Errors

Common issues:

- Missing full stop `.` at end of goal or clause.
- Unknown operator or term syntax. Check operators and lists.
- Non‑associative operator used in a chain. For example `B1 #<=> B2 #<=> B3` needs parentheses: `B1 #<=> (B2 #<=> B3)`.
- Non‑linear CLP(FD) term like `X*Y`. Only linear arithmetic is supported.
- Empty domain after posting constraints. Relax constraints or domains.

Use smaller examples to isolate errors and add constraints step by step.
