# Reification (preview)

Reification links the truth of a constraint to a Boolean variable (typically 0/1).
PyLog now parses the standard CLP(FD) reification operators:

- `B #<=> C` — equivalence: B is true iff constraint C holds
- `B #==> C` — forward implication: if B is true then C holds
- `B #<== C` — backward implication: if C holds then B is true

Precedence and associativity:

- All three are non‑associative `xfx` at precedence 900
- They bind tighter than `,` and `;`, but looser than arithmetic comparisons
- Chaining requires parentheses: `B1 #<=> (B2 #<=> B3)`

Examples (parsing):

```text
?- B #<=> (X #= Y).
?- B #==> (X #< 5).
?- B #<== (Y #> 0).
```

Typical modelling uses `B in 0..1` to declare a Boolean:

```text
?- B in 0..1, B #==> (X #< 5).
```

Note
: Currently, only parsing support is implemented for these operators. The actual
  constraint solver semantics have not yet been implemented. Programs using
  reification operators will parse successfully but cannot be executed.

See also:

- Reference → Operators table
- CLP(FD) → Linear Constraints

