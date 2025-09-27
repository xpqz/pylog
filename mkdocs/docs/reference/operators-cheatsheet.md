# Operators cheat sheet

A quick reference for precedence, associativity, and common parsing pitfalls.

Precedence (higher binds tighter)
---------------------------------

- 1100 xfy: `;`
- 1050 xfy: `->`
- 1000 xfy: `,`
- 900  xfx: CLP(FD) reification `#<=>`, `#==>`, `#<==`
- 700  xfx: `=`, `\=`, `==`, `\==`, `=..`, `is`, `=:=`, `=\=`, `<`, `=<`, `>`, `>=`, `in`
- 500  yfx: `+`, `-`
- 400  yfx: `*`, `/`, `//`, `mod`
- 200   fy: unary `-`, unary `+`

Associativity
-------------

- `xfy` (right‑assoc): `A -> B -> C` is `A -> (B -> C)`
- `yfx` (left‑assoc): `1 - 2 - 3` is `(1 - 2) - 3`
- `xfx` (non‑assoc): cannot chain without parentheses

Non‑associative examples
------------------------

```text
?- X = Y = Z.
% error: use parentheses
?- X = (Y = Z).
```

Operator vs separator commas
----------------------------

- `a, b` uses `,` as a control operator (conjunction)
- `p(a, b, c)` uses commas as argument separators (not operators)

Prefix operators
----------------

- `-3 + 2` parses as `(-3) + 2`
- `\+ Goal` is negation as failure (see Control)

If–then–else
------------

- `A -> B ; C` parses as `(A -> B) ; C`
- Parenthesize when mixing with `,` and `;`: `(A, B -> C ; D)`

CLP(FD)
-------

- Comparisons: `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`
- Reification: `#<=>`, `#==>`, `#<==` (xfx at 900)

See also: Basics → Operators, Reference → Operators table
