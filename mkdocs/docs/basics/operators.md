# Operators

PyLog supports standard Prolog operators with defined precedence and associativity. The reader parses infix/prefix forms into canonical structures.

Main categories
---------------

- Arithmetic: `+`, `-`, `*`, `/`, `//`, `mod`
- Arithmetic compare: `=:=`, `=\=`, `<`, `=<`, `>`, `>=`
- Term equality/identity: `=`, `\=`, `==`, `\==`, `=..`
- Control: `,` (conjunction), `;` (disjunction), `->/2` (if‑then)
- Negation: `\+/1` (prefix)
- CLP(FD): `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=` and reification `#<=>`, `#==>`, `#<==`

Precedence and associativity
----------------------------

Higher precedence binds tighter. Associativity determines grouping at the same precedence.

- `;` (1100, xfy) and `->` (1050, xfy) are right‑associative control constructs.
- `,` (1000, xfy) is conjunction and is right‑associative.
- Comparisons (700, xfx) are non‑associative: you cannot chain them without parentheses.
- `is/2` and arithmetic comparisons are at precedence 700.
- `+` and `-` (500, yfx) are left‑associative; `*`, `/`, `//`, `mod` (400, yfx) bind tighter than `+`/`-`.
- Prefix `-`, `+` have high precedence (200, fy): `-3 + 2` parses as `(-3) + 2`.

Examples
--------

```text
?- 1 - 2 - 3.
% parses as (1 - 2) - 3 (left‑assoc)

?- A -> B ; C.
% parses as (A -> B) ; C (right‑assoc)

?- X = 1, Y = 2, (X =:= Y ; X < Y).
X = 1, Y = 2.
```

Non‑associative operators (xfx)
--------------------------------

Operators with type `xfx` cannot be chained at the same precedence. Use parentheses to group.

```text
?- X = Y = Z.
% error: non‑associative operator requires parentheses
?- X = (Y = Z).
```

Prefix operators
----------------

- Unary `-` and `+` apply to numeric expressions.
- `\+/1` is negation as failure; see Basics → Control for semantics and caveats.

Comma: operator vs separator
----------------------------

- `,` is a control operator in goals: `a, b` means run `a` then `b`.
- Inside argument lists, commas separate arguments and are not operators: `p(a, b, c)` is a single goal with three arguments.

Quoting operator atoms
----------------------

Operators are atoms in canonical form and can be used as functors when quoted: `'+'(1,2)` is the same as `1 + 2`. This is rarely needed but useful for meta‑programming.

CLP(FD) reification
-------------------

The reader supports `#<=>`, `#==>`, `#<==` as non‑associative (xfx) at precedence 900. See CLP(FD) → Reification for details.

See also
--------

- Basics → Arithmetic for `is/2` and numeric comparisons
- Reference → Operators table for a compact overview
