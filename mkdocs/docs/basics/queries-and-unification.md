# Queries and Unification

Queries
-------

- A query is a goal (or a comma‑separated list of goals) ending with a full stop.
- The REPL prints bindings for variables that appear in the query.

```text
?- parent(tom, bob).
true.

?- parent(tom, X).
X = bob.
```

- Multiple goals use `,` (conjunction). Goals run left‑to‑right, backtracking as needed.

```text
?- parent(tom, X), parent(X, Z).
X = bob, Z = ann.
```

- Alternatives use `;` (disjunction). Parenthesize when mixing with `,`.

```text
?- (X = a ; X = b), Y = t.
X = a, Y = t ;
X = b, Y = t.
```

Unification (=)
----------------

Unification tries to make two terms equal by binding variables.

```text
?- X = a.
X = a.

?- point(X, 2) = point(1, Y).
X = 1, Y = 2.

?- a = b.
false.
```

Negated unification (`\=`) succeeds only if the terms cannot unify:

```text
?- a \\= b.
true.
?- X \\= a.
false.
```

Structural vs arithmetic comparisons
------------------------------------

- Structural equality/inequality (no unification): `==/2`, `\==/2`

```text
?- X == Y.
false.  % different unbound variables are not identical
?- a == a.
true.
```

- Arithmetic evaluation and comparison: use `is/2` to evaluate, `=:=/2` and `=\=/2` to compare values.

```text
?- X is 1+2.
X = 3.
?- 1+2 =:= 3.
true.
?- 1+2 = 3.
false.  % = is unification, not arithmetic
```

Multiple solutions and backtracking
-----------------------------------

Backtracking explores alternatives to find more solutions.

```prolog
color(red). color(green). color(blue).
```

```text
?- color(C).
C = red ;
C = green ;
C = blue ;
false.
```

Anonymous variables
-------------------

- `_` is the anonymous variable; each occurrence is fresh and never shares a binding.
- Use `_` for “don’t care” positions to avoid cluttering output.

```text
?- parent(tom, _).
true.
```

Occurs check
------------

- Like most Prologs, PyLog does not perform the occurs check by default. Queries such as `X = f(X)` will succeed and create a cyclic term. Prefer acyclic data where possible.

See also
--------

- Basics → Operators: for `,`, `;`, `->`, and equality/compare operators
- Basics → Arithmetic: `is/2`, `=:=/2`, `=\=/2`, `<`, `=<`, `>`, `>=`

