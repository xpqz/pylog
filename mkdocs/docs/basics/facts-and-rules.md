# Facts and Rules

Facts state things that are unconditionally true. Rules define truths that depend on other goals.

Syntax
------

- A fact is a term followed by a full stop: `parent(tom, bob).`
- A rule has the form `Head :- Body.` where `Body` is a comma‑separated conjunction of goals.
- Clauses end with `.`. Whitespace and newlines are flexible.

Examples
--------

Family relations with recursion:

```prolog
parent(tom, bob).
parent(bob, ann).

ancestor(X, Z) :- parent(X, Z).              % base case
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).  % recursive case
```

Queries and multiple solutions:

```text
?- ancestor(tom, Z).
Z = bob ;
Z = ann ;
false.
```

Variables and scope
-------------------

- Variables are implicitly universally quantified per clause (they must work for all values that make the body true).
- The same variable name within a clause denotes the same value (unification).
- Variables do not carry across clauses; each clause has its own scope.

Conjunction and disjunction
---------------------------

- `,` (comma) is logical AND (conjunction) in rule bodies.
- `;` (semicolon) is logical OR (disjunction). You can use it in bodies to express alternatives.

```prolog
edge(a,b). edge(b,c).
connected(X,Y) :- edge(X,Y).
connected(X,Y) :- edge(X,Z) ; edge(Z,Y).  % example of disjunction
```

Determinism and search order
----------------------------

- Clauses are tried top‑to‑bottom; goals in a body run left‑to‑right.
- Backtracking finds alternative solutions by revisiting choice points (other clauses, other matches).
- Prefer placing deterministic tests early in bodies to prune search.

Rules with conditions
---------------------

You can use arithmetic comparisons or constraints in bodies to guard rules:

```prolog
% max of two integers
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

% within a range using CLP(FD)
in_range(X, A, B) :- X in A..B.
```

Predicate arity and multi‑clause definitions
-------------------------------------------

- A predicate is identified by its name and arity, e.g. `ancestor/2`.
- You can define a predicate with multiple clauses; they are alternatives tried in order.

Good style
----------

- Separate base and recursive cases; put the base case first when it helps termination/readability.
- Use meaningful variable names (`Parent`, `Child`) to improve trace readability.
- Keep each clause focused; extract helpers for clarity.
