# Runtime Database Predicates

PyLog supports a small set of runtime database predicates for adding,
removing, and declaring predicates during a REPL session.

These are intended for interactive use and simple tooling. They are not yet a
complete replacement for advanced database features in full Prolog systems.

## Predicates

- `dynamic(+PI)`
- `assertz(+Clause)`
- `asserta(+Clause)`
- `retract(+Clause)`
- `retractall(+HeadOrClause)`
- `abolish(+PI)`

Where `PI` is a predicate indicator `name/arity` (e.g., `parent/2`).

### dynamic/1

Declare one or more predicate indicators as dynamic (i.e., allowed to be
modified at runtime).

Forms accepted:

- Single: `dynamic(parent/2)`
- List: `dynamic([edge/2, path/2])`
- Comma‑conjunction: `dynamic((edge/2, path/2))`

A predicate must be declared dynamic before it can be modified with
`assertz/1`, `asserta/1`, `retract/1`, or `abolish/1`.

### assertz/1 and asserta/1

Add a clause to a dynamic predicate.

- `assertz(+Clause)` appends to the end of the predicate’s clause list.
- `asserta(+Clause)` inserts at the beginning.

Examples:

```prolog
?- dynamic(parent/2).
true.

?- assertz(parent(tom, bob)).
true.

?- asserta((grandparent(X, Z) :- parent(X, Y), parent(Y, Z))).
true.
```

`Clause` can be a fact (e.g., `p(1).`) or a rule written as `Head :- Body`.

### retract/1 and retractall/1

Remove clause(s) matching the given pattern from a dynamic predicate.

- `retract/1`:
  - Succeeds once per call, removing the first matching clause.
  - Matches by unification (variables in the pattern are bound on success).
  - Effects persist across backtracking (i.e., removed clauses stay removed).

- `retractall/1`:
  - Removes all matching clauses in one call.
  - Matches by unification but does not bind variables in the query.
  - Always succeeds (even if nothing matched).

Examples:

```prolog
?- dynamic(p/1), assertz(p(1)), assertz(p(2)).
true.

?- retract(p(X)).
X = 1.

?- p(1).
false.

?- p(2).
true.
```

Call `retract/1` again to remove the next matching clause.

Using retractall/1:

```prolog
?- dynamic(p/1), assertz(p(1)), assertz(p(2)).
true.

?- retractall(p(X)).
true.

?- p(X).
false.
```

### abolish/1

Remove all clauses of a dynamic predicate.

Forms accepted:

- Single: `abolish(parent/2)`
- List: `abolish([edge/2, path/2])`
- Comma‑conjunction: `abolish((edge/2, path/2))`

## Notes and limitations

- A predicate must be declared with `dynamic/1` before it can be changed at runtime.
- `retract/1` currently removes one clause per call. Invoke it repeatedly to remove multiple clauses.
- Updates preserve the engine’s indexing mode internally.
- These predicates are designed for the REPL and small scripts; they are not a full module system.

## See also

- REPL: [Loading code and interactive input](../getting-started/repl.md#loading-code)
- REPL commands: [Reference](./repl-commands.md)
