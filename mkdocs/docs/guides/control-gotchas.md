# Control gotchas (with counterexamples)

Common pitfalls with cuts, negation, conditionals, and search order — and how to fix them.

Cut scope and unintended commitment
-----------------------------------

A cut only commits within the current predicate clause; it does not jump across predicate boundaries. Using it to change logical meaning (a “red cut”) can hide valid solutions.

```prolog
% Intended: max of A,B is A if A>=B else B
max_bad(A,B,A) :- A >= B, !.
max_bad(_,B,B).
```

Counterexample:

```text
?- max_bad(3, 5, 3).
false.  % OK
?- max_bad(3, 5, 5).
true.   % OK
?- max_bad(3, 3, M).
M = 3.  % OK, but cut also commits even if later goals would fail
```

Prefer a formulation whose correctness doesn’t rely on cut:

```prolog
max_ok(A,B,A) :- A >= B.
max_ok(A,B,B) :- A < B.
```

If–then–else precedence surprises
---------------------------------

`->` binds tighter than `;`, and `,` binds tighter than `->`. Missing parentheses can change meaning.

```prolog
p :- a, b -> c ; d.
```
Parses as `((a, b) -> c) ; d`, not `(a, (b -> c)) ; d`.

Counterexample:

```prolog
ok :- true, fail -> writeln(then) ; writeln(else).
```

```text
?- ok.
else
true.
```

Always parenthesize:

```prolog
p :- (a, b -> c ; d).
```

If–then–else does not backtrack into else
-----------------------------------------

Once `If` succeeds, control commits to `Then`. If `Then` later fails, `Else` is not tried.

```prolog
demo :- ( member(X,[1,2]), X>5 -> writeln(big) ; writeln(small_or_none) ).
```

```text
?- demo.
small_or_none
true.
```

To get “try else if Then fails”, encode explicitly:

```prolog
demo2 :- ( member(X,[1,2]), X>5 -> writeln(big) ; true ), writeln(done).
```

Non‑ground negation (\+) is non‑logical
---------------------------------------

`\+ Goal` is sound only when `Goal` is ground. With variables, it may succeed merely because `Goal` hasn’t been instantiated yet.

Counterexample:

```text
?- \+ (X = 1).
true.   % X can still be 1 later
```

Safer pattern: ensure groundness or restructure logic so negation applies to ground checks.

Cut in disjunction needs grouping
---------------------------------

```prolog
p :- a, ! ; b.
```

Parses as `(a, !) ; b`. Often the intent is: if `a` then commit, else try `b`.

Write with if–then–else:

```prolog
p :- ( a -> true ; b ).
```

Or with explicit parentheses and cut scope:

```prolog
p :- ( a, ! ) ; b.
```

Cuts don’t affect callers
-------------------------

A cut inside `q/0` does not prune alternatives in its caller `p/0`.

```prolog
q :- a, !, b.

p :- q ; r.
```

Even if `q` commits to its path, `p` still has the alternative `r` if `q` fails.

Swallowing exceptions with catch/3
----------------------------------

Catching too broadly hides bugs:

```prolog
unsafe(Goal, Result) :- catch(Goal, _, Result = ok).
```

This converts any error into `ok`. Prefer precise patterns and handle only expected errors:

```prolog
safe_div(X,0,_) :- throw(error(division_by_zero)).
safe_div(X,Y,R) :- Y =\= 0, R is X / Y.

use_div(X,Y,R) :- catch(safe_div(X,Y,R), error(division_by_zero), R = inf).
```

Search order and non‑termination
--------------------------------

Left‑recursive definitions or unguarded recursion can loop before producing answers.

```prolog
ancestor_bad(X,Z) :- ancestor_bad(X,Y), parent(Y,Z).   % left recursion first
ancestor_bad(X,Z) :- parent(X,Z).
```

Prefer placing the shrinking step first:

```prolog
ancestor_ok(X,Z) :- parent(X,Y), ancestor_ok(Y,Z).
ancestor_ok(X,Z) :- parent(X,Z).
```

`once/1` versus cut
-------------------

`once(G)` commits to the first solution of `G` without needing to place `!` correctly.

```text
?- once(member(X,[a,b,c])).
X = a.
```

Use `once/1` for local commitment; avoid scattering cuts for readability unless necessary.

Checklist
---------

- Parenthesize `(If -> Then ; Else)` when mixing with `,` and `;`.
- Keep goals under `\+` ground, or refactor.
- Prefer green cuts; avoid changing logical meaning.
- Don’t expect `Else` to run if `Then` fails after `If` succeeded.
- Be specific with `catch/3` patterns.
- Order recursive calls to make progress; avoid left‑recursion unless controlled.
