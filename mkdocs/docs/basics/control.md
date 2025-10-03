# Control

Prolog offers control constructs for pruning search, structuring conditionals, and handling exceptions. This page covers the most common ones with examples and caveats.

Truth and failure
-----------------

- `true/0` always succeeds; `fail/0` always fails.

```text
?- true.
true.
?- fail.
false.
```

Negation as failure (\+)
------------------------

`\+/1` succeeds when its argument cannot be proven (i.e., fails), and fails when it succeeds.

```prolog
not_member(X, L) :- \+ member(X, L).
```

```text
?- \+ (1 = 2).
true.
?- \+ member(2, [1,2,3]).
false.
```

Note: Negation with non‑ground goals is non‑logical; prefer ground goals under `\+`.

Cut (!)
-------

The cut `!/0` commits to choices made so far in the current predicate, pruning alternative clauses and choice points to the left of the cut in the current clause.

Green‑cut example (prunes only redundant work):

```prolog
max(A, B, A) :- A >= B, !.
max(_, B, B).
```

```text
?- max(3, 5, M).
M = 5.
```

Avoid “red” cuts that change logical meaning unless intentional. Prefer guards that do not rely on cut if clarity suffers.

If–then–else (-> ;)
-------------------

Use `(If -> Then ; Else)` for conditionals. Parentheses are recommended due to operator precedence (`->` binds tighter than `;`, and `,` binds tighter than `->`).

```prolog
abs(X, Y) :- (X >= 0 -> Y is X ; Y is -X).
```

```text
?- abs(-3, Y).
Y = 3.
```

Remember: `A, B -> C ; D` parses as `((A, B) -> C) ; D`.

Parentheses for grouping
------------------------

You can group sub‑goals in clause bodies, queries, and directives using parentheses. This improves readability and clarifies precedence when mixing `,`, `;`, and `->`.

```prolog
% Equivalent forms
p :- (a, b).
p :- a, b.

% Grouping helps when combining with disjunction or if–then–else
q :- (a, b -> c ; d).
```

once/1
------

`once(G)` runs goal `G` and commits to the first solution (as if `G, !`). Useful to avoid exploring alternatives.

```text
?- once(member(X, [a,b,c])).
X = a.
```

call/N
------

`call/1` meta‑calls a term as a goal. It is handy when constructing goals dynamically.

```text
?- G = member(X, [1,2,3]), call(G).
G = member(X, [1,2,3]), X = 1 ;
G = member(X, [1,2,3]), X = 2 ;
G = member(X, [1,2,3]), X = 3 ;
false.
```

Exceptions: throw/1 and catch/3
--------------------------------

Raise and handle exceptions with `throw/1` and `catch/3`.

```prolog
safe_div(X, 0, _) :- throw(error(division_by_zero)).
safe_div(X, Y, R) :- Y =\= 0, R is X / Y.
```

```text
?- catch(safe_div(10, 0, R), error(E), (R = error(E))).
R = error(division_by_zero).

?- catch(safe_div(10, 2, R), error(_), fail).
R = 5.0.
```

Notes:

- The catcher pattern unifies with the thrown term; any bindings (e.g. variables in the pattern) are visible to the recovery goal.
- Prefer specific catcher patterns (e.g. `error(division_by_zero)`) to avoid masking unrelated errors.

Disjunction and commitment
--------------------------

`;` offers alternatives. Combined with cut, you can commit to an alternative:

```prolog
select_first_even(L, X) :- ( member(X, L), 0 is X mod 2 -> ! ; fail ).
```

But often a clearer formulation avoids cut:

```prolog
first_even(L, X) :- member(X, L), 0 is X mod 2, !.
```

Tips
----

- Use cut for performance/commitment (green cuts) after correctness is clear.
- Prefer if–then–else for clear branching; add parentheses when mixing `,`, `->`, and `;`.
- Use `once/1` when only the first solution matters.
- Keep negation goals ground to avoid surprises with `\+`.
