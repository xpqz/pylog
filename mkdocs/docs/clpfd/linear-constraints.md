# Linear Constraints

PyLog supports linear arithmetic relations over integers and enforces them by domain propagation.

Relations
---------

- Equality/disequality: `#=`, `#\=`
- Ordering: `#<`, `#=<`, `#>`, `#>=`

Examples
--------

```text
?- X in 1..10, Y in 1..10, X + Y #= 10.
true.  % domains are narrowed internally

?- X in 1..10, Y in 1..10, 2*X + 3*Y #=< 20.
true.

?- X in 1..10, Y in 1..10, X - Y #> 3.
true.
```

Propagation insight
-------------------

Constraints narrow variable domains before any search. For `X+Y #= 10` with `X,Y in 1..10`, both domains become `1..9` since a variable cannot be 10 unless the other is 0 (outside the domain).

Difference constraints
----------------------

Constraints of the form `X - Y #=< C` (and relatives) propagate efficiently and are useful to encode precedence and spacing.

```text
?- S1 in 0..10, S2 in 0..10, E1 #= S1 + 3, E2 #= S2 + 2, E1 #=< S2.
true.
```

Linearity requirement
---------------------

All terms must be linear combinations of variables and integers. Non‑linear terms such as `X*Y` or `X*X` are rejected.

```text
?- X in 1..9, Y in 1..9, X*Y #= 6.
% error: non‑linear term not supported
```

Modeling sums
-------------

Sum constraints can be modeled by a chain of linear relations.

```prolog
sum_list([], 0).
sum_list([X|Xs], S) :- sum_list(Xs, T), S #= X + T.

% Helper for posting domains to multiple variables
ins([], _).
ins([V|Vs], Dom) :- V in Dom, ins(Vs, Dom).
```

```text
?- Vs = [X,Y,Z], ins(Vs, 1..3), sum_list(Vs, 6), label(Vs).
X = 1, Y = 2, Z = 3 ; ...
```

Tips
----

- Post constraints as equations/inequalities; avoid mixing with `is/2` (which evaluates) in constraint models.
- Normalize expressions to make linearity obvious (e.g., `2*X + 3*Y #=< 20`).
