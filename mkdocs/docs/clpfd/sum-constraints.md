# Sum Constraints

Sums of variables are common. Encode them as linear equalities or with a helper like `sum_list/2`.

Basic sums
----------

```text
?- X in 1..10, Y in 1..10, X + Y #= 10.
true.

?- A in 1..5, B in 1..5, C in 1..5, A + B + C #= 10.
true.
```

List sum helper
---------------

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

Weighted sums (scalar product)
------------------------------

```prolog
scalar_product([], [], 0).
scalar_product([W|Ws], [X|Xs], S) :- scalar_product(Ws, Xs, T), S #= T + W*X.
```

```text
?- Weights = [2,3,5], Vars = [A,B,C], ins(Vars, 0..3), scalar_product(Weights, Vars, Sum), Sum #=< 10.
true.
```

Tips
----

- Keep sums linear (variable times constant is fine; variable times variable is not).
- Combine with `all_different/1` and ordering constraints to narrow domains quickly.
