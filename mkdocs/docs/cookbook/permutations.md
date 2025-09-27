# Permutations

Generate all permutations of a list using selection.

```prolog
perm([], []).
perm(L, [X|R]) :- select(X, L, T), perm(T, R).

% select(Element, List, Remainder)
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :- select(X, Ys, Zs).
```

```text
?- perm([1,2,3], P).
P = [1,2,3] ; P = [1,3,2] ; P = [2,1,3] ; ...
```

