# Peano arithmetic

Natural numbers as `0` and successors `s(N)`. Define addition and multiplication.

```prolog
% add(X, Y, Z)  means X + Y = Z
add(0, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% mul(X, Y, Z)  means X * Y = Z
mul(0, _Y, 0).
mul(s(X), Y, Z) :- mul(X, Y, T), add(Y, T, Z).
```

```text
?- add(s(s(0)), s(0), R).
R = s(s(s(0))).

?- mul(s(s(0)), s(s(s(0))), R).
R = s(s(s(s(s(s(0)))))).  % 2 * 3 = 6
```

Tip: These are purely declarative and work in multiple directions (e.g., as relations).
