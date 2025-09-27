# Flatten a nested list

Turn a nested list into a flat list. This version expects proper, ground lists.

```prolog
flatten([], []).
flatten([X|Xs], Ys) :-
    flatten(X, Y),
    flatten(Xs, YS),
    append(Y, YS, Ys).
flatten(X, [X]).  % base case for non-list elements

% append/3
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- flatten([a,[b,[c,d],[]],e], R).
R = [a,b,c,d,e].
```

Tip: For general use with variables and improper lists, more robust versions perform type checks.
