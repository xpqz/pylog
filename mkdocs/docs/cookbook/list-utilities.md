# List utilities

Last element, Nth element, prefixes and suffixes.

```prolog
% last/2
last_([X], X).
last_([_|T], X) :- last_(T, X).

% nth1/3 (1-based index)
nth1(1, [X|_], X).
nth1(N, [_|T], X) :- N > 1, N1 is N - 1, nth1(N1, T, X).

% prefix/2 and suffix/2 via append/3
prefix(P, L) :- append(P, _, L).
suffix(S, L) :- append(_, S, L).

% append/3
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- last_([a,b,c], X).
X = c.

?- nth1(3, [10,20,30,40], X).
X = 30.

?- prefix(P, [1,2,3]).
P = [] ; P = [1] ; P = [1,2] ; P = [1,2,3].
```

