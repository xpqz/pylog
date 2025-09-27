# Path with visited set

Avoid cycles by tracking visited nodes.

```prolog
edge(a,b). edge(b,c). edge(c,a). edge(c,d).

path(X, Y) :- path_(X, Y, [X]).

path_(X, Y, _) :- edge(X, Y).
path_(X, Z, Visited) :-
    edge(X, Y),
    \+ member(Y, Visited),
    path_(Y, Z, [Y|Visited]).

% member/2
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
```

```text
?- path(a, d).
true.

?- path(a, a).
false.  % cycle avoided by visited set
```
