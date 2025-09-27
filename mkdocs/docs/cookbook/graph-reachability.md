# Graph reachability

Define edges and a transitive path.

```prolog
edge(a, b). edge(b, c). edge(c, d).
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

```text
?- path(a, d).
true.
```

