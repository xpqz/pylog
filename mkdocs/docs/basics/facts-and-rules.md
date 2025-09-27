# Facts and Rules

Facts state things that are true. Rules define truths that depend on other goals.

```prolog
edge(a, b).
edge(b, c).

path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

Querying:

```text
?- path(a, c).
true.
```

