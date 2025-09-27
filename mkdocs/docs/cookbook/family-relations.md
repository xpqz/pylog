# Family relations

Facts and simple recursion.

```prolog
parent(tom, bob).
parent(bob, ann).

ancestor(X, Z) :- parent(X, Z).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

```text
?- ancestor(tom, ann).
true.
```

