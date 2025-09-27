# Queries and Unification

A query asks whether a goal can be proven. Unification matches terms and binds variables.

```text
?- X = a.
X = a.

?- point(X, 2) = point(1, Y).
X = 1, Y = 2.

?- a = b.
false.
```

Negated unification fails if terms could unify:

```text
?- a \= b.
true.
?- X \= a.
false.
```

