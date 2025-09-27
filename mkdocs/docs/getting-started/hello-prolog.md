# Hello Prolog

Create a small knowledge base:

```prolog
parent(tom, bob).
parent(bob, ann).

ancestor(X, Z) :- parent(X, Z).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

Query in the REPL:

```text
?- parent(tom, bob).
true.

?- ancestor(tom, ann).
true.

?- ancestor(tom, Who).
Who = bob ;
Who = ann.
```

