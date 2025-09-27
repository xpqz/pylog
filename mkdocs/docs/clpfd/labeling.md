# Labelling

Labelling assigns concrete values to variables after propagation has reduced their domains. Use `label/1` on a list of variables.

Basic usage
-----------

```text
?- X in 1..3, Y in 1..3, X #< Y, label([X,Y]).
X = 1, Y = 2 ;
X = 1, Y = 3 ;
X = 2, Y = 3.
```

Order matters
-------------

The order of variables in `label/1` determines the search order. Place the most constrained variables first to reduce branching.

```text
?- X in 1..100, Y in 1..3, X #> Y, label([Y,X]).
% tries Y first (smaller domain), often faster
```

Tips
----

- Post as many constraints as possible before labelling to maximize propagation.
- Try different variable orders if search seems slow.
- For large problems, consider bespoke search (branch‑and‑bound) in future stages.
