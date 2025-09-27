# all_different/1

`all_different(List)` enforces that all variables in `List` take distinct values.

Propagation uses value elimination and Hall‑interval pruning.

Example:

```text
?- X in 1..2, Y in 1..2, Z in 1..3, all_different([X,Y,Z]).
true.
?- fd_dom(Z, DZ).
DZ = 3..3.
```

Pigeonhole conflicts fail early:

```text
?- X in 1..2, Y in 1..2, Z in 1..2, all_different([X,Y,Z]).
false.
```

