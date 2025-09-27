# all_different/1

`all_different(List)` enforces pairwise distinctness of all variables in `List`.

Propagation
-----------

The propagator prunes values using value elimination and Hall‑interval reasoning. This often fixes variables early when domains are tight.

Examples
--------

```text
?- X in 1..2, Y in 1..2, Z in 1..3, all_different([X,Y,Z]).
true.
?- fd_dom(Z, DZ).
DZ = 3..3.
```

Pigeonhole principle
--------------------

If you have more variables than available values, failure is detected without search.

```text
?- X in 1..2, Y in 1..2, Z in 1..2, all_different([X,Y,Z]).
false.
```

Tips
----

- Combine with sum/ordering constraints for stronger propagation.
- Avoid redundant `#\=` pairs when `all_different/1` is present; let the global constraint propagate.
