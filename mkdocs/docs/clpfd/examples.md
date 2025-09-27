# CLP(FD) Examples

This section collects short examples that combine domain posting, constraints, and labelling.

Two variables summing to ten:

```text
?- X in 1..9, Y in 1..9, X + Y #= 10, label([X,Y]).
X = 1, Y = 9 ; ...
```

Distinct choices:

```text
?- X in 1..3, Y in 1..3, all_different([X,Y]), label([X,Y]).
X = 1, Y = 2 ; X = 1, Y = 3 ; ...
```

Reification (parsing example):

```text
?- B in 0..1, B #==> (X #< 10).
```
See CLP(FD) → Reification (preview) for details.
