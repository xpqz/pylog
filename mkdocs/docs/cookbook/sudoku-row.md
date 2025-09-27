# Sudoku row

Nine variables, domain 1..9, all different.

```prolog
row(A,B,C,D,E,F,G,H,I) :-
    A in 1..9, B in 1..9, C in 1..9, D in 1..9, E in 1..9, F in 1..9, G in 1..9, H in 1..9, I in 1..9,
    all_different([A,B,C,D,E,F,G,H,I]),
    label([A,B,C,D,E,F,G,H,I]).
```

```text
?- row(A,B,C,D,E,F,G,H,I).
... many permutations of 1..9 ...
```

