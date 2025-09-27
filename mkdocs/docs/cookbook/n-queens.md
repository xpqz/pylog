# N‑queens

Place N queens so none attack each other.

```prolog
nqueens8(Qs) :-
    Qs = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8],
    Q1 in 1..8, Q2 in 1..8, Q3 in 1..8, Q4 in 1..8,
    Q5 in 1..8, Q6 in 1..8, Q7 in 1..8, Q8 in 1..8,
    all_different(Qs),
    safe(Qs),
    label(Qs).

safe([]).
safe([Q|Qs]) :- safe_from(Q, Qs, 1), safe(Qs).

safe_from(_, [], _).
safe_from(Q, [R|Rs], D) :-
    Q #\= R + D, Q #\= R - D,
    D1 is D + 1,
    safe_from(Q, Rs, D1).
```

```text
?- nqueens8(Qs).
Qs = [1,5,8,6,3,7,2,4] ; ...
```

