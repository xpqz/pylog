# SEND+MORE=MONEY

Model digits with domains, enforce all‑different, and post the sum.

```prolog
sendmore(S,E,N,D,M,O,R,Y) :-
    S in 1..9, E in 0..9, N in 0..9, D in 0..9,
    M in 1..9, O in 0..9, R in 0..9, Y in 0..9,
    all_different([S,E,N,D,M,O,R,Y]),
    1000*S + 100*E + 10*N + D +
    1000*M + 100*O + 10*R + E #=
    10000*M + 1000*O + 100*N + 10*E + Y,
    label([S,E,N,D,M,O,R,Y]).
```

```text
?- sendmore(S,E,N,D,M,O,R,Y).
S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2.
```

