# Scheduling

Simple precedence constraints.

```prolog
schedule(S1,S2,S3) :-
  S1 in 0..10, S2 in 0..10, S3 in 0..10,
  E1 #= S1 + 3, E2 #= S2 + 2, E3 #= S3 + 4,
  E1 #=< S2,  % 1 before 2
  E2 #=< S3,  % 2 before 3
  E3 #=< 15,
  label([S1,S2,S3]).
```

```text
?- schedule(S1,S2,S3).
S1 = 0, S2 = 3, S3 = 5 ; ...
```

