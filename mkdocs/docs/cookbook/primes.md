# Primes

Trial division primality test and a simple enumerator up to N.

```prolog
is_prime(2).
is_prime(3).
is_prime(N) :-
    integer(N), N > 3, N mod 2 =\= 0,
    \+ has_factor(N, 3).

has_factor(N, F) :- N mod F =:= 0.
has_factor(N, F) :- F*F < N, F2 is F + 2, has_factor(N, F2).

primes_to(2, [2]).
primes_to(N, Ps) :-
    N > 2,
    N1 is N - 1,
    primes_to(N1, P1),
    ( is_prime(N) -> append(P1, [N], Ps) ; Ps = P1 ).

% append/3
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- primes_to(20, Ps).
Ps = [2,3,5,7,11,13,17,19].
```

Note: This is a simple approach for small N. More efficient sieves are possible but longer.
