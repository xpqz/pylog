# Sorting (mergesort)

Stable O(n log n) sorting with a divide‑and‑conquer merge.

```prolog
msort([], []).
msort([X], [X]).
msort(L, S) :-
    split(L, L1, L2),
    msort(L1, S1),
    msort(L2, S2),
    merge(S1, S2, S).

split([], [], []).
split([X], [X], []).
split([X,Y|T], [X|L1], [Y|L2]) :- split(T, L1, L2).

merge([], Ys, Ys).
merge(Xs, [], Xs).
merge([X|Xs], [Y|Ys], [X|Zs]) :- X =< Y, merge(Xs, [Y|Ys], Zs).
merge([X|Xs], [Y|Ys], [Y|Zs]) :- X  > Y, merge([X|Xs], Ys, Zs).
```

```text
?- msort([3,1,4,1,5,2], S).
S = [1,1,2,3,4,5].
```

Note: Uses arithmetic comparisons `=</2` and `>/2`.
