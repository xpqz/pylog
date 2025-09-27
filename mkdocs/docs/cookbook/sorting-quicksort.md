# Sorting (quicksort)

Declarative quicksort over lists.

```prolog
qsort([], []).
qsort([X|Xs], Ys) :-
    partition_(Xs, X, Ls, Gs),
    qsort(Ls, LsS),
    qsort(Gs, GsS),
    append(LsS, [X|GsS], Ys).

partition_([], _Pivot, [], []).
partition_([X|Xs], Pivot, [X|Ls], Gs) :- X =< Pivot, partition_(Xs, Pivot, Ls, Gs).
partition_([X|Xs], Pivot, Ls, [X|Gs]) :- X  > Pivot, partition_(Xs, Pivot, Ls, Gs).

% append/3
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- qsort([3,1,4,1,5,2], S).
S = [1,1,2,3,4,5].
```

Note: This version is not stable and uses `=</2` and `>/2` arithmetic comparisons.
