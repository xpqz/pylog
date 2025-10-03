# Towers of Hanoi

Generate the sequence of moves for N disks as a list.

```prolog
hanoi(0, _, _, _, []).
hanoi(N, From, To, Aux, Moves) :-
    N > 0,
    N1 is N - 1,
    hanoi(N1, From, Aux, To, M1),
    hanoi(N1, Aux, To, From, M2),
    append(M1, [move(From,To)|M2], Moves).

% append/3
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

REPL usage
----------

Option A — load the example file, then query:

```text
?- consult('prolog/examples/hanoi.pl').
true.

?- hanoi(0, _, _, _, []).
true.

?- hanoi(3, left, right, middle, Moves).
Moves = [move(left,right),move(left,middle),move(right,middle),
         move(left,right),move(middle,left),move(middle,right),
         move(left,right)].
```

Option B — paste the code block above into the REPL, ensuring every clause ends with a period (`.`), then run the same queries.
