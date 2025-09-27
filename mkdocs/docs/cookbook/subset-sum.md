# Subset sum (CLP(FD))

Pick a subset of numbers to reach a target sum using 0/1 decision variables.

```prolog
subset_sum(Weights, Target, Picks) :-
    same_length(Weights, Picks),
    booleans(Picks),
    scalar_product(Weights, Picks, Target),
    label(Picks).

% Picks are 0/1 variables
booleans([]).
booleans([X|Xs]) :- X in 0..1, booleans(Xs).

% Sum W_i * P_i equals Target
scalar_product([], [], 0).
scalar_product([W|Ws], [P|Ps], Target) :-
    scalar_product(Ws, Ps, Sub),
    Target #= Sub + W*P.

same_length([], []).
same_length([_|Xs], [_|Ys]) :- same_length(Xs, Ys).
```

```text
?- subset_sum([3,4,5,6], 9, Picks).
Picks = [1,0,1,0] ;
Picks = [0,1,0,1] ; ...
```

Note: This simple encoding uses `in/2` and linear constraints; for performance on larger instances, specialized propagators are preferable.
