% Towers of Hanoi
% Generate the sequence of moves for N disks as a list.

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

