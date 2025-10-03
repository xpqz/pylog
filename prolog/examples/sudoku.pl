% Sudoku solver for PyLog (CLP(FD))
%  - Provides sudoku0/2 (zeros to vars + solve)
%  - Provides sudoku/1 (constraints + labeling)
%  - Includes a sample puzzle/1 fact

% Entry with 0-as-blank input:
% sudoku0(InRows, OutRows) replaces 0s by fresh vars, posts constraints, and labels.
sudoku0(In, Out) :-
    copy_rows(In, Out),
    sudoku(Out).

% Core constraints on a 9x9 grid (list of 9 lists of 9 vars/ints)
sudoku(Rows) :-
    ins_rows(Rows),                     % Domains 1..9
    rows_all_different9(Rows),          % All rows all_different (length-9 rows)
    transpose(Rows, Cols),
    rows_all_different9(Cols),          % All cols all_different
    all_blocks(Rows),                   % 3x3 blocks all_different
    flatten_rows(Rows, Vars),
    once(labeling([first_fail], Vars)). % First solution with MRV-like heuristic

% Replace 0s with fresh variables
copy_rows([], []).
copy_rows([R|Rs], [R2|Rs2]) :- copy_row(R, R2), copy_rows(Rs, Rs2).
copy_row([], []).
copy_row([0|Xs], [_|Ys]) :- copy_row(Xs, Ys).
copy_row([N|Xs], [N|Ys]) :- N #>= 1, N #=< 9, copy_row(Xs, Ys).

% Set domains 1..9 for all cells
ins_rows([]).
ins_rows([R|Rs]) :- ins_row(R), ins_rows(Rs).
ins_row([]).
ins_row([X|Xs]) :- X in 1..9, ins_row(Xs).

% Enforce all_different on each row of a matrix
% Enforce all_different on each row with explicit 9-length destructuring to
% pass a concrete list literal to the builtin (avoids deref-var issue).
rows_all_different9([]).
rows_all_different9([R|Rs]) :-
    R = [A,B,C,D,E,F,G,H,I],
    all_different([A,B,C,D,E,F,G,H,I]),
    rows_all_different9(Rs).

% Transpose a matrix
transpose([[]|_], []).
transpose(Matrix, [Heads|RestT]) :-
    heads_tails(Matrix, Heads, Tails),
    transpose(Tails, RestT).

heads_tails([], [], []).
heads_tails([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    heads_tails(Rows, Hs, Ts).

% 3x3 block constraints (Rows grouped in triples)
all_blocks([]).
all_blocks([R1,R2,R3|Rs]) :-
    row_blocks(R1,R2,R3,Bs),
    all_different_each(Bs),
    all_blocks(Rs).

% Split three rows into 3 blocks each
row_blocks([], [], [], []).
row_blocks([A1,A2,A3|AR], [B1,B2,B3|BR], [C1,C2,C3|CR],
           [[A1,A2,A3,B1,B2,B3,C1,C2,C3] | Tail]) :-
    row_blocks(AR, BR, CR, Tail).

all_different_each([]).
all_different_each([Block|Blocks]) :-
    all_different(Block),
    all_different_each(Blocks).

% Flatten matrix to a single list
flatten_rows([], []).
flatten_rows([R|Rs], Vs) :- flatten_rows(Rs, Vt), append2(R, Vt, Vs).

append2([], L, L).
append2([H|T], L, [H|R]) :- append2(T, L, R).

% Example puzzle (0 = blank)
puzzle([
  [0,0,0, 2,6,0, 7,0,1],
  [6,8,0, 0,7,0, 0,9,0],
  [1,9,0, 0,0,4, 5,0,0],

  [8,2,0, 1,0,0, 0,4,0],
  [0,0,4, 6,0,2, 9,0,0],
  [0,5,0, 0,0,3, 0,2,8],

  [0,0,9, 3,0,0, 0,7,4],
  [0,4,0, 0,5,0, 0,3,6],
  [7,0,3, 0,1,8, 0,0,0]
]).

