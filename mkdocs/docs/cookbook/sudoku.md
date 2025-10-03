# Sudoku (9x9)

Solve a 9×9 Sudoku with CLP(FD): domains 1..9, all_different for rows, columns, and 3×3 boxes, then label with a good heuristic.

Model
-----

```prolog
% Replace zeros with fresh variables, then solve
sudoku0(In, Out) :-
    copy_rows(In, Out),
    sudoku(Out).

% Core constraints on a 9×9 grid (list of 9 lists of 9 vars/ints)
sudoku(Rows) :-
    ins_rows(Rows),
    rows_all_different9(Rows),
    transpose(Rows, Cols),
    rows_all_different9(Cols),
    all_blocks(Rows),
    flatten_rows(Rows, Vars),
    once(labeling([first_fail], Vars)).  % first solution fast

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

% Enforce all_different on each row of a matrix (explicit 9‑length rows)
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
heads_tails([[H|T]|Rows], [H|Hs], [T|Ts]) :- heads_tails(Rows, Hs, Ts).

% 3×3 block constraints (rows grouped in triples)
all_blocks([]).
all_blocks([R1,R2,R3|Rs]) :-
    row_blocks(R1,R2,R3,Bs),
    all_different_each(Bs),
    all_blocks(Rs).
row_blocks([], [], [], []).
row_blocks([A1,A2,A3|AR], [B1,B2,B3|BR], [C1,C2,C3|CR],
           [[A1,A2,A3,B1,B2,B3,C1,C2,C3] | Tail]) :-
    row_blocks(AR, BR, CR, Tail).
all_different_each([]).
all_different_each([Block|Blocks]) :- all_different(Block), all_different_each(Blocks).

% Flatten matrix to a single list
flatten_rows([], []).
flatten_rows([R|Rs], Vs) :- flatten_rows(Rs, Vt), append2(R, Vt, Vs).
append2([], L, L).
append2([H|T], L, [H|R]) :- append2(T, L, R).
```

Example puzzle (0 = blank)
--------------------------

```prolog
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
```

REPL session
------------

Option A — load example file, then solve:

```text
?- consult('prolog/examples/sudoku.pl').
true.
```

```text
?- puzzle(P), sudoku0(P, Sol).
Sol = [[4,3,5,2,6,9,7,8,1],
       [6,8,2,5,7,1,4,9,3],
       [1,9,7,8,3,4,5,6,2],
       [8,2,6,1,9,5,3,4,7],
       [3,7,4,6,8,2,9,1,5],
       [9,5,1,7,4,3,6,2,8],
       [5,1,9,3,2,6,8,7,4],
       [2,4,8,9,5,7,1,3,6],
       [7,6,3,4,1,8,2,5,9]].
```

Option B — paste the model into the REPL, then run the same query.

```text
?- puzzle(P), sudoku0(P, Sol).
Sol = [[4,3,5,2,6,9,7,8,1],
       [6,8,2,5,7,1,4,9,3],
       [1,9,7,8,3,4,5,6,2],
       [8,2,6,1,9,5,3,4,7],
       [3,7,4,6,8,2,9,1,5],
       [9,5,1,7,4,3,6,2,8],
       [5,1,9,3,2,6,8,7,4],
       [2,4,8,9,5,7,1,3,6],
       [7,6,3,4,1,8,2,5,9]].
```

Notes
-----

- Use `once/1` to stop after the first solution; removing it enumerates all (for puzzles, that’s usually one).
- `labeling([first_fail], Vars)` (MRV) typically reduces backtracking dramatically versus `label/1`.
- Post all givens (non‑zeros) before labeling so propagation can prune domains early.

See also
--------

- Guides → Fast CLP(FD) in REPL
- Cookbook → Sudoku row
- CLP(FD) → Labeling
