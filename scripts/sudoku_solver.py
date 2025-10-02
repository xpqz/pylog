#!/usr/bin/env python3
"""Sudoku solver using PyLog's CLP(FD).

Run:
  uv run python scripts/sudoku_solver.py
or
  python scripts/sudoku_solver.py

This loads a Prolog program that models Sudoku with all_different/1 and solves
an example puzzle (0 = blank), then prints the solved grid.
"""

from prolog.engine.engine import Engine, Program


SUDOKU_PROLOG = r"""
% Sudoku solver for PyLog (CLP(FD))

% Entry with 0-as-blank input:
% sudoku0(InRows, OutRows) replaces 0s by fresh vars, posts constraints, and labels.
sudoku0(In, Out) :-
    copy_rows(In, Out),
    sudoku(Out).

% Core constraints on a 9x9 grid (list of 9 lists of 9 vars/ints)
sudoku(Rows) :-
    ins_rows(Rows),                     % Domains 1..9
    rows_all_different(Rows),           % All rows all_different
    transpose(Rows, Cols),
    rows_all_different(Cols),           % All cols all_different
    all_blocks(Rows),                   % 3x3 blocks all_different
    flatten_rows(Rows, Vars),
    once(label(Vars)).                  % First solution fast

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
rows_all_different([]).
rows_all_different([R|Rs]) :- all_different(R), rows_all_different(Rs).

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
all_different_each([L|Ls]) :- all_different(L), all_different_each(Ls).

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
"""


def _format_solution(sol_rows):
    """Return a list of Python lists with plain ints from a Prolog list of lists."""
    rows = []
    for row in sol_rows.items:  # row is a List term
        out_row = []
        for cell in row.items:
            # Cells should be Int terms after labeling
            out_row.append(getattr(cell, "value", cell))
        rows.append(out_row)
    return rows


def main() -> None:
    engine = Engine(Program([]))
    engine.consult_string(SUDOKU_PROLOG)

    query = "?- puzzle(P), sudoku0(P, Sol)."
    sols = list(engine.query(query))
    if not sols:
        print("No solution found")
        return

    sol_rows = sols[0]["Sol"]
    pretty = _format_solution(sol_rows)
    for row in pretty:
        print(" ".join(str(x) for x in row))


if __name__ == "__main__":
    main()
