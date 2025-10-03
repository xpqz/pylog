#!/usr/bin/env python3
"""Vanilla Prolog Sudoku solver (no CLP(FD)).

Generate-and-test using permutation/2 for rows, columns, and 3x3 blocks.

Run:
  uv run python scripts/sudoku_vanilla.py
or
  python scripts/sudoku_vanilla.py
"""

import time
from prolog.engine.engine import Engine, Program


SUDOKU_VANILLA = r"""
% ----- Utility: select/3 and permutation/2 -----
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :- select(X, Ys, Zs).

permutation([], []).
permutation(L, [X|Xs]) :- select(X, L, L1), permutation(L1, Xs).

digits([1,2,3,4,5,6,7,8,9]).

% ----- Transpose (matrix) -----
transpose([[]|_], []).
transpose(Matrix, [Heads|RestT]) :-
    heads_tails(Matrix, Heads, Tails),
    transpose(Tails, RestT).

heads_tails([], [], []).
heads_tails([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    heads_tails(Rows, Hs, Ts).

% ----- Copy 0s to fresh variables (keep nonzero as givens) -----
copy_rows([], []).
copy_rows([R|Rs], [R2|Rs2]) :- copy_row(R, R2), copy_rows(Rs, Rs2).
copy_row([], []).
copy_row([0|Xs], [_|Ys]) :- copy_row(Xs, Ys).
copy_row([N|Xs], [N|Ys]) :- N \= 0, copy_row(Xs, Ys).

% ----- Block helpers -----
row_blocks([], [], [], []).
row_blocks([A1,A2,A3|AR], [B1,B2,B3|BR], [C1,C2,C3|CR],
           [[A1,A2,A3,B1,B2,B3,C1,C2,C3] | Tail]) :-
    row_blocks(AR, BR, CR, Tail).

all_blocks([]).
all_blocks([R1,R2,R3|Rs]) :-
    row_blocks(R1,R2,R3,Bs),
    blocks_ok(Bs),
    all_blocks(Rs).

blocks_ok([]).
blocks_ok([B|Bs]) :- digits(D), permutation(D, B), blocks_ok(Bs).

% ----- Core vanilla solver -----
sudoku_vanilla(In, Out) :-
    copy_rows(In, Out),
    % Constrain rows to be permutations of 1..9
    rows_ok(Out),
    % Columns must be permutations as well
    transpose(Out, Cols),
    rows_ok(Cols),
    % 3x3 blocks must be permutations
    all_blocks(Out).

rows_ok([]).
rows_ok([R|Rs]) :- digits(D), permutation(D, R), rows_ok(Rs).

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
    rows = []
    for row in sol_rows.items:
        out_row = []
        for cell in row.items:
            out_row.append(getattr(cell, "value", cell))
        rows.append(out_row)
    return rows


def main() -> None:
    engine = Engine(Program([]))
    engine.consult_string(SUDOKU_VANILLA)

    start = time.perf_counter()
    solutions = list(engine.query("?- puzzle(P), sudoku_vanilla(P, Sol)."))
    elapsed = time.perf_counter() - start

    if not solutions:
        print("No solution found (vanilla)")
        print(f"Elapsed: {elapsed:.3f}s")
        return

    sol_rows = solutions[0]["Sol"]
    pretty = _format_solution(sol_rows)
    for row in pretty:
        print(" ".join(str(x) for x in row))
    print(f"Elapsed (vanilla): {elapsed:.3f}s")


if __name__ == "__main__":
    main()
