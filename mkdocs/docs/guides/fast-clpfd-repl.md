# Fast CLP(FD) in the REPL

Practical tips and ready‑to‑run REPL sessions for writing fast CLP(FD) programs.

Key techniques
--------------

- Constrain early: post domains and structural constraints before search.
- Use `all_different/1` instead of many pairwise `#\=/2`.
- Choose a good variable order: `labeling([first_fail], Vars)` tries the smallest domain first.
- If only the first solution is needed, wrap in `once/1`.
- Keep domains tight (e.g., leading digits in 1..9 instead of 0..9).

SEND+MORE=MONEY
----------------

```text
?- S in 1..9, E in 0..9, N in 0..9, D in 0..9,
   M in 1..9, O in 0..9, R in 0..9, Y in 0..9,
   all_different([S,E,N,D,M,O,R,Y]),
   1000*S + 100*E + 10*N + D + 1000*M + 100*O + 10*R + E #=
   10000*M + 1000*O + 100*N + 10*E + Y,
   once(labeling([first_fail], [S,E,N,D,M,O,R,Y])).
S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2.
```

Sudoku (row, column, box)
-------------------------

Start with a row (9 variables, all different):

```text
?- Row = [A,B,C,D,E,F,G,H,I],
   A in 1..9, B in 1..9, C in 1..9, D in 1..9, E in 1..9,
   F in 1..9, G in 1..9, H in 1..9, I in 1..9,
   all_different(Row),
   labeling([first_fail], Row).
```

For a full 9×9 puzzle, assert nine rows with domains, add `all_different/1` for each row, each column, and each 3×3 box, then `labeling([first_fail], Vars)` where `Vars` flattens all 81 cells. Real puzzles solve quickly when you post all givens first.

N‑queens (8)
------------

```text
?- Qs = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8],
   Q1 in 1..8, Q2 in 1..8, Q3 in 1..8, Q4 in 1..8,
   Q5 in 1..8, Q6 in 1..8, Q7 in 1..8, Q8 in 1..8,
   all_different(Qs),
   % diagonals
   safe(Qs),
   labeling([first_fail], Qs).
```

where `safe/1` enforces diagonal separation (see Cookbook → N‑queens).

Scheduling (toy)
----------------

```text
?- Start1 in 0..10, Start2 in 0..10, Start3 in 0..10,
   D1 = 3, D2 = 2, D3 = 4,
   End1 #= Start1 + D1,
   End2 #= Start2 + D2,
   End3 #= Start3 + D3,
   End1 #=< Start2, End2 #=< Start3, End3 #=< 15,
   labeling([first_fail], [Start1,Start2,Start3]).
```

Troubleshooting slowdowns
-------------------------

- Are domains as tight as possible? Add implied bounds early.
- Can you add global constraints (`all_different/1`) or simple linear equalities to prune more?
- Try a different variable order: `first_fail`, `most_constrained`, or reorder the list passed to labeling.
- Use `trace on` in the REPL to watch search; excessive REDO often indicates weak propagation.

See also
--------

- CLP(FD) → Labeling for strategy options
- Cookbook → SEND+MORE=MONEY, Sudoku row, N‑queens
- Guides → Tracing and debugging
