# Labelling

Labelling assigns concrete values to variables after propagation has reduced their domains. Use `label/1` on a list of variables.

Basic usage
-----------

```text
?- X in 1..3, Y in 1..3, X #< Y, label([X,Y]).
X = 1, Y = 2 ;
X = 1, Y = 3 ;
X = 2, Y = 3.
```

Order matters
-------------

The order of variables in `label/1` determines the search order. Place the most constrained variables first to reduce branching.

```text
?- X in 1..100, Y in 1..3, X #> Y, label([Y,X]).
% tries Y first (smaller domain), often faster
```

Tips
----

- Post as many constraints as possible before labelling to maximize propagation.
- Try different variable orders if search seems slow.
- For large problems, consider bespoke search (branch‑and‑bound) in future stages.

Strategies (labeling/2)
-----------------------

Use `labeling(Options, Vars)` to guide search:

- Variable selection: `first` (default), `first_fail`, `most_constrained`, `smallest`, `largest`
- Value selection: `indomain_min` (default), `indomain_max`, `indomain_middle`, `indomain_random`, `indomain_split`
- Random: include `seed(N)` with `indomain_random` for reproducible runs

Examples:

```text
?- X in 1..100, Y in 1..3, X #> Y,
   labeling([first_fail, indomain_min], [X,Y]).

?- Vs = [A,B,C], A in 1..9, B in 1..9, C in 1..9, all_different(Vs),
   labeling([most_constrained, indomain_middle], Vs).

?- Vs = [X,Y,Z], X in 1..50, Y in 1..50, Z in 1..50,
   labeling([first_fail, indomain_random, seed(42)], Vs).
```

Only need one solution? Wrap with `once/1`:

```text
?- constraints(...), once(labeling([first_fail], Vars)).
```

Booleans and search order
-------------------------

- Boolean variables used in reification take values in `{0,1}`. You can include them in `label/1` alongside integer variables.
- Search order still matters: placing Booleans before integers can change the shape of the search tree. The solver enumerates all solutions; choose the order that best prunes your model.
- When mixing `,`, `;`, and `->` around labelling, prefer explicit parentheses to make intent clear, e.g. `label(Vs) ; (post_more, label(Vs))`.
