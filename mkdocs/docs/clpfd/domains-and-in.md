# Domains and `in/2`

Use `in/2` to post an integer domain for a variable. Domains restrict possible values and drive propagation.

Domain forms
------------

- Interval: `Low..High` (inclusive bounds)
- Union: `D1 \/ D2` (domain union)
- Finite set: `{1,3,5}`

Examples
--------

```text
?- X in 1..9, Y in {1,3,5}.
true.

?- X in 5..5.
% X is fixed to a single value
true.
```

Combining domains
-----------------

Posting multiple `in/2` goals for the same variable intersects domains.

```text
?- X in 1..10, X in {2,4,6,8,10}.
true.
?- label([X]).
X = 2 ; X = 4 ; X = 6 ; X = 8 ; X = 10.
```

Unions and holes
----------------

Use union `\/` to create non‑contiguous domains.

```text
?- X in (1..3) \/ (7..9).
true.
?- label([X]).
X = 1 ; X = 2 ; X = 3 ; X = 7 ; X = 8 ; X = 9.
```

Good practices
--------------

- Post domains early to maximize propagation.
- Use the tightest bounds you know; wide domains increase search.
- Prefer intervals/unions for performance over large explicit sets.
