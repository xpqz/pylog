# Control

Useful control built‑ins:

- `true/0` and `fail/0`
- cut `!/0`
- `once/1`
- `call/1`

Example:

```prolog
max(A, B, A) :- A >= B, !.
max(_, B, B).
```

```text
?- max(3, 5, M).
M = 5.
```

