# Terms

PyLog supports:

- atoms: `a`, `foo`, `[]`
- integers: `0`, `42`, `-3`
- variables: `X`, `Y`, `_G0`
- compound terms: `point(1,2)`, `edge(a,b)`
- lists: `[]`, `[a,b,c]`, `[H|T]`

Examples:

```text
?- X = point(1, 2).
X = point(1, 2).

?- [H|T] = [a,b,c].
H = a, T = [b, c].
```

