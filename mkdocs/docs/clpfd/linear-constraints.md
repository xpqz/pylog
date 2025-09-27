# Linear Constraints

PyLog parses linear arithmetic on integers and enforces it during propagation.

Supported relations:

- equality and disequality: `#=`, `#\=`
- ordering: `#<`, `#=<`, `#>`, `#>=`

Examples:

```text
?- X in 1..10, Y in 1..10, X + Y #= 10.
true.  % domains are narrowed internally
?- fd_dom(X, DX), fd_dom(Y, DY).
DX = 1..9, DY = 1..9.

?- X in 1..10, Y in 1..10, 2*X + 3*Y #=< 20.
true.
```

Non‑linear terms such as `X*Y` are rejected.

