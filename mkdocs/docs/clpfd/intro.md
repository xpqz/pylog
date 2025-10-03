# CLP(FD) Introduction

Constraint Logic Programming over Finite Domains (CLP(FD)) lets you state relations between integer variables and prune their possible values before search.

You post domain limits with `in/2`, add constraints such as `#=`, and then search with `label/1` to obtain concrete values.

Example:

```text
?- X in 1..9, Y in 1..9, X + Y #= 10, label([X,Y]).
X = 1, Y = 9 ;
...
X = 9, Y = 1.
```

PyLog also supports CLP(FD) reification operators that relate a Boolean to a
constraint's truth, for example `B #<=> (X #= Y)` or `B #==> (X #< 5)`.
See CLP(FD) → Reification.
