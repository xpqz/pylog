# Sum Constraints

Sums of variables are common. You can write them as a linear equality.

Examples:

```text
?- X in 1..10, Y in 1..10, X + Y #= 10.
true.

?- A in 1..5, B in 1..5, C in 1..5, A + B + C #= 10.
true.
```

The solver narrows bounds before search. Use `label/1` to obtain concrete solutions.

```text
?- X in 1..3, Y in 1..3, Z in 1..3, X+Y+Z #= 6, label([X,Y,Z]).
X = 1, Y = 2, Z = 3 ;
...
```

