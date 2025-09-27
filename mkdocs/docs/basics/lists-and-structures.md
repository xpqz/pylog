# Lists and Structures

Lists are either empty `[]` or `Head|Tail` pairs.

```text
?- [H|T] = [1,2,3].
H = 1, T = [2, 3].
```

Compound terms (structures) have a functor and arguments.

```text
?- functor(point(1,2), F, N).
F = point, N = 2.

?- arg(2, point(1,2), X).
X = 2.
```

