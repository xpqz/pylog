# Lists and Structures

Prolog programs are built from lists and compound terms (structures). This page shows how to construct, deconstruct, and traverse them.

Lists
-----

- A list is either the empty list `[]` or a non‑empty pair `[Head|Tail]` where `Tail` is a list.
- Literal lists are written with brackets: `[a,b,c]` is shorthand for `[a|[b|[c|[]]]]`.

Matching and deconstruction:

```text
?- [H|T] = [1,2,3].
H = 1, T = [2, 3].

?- [X,Y|Rest] = [a,b,c,d].
X = a, Y = b, Rest = [c, d].

?- [A,B] = [1].
false.  % lengths must match for fixed-length patterns
```

Proper vs improper lists:

```text
?- [a|[]] = [a].
true.

?- [a|b].
[a|b].  % an improper list (tail not a list) – avoid unless intentional
```

Lists are just terms with functor `'.'/2` (dot) under the hood:

```text
?- [a,b] =.. L.
L = ['.', a, ['.', b, []]].
```

Common list predicates (examples)
---------------------------------

member/2 and append/3 illustrate head‑tail recursion:

```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- member(2, [1,2,3]).
true ; false.

?- append([1,2], [3,4], Z).
Z = [1, 2, 3, 4].
```

Traversing lists (length, map)
------------------------------

```prolog
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.

inc_list([], []).
inc_list([X|Xs], [Y|Ys]) :- Y is X + 1, inc_list(Xs, Ys).
```

```text
?- len([a,b,c], N).
N = 3.

?- inc_list([1,2,3], R).
R = [2, 3, 4].
```

Structures (compound terms)
---------------------------

- A structure has a functor (name) and a fixed number of arguments (arity).
- Examples: `point(1,2)` is a `point/2`; `tree(L,Key,R)` is a `tree/3`.

Inspecting and constructing structures:

```text
?- functor(point(1,2), F, N).
F = point, N = 2.

?- arg(2, point(1,2), X).
X = 2.

?- T =.. [edge, a, b].
T = edge(a, b).

?- edge(a,b) =.. L.
L = [edge, a, b].
```

Pattern matching on structures
------------------------------

You can destructure by unification:

```text
?- point(X, Y) = point(10, 20).
X = 10, Y = 20.

?- tree(L, K, R) = tree(tree(nil,1,nil), 2, nil).
L = tree(nil, 1, nil), K = 2, R = nil.
```

Lists of structures
-------------------

Lists commonly hold structures; combine both patterns naturally:

```prolog
edges([edge(a,b), edge(b,c), edge(c,d)]).

has_path([edge(X,Y)|_], X, Y).
has_path([_|Es], X, Y) :- has_path(Es, X, Y).
```

```text
?- edges(Es), has_path(Es, a, b).
Es = [edge(a, b), edge(b, c), edge(c, d)].
true.
```

Tips
----

- `[H,T]` means a 2‑element list; `[H|T]` means head/tail. They are different.
- Use `_` for don’t‑care parts of list/structure patterns.
- Keep recursive list processing tail‑recursive when possible for performance (e.g., accumulators).
