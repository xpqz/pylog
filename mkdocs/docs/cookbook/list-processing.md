# List processing

Classic list predicates and common patterns.

Member and append
-----------------

```prolog
% Is X a member of a list?
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Append two lists to make a third
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- member(2, [1,2,3]).
true ; false.

?- append([1,2], [3,4], Z).
Z = [1,2,3,4].
```

Reverse
-------

Naive reverse builds with append/3 and is O(n^2):

```prolog
rev([], []).
rev([X|Xs], Ys) :- rev(Xs, Rs), append(Rs, [X], Ys).
```

Tail‑recursive reverse with an accumulator is O(n):

```prolog
rev_acc(Xs, Ys) :- rev_acc_(Xs, [], Ys).

rev_acc_([], Acc, Acc).
rev_acc_([X|Xs], Acc, Ys) :- rev_acc_(Xs, [X|Acc], Ys).
```

```text
?- rev([1,2,3], R).
R = [3,2,1].

?- rev_acc([1,2,3], R).
R = [3,2,1].
```

Length and sum
--------------

```prolog
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.

sum([], 0).
sum([X|Xs], S) :- sum(Xs, T), S is T + X.
```

```text
?- len([a,b,c], N).
N = 3.

?- sum([1,2,3,4], S).
S = 10.
```

