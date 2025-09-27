# Difference lists

Build lists in O(1) per append using a pair `List-Tail`.

```prolog
% Append with difference lists
append_dl(Xs-Ys, Ys-Zs, Xs-Zs).

% Convert a standard list to a difference list
to_dl([], Xs-Xs).
to_dl([X|Xs], [X|Ys]-Zs) :- to_dl(Xs, Ys-Zs).

% Convert back to a proper list
from_dl(Xs-[], Xs).
```

```text
?- to_dl([a,b], DL), append_dl(DL, [c,d]-[], R), from_dl(R, L).
DL = [a,b|_G1]-_G1,
R = [a,b,c,d]-[],
L = [a,b,c,d].
```

Difference lists are useful in grammar processing and when constructing large lists via many concatenations.
