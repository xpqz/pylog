% test predicates for section 8.10 tests

a(1,f(_)).
a(2,f(_)).

b(1, 1).
b(1, 1).
b(1, 2).
b(2, 1).
b(2, 2).
b(2, 2).

d(1,1).
d(1,2).
d(1,1).
d(2,2).
d(2,1).
d(2,2).

member(X, [X|_]).
member(X, [_|L]) :-
	member(X, L).

