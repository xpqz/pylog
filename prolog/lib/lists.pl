% Standard list predicates implemented in pure Prolog
% These serve as both library functions and acceptance tests for the PyLog engine

% append/3 - Concatenate two lists or generate list decompositions
% append(?List1, ?List2, ?List3)
% True when List3 is the concatenation of List1 and List2
append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).

% member/2 - Check membership or generate list elements
% member(?Element, ?List)
% True when Element is a member of List
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% reverse/2 - Reverse a list
% reverse(?List1, ?List2)
% True when List2 is the reverse of List1
reverse(L, R) :- reverse_acc(L, [], R).

% Helper predicate for reverse with accumulator
reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).

% length/2 - Relate a list to its length using successor notation
% length(?List, ?Length)
% True when List has Length elements (using successor notation for Stage 1)
length([], 0).
length([_|T], s(N)) :- length(T, N).

% last/2 - Get the last element of a list
% last(?List, ?Element)
% True when Element is the last element of List
last([X], X).
last([_|T], X) :- last(T, X).

% nth_s/3 - Access list element by successor-based index
% nth_s(?Index, ?List, ?Element)
% True when Element is the Index-th element of List (successor-based)
nth_s(0, [H|_], H).
nth_s(s(N), [_|T], E) :- nth_s(N, T, E).

% select/3 - Select an element from a list, leaving the rest
% select(?Element, ?List, ?Rest)
% True when List contains Element and Rest is List without Element
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

% Additional utility predicates

% take/3 - Take first N elements from a list (successor-based)
% take(?N, ?List, ?Prefix)
take(0, _, []).
take(s(N), [H|T], [H|R]) :- take(N, T, R).

% drop/3 - Drop first N elements from a list (successor-based)
% drop(?N, ?List, ?Suffix)
drop(0, L, L).
drop(s(N), [_|T], R) :- drop(N, T, R).

% prefix/2 - Check if one list is a prefix of another
% prefix(?Prefix, ?List)
prefix([], _).
prefix([H|T1], [H|T2]) :- prefix(T1, T2).

% suffix/2 - Check if one list is a suffix of another
% suffix(?Suffix, ?List)
suffix(S, L) :- append(_, S, L).

% sublist/2 - Check if one list is a contiguous sublist of another
% sublist(?SubList, ?List)
sublist(S, L) :- prefix(S, L).
sublist(S, [_|T]) :- sublist(S, T).