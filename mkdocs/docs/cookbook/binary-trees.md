# Binary search trees

Simple BST with membership, insertion, and inorder traversal.

```prolog
% Tree is either empty or t(Key, Left, Right)

bst_member(X, t(X, _, _)).
bst_member(X, t(Y, L, _)) :- X < Y, bst_member(X, L).
bst_member(X, t(Y, _, R)) :- X > Y, bst_member(X, R).

bst_insert(X, nil, t(X, nil, nil)).
bst_insert(X, t(X, L, R), t(X, L, R)).  % no duplicates
bst_insert(X, t(Y, L, R), t(Y, L1, R)) :- X < Y, bst_insert(X, L, L1).
bst_insert(X, t(Y, L, R), t(Y, L, R1)) :- X > Y, bst_insert(X, R, R1).

inorder(nil, []).
inorder(t(K, L, R), S) :- inorder(L, SL), inorder(R, SR), append(SL, [K|SR], S).

% append/3
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

```text
?- bst_insert(3, nil, T1), bst_insert(1, T1, T2), bst_insert(4, T2, T3), inorder(T3, S).
S = [1,3,4].

?- bst_member(4, t(3, t(1,nil,nil), t(4,nil,nil))).
true.
```

Note: Uses arithmetic comparisons on keys.
