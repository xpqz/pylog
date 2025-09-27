# Map coloring (CLP(FD))

Color adjacent regions differently using small integer domains.

```prolog
% Australia map adjacency
adj(sa, wa). adj(sa, nt). adj(sa, q). adj(sa, nsw). adj(sa, v).
adj(wa, nt). adj(nt, q). adj(q, nsw). adj(nsw, v).

coloring(Colors) :-
    Colors = [wa, nt, sa, q, nsw, v, t],
    WA in 1..3, NT in 1..3, SA in 1..3, Q in 1..3, NSW in 1..3, V in 1..3, T in 1..3,
    all_different_neighbors([wa-WA, nt-NT, sa-SA, q-Q, nsw-NSW, v-V, t-T]),
    label([WA,NT,SA,Q,NSW,V,T]).

all_different_neighbors(Pairs) :-
    \+ ( member(A-CA, Pairs), member(B-CB, Pairs), A @< B, adj(A,B), CA #= CB ).

% member/2
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
```

```text
?- coloring(Cs).
Cs = [wa, nt, sa, q, nsw, v, t].  % with assigned numbers for each variable
```

Note: Region variables (WA, NT, …) take values 1..3 (3‑coloring). Increase to 1..4 to allow more flexibility.
