:- use_module(library(clpfd)).

test :-
    X in 5..15,
    Y in 5..15,
    X + Y #=< 10,
    write('X domain: '), fd_dom(X, XDom), write(XDom), nl,
    write('Y domain: '), fd_dom(Y, YDom), write(YDom), nl.
