:- use_module(library(clpfd)).

test_subtraction :-
    X in 1..10,
    Y in 1..10,
    X - Y #= 5,
    write('X domain: '), fd_dom(X, XDom), write(XDom), nl,
    write('Y domain: '), fd_dom(Y, YDom), write(YDom), nl.

test_weighted :-
    X in 1..10,
    Y in 1..10,
    2*X + 3*Y #= 20,
    write('X domain: '), fd_dom(X, XDom), write(XDom), nl,
    write('Y domain: '), fd_dom(Y, YDom), write(YDom), nl.
