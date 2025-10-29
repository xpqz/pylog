% ISO-Prolog test patterns
% Author: Joachim Schimpf, 2013
% This code is given to the public domain "as is". Use at your own risk.
%
% These tests focus on error handling and corner cases of functionality.
% Basic functionality is expected to be covered elsewhere.
%
% See harness.pl for how to write test patterns.
%
% See fixme ... lines for remaining issues that need addressing.
% Deviations from the literal standard are marked with %%%

% Assumes the following files in the same directory:
%	nosuch		this file must not exist
%	nowrite		a file with no write permissions
%	empty		an empty r/w file
%	hello		a readable file containing "hello.\nworld.\n"
%	scowen		it must be possible to create and write this file

% Assumes that the following auxiliary predicates have been defined:
%	iso_test_ensure_loaded(File)
%	iso_test_variant(X,Y)
%	iso_test_os(OS)
%	iso_test_non_repositionable_stream(S)
%	iso_test_same_members(Xs,Ys)


%----------- 7.8.3 call/1 ----------------

call(!)			should_give true.
call(fail)		should_fail.
call((fail,_))		should_fail.
call((fail,call(1)))	should_fail.

Z=!, call((Z=!,(X=1;X=2),Z)) should_give
			multiple_solutions(K, K==1, ( K==1, Z==!, X==1)).

call((Z=!,(X=1;X=2),Z))	should_give
			multiple_solutions(K, K==2, (Z==!, K==X)).

call((write(3),_))	should_throw error(instantiation_error,_).
call((write(3),call(1))) should_throw error(type_error(callable,1),_).
call(_)			should_throw error(instantiation_error,_).
call(1)			should_throw error(type_error(callable,1),_).
call((fail,1))		should_throw error(type_error(callable,(fail,1)),_).
call((write(3),1))	should_throw error(type_error(callable,(write(3),1)),_).
call((1;true))		should_throw error(type_error(callable,(1;true)),_).


%----------- 7.8.4 !/0 ----------------

!			should_give true.
(!, fail; true)		should_fail.
(call(!), fail; true)	should_give true.
(X=1;X=2), !		should_give multiple_solutions(K,K==1,X==K).
(!,X=1;X=2)		should_give multiple_solutions(K,K==1,X==K).
(X=1;X=2), (true;!)	should_give multiple_solutions(K,K==2,X==1).
(X=1;X=2), (!,fail;true) should_fail.
(X=!;X=true), call(X)	should_give multiple_solutions(K,K==2,
				(K==1-> X==! ; K==2->X==true)).
(G=((X=1;X=2),!);G=(X=3)), call(G) should_give multiple_solutions(K,K==2,
				(K==1-> X==1 ; K==2->X==3)).
(X=1;X=2), \+(\+(!))	should_give multiple_solutions(K,K==2,K==X).
(X=1;X=2), once(!)	should_give multiple_solutions(K,K==2,K==X).
(X=1;X=2), call(!)	should_give multiple_solutions(K,K==2,K==X).


%----------- 7.8.10 throw/1 ----------------

throw(_)		should_throw error(instantiation_error,_).
throw(a)		should_throw a.
throw(1)		should_throw 1.
throw(1.0)		should_throw 1.0.
throw(f(a))		should_throw f(a).
catch(throw(f(X)),T,true)	should_give T=f(Y), var(Y).


%----------- 8.2 term unification ----------------

% Official examples
unify_with_occurs_check(1, 1)			should_give true.
unify_with_occurs_check(X, 1)			should_give X == 1.
unify_with_occurs_check(X, Y)			should_give X == Y.
unify_with_occurs_check(_, _)			should_give true.
unify_with_occurs_check(X, Y),
unify_with_occurs_check(X, abc)			should_give X == abc, Y == abc.
unify_with_occurs_check(f(X, def), f(def, Y))	should_give X == def, Y == def.
unify_with_occurs_check(1, 2)					should_fail.
unify_with_occurs_check(1, 1.0)					should_fail.
unify_with_occurs_check( g(X), f(f(X)) )			should_fail.
unify_with_occurs_check( f(X, 1), f(a(X) ) )			should_fail.
unify_with_occurs_check( f(X, Y, X ), f(a(X), a(Y), Y, 2) )	should_fail.
unify_with_occurs_check( X, a(X) )				should_fail.
unify_with_occurs_check( f(X, 1), f(a(X), 2) )			should_fail.
unify_with_occurs_check( f(1, X, 1), f(2, a(X), 2) )		should_fail.
unify_with_occurs_check( f(1, X), f(2, a(X)) )			should_fail.
unify_with_occurs_check( f(X, Y, X, 1), f(a(X), a(Y), Y, 2) )	should_fail.


subsumes_term(a,a)				should_give true.
subsumes_term(f(X,Y), f(Z,Z))			should_give true.
subsumes_term(f(Z,Z), f(X,Y))			should_fail.
subsumes_term(g(X), g(f(X)))			should_fail.
subsumes_term(X, f(X))				should_fail.
subsumes_term(X,Y), subsumes_term(Y,f(X))	should_give true.


%----------- 8.3 type testing ----------------
% no errors

var(_)		should_give true.
var(3)		should_fail.

atom(a)		should_give true.
atom(3)		should_fail.
atom(_)		should_fail.

integer(3)	should_give true.
integer(a)	should_fail.
integer(_)	should_fail.

float(3.1)	should_give true.
float(a)	should_fail.
float(_)	should_fail.

atomic(a)	should_give true.
atomic(3)	should_give true.
atomic(3.1)	should_give true.
atomic(f(b))	should_fail.
atomic([a])	should_fail.
atomic(_)	should_fail.

compound(f(b))	should_give true.
compound([a])	should_give true.
compound(a)	should_fail.
compound(3)	should_fail.
compound(3.1)	should_fail.
compound(_)	should_fail.

nonvar(3)	should_give true.
nonvar(_)	should_fail.

number(3)	should_give true.
number(3.1)	should_give true.
number(a)	should_fail.
number(_)	should_fail.

callable(f(b))	should_give true.
callable([a])	should_give true.
callable(a)	should_give true.
callable(3)	should_fail.
callable(3.1)	should_fail.
callable(_)	should_fail.

ground(a)	should_give true.
ground(f(3))	should_give true.
ground(_)	should_fail.
ground(f(_))	should_fail.

acyclic_term(foo(_))		should_give true.
X=f(X), acyclic_term(X)		should_fail.
X=f(X,a), acyclic_term(X)	should_fail.


%----------- 8.4 term comparison ----------------

compare(3,4,5)		should_throw error(type_error(atom,3),_).
compare($,4,5)		should_throw error(domain_error(order,$),_).


sort(_,_)		should_throw error(instantiation_error,_).
sort([a|_],_)		should_throw error(instantiation_error,_).
sort(3,_)		should_throw error(type_error(list,3),_).
sort([a|b],_)		should_throw error(type_error(list,[a|b]),_).
sort([],3)		should_throw error(type_error(list,3),_).
sort([],[a|b])		should_throw error(type_error(list,[a|b]),_).


keysort(_,_)		should_throw error(instantiation_error,_).
keysort([1-a|_],_)	should_throw error(instantiation_error,_).
keysort(3,_)		should_throw error(type_error(list,3),_).
keysort([1-a|b],_)	should_throw error(type_error(list,[1-a|b]),_).
keysort([],3)		should_throw error(type_error(list,3),_).
keysort([],[1-a|b])	should_throw error(type_error(list,[1-a|b]),_).
keysort([_],_)		should_throw error(instantiation_error,_).
keysort([1/a],_)	should_throw error(type_error(pair,1/a),_).
keysort([],[1/a])	should_throw error(type_error(pair,1/a),_).


%----------- 8.5.1 functor/3 ----------------

% Official examples
functor(foo(a, b, c), foo, 3)	should_give true.
functor(foo(a, b, c), X, Y)	should_give X==foo, Y==3.
functor(X, foo, 3)		should_give X=foo(_,_,_).
functor(X, foo, 0)		should_give X==foo.
functor(mats(A, B), A, B)	should_give A==mats, B==2.
functor(foo(a), foo, 2)		should_fail.
functor(foo(a), fo, 1)		should_fail.
functor(1, X, Y)		should_give X==1, Y==0.
functor(X, 1.1, 0)		should_give X==1.1.
functor([_|_], '.', 2)		should_give true.
functor([], [], 0)		should_give true.
functor(X, Y, 3)		should_throw error(instantiation_error, _).
functor(X, foo, N)		should_throw error(instantiation_error, _).
functor(X, foo, a)		should_throw error(type_error(integer, a), _).
functor(F, 1.5, 1)		should_throw error(type_error(atom, 1.5), _).
functor(F, foo(a), 1)		should_throw error(type_error(atomic, foo(a)), _).
%current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X)
%		should_throw error(representation_error(max_arity), _).
functor(F, foo, -1)		should_throw error(domain_error(not_less_than_zero, -1), _).

% 2^63
A is 9223372036854775808, functor(_,f,A)
		should_throw error(representation_error(max_arity), _).

%----------- 8.5.2 arg/3 ----------------

% Official examples
arg(1, foo(a, b), a)	should_give true.
arg(1, foo(a, b), X)	should_give X==a.
arg(1, foo(X, b), a)	should_give X==a.
arg(1, foo(a, b), b)	should_fail.
arg(1, foo(a, b), b)	should_fail.
arg(0, foo(a, b), foo)	should_fail.
arg(3, foo(3, 4), N)	should_fail.
arg(X, foo(a, b), a)	should_throw error(instantiation_error, _).
arg(1, X, a)		should_throw error(instantiation_error, _).
arg(0, atom, A)		should_throw error(type_error(compound, atom), _).
arg(0, 3, A)		should_throw error(type_error(compound, 3), _).
%arg(1, foo(X), u(X)).  Undefined.

arg(a, foo(a, b), X)	should_throw error(type_error(integer, a), _).
arg(-1, foo(a, b), X)	should_throw error(domain_error(not_less_than_zero, -1), _).


%----------- 8.5.3 =../2 ----------------

% Official examples
'=..'(foo(a, b), [foo, a, b])	should_give true.
'=..'(X, [foo, a, b])		should_give X==foo(a,b).
'=..'(foo(a, b), L)		should_give L==[foo,a,b].
'=..'(foo(X, b), [foo, a, Y])	should_give X==a, Y==b.
'=..'(1, [1])			should_give true.
'=..'(foo(a, b), [foo, b, a])	should_fail.
'=..'(X, Y)			should_throw error(instantiation_error, _).
'=..'(X, [foo, a | Y])		should_throw error(instantiation_error, _).
'=..'(X, [foo|bar])		should_throw error(type_error(list, [foo|bar]), _).
'=..'(X, [Foo, bar])		should_throw error(instantiation_error, _).
'=..'(X, [3, 1])		should_throw error(type_error(atom, 3), _).
'=..'(X, [1.1, foo])		should_throw error(type_error(atom, 1.1), _).
'=..'(X, [a(b), 1])		should_throw error(type_error(atom, a(b)), _).
'=..'(X, 4)			should_throw error(type_error(list, 4), _).
%'=..'(f(X), [f, u(X)]).  Undefined.


%----------- 8.5.4 copy_term/2 ----------------

% Official examples
copy_term(X, Y)			should_give true.
copy_term(X, 3)			should_give true.
copy_term(_, a)			should_give true.
copy_term(a+X, X+b)		should_give X==a.
copy_term(_, _)			should_give true.
copy_term(X+X+Y, A+B+B)		should_give A==B.
copy_term(a, b)			should_fail.
copy_term(a+X, X+b), copy_term(a+X, X+b)	should_fail.
%copy_term(demoen(X, X), demoen(Y, f(Y))).  Undefined.


%----------- 8.5.5 term_variables/2 ----------------

term_variables(foo, 3)		should_throw error(type_error(list, 3), _).
term_variables(foo, [a|b])	should_throw error(type_error(list, [a|b]), _).
term_variables(foo(X,Y,X,Z),Vs)	should_give Vs == [X,Y,Z].


%----------- 8.6.1 is/2 ----------------

% Official examples
'is'(Result, 3+11.0)	should_give Result==14.0.
X = 1+2, Y is X * 3	should_give X==1+2, Y==9.
'is'(3, 3)		should_give true.
'is'(3, 3.0)		should_fail.
'is'(foo, 77)		should_fail.
'is'(77, N)		should_throw error(instantiation_error, _).


%----------- 8.7 arithmetic comparisons ----------------

% Official examples
'=:='(0, 1)		should_fail.
'=\\='(0, 1)		should_give true.
'<'(0, 1)		should_give true.
'>'(0, 1)		should_fail.
'>='(0, 1)		should_fail.
'=<'(0, 1)		should_give true.
'=:='(1.0, 1)		should_give true.
=\=(1.0, 1)		should_fail.
'<'(1.0, 1)		should_fail.
'>'(1.0, 1)		should_fail.
'>='(1.0, 1)		should_give true.
'=<'(1.0, 1)		should_give true.
'=:='(3*2, 7-1)		should_give true.
'=\\='(3*2, 7-1)	should_fail.
'<'(3*2, 7-1)		should_fail.
'>'(3*2, 7-1)		should_fail.
'>='(3*2, 7-1)		should_give true.
'=<'(3*2, 7-1)		should_give true.
'=:='(X, 5)		should_throw error(instantiation_error, _).
=\=(X, 5)		should_throw error(instantiation_error, _).
'<'(X, 5)		should_throw error(instantiation_error, _).
'>'(X, 5)		should_throw error(instantiation_error, _).
'>='(X, 5)		should_throw error(instantiation_error, _).
'=<'(X, 5)		should_throw error(instantiation_error, _).



%----------- 8.8 clause retrieval and information ----------------

% Auxiliary predicates for this section
iso_test_ensure_loaded(iso_8_8)	should_give true.

% Official examples
clause(cat, true)		should_give true.
clause(dog, true)		should_give true.
clause(legs(I, 6), Body)	should_give Body==insect(I).
clause(legs(C, 7), Body)	should_give Body==(call(C),call(C)).
clause(insect(I), T)		should_give I==ant,T==true.
clause(insect(I), T),I\==ant	should_give I==bee,T==true.
clause(x, Body)			should_fail.
clause(_, B)			should_throw error(instantiation_error, _).
clause(4, X)			should_throw error(type_error(callable,4), _).
clause(elk(N), Body)		should_throw error(permission_error(access, private_procedure, elk/1), _).
clause(atom(_), Body)		should_throw error(permission_error(access, private_procedure, atom/1), _).
%clause(legs(A, 6), insect(f(A))).  Undefined.

% Official examples
current_predicate(dog/0)		should_give true.
current_predicate(current_predicate/1)	should_fail.
current_predicate(elk/Arity)		should_give Arity==1.
current_predicate(foo/A)		should_fail.
current_predicate(Name/1),Name==elk	should_give true.	% modified
current_predicate(Name/1),Name==insect	should_give true.	% modified
current_predicate(4)			should_throw error(type_error(predicate_indicator,4), _).

current_predicate(3/3)			should_throw error(type_error(predicate_indicator,3/3), _).
current_predicate(f/f)			should_throw error(type_error(predicate_indicator,f/f), _).
current_predicate(f/ -1)		should_throw error(type_error(predicate_indicator,f/ -1), _).
current_predicate(call/8)		should_fail.
current_predicate(call/9)		should_fail.


%----------- 8.9 clause creation and destruction ----------------

% Official examples
asserta(legs(octopus, 8))		should_give true.
asserta( (legs(A, 4) :- animal(A)) )	should_give true.
asserta( (foo(X) :- X, call(X)) )	should_give true.
asserta(_)		should_throw error(instantiation_error, asserta/1).
asserta(4)		should_throw error(type_error(callable, 4), asserta/1).
asserta( (foo :- 4) )	should_throw error(type_error(callable, 4), asserta/1).
asserta( (atom(_) :- true) )	should_throw
	error(permission_error(modify, static_procedure, atom/1), asserta/1).

assertz(legs(spider, 8))		should_give true.
assertz( (legs(B, 2) :- bird(B)) )		should_give true.
assertz( (foo(X) :- X -> call(X)) )		should_give true.
assertz(_)		should_throw error(instantiation_error, assertz/1).
assertz(4)		should_throw error(type_error(callable, 4), assertz/1).
assertz( (foo :- 4) )	should_throw error(type_error(callable, 4), assertz/1).
assertz( (atom(_) :- true) )	should_throw
	error(permission_error(modify, static_procedure, atom/1), assertz/1).


retract((legs(_, 7):-_))	should_give true.
retract(legs(octopus, 8))	should_give true.
retract(legs(spider, 6))	should_fail.
retract(mammal(_))		should_fail.
retract( (legs(X, 2) :- T) )	should_give T==bird(X).
findall([X,Y,Z], retract( (legs(X, Y) :- Z) ), Sols)	should_give
	Sols = [[X,4,animal(X)], [X,6,insect(X)], [spider,8,true]].
retract( (legs(X, Y) :- Z) )	should_fail.
retract(insect(I)), write(I), retract(insect(bee)), fail should_fail.

%retract(( foo(A) :- A, call(A) )).
%	Undefined
%	[ An attempt to unify two terms:
%	:-(foo(A), (A, call(A))) and
%	:-(foo(X), (call(X), call(X)))
%	when examining the clause
%'foo(X) :- call(X), call(X)'].

retract(( foo(C) :- A -> B ))		should_give A==call(C),B==call(C).
retract( (X :- in_eec(Y)) )		should_throw error(instantiation_error, retract/1).
retract( (4 :- X) )			should_throw error(type_error(callable, 4), retract/1).
retract( (atom(X) :- X == '[]') )	should_throw error(permission_error(modify, static_procedure, atom/1), retract/1).


abolish(_)		should_throw error(instantiation_error, _).
abolish(foo/_)		should_throw error(instantiation_error, _).
abolish(_/2)		should_throw error(instantiation_error, _).
abolish(foo/2)		should_give true.
abolish(foo)		should_throw error(type_error(predicate_indicator, foo), _).
abolish(foo(_))		should_throw error(type_error(predicate_indicator, foo(_)), _).
abolish(1/2)		should_throw error(type_error(atom,1), _).
abolish(foo/bar)	should_throw error(type_error(integer,bar), _).
abolish(foo/ -1)	should_throw error(domain_error(not_less_than_zero,-1), _).
%abolish(foo/9999)	should_throw error(representation_error(max_arity), _).
abolish(abolish/1)	should_throw error(permission_error(modify, static_procedure, abolish/1), _).
abolish(elk/1)		should_throw error(permission_error(modify, static_procedure, elk/1), _).



assertz(insect(fly(house))),
assertz(insect(beetle(stag))),
assertz(insect(fly(fruit)))	should_give true.
retractall(insect(fly(_)))	should_give true.
insect(fly(_))			should_fail.
insect(I)			should_give I==beetle(stag).
retractall(insect(spider))	should_give true.
retractall(mammal(_))		should_give true.
retractall(_)			should_throw error(instantiation_error,_).
retractall(3)			should_throw error(type_error(callable,3),_).
retractall(elk(_))		should_throw error(permission_error(modify,static_procedure,elk/1),_).
retractall(atom(_))		should_throw error(permission_error(modify,static_procedure,atom/1),_).


%----------- 8.10 all solutions ----------------

% Auxiliary predicates for this section
iso_test_ensure_loaded(iso_8_10)	should_give true.


findall(X, (X=1; X=2), S)	should_give S == [1,2].
findall(X+Y, (X=1), S)		should_give iso_test_variant(S, [1+_]).
findall(X, fail, L)		should_give L == [].
findall(X, (X=1; X=1), S)	should_give S == [1, 1].
findall(X, (X=2; X=1), [1, 2])	should_fail.
findall(X, (X=1;X=2), [X, Y])	should_give X == 1, Y == 2.
findall(X, Goal, S)	should_throw error(instantiation_error,_).
findall(X, 4, S)	should_throw error(type_error(callable,4),_).

findall(X, (X=2; X=1), 12)	should_throw
	error(type_error(list,12),findall/3).
findall(X, (X=2; X=1), [1|2])	should_throw
	error(type_error(list,[1|2]),findall/3).



bagof(X, (X=1 ; X=2), S)	should_give S == [1,2].
bagof(X, (X=1 ; X=2), X)	should_give X == [1,2].
bagof(X, (X=Y ; X=Z), S)	should_give S == [Y,Z].
bagof(X, fail, S)		should_fail.
bagof(1, (Y=1 ; Y=2), L)	should_give multiple_solutions(K,K==2,
	% Free variables set: {Y}.
	( K==1 -> L==[1], Y==1
	; K==2 -> L==[1], Y==2 )).
bagof(f(X, Y), (X=a ; Y=b), L)	should_give
	% Free variables set: {}.
	iso_test_variant(L, [f(a, _), f(_, b)]).
bagof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S)	should_give S == [1,2].
bagof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S)	should_give
	% Free variables set: {}.
	iso_test_variant(S, [1, _, 2]).
set_prolog_flag(unknown, fail),
bagof(X, (Y^(X=1 ; Y=2) ; X=3), S)	should_give
	% Free variables set: {Y}.
	% ^/2 should trigger undefined procedure warning and fail
	S == [3], var(Y).
set_prolog_flag(unknown, error) should_give true.
bagof(X, (X=Y ; X=Z ; Y=1), S)		should_give multiple_solutions(K,K==2,
	% Free variables set: {Y,Z}.
	( K==1 -> S == [Y, Z], var(Y)
	; K==2 -> iso_test_variant(S, [_]), Y==1)).
bagof(X, a(X, Y), L) should_give
	% Free variables set: {Y}.
	L == [1, 2], iso_test_variant(Y, f(_)).

bagof(X, b(X, Y), L) should_give multiple_solutions(K,K==2,
	% Free variables set: {Y}.
	( K==1 -> L == [1,1,2], Y == 1
	; K==2 -> L == [1,2,2], Y == 2)).
bagof(X, Y^Z, L) should_throw error(instantiation_error, _).
bagof(X, 1, L) should_throw error(type_error(callable,1), _).

bagof(X, (X=2; X=1), 12)	should_throw
	error(type_error(list,12),bagof/3).
bagof(X, (X=2; X=1), [1|2])	should_throw
	error(type_error(list,[1|2]),bagof/3).



setof(X, (X=1; X=2), S)	should_give S == [1,2].
setof(X, (X=1; X=2), X)	should_give X == [1,2].
setof(X, (X=2; X=1), S)	should_give S == [1,2].
setof(X, (X=2; X=2), S)	should_give S == [2].
setof(X, (X=Y; X=Z), S)	should_give
	% Free variables set: {Y,Z}.
	% Succeeds, unifying S with [Y, Z] or [Z, Y].
	iso_test_same_members(S, [Y,Z]).
setof(X, fail, S)	should_fail.
setof(1, (Y=2 ; Y=1), L)	should_give multiple_solutions(K,K==2,
	% Free variables set: {Y}.
	%[The order of solutions is undefined]
	(K==1-> L == [1], Y == 1
	;K==2-> L == [1], Y == 2)).
setof(f(X,Y), (X=a ; Y=b), L)	should_give
	% Free variables set: {}.
	iso_test_variant(L, [f(_,b),f(a,_)]).
setof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S)	should_give
	% Free variables set: {}.
	S == [1,2].
setof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S)	should_give
	% Free variables set: {}.
	iso_test_variant(S, [_,1,2]).
set_prolog_flag(unknown, fail),
setof(X, (Y^(X=1 ; Y=2) ; X=3), S)	should_give
	%Free variables set: {Y}.
	%Warning: the procedure (^)/2 is undefined.
	S == [3], var(Y).
set_prolog_flag(unknown, error) should_give true.
setof(X, (X=Y ; X=Z ; Y=1), S)	should_give multiple_solutions(K,K==2,
	%Free variables set: {Y,Z}.
	(K==1-> iso_test_same_members(S, [Y,Z])
	;K==2-> iso_test_variant(S, [_]), Y == 1)).
setof(X, a(X, Y), L)	should_give
	%Free variables set: {Y}.
	L == [1, 2], iso_test_variant(Y, f(_)).
setof(X, member(X,[f(U,b),f(V,c)]), L)	should_give
	%Free variables set: {U,V}.
	%Implementation dependent.
	( iso_test_variant(L, [f(U,b),f(V,c)])
	; iso_test_variant(L, [f(V,c),f(U,b)])).
setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)])
    should_fail.
%    should_give U == a, V == a.
	%Free variables set: {U,V}.
	%Implementation dependent.
	%[If the previous example succeeds,
	%unifying L with [f(U,b),f(V,c)],
	%then this example fails.
	%If the previous example succeeds,
	%unifying L with [f(V,c),f(U,b)],
	%then this example succeeds,
	%unifying U with a, and V with a).]
setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)])	should_give
	%Free variables set: {U,V}.
	U == a, V == a.
setof(X, member(X,[V,U,f(U),f(V)]), L)	should_give
	%Free variables set: {U,V}.
	( iso_test_variant(L, [U,V,f(U),f(V)])
	; iso_test_variant(L, [V,U,f(V),f(U)])).
setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)])	should_give
	%Free variables set: {U,V}.
	%Implementation dependent.
	%Succeeds, unifying U with a, and V with b;
	%or, unifying U with b, and V with a.
	( U == a, V == b
	; U == b, V == a).
setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)])	should_fail.
	%Free variables set: {U,V}.
setof(X,
	(exists(U,V)^member(X,[V,U,f(U),f(V)])),
	[a,b,f(b),f(a)])	should_give true.
	% Free variables set: {}.
setof(X, b(X, Y), L)	should_give multiple_solutions(K,K==2,
	%Free variables set: {Y}.
	%[The order of solutions is undefined]
	(K==1-> L == [1, 2], Y == 1
	;K==2-> L == [1, 2], Y == 2)).
setof(X-Xs,Y^setof(Y,b(X,Y),Xs),L)	should_give
	%Free variables set: {}.
	L == [1-[1,2],2-[1,2]].
setof(X-Xs,setof(Y,b(X,Y),Xs),L)	should_give
	%Free variables set: {Y}.
	L == [1-[1,2],2-[1,2]], var(Y).
setof(X-Xs,bagof(Y,d(X,Y),Xs),L)	should_give
	%Free variables set: {Y}.
	L == [1-[1,2,1],2-[2,1,2]], var(Y).

setof(X, (X=2; X=1), 12)	should_throw
	error(type_error(list,12),setof/3).
setof(X, (X=2; X=1), [1|2])	should_throw
	error(type_error(list,[1|2]),setof/3).


%----------- 8.11 stream selection and control ----------------


current_input(a) should_throw error(domain_error(stream,a),_).
current_input(3.5) should_throw error(domain_error(stream,3.5),_).
current_input(S) should_give ground(S), \+atom(S).

current_output(a) should_throw error(domain_error(stream,a),_).
current_output(3.5) should_throw error(domain_error(stream,3.5),_).
current_output(S) should_give ground(S), \+atom(S).

set_input(_)	should_throw error(instantiation_error,_).
set_input(3.5)	should_throw error(domain_error(stream_or_alias,3.5),_).
set_input(xyz)	should_throw error(existence_error(stream,xyz),_).
current_input(S), set_input(S) should_give true.
current_output(S), set_input(S) should_throw error(permission_error(input,stream,S),_).

set_output(_)	should_throw error(instantiation_error,_).
set_output(3.5)	should_throw error(domain_error(stream_or_alias,3.5),_).
set_output(xyz) should_throw error(existence_error(stream,xyz),_).
current_output(S), set_output(S) should_give true.
current_input(S), set_output(S) should_throw error(permission_error(output,stream,S),_).


open('hello', read, D, [type(binary)]) should_give
	stream_property(D,mode(read)).
% CAUTION: this stream remains open under its alias!
open('scowen', write, D, [alias(editor)]) should_give
	stream_property(D,mode(write)),
	stream_property(D,alias(editor)).
open('hello', read, DD, []) should_give
	stream_property(D,mode(read)).

open(_, read, S, [])			should_throw error(instantiation_error,_).
open(scowen, _, S, [])			should_throw error(instantiation_error,_).
open(scowen, read, S, _)		should_throw error(instantiation_error,_).
open(scowen, read, S, [_])		should_throw error(instantiation_error,_).
open(scowen, read, S, [type(binary)|_])	should_throw error(instantiation_error,_).
open(scowen, 3, S, [])			should_throw error(type_error(atom,3),_).
open(scowen, read, S, 3)		should_throw error(type_error(list,3),_).
open(scowen, read, S, [type(binary)|b])	should_throw error(type_error(list,[type(binary)|b]),_).
open(scowen, read, a, [])		should_throw error(uninstantiation_error(a),_).
open(scowen, mangle, S, [])		should_throw error(domain_error(io_mode,mangle),_).
open(scowen, update, S, [])		should_throw error(domain_error(io_mode,update),_).
open(scowen, read, S, [3])		should_throw error(domain_error(stream_option,3),_).
open(scowen, read, S, [force(true)])	should_throw error(domain_error(stream_option,force(true)),_).
% This is broken on sparc_sunos with Java embedding - temporarily disabled:
fixme open(nosuch, read, S, [])		should_throw error(existence_error(source_sink,nosuch),_).
% Disabled because we can't portably create a readonly file:
fixme open(nowrite, write, S, [])		should_throw error(permission_error(open,source_sink,nowrite),_).
open(scowen, read, S, [alias(editor)])	should_throw error(permission_error(open,source_sink,alias(editor)),_).
open(scowen, read, S, [alias(foo)])	should_give true.

% No portable way of opening a tty
fixme
    ( iso_test_os(win) -> TTY=con ; TTY='/dev/tty' ),
    open(Source, read, _, [reposition(true)])
	should_throw error(permission_error(open,source_sink,reposition(true)),_).

close(_)			should_throw error(instantiation_error,_).
close(editor,_)			should_throw error(instantiation_error,_).
close(editor,[_])		should_throw error(instantiation_error,_).
close(editor,[force(true)|_])	should_throw error(instantiation_error,_).
close(editor,3)			should_throw error(type_error(list,3),_).
close(editor,[force(true)|b])	should_throw error(type_error(list,[force(true)|b]),_).
close(editor,[farce(true)])	should_throw error(domain_error(close_option,farce(true)),_).
close(3.5,[])			should_throw error(domain_error(stream_or_alias,3.5),_).
close(editor)			should_give true.	% closes previously opened stream
close(foo,[])			should_give true.	% closes previously opened stream
close(foo,[])			should_throw error(existence_error(stream,foo),_).
close(foo,[force(true)])	should_give true.


flush_output(_)			should_throw error(instantiation_error,_).
flush_output(3.5)		should_throw error(domain_error(stream_or_alias,3.5),_).
flush_output(foo)		should_throw error(existence_error(stream,foo),_).
flush_output(user_input)	should_throw error(permission_error(output,stream,user_input),_).
flush_output(user_output)	should_give true.


% Not clear whether to make type/domain checks on the property arguments
stream_property(_,_)					should_give true.
stream_property(3.5, _)					should_throw error(domain_error(stream,3.5),_).
open(hello, read, S), stream_property(S, file_name(F))	should_give F==hello.
current_input(S), stream_property(S, mode(M))		should_give M==read.
current_input(S), stream_property(S, input)		should_give true.
current_input(S), stream_property(S, output)		should_fail.
current_input(S), stream_property(S, alias(A))		should_give true.
current_input(S), stream_property(S, alias(3.5))	should_fail.	% ???
current_input(S), stream_property(S, position(P))	should_give nonvar(P).
% the next 4 fixmes are because stdin is not at eof in some environments
fixme current_input(S), stream_property(S, end_of_stream(P))	should_give P==at.
current_input(S), stream_property(S, eof_action(A))	should_give member(A,[error,eof_code,reset]).
current_input(S), stream_property(S, reposition(P))	should_give member(P,[true,false]).
current_input(S), stream_property(S, type(T))		should_give member(P,[binary,text]).
current_input(S), stream_property(S, noprop(T))	should_throw error(domain_error(stream_property,noprop(E)),_).


fixme at_end_of_stream			should_give true.


fixme at_end_of_stream(user_input)		should_give true.
at_end_of_stream(user_output)		should_fail.
fixme current_input(S), at_end_of_stream(S)	should_give true.
at_end_of_stream(_)			should_throw error(instantiation_error,_).
at_end_of_stream(3.5)			should_throw error(domain_error(stream_or_alias,3.5),_).
at_end_of_stream(foo)			should_throw error(existence_error(stream,foo),_).


open(hello, read, S1), read(S1, _), stream_property(S1, position(P)), close(S1),
set_stream_position(_, P)		should_throw error(instantiation_error,_).
set_stream_position(user_input, _)	should_throw error(instantiation_error,_).

open(hello, read, S1), read(S1, _), stream_property(S1, position(P)), close(S1),
set_stream_position(3.5,P)		should_throw error(domain_error(stream_or_alias,3.5),_).

open(hello, read, S1), read(S1, _), stream_property(S1, position(P)), close(S1),
set_stream_position(S1, 3.5)		should_throw error(domain_error(stream_position,3.5),_).

open(hello, read, S1), read(S1, _), stream_property(S1, position(P)), close(S1),
set_stream_position(foo,P)		should_throw error(existence_error(stream,foo),_).

open(hello, read, S1), read(S1, _), stream_property(S1, position(P)), close(S1),
iso_test_non_repositionable_stream(S),
set_stream_position(S,P)	should_throw error(permission_error(reposition,stream,S),_).

open(hello, read, S1), read(S1, Hello), stream_property(S1, position(P)), close(S1),
open(hello, read, S2), set_stream_position(S2, P), read(S2, World), close(S2)
	should_give Hello==hello, World==world.



%----------- 8.12 character input/output ----------------

get_char(_, _)			should_throw error(instantiation_error,_).
get_char(user_input, 65)	should_throw error(type_error(in_character,65),_).
get_char(user_input, abc)	should_throw error(type_error(in_character,abc),_).
get_char(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
get_char(foo, _)		should_throw error(existence_error(stream,foo),_).
get_char(user_output, _)	should_throw error(permission_error(input,stream,user_output),_).
open(hello, read, S, [type(binary)]),
get_char(S, _)			should_throw error(permission_error(input,binary_stream,S),_).
open(empty, read, S, [type(text)]),
get_char(S, _), get_char(S, C)	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(empty, read, S, [type(text),eof_action(eof_code)]),
get_char(S, C1), get_char(S, C2)	should_give C1==end_of_file,C2==end_of_file.
open(hello, read, S, [type(text)]),
get_char(S, C), close(S)	should_give C==h.
open(hello, read, S, [type(text)]),
get_char(S, h), close(S)	should_give true.
open(empty, read, S, [type(text)]),
get_char(S, C), close(S)	should_give C==end_of_file.
open(empty, read, S, [type(text)]),
get_char(S, end_of_file), close(S)	should_give true.


get_code(_, _)			should_throw error(instantiation_error,_).
get_code(user_input, -2)	should_throw error(representation_error(in_character_code),_).
get_code(user_input, abc)	should_throw error(type_error(integer,abc),_).
get_code(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
get_code(foo, _)		should_throw error(existence_error(stream,foo),_).
get_code(user_output, _)	should_throw error(permission_error(input,stream,user_output),_).
open(hello, read, S, [type(binary)]),
get_code(S, _)			should_throw error(permission_error(input,binary_stream,S),_).
open(empty, read, S, [type(text)]),
get_code(S, _), get_code(S, C)	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(empty, read, S, [type(text),eof_action(eof_code)]),
get_code(S, C1), get_code(S, C2)	should_give C1== -1,C2== -1.
open(hello, read, S, [type(text)]),
get_code(S, C), close(S)	should_give C== 0'h.
open(hello, read, S, [type(text)]),
get_code(S, 0'h), close(S)	should_give true.
open(empty, read, S, [type(text)]),
get_code(S, C), close(S)	should_give C== -1.
open(empty, read, S, [type(text)]),
get_code(S, -1), close(S)	should_give true.


peek_char(_, _)			should_throw error(instantiation_error,_).
peek_char(user_input, 65)	should_throw error(type_error(in_character,65),_).
peek_char(user_input, abc)	should_throw error(type_error(in_character,abc),_).
peek_char(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
peek_char(foo, _)		should_throw error(existence_error(stream,foo),_).
peek_char(user_output, _)	should_throw error(permission_error(input,stream,user_output),_).
open(hello, read, S, [type(binary)]),
peek_char(S, _)			should_throw error(permission_error(input,binary_stream,S),_).
open(empty, read, S, [type(text)]),
get_char(S, _), peek_char(S, C)	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(hello, read, S, [type(text)]),
peek_char(S, C), close(S)	should_give C==h.
open(hello, read, S, [type(text)]),
peek_char(S, h), close(S)	should_give true.
open(empty, read, S, [type(text)]),
peek_char(S, C), close(S)	should_give C==end_of_file.
open(empty, read, S, [type(text)]),
peek_char(S, end_of_file), close(S)	should_give true.


peek_code(_, _)			should_throw error(instantiation_error,_).
peek_code(user_input, -2)	should_throw error(representation_error(in_character_code),_).
peek_code(user_input, abc)	should_throw error(type_error(integer,abc),_).
peek_code(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
peek_code(foo, _)		should_throw error(existence_error(stream,foo),_).
peek_code(user_output, _)	should_throw error(permission_error(input,stream,user_output),_).
open(hello, read, S, [type(binary)]),
peek_code(S, _)			should_throw error(permission_error(input,binary_stream,S),_).
open(empty, read, S, [type(text)]),
get_code(S, _), peek_code(S, C)	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(hello, read, S, [type(text)]),
peek_code(S, C), close(S)	should_give C== 0'h.
open(hello, read, S, [type(text)]),
peek_code(S, 0'h), close(S)	should_give true.
open(empty, read, S, [type(text)]),
peek_code(S, C), close(S)	should_give C== -1.
open(empty, read, S, [type(text)]),
peek_code(S, -1), close(S)	should_give true.


put_char(_, a)			should_throw error(instantiation_error,_).
put_char(user_output, _)	should_throw error(instantiation_error,_).
put_char(user_output, 65)	should_throw error(type_error(character,65),_).
put_char(user_output, end_of_file)	should_throw error(type_error(character,end_of_file),_).
put_char(user_output, abc)	should_throw error(type_error(character,abc),_).
put_char(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
put_char(foo, _)		should_throw error(existence_error(stream,foo),_).
put_char(user_input, _)		should_throw error(permission_error(output,stream,user_input),_).
open(scowen, write, S, [type(binary)]),
put_char(S, a)			should_throw error(permission_error(output,binary_stream,S),_).
open(scowen, write, S, [type(text)]),
put_char(S, a), close(S)	should_give true.
open(scowen, write, S, [type(text)]),
put_char(S, h), close(S)	should_give true.


put_code(_, _)			should_throw error(instantiation_error,_).
put_code(user_output, -2)	should_throw error(representation_error(character_code),_).
put_code(user_output, -1)	should_throw error(representation_error(character_code),_).
put_code(user_output, abc)	should_throw error(type_error(integer,abc),_).
put_code(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
put_code(foo, _)		should_throw error(existence_error(stream,foo),_).
put_code(user_input, _)		should_throw error(permission_error(output,stream,user_input),_).
open(scowen, write, S, [type(binary)]),
put_code(S, 0'a)		should_throw error(permission_error(output,binary_stream,S),_).
open(scowen, write, S, [type(text)]),
put_code(S, 0'a), close(S)	should_give true.
open(scowen, write, S, [type(text)]),
put_code(S, 0'h), close(S)	should_give true.


%----------- 8.13 byte input/output ----------------

% open a binary input stream for testing
close(bin, [force(true)]), open(hello, read, S, [type(binary),alias(bin)])	should_give true.

get_byte(_, _)			should_throw error(instantiation_error,_).
get_byte(bin, -2)		should_throw error(type_error(in_byte,-2),_).
get_byte(bin, abc)		should_throw error(type_error(in_byte,abc),_).
get_byte(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
get_byte(foo, _)		should_throw error(existence_error(stream,foo),_).
get_byte(user_output, _)	should_throw error(permission_error(input,stream,user_output),_).
open(hello, read, S, [type(text)]),
get_byte(S, _)			should_throw error(permission_error(input,text_stream,S),_).
open(empty, read, S, [type(binary)]),
get_byte(S, C), close(S)	should_give C== -1.
open(empty, read, S, [type(binary)]),
get_byte(S, _), get_byte(S, C)	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(hello, read, S, [type(binary)]),
get_byte(S, C), close(S)	should_give C== 0'h.
open(hello, read, S, [type(binary)]),
get_byte(S, 0'h), close(S)	should_give true.


peek_byte(_, _)			should_throw error(instantiation_error,_).
peek_byte(bin, -2)		should_throw error(type_error(in_byte,-2),_).
peek_byte(bin, abc)		should_throw error(type_error(in_byte,abc),_).
peek_byte(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
peek_byte(foo, _)		should_throw error(existence_error(stream,foo),_).
peek_byte(user_output, _)	should_throw error(permission_error(input,stream,user_output),_).
open(hello, read, S, [type(text)]),
peek_byte(S, _)			should_throw error(permission_error(input,text_stream,S),_).
open(empty, read, S, [type(binary)]),
peek_byte(S, C), close(S)	should_give C== -1.
open(empty, read, S, [type(binary)]),
get_byte(S, _), peek_byte(S, C)	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(hello, read, S, [type(binary)]),
peek_byte(S, C), close(S)	should_give C== 0'h.
open(hello, read, S, [type(binary)]),
peek_byte(S, 0'h), close(S)	should_give true.


% open a binary output stream for testing
close(bin, [force(true)]), open(scowen, write, S, [type(binary),alias(bin)])	should_give true.

put_byte(_, _)			should_throw error(instantiation_error,_).
put_byte(bin, -2)		should_throw error(type_error(byte,-2),_).
put_byte(bin, -1)		should_throw error(type_error(byte,-1),_).
put_byte(bin, abc)		should_throw error(type_error(byte,abc),_).
put_byte(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
put_byte(foo, _)		should_throw error(existence_error(stream,foo),_).
put_byte(user_input, _)		should_throw error(permission_error(output,stream,user_input),_).
open(scowen, write, S, [type(text)]),
put_byte(S, 0'a)		should_throw error(permission_error(output,text_stream,S),_).
open(scowen, write, S, [type(binary)]),
put_byte(S, 0'a), close(S)	should_give true.
open(scowen, write, S, [type(binary)]),
put_byte(S, 0'h), close(S)	should_give true.

close(bin)			should_give true.


%----------- 8.14 term input/output ----------------

open(hello, read, _, [alias(tin),type(text)])	should_give true.
open(hello, read, _, [alias(bin),type(binary)])	should_give true.

read_term(_, _, [])		should_throw error(instantiation_error,_).
read_term(tin, _, _)		should_throw error(instantiation_error,_).
read_term(tin, _, [_])		should_throw error(instantiation_error,_).
read_term(tin, _, [variables(_)|_])	should_throw error(instantiation_error,_).
read_term(tin, _, q)		should_throw error(type_error(list,q),_).
read_term(tin, _, [variables(_)|q])	should_throw error(type_error(list,[variables(_)|q]),_).
read_term(3.5, _, [])		should_throw error(domain_error(stream_or_alias,3.5),_).
read_term(tin, _, [q])		should_throw error(domain_error(read_option,q),_).
read_term(foo, _, [])		should_throw error(existence_error(stream,foo),_).
read_term(user_output, _, [])	should_throw error(permission_error(input,stream,user_output),_).
stream_property(S, alias(bin)),
read_term(bin, _, [])		should_throw error(permission_error(input,binary_stream,S),_).
open(empty, read, S, []),
read_term(S,_,[]), read_term(S,_,[])	should_throw error(permission_error(input,past_end_of_stream,S),_).
open(empty, read, S, []),
read_term(S, T, []), close(S)	should_give T==end_of_file.
open(hello, read, S, []),
read_term(S, T, []), close(S)	should_give T==hello.


read_term(_, _)			should_throw error(instantiation_error,_).
read_term(_, [_])		should_throw error(instantiation_error,_).
read_term(_, [variables(_)|_])	should_throw error(instantiation_error,_).
read_term(_, q)			should_throw error(type_error(list,q),_).
read_term(_, [variables(_)|q])	should_throw error(type_error(list,[variables(_)|q]),_).
read_term(_, [q])		should_throw error(domain_error(read_option,q),_).


read(_, _)			should_throw error(instantiation_error,_).
read(3.5, _)			should_throw error(domain_error(stream_or_alias,3.5),_).
read(foo, _)			should_throw error(existence_error(stream,foo),_).
read(user_output, _)		should_throw error(permission_error(input,stream,user_output),_).
stream_property(S, alias(bin)),
read(bin, _)			should_throw error(permission_error(input,binary_stream,S),_).
open(empty, read, S, []),
read(S,_), read(S,_)		should_throw error(permission_error(input,past_end_of_stream,S),_).
open(empty, read, S, []),
read(S, T), close(S)		should_give T==end_of_file.
open(hello, read, S, []),
read(S, T), close(S)		should_give T==hello.

close(tin), close(bin)		should_give true.


open(scowen, write, _, [alias(tout),type(text)])	should_give true.
open(scowen, write, _, [alias(bout),type(binary)])	should_give true.

write_term(_, _, [])		should_throw error(instantiation_error,_).
write_term(tout, _, _)		should_throw error(instantiation_error,_).
write_term(tout, _, [_])	should_throw error(instantiation_error,_).
write_term(tout, _, [quoted(true)|_])	should_throw error(instantiation_error,_).
write_term(tout, _, q)		should_throw error(type_error(list,q),_).
write_term(tout, _, [quoted(true)|q])	should_throw error(type_error(list,[quoted(true)|q]),_).
write_term(3.5, _, [])		should_throw error(domain_error(stream_or_alias,3.5),_).
write_term(tout, _, [q])	should_throw error(domain_error(write_option,q),_).
write_term(tout, _, [quoted(on)])	should_throw error(domain_error(write_option,quoted(on)),_).
write_term(tout, _, [attributes(none)])	should_throw error(domain_error(write_option,attributes(none)),_).
write_term(foo, _, [])		should_throw error(existence_error(stream,foo),_).
write_term(user_input, _, [])	should_throw error(permission_error(output,stream,user_input),_).
stream_property(S, alias(bout)),
write_term(bout, _, [])		should_throw error(permission_error(output,binary_stream,S),_).


write_term(_, _)		should_throw error(instantiation_error,_).
write_term(_, _)		should_throw error(instantiation_error,_).
write_term(_, [_])		should_throw error(instantiation_error,_).
write_term(_, [quoted(true)|_])	should_throw error(instantiation_error,_).
write_term(_, q)		should_throw error(type_error(list,q),_).
write_term(_, [quoted(true)|q])	should_throw error(type_error(list,[quoted(true)|q]),_).
write_term(_, [q])		should_throw error(domain_error(write_option,q),_).
write_term(_, [quoted(on)])	should_throw error(domain_error(write_option,quoted(on)),_).
write_term(_, [attributes(none)])	should_throw error(domain_error(write_option,attributes(none)),_).


write(_, _)			should_throw error(instantiation_error,_).
write(3.5, _)			should_throw error(domain_error(stream_or_alias,3.5),_).
write(foo, _)			should_throw error(existence_error(stream,foo),_).
write(user_input, _)		should_throw error(permission_error(output,stream,user_input),_).
stream_property(S, alias(bout)),
write(bout, _)			should_throw error(permission_error(output,binary_stream,S),_).


writeq(_, _)			should_throw error(instantiation_error,_).
writeq(3.5, _)			should_throw error(domain_error(stream_or_alias,3.5),_).
writeq(foo, _)			should_throw error(existence_error(stream,foo),_).
writeq(user_input, _)		should_throw error(permission_error(output,stream,user_input),_).
stream_property(S, alias(bout)),
writeq(bout, _)			should_throw error(permission_error(output,binary_stream,S),_).


write_canonical(_, _)		should_throw error(instantiation_error,_).
write_canonical(3.5, _)		should_throw error(domain_error(stream_or_alias,3.5),_).
write_canonical(foo, _)		should_throw error(existence_error(stream,foo),_).
write_canonical(user_input, _)	should_throw error(permission_error(output,stream,user_input),_).
stream_property(S, alias(bout)),
write_canonical(bout, _)	should_throw error(permission_error(output,binary_stream,S),_).

close(tout), close(bout)	should_give true.


op(_,fx,fx)			should_throw error(instantiation_error,_).
op(1,_,fx)			should_throw error(instantiation_error,_).
op(1,fx,_)			should_throw error(instantiation_error,_).
op(1,fx,[fx|_])			should_throw error(instantiation_error,_).
op(1,fx,[_])			should_throw error(instantiation_error,_).
op(a,fx,fx)			should_throw error(type_error(integer,a),_).
op(1,2,fx)			should_throw error(type_error(atom,2),_).
op(1,fx,3)			should_throw error(type_error(list,3),_).	%%% std says 'list'
op(1,fx,[fx|y])			should_throw error(type_error(list,[fx|y]),_).
op(1,fx,[[fx]])			should_throw error(type_error(atom,[fx]),_).
op(1,fx,[3])			should_throw error(type_error(atom,3),_).
op(1201,fx,fx)			should_throw error(domain_error(operator_priority,1201),_).
op(1,xyf,f)			should_throw error(domain_error(operator_specifier,xyf),_).
op(1,fxy,f)			should_throw error(domain_error(operator_specifier,fxy),_).
op(500,xfy,[])			should_throw error(permission_error(create,operator,[]),_).
op(500,xfy,[[]])		should_throw error(permission_error(create,operator,[]),_).
op(500,xfy,{})			should_throw error(permission_error(create,operator,{}),_).
op(500,xfy,',')			should_throw error(permission_error(modify,operator,','),_).
op(500,xfy,[','])		should_throw error(permission_error(modify,operator,','),_).
op(1000,xfy,'|')		should_throw error(permission_error(create,operator,'|'),_).
op(1,xfy,pi)			should_give true.
op(1,xf,pi)			should_throw error(permission_error(create,operator,pi),_).
op(0,xfy,pi)			should_give true.
op(1001,xfy,'|')		should_give true.
op(0,xfy,'|')			should_give true.

% Official examples
op(30, xfy, ++)			should_give true.
op(0, yfx, ++)			should_give true.
op(max, xfy, ++)		should_throw error(type_error(integer, max),_).
op(-30, xfy, ++)		should_throw error(domain_error(operator_priority, -30),_).
op(1201, xfy, ++)		should_throw error(domain_error(operator_priority, 1201),_).
op(30, XFY, ++)			should_throw error(instantiation_error,_).
op(30, yfy, ++)			should_throw error(domain_error(operator_specifier, yfy),_).
op(30, xfy, 0)			should_throw error(type_error(list, 0),_).	%%% std says 'list'
op(30, xfy, ++), op(40, xfx, ++) should_give true.
	%Succeeds, making ++ a non-associative infix operator with priority 40.
op(30, xfy, ++), op(50, yf, ++)	should_throw error(permission_error(create, operator, ++),_).
	%[There cannot be an infix and a postfix operator with the same name.]
op(0, yfx, ++)			should_give true.


%current_op(?integer,?operator_specifier,?atom)
% Next line required by std, but should be type_error(integer)
current_op(a,_,_)		should_throw error(domain_error(operator_priority,a),_).
current_op(-1,_,_)		should_throw error(domain_error(operator_priority,-1),_).
% Next line required by std, but should be type_error(atom)
%current_op(1,2,_)		should_throw error(domain_error(operator_specifier,2),_).
current_op(1,2,_)		should_throw error(type_error(atom,2),_).
current_op(1,xyf,_)		should_throw error(domain_error(operator_specifier,xyf),_).
current_op(1,fxy,_)		should_throw error(domain_error(operator_specifier,fxy),_).
current_op(1,fx,3)		should_throw error(type_error(atom,3),_).


char_conversion(_,b)		should_throw error(instantiation_error,_).
char_conversion(a,_)		should_throw error(instantiation_error,_).
char_conversion(1,b)		should_throw error(representation_error(character),_).
char_conversion(a,2)		should_throw error(representation_error(character),_).
current_char_conversion(1,b)	should_throw error(type_error(character,1),_).
current_char_conversion(a,2)	should_throw error(type_error(character,2),_).
char_conversion('&',',')	should_give true.
current_char_conversion('&',I)	should_give I==(',').
current_char_conversion(X,',')	should_give X=='&'.
char_conversion('&','&')	should_give true.
current_char_conversion(_,',')	should_fail.


%----------- 8.15 logic and control ----------------

\+ _				should_throw error(instantiation_error,_).
\+ 3				should_throw error(type_error(callable,3),_).


once(_)				should_throw error(instantiation_error,_).
once(3)				should_throw error(type_error(callable,3),_).


repeat				should_give true.
repeat, !, fail			should_fail.


call(_,_)			should_throw error(instantiation_error,_).
call(3,_)			should_throw error(type_error(callable,3),_).
call(3,_,_)			should_throw error(type_error(callable,3),_).
call(3,_,_,_)			should_throw error(type_error(callable,3),_).
call(3,_,_,_,_)			should_throw error(type_error(callable,3),_).
call(3,_,_,_,_,_)		should_throw error(type_error(callable,3),_).
call(3,_,_,_,_,_,_)		should_throw error(type_error(callable,3),_).
call(3,_,_,_,_,_,_,_)		should_throw error(type_error(callable,3),_).
%current_prolog_flag(max_arity, A), functor(T, foo, a)
%call(T,_)			should_throw error(representation_error(max_arity),_).

call(integer, 3)		should_give true.
call(functor(F,c), 0)		should_give F==c.
call(call(call(atom_concat, pro), log), Atom)	should_give Atom==prolog.
call(;, X = 1, X = 2)		should_give multiple_solutions(K,K==2,X==K).
call(;, (true->fail), X=1)	should_fail.

call(',', fail, 3)		should_throw error(type_error(callable,(fail,3)),_).
call(';', !, 3)			should_throw error(type_error(callable,(!;3)),_).
call('->', fail, 3)		should_throw error(type_error(callable,(fail->3)),_).
call(',', C=!, (X=1,C;X=2))	should_give multiple_solutions(K,K==2,X==K).

call(','(fail), 3)		should_throw error(type_error(callable,(fail,3)),_).
call(';'(!), 3)			should_throw error(type_error(callable,(!;3)),_).
call('->'(fail), 3)		should_throw error(type_error(callable,(fail->3)),_).
call(','(C=!), (X=1,C;X=2))	should_give multiple_solutions(K,K==2,X==K).


false				should_fail.


%----------- 8.16 atomic term processing ----------------

atom_length(_,3)			should_throw error(instantiation_error,_).
atom_length(1,3)			should_throw error(type_error(atom,1),_).
atom_length(a,a)			should_throw error(type_error(integer,a),_).
atom_length(a,-1)			should_throw error(domain_error(not_less_than_zero,-1),_).


atom_concat(_,b,_)			should_throw error(instantiation_error,_).
atom_concat(a,_,_)			should_throw error(instantiation_error,_).
atom_concat(1,b,_)			should_throw error(type_error(atom,1),_).
atom_concat(a,2,_)			should_throw error(type_error(atom,2),_).
atom_concat(a,b,3)			should_throw error(type_error(atom,3),_).

% Official examples
atom_concat(hello,' world',S3)		should_give S3=='hello world'.
atom_concat(T,' world','small world')	should_give T==small.
atom_concat(hello,' world','small world')	should_fail.
atom_concat(T1,T2,hello)		should_give multiple_solutions(K,K==6,
	(K==1-> T1=='', T2==hello
	;K==2-> T1==h,   T2==ello
	;K==3-> T1==he,   T2==llo
	;K==4-> T1==hel,   T2==lo
	;K==5-> T1==hell,   T2==o
	;K==6-> T1==hello, T2==''
	)).


sub_atom(_,_,_,_,_)			should_throw error(instantiation_error,_).
sub_atom(3,_,_,_,_)			should_throw error(type_error(atom,3),_).
sub_atom(a,_,_,_,3)			should_throw error(type_error(atom,3),_).
sub_atom(a,b,_,_,_)			should_throw error(type_error(integer,b),_).
sub_atom(a,_,c,_,_)			should_throw error(type_error(integer,c),_).
sub_atom(a,_,_,d,_)			should_throw error(type_error(integer,d),_).
sub_atom(a,-1,_,_,_)			should_throw error(domain_error(not_less_than_zero,-1),_).
sub_atom(a,_,-1,_,_)			should_throw error(domain_error(not_less_than_zero,-1),_).
sub_atom(a,_,_,-1,_)			should_throw error(domain_error(not_less_than_zero,-1),_).

% Official examples
sub_atom(abracadabra, 0, 5, _, S2)	should_give S2==abrac.
sub_atom(abracadabra, _, 5, 0, S2)	should_give S2==dabra.
sub_atom(abracadabra, 3, L, 3, S2)	should_give L==5, S2 == 'acada'.
sub_atom(abracadabra, B, 2, A, ab)	should_give multiple_solutions(K,K==2,
	(K==1-> B == 0 , A == 9
	;K==2-> B == 7 , A == 2)).
sub_atom('Banana', 3, 2, _, S2)		should_give S2==an.
sub_atom('charity', _, 3, _, S2)	should_give multiple_solutions(K,K==5,
	(K==1-> S2 == 'cha'
	;K==2-> S2 == 'har'
	;K==3-> S2 == 'ari'
	;K==4-> S2 == 'rit'
	;K==5-> S2 == 'ity'
	)).
sub_atom('ab', Start, Length, _, Sub_atom) should_give multiple_solutions(K,K==6,
	(K==1-> Start == 0, Length == 0, Sub_atom == ''
	;K==2-> Start == 0, Length == 1, Sub_atom == 'a'
	;K==3-> Start == 0, Length == 2, Sub_atom == 'ab'
	;K==4-> Start == 1, Length == 0, Sub_atom == ''
	;K==5-> Start == 1, Length == 1, Sub_atom == 'b'
	;K==6-> Start == 2, Length == 0, Sub_atom == ''
	)).


atom_chars(_,_)			should_throw error(instantiation_error,_).
atom_chars(_,[a|_])		should_throw error(instantiation_error,_).
atom_chars(_,[a,_])		should_throw error(instantiation_error,_).
atom_chars(1,_)			should_throw error(type_error(atom,1),_).
atom_chars(_,b)			should_throw error(type_error(list,b),_).
atom_chars(_,[a|b])		should_throw error(type_error(list,[a|b]),_).
atom_chars(_,[1])		should_throw error(type_error(character,1),_).
atom_chars(_,[abc])		should_throw error(type_error(character,abc),_).


atom_codes(_,_)			should_throw error(instantiation_error,_).
atom_codes(_,[0'a|_])		should_throw error(instantiation_error,_).
atom_codes(_,[0'a,_])		should_throw error(instantiation_error,_).
atom_codes(1,_)			should_throw error(type_error(atom,1),_).
atom_codes(_,0'b)		should_throw error(type_error(list,0'b),_).
atom_codes(_,[0'a|0'b])		should_throw error(type_error(list,[0'a|0'b]),_).
%atom_codes(_,[a])		should_throw error(type_error(integer,a),_).
atom_codes(_,[a])		should_throw error(representation_error(character_code),_).
atom_codes(_,[-1])		should_throw error(representation_error(character_code),_).


char_code(_,_)			should_throw error(instantiation_error,_).
char_code(1,_)			should_throw error(type_error(character,1),_).
char_code(abc,_)		should_throw error(type_error(character,abc),_).
char_code(_,a)			should_throw error(type_error(integer,a),_).
char_code(_,-1)			should_throw error(representation_error(character_code),_).


number_chars(_,_)		should_throw error(instantiation_error,_).
number_chars(_,[a|_])		should_throw error(instantiation_error,_).
number_chars(_,[a,_])		should_throw error(instantiation_error,_).
number_chars(a,_)		should_throw error(type_error(number,a),_).
number_chars(_,b)		should_throw error(type_error(list,b),_).
number_chars(_,[a|b])		should_throw error(type_error(list,[a|b]),_).
number_chars(_,[1])		should_throw error(type_error(character,1),_).
number_chars(_,[abc])		should_throw error(type_error(character,abc),_).
number_chars(_,[a])		should_throw error(syntax_error(_),_).

% Neumerkel instances
number_chars(1.2,['1',.,'2'])	should_give true.
number_chars(1.0e9,['1',.,'0','E','9'])	should_give true.
number_chars(1,['0','1'])	should_give true.
number_chars(1,[a])		should_throw error(syntax_error(_),_).
number_chars(1,[])		should_throw error(syntax_error(_),_).
number_chars(1,[[]])		should_throw error(type_error(character,[]),_).
number_chars(1,[' ',[]])	should_throw error(type_error(character,[]),_).
number_chars(1,[0])		should_throw error(type_error(character,0),_).
number_chars(1,[_,[]])		should_throw error(type_error(character,[]),_).
number_chars(N,[X])		should_throw error(instantiation_error,_).
number_chars(N,['0'|_])		should_throw error(instantiation_error,_).
number_chars(N,'1')		should_throw error(type_error(list,'1'),_).
number_chars(N,[a|a])		should_throw error(type_error(list,[a|a]),_).
number_chars(N,[49])		should_throw error(type_error(character,49),_).
number_chars(N,[])		should_throw error(syntax_error(_),_).
number_chars(N,['3',' '])	should_throw error(syntax_error(_),_).
number_chars(N,['3',.])		should_throw error(syntax_error(_),_).
number_chars(N,[' ','1'])	should_give N== 1.
number_chars(N,['\n','1'])	should_give N== 1.
number_chars(N,[' ','0','\'',a])	should_give N==97.
number_chars(N,[-,' ','1'])	should_give N== -1.
number_chars(N,[/,*,*,/,'1'])	should_give N== 1.
number_chars(N,['%','\n','1'])	should_give N== 1.
number_chars(N,[-,/,*,*,/,'1'])	should_throw error(syntax_error(_),_).
number_chars(N,['1',e,'1'])	should_throw error(syntax_error(_),_).
number_chars(N,['1',.,'0',e])	should_throw error(syntax_error(_),_).
number_chars(N,['1',.,'0',e,e])	should_throw error(syntax_error(_),_).
number_chars(N,['0',x,'1'])	should_give N==1.
number_chars(N,['0','X','1'])	should_throw error(syntax_error(_),_).
number_chars(1,[C])		should_give C=='1'.
number_chars(1,[C,D])		should_fail.
number_chars(1,[C,C])		should_fail.
number_chars(0,[C,C])		should_fail.
number_chars(10,[C,D])		should_give [C,D]==['1','0'].
number_chars(100,[C,D])		should_fail.
number_chars(N,[X|2])		should_throw error(type_error(list,[_|2]),_).
number_chars(N,[1|_])		should_throw error(type_error(character,1),_). % or 4
number_chars(V,[1|2])		should_throw error(type_error(character,1),_).
number_chars([],1)		should_throw error(type_error(list,1),_).
number_chars(1,1)		should_throw error(type_error(list,1),_).
number_chars(1,[a|2])		should_throw error(type_error(list,[a|2]),_).
number_chars(1,[_|2])		should_throw error(type_error(list,[_|2]),_).
number_chars(1,[[]|_])		should_throw error(type_error(character,[]),_).
number_chars(1,[[]|2])		should_throw error(type_error(character,[]),_).
%L=['1'|L], number_chars(N,L)	should_fail.


number_codes(_,_)		should_throw error(instantiation_error,_).
number_codes(_,[0'a|_])		should_throw error(instantiation_error,_).
number_codes(_,[0'a,_])		should_throw error(instantiation_error,_).
number_codes(a,_)		should_throw error(type_error(number,a),_).
number_codes(_,0'b)		should_throw error(type_error(list,0'b),_).
number_codes(_,[0'a|0'b])	should_throw error(type_error(list,[0'a|0'b]),_).
%number_codes(_,[a])		should_throw error(type_error(integer,a),_).
number_codes(_,[a])		should_throw error(representation_error(character_code),_).
number_codes(_,[-1])		should_throw error(representation_error(character_code),_).
number_codes(_,[0'a])		should_throw error(syntax_error(_),_).
number_codes(N,"- /**/1")	should_give N== -1.
number_codes(N,"/**/- /**/3")	should_give N== -3.
number_codes(N,"3/**/")		should_throw error(syntax_error(_),_).

% Neumerkel instances
number_codes(1.2,"1.2")		should_give true.
number_codes(1.0e9,"1.0E9")	should_give true.
number_codes(1,"01")		should_give true.
number_codes(1,"a")		should_throw error(syntax_error(_),_).
number_codes(1,"")		should_throw error(syntax_error(_),_).
number_codes(1,[[]])		should_throw error(representation_error(character_code),_).
number_codes(1,[0' ,[]])	should_throw error(representation_error(character_code),_).
number_codes(1,['0'])		should_throw error(representation_error(character_code),_).
number_codes(1,[_,[]])		should_throw error(representation_error(character_code),_).
number_codes(N,[X])		should_throw error(instantiation_error,_).
number_codes(N,[0'0|_])		should_throw error(instantiation_error,_).
number_codes(N,0'1)		should_throw error(type_error(list,0'1),_).
number_codes(N,[0'a|0'a])	should_throw error(type_error(list,[0'a|0'a]),_).
number_codes(N,['1'])		should_throw error(representation_error(character_code),_).
number_codes(N,"")		should_throw error(syntax_error(_),_).
number_codes(N,"3 ")		should_throw error(syntax_error(_),_).
number_codes(N,"3.")		should_throw error(syntax_error(_),_).
number_codes(N," 1")		should_give N== 1.
number_codes(N,"\n1")		should_give N== 1.
number_codes(N," 0'a")		should_give N==97.
number_codes(N,"- 1")		should_give N== -1.
number_codes(N,"/**/1")		should_give N== 1.
number_codes(N,"%\n1")		should_give N== 1.
number_codes(N,"-/**/1")	should_throw error(syntax_error(_),_).
number_codes(N,"1e1")		should_throw error(syntax_error(_),_).
number_codes(N,"1.0e")		should_throw error(syntax_error(_),_).
number_codes(N,"1.0ee")		should_throw error(syntax_error(_),_).
number_codes(N,"0x1")		should_give N==1.
number_codes(N,"0X1")		should_throw error(syntax_error(_),_).
number_codes(1,[C])		should_give C==0'1.
number_codes(1,[C,D])		should_fail.
number_codes(1,[C,C])		should_fail.
number_codes(0,[C,C])		should_fail.
number_codes(10,[C,D])		should_give [C,D]==[0'1,0'0].
number_codes(100,[C,D])		should_fail.
number_codes(N,[X|2])		should_throw error(type_error(list,[_|2]),_).
number_codes(N,['1'|_])		should_throw error(representation_error(character_code),_). % or 4
number_codes(V,['1'|2])		should_throw error(representation_error(character_code),_).
number_codes([],1)		should_throw error(type_error(list,1),_).
number_codes(1,1)		should_throw error(type_error(list,1),_).
number_codes(1,[0'a|2])		should_throw error(type_error(list,[0'a|2]),_).
number_codes(1,[_|2])		should_throw error(type_error(list,[_|2]),_).
number_codes(1,[[]|_])		should_throw error(representation_error(character_code),_).
number_codes(1,[[]|2])		should_throw error(representation_error(character_code),_).
%L=['1'|L], number_codes(N,L)	should_fail.


%----------- 8.17 implementation-defined hooks ----------------

set_prolog_flag(_, off)		should_throw error(instantiation_error,_).
set_prolog_flag(debug, _)	should_throw error(instantiation_error,_).
set_prolog_flag(5, decimals)	should_throw error(type_error(atom,5),_).
set_prolog_flag(f(b), 0)	should_throw error(type_error(atom,f(b)),_).
set_prolog_flag(date, 'July 1988')	should_throw error(domain_error(prolog_flag,date),_).
set_prolog_flag(debug, trace)	should_throw error(domain_error(flag_value,debug+trace),_).
set_prolog_flag(bounded, true)	should_throw error(permission_error(modify,flag,bounded),_).
set_prolog_flag(max_integer, 2)	should_throw error(permission_error(modify,flag,max_integer),_).

current_prolog_flag(debug, off)	should_give true.
current_prolog_flag(5, _)	should_throw error(type_error(atom,5),_).
current_prolog_flag(f(b), _)	should_throw error(type_error(atom,f(b)),_).
current_prolog_flag(date, _)	should_throw error(domain_error(prolog_flag,date),_).
current_prolog_flag(_,_)	should_give multiple_solutions(K,K>=7,true).

current_prolog_flag(bounded,X)
	should_give (X==true;X==false).
(current_prolog_flag(bounded,false)-> X=0 ; current_prolog_flag(max_integer,X))
	should_give integer(X).
(current_prolog_flag(bounded,false)-> X=0 ; current_prolog_flag(min_integer,X))
	should_give integer(X).
current_prolog_flag(integer_rounding_function,X)
	should_give (X==down;X==toward_zero).
current_prolog_flag(max_arity,X)
	should_give (integer(X);X==unbounded).
current_prolog_flag(debug,X)
	should_give (X==on;X==off).
current_prolog_flag(char_conversion,X)
	should_give (X==on;X==off).
current_prolog_flag(double_quotes,X)
	should_give (X==chars;X==codes;X==atom).
current_prolog_flag(unknown,X)
	should_give (X==error;X==fail;X==warning).


%----------- 9 Arithmetic ----------------

% In ECLiPSe:
% float_overflow -> infinity
% float_underflow -> 0.0 or denormal
% int_overflow -> out of memory

X is _			should_throw error(instantiation_error,_).
X is -_			should_throw error(instantiation_error,_).
X is foo		should_throw error(type_error(evaluable,foo/0),_).
X is foo(9)		should_throw error(type_error(evaluable,foo/1),_).
X is foo+1		should_throw error(type_error(evaluable,foo/0),_).

X is 1 // 0		should_throw error(evaluation_error(zero_divisor),_).
X is 0 mod 0		should_throw error(evaluation_error(zero_divisor),_).
X is 1 mod 0		should_throw error(evaluation_error(zero_divisor),_).
X is 0 div 0		should_throw error(evaluation_error(zero_divisor),_).
X is 1 div 0		should_throw error(evaluation_error(zero_divisor),_).
X is rem(1,0)		should_throw error(type_error(evaluable,(rem)/2),_).

X is 5**3		should_give X== 125.0.
X is -5.0**3		should_give X== -125.0.
X is 5** -1		should_give X== 0.2.
X is 5** 3.0		should_give X== 125.0.
X is 0.0**0		should_give X== 1.0.
X is -2**3.1		should_throw error(evaluation_error(undefined),_).
X is -2.0**3.1		should_throw error(evaluation_error(undefined),_).
X is -2**3.0		should_throw error(evaluation_error(undefined),_).
X is -2.0**3.0		should_throw error(evaluation_error(undefined),_).
%X is 0 ** -2		should_throw error(evaluation_error(undefined),_).	%%% should be 'zero_divisor'
X is 0 ** -2		should_give X==1.0Inf.	% zero_divisor
X is 0.0 ** -2		should_give X==1.0Inf.	% zero_divisor
X is 0.0 ** -2.0	should_give X==1.0Inf.	% zero_divisor

X is 5^3		should_give X== 125.
X is -5^3		should_give X== -125.
X is -5.0^3		should_give X== -125.0.
X is 5^ -1		should_throw error(type_error(float,5),_).		% almost (e)
X is 5^3.0		should_give X== 125.0.
X is 0.0^0		should_give X== 1.0.
X is -2^3.1		should_throw error(evaluation_error(undefined),_).	% (c)
X is -2^3.0		should_give X== -8.0.
X is 0 ^ -2		should_throw error(evaluation_error(undefined),_).	%%% (d) should be 'zero_divisor'

X is 0^0		should_give X== 1.
X is 3^1.0		should_give X== 3.0.
X is 3^3		should_give X== 27.
X is 3^27		should_give X== 7625597484987.
X is 3^3^3		should_give X== 7625597484987.
X is 2^ -1		should_throw error(type_error(float,2),_).		% almost (e), COR2 says evaluation_error(undefined)
X is 2^ -2		should_throw error(type_error(float,2),_).		% (e)
X is 1^ -1		should_give X== 1.
X is 2^ -1.5		should_give abs(X-0.35353) < 0.0001.
%X is 0.0 ^ -2.0	should_throw error(evaluation_error(undefined),_).	%%% (d) should be 'zero_divisor'
X is 0.0 ^ -2.0		should_give X==1.0Inf.	% zero_divisor
X is 0 ^ -2.0		should_give X==1.0Inf.	% zero_divisor
X is 0.0 ^ -2		should_give X==1.0Inf.	% zero_divisor

X is 2 ^ -2		should_throw error(type_error(float,2),_).
X is 1 ^ -2		should_give X == 1.
%X is 0 ^ -2		should_throw error(evaluation_error(zero_divisor),_).
X is 0 ^ -2		should_throw error(evaluation_error(undefined),_).	%%% (d) should be 'zero_divisor'
X is -1 ^ -2		should_give X == 1.
X is -2 ^ -2		should_throw error(type_error(float,-2),_).

X is 2 ^ -1		should_throw error(type_error(float,2),_).
X is 1 ^ -1		should_give X == 1.
%X is 0 ^ -1		should_throw error(evaluation_error(zero_divisor),_).
X is 0 ^ -1		should_throw error(evaluation_error(undefined),_).	%%% (d) should be 'zero_divisor'
X is -1 ^ -1		should_give X == -1.
X is -2 ^ -1		should_throw error(type_error(float,-2),_).

X is asin(1.1)		should_throw error(evaluation_error(undefined),_).
X is asin(-1.1)		should_throw error(evaluation_error(undefined),_).
X is acos(1.1)		should_throw error(evaluation_error(undefined),_).
X is acos(-1.1)		should_throw error(evaluation_error(undefined),_).
X is atan2(0.0,0.0)	should_throw error(evaluation_error(undefined),_).	% debatable
X is sqrt(-1)		should_throw error(evaluation_error(undefined),_).

% These are ISO requirements, but contradicting IEEE 754:
%X is 0 / 0		should_throw error(evaluation_error(zero_divisor),_).	%%% should be 'undefined'
%X is 0 // 0		should_throw error(evaluation_error(zero_divisor),_).	%%% should be 'undefined'
%X is log(0.0)		should_throw error(evaluation_error(undefined),_).	%%% should be 'zero_divisor'
%X is 0.0 ** -2.0	should_throw error(evaluation_error(undefined),_).	%%% should be 'zero_divisor'
%X is 0.0 ^  -2.0	should_throw error(evaluation_error(undefined),_).	%%% should be 'zero_divisor'
%X is 0 ^ -2		should_throw error(evaluation_error(undefined),_).	%%% should be 'zero_divisor'

% ... and the corresponding desirable results:
X is 0 / 0		should_throw error(evaluation_error(undefined),_).	% nan
X is 0 // 0		should_throw error(evaluation_error(undefined),_).
%X is log(0.0)		should_throw error(evaluation_error(zero_divisor),_).	% -inf
%X is 0.0 ** -2.0	should_throw error(evaluation_error(zero_divisor),_).	% inf
%X is 0.0 ^  -2.0	should_throw error(evaluation_error(zero_divisor),_).	% inf
%X is 0 ^ -2		should_throw error(evaluation_error(zero_divisor),_).	% inf

% Selection of critical examples (for ECLiPSe)
X is floor(3.5)		should_give X==3.
X is ceiling(3.5)	should_give X==4.
X is truncate(3.5)	should_give X==3.
X is round(3.5)		should_give X==4.
X is round(4.5)		should_give X==5.
X is floor(-3.5)	should_give X== -4.
X is ceiling(-3.5)	should_give X== -3.
X is truncate(-3.5)	should_give X== -3.
X is round(-3.5)	should_give X== -3.
X is round(-4.5)	should_give X== -4.
X=log(9.9), X>0		should_give true.
X=log(9.9), _ is X+1	should_give true.

X is log(0.0)		should_give X == -1.0Inf.
X is log(0)		should_give X == -1.0Inf.

X is 1.0 /\ 2		should_throw error(type_error(integer,1.0),_).
X is 1 /\ 2.0		should_throw error(type_error(integer,2.0),_).
X is 5 /\ 7		should_give X==5.

X is 1.0 \/ 2		should_throw error(type_error(integer,1.0),_).
X is 1 \/ 2.0		should_throw error(type_error(integer,2.0),_).
X is 5 \/ 7		should_give X==7.

X is \1.0		should_throw error(type_error(integer,1.0),_).
X is \5			should_give X== -6.

X is xor(1.0,2)		should_throw error(type_error(integer,1.0),_).
X is xor(1,2.0)		should_throw error(type_error(integer,2.0),_).
X is xor(5,7)		should_give X==2.

X is 1.0 >> 2		should_throw error(type_error(integer,1.0),_).
X is 1 >> 2.0		should_throw error(type_error(integer,2.0),_).
X is 10 >> 1		should_give X==5.

X is 1.0 << 2		should_throw error(type_error(integer,1.0),_).
X is 1 << 2.0		should_throw error(type_error(integer,2.0),_).
X is 10 << 1		should_give X==20.

X is 1.0 // 2		should_throw error(type_error(integer,1.0),_).
X is 1.0 div 2		should_throw error(type_error(integer,1.0),_).
X is 1.0 mod 2		should_throw error(type_error(integer,1.0),_).

X is 1 // 2.0		should_throw error(type_error(integer,2.0),_).
X is 1 div 2.0		should_throw error(type_error(integer,2.0),_).
X is 1 mod 2.0		should_throw error(type_error(integer,2.0),_).

X is min(2,3)		should_give X==2.
X is min(2,3.0)		should_give X=:=2.
X is min(2.0,3)		should_give X=:=2.
X is min(0,0.0)		should_give X=:=0.

X is max(2,3)		should_give X==3.
X is max(2,3.0)		should_give X=:=3.
X is max(2.0,3)		should_give X=:=3.
X is max(0,0.0)		should_give X=:=0.

X is pi			should_give abs(X-3.14159) < 0.0001.

