% Auxiliary predicates used in the tests.  Some of these cannot be
% written with strict ISO functionality and are thus system-specific.
%
% This version is for ECLiPSe - adapt for your system


% compile/load a source file
iso_test_ensure_loaded(File) :-
	eclipse_language:ensure_loaded(File).

% return operating system name as 'win' or 'unix'
iso_test_os(OS) :- 
	eclipse_language:get_flag(hostarch, ArchS),
	eclipse_language:atom_string(Arch, ArchS),
	( sub_atom(Arch, _, _, 0, '_nt') -> OS=win ; OS=unix ).

% create a non-repositionable I/O stream
iso_test_non_repositionable_stream(S) :-
	eclipse_language:atom_string(abc,Content),
	eclipse_language:open(queue(Content),read,S).

% test whether two terms are variants of each other
iso_test_variant(X,Y) :-
	subsumes_term(X, Y),
	subsumes_term(Y, X).

% test whether two lists have the same set of members
iso_test_same_members(Xs,Ys) :-
	sort(Xs,SXs),
	sort(Ys,SYs),
	SXs==SYs.

