% Test program for fresh variables per clause instance
% This corresponds to test_fresh_vars_per_clause_instance

t(X) :- X = 1.
t(X) :- X = 2.

test_query :-
    findall(X-Y, (t(X), t(Y)), Solutions),
    writeln(Solutions).

% Interactive test - shows the trace
test_with_trace :-
    trace,
    t(X), t(Y),
    writeln(X-Y),
    fail.
test_with_trace.

% Automated trace dump
test_with_json_trace :-
    consult(trace_dump),
    with_trace_json((t(X), t(Y), writeln(X-Y), fail ; true), 'swi_trace.jsonl', []).