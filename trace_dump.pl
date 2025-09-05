:- module(trace_dump,
  [ with_trace_json/2,           % +Goal, +File
    with_trace_json/3,           % +Goal, +File, +Options
    enable_trace_json/1,         % +File
    disable_trace_json/0
  ]).

:- use_module(library(http/json)).     % json_write_dict/3
:- use_module(library(lists)).         % memberchk/2

:- thread_local tl_trace_stream/1.
:- thread_local tl_trace_opts/1.
:- multifile user:prolog_trace_interception/4.

%% Public API

with_trace_json(Goal, File) :-
    with_trace_json(Goal, File, []).

with_trace_json(Goal, File, Options0) :-
    default_options(Defaults),
    (Options0 == [] -> Opts = Defaults ; merge_options_(Options0, Defaults, Opts)),
    setup_call_cleanup(
        ( open(File, write, S),
          asserta(tl_trace_stream(S)),
          asserta(tl_trace_opts(Opts)),
          set_prolog_flag(color_term, false),
          leash(-all),
          visible(+all),
          trace ),
        call(Goal),
        ( notrace,
          retractall(tl_trace_opts(_)),
          retractall(tl_trace_stream(_)),
          close(S)
        )).

enable_trace_json(File) :-
    default_options(Defaults),
    merge_options_([], Defaults, Opts),
    open(File, write, S),
    asserta(tl_trace_stream(S)),
    asserta(tl_trace_opts(Opts)),
    set_prolog_flag(color_term, false),
    leash(-all),
    visible(+all),
    trace.

disable_trace_json :-
    notrace,
    ( retract(tl_trace_stream(S)) -> close(S) ; true ),
    retractall(tl_trace_opts(_)).

%% Options and defaults

default_options(_{
  ports: [call,redo,exit,fail],     % which tracer ports to log
  max_depth: 50,                    % limit term printing depth
  include_source: true,             % include file/line if available
  include_alts: true,               % include has_alternatives flag
  include_pi: true,                 % include predicate indicator
  include_goal: true,               % include goal as string
  include_thread: true,             % include thread id
  max_events: infinite              % or an integer to stop early
}).

merge_options_(User, Def, Merged) :-
    % simple record merge (no extra deps)
    dict_pairs(Def, _, DefPairs),
    foldl(merge_kv_(User), DefPairs, Def, Merged).

merge_kv_(User, K-_, Acc0, Acc) :-
    (   get_dict(K, User, V)
    ->  put_dict(K, Acc0, V, Acc)
    ;   Acc = Acc0).

%% Tracer hook: emit one JSON object per event
user:prolog_trace_interception(Port, Frame, _PC, continue) :-
    tl_trace_stream(S), tl_trace_opts(Opts), !,
    (   should_log_port_(Port, Opts)
    ->  event_dict_(Port, Frame, Opts, Dict),
        json_write_dict(S, Dict, [width(0)]), nl(S),
        maybe_stop_(Opts)
    ;   true).
user:prolog_trace_interception(_,_,_,continue).

should_log_port_(Port, Opts) :-
    Ports = Opts.ports,
    ( Ports == all -> true ; memberchk(Port, Ports) ).

maybe_stop_(Opts) :-
    ( integer(Opts.max_events)
    -> nb_increment_counter('__trace_events', N, 1),
       ( N >= Opts.max_events -> notrace ; true )
    ;  true ).

event_dict_(Port, Frame, Opts, Dict) :-
    get_time(TS),                         % wall-clock seconds
    prolog_frame_attribute(Frame, level, Depth),
    ( Opts.include_goal -> safe_goal_string_(Frame, Opts.max_depth, GoalS) ; GoalS = _ ),
    ( Opts.include_pi   -> ( prolog_frame_attribute(Frame, predicate_indicator, PI0), term_string(PI0, PI) -> true ; PI = _ ) ; PI = _ ),
    ( Opts.include_alts -> ( prolog_frame_attribute(Frame, has_alternatives, Alts) -> true ; Alts = _ ) ; Alts = _ ),
    ( Opts.include_source -> source_info_(Frame, File, Line) ; (File=_, Line=_) ),
    TID = _,  % skip thread for simplicity
    Dict0 = _{ port:Port, depth:Depth, ts:TS },
    maybe_put_(goal, GoalS, Dict0, D1),
    maybe_put_(pi, PI, D1, D2),
    maybe_put_(alts, Alts, D2, D3),
    maybe_put_(file, File, D3, D4),
    maybe_put_(line, Line, D4, D5),
    maybe_put_(thread, TID, D5, Dict).

maybe_put_(Key, Value, DictIn, DictOut) :-
    ( var(Value) -> DictOut = DictIn
    ; put_dict(Key, DictIn, Value, DictOut)
    ).

source_info_(Frame, File, Line) :-
    ( prolog_frame_attribute(Frame, clause, ClRef),
      clause_property(ClRef, file(File)),
      clause_property(ClRef, line_count(Line))
    ) -> true ; (File = _, Line = _).

safe_goal_string_(Frame, MaxDepth, String) :-
    prolog_frame_attribute(Frame, goal, Goal0),
    copy_term(Goal0, Goal1),
    numbervars(Goal1, 0, _, [singletons(true)]),
    with_output_to(string(String),
      write_term(Goal1,
        [ quoted(true), portray(true), max_depth(MaxDepth), priority(999)
        ])).