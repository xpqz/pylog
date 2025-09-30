# REPL commands reference

This page lists the interactive commands recognized by the PyLog REPL and their effects.

Commands are entered at the `?-` prompt without a trailing period, unless noted.

## Session control

- `help` — show inline help
- `quit` | `exit` | `halt` — exit the REPL

## Loading code

- `consult('path/to/file.pl').` — load a Prolog source file (note the trailing period and quoted path)
  - On success, the completer refreshes with new predicates

## Tracing

- `trace on` — enable tracing (pretty format to stdout)
- `trace off` — disable tracing
- `trace pretty FILE` — pretty trace to FILE
- `trace json FILE` — JSON Lines trace to FILE
- `trace sample N` — sample 1 in N events (requires tracing enabled)

Spypoints (predicate filters):

- `spy name/arity` — add spypoint (e.g., `spy append/3`)
- `unspy name/arity` — remove spypoint
- `spys` — list active spypoints
- `untrace` — clear all spypoints (does not disable tracing)

See Guides → Tracing and debugging for sink types and JSONL schema.

## Debug & metrics

- `snapshot` — print engine snapshot (store size, stacks)
- `metrics on` — enable metrics collection
- `metrics off` — disable metrics collection
- `metrics reset` — reset counters
- `metrics` — print current counters (global + per‑predicate)

Notes:
- Metrics add small overhead; disable when not needed.
- Enabling/disabling tracing or metrics restarts the engine with the same program.

## Queries

- Queries can be written with or without the `?-` prefix
- Always end queries with a trailing period (`.`)
- For multiple solutions, the REPL prints the first and waits:
  - Enter `;` to request the next solution
  - Enter `.` to stop

Examples:

```text
?- member(X, [a,b]).
X = a
 ;
X = b
 .

?- consult('prolog/lib/lists.pl').
Loaded: prolog/lib/lists.pl
true.

?- trace on.
Tracing enabled (pretty format to stdout)
true.
```

