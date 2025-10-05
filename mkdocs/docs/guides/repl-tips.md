# REPL tips & shortcuts

This page collects practical tips for using the PyLog REPL efficiently.

See also Getting Started → [REPL](../getting-started/repl.md) for a walkthrough, and Guides → [Tracing and Debugging](./tracing-and-debugging.md) for the tracer.

## Line editing (prompt_toolkit)

- History is saved in `~/.pylog_history`
  - Up/Down to navigate
  - Ctrl‑R for reverse‑i‑search
- Auto‑suggest from history (ghost text); press Right arrow to accept
- Syntax highlighting (Prolog lexer)
- Tab completion for predicate names
  - Updated automatically after `consult('file.pl')`
- Single‑line prompt; incomplete inputs without a trailing `.` are kept until you complete them

Common key bindings:
- Ctrl‑A / Ctrl‑E — beginning/end of line
- Alt‑F / Alt‑B — move by word
- Ctrl‑W — delete previous word
- Ctrl‑K — delete to end of line
- Ctrl‑U — delete to beginning of line
- Ctrl‑C — cancel current input
- Ctrl‑D — exit

## Multiple solutions

Use `;` for the next solution and `.` to stop:

```text
?- between(1,3,X).
X = 1
 ;
X = 2
 ;
X = 3
 .
```

The REPL prints `.` after the last solution to indicate completion.

## Help & guidance

- `help.` shows a quick-reference table.
- `help TOPIC` or `help(TOPIC)` gives focused notes. Topics: `repl`, `db`, `trace`, `files`, `operators`.
- Inline reminders appear when you enter `consult(user).` or other modes.

## Loading files and interactive input

Use `consult('path/to/file.pl').` (quoted string) to load a file, or `consult(user).` to type clauses inline:

```text
?- consult('prolog/lib/lists.pl').
Loaded: prolog/lib/lists.pl
true.
```

Interactive session:

```text
?- consult(user).
|: edge(a, b).
|: edge(b, c).
|: path(X, Z) :- edge(X, Z).
|: path(X, Z) :- edge(X, Y), path(Y, Z).
|: .
true.
```

On success the completer is refreshed with any new predicates.

## Tracing from the REPL

Enable/disable and choose sinks:

- `trace on` — pretty to stdout
- `trace off`
- `trace pretty FILE` — pretty to file
- `trace json FILE` — JSON Lines to file
- `trace sample N` — sample 1 in N events

Spypoints:

- `spy name/arity` — add a spypoint
- `unspy name/arity` — remove a spypoint
- `spys` — list active spypoints
- `untrace` — clear spypoints

Tip: add spypoints before `trace on` so the filter is applied immediately.

## Debug helpers

- `snapshot` — prints compact engine state (store size, stacks)
- `metrics on|off` — enable/disable counters; `metrics` to show
- `metrics reset` — clear counters

Metrics add small overhead; leave off unless needed.

## Query basics & errors

- `?-` prefix is optional; always end with `.`
- Parse errors show a short message; the REPL hints at operator‑free syntax if needed
- File decode errors (UTF‑8) and missing files include actionable messages

## Exiting

- `quit`, `exit`, or `halt`, or press Ctrl‑D
