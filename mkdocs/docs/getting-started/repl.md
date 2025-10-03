# REPL

PyLog ships with an interactive REPL powered by `prompt_toolkit` (rich line
editing, history, completion, and syntax highlighting) and integrated tracing.

## Quick links

- Tracing commands: [Using the tracer from the REPL](../guides/tracing-and-debugging.md#using-the-tracer-from-the-repl)
- Tips: [REPL tips & shortcuts](../guides/repl-tips.md#tracing-from-the-repl)

## Starting

```bash
pylog
# or
python -m prolog.repl
```

You'll see a `?-` prompt. Enter a goal and end with a period `.`

```text
?- true.
true.
```

Variables are shown as bindings in answers:

```text
?- X = a.
X = a.
```

Multi‑solution queries use `;` and `.` just like Prolog:

```text
?- member(X, [a,b]).
X = a
 ;
X = b
 .
```

Type `;` for the next solution, `.` to stop.

## Command‑line usage

You can consult files and run a goal directly from the shell without entering the REPL.

Basics:

```bash
# Consult one or more files, then start the REPL
pylog prolog/examples/hanoi.pl

# Run a goal and exit after the first solution
pylog prolog/examples/hanoi.pl -g "hanoi(3,left,right,middle,Moves)" --once --noninteractive

# Run a goal and print all solutions
pylog -g "member(X,[a,b])" --all --noninteractive

# Enable pretty tracing while running
pylog -g "append([1],[2],X)" --once --trace --noninteractive
```

Notes:

- `-g/--goal` takes a query without the `?-` prefix and without the trailing `.`
- `--once` prints the first solution (default if `--all` is not given)
- `--all` prints all solutions separated by newlines
- `--noninteractive` prevents the REPL from starting after running the goal
- `--trace` enables pretty tracing to stdout for the command‑line run

## Editing, history, and completion

The REPL uses `prompt_toolkit` for a pleasant terminal experience:

- History persisted to `~/.pylog_history`
  - Up/Down to navigate; Ctrl‑R for reverse‑i‑search
- Syntax highlighting via Pygments (Prolog lexer)
- Auto‑suggest from history (ghost text)
- Tab completion for predicate names
  - Includes common built‑ins
  - Updates when you `consult('file.pl')`
- Single‑line input; unfinished queries (missing final `.`) are detected and kept as "incomplete" until you complete them

Tips:
- Press Tab to complete a partially typed predicate
- Use Ctrl‑C to cancel the current input; Ctrl‑D (EOF) to exit

## Loading code

Use `consult('path/to/file.pl').`

```text
?- consult('prolog/lib/lists.pl').
Loaded: prolog/lib/lists.pl
true.
```

On load, the completer is refreshed with newly defined predicate names.

## Listing loaded clauses

Use `listing.` to print all currently loaded user clauses, or `listing(name/N).` to restrict to a predicate indicator.

```text
?- consult('prolog/examples/hanoi.pl').
Loaded: prolog/examples/hanoi.pl
true.

?- listing(hanoi/5).
hanoi(0, _, _, _, ...).
hanoi(N, From, To, Aux, Moves) :- ...
```

## Tracing from the REPL

You can toggle tracing and choose sinks without leaving the REPL.

Commands:

- `trace on` — enable tracing to stdout in pretty format
- `trace off` — disable tracing
- `trace json FILE` — write JSONL trace to FILE
- `trace pretty FILE` — write human‑readable trace to FILE
- `trace sample N` — enable 1‑in‑N event sampling (with trace on)

Examples:

```text
?- trace on.
Tracing enabled (pretty format to stdout)
true.

?- spy member/2.
Spypoint added: member/2
true.

?- member(X, [a,b]).
CALL member/2 ...
EXIT member/2 ...
X = a
 ;
EXIT member/2 ...
X = b
 .

?- trace pretty out.log.
Tracing enabled (pretty format to out.log)
true.
```

### Spypoints

Spypoints control which predicates generate trace events:

- `spy Name/Arity` — add a spypoint
- `unspy Name/Arity` — remove a spypoint
- `spys` — list spypoints
- `untrace` — clear all spypoints

See Guides → Tracing and Debugging for the full trace event model and formats.

## Debug commands

Built‑in helpers for quick inspection:

- `snapshot` — print a compact engine state snapshot (store size, stacks)
- `metrics on|off` — enable/disable metrics collection
- `metrics reset` — reset counters
- `metrics` — print current counters (global and per‑predicate)

Note: metrics add small overhead; leave off unless needed.

## Query basics

- Prefix `?-` is optional; both `?- goal.` and `goal.` work
- Always terminate queries with a final period `.`
- For multiple answers, use `;` (next) and `.` (stop)

Common examples:

```text
?- append([1,2], [3], X).
X = [1,2,3]
.

?- between(1, 3, X).
X = 1
 ;
X = 2
 ;
X = 3
 .
```

## Error messages

- Parse errors show a short message; if you accidentally use infix operators before Stage 1.5 is enabled, the REPL includes hints on operator‑free syntax.
- File loading errors explain encoding issues and file not found.

## Exiting

- `quit`, `exit`, or `halt` — or press Ctrl‑D (EOF)

## See also

- Getting Started → Install / Hello Prolog
- Guides → Tracing and Debugging
- Basics → Builtins / Operators
- Reference → Trace format
