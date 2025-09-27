# Trace format (JSONL)

PyLog emits compact JSON Lines (JSONL) traces for efficient capture and analysis. This page summarizes the schema used by the tracer (schema version 1).

Required keys
-------------

- `v` (int): Schema version (1)
- `rid` (string): Run ID (UUID for this query execution)
- `sid` (int): Step ID (monotonic, assigned after filtering)
- `p` (int): Port — 0=call, 1=exit, 2=redo, 3=fail
- `pid` (string): Predicate ID in `name/arity` form
- `g` (string): Pretty‑printed goal
- `fd` (int): Frame depth (engine frame stack)
- `cd` (int): Choicepoint depth (engine CP stack)
- `gh` (int): Goal height (goal stack)
- `ws` (int): Write stamp (engine write counter)

Optional keys
-------------

- `ts` (int): Monotonic timestamp (nanoseconds)
- `b` (object): Variable bindings (when enabled)

Internal events
---------------

When internal events are enabled, the tracer may emit records with `type: "internal"`:

- `v` (int): 1
- `type` (string): `internal`
- `sid` (int): Step ID
- `kind` (string): Event kind (e.g., `cp_push`, `frame_pop`)
- `details` (object): Event details

Example
-------

```
{"v":1,"rid":"…","sid":1,"p":0,"pid":"append/3","g":"append([1,2],[3],X)","fd":0,"cd":0,"gh":1,"ws":0}
{"v":1,"rid":"…","sid":2,"p":1,"pid":"append/3","g":"append([], [3], [3])","fd":2,"cd":2,"gh":2,"ws":15}
```

Notes
-----

- Keys are minimized for size and streaming friendliness
- Deterministic output when timestamps are disabled
- See `prolog/debug/sinks.py` for the encoder and `prolog/debug/invariant_checker.py` for invariants

