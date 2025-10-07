# JSON Quickstart

This page shows the fastest way to parse and generate JSON in PyLog using the new built‑ins and dual representations.

Parse JSON text (classic)
-------------------------

```
?- atom_json_term('{"name":"test","value":42}', T, []).
T = json([name=test, value=42]).

?- atom_json_term('[1,2,true,null]', L, []).
L = [1, 2, @(true), @(null)].
```

Parse JSON text (dict mode)
---------------------------

```
?- atom_json_term('{"name":"alice","age":30}', D, [mode(dict)]).
D = {name:alice, age:30}.

% SWI‑compatible alias
?- atom_json_term('{"name":"alice","age":30}', D, [json_object(dict)]).
D = {name:alice, age:30}.
```

Generate JSON from terms
------------------------

```
% Classic → JSON atom
?- T = json([inner=json([value=42])]), atom_json_term(A, T, []).
A = '{"inner": {"value": 42}}'.

% Dict → JSON atom (dict mode)
?- D = {ok:true}, atom_json_term(A, D, [mode(dict)]).
A = '{"ok": true}'.
```

Round‑trips
-----------

```
% Classic
?- atom_json_term(A, json([k=v]), []).
A = '{"k": "v"}'.

% Dict
?- atom_json_term('{"k":true}', D, [mode(dict)]), atom_json_term(A, D, [mode(dict)]).
D = {k:true},
A = '{"k": true}'.
```

Constants and representations
-----------------------------

- Classic uses `@(null)`, `@(true)`, `@(false)`.
- Dict uses atoms `null`, `true`, `false`.
- Objects are unordered; PyLog uses canonical key sorting.

Notes
-----

- Prefer `atom_json_term/3` for pure Prolog workflows.
- For file/stream I/O (embedding PyLog in Python), use `json_read/3`, `json_write/3`, `json_read_dict/3`, `json_write_dict/3` with file‑like streams. See Reference → JSON for details.

See also
--------

- Reference → JSON: mapping, options (`mode/1`, `json_object/1`), error handling
- Basics → Dicts: `dict_create/3`, `get_dict/3`, `put_dict/3`

