# JSON recipes

Practical examples for reading, writing, and converting JSON using PyLog’s built‑ins. Use these snippets in the REPL or embed PyLog in Python.

Parse JSON text (classic)
-------------------------

```
?- atom_json_term('{"name":"test","value":42}', T, []).
T = json([name=test, value=42]).
```

Parse JSON text (dict)
----------------------

```
?- atom_json_term('{"name":"alice","age":30}', D, [mode(dict)]).
D = {name:alice, age:30}.

?- atom_json_term('{"name":"alice","age":30}', D, [json_object(dict)]).
D = {name:alice, age:30}.
```

Serialize term to JSON
----------------------

```
?- T = json([inner=json([value=42])]), atom_json_term(A, T, []).
A = '{"inner": {"value": 42}}'.
```

Round‑trip (dict)
-----------------

```
?- atom_json_term('{"ok":true}', D, [mode(dict)]), atom_json_term(A, D, [mode(dict)]).
D = {ok:true},
A = '{"ok": true}'.
```

Constants (classic)
-------------------

```
?- atom_json_term('{"xs":[null,true,false]}', T, []).
T = json([xs=[@(null), @(true), @(false)]]).
```

Dict updates
------------

```
?- dict_create(D0, none, [count-1]), put_dict([count-2, added-yes], D0, D1), get_dict(count, D1, V).
D0 = {count:1},
D1 = {added:yes, count:2},
V = 2.
```

Notes
-----

- Prefer `atom_json_term/3` in pure Prolog. `json_read/3` and `json_write/3` work with file‑like streams when embedding PyLog in Python.
- JSON objects are unordered; PyLog uses canonical key sorting.
- Special floats (NaN, ±Inf) are rejected.
- Dict mode uses atoms `true/false/null`; strings "true"/"false" map to the same atoms.

