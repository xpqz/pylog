# Embedding JSON I/O from Python

This guide shows how to use PyLog’s JSON features from Python code, both via normal Prolog queries and via low‑level stream helpers.

Recommended: convert via queries
--------------------------------

Use the standard predicate `atom_json_term/3` through the query API. This is the most portable and future‑proof path.

```python
from prolog.engine.engine import Engine
from prolog.parser.reader import Reader
from prolog.tests.helpers import program  # or construct your own Program

engine = Engine(program())
reader = Reader()

# Atom (JSON) -> Term (classic)
q = reader.read_term("atom_json_term('{\"name\":\"alice\"}', T, [])")
solutions = list(engine.solve(q))
assert solutions and str(solutions[0]["T"]) == "json([name=alice])"

# Term -> Atom (classic)
q2 = reader.read_term("atom_json_term(A, json([k=v]), [])")
solutions2 = list(engine.solve(q2))
assert solutions2 and solutions2[0]["A"].name == '{"k": "v"}'

# Dict mode with PyLog option
q3 = reader.read_term("atom_json_term('{\"ok\":true}', D, [mode(dict)])")
solutions3 = list(engine.solve(q3))
assert solutions3 and str(solutions3[0]["D"]) == "{ok:true}"

# Dict mode with SWI alias
q4 = reader.read_term("atom_json_term('{\"ok\":true}', D, [json_object(dict)])")
solutions4 = list(engine.solve(q4))
assert solutions4 and str(solutions4[0]["D"]) == "{ok:true}"
```

Low‑level: stream helpers (Python file‑like objects)
---------------------------------------------------

For embedding scenarios that work directly with Python text streams, you can call the engine’s internal JSON helpers. They return `True/False` and unify into/out of Prolog terms and variables.

```python
from io import StringIO
from prolog.engine.engine import Engine
from prolog.tests.helpers import program
from prolog.ast.terms import Var, List, Atom, Int, Struct, PrologDict

engine = Engine(program())

# Read JSON from a stream (classic)
stream = StringIO('{"name":"test","value":42}')
term_var = Var(engine.store.new_var("Term"), "Term")
options = List(())  # []
ok = engine._builtin_json_read((stream, term_var, options))
assert ok is True
status, _, term_val = engine.store.deref(term_var.id)
assert status == "BOUND" and str(term_val) == "json([name=test, value=42])"

# Write a classic json/1 term to a stream
pairs = List((
    Struct("=", (Atom("name"), Atom("test"))),
    Struct("=", (Atom("value"), Int(42))),
))
json_term = Struct("json", (pairs,))
out = StringIO()
ok = engine._builtin_json_write((out, json_term, options))
assert ok is True and out.getvalue() == '{"name": "test", "value": 42}'

# Read JSON as dict (dict mode)
stream = StringIO('{"name":"alice","age":30}')
dict_var = Var(engine.store.new_var("Dict"), "Dict")
ok = engine._builtin_json_read_dict((stream, dict_var, options))
assert ok is True
status, _, dict_val = engine.store.deref(dict_var.id)
assert status == "BOUND" and isinstance(dict_val, PrologDict)

# Write dict to JSON
d = PrologDict(((Atom("name"), Atom("alice")), (Atom("age"), Int(30))))
out = StringIO()
ok = engine._builtin_json_write_dict((out, d, options))
assert ok is True and out.getvalue() == '{"name": "alice", "age": 30}'

# Using options to force dict mode (same effect as *_dict builtins)
dict_mode = List((Struct("mode", (Atom("dict"),)),))
out = StringIO()
ok = engine._builtin_json_write((out, d, dict_mode))
assert ok is True and out.getvalue() == '{"name": "alice", "age": 30}'

# SWI alias option also works: json_object(dict|term)
swi_dict = List((Struct("json_object", (Atom("dict"),)),))
out = StringIO()
ok = engine._builtin_json_write((out, d, swi_dict))
assert ok is True
```

File I/O example
----------------

```python
import json, os, tempfile
from prolog.ast.terms import Var, List

engine = Engine(program())
data = {"greeting": "hello", "n": 3}

with tempfile.NamedTemporaryFile(mode="w", delete=False, suffix=".json", encoding="utf-8") as f:
    json.dump(data, f)
    path = f.name

try:
    # Read from file
    with open(path, "r", encoding="utf-8") as f:
        term_var = Var(engine.store.new_var("Term"), "Term")
        ok = engine._builtin_json_read((f, term_var, List(())))
        assert ok is True
finally:
    os.unlink(path)
```

Error handling
--------------

- Malformed JSON or unsupported values cause helpers to return `False`.
- Special floats are rejected (JSON spec) — e.g., writing `[NaN]` fails.

```python
from prolog.ast.terms import List, Int, Float

# Malformed input
ok = engine._builtin_json_read((StringIO('{"incomplete": '), Var(engine.store.new_var("T"), "T"), List(())))
assert ok is False

# Special float on write
arr = List((Int(1), Float(float("nan")), Int(3)))
ok = engine._builtin_json_write((StringIO(), arr, List(())))
assert ok is False
```

Notes
-----

- `atom_json_term/3` supports both `mode(dict|classic)` and `json_object(dict|term)`.
- JSON objects are unordered; PyLog uses canonical key sorting.
- Dict mode uses atoms `true/false/null`. JSON strings "true"/"false" map to the same atoms and serialize as booleans.

See also
--------

- Getting Started → JSON Quickstart
- Reference → JSON (modes, options, mapping, errors)
- Basics → Dicts (dict_create/3, get_dict/3, put_dict/3)

