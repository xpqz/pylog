# JSON Support

PyLog provides SWI‑style JSON support with two representation modes and a set of built‑ins for reading, writing, and converting JSON. This page explains the representations, options, predicates, and edge cases.

## Representations

- Classic mode (term): JSON objects are represented as `json([key=value, ...])`.
  - Example: `{"name":"test","value":42}` → `json([name=test, value=42])`
  - JSON constants use `@(null)`, `@(true)`, `@(false)`.
- Dict mode (modern): JSON objects are represented as `PrologDict` terms (internal sorted dicts).
  - Example: `{"name":"test","value":42}` → a dict with pairs `[(name=test),(value=42)]`.
  - JSON constants are plain atoms: `null`, `true`, `false`.

Notes
- JSON objects are unordered. PyLog uses canonical key sorting internally for deterministic behavior and fast lookup.
- Lists map to Prolog lists. Only proper lists (empty tail) are convertible to JSON.

## Built‑ins

- `json_read(+Stream, -Term, +Options)`
  - Reads JSON from a text stream into a Prolog term.
  - Default mode: classic. Use `mode(dict)` to read as a dict.
- `json_write(+Stream, +Term, +Options)`
  - Writes a Prolog term as JSON to a text stream.
  - Default mode: classic. Use `mode(dict)` to interpret dict terms.
- `json_read_dict(+Stream, -Dict, +Options)`
  - Reads JSON as a dict (dict mode). Equivalent to `json_read/3` with `mode(dict)`.
- `json_write_dict(+Stream, +Dict, +Options)`
  - Writes a dict as JSON (dict mode). Equivalent to `json_write/3` with `mode(dict)`.
- `atom_json_term(?Atom, ?Term, +Options)`
  - Bidirectional conversion between an atom containing JSON text and a Prolog term.
  - Default mode: classic. Use `mode(dict)` for dicts.

Options
- `mode(classic)` | `mode(dict)` — selects representation.
- `json_object(term)` | `json_object(dict)` — SWI‑Prolog compatible aliases for classic/dict.
- Constants mapping is fixed by mode; custom mappings are not exposed via options yet.

Examples

- Classic parsing and matching
```
?- atom_json_term('{"name":"test","value":42}', T, []).
T = json([name=test, value=42]).
```

- Dict parsing
```
?- atom_json_term('{"name":"alice","age":30}', D, [mode(dict)]).
D = {name:alice, age:30}.   % internal dict; printed via pretty printer
```

- Dict parsing (SWI alias)
```
?- atom_json_term('{"name":"alice","age":30}', D, [json_object(dict)]).
D = {name:alice, age:30}.
```

- Classic constants
```
?- atom_json_term('{"xs":[null,true,false]}', T, []).
T = json([xs=[@(null), @(true), @(false)]]).
```

- Writing back to JSON
```
?- T = json([inner=json([value=42])]), atom_json_term(A, T, []).
A = '{"inner": {"value": 42}}'.
```

- Dict round‑trip
```
?- atom_json_term('{"ok":true}', D, [mode(dict)]), atom_json_term(A, D, [mode(dict)]).
D = {ok:true},
A = '{"ok": true}'.
```

- Classic via SWI alias
```
?- atom_json_term(A, json([k=v]), [json_object(term)]).
A = '{"k": "v"}'.
```

## Type mapping

- Numbers → `Int(...)` or `Float(...)`
- Strings → atoms
- Arrays → lists
- Objects → classic `json([key=value,...])` or dict
- Constants:
  - Classic: `@(null)`, `@(true)`, `@(false)`
  - Dict: `null`, `true`, `false`

Ambiguity (dict mode)
- In dict mode, the atom `true` is used for both the JSON boolean `true` and the JSON string "true" after conversion. Converting such an atom back yields the JSON boolean (`true`). This mirrors the current AST and is acceptable for most workloads.

## Error handling

The following cases fail or raise errors during conversion:
- Non‑string JSON object keys on input
- Lists with non‑empty tails when converting to JSON
- Special float values (NaN, ±Inf) in either direction
- Malformed classic structures (e.g., `json/1` with wrong shapes)
- Duplicate keys in classic `json([key=value,...])` when converting to JSON

## SWI compatibility

- Classic mode matches SWI’s `json([key=value,...])` representation and constants `@(null)`, `@(true)`, `@(false)`.
- Dict mode aligns with SWI’s dict‑based workflow.
- Compatibility is covered by a baseline test suite that cross‑checks behavior against SWI.

## See also

- Basics → Dicts: PrologDict, core dict operations, and usage.
