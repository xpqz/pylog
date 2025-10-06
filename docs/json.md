# JSON Support in PyLog

PyLog provides comprehensive JSON support through dual representation modes, following SWI-Prolog's approach. This enables PyLog programs to read, write, and manipulate JSON data using both classical term representation and modern dict-based representation.

## Representation Modes

### Classic Mode (`CLASSIC_MODE`)

In classic mode, JSON objects are represented using traditional Prolog terms:

- **JSON objects** → `json([key-value, ...])` structures
- **Key-value pairs** → `key-value` terms using the `-` functor
- **Constants** → `@(constant)` structures:
  - `null` → `@(null)`
  - `true` → `@(true)`
  - `false` → `@(false)`

```python
from prolog.engine.json_convert import json_to_prolog, CLASSIC_MODE

# JSON object {"name": "test", "value": 42}
term = json_to_prolog({"name": "test", "value": 42}, CLASSIC_MODE)
# Result: json([name-test, value-42])
```

### Dict Mode (`DICT_MODE`)

In dict mode, JSON objects map directly to PyLog's PrologDict terms:

- **JSON objects** → `PrologDict` terms
- **Constants** → simple atoms:
  - `null` → `null`
  - `true` → `true`
  - `false` → `false`

```python
from prolog.engine.json_convert import json_to_prolog, DICT_MODE

# JSON object {"name": "test", "value": 42}
term = json_to_prolog({"name": "test", "value": 42}, DICT_MODE)
# Result: {name: test, value: 42} (PrologDict)
```

## Type Mapping

| JSON Type | Classic Mode | Dict Mode |
|-----------|--------------|-----------|
| `null` | `@(null)` | `null` |
| `true` | `@(true)` | `true` |
| `false` | `@(false)` | `false` |
| Numbers | `Int(42)`, `Float(3.14)` | `Int(42)`, `Float(3.14)` |
| Strings | `Atom("hello")` | `Atom("hello")` |
| Arrays | `List([...])` | `List([...])` |
| Objects | `json([key-value, ...])` | `PrologDict(...)` |

## API Functions

### Core Conversion Functions

```python
from prolog.engine.json_convert import json_to_prolog, prolog_to_json, CLASSIC_MODE, DICT_MODE

# JSON to Prolog
json_to_prolog(json_obj, mode=CLASSIC_MODE, constants=None)

# Prolog to JSON
prolog_to_json(term, mode=CLASSIC_MODE, constants=None)
```

### Custom Constants

You can customize how JSON constants are represented:

```python
custom_constants = {
    "null": Atom("nil"),
    "true": Int(1),
    "false": Int(0)
}

term = json_to_prolog(None, CLASSIC_MODE, custom_constants)
# Result: nil (instead of @(null))
```

## Predicates (Phase 2 - Coming Soon)

PyLog will provide SWI-Prolog compatible JSON predicates:

- `json_read(+Stream, -Term, +Options)` - Read JSON from stream (classic mode)
- `json_write(+Stream, +Term, +Options)` - Write JSON to stream (classic mode)
- `json_read_dict(+Stream, -Dict, +Options)` - Read JSON as dict (dict mode)
- `json_write_dict(+Stream, +Dict, +Options)` - Write dict as JSON (dict mode)
- `atom_json_term(?Atom, ?Term, +Options)` - Convert between atom and term

## Important Notes

### String vs Boolean Ambiguity in Dict Mode

In dict mode, there's an inherent ambiguity between JSON strings `"true"`/`"false"` and JSON booleans `true`/`false`:

```python
# Both result in the same Prolog term
json_to_prolog(True, DICT_MODE)    # → Atom("true")
json_to_prolog("true", DICT_MODE)  # → Atom("true")

# Converting back always yields boolean
prolog_to_json(Atom("true"), DICT_MODE)  # → True (boolean, not string)
```

This is acceptable given PyLog's unified treatment of strings and atoms in the AST.

### Integer Keys in PrologDict

PrologDict allows integer keys, which are converted to string keys in JSON:

```python
dict_term = PrologDict(((Int(1), Atom("value")),))
json_obj = prolog_to_json(dict_term, DICT_MODE)
# Result: {"1": "value"}  # Int key became string
```

This is asymmetric with JSON→Prolog conversion, which rejects non-string keys per JSON specification.

### Canonical Representation

- **Object keys** are always sorted for canonical representation
- **Proper lists only** - lists with non-empty tails cannot be converted to JSON
- **Special float values** (NaN, ±Infinity) are rejected in both directions
- **Unicode strings** are fully supported for both keys and values

## Round-trip Integrity

Both modes preserve round-trip conversion integrity for all supported JSON values:

```python
# Classic mode round-trip
original = {"name": "test", "arrays": [1, 2, 3], "active": True}
term = json_to_prolog(original, CLASSIC_MODE)
converted_back = prolog_to_json(term, CLASSIC_MODE)
assert converted_back == original

# Dict mode round-trip
term = json_to_prolog(original, DICT_MODE)
converted_back = prolog_to_json(term, DICT_MODE)
assert converted_back == original
```

## Error Handling

The conversion functions provide comprehensive error handling:

- **Invalid mode** → `ValueError`
- **Non-string JSON object keys** → `ValueError`
- **Special float values** → `ValueError`
- **Improper lists** → `ValueError`
- **Invalid term types** → `ValueError`
- **Duplicate keys in classic structures** → `ValueError`

## Performance Considerations

- Leverages Python's optimized `json` module for parsing/generation
- Reuses existing PrologDict implementation (already optimized)
- Avoids unnecessary term copying during conversion
- Minimal overhead when using appropriate mode for your use case