# Dicts

PyLog provides an immutable, sorted dictionary term called `PrologDict` for efficient key lookup and predictable ordering. Dicts integrate naturally with JSON (dict mode) and have dedicated built‑ins for creation and updates.

Overview
- Keys must be atoms or integers (atoms sort before integers).
- Keys are unique; pairs are stored in canonical sorted order for fast binary search.
- Dicts are immutable; updates produce a new dict.

Creating dicts

- `dict_create(-Dict, +Tag, +Data)`
  - Tag must currently be `none` (anonymous dicts).
  - Data is a list of key‑value items. Supported item forms:
    - `Key-Value`     (e.g., `[name-test, value-42]`)
    - `Key:Value`     (e.g., `[name:test, value:42]`)
    - `Key(Value)`    (e.g., `[name(test), value(42)]`)

Examples
```
?- dict_create(D, none, [name-test, value-42]).
D = {name:test, value:42}.

?- dict_create(D, none, [a:1, b:2]).
D = {a:1, b:2}.
```

Accessing values

- `get_dict(+Key, +Dict, -Value)` — retrieves the value for `Key`.

```
?- dict_create(D, none, [name-test, value-42]), get_dict(name, D, V).
D = {name:test, value:42},
V = test.
```

Updating dicts

- `put_dict(+KeyValuePairs, +DictIn, -DictOut)` — adds/updates pairs; last value wins on duplicates.

```
?- dict_create(D0, none, [count-1]), put_dict([count-2, added-yes], D0, D1).
D0 = {count:1},
D1 = {added:yes, count:2}.
```

Sorting and lookup
- Dicts maintain keys in canonical order: atoms by name first, then integers by value.
- Lookups use binary search for performance.

Interoperability with JSON
- JSON dict mode maps JSON objects to `PrologDict` and vice versa.
- Integer keys become strings when converting dict → JSON (e.g., key `1` becomes `"1"`).
- See Reference → JSON for JSON built‑ins and options.

Constraints & errors
- Invalid key types (not atom/int) are rejected.
- Duplicate keys at construction are rejected.
- `put_dict/3` accepts duplicate keys in its input list; the last occurrence wins.

