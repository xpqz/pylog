# ISO compatibility

PyLog follows a useful subset of ISO Prolog. Differences exist in areas such as modules, I/O, and full numeric tower support. CLP(FD) adds non‑ISO operators with `#` prefixes.

If you rely on specific ISO features not covered here, check tests or open an issue with a small example.

JSON compatibility (SWI)
------------------------

- PyLog implements SWI‑style JSON dual representation:
  - Classic (term): `json([key=value,...])` with constants `@(null)`, `@(true)`, `@(false)`
  - Dict (modern): `PrologDict`
- Option aliases supported alongside PyLog’s `mode/1`:
  - `json_object(term)` ↔ classic (same as `mode(classic)`)
  - `json_object(dict)` ↔ dict (same as `mode(dict)`)
- Predicates: `json_read/3`, `json_write/3`, `json_read_dict/3`, `json_write_dict/3`, `atom_json_term/3`
- See Reference → JSON for mapping details and examples.
