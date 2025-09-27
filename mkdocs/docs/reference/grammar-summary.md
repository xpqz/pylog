# Grammar summary

Terms:

- atom, integer, variable, structure, list
- lists: `[]` or `[E1,E2|Tail]`

Clauses:

- fact: `head.`
- rule: `head :- goal1, goal2.`

Queries:

- `?- goal.`

Operators follow standard Prolog precedence and associativity used by the reader.
The reader also recognizes CLP(FD) reification operators `#<=>`, `#==>`, `#<==`.
