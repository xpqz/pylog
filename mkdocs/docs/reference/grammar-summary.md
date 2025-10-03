# Grammar summary

Terms
-----

- atom, integer, variable, structure, list
- lists: `[]` or `[E1,E2|Tail]`
- parentheses for grouping goals: `(A, B, ...)`

Clauses
-------

- fact: `head.`
- rule: `head :- goal1, goal2.`

Queries
-------

- `?- goal.`

Notes
-----

- Parentheses group goals in clause bodies, queries, and directives. For example: `p :- (a, b).` This is parsed directly and behaves like `p :- a, b.`
- Operators follow standard Prolog precedence and associativity via the reader.
- The reader recognizes CLP(FD) reification operators `#<=>`, `#==>`, `#<==`.
