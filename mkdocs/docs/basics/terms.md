# Terms

Terms are the data values of Prolog. PyLog supports the standard term types:

- Atoms: `a`, `foo`, `[]`, `'Hello'`, `'@<'`
- Integers: `0`, `42`, `-3`
- Variables: `X`, `Y`, `_`, `_Tmp`, `_G123`
- Compound terms: `point(1,2)`, `edge(a,b)`, `tree(Left,Key,Right)`
- Lists (syntactic sugar): `[]`, `[a,b,c]`, `[H|T]`, `[a,b|T]`

Atoms
-----

- Unquoted atoms start with a lowercase letter and may contain letters, digits, and `_`.
- Use single quotes for atoms with spaces or special characters: `'hello world'`, `'->'`.
- `[]` is the empty list; it’s also an atom. The list sugar desugars to the functor `'.'/2` internally.

Variables
---------

- Variables start with an uppercase letter or underscore: `X`, `Value`, `_Tail`.
- `_` is the anonymous variable; each occurrence is distinct and never binds across occurrences.
- Variable scope is a single clause (fact or rule). Repeated occurrences of the same variable name within a clause must unify to the same value.

Compound terms
--------------

- A compound term has a functor name and a fixed arity: `functor/arity`.
- Examples: `point(1,2)` has functor `point` and arity 2; `tree(L,K,R)` has functor `tree` and arity 3.
- Use `functor/3` and `arg/3` to inspect, or `=../2` (univ) to convert between a term and list form.

Lists
-----

- `[a,b,c]` is shorthand for `.(a,.(b,.(c,[])))`.
- `[H|T]` matches a nonempty list with head `H` and tail `T`.
- Common patterns:

```text
?- [H|T] = [a,b,c].
H = a, T = [b, c].

?- [X,Y|Rest] = [1,2,3,4].
X = 1, Y = 2, Rest = [3, 4].
```

Quoted atoms and operators
--------------------------

- Operators like `,`, `;`, `->`, `=..` are atoms in canonical form when quoted, e.g. `','`, `';'`, `'->'`.
- The reader uses precedence and associativity to parse operator expressions into the underlying term structures. See Basics → Operators.

Examples
--------

Unification builds and deconstructs terms:

```text
?- X = point(1, 2).
X = point(1, 2).

?- point(X, 2) = point(1, Y).
X = 1, Y = 2.

?- edge(a,b) =.. L.
L = [edge, a, b].

?- T =.. [edge, a, b].
T = edge(a, b).
```
