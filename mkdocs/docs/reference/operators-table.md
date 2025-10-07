# Operators table (overview)

Common operators and categories:

- Term: `=`, `\=`
- Term order: `@<`, `@=<`, `@>`, `@>=`
- Arithmetic compare: `=:=`, `=\=`, `<`, `=<`, `>`, `>=`
- Arithmetic eval: `is`
- Arithmetic: `+`, `-`, `*`, `//`, `mod`
- Control: `,`, `;`, `->`
- Prefix (unary): `-`, `+`, `\+`, `@` (JSON constants)

CLP(FD) operators:

- Linear relations: `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`
- Reification: `#<=>`, `#==>`, `#<==`

For precedence and associativity, see the implementation notes and tests under operators. Reification operators are non‑associative (xfx) at precedence 900. The prefix `@/1` has precedence 300 and is used for classic JSON constants, e.g., `@(null)`.

