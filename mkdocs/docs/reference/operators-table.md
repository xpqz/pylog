# Operators table (overview)

Common operators and categories:

- Term: `=`, `\=`
- Arithmetic compare: `=:=`, `=\=`, `<`, `=<`, `>`, `>=`
- Arithmetic eval: `is`
- Arithmetic: `+`, `-`, `*`, `//`, `mod`
- Control: `,`, `;`, `->`

CLP(FD) operators:

- Linear relations: `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`
- Reification: `#<=>`, `#==>`, `#<==`

For precedence and associativity, see the implementation notes and tests under operators. Reification operators are non‑associative (xfx) at precedence 900.
