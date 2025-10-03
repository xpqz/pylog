# Built‑ins reference (summary)

Selected built‑ins provided by PyLog.

Core predicates:
- `true/0`, `fail/0`, `!/0`
- `var/1`, `nonvar/1`, `atom/1`
- `=/2`, `\=/2`, `=../2`, `functor/3`, `arg/3`
- `is/2`, `=:=/2`, `=\=/2`, `</2`, `=</2`, `>/2`, `>=/2`, `// /2`, `mod/2`
- `call/1`, `once/1`
- `throw/1`, `catch/3`

Attributed variables:
- `put_attr/3`, `get_attr/3`, `del_attr/2`

CLP(FD):
- `in/2` — post a finite domain
- `#=/2`, `#\=/2`, `#</2`, `#=</2`, `#>/2`, `#>=/2` — linear constraints on integers
- `all_different/1` — global constraint for pairwise distinctness
- `label/1` — assign values to variables
- `labeling/2` — labeling with options (limited support)
- `fd_var/1` — check if variable has a finite domain
- `fd_inf/2`, `fd_sup/2` — get domain bounds
- `fd_dom/2` — get domain as term

See the Basics and CLP(FD) sections for usage examples.

Reification
-----------

- `#<=>/2`, `#==>/2`, `#<==/2` — relate a Boolean to a constraint's truth. See CLP(FD) → Reification for semantics and examples.
