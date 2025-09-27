# Arithmetic

Evaluating expressions
----------------------

- Use `is/2` to evaluate arithmetic on the right and unify the result on the left.

```text
?- X is 1 + 2 * 3.
X = 7.

?- Y is -3.
Y = -3.

?- Z is (10 - 3) * 2.
Z = 14.
```

Notes
-----

- `is/2` is not symmetric: `3 is 1+2` succeeds, but `1+2 is 3` is a type error in standard Prolog. Always put the variable/result on the left.
- The right side must be fully evaluable (its variables already bound).
- Unary minus binds tightly; `-3` and `-(3)` are equivalent. Prefer no space: `- 3` may be tokenized as two tokens.

Integer vs floating division
----------------------------

- `/` is floating division; `//` is integer (floor) division; `mod/2` is remainder.

```text
?- A is 7 / 2.
A = 3.5.
?- B is 7 // 2.
B = 3.
?- R is 7 mod 2.
R = 1.
```

Comparing numbers
-----------------

- Arithmetic comparison operators evaluate both sides before comparison:
  - `=:=` equal, `=\=` not equal
  - `<`, `=<`, `>`, `>=`

```text
?- 7 =:= 3 + 4.
true.
?- 7 =\= 2 * 4.
true.
?- 2 < 3.
true.
```

Arithmetic vs unification
-------------------------

- `=:=/2` compares numeric values, while `=/2` unifies structures.

```text
?- 1+2 =:= 3.
true.
?- 1+2 = 3.
false.  % left is a structure, not a number
```

CLP(FD) arithmetic
------------------

- For constraints over integers and domain reasoning, use CLP(FD) operators instead of `is/2`: `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`.
- See CLP(FD) → Linear Constraints for propagation examples.

