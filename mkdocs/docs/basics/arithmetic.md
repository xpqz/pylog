# Arithmetic

`is/2` evaluates arithmetic expressions and unifies with the result.

```text
?- X is 1 + 2 * 3.
X = 7.

?- Y is - (4).
Y = -4.
```

Use comparison operators to compare numbers: `=:=`, `=\=`, `<`, `=<`, `>`, `>=`.

```text
?- 7 =:= 3 + 4.
true.
?- 7 =\= 2 * 4.
true.
```

