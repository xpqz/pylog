# Domains and `in/2`

Use `in/2` to set an integer domain for a variable.

Forms:

- interval: `Low..High`
- union: `D1 \/ D2`
- set: `{1,3,5}`

Examples:

```text
?- X in 1..9, Y in {1,3,5}.
true.

?- X in 5..5.
X is fixed to 5.
```

Domains combine by intersection when you post multiple `in/2` goals for the same variable.

