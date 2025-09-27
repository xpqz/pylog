# Labelling

Labelling assigns concrete values to variables after propagation has reduced their domains. Use `label/1` on a list of variables.

```text
?- X in 1..3, Y in 1..3, X #< Y, label([X,Y]).
X = 1, Y = 2 ;
X = 1, Y = 3 ;
X = 2, Y = 3.
```

Labelling strategies can affect run time. Start with the default and refine if needed in later versions.

