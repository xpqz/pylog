# REPL

The REPL lets you enter goals and see answers interactively.

Start the REPL:

```bash
pylog
# or
python -m prolog.repl
```

Enter a goal and end with a full stop:

```text
?- true.
true.
```

Variables are shown as bindings in answers:

```text
?- X = a.
X = a.
```

To ask for the next answer, enter a new goal. PyLog computes all answers for each query unless you limit solutions in an integration script.

