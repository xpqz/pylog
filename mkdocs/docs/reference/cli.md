# CLI Reference

PyLog provides a simple command‑line interface to consult files and run goals without entering the interactive REPL.

Usage
-----

```text
pylog [FILES ...] [OPTIONS]

python -m prolog.repl [FILES ...] [OPTIONS]
```

Options
-------

- `-g GOAL`, `--goal GOAL`
  - Run a query and print results.
  - Write the goal without the `?-` prefix and without the trailing `.`

- `--once`
  - Print only the first solution. Default when `--all` is not given.

- `--all`
  - Print all solutions, one per line.

- `--noninteractive`
  - Do not start the REPL after running the goal.

- `--trace`
  - Enable pretty tracing to stdout during the command‑line run.

Examples
--------

Consult files and start the REPL:

```bash
pylog prolog/examples/hanoi.pl
```

Run a goal and exit after the first solution:

```bash
pylog prolog/examples/hanoi.pl -g "hanoi(3,left,right,middle,Moves)" --once --noninteractive
```

Print all solutions of a pure goal:

```bash
pylog -g "member(X,[a,b,c])" --all --noninteractive
```

Enable tracing while running a goal:

```bash
pylog -g "append([1],[2],X)" --once --trace --noninteractive
```

Notes
-----

- Multiple files can be listed; they are consulted in order.
- Use shell quoting for goals that contain spaces or operator characters.
- The Python module entry point (`python -m prolog.repl`) supports the same options.

See also
--------

- Getting Started → REPL
- Guides → Tracing and Debugging
