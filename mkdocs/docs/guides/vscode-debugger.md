# VS Code Debugger

Debug Prolog programs in VS Code using PyLog’s built‑in Debug Adapter Protocol (DAP) server and the companion VS Code extension. This guide covers installation, configuration, and day‑to‑day usage.

See also:
- Reference → [CLI](../reference/cli.md) for running goals from the terminal
- Guides → [Tracing and debugging](./tracing-and-debugging.md) for PyLog’s tracer

## Features

- Step through execution at Prolog ports: CALL, EXIT, REDO, FAIL
- Predicate breakpoints by functor/arity (e.g. `append/3`)
- Stepping controls: Continue, Step Over, Step In, Step Out
- Variables view for query variable bindings and current goal
- Optional port filtering and execution options (occurs check, indexing)

## Requirements

- VS Code 1.75.0+
- Python 3.11+
- PyLog installed in your environment (`uv sync` or `pip install -e '.[dev]'`)

## Install the VS Code Extension

If you have a packaged VSIX:

```bash
code --install-extension pylog-vscode-0.1.0.vsix
```

To build the extension from source (from the repo root):

```bash
cd vscode-pylog
npm install
npm run compile
npm run package
# then install the generated pylog-vscode-*.vsix
```

## Create a Launch Configuration

Add `.vscode/launch.json` to your workspace:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "pylog",
      "request": "launch",
      "name": "Debug PyLog",
      "program": "${workspaceFolder}/main.pl",
      "stopOnEntry": true
    }
  ]
}
```

Common options:

- `program` (string, required): Path to your `.pl` file
- `query` (string, default `"?- main."`): Query to execute when starting
- `stopOnEntry` (bool, default `true`): Pause at the first port
- `ports` (string[], default `["CALL","EXIT","FAIL"]`): Eligible port events for pausing/stepping
- `predicateBreakpoints` (array, default `[]`): Breakpoints by functor/arity
- `occursCheck` (bool, default `false`): Enable occurs check
- `useIndexing` (bool, default `true`): Enable first‑argument indexing

Example with predicate breakpoints and ports filter:

```json
{
  "type": "pylog",
  "request": "launch",
  "name": "Debug with Breakpoints",
  "program": "${workspaceFolder}/main.pl",
  "query": "?- solve(Result).",
  "ports": ["CALL", "EXIT", "FAIL"],
  "predicateBreakpoints": [
    { "functor": "member", "arity": 2, "ports": ["CALL"] },
    { "functor": "append", "arity": 3 }
  ]
}
```

## Using the Debugger

1. Open a `.pl` file in VS Code
2. Select your `PyLog` configuration and press F5
3. Use the Debug toolbar:
   - Continue (F5): resume execution
   - Step Over (F10): next goal at same depth
   - Step In (F11): enter called predicate
   - Step Out (Shift+F11): return to caller
4. Use the Variables pane to inspect query variables and the current goal

### Predicate Breakpoints

Break on predicate calls by functor/arity using `predicateBreakpoints` in `launch.json`. Optionally restrict by port:

```json
{ "functor": "member", "arity": 2, "ports": ["CALL"] }
```

### Tips

- Keep queries small while iterating; prefer a single entry predicate that you run from `query`
- Use `ports` to reduce noise if you only care about CALL/FAIL, for example
- Combine tracing (Guides → Tracing and debugging) when you want a full event log in addition to stepping

## Troubleshooting

- Extension not activating
  - Ensure VS Code 1.75+ and that the PyLog extension is installed/enabled
  - Check View → Output → “PyLog Debugger” for logs
- Server fails to start
  - Verify Python is available and PyLog is installed (`uv sync`)
  - Ensure the `program` path is correct
- Breakpoints not hit
  - Confirm functor and arity match exactly (e.g. `append/3`)
  - If using port filters, include the port where you expect to stop

## Related Docs

- VS Code extension: `vscode-pylog/README.md` (in this repo)
- DAP architecture and deeper details: `docs/dap.md` (in this repo)

