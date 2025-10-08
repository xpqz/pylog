# PyLog Debugger for VS Code

Debug Adapter for the PyLog Prolog interpreter.

## Features

- **Step through Prolog execution**: Step in, step over, and step out at Prolog ports (CALL, EXIT, FAIL, REDO)
- **Predicate breakpoints**: Set breakpoints by functor and arity
- **Call stack inspection**: View the Prolog goal stack
- **Variable inspection**: Examine query variable bindings
- **Port filtering**: Choose which port events to pause on

## Requirements

- **VS Code**: Version 1.75.0 or higher
- **Python**: Version 3.10 or higher
- **PyLog**: PyLog interpreter with DAP support installed

## Installation

### Prerequisites

1. Install Python 3.10 or higher
2. Install PyLog with DAP support:
   ```bash
   # From the PyLog repository root
   uv sync
   ```

3. Verify the DAP server is available:
   ```bash
   uv run python -m prolog.dap.server --version
   ```

### Install Extension

Currently, the extension is in development. To install:

1. Download the `.vsix` file (or build from source)
2. Install via command line:
   ```bash
   code --install-extension pylog-vscode-X.Y.Z.vsix
   ```

Or install via VS Code:
- Open Extensions view (Ctrl+Shift+X / Cmd+Shift+X)
- Click "..." menu → "Install from VSIX..."
- Select the `.vsix` file

## Quick Start

1. **Create a Prolog file** (e.g., `main.pl`)
2. **Create a launch configuration**: Press F5 or go to Run and Debug view
3. **Select "PyLog: Launch"** from the configuration snippets
4. **Set breakpoints** (optional): Use predicate breakpoints in `launch.json`
5. **Start debugging**: Press F5

### Example Launch Configuration

Create `.vscode/launch.json` in your workspace:

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

### Advanced Configuration

```json
{
  "type": "pylog",
  "request": "launch",
  "name": "Debug with Breakpoints",
  "program": "${workspaceFolder}/main.pl",
  "query": "?- solve(Result).",
  "ports": ["CALL", "EXIT", "FAIL"],
  "predicateBreakpoints": [
    {
      "functor": "member",
      "arity": 2,
      "ports": ["CALL"]
    }
  ],
  "occursCheck": false,
  "useIndexing": true
}
```

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `program` | string | *required* | Path to the Prolog program file (.pl) |
| `query` | string | `"?- main."` | Query to execute |
| `stopOnEntry` | boolean | `true` | Pause at the first port event |
| `ports` | string[] | `["CALL", "EXIT", "FAIL"]` | Eligible port events for pausing |
| `predicateBreakpoints` | array | `[]` | Predicate breakpoints (functor/arity) |
| `occursCheck` | boolean | `false` | Enable occurs check during unification |
| `useIndexing` | boolean | `true` | Enable first-argument indexing |

## Troubleshooting

### Extension not activating
- Ensure VS Code version is 1.75.0 or higher
- Check the Output panel (View → Output) and select "PyLog Debugger"

### Server fails to start
- Verify Python is installed: `python --version`
- Verify PyLog DAP server is available: `uv run python -m prolog.dap.server --version`
- Check the Debug Console for error messages

### Breakpoints not working
- Ensure the predicate name and arity match exactly
- Check that the port filter includes the port where you expect to break

## Documentation

For complete documentation, see:
- [docs/dap.md](../docs/dap.md) - DAP architecture and implementation details
- [PyLog Documentation](../README.md) - Main PyLog project documentation

## Development

### Building from Source

```bash
cd vscode-pylog
npm install
npm run compile
```

### Running in Development Mode

1. Open the `vscode-pylog` folder in VS Code
2. Press F5 to launch Extension Development Host
3. In the new window, open a Prolog workspace and start debugging

## License

MIT

## Issues

Report issues at: https://github.com/xpqz/pylog/issues
