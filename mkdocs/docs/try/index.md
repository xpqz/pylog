# Try PyLog

Experience PyLog directly in your browser! This interactive terminal REPL runs entirely client-side using WebAssembly via Pyodide.

<div id="pylog-repl-container">
  <!-- Terminal will be initialized here automatically -->
</div>

## Quick Examples

Try these queries to get started:

- **Simple unification**: `X = hello`
- **List membership**: `member(X, [1, 2, 3])`
- **Append lists**: `append([1, 2], [3, 4], X)`
- **CLP(FD) constraints**: `X in 1..10, X #> 5, label([X])`

## Safety Features

- **Step limit**: Queries are limited to prevent infinite loops
- **Solution limit**: Maximum number of solutions returned
- **Timeout protection**: Queries automatically abort after time limit
- **Worker isolation**: Runs in separate Web Worker for UI responsiveness

## Learning Resources

- [Getting Started Guide](../getting-started/install.md) - Installation and basics
- [Prolog Basics](../basics/terms.md) - Core Prolog concepts
- [CLP(FD) Introduction](../clpfd/intro.md) - Constraint programming
- [Cookbook](../cookbook/list-processing.md) - Examples and recipes

<!-- Pyodide for Python in browser -->
<script src="https://cdn.jsdelivr.net/pyodide/v0.24.1/full/pyodide.js"></script>

<!-- PyLog REPL implementation -->
<!-- Legacy xterm build retained for reference: xterm.js, local-echo, pyrepl-xterm.js -->
<script src="pyrepl-terminal.js"></script>
