# Try PyLog

Experience PyLog directly in your browser! This interactive terminal REPL runs entirely client-side using WebAssembly via Pyodide.

<div id="pylog-repl-container">
  <div id="pylog-loading" style="text-align: center; padding: 20px;">
    <p>Loading PyLog REPL...</p>
    <p><small>Click "Start REPL" to initialize Pyodide (this may take a few seconds on first load)</small></p>
  </div>

  <div id="pylog-repl" style="display: none;">
    <!-- REPL UI will be injected here -->
  </div>
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

<script src="pyrepl-terminal.js"></script>
<!-- <script src="examples.js"></script> Not needed for terminal UI -->