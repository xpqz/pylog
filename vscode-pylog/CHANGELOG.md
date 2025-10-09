# Change Log

All notable changes to the "pylog-vscode" extension will be documented in this file.

## [Unreleased]

## [0.1.0] - 2025-10-09

### Added
- Initial extension scaffolding and package setup (#283)
- TypeScript configuration for VS Code extension development
- Debug adapter descriptor factory implementation (#284)
- Python environment auto-detection (workspace venv, system python)
- PyLog DAP server verification and spawning
- Launch configuration schema and snippets (#285)
- Configuration support for custom Python path
- Output channel for diagnostic logging
- Error handling and user-friendly error messages
- Documentation (README and CHANGELOG)
- Extension packaging as VSIX (#286)

### Features
- Step control (continue, step in, step over, step out)
- Predicate breakpoints by functor/arity
- Call stack and variable inspection
- Port filtering (CALL, EXIT, FAIL, REDO)
- Engine configuration (occurs check, indexing)
