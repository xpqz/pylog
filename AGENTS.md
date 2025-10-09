# Repository Guidelines

## Project Structure & Module Organization
- `prolog/`: Python source (engine, parser, CLP(FD), REPL). Examples in `prolog/examples/`.
- `prolog/tests/`: Test suite — `unit/`, `scenarios/`, `benchmarks/`.
- `mkdocs/`: User docs site (`mkdocs/docs/`, config in `mkdocs/mkdocs.yml`).
- `scripts/`, `Makefile`: Helper scripts and common tasks.
- `pyproject.toml`: Tooling config; CLI entrypoint `pylog = prolog.repl:main`.

## Build, Test, and Development Commands
- Prereqs: Python 3.11+. Recommended: `uv`.
- Setup: `uv sync` (or `pip install -e '.[dev]'`).
- Run tests: `make test`; fast subset: `make test-fast`.
- Coverage: `make coverage` or `make coverage-html` (see `htmlcov/index.html`).
- Lint/format: `make lint`; `make format` (Black).
- REPL/CLI: `pylog` or `python -m prolog.repl` (e.g., `pylog -g "member(X,[a,b])" --all --noninteractive`).

## Coding Style & Naming Conventions
- Indentation: 4 spaces. Formatter: Black (line length 88).
- Lint: Ruff (`make lint`); keep warnings clean.
- Names: modules/vars `snake_case`, classes `CapWords`, constants `UPPER_SNAKE`.
- Imports: group/order consistently (isort style).

## Testing Guidelines
- Framework: `pytest` (+ `pytest-cov`). Tests live under `prolog/tests/`.
- Naming: files `test_*.py`; test functions `test_*`.
- Markers: use `slow`, `stress`, `swi_baseline` as appropriate. Fast suite excludes slow/stress.
- Examples: run one test `uv run pytest prolog/tests/unit/test_unify.py::test_occurs_check`.
- Test integrity: do not change tests to make code pass — see `CLAUDE.md`.

## Commit & Pull Request Guidelines
- Branches: `{issue-id}-{short-desc}` (e.g., `271-stepping-control`).
- Commits: imperative, focused, reference issues (e.g., "Implement DAP event emission (#271)").
- Before PR: `make test-fast`, `make lint`, ensure coverage is reported.
- PRs: clear summary, linked issues, describe behavior changes; include tests.
- Git hooks: `make install-git-hooks` (pre-commit: Black/Ruff; commit-msg: no AI mentions; pre-push: fast tests).

## Docs
- Build: `make docs`; serve locally: `make docs-serve` (at `127.0.0.1:8000`).
- Place user docs in `mkdocs/docs/`; keep reference examples runnable.

