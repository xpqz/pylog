.PHONY: help test coverage coverage-html coverage-report clean format lint docs docs-serve docs-clean all

help:  ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

test:  ## Run all tests
	uv run pytest

test-fast:  ## Run tests excluding slow/stress tests
	uv run pytest -m "not slow and not stress"

coverage:  ## Run tests with coverage
	uv run pytest --cov=prolog --cov-branch --cov-report=term-missing:skip-covered

coverage-fast:  ## Run fast tests with coverage (exclude slow/stress)
	uv run pytest -m "not slow and not stress" --cov=prolog --cov-branch --cov-report=term-missing:skip-covered

coverage-html:  ## Generate HTML coverage report
	uv run pytest --cov=prolog --cov-branch --cov-report=html --cov-report=term
	@echo "Coverage report generated in htmlcov/index.html"

coverage-report:  ## Show coverage report in terminal
	uv run coverage report

coverage-xml:  ## Generate XML coverage report for CI
	uv run pytest --cov=prolog --cov-branch --cov-report=xml --cov-report=term

coverage-combine:  ## Combine parallel coverage files
	uv run coverage combine || true
	uv run coverage xml -i

clean:  ## Clean up generated files
	rm -rf htmlcov/
	rm -f .coverage
	rm -f coverage.xml
	rm -f pytest.xml
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete

format:  ## Format code with black
	uv run black .

lint:  ## Run linters
	uv run ruff check .
	uv run black --check .

all: format lint test  ## Format, lint, and test

docs: ## Build MkDocs site (output to mkdocs/site)
	uv run --extra docs mkdocs build -f mkdocs/mkdocs.yml

docs-serve: ## Serve MkDocs site locally with live reload
	uv run --extra docs mkdocs serve -f mkdocs/mkdocs.yml -a 127.0.0.1:8000

docs-clean: ## Clean built MkDocs site
	rm -rf mkdocs/site/
