#!/usr/bin/env python3
"""Fail if any Python file has conditional or late imports.

Rules enforced (per coderules.md):
- No conditional imports (no imports inside if/try/except/def/class, etc.)
- All imports must appear at the top of the file only, after optional
  module docstring, before any other top-level statement.

This script inspects the staged contents of files (from the Git index) when
available; it falls back to the working tree if the file is not in the index.
"""

from __future__ import annotations

import argparse
import warnings
import ast
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Tuple


def read_staged_or_worktree(path: Path) -> str:
    """Return the file contents from the Git index (staged), or working tree.

    Uses `git show :<path>` to read the staged version. If that fails (e.g.
    file not yet tracked), reads from disk.
    """
    rel = str(path)
    try:
        cp = subprocess.run(
            ["git", "show", f":{rel}"],
            check=False,
            capture_output=True,
            text=True,
        )
        if cp.returncode == 0 and cp.stdout:
            return cp.stdout
    except Exception:
        pass
    return path.read_text(encoding="utf-8")


def build_parent_map(node: ast.AST) -> Dict[ast.AST, Optional[ast.AST]]:
    parent: Dict[ast.AST, Optional[ast.AST]] = {node: None}

    class ParentVisitor(ast.NodeVisitor):
        def generic_visit(self, n: ast.AST) -> None:
            for child in ast.iter_child_nodes(n):
                parent[child] = n
            super().generic_visit(n)

    ParentVisitor().visit(node)
    return parent


def is_docstring_stmt(stmt: ast.stmt) -> bool:
    return isinstance(stmt, ast.Expr) and isinstance(getattr(stmt, "value", None), ast.Constant) and isinstance(stmt.value.value, str)


def check_file(path: Path) -> List[str]:
    src = read_staged_or_worktree(path)

    try:
        # Silence SyntaxWarning noise from docstrings with backslashes, etc.
        warnings.filterwarnings("ignore", category=SyntaxWarning)
        tree = ast.parse(src, filename=str(path))
    except SyntaxError as e:
        # Don't fail commits for syntax errors here; let linters/tests handle it
        return []

    parent = build_parent_map(tree)
    errors: List[str] = []

    # 1) Reject any imports not directly under ast.Module (conditional imports)
    for node in ast.walk(tree):
        if isinstance(node, (ast.Import, ast.ImportFrom)):
            p = parent.get(node)
            if not isinstance(p, ast.Module):
                # Conditional/inner import
                where = type(p).__name__ if p is not None else "<unknown>"
                errors.append(
                    f"{path}:{node.lineno}: conditional import inside {where}"
                )

    # 2) Ensure all top-level imports are before any other top-level stmt
    if isinstance(tree, ast.Module):
        seen_non_import = False
        for stmt in tree.body:
            if is_docstring_stmt(stmt):
                # Module docstring allowed before imports
                if seen_non_import:
                    # Import appearing after non-import already flagged below
                    pass
                continue
            if isinstance(stmt, (ast.Import, ast.ImportFrom)):
                if seen_non_import:
                    # Late import at top level
                    errors.append(
                        f"{path}:{stmt.lineno}: import not at top of file"
                    )
                continue
            # Any other top-level statement marks end of import section
            seen_non_import = True

    return errors


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(description="Reject conditional or late imports")
    ap.add_argument("files", nargs="*", help="Python files to check")
    args = ap.parse_args(argv)

    py_files = [Path(f) for f in args.files if f.endswith(".py")]
    all_errors: List[str] = []
    for f in py_files:
        if not f.exists():
            # Might be deleted or renamed; ignore
            continue
        all_errors.extend(check_file(f))

    if all_errors:
        print("Found forbidden imports:")
        for e in all_errors:
            print(f"  - {e}")
        print("\nPolicy: No conditional imports; all imports at file top only.")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
