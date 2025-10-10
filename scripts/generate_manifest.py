#!/usr/bin/env python3
"""Generate manifest.json for Web REPL assets.

This script generates the manifest file that the Web REPL uses to load
wheel files. It includes the current git commit SHA for cache-busting.
"""

import json
import subprocess
from datetime import datetime, timezone
from pathlib import Path


def get_git_commit_sha():
    """Get the current git commit SHA."""
    result = subprocess.run(
        ["git", "rev-parse", "HEAD"], capture_output=True, text=True, check=True
    )
    return result.stdout.strip()


def get_pylog_version():
    """Extract PyLog version from pyproject.toml."""
    pyproject_path = Path(__file__).parent.parent / "pyproject.toml"
    with open(pyproject_path) as f:
        for line in f:
            if line.startswith("version = "):
                # Extract version from: version = "0.1.0"
                return line.split('"')[1]
    return "0.1.0"  # fallback


def main():
    """Generate the manifest.json file."""
    # Get current commit SHA
    commit_sha = get_git_commit_sha()
    print(f"Current commit SHA: {commit_sha}")

    # Get PyLog version
    pylog_version = get_pylog_version()
    print(f"PyLog version: {pylog_version}")

    # Generate manifest
    manifest = {
        "version": pylog_version,
        "tag": f"v{pylog_version}",
        "commit_sha": commit_sha,
        "pylog": {
            "wheel": f"pylog-{pylog_version}-py3-none-any.whl",
            "version": pylog_version,
            "url": f"https://cdn.jsdelivr.net/gh/xpqz/pylog@{commit_sha}/wheels/pylog-{pylog_version}-py3-none-any.whl",
        },
        "lark": {
            "wheel": "lark-1.3.0-py3-none-any.whl",
            "url": f"https://cdn.jsdelivr.net/gh/xpqz/pylog@{commit_sha}/wheels/lark-1.3.0-py3-none-any.whl",
        },
        "generated_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "checksums": "checksums.txt",
    }

    # Write manifest
    manifest_path = (
        Path(__file__).parent.parent
        / "mkdocs"
        / "docs"
        / "try"
        / "assets"
        / "manifest.json"
    )
    manifest_path.parent.mkdir(parents=True, exist_ok=True)

    with open(manifest_path, "w") as f:
        json.dump(manifest, f, indent=2)
        f.write("\n")

    print(f"Manifest written to {manifest_path}")
    print(json.dumps(manifest, indent=2))


if __name__ == "__main__":
    main()
