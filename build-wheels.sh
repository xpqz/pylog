#!/bin/bash
# Build script for PyLog wheels
# Creates both standard and minimal web wheels

set -e

echo "Building PyLog wheels..."

# Clean previous builds
rm -rf dist/
mkdir -p dist/

# Build standard wheel (full dependencies)
echo "Building standard wheel..."
uv build --wheel
mv dist/pylog-0.1.0-py3-none-any.whl dist/pylog-0.1.0-standard-py3-none-any.whl

# Build web wheel (minimal dependencies)
echo "Building web wheel..."
cp pyproject.toml pyproject-full.toml
cp pyproject-web.toml pyproject.toml
uv run --with build python -m build --wheel
mv dist/pylog-0.1.0-py3-none-any.whl dist/pylog-0.1.0-web-py3-none-any.whl
cp pyproject-full.toml pyproject.toml

echo "Built wheels:"
ls -la dist/*.whl

echo "Standard wheel dependencies:"
unzip -q -c dist/pylog-0.1.0-standard-py3-none-any.whl pylog-0.1.0.dist-info/METADATA | grep "Requires-Dist:" | head -3

echo "Web wheel dependencies:"
unzip -q -c dist/pylog-0.1.0-web-py3-none-any.whl pylog-0.1.0.dist-info/METADATA | grep "Requires-Dist:"

echo "Done!"