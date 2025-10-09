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

# Find the generated standard wheel and rename it
STANDARD_WHEEL=$(ls dist/pylog-*-py3-none-any.whl)
if [ -z "$STANDARD_WHEEL" ]; then
    echo "Error: No standard wheel found in dist/"
    exit 1
fi

# Extract version from filename for consistent naming
WHEEL_BASENAME=$(basename "$STANDARD_WHEEL" .whl)
VERSION=$(echo "$WHEEL_BASENAME" | sed 's/pylog-\(.*\)-py3-none-any/\1/')

echo "Detected version: $VERSION"
mv "$STANDARD_WHEEL" "dist/pylog-${VERSION}-standard-py3-none-any.whl"

# Build web wheel (minimal dependencies)
echo "Building web wheel..."
cp pyproject.toml pyproject-full.toml
cp pyproject-web.toml pyproject.toml
uv run --with build python -m build --wheel

# Find and rename the web wheel (exclude the already renamed standard wheel)
WEB_WHEEL=$(ls dist/pylog-*-py3-none-any.whl | grep -v standard)
if [ -z "$WEB_WHEEL" ]; then
    echo "Error: No web wheel found in dist/"
    exit 1
fi

mv "$WEB_WHEEL" "dist/pylog-${VERSION}-web-py3-none-any.whl"
cp pyproject-full.toml pyproject.toml

echo "Built wheels:"
ls -la dist/*.whl

echo "Standard wheel dependencies:"
STANDARD_WHEEL_FINAL="dist/pylog-${VERSION}-standard-py3-none-any.whl"
unzip -q -c "$STANDARD_WHEEL_FINAL" "pylog-${VERSION}.dist-info/METADATA" | grep "Requires-Dist:" | head -3

echo "Web wheel dependencies:"
WEB_WHEEL_FINAL="dist/pylog-${VERSION}-web-py3-none-any.whl"
unzip -q -c "$WEB_WHEEL_FINAL" "pylog-${VERSION}.dist-info/METADATA" | grep "Requires-Dist:"

echo "Done!"