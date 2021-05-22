#!/bin/bash
set -euo pipefail

# Settings
EXTOOLS_TAG=v0.0.7
EXTOOLS_DLL_URL=https://github.com/tgstation/tgstation/raw/34f0cc6394a064b87cbd1d6cb225f1d3df444ba7/byond-extools.dll
EXTOOLS_DLL_SHA256=073dd08790a13580bae71758e9217917700dd85ce8d35cb030cef0cf5920fca8

# -----------------------------------------------------------------------------
cd "$(dirname "${BASH_SOURCE[0]}")"
mkdir -p "../target/deps"
cd "../target/deps"

EXTOOLS_BUNDLE_DLL="$PWD/extools.dll"
echo "export EXTOOLS_BUNDLE_DLL=$EXTOOLS_BUNDLE_DLL"
echo "export EXTOOLS_COMMIT_HASH=$EXTOOLS_TAG"

if ! test -f "$EXTOOLS_BUNDLE_DLL" || ! sha256sum -c <<<"$EXTOOLS_DLL_SHA256  $EXTOOLS_BUNDLE_DLL" >/dev/null 2>/dev/null; then
    wget -q -O "$EXTOOLS_BUNDLE_DLL" "$EXTOOLS_DLL_URL" >&2
    sha256sum -c <<<"$EXTOOLS_DLL_SHA256  $EXTOOLS_BUNDLE_DLL" >&2
fi
