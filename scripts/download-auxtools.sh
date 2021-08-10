#!/bin/bash
set -euo pipefail

# Settings
DEBUG_SERVER_TAG=v2.2.2
DEBUG_SERVER_DLL_URL=https://github.com/willox/auxtools/releases/download/v2.2.2/debug_server.dll
DEBUG_SERVER_DLL_SHA256=847c3bfbd35f2eb57f43d0ba08df323c0f0beb1f7d518e7e54791a1aff13a56f

# -----------------------------------------------------------------------------
cd "$(dirname "${BASH_SOURCE[0]}")"
mkdir -p "../target/deps"
cd "../target/deps"

AUXTOOLS_BUNDLE_DLL="$PWD/debug_server.dll"
echo "export AUXTOOLS_BUNDLE_DLL=$AUXTOOLS_BUNDLE_DLL"
echo "export AUXTOOLS_COMMIT_HASH=$DEBUG_SERVER_TAG"

if ! test -f "$AUXTOOLS_BUNDLE_DLL" || ! sha256sum -c <<<"$DEBUG_SERVER_DLL_SHA256  $AUXTOOLS_BUNDLE_DLL" >/dev/null 2>/dev/null; then
    wget -q -O "$AUXTOOLS_BUNDLE_DLL" "$DEBUG_SERVER_DLL_URL" >&2
    sha256sum -c <<<"$DEBUG_SERVER_DLL_SHA256  $AUXTOOLS_BUNDLE_DLL" >&2
fi
