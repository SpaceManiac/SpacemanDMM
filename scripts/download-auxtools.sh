#!/bin/bash
set -euo pipefail

# Settings
DEBUG_SERVER_TAG=v2.2.4
DEBUG_SERVER_DLL_URL=https://github.com/willox/auxtools/releases/download/v2.2.4/debug_server.dll
DEBUG_SERVER_DLL_SHA256=2a81e64d5cacef372043af476e9b1b2d21e38a6f9f708e0da8ca573bdcfcf0d3

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
