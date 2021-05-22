#!/bin/bash
set -euo pipefail

# Settings
DEBUG_SERVER_TAG=v2.2.0
DEBUG_SERVER_DLL_URL=https://github.com/willox/auxtools/releases/download/v2.2.0/debug_server.dll
DEBUG_SERVER_DLL_SHA256=495681eece67ff27019c56fbfc98cbf1d7f2a0442732aa34c342755926d8e122

# -----------------------------------------------------------------------------
cd "$(dirname "${BASH_SOURCE[0]}")"
mkdir -p "../target/deps"
cd "../target/deps"

AUXTOOLS_BUNDLE_DLL="$PWD/debug_server.dll"
echo "AUXTOOLS_BUNDLE_DLL=$AUXTOOLS_BUNDLE_DLL"
echo "AUXTOOLS_COMMIT_HASH=$DEBUG_SERVER_TAG"

if ! test -f "$AUXTOOLS_BUNDLE_DLL"; then
    wget -q -O "$AUXTOOLS_BUNDLE_DLL" "$DEBUG_SERVER_DLL_URL" >&2
fi
sha256sum -c <<<"$DEBUG_SERVER_DLL_SHA256  $AUXTOOLS_BUNDLE_DLL" >&2
