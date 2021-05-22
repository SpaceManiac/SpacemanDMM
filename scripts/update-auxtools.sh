#!/bin/bash
set -euo pipefail

# Settings
API='https://api.github.com'
REPO='willox/auxtools'

# -----------------------------------------------------------------------------
cd "$(dirname "${BASH_SOURCE[0]}")"
DEBUG_SERVER_TAG=$(curl -s "$API/repos/$REPO/releases" | jq -re '.[0].tag_name')
DEBUG_SERVER_DLL_URL=https://github.com/willox/auxtools/releases/download/$DEBUG_SERVER_TAG/debug_server.dll
DEBUG_SERVER_DLL_SHA256=$(curl -L -s "$DEBUG_SERVER_DLL_URL" | sha256sum | cut -d' ' -f1)

sed \
    -e "/^DEBUG_SERVER_TAG=/c DEBUG_SERVER_TAG=$DEBUG_SERVER_TAG" \
    -e "/^DEBUG_SERVER_DLL_URL=/c DEBUG_SERVER_DLL_URL=$DEBUG_SERVER_DLL_URL" \
    -e "/^DEBUG_SERVER_DLL_SHA256=/c DEBUG_SERVER_DLL_SHA256=$DEBUG_SERVER_DLL_SHA256" \
    --in-place \
    download-auxtools.sh

# Prepare the commit
git add download-auxtools.sh
git commit -m "Update to auxtools debug server $DEBUG_SERVER_TAG"
