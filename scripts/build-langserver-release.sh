#!/bin/bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"
cd ..

# -----------------------------------------------------------------------------
# Prepare dependency DLLs
eval "$(scripts/download-extools.sh)"
eval "$(scripts/download-auxtools.sh)"

# -----------------------------------------------------------------------------
# Cargo build
touch src/langserver/build.rs

echo '==== Linux build ===='
cargo build --release --target x86_64-unknown-linux-musl -p dm-langserver

echo '==== Windows build ===='
cargo build --release --target x86_64-pc-windows-gnu -p dm-langserver

# -----------------------------------------------------------------------------
# Organize binaries into a directory for upload
echo '==== Organize files ===='
DEST=target/langserver-dist
rm -rf "$DEST"
mkdir -p "$DEST"
cp target/x86_64-unknown-linux-musl/release/dm-langserver "$DEST/x64-linux"
cp target/x86_64-pc-windows-gnu/release/dm-langserver.exe "$DEST/x64-win32.exe"
strip "$DEST/x64-linux"
x86_64-w64-mingw32-strip "$DEST/x64-win32.exe"
ls -lh --color=auto "$DEST"
