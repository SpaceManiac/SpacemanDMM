#!/bin/bash
set -euo pipefail

relname=$(git describe --tags --exact)
echo "Using tag name: $relname"

touch src/*/build.rs

echo '==== Linux build ====' # ------------------------------------------------
cargo build --release --target x86_64-unknown-linux-musl

echo '==== Windows build ====' # ----------------------------------------------
cargo build --release --target x86_64-pc-windows-gnu

echo '==== Organize files ====' # ---------------------------------------------
DEST=target/dist
rm -rf "$DEST"
mkdir -p "$DEST"
cp \
    target/x86_64-unknown-linux-musl/release/{dreamchecker,dmdoc,dmm-tools,dm-langserver} \
    target/x86_64-pc-windows-gnu/release/{dreamchecker,dmdoc,dmm-tools,dm-langserver}.exe \
    "$DEST"
echo "# SpacemanDMM $relname" | tee $DEST/$relname.sha256
sha256sum "$DEST"/* | tee -a $DEST/$relname.sha256
gpg2 --sign --armor --detach $DEST/$relname.sha256
gpg2 --verify $DEST/$relname.sha256.asc
ls -lh --color=auto "$DEST"
