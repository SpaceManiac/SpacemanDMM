#!/bin/bash

set -e

DIR=debug
FLAG=
if [ "$1" == "--release" ]; then
    DIR=release
    FLAG=--release
fi

echo "Building $DIR WASM..."
cargo rustc -p dm-langserver --target wasm32-unknown-emscripten $FLAG -- \
    -C link-args="--js-library src/langserver/wasm-imports.js"

echo "Post-processing JS..."
DIR=target/wasm32-unknown-emscripten/$DIR
(
    echo "export default function(Module) {"
    cat $DIR/dm-langserver.js
    echo "return Module; }"
) >$DIR/dm_langserver.js
