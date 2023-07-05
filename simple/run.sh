#!/usr/bin/env bash

set -e -x

TEST_GHC="${TEST_GHC:-ghc}"
CABAL="${CABAL:-cabal}"
# QEMU executable
EMULATOR="${EMULATOR:-}"

rm -Rf dist-newstyle
args=(
    "-w$TEST_GHC"
    "--ghc-options=$@"
    "--disable-optimization"
)

"$CABAL" build "${args[@]}" -v3 simple
exe=$("$CABAL" list-bin "${args[@]}" simple)

echo "Using executable: $exe"

if [ -n "$EMULATOR" ]; then
    $EMULATOR -g 1111 $exe &
    gdb \
        -ex "file $exe" \
        -ex "break test" \
        -ex "target remote localhost:1111" \
        -ex "continue" \
        -ex "disassemble"
else
    gdb "$exe" \
        -ex "break test" \
        -ex "run" \
        -ex "disassemble"
fi
