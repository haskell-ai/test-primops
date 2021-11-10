#!/usr/bin/env bash

set -e -x

TEST_GHC="${TEST_GHC:-ghc}"
CABAL="${CABAL:-cabal}"

rm -Rf dist-newstyle
args=(
    "-w$TEST_GHC"
    "--ghc-options=$@"
    "--disable-optimization"
    "-v3"
)

"$CABAL" build "${args[@]}" simple
exe="$("$CABAL" list-bin "${args[@]}" simple)"

gdb "$exe" \
    -ex "break test" \
    -ex "run" \
    -ex "disassemble"
