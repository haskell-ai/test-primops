#!/usr/bin/env bash

set -e -x

TEST_GHC="${TEST_GHC:-ghc}"
CABAL="${CABAL:-cabal}"

"$CABAL" build -w "$TEST_GHC"
exe="$("$CABAL" list-bin -w "$TEST_GHC" simple)"
gdb "$exe" \
    -ex "break test" \
    -ex "run" \
    -ex "disassemble"
