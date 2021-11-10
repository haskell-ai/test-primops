#!/usr/bin/env bash

set -e -x

TEST_GHC="${TEST_GHC:-ghc}"
CABAL="${CABAL:-cabal}"

rm -Rf dist-newstyle
"$CABAL" build -w "$TEST_GHC" --ghc-options="$@" -v3
exe="$("$CABAL" list-bin -w "$TEST_GHC" --ghc-options="$@" simple)"
gdb "$exe" \
    -ex "break test" \
    -ex "run" \
    -ex "disassemble"
