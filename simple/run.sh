#!/usr/bin/env bash

set -e -x

TEST_GHC="${TEST_GHC:-ghc}"
cabal build -w "$TEST_GHC"
exe="$(cabal list-bin -w "$TEST_GHC" simple)"
gdb "$exe" \
    -ex "break test" \
    -ex "run" \
    -ex "disassemble"
