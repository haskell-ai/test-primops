#!/usr/bin/env bash

set -e -x

TEST_GHC="${TEST_GHC:-ghc}"
cabal build
gdb "$(cabal list-bin simple)" \
    -ex "break test" \
    -ex "run" \
    -ex "disassemble"
