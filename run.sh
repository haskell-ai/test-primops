#!/usr/bin/env bash

set -e -o pipefail

BOOT_GHC="${BOOT_GHC:-ghc}"
TEST_GHC="${TEST_GHC:-ghc}"

ALLOW_NEWER="--allow-newer=base"
cabal build $ALLOW_NEWER -w "$TEST_GHC" run-it
RUNIT="$(cabal list-bin $ALLOW_NEWER -w "$TEST_GHC" run-it)"
echo "runit is $RUNIT"

cabal run -w "$BOOT_GHC" test-primops -- \
    --ghc-path="$TEST_GHC" \
    --run-it-path="$RUNIT" \
    $@
