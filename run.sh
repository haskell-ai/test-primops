#!/usr/bin/env ghc

set -e -o pipefail

BOOT_GHC="${BOOT_GHC:-ghc}"
TEST_GHC="${TEST_GHC:-ghc}"

cabal build -w "$TEST_GHC" run-it
RUNIT="$(cabal list-bin -w "$TEST_GHC" run-it)"

cabal run -w "$BOOT_GHC" test-primops -- \
    --ghc-path="$TEST_GHC" \
    --run-it-path="$RUNIT" \
    $@
