#!/usr/bin/env bash

set -e -o pipefail

BOOT_GHC="${BOOT_GHC:-ghc}"
TEST_GHC="${TEST_GHC:-ghc}"

build_runit() {
    ALLOW_NEWER="--allow-newer=base"
    cabal build $ALLOW_NEWER -w "$TEST_GHC" run-it
    RUNIT="$(cabal list-bin $ALLOW_NEWER -w "$TEST_GHC" run-it)"
    echo "runit is $RUNIT"
}

run() {
    build_runit
    cabal run -w "$BOOT_GHC" test-primops -- \
        --ghc-path="$TEST_GHC" \
        --run-it-path="$RUNIT" \
        $@
}

repl() {
    build_runit
    cat >.ghci <<EOF
let ghcPath = "$TEST_GHC";
let runItPath = "$RUNIT";
let comp = (basicCompiler ghcPath) { compRunIt = runItPath };
putStrLn "Hello world"
putStrLn $ "Compiler under test is " ++ ghcPath

EOF
    cabal repl -w "$BOOT_GHC" test-primops
}

mode="$1"
case $mode in
  "") run ;;
  run) shift; run ;;
  repl) shift; repl ;;
esac
