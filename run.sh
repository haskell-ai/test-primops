#!/usr/bin/env bash

set -e -o pipefail

BOOT_GHC="${BOOT_GHC:-ghc}"
TEST_GHC="${TEST_GHC:-ghc}"
CABAL="${CABAL:-cabal}"
EMULATOR="${EMULATOR:-}"

build_runit() {
    ALLOW_NEWER="--allow-newer=base"
    "$CABAL" build $ALLOW_NEWER -w "$TEST_GHC" run-it
    RUNIT="$("$CABAL" list-bin $ALLOW_NEWER -w "$TEST_GHC" run-it)"
    echo "runit is $RUNIT"
}

run() {
    build_runit
    args=(
        "--ghc-path=$TEST_GHC"
        "--run-it-path=$RUNIT"
    )
    if [[ -n "$EMULATOR" ]]; then
        args+=( "--emulator=$EMULATOR" )
    fi
    "$CABAL" run -w "$BOOT_GHC" test-primops -- "${args[@]}" "$@"
}

repl() {
    build_runit
    cat >.ghci <<EOF
let ghcPath = "$TEST_GHC";
let runItPath = "$RUNIT";
let comp = (basicCompiler ghcPath) { RunGhc.compRunIt = runItPath };
putStrLn "Hello world"
putStrLn $ "Compiler under test is " ++ ghcPath

EOF
    "$CABAL" repl -w "$BOOT_GHC" test-primops
}

mode="$1"
case $mode in
  run) shift; run "$@" ;;
  repl) shift; repl ;;
  *) run "$@" ;;
esac
