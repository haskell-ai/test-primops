#!/usr/bin/env bash

set -e -o pipefail

BOOT_GHC="${BOOT_GHC:-ghc}"
TEST_GHC="${TEST_GHC:-ghc}"
CABAL="${CABAL:-cabal}"
EMULATOR="${EMULATOR:-}"

if "$TEST_GHC" --info | grep -q '("target word size","8")'; then
    echo "Found 64-bit target"
else
    echo "Found 32-bit target"
    CABAL_ARGS="-ftarget-32bit"
fi

build_runit() {
    ALLOW_NEWER="--allow-newer=base"
    "$CABAL" build $ALLOW_NEWER -w "$TEST_GHC" $CABAL_ARGS run-it
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
    "$CABAL" run -w "$BOOT_GHC" $CABAL_ARGS test-primops -- "${args[@]}" "$@"
}

repl() {
    build_runit

    cat >test-primops.ghci <<EOF
:m + RunGhc ToCmm Expr.Parse Expr Width
let ghcPath = "$TEST_GHC";
let runItPath = "$RUNIT";
let comp = Compiler.basicCompiler ghcPath;
let staticEval = RunGhc.staticEvalMethod comp;
let dynEval = RunGhc.DynamicEval comp runItPath;
let interpreter = Interpreter.ghcInterpreter dynEval;
putStrLn "Hello world"
putStrLn $ "Compiler under test is " ++ ghcPath
EOF

    printf "For best results, run\n\n"
    printf "  :script test-primops.ghci\n\n"

    "$CABAL" repl \
        -w "$BOOT_GHC" \
        lib:test-primops
}

mode="$1"
case $mode in
  run) shift; run "$@" ;;
  repl) shift; repl ;;
  *) run "$@" ;;
esac
