#!/usr/bin/env bash

set -Eeuo pipefail

CI_PROJECT_DIR="${CI_PROJECT_DIR:=$PWD}"
echo "$CI_PROJECT_DIR"

echo "Fetching bootstrap GHC via ghcup..."
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
export BOOTSTRAP_HASKELL_GHC_VERSION=9.4.5
export GHCUP_DIR="$CI_PROJECT_DIR/ghcup"
export GHCUP_USE_XDG_DIRS=1
# To prevent conflicts with other jobs on this Gitlab runner, install
# ghcup in local folders.
export XDG_DATA_HOME="$GHCUP_DIR/share"
export XDG_BIN_HOME="$GHCUP_DIR/bin"
export XDG_CACHE_HOME="$GHCUP_DIR/cache"
export XDG_CONFIG_HOME="$GHCUP_DIR/config"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
export GHC="$GHCUP_DIR/bin/ghc"
export CABAL="$GHCUP_DIR/bin/cabal"
source $GHCUP_DIR/share/ghcup/env



if [ -z ${BINDIST_URL+x} ]; then
  echo "Building from latest-nightly"
  ghcup config add-release-channel https://ghc.gitlab.haskell.org/ghcup-metadata/ghcup-nightlies-0.0.7.yaml
  ghcup install ghc latest-nightly
  export TEST_GHC="$(ghcup whereis ghc latest-nightly)"
else
  echo "Building from $BINDIST_URL"
  ghcup install ghc -u $BINDIST_URL head
  export TEST_GHC="$(ghcup whereis ghc head)"
fi

export BOOT_GHC="$GHC"

echo "Boot GHC:"
"$BOOT_GHC" --version
echo ""

echo "Test GHC:"
"$TEST_GHC" --version
echo ""

ARGS=()
if [ ! -z ${NO_LLVM+x} ]; then
  echo "Skipping LLVM backend tests..."
  ARGS+=("-p" '$2 !~ /llvm/')
fi

if [ ! -z ${GHC_ARGS+x} ]; then
  ARGS+=("--ghc-args" "$GHC_ARGS")
fi

./run.sh run "${ARGS[@]}"

