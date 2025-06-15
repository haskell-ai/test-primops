#!/usr/bin/env bash

set -euo pipefail

apt update

apt install -y \
  libgmp-dev \
  zstd

curl -f -L --retry 5 "https://github.com/haskell-ai/test-primops/releases/download/lkgr-$(awk -F"'" '/branch / { print $2; exit }' .git/FETCH_HEAD)/codex-cache.tar.zst" -o codex-cache.tar.zst

unzstd --stdout codex-cache.tar.zst | tar x --absolute-names

rm codex-cache.tar.zst

. ~/.ghcup/env

exec cabal update
