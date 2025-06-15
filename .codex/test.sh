#!/usr/bin/env bash

set -euo pipefail

apt update

apt install -y \
  libgmp-dev \
  zstd

unzstd --stdout codex-cache.tar.zst | tar x --absolute-names

. ~/.ghcup/env

cabal update

exec cabal build
