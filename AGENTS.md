# `test-primops`

## Setting up the build environment

This is the source repository of `test-primops`, and you need to
execute these steps in the interactive bash session exactly once, to
set up the build environment:

1. Run `./.codex/init.sh` in the repository root directory. It will
   take quite a bit of time to initialize necessary build tools, do
   not interrupt!
2. Then, do `. ~/.ghcup/env` to make sure the build tools are
   available in `PATH`.

Afterwards, you can build `test-primops` using the instructions below.

## Building `test-primops`

Use `cabal build` to build `test-primops`. When you put up a patch,
always do this to ensure your patch compiles correctly. Do not
interrupt this step, it will take quite a bit of time but is critical
for quality assurance of your work!
