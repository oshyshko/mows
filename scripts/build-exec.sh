#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

#exprot WONDER_PORT=8080
#export TICK_INTERVAL=2

# export LDFLAGS="-L/usr/local/opt/llvm@9/lib"
# export CPPFLAGS="-I/usr/local/opt/llvm@9/include"
# export PATH="/usr/local/opt/llvm@9/bin:$PATH"

# mkdir -p ./dist

# stack build --copy-bins --local-bin-path=./dist
stack build

# see https://ghc.readthedocs.io/en/8.0.1/sooner.html
# see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html

stack exec mows -- $@
