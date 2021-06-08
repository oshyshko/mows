#!/bin/sh

set -xue

EXE=mows-exe

# jump to project directory
cd "$(dirname $0)/../"

stack --work-dir .stack-work-profile build --executable-profiling --no-library-profiling
stack --work-dir .stack-work-profile exec  --executable-profiling --no-library-profiling \
   -- "$EXE" +RTS \
   -xc \
   -p  \
   -hy \
   -s  \
   --RTS $@

# see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
# -xc  -- Show current cost centre stack on raising an exception
# -p   -- Time/allocation profile in tree format
#         https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html#time-and-allocation-profiling
# -hc  -- Produce a heap profile grouped by closure type
# -hy  -- Produce a heap profile grouped by type
# -T   -- enable GHC.Stats

# run profiteur, if installed
if [ -x "$(command -v profiteur)" ]; then
  profiteur "$EXE.prof"

  # open profiteur report in browser, if on macos
  if [[ $OSTYPE == darwin* ]]; then
    open "$EXE.prof.html"
  fi
fi

# visualize heap allocations
stack exec hp2ps -- -e8in -c "$EXE.hp"
open "$EXE.ps"
