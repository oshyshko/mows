#!/bin/sh

# see https://downloads.haskell.org/~ghc/8.10.4/docs/html/users_guide/runtime_control.html#rts-flag---nonmoving-gc
# -xn -- non-moving GC

NAME="aeson-xn-th-dt-wscomp"
CMD_SERVER="stack exec mows -- +RTS -s -xn"
CMD_BOT="java -cp ./doc/wonder.jar BotKt 2000 200 60"

REP_SERVER="$PWD/$$-server.txt"
REP_BOT="$PWD/$$-bot.txt"
REP_ALL="$PWD/$NAME-$$.txt"


export TARGET_URL="127.0.0.1:8080"

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# jump to project directory
cd "$(dirname $0)/../"

set -xue

stack build

eval "$CMD_SERVER 2>>$REP_SERVER &"
PID_SERVER=$!

sleep 2

eval "$CMD_BOT >>$REP_BOT"
PID_BOT=$!

kill -s INT "$PID_SERVER"

sleep 1

echo "\n===== $CMD_SERVER\n" >> $REP_ALL
cat $REP_SERVER              >> $REP_ALL
echo "\n===== $CMD_BOT\n"    >> $REP_ALL
cat $REP_BOT                 >> $REP_ALL

rm -f $REP_SERVER $REP_BOT