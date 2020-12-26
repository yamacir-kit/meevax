#!/bin/sh -e

here="$(cd "$(dirname "$0")"; pwd)"

# root="$(git rev-parse --show-toplevel)"

cd "$here"

command time -pv meevax ack.ss

gprof /usr/local/bin/meevax gmon.out > "$here/gmon.cpp"

cd -
