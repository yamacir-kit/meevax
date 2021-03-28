#!/bin/sh -e

here="$(cd "$(dirname "$0")"; pwd)"

# sudo apt install graphviz python3 python3-pip
# sudo pip3 install gprof2dot

cd "$here"

# command time -pv meevax ack.ss
# gprof /usr/local/bin/meevax gmon.out > "$here/gmon.cpp"
# cat "$here/gmon.cpp" | gprof2dot --wrap | dot -Tpng -o gmon.png
# eog gmon.png

valgrind --tool=callgrind --callgrind-out-file=/tmp/callgrind.out meevax
gprof2dot -f callgrind /tmp/callgrind.out | dot -Tsvg -o /tmp/callgrind.svg
eog /tmp/callgrind.svg

cd -
