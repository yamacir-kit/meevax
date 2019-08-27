#!/bin/sh -eu

repository="$(git rev-parse --show-toplevel)" # repository root path

cd $repository/build
make clean
cmake .. -DCMAKE_BUILD_TYPE=release
make -j4

script="$repository/test/tarai.scm"

sudo time perf record -- $repository/build/bin/meevax < $script
sudo perf report

