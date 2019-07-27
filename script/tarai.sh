#!/bin/sh -eu

repository="$(git rev-parse --show-toplevel)" # repository root path

cd $repository/build
make clean
cmake .. -DCMAKE_BUILD_TYPE=release
make

script="$repository/test/tarai.scm"

sudo time perf record -- $repository/build/meevax < $script
sudo perf report

