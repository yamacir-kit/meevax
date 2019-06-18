#!/bin/sh -eu

project="$(cd "$(dirname $0)"; pwd)" # script path

cd $project/build
make clean
cmake .. -DCMAKE_BUILD_TYPE=release
make

script="$project/test/tarai.scm"

sudo time perf record -- $project/build/meevax < $script
sudo perf report

