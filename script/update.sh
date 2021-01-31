#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

"$root/script/version.sh"

cd "$root/build"

make clean
make uninstall

cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=g++

make -j

sudo make -j install

make test -j
