#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

"$root/script/version.sh"

cd "$root/build"

make clean
make uninstall

cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=clang++

time make -j
sudo make install

make test -j
