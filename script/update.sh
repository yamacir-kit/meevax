#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

"$root/script/version.sh"

rm -rf "$root/build"

cmake -B "$root/build" -S "$root" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++

cd "$root/build"

make uninstall
make -j"$(nproc)"
sudo make install -j"$(nproc)"
make test -j"$(nproc)"
