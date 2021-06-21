#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

"$root/script/version.sh"

rm -rf "$root/build"

cmake -B "$root/build" -S "$root" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++

cd "$root/build"

make uninstall

# make -j"$(nproc)"
make

sudo make install -j"$(nproc)"

rm -f "$root/build/install_manifest.txt"

make test ARGS=-j"$(nproc)"
