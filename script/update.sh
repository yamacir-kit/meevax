#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

sudo dpkg -r meevax

"$root/script/version.sh"

rm -rf "$root/build"

cmake -B "$root/build" -S "$root" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++

cmake --build "$root/build" --parallel $(nproc)

cmake --build "$root/build" --parallel $(nproc) --target package

sudo dpkg -i "$root/build/meevax_$(cat $root/VERSION)_amd64.deb"

cmake --build "$root/build" --parallel $(nproc) --target test
