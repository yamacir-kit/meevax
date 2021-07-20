#!/bin/sh -e

root="$(git rev-parse --show-toplevel)"

# ---- Phase 1 -----------------------------------------------------------------

sudo dpkg -r meevax

echo "\e[1;31mBUILD AND INSTALL MEEVAX $("$root"/script/version.sh) FROM SOURCE\e[0m"

rm -rf "$root/build"

cmake -B "$root/build" -S "$root" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++
cmake --build "$root/build" --parallel "$(nproc)"
cmake --build "$root/build" --parallel "$(nproc)" --target test -- ARGS=-j"$(nproc)"
cmake --build "$root/build" --parallel "$(nproc)" --target package

sudo dpkg -i "$root/build/meevax_$(cat "$root"/VERSION)_amd64.deb"

# ---- Phase 2 -----------------------------------------------------------------

rm -rf "$root/example/build"

cmake -B "$root/example/build" -S "$root/example" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++
cmake --build "$root/example/build" --parallel "$(nproc)"
cmake --build "$root/example/build" --parallel "$(nproc)" --target test -- ARGS=-j"$(nproc)"
